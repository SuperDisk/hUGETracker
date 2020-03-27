unit Codegen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, Instruments, Song, Utils,
  HugeDatatypes, Constants, dialogs, strutils, FileUtil, LazFileUtils,
  lclintf;

type
  TExportMode = (emNormal, emPreview, emGBS);

function RenderPreviewRom(Song: TSong): Boolean;
function RenderSongToFile(Song: TSong; Filename: String; Mode: TExportMode = emNormal): Boolean;

implementation

uses process;

function RenderOrderTable(OrderMatrix: TOrderMatrix): String;
function ArrayHelper(Ints: array of Integer): String;
var
  I: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  SL.StrictDelimiter := True;
  SL.Delimiter := ',';
  for I := Low(Ints) to High(Ints) do
    SL.Add('P'+IntToStr(Ints[I]));
  Result := SL.DelimitedText;
  SL.Free;
end;

var
  Res: TStringList;
  OrderCnt: Integer;
begin
  Res := TStringList.Create;
  OrderCnt := maxvalue([High(OrderMatrix[0]), High(OrderMatrix[1]),
                        High(OrderMatrix[2]), High(OrderMatrix[3])]);

  Res.Add('order_cnt: db ' + IntToStr(OrderCnt*2));
  Res.Add('order1: dw ' + ArrayHelper(OrderMatrix[0]));
  Res.Add('order2: dw ' + ArrayHelper(OrderMatrix[1]));
  Res.Add('order3: dw ' + ArrayHelper(OrderMatrix[2]));
  Res.Add('order4: dw ' + ArrayHelper(OrderMatrix[3]));

  Result := Res.Text;
  Res.Free;
end;

function RenderInstruments(Instruments: TInstrumentBank): String;
var
  SL, ResultSL: TStringList;
  AsmInstrument: TAsmInstrument;
  I, J: Integer;
begin
  ResultSL := TStringList.Create;

  for I := Low(Instruments) to High(Instruments) do begin
    AsmInstrument := InstrumentToBytes(Instruments[I]);
    SL := TStringList.Create;
    SL.StrictDelimiter := True;
    SL.Delimiter := ',';

    for J := Low(AsmInstrument) to High(AsmInstrument) do
      SL.Add(IntToStr(AsmInstrument[J]));

    ResultSL.Add(Format('%s: db %s', ['inst'+IntToStr(I), SL.DelimitedText]));
    SL.Free;
  end;

  Result := ResultSL.Text;
  ResultSL.Free;
end;

function RenderCell(Cell: TCell): String;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  SL.Delimiter := ',';
  SL.StrictDelimiter := True;

  if Cell.Note = NO_NOTE then
    SL.Add('___')
  else
    SL.Add(NoteToDriverMap.KeyData[Cell.Note]);

  if InRange(Cell.Instrument, 0, 15) then
    SL.Add(IntToStr(Cell.Instrument))
  else
    SL.Add('0');

  SL.Add('$' + EffectCodeToStr(Cell.EffectCode, Cell.EffectParams));

  // RGBDS thinks you're defining a new macro if you don't have a space first.
  Result := ' dn ' + SL.DelimitedText;
  SL.Free;
end;

function RenderPattern(Name: String; Pattern: TPattern): String;
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  SL.Add(Name + ':');

  for I := Low(TPattern) to High(TPattern) do
    SL.Add(RenderCell(Pattern[I]));

  Result := SL.Text;
  SL.Free;
end;

function RenderWaveforms(Waves: TWaveBank): String;
var
  SL, ResultSL: TStringList;
  I, J: Integer;
begin
  ResultSL := TStringList.Create;

  for I := Low(Waves) to High(Waves) do begin
    SL := TStringList.Create;
    SL.StrictDelimiter := True;
    SL.Delimiter := ',';

    J := Low(Waves[I]);
    while J < High(Waves[I]) do begin
      SL.Add(IntToStr((Waves[I, J] shl 4) or Waves[I, J+1]));
      Inc(J, 2);
    end;
    ResultSL.Add(Format('wave%d: db %s', [I, SL.DelimitedText]));
    SL.Free;
  end;

  Result := ResultSL.Text;
  ResultSL.Free;
end;

function RenderConstants(Song: TSong; Mode: TExportMode): String;
var
  SL: TStringList;
begin
  SL := TSTringList.Create;

  if Mode = emPreview then
    SL.Add('PREVIEW_MODE EQU 1')
  else if Mode = emGBS then begin
    SL.Add('GBS_MODE EQU 1');
    SL.Add('GBS_TITLE EQUS "\"' + PadRight(Song.Name, 32) + '\""');
    SL.Add('GBS_AUTHOR EQUS "\"' + PadRight(Song.Artist, 32) + '\""');
    SL.Add('GBS_COPYRIGHT EQUS "\"' + PadRight(IntToStr(CurrentYear), 32) + '\""');
  end;
  SL.Add('TICKS EQU '+IntToStr(Song.TicksPerRow));

  Result := SL.Text;
  SL.Free;
end;

procedure WriteRoutinesToFile(Routines: TRoutineBank);
var
  I: Integer;
  F: Text;
begin
  for I := Low(TRoutineBank) to High(TRoutineBank) do begin
    AssignFile(F, './hUGEDriver/routine'+IntToStr(I)+'.htt');
    Rewrite(F);
    Write(F, Routines[I]);
    CloseFile(F);
  end;
end;

function RenderPreviewROM(Song: TSong): Boolean;
begin
  Result := RenderSongToFile(Song, 'preview.gb', emPreview);
end;

function RenderSongToFile(Song: TSong; Filename: String; Mode: TExportMode = emNormal): Boolean;
var
  OutFile: Text;
  I: Integer;
  Proc: TProcess;
  OutSL: TStringList;
  FilePath: String;
label AssemblyError, Cleanup; // Eh, screw good practice. How bad can it be?
begin
  FilePath := Filename;
  Filename := ExtractFileNameWithoutExt(ExtractFileNameOnly(Filename));

  AssignFile(OutFile, './hUGEDriver/constants.htt');
  Rewrite(OutFile);
  Write(OutFile, RenderConstants(Song, Mode));
  CloseFile(OutFile);

  AssignFile(OutFile, './hUGEDriver/wave.htt');
  Rewrite(OutFile);
  Write(OutFile, RenderWaveforms(Song.Waves));
  CloseFile(OutFile);

  AssignFile(OutFile, './hUGEDriver/order.htt');
  Rewrite(OutFile);
  Write(OutFile, RenderOrderTable(Song.OrderMatrix));
  CloseFile(OutFile);

  AssignFile(OutFile, './hUGEDriver/instrument.htt');
  Rewrite(OutFile);
  Write(OutFile, RenderInstruments(Song.Instruments));
  CloseFile(OutFile);

  WriteRoutinesToFile(Song.Routines);

  AssignFile(OutFile, './hUGEDriver/pattern.htt');
  Rewrite(OutFile);

  // Are keys and data defined to be aligned? Seems like they are but
  // should probably find out if that's just an implementation detail...
  for I := 0 to Song.Patterns.Count-1 do
    Write(OutFile, RenderPattern('P'+IntToStr(Song.Patterns.Keys[I]),
                                 Song.Patterns.Data[I]^));

  CloseFile(OutFile);

  // Assemble the ROM
  Chdir('hUGEDriver');

  OutSL := TStringList.Create;
  Proc := TProcess.Create(nil);
  Proc.Options := Proc.Options + [poWaitOnExit, poUsePipes, poStdErrToOutput,
                                  poNoConsole];

  Proc.Executable := 'rgbasm';
  Proc.Parameters.Clear;
  Proc.Parameters.add('-o'+Filename+'.obj');
  Proc.Parameters.add('driverLite.z80');
  Proc.Execute;
  if Proc.ExitCode <> 0 then goto AssemblyError;

  Proc.Executable := 'rgblink';
  Proc.Parameters.Clear;
  Proc.Parameters.add('-m'+Filename+'.map');
  Proc.Parameters.add('-n'+Filename+'.sym');
  if Mode = emGBS then
    Proc.Parameters.add('-o'+Filename+'.gbs')
  else
    Proc.Parameters.add('-o'+Filename+'.gb');
  Proc.Parameters.add(Filename+'.obj');
  Proc.Execute;
  if Proc.ExitCode <> 0 then goto AssemblyError;

  if Mode <> emGBS then begin
    Proc.Executable := 'rgbfix';
    Proc.Parameters.Clear;
    Proc.Parameters.add('-p0');
    Proc.Parameters.add('-v');
    Proc.Parameters.add(Filename+'.gb');
    Proc.Execute;
    if Proc.ExitCode <> 0 then goto AssemblyError;
  end;

  if Mode <> emPreview then begin
    if FileExists(FilePath) then DeleteFile(FilePath);

    if not RenameFile(Filename+IfThen(Mode = emGBS, '.gbs', '.gb'), FilePath) then begin
      MessageDlg('Error!',
                 'Couldn''t create file at '+FilePath+'. Make sure your path is correct!',
                 mtError,
                 [mbOk],
                 0);
      Result := False;
      goto Cleanup;
    end;

    DeleteFile(Filename+'.obj');
    DeleteFile(Filename+'.sym');
    DeleteFile(Filename+'.map');
  end;

  Result := True;
  goto Cleanup;

  AssemblyError:
  Result := False;
  OutSL.LoadFromStream(Proc.Output);

  if OutSL.Text.Contains('routine') then
    MessageDlg(
      'Error!',
      'There was an error assembling the song for playback.'+LineEnding+LineEnding+
        Proc.Executable+' output:'+LineEnding+
        OutSL.Text,
      mtError,
      [mbOK],
      0)
  else begin
    MessageDlg(
      'Error!',
      'There was an error assembling the song for playback.'+LineEnding+LineEnding+
        Proc.Executable+' output:'+LineEnding+
        OutSL.Text+LineEnding+LineEnding+
        'Please report this issue on the hUGETracker GitHub issues page, and '+
        'post your song file!',
      mtError,
      [mbOK],
      0);

    {$ifdef PRODUCTION}OpenURL('https://github.com/SuperDisk/UGE/issues');{$endif}
  end;

  Cleanup:
  Proc.Free;
  OutSL.Free;
  Chdir('..');
end;

end.


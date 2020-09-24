unit Codegen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Instruments, Song, Utils,
  HugeDatatypes, Constants, Dialogs, strutils, FileUtil, LazFileUtils,
  lclintf;

type
  TExportMode = (emNormal, emPreview, emGBS);

function RenderPreviewRom(Song: TSong): boolean;
function RenderSongToFile(Song: TSong; Filename: string;
  Mode: TExportMode = emNormal): boolean;

implementation

uses process;

function RenderOrderTable(OrderMatrix: TOrderMatrix): string;

  function ArrayHelper(Ints: array of integer): string;
  var
    I: integer;
    SL: TStringList;
  begin
    SL := TStringList.Create;
    SL.StrictDelimiter := True;
    SL.Delimiter := ',';
    for I := Low(Ints) to High(Ints) do
      SL.Add('P' + IntToStr(Ints[I]));
    Result := SL.DelimitedText;
    SL.Free;
  end;

var
  Res: TStringList;
  OrderCnt: integer;
begin
  Res := TStringList.Create;
  OrderCnt := MaxIntValue([High(OrderMatrix[0]), High(OrderMatrix[1]),
    High(OrderMatrix[2]), High(OrderMatrix[3])]);

  Res.Add('order_cnt: db ' + IntToStr(OrderCnt * 2));
  Res.Add('order1: dw ' + ArrayHelper(OrderMatrix[0]));
  Res.Add('order2: dw ' + ArrayHelper(OrderMatrix[1]));
  Res.Add('order3: dw ' + ArrayHelper(OrderMatrix[2]));
  Res.Add('order4: dw ' + ArrayHelper(OrderMatrix[3]));

  Result := Res.Text;
  Res.Free;
end;

function RenderInstruments(Instruments: TInstrumentBank): string;
var
  SL, ResultSL: TStringList;
  AsmInstrument: TAsmInstrument;
  I, J: integer;
  TypePrefix: string;
  HighMask: byte;
begin
  ResultSL := TStringList.Create;

  for I := Low(Instruments) to High(Instruments) do
  begin
    AsmInstrument := InstrumentToBytes(Instruments[I]);
    SL := TStringList.Create;
    SL.StrictDelimiter := True;
    SL.Delimiter := ',';

    if Instruments[I].Type_ = itNoise then
    begin
      SL.Add(IntToStr(AsmInstrument[1]));

      HighMask := %00000000;
      if Instruments[I].LengthEnabled then
        HighMask := HighMask or %01000000;
      if Instruments[I].CounterStep = swSeven then
        HighMask := HighMask or %10000000;
      SL.Add(IntToStr(HighMask));

      for J := Low(TNoiseMacro) to High(TNoiseMacro) do
        SL.Add(IntToStr(Instruments[I].NoiseMacro[J]));
    end
    else
      for J := Low(AsmInstrument) to High(AsmInstrument) do
        SL.Add(IntToStr(AsmInstrument[J]));

    WriteStr(TypePrefix, Instruments[I].Type_);
    ResultSL.Add(Format('%s%s: db %s', [TypePrefix, 'inst' + IntToStr(I),
      SL.DelimitedText]));
    SL.Free;
  end;

  Result := ResultSL.Text;
  ResultSL.Free;
end;

function RenderCell(Cell: TCell): string;
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

function RenderPattern(Name: string; Pattern: TPattern): string;
var
  SL: TStringList;
  I: integer;
begin
  SL := TStringList.Create;
  SL.Add(Name + ':');

  for I := Low(TPattern) to High(TPattern) do
    SL.Add(RenderCell(Pattern[I]));

  Result := SL.Text;
  SL.Free;
end;

function RenderWaveforms(Waves: TWaveBank): string;
var
  SL, ResultSL: TStringList;
  I, J: integer;
begin
  ResultSL := TStringList.Create;

  for I := Low(Waves) to High(Waves) do
  begin
    SL := TStringList.Create;
    SL.StrictDelimiter := True;
    SL.Delimiter := ',';

    J := Low(Waves[I]);
    while J < High(Waves[I]) do
    begin
      SL.Add(IntToStr((Waves[I, J] shl 4) or Waves[I, J + 1]));
      Inc(J, 2);
    end;
    ResultSL.Add(Format('wave%d: db %s', [I, SL.DelimitedText]));
    SL.Free;
  end;

  Result := ResultSL.Text;
  ResultSL.Free;
end;

function RenderConstants(Song: TSong; Mode: TExportMode): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  SL.Add('STANDALONE_MODE EQU 1');

  if Mode = emPreview then
    SL.Add('PREVIEW_MODE EQU 1')
  else if Mode = emGBS then
  begin
    SL.Add('GBS_MODE EQU 1');
    SL.Add('GBS_TITLE EQUS "\"' + PadRight(Song.Name, 32) + '\""');
    SL.Add('GBS_AUTHOR EQUS "\"' + PadRight(Song.Artist, 32) + '\""');
    SL.Add('GBS_COPYRIGHT EQUS "\"' + PadRight(IntToStr(CurrentYear), 32) + '\""');
  end;
  SL.Add('TICKS EQU ' + IntToStr(Song.TicksPerRow));

  Result := SL.Text;
  SL.Free;
end;

procedure WriteRoutinesToFile(Routines: TRoutineBank);
var
  I: integer;
  F: Text;
begin
  for I := Low(TRoutineBank) to High(TRoutineBank) do
  begin
    AssignFile(F, './hUGEDriver/routine' + IntToStr(I) + '.htt');
    Rewrite(F);
    Write(F, Routines[I]);
    CloseFile(F);
  end;
end;

procedure RectifyGBSFile(GBSFile: String);
{ This procedure removes the useless $400 bytes located after the GBS header.
  Since RGBDS has no feature to simply offset a section of code, we have to
  manually introduce padding and then remove it with this silly routine. }
var
  Stream: TFileStream;
  Buffer: array of Byte;
begin
  Stream := TFileStream.Create(GBSFile, fmOpenRead);
  SetLength(Buffer, Stream.Size - $400);
  Stream.Read(Buffer[0], $70);
  Stream.Seek($400, soCurrent); // Skip the empty byte padding
  Stream.Read(Buffer[$70], Stream.Size - Stream.Position);
  Stream.Free;

  Stream := TFileStream.Create(GBSFile, fmOpenWrite);
  Stream.Write(Buffer[0], Length(Buffer));
  Stream.Free;
end;

function RenderPreviewROM(Song: TSong): boolean;
begin
  Result := RenderSongToFile(Song, 'preview.gb', emPreview);
end;

function RenderSongToFile(Song: TSong; Filename: string;
  Mode: TExportMode = emNormal): boolean;
var
  OutFile: Text;
  I: integer;
  Proc: TProcess;
  OutSL: TStringList;
  FilePath: string;

  procedure WriteHTT(F: String; S: String);
  begin
    AssignFile(OutFile, F);
    Rewrite(OutFile);
    Write(OutFile, S);
    CloseFile(OutFile);
  end;

  function Assemble(OutFile: string; InFile: string; Defines: array of string): integer;
  var
    Define: string;
  begin
    Proc.Executable := 'rgbasm';
    Proc.Parameters.Clear;
    Proc.Parameters.Add('-o' + OutFile);
    for Define in Defines do
      Proc.Parameters.Add('-D' + Define);
    Proc.Parameters.Add(InFile);
    Proc.Execute;

    Result := Proc.ExitCode;
  end;

  function Link(OutFile: string; InFiles: array of string; Map: string = '';
    Sym: string = ''): integer;
  var
    InFile: string;
  begin
    Proc.Executable := 'rgblink';
    Proc.Parameters.Clear;
    Proc.Parameters.Add('-o' + OutFile);
    if Map <> '' then
      Proc.Parameters.Add('-m' + Map);
    if Sym <> '' then
      Proc.Parameters.Add('-n' + Sym);
    for InFile in InFiles do
      Proc.Parameters.Add(InFile);
    Proc.Execute;

    Result := Proc.ExitCode;
  end;

  function Fix(GBFile: string): integer;
  begin
    Proc.Executable := 'rgbfix';
    Proc.Parameters.Clear;
    Proc.Parameters.Add('-p0');
    Proc.Parameters.Add('-v');
    Proc.Parameters.Add(GBFile);
    Proc.Execute;

    Result := Proc.ExitCode;
  end;

label
  AssemblyError, Cleanup; // Eh, screw good practice. How bad can it be?
begin
  FilePath := Filename;
  Filename := ExtractFileNameWithoutExt(ExtractFileNameOnly(Filename));

  WriteHTT('./hUGEDriver/constants.htt', RenderConstants(Song, Mode));
  WriteHTT('./hUGEDriver/wave.htt', RenderWaveforms(Song.Waves));
  WriteHTT('./hUGEDriver/order.htt', RenderOrderTable(Song.OrderMatrix));
  WriteHTT('./hUGEDriver/duty_instrument.htt', RenderInstruments(Song.Instruments.Duty));
  WriteHTT('./hUGEDriver/wave_instrument.htt', RenderInstruments(Song.Instruments.Wave));
  WriteHTT('./hUGEDriver/noise_instrument.htt', RenderInstruments(Song.Instruments.Noise));
  WriteRoutinesToFile(Song.Routines);

  AssignFile(OutFile, './hUGEDriver/pattern.htt');
  Rewrite(OutFile);

  // TODO: Are keys and data defined to be aligned? Seems like they are but
  // should probably find out if that's just an implementation detail...
  for I := 0 to Song.Patterns.Count - 1 do
    Write(OutFile, RenderPattern('P' + IntToStr(Song.Patterns.Keys[I]),
      Song.Patterns.Data[I]^));

  CloseFile(OutFile);

  // Build the file
  Chdir('hUGEDriver');

  OutSL := TStringList.Create;
  Proc := TProcess.Create(nil);
  Proc.Options := Proc.Options + [poWaitOnExit, poUsePipes, poStdErrToOutput,
    poNoConsole];

  // Assemble

  // TODO: this is fugly, wish there was a ternary operator in this language...
  if Mode = emPreview then begin
    if Assemble(Filename + '_driver.obj', 'driver.z80', ['PREVIEW_MODE']) <> 0 then
      goto AssemblyError;
  end else begin
    if Assemble(Filename + '_driver.obj', 'driver.z80', []) <> 0 then
      goto AssemblyError;
  end;

  if Assemble(Filename + '_song.obj', 'song.z80', []) <> 0 then
    goto AssemblyError;

  if Mode = emGBS then begin
    if Assemble(Filename + '_gbs.obj', 'gbs.z80', []) <> 0 then
      goto AssemblyError;
  end
  else begin
    if Assemble(Filename + '_player.obj', 'player.z80', []) <> 0 then
      goto AssemblyError;
  end;

  // Link
  if Mode = emGBS then
  begin
    if Link(Filename+'.gbs', [Filename + '_driver.obj',
      Filename + '_song.obj', Filename + '_gbs.obj']) <> 0 then
      goto AssemblyError;
  end
  else begin
    if Link(Filename + '.gb', [Filename + '_driver.obj',
      Filename + '_song.obj', Filename + '_player.obj'], Filename + '.map',
      Filename + '.sym') <> 0 then
      goto AssemblyError;
  end;

  // Fix
  if Mode = emGBS then
    RectifyGBSFile(Filename+'.gbs')
  else begin
    if (Fix(Filename + '.gb') <> 0) then
      goto AssemblyError;
  end;

  // Move to destination
  if Mode <> emPreview then
  begin
    if FileExists(FilePath) then
      DeleteFile(FilePath);

    if not RenameFile(Filename + IfThen(Mode = emGBS, '.gbs', '.gb'), FilePath) then
    begin
      MessageDlg('Error!',
        'Couldn''t create file at ' + FilePath +
        '. Make sure your path is correct!',
        mtError,
        [mbOK],
        0);
      Result := False;
      goto Cleanup;
    end;
    {$ifdef DEVELOPMENT}
    RenameFile(Filename + '.obj', FilePath + '.obj');
    RenameFile(Filename + '.sym', FilePath + '.sym');
    RenameFile(Filename + '.map', FilePath + '.map');
    {$endif}
    {$ifdef PRODUCTION}
    DeleteFile(Filename + '.obj');
    DeleteFile(Filename + '.sym');
    DeleteFile(Filename + '.map');
    {$endif}
  end;

  Result := True;
  goto Cleanup;

  AssemblyError:
    Result := False;
  OutSL.LoadFromStream(Proc.Output);

  if OutSL.Text.Contains('routine') then
    MessageDlg(
      'Error!',
      'There was an error assembling the song for playback.' + LineEnding + LineEnding +
      Proc.Executable + ' output:' + LineEnding + OutSL.Text,
      mtError,
      [mbOK],
      0)
  else
  begin
    MessageDlg(
      'Error!',
      'There was an error assembling the song for playback.' + LineEnding + LineEnding +
      Proc.Executable + ' output:' + LineEnding + OutSL.Text + LineEnding + LineEnding +
      'Please report this issue on the hUGETracker GitHub issues page, and ' +
      'post your song file!',
      mtError,
      [mbOK],
      0);

    {$ifdef PRODUCTION}
    OpenURL('https://github.com/SuperDisk/hUGETracker/issues');
{$endif}
  end;

  Cleanup:
    Proc.Free;
  OutSL.Free;
  Chdir('..');
end;

end.

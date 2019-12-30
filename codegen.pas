unit Codegen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math,
  Waves, Instruments, Song, Utils,
  HugeDatatypes, Constants, dialogs;

function RenderOrderTable(OrderMatrix: TOrderMatrix): String;
function RenderInstruments(Instruments: TInstrumentBank): String;
function RenderPattern(Name: String; Pattern: TPattern): String;
function RenderWaveforms(Waves: TWaveBank): String;

function RenderPreviewRom(Song: TSong): Boolean;
procedure RenderSongToFile(Song: TSong; Filename: String);

//TODO: RenderRoutines

implementation

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

  SL.Add(IntToStr(Cell.Instrument));
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

function RenderPreviewROM(Song: TSong): Boolean;
var
  OutFile: Text;
  I: Integer;
label AssemblyError, Cleanup;
begin
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

  AssignFile(OutFile, './hUGEDriver/pattern.htt');
  Rewrite(OutFile);

  for I := 0 to Song.Patterns.Count-1 do
    Write(OutFile, RenderPattern('P'+IntToStr(I), Song.Patterns.Data[I]^));

  CloseFile(OutFile);

  // Assemble the ROM
  Chdir('hUGEDriver');

  if ExecuteProcess(('rgbasm'),
    '-opreview.obj driverLite.z80', []) <> 0 then
    goto AssemblyError;

  if ExecuteProcess(('rgblink'),
    '-mpreview.map -npreview.sym -opreview.gb preview.obj', []) <> 0 then
    goto AssemblyError;

  if ExecuteProcess(('rgbfix'),
    '-p0 -v preview.gb', []) <> 0 then
    goto AssemblyError;

  Result := True;
  goto Cleanup;

  // Eh, screw good practice. How bad can it be?
  AssemblyError:
  Result := False;
  MessageDlg('Error!', 'There was an error assembling the song for playback. Write the rest of this dialog.', mtError, [mbOK], 0);

  Cleanup:
  Chdir('..');
end;

procedure RenderSongToFile(Song: TSong; Filename: String);
begin
end;

end.


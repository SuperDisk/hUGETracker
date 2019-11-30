unit Codegen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math,
  Waves, Instruments, Song,
  HugeDatatypes;

function RenderOrderTable(OrderMatrix: TOrderMatrix): String;

function RenderInstruments(Instruments: TInstrumentBank): String;

function RenderPattern(Name: String; Pattern: TPattern): String;

function RenderWaveforms(Waves: TWaveBank): String;

procedure RenderSongToFile(Song: TSong; Filename: String);

//TODO: RenderRoutines

implementation

function RenderOrderTable(OrderMatrix: TOrderMatrix): String;
var
  I: Integer;
  Res: TStringList;
  OrderCnt: Integer;

function ArrayHelper(Ints: array of Integer): String;
var
  I: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  SL.Delimiter := ',';
  for I := Low(Ints) to High(Ints) do
    SL.Add('o' + IntToStr(Ints[I]));
  Result := SL.DelimitedText;
  SL.Free;
end;
begin
  Res := TStringList.Create;
  Res.Delimiter := #10;
  OrderCnt := maxvalue([High(OrderMatrix[0]), High(OrderMatrix[1]),
                        High(OrderMatrix[2]), High(OrderMatrix[3])]);

  Res.Add(Format('order_cnt: db %s', [OrderCnt*2]));
  Res.Add(Format('order1: dw %s', [ArrayHelper(OrderMatrix[0])]));
  Res.Add(Format('order2: dw %s', [ArrayHelper(OrderMatrix[1])]));
  Res.Add(Format('order3: dw %s', [ArrayHelper(OrderMatrix[2])]));
  Res.Add(Format('order4: dw %s', [ArrayHelper(OrderMatrix[3])]));

  Result := Res.DelimitedText;
  Res.Free;
end;

function RenderInstruments(Instruments: TInstrumentBank): String;
var
  SL, ResultSL: TStringList;
  AsmInstrument: TAsmInstrument;
  I, J: Integer;
begin
  ResultSL := TStringList.Create;
  ResultSL.Delimiter := #10;

  for I := Low(Instruments) to High(Instruments) do begin
    AsmInstrument := InstrumentToBytes(Instruments[I]);
    SL := TStringList.Create;
    SL.Delimiter := ',';

    for J := Low(AsmInstrument) to High(AsmInstrument) do
      SL.Add(IntToStr(AsmInstrument[J]));

    ResultSL.Add(Format('%s: db %s', ['inst'+IntToStr(I), SL.DelimitedText]));
    SL.Free;
  end;

  Result := ResultSL.DelimitedText;
  ResultSL.Free;
end;

function RenderPattern(Name: String; Pattern: TPattern): String;
begin

end;

function RenderWaveforms(Waves: TWaveBank): String;
var
  SL, ResultSL: TStringList;
  I, J: Integer;
begin
  ResultSL := TStringList.Create;
  ResultSL.Delimiter := #10;

  for I := Low(Waves) to High(Waves) do begin
    SL := TStringList.Create;
    SL.Delimiter := ',';
    for J := Low(Waves[I]) to High(Waves[I]) do
      SL.Add(IntToStr(Waves[I][J]));
    Result := Format('wave%d: db %s', [I, SL.DelimitedText]);
    SL.Free;
  end;

  Result := ResultSL.DelimitedText;
  ResultSL.Free;
end;

procedure RenderSongToFile(Song: TSong; Filename: String);
var
  OutFile: Text;
begin
  Assign(OutFile, 'C:/test/wave');
  Rewrite(OutFile);
  Write(OutFile, RenderWaveforms(Song.Waves));
  Close(OutFile);

  {Assign(OutFile, 'C:/test/order');
  Rewrite(OutFile);
  Write(RenderOrderTable(Song.Waves));
  Close(OutFile);}

  Assign(OutFile, 'C:/test/instrument');
  Rewrite(OutFile);
  Write(OutFile, RenderInstruments(Song.Instruments));
  Close(OutFile);
end;

end.


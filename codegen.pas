unit Codegen;

{$mode objfpc}

interface

uses
  Classes, SysUtils, math,
  Waves, Instruments,
  HugeDatatypes;

function RenderOrderTable(OrderMatrix: TOrderMatrix): String;
function RenderInstrument(Name: String; Instr: TInstrument): String;
function RenderPattern(Name: String; Pattern: TPattern): String;
function RenderWaveform(Name: String; Wave: TWave): String;

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
  Strs: array of string;
begin
  for I := Low(Ints) to High(Ints) do
    Strs[I] := 'o' + IntToStr(Ints[I]);
  Result := TStringHelper.Join(',', Strs);
end;
begin
  Res := TStringList.Create;
  Res.Delimiter := LineEnding;
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

function RenderInstrument(Name: String; Instr: TInstrument): String;
var
  SL: TStringList;
begin

end;

function RenderPattern(Name: String; Pattern: TPattern): String;
begin

end;

function RenderWaveform(Name: String; Wave: TWave): String;
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  SL.Delimiter := ', ';
  for I := Low(Wave) to High(Wave) do
    SL.Add(IntToStr(Wave[I]));
  Result := Name + SL.DelimitedText;
  SL.Free;
end;

end.


unit ClipboardUtils;

{$mode delphi}

interface

uses
  Classes, SysUtils, Constants, Clipbrd, HugeDatatypes;

{type
  TModplugClipboardCell = packed record
    Note: array[0..2] of Char;
    Instrument: array[0..1] of Char;
    Volume: array[0..2] of Char;
    EffectCode: Char;
    EffectParams: array[0..1] of Char;
  end;}

// function ModplugToHuge(MPCell: TModplugClipboardCell): TCell;
function GetPastedCells: TSelection;

implementation

{function ModplugToHuge(MPCell: TModplugClipboardCell): TCell;
begin
  Result.Note := NoteToCodeMap.KeyData[String(MPCell.Note)];
  Result.Instrument := StrToInt(String(MPCell.Instrument));
  Result.EffectCode := StrToInt(String('0x'+MPCell.EffectCode));
  Result.EffectParams.Value := StrToInt(String('0x'+MPCell.EffectParams));
end;}

function ParseCell(Cell: String): TCell;
var
  Note, Instr, EffectCode, EffectParam1, EffectParam2: String;
  Temp: Integer;
begin
  Note := Cell.Substring(0, 3);
  Instr := Cell.Substring(3, 2);
  EffectCode := Cell.Substring(8, 1);
  EffectParam1 := Cell.Substring(9, 1);
  EffectParam2 := Cell.Substring(10, 1);

  if not NoteToCodeMap.TryGetData(Note, Result.Note) then
    Result.Note := NO_NOTE;

  if not TryStrToInt(Instr, Result.Instrument) then
    Result.Instrument := 0;

  if not TryStrToInt('x'+EffectCode, Result.EffectCode) then
    Result.EffectCode := 0;

  if TryStrToInt('x'+EffectParam1, Temp) then
    Result.EffectParams.Param1 := Temp
  else
    Result.EffectParams.Param1 := 0;

  if TryStrToInt('x'+EffectParam2, Temp) then
    Result.EffectParams.Param2 := Temp
  else
    Result.EffectParams.Param2 := 0;
end;

function GetPastedCells: TSelection;
var
  SL: TStringList;
  StringCells: TStringArray;
  Row, Cell, S: String;
  I, J: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Text := Clipboard.AsText;

    // First line is the header, last is a blank line
    SL.Delete(0);
    SetLength(Result, SL.Count);

    I := 0;
    for Row in SL do begin
      StringCells := Row.Split('|');

      SetLength(Result[I], Length(StringCells)-1);
      for J := 0 to High(StringCells)-1 do
        Result[I, J] := ParseCell(StringCells[J+1]);
      Inc(I);
    end;
  finally
    SL.Free;
  end;
end;

end.


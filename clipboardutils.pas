unit ClipboardUtils;

{$mode delphi}

interface

uses
  Classes, SysUtils, Constants, Clipbrd, HugeDatatypes, Utils;

function GetPastedCells: TSelection;
procedure CopyCells(Selection: TSelection);

implementation

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
    Result.EffectParams.Value := 0;

  if TryStrToInt('x'+EffectParam2, Temp) then
    Result.EffectParams.Param2 := Temp
  else
    Result.EffectParams.Value := 0;
end;

function GetPastedCells: TSelection;
var
  SL: TStringList;
  StringCells: TStringArray;
  Row: String;
  I, J: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Text := Clipboard.AsText;

    // Delete lines until we reach the note data
    while not SL.Strings[0].StartsWith('|') do
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

function SerializeCell(Cell: TCell): String;
begin
  Result := '|';

  Result += NoteCodeToString(Cell.Note);
  Result += FormatFloat('00', Cell.Instrument);
  Result += '...'; // volume
  Result += EffectCodeToStr(Cell.EffectCode, Cell.EffectParams);
end;

procedure CopyCells(Selection: TSelection);
var
  C, R: Integer;
  S: String;
  SL: TStringList;
begin
  SL := TStringList.Create;
  SL.Add('The hUGETracker paste format is compatible with...');
  SL.Add('ModPlug Tracker  XM');
  try
    for R := 0 to High(Selection) do begin
      S := '';
      for C := 0 to High(Selection[R]) do
        S += SerializeCell(Selection[R, C]);
      SL.Add(S);
    end;

    Clipboard.AsText := SL.Text;
  finally
    SL.Free;
  end;
end;

end.


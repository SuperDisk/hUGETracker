unit ClipboardUtils;

{$mode delphi}

interface

uses
  Classes, SysUtils, Constants, Clipbrd, HugeDatatypes, Utils;

type
  EClipboardFormatException = class(Exception);

function GetPastedCells: TSelection;
procedure CopyCells(Selection: TSelection);

implementation

function ParseCell(Cell: String): TSelectedCell;

function StrToInt_(S: String; out I: Integer; Hex: Boolean = False): Boolean;
begin
  if Trim(S) = '' then Exit(False);
  if Hex then S := 'x'+S;
  if not TryStrToInt(S, I) then
    I := 0;
  Result := True;
end;

var
  Note, Instr, EffectCode, EffectParam: String;
  Temp: Integer;
begin
  Note := Cell.Substring(0, 3);
  Instr := Cell.Substring(3, 2);
  EffectCode := Cell.Substring(8, 1);
  EffectParam := Cell.Substring(9, 2);
  //EffectParam1 := Cell.Substring(9, 1);
  //EffectParam2 := Cell.Substring(10, 1);

  Result.Parts := [cpNote, cpInstrument, cpEffectCode, cpEffectParams];

  if Trim(Note) = '' then Exclude(Result.Parts, cpNote)
  else if not NoteToCodeMap.TryGetData(Note, Result.Cell.Note) then
    Result.Cell.Note := NO_NOTE;

  if not StrToInt_(Instr, Result.Cell.Instrument) then
    Exclude(Result.Parts, cpInstrument);
  if not StrToInt_(EffectCode, Result.Cell.EffectCode, True) then
    Exclude(Result.Parts, cpEffectCode);
  if StrToInt_(EffectParam, Temp, True) then
    Result.Cell.EffectParams.Value := Temp
  else
    Exclude(Result.Parts, cpEffectParams);
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
    except
      on E: Exception do
        raise EClipboardFormatException.Create('Clipboard contained invalid data!');
    end;
  finally
    SL.Free;
  end;
end;

function SerializeCell(Cell: TSelectedCell): String;
begin
  Result := '|';

  if cpNote in Cell.Parts then
    Result += NoteCodeToString(Cell.Cell.Note)
  else
    Result += '   ';

  if cpInstrument in Cell.Parts then
    Result += FormatFloat('00', Cell.Cell.Instrument)
  else
    Result += '  ';

  Result += '...'; // volume

  if (cpEffectCode in Cell.Parts) or (cpEffectParams in Cell.Parts) then
    Result += EffectCodeToStr(Cell.Cell.EffectCode, Cell.Cell.EffectParams)
  else
    Result += '   ';
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


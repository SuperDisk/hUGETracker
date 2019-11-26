unit ClipboardUtils;

{$mode delphi}

interface

uses
  Classes, SysUtils, Constants, Clipbrd, HugeDatatypes;

type
  TModplugClipboardCell = packed record
    Pipe: Char;
    Note: array[0..2] of Char;
    Instrument: array[0..1] of Char;
    Volume: array[0..2] of Char;
    EffectCode: Char;
    EffectParams: array[0..1] of Char;
  end;

function ModplugToHuge(MPCell: TModplugClipboardCell): TCell;

implementation

function ModplugToHuge(MPCell: TModplugClipboardCell): TCell;
begin
  Writeln(String(MPCell.Pipe));
  Writeln(String(MPCell.Note));
  Result.Note := NoteToCodeMap.KeyData[String(MPCell.Note)];
  Result.Instrument := StrToInt(String(MPCell.Instrument));
  Result.EffectCode := StrToInt(String('0x'+MPCell.EffectCode));
  Result.EffectParams.Value := StrToInt(String('0x'+MPCell.EffectParams));
end;

function GetPastedCells: TSelection;
var
  C: TCell;
  X: TModplugClipboardCell;
  ClipboardDynamicBytes: TBytes;
  ClipboardBytes: array[0..sizeof(X)] of Byte absolute X;
  I: Integer;
begin
  ClipboardDynamicBytes := TEncoding.UTF8.GetBytes(Clipboard.AsText);
  for I := 0 to sizeof(X) do
    ClipboardBytes[I] := ClipboardDynamicBytes[I];

  C := ModplugToHuge(X);
end;

end.


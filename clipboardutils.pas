unit ClipboardUtils;

{$mode delphi}

interface

uses
  Classes, SysUtils, Constants, HugeDatatypes;

type
  TModplugClipboardCell = packed record
    Pipe: Char;
    Note: array[0..2] of Char;
    Instrument: array[0..1] of Char;
    Volume: array[0..2] of Char;
    EffectCode: Char;
    EffectParam: array[0..2] of Char;
  end;

function ModplugToHuge(MPCell: TModplugClipboardCell): TCell;

implementation

function ModplugToHuge(MPCell: TModplugClipboardCell): TCell;
begin
  if not NoteToCodeMap.TryGetData(String(MPCell.Note), Result.Note) then begin
    Result := nil;
    exit;
  end;
  try
    Result.Instrument := StrToInt(String(MPCell.Instrument));
    Result.EffectCode := StrToInt(String(MPCell.EffectCode));
    Result.EffectParam := StrToInt(String(MPCell.EffectParam));
  except
    on E: EConvertError do Result := nil;
  end;
end;

end.


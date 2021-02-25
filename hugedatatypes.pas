unit HugeDatatypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, instruments, waves;

type
  Nibble = $0..$F;

  TRoutine = string;

  TInstrumentBankV1 = packed array[1..15] of TInstrumentV1;
  TInstrumentCollectionV1 = packed record
    case Boolean of
      False: (Duty, Wave, Noise: TInstrumentBankV1);
      True: (All: packed array[1..45] of TInstrumentV1);
  end;

  TInstrumentBankV2 = packed array[1..15] of TInstrumentV2;
  TInstrumentCollectionV2 = packed record
    case Boolean of
      False: (Duty, Wave, Noise: TInstrumentBankV2);
      True: (All: packed array[1..45] of TInstrumentV2);
  end;

  TInstrumentBank = TInstrumentBankV2;
  TInstrumentCollection = TInstrumentCollectionV2;

  TWaveBankV1 = packed array[0..15] of TWaveV1;
  TWaveBankV2 = packed array[0..15] of TWaveV2;
  TWaveBank = TWaveBankV2;
  TRoutineBank = packed array[0..15] of TRoutine;

  TEffectParams = bitpacked record
    case Boolean of
      True: (Param2, Param1: Nibble);
      False: (Value: Byte);
  end;

  TCell = packed record
    Note: Integer;
    Instrument: Integer;
    EffectCode: Integer;
    EffectParams: TEffectParams;
  end;
  PCell = ^TCell;

  TPattern = packed array[0..63] of TCell;
  PPattern = ^TPattern;

  //TPatternMap = specialize TFPGMap<Integer, PPattern>;
  TPatternMap = class(specialize TFPGMap<Integer, PPattern>)
    function GetOrCreateNew(Key: Integer): PPattern;
    function MaxKey: Integer;

    procedure DeletePattern(Key: Integer);

    destructor Destroy; override;
  end;

  TOrderMatrix = packed array[0..3] of array of Integer;

  TCellPart = (
    cpNote = 0,
    cpInstrument = 1,
    cpVolume = 2,
    cpEffectCode = 3,
    cpEffectParams = 4
  );

  TSelectionPos = record
    X, Y: Integer;
    SelectedPart: TCellPart
  end;

  TSelectedCell = record
    Cell: TCell;
    Parts: set of TCellPart;
  end;

  TSelectionRow = array of TSelectedCell;
  TSelection = array of TSelectionRow;

operator > (L, R: TSelectionPos): Boolean;
operator < (L, R: TSelectionPos): Boolean;
operator >= (L, R: TSelectionPos): Boolean;
operator <= (L, R: TSelectionPos): Boolean;
operator = (L, R: TSelectionPos): Boolean;

procedure IncSelectionPos(var SP: TSelectionPos);
procedure DecSelectionPos(var SP: TSelectionPos);

implementation

uses Utils;

operator>(L, R: TSelectionPos): Boolean;
begin
  if L.X > R.X then
    Result := True
  else if (L.X = R.X) and (L.SelectedPart > R.SelectedPart) then
    Result := True
  else
    Result := False;
end;

operator<(L, R: TSelectionPos): Boolean;
begin
  if L.X < R.X then
    Result := True
  else if (L.X = R.X) and (L.SelectedPart < R.SelectedPart) then
    Result := True
  else
    Result := False;
end;

operator>=(L, R: TSelectionPos): Boolean;
begin
  Result := (L > R) or (L = R);
end;

operator<=(L, R: TSelectionPos): Boolean;
begin
  Result := (L < R) or (L = R);
end;

operator=(L, R: TSelectionPos): Boolean;
begin
  Result := (L.X = R.X) and (L.SelectedPart = R.SelectedPart);
end;

procedure IncSelectionPos(var SP: TSelectionPos);
begin
  if SP.SelectedPart = High(TCellPart) then begin
    SP.SelectedPart := Low(TCellPart);
    Inc(SP.X);
  end
  else Inc(SP.SelectedPart);
end;

procedure DecSelectionPos(var SP: TSelectionPos);
begin
  if SP.SelectedPart = Low(TCellPart) then begin
    SP.SelectedPart := High(TCellPart);
    Dec(SP.X);
  end
  else Dec(SP.SelectedPart);
end;

{ TPatternMap }

function TPatternMap.GetOrCreateNew(Key: Integer): PPattern;
begin
  if Self.IndexOf(Key) <> -1 then
    Result := Self.KeyData[Key]
  else begin
    New(Result);
    BlankPattern(Result);
    Self.Add(Key, Result);
  end;
end;

function TPatternMap.MaxKey: Integer;
var
  X: Integer;
begin
  Result := 0;
  for X := 0 to Self.Count-1 do
    if Self.Keys[X] > Result then Result := Self.Keys[X];
  Inc(Result);
end;

procedure TPatternMap.DeletePattern(Key: Integer);
begin
  Dispose(KeyData[Key]);
  Self.Remove(Key);
end;

destructor TPatternMap.Destroy;
var
  I: Integer;
begin
  for I := 0 to Self.Count-1 do
    Dispose(Self.Data[I]);

  inherited;
end;

end.


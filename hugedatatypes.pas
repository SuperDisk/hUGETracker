unit HugeDatatypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, instruments, waves;

type
  Nibble = $0..$F;

  TRoutine = string;

  TInstrumentBank = packed array[1..15] of TInstrument;
  TInstrumentCollection = packed record
    case Boolean of
      False: (DutyInstruments, WaveInstruments, NoiseInstruments: TInstrumentBank);
      True: (All: packed array[1..45] of TInstrument);
  end;

  TWaveBank = packed array[0..15] of TWave;
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

  TPatternMap = specialize TFPGMap<Integer, PPattern>;
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

end.


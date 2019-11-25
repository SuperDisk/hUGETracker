unit TrackerGrid;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, Graphics, Constants, LCLType, math,
  LCLIntf, LMessages, ClipboardUtils, HugeDatatypes;

// TODO: Maybe read these from a config file
const
  clNote = TColor($7F4A00);
  clInstrument = TColor($7F7F00);
  clVolume = TColor($99FFB7);

  clFxMisc = TColor($3F3F7C);
  clFxPitch = TColor($006262);
  clFxVolume = TColor($007F26);
  clFxPan = clInstrument;
  clFxSong = TColor($00007F);

  NumColumns = 4;
  NumRows = 64;

type
  TCellPart = (cpNote = 0, cpInstrument = 1, cpVolume = 2, cpEffect = 3);

  TSelectionPos = record
    X, Y: Integer;
    SelectedPart: TCellPart
  end;

  { TTrackerGrid }

  TTrackerGrid = class(TCustomControl)
    constructor Create(AOwner: TComponent; Parent: TWinControl);
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure DoPaste(var Msg: TLMessage); message LM_PASTE;
    procedure DoCopy(var Msg: TLMessage); message LM_COPY;
    procedure DoCut(var Msg: TLMessage); message LM_CUT;

    procedure RenderSelectedArea;
    procedure ClampCursors;

  private
    Cells: array[0..3, 0..63] of TCell;

    ColumnWidth: Integer;
    RowHeight: Integer;
    CharHeight: Integer;
    CharWidth: Integer;

    MouseButtonDown: Boolean;
    Selecting: Boolean;

    procedure RenderRow(Row: Integer);
    procedure RenderCell(Cell: TCell);

    function GetEffectColor(EffectCode: Integer): TColor;
    function SelectionsToRect(S1, S2: TSelectionPos): TRect;
    function SelectionToRect(Selection: TSelectionPos): TRect;
    function MousePosToSelection(X, Y: Integer): TSelectionPos;

  public
    Cursor, Other: TSelectionPos;
  end;

implementation

{ TTrackerGrid }

constructor TTrackerGrid.Create(AOwner: TComponent; Parent: TWinControl);
var
  X, Y: Integer;
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csDoubleClicks];
  Self.Parent := Parent;

  // Kind of a hack
  with Canvas.Font do begin
    Name := 'Pixelite';
    Size := 12;
    ColumnWidth := GetTextWidth('C-5 01v64C01 ');
    RowHeight := GetTextHeight('C-5 01v64C01 ');

    CharHeight := RowHeight;
    CharWidth := GetTextWidth('C');
  end;

  Width := ColumnWidth*4;
  Height := RowHeight*64;

  for X := 0 to High(Cells) do
    for Y := 0 to High(Cells[X]) do
      Cells[X, Y].Note := NO_NOTE;
end;

procedure TTrackerGrid.Paint;
var
  I: Integer;
  R: TRect;
begin
  inherited Paint;

  with Canvas do begin
    Brush.Color := RGBToColor(225, 219, 208);
    Clear;

    for I := 0 to 64 do begin
      if (I mod 4) = 0 then begin
        Brush.Color := RGBToColor(216, 209, 195);
        FillRect(0, I*RowHeight, Width, (I+1)*RowHeight);
      end;
      if (I mod 16) = 0 then begin
        Brush.Color := RGBToColor(206, 197, 181);
        FillRect(0, I*RowHeight, Width, (I+1)*RowHeight);
      end;
      if I = Cursor.Y then begin
        Brush.Color := RGBToColor(169, 153, 122);
        FillRect(0, I*RowHeight, Width, (I+1)*RowHeight);
      end;

      MoveTo(0, I*RowHeight);
      RenderRow(I);
    end;
  end;

  Canvas.Pen.Color := TColor($ABB7BC);
  Canvas.Pen.Width := 2;
  R := TRect.Create(0, 0, ColumnWidth+1, Height);
  Canvas.Rectangle(R);
  R := TRect.Create(ColumnWidth, 0, ColumnWidth, Height);
  Canvas.Rectangle(R);
  R := TRect.Create(ColumnWidth*2, 0, ColumnWidth, Height);
  Canvas.Rectangle(R);
  R := TRect.Create(ColumnWidth*3, 0, ColumnWidth, Height);
  Canvas.Rectangle(R);
  R := TRect.Create(ColumnWidth*4, 0, ColumnWidth, Height);
  Canvas.Rectangle(R);

  RenderSelectedArea;
end;

procedure TTrackerGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  MouseButtonDown := True;
  Selecting := False;

  if not (csDesigning in ComponentState) and CanFocus then
    SetFocus;

  Cursor := MousePosToSelection(X, Y);

  ClampCursors;
  Other := Cursor;

  Invalidate;
end;

procedure TTrackerGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  if MouseButtonDown then begin
    Selecting := True;
    Other := MousePosToSelection(X, Y);

    ClampCursors;

    Invalidate;
  end;
end;

procedure TTrackerGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  MouseButtonDown := False;
end;

procedure TTrackerGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  Writeln(Cursor.SelectedPart, ' ', High(TCellPart), ' ', Low(TCellPart));
  case Key of
    VK_UP: Cursor.Y -= 1;
    VK_DOWN: Cursor.Y += 1;
    VK_LEFT: begin
      if ssCtrl in Shift then
        Cursor.X -= 1
      else if Cursor.SelectedPart > Low(TCellPart) then begin
        Cursor.SelectedPart := Pred(Cursor.SelectedPart);
      end;
    end;
    VK_RIGHT: begin
      if ssCtrl in Shift then
        Cursor.X += 1
      else if Cursor.SelectedPart < High(TCellPart) then begin
        Cursor.SelectedPart := Succ(Cursor.SelectedPart);
      end;
    end;
    else begin
      with Cells[Cursor.X, Cursor.Y] do begin
        Instrument := Random(20);
        EffectCode := Random($F);
        EffectParams := Random($FF);
        Note := NoteMap.Keys[Random(NoteMap.Count)];
      end;
    end;
  end;

  if not (ssShift in Shift) then
    Other := Cursor;
  Selecting := ssShift in Shift;

  ClampCursors;

  Invalidate
end;

procedure TTrackerGrid.DoPaste(var Msg: TLMessage);
begin
end;

procedure TTrackerGrid.DoCopy(var Msg: TLMessage);
begin
end;

procedure TTrackerGrid.DoCut(var Msg: TLMessage);
begin
end;

procedure TTrackerGrid.RenderSelectedArea;
var
  R: TRect;
begin
    R := SelectionsToRect(Cursor, Other);

    with Cursor do begin;
      BitBlt(
        Canvas.Handle,
        R.Left,
        R.Top,
        R.Width,
        R.Height,
        Canvas.Handle,
        R.Left,
        R.Top,
        DSTINVERT);
    end;
end;

procedure TTrackerGrid.ClampCursors;
begin
  Cursor.Y := Min(63, Max(0, Cursor.Y));
  Cursor.X := Min(3, Max(0, Cursor.X));
  Other.Y := Min(63, Max(0, Other.Y));
  Other.X := Min(3, Max(0, Other.X));
end;

procedure TTrackerGrid.RenderRow(Row: Integer);
var
  I: Integer;
begin
  with Canvas do begin
    Brush.Style := bsClear;
    Pen.Color := RGBToColor(58, 52, 39);

    for I := 0 to 4 do
      RenderCell(Cells[I, Row]);
  end;
end;

procedure TTrackerGrid.RenderCell(Cell: TCell);
var
  NoteString: ShortString;
begin
  with Canvas do begin
    if NoteMap.TryGetData(Cell.Note, NoteString) then begin
      Font.Color := clNote;
      TextOut(PenPos.X, PenPos.Y, NoteString);
    end
    else begin
      Font.Color := clGray;
      TextOut(PenPos.X, PenPos.Y, '...');
    end;

    TextOut(PenPos.X, PenPos.Y, ' ');

    if Cell.Instrument <> 0 then begin
      Font.Color := clInstrument;
      TextOut(PenPos.X, PenPos.Y, FormatFloat('00', Cell.Instrument));
    end
    else begin
      Font.Color := clGray;
      TextOut(PenPos.X, PenPos.Y, '..');
    end;

    //Font.Color := clDark; //clGreen;
    Font.Color := clGray;
    TextOut(PenPos.X, PenPos.Y, '...');

    if (Cell.EffectCode <> 0) or (Cell.EffectParams <> 0) then begin
      Font.Color := GetEffectColor(Cell.EffectCode);
      TextOut(
        PenPos.X,
        PenPos.Y,
        IntToHex(Cell.EffectCode, 1) + IntToHex(Cell.EffectParams, 2)
      );
    end
    else begin
      Font.Color := clGray;
      TextOut(PenPos.X, PenPos.Y, '...');
    end;

    TextOut(PenPos.X, PenPos.Y, ' ');
  end;
end;

function TTrackerGrid.GetEffectColor(EffectCode: Integer): TColor;
begin
  Result := clBlack;
  case EffectCode of
    $0: Result := clFxMisc;
    $1: Result := clFxPitch;
    $2: Result := clFxPitch;
    $3: Result := clFxPitch;
    $4: Result := clFxPitch;
    $5: Result := clFxSong;
    $6: Result := clFxMisc;
    $7: Result := clFxMisc;
    $8: Result := clFxPan;
    $9: Result := clFxSong;
    $A: Result := clFxVolume;
    $B: Result := clFxSong;
    $C: Result := clFxVolume;
    $D: Result := clFxSong;
    $E: Result := clFxMisc;
    $F: Result := clFxSong;
  end;
end;

function TTrackerGrid.SelectionsToRect(S1, S2: TSelectionPos): TRect;
begin
  Result := SelectionToRect(S1);
  Result.Union(SelectionToRect(S2));
end;

function TTrackerGrid.SelectionToRect(Selection: TSelectionPos): TRect;
var
  CharLeft, CharRight: Integer;
begin
  case Selection.SelectedPart of
    cpNote: begin
      CharLeft := 0;
      CharRight := 3;
    end;
    cpInstrument: begin
      CharLeft := 4;
      CharRight := 6
    end;
    cpVolume: begin
      CharLeft := 6;
      CharRight := 9;
    end;
    cpEffect: begin
      CharLeft := 9;
      CharRight := 12;
    end;
  end;

  Result.Left := (Selection.X*ColumnWidth) + (CharLeft*CharWidth);
  Result.Right := (Selection.X*ColumnWidth) + (CharRight*CharWidth);
  Result.Top := (Selection.Y*RowHeight);
  Result.Bottom := (Selection.Y*RowHeight) + RowHeight;
end;

function TTrackerGrid.MousePosToSelection(X, Y: Integer): TSelectionPos;
begin
  Result.X := Trunc((X/Width)*NumColumns);
  Result.Y := Trunc((Y/Height)*NumRows);

  X := (min(X, width-1) mod ColumnWidth);
  X := Trunc((X/ColumnWidth)*13);
  case X of
    0..3: Result.SelectedPart := cpNote;
    4..6: Result.SelectedPart := cpInstrument;
    7..9: Result.SelectedPart := cpVolume;
    10..12: Result.SelectedPart := cpEffect;
  end;
end;

end.


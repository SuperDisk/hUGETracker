unit TrackerGrid;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, Graphics, Constants, LCLType, math, contnrs,
  LCLIntf, LMessages, HugeDatatypes, ClipboardUtils;

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

  clHighlighted = TColor($7A99A9);
  clSelected = TColor($9EB4C0);

  NUM_COLUMNS = 4;
  NUM_ROWS = 64;

type

  { TSavedPattern }

  TSavedPattern = object
    PatternNumber: Integer;
    Pattern: TPattern;
  end;

  TUndoRedoAction = array[0..3] of TSavedPattern;
  TUndoRedoStack = TObjectStack;

  { TTrackerGrid }

  TTrackerGrid = class(TCustomControl)
    constructor Create(AOwner: TComponent; Parent: TWinControl);

    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  private
    procedure PerformPaste(Paste: TSelection);
    procedure PerformCopy;
    procedure DoRepeatPaste;
    procedure DoPaste(var Msg: TLMessage); message LM_PASTE;
    procedure DoCopy(var Msg: TLMessage); message LM_COPY;
    procedure DoCut(var Msg: TLMessage); message LM_CUT;
    procedure SaveUndoState;
    procedure DoUndo;
    procedure DoRedo;

    procedure RenderSelectedArea;
    procedure ClampCursors;
    procedure NormalizeCursors;

    procedure EraseSelection;
    procedure InputNote(Key: Word);
    procedure InputInstrument(Key: Word);
    procedure InputVolume(Key: Word);
    procedure InputEffectCode(Key: Word);
    procedure InputEffectParams(Key: Word);

    procedure RenderRow(Row: Integer);
    procedure RenderCell(const Cell: TCell);

    procedure SetHighlightedRow(Row: Integer);
    procedure SetSelectionGridRect(R: TRect);
    function GetSelectionGridRect: TRect;
    function GetEffectColor(EffectCode: Integer): TColor;
    function SelectionsToRect(S1, S2: TSelectionPos): TRect;
    function SelectionToRect(Selection: TSelectionPos): TRect;
    function MousePosToSelection(X, Y: Integer): TSelectionPos;
    function KeycodeToHexNumber(Key: Word; out Num: Nibble): Boolean; overload;
    function KeycodeToHexNumber(Key: Word; out Num: Integer): Boolean; overload;
  private
    Patterns: array[0..3] of PPattern;

    CharHeight: Integer;
    CharWidth: Integer;

    MouseButtonDown: Boolean;
    Selecting: Boolean;

    Performed, Recall: TUndoRedoStack;

    DigitInputting: Boolean;

    FHighlightedRow: Integer;
  public
    Cursor, Other: TSelectionPos;
    ColumnWidth, RowHeight: Integer;

    property HighlightedRow: Integer read FHighlightedRow write SetHighlightedRow;
    property SelectionGridRect: TRect read GetSelectionGridRect write SetSelectionGridRect;
    procedure LoadPattern(Idx: Integer; Pat: PPattern);

    function GetAt(SelectionPos: TSelectionPos): Integer;
    procedure SetAt(SelectionPos: TSelectionPos; Value: Integer);
    procedure ClearAt(SelectionPos: TSelectionPos);
  end;

implementation

{ TTrackerGrid }

constructor TTrackerGrid.Create(
  AOwner: TComponent;
  Parent: TWinControl);
begin
  inherited Create(AOwner);

  Performed := TUndoRedoStack.Create;
  Recall := TUndoRedoStack.Create;

  DoubleBuffered := True;
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

    for I := 0 to High(TPattern) do begin
      if (I mod 4) = 0 then
        Brush.Color := RGBToColor(216, 209, 195);
      if (I mod 16) = 0 then
        Brush.Color := RGBToColor(206, 197, 181);
      if I = Cursor.Y then
        Brush.Color := clSelected;
      if I = FHighlightedRow then
        Brush.Color := clHighlighted;

      FillRect(0, I*RowHeight, Width, (I+1)*RowHeight);
      MoveTo(0, I*RowHeight);
      RenderRow(I);
    end;
  end;

  // Draw borders between columns
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

  if Button = mbLeft then
    MouseButtonDown := True;
  Selecting := False;
  DigitInputting := False;

  if not (csDesigning in ComponentState) and CanFocus then
    SetFocus;

  Cursor := MousePosToSelection(X, Y);
  Other := Cursor;
  ClampCursors;

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
  DigitInputting := False;
end;

procedure TTrackerGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  case Key of
    VK_V: if ssShift in Shift then DoRepeatPaste;
    VK_L: begin
      if ssCtrl in Shift then begin
        Cursor.Y := 0;
        Other.Y := High(TPattern);
        Cursor.SelectedPart := Low(TCellPart);
        Other.SelectedPart := High(TCellPart);
      end;
    end;
    VK_DELETE: begin
      EraseSelection;
    end;
    VK_UP: begin
      Cursor.Y -= 1;
      DigitInputting := False;
    end;
    VK_DOWN: begin
      Cursor.Y += 1;
      DigitInputting := False;
    end;
    VK_LEFT: begin
      if ssCtrl in Shift then
        Cursor.X -= 1
      else if Cursor.SelectedPart > Low(TCellPart) then begin
        Cursor.SelectedPart := Pred(Cursor.SelectedPart);
      end;
      DigitInputting := False;
    end;
    VK_RIGHT: begin
      if ssCtrl in Shift then
        Cursor.X += 1
      else if Cursor.SelectedPart < High(TCellPart) then begin
        Cursor.SelectedPart := Succ(Cursor.SelectedPart);
      end;
      DigitInputting := False;
    end;
    else begin
      if not (ssCtrl in Shift) then
        case Cursor.SelectedPart of
          cpNote:         InputNote(Key);
          cpInstrument:   InputInstrument(Key);
          cpVolume:       InputVolume(Key);
          cpEffectCode:   InputEffectCode(Key);
          cpEffectParams: InputEffectParams(Key);
        end;
    end;
  end;

  if not (ssShift in Shift) and not(ssCtrl in Shift) then
    Other := Cursor;
  Selecting := ssShift in Shift;

  ClampCursors;

  Invalidate
end;

procedure TTrackerGrid.PerformPaste(Paste: TSelection);
var
  X, Y: Integer;
begin
  try
    for Y := 0 to High(Paste) do begin
      if Cursor.Y+Y > High(TPattern) then Break;
      for X := 0 to High(Paste[Y]) do begin
        if Cursor.X+X > High(Patterns) then Break;
        Patterns[Cursor.X + X]^[Cursor.Y + Y] := Paste[Y, X];
      end;
    end;
    Other.X := Cursor.X + X;
    Other.Y := Cursor.Y + Y;
    Cursor.SelectedPart := Low(TCellPart);
    Other.SelectedPart := High(TCellPart);
  except
    on E: Exception do
      WriteLn(StdErr, 'Clipboard did not contain valid note data!');
  end;

  Invalidate;
end;

procedure TTrackerGrid.PerformCopy;
begin

end;

procedure TTrackerGrid.DoRepeatPaste;
var
  Selection: TSelection;
  I: Integer;
begin
  Selection := GetPastedCells;
  I := Cursor.Y;
  while I <= High(TPattern) do begin
    Cursor.Y := I;
    PerformPaste(Selection);
    Inc(I, High(Selection)+1);
  end;
end;

procedure TTrackerGrid.DoPaste(var Msg: TLMessage);
begin
  PerformPaste(GetPastedCells);
end;

procedure TTrackerGrid.DoCopy(var Msg: TLMessage);
var
  Selection: TSelection;
  R, C: Integer;
  Rect: TRect;
begin
  Rect := SelectionGridRect;
  SetLength(Selection, Rect.Height+1);
  for R := 0 to Rect.Height do begin
    SetLength(Selection[R], Rect.Width+1);
    for C := 0 to Rect.Width do begin
      Selection[R, C] := Patterns[Rect.Left+C]^[Rect.Top+R];
    end;
  end;

  CopyCells(Selection);
end;

procedure TTrackerGrid.DoCut(var Msg: TLMessage);
begin
  DoCopy(Msg);
  EraseSelection;
end;

procedure TTrackerGrid.SaveUndoState;
begin

end;

procedure TTrackerGrid.DoUndo;
begin

end;

procedure TTrackerGrid.DoRedo;
begin

end;

procedure TTrackerGrid.RenderSelectedArea;
var
  R: TRect;
begin
    R := SelectionsToRect(Cursor, Other);

    with Cursor do
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

procedure TTrackerGrid.ClampCursors;
begin
  Cursor.Y := Min(High(TPattern), Max(Low(TPattern), Cursor.Y));
  Cursor.X := Min(High(Patterns), Max(Low(Patterns), Cursor.X));
  Other.Y := Min(High(TPattern), Max(Low(TPattern), Other.Y));
  Other.X := Min(High(Patterns), Max(Low(Patterns), Other.X));
end;

procedure TTrackerGrid.NormalizeCursors;
var
  TempI: Integer;
  TempS: TCellPart;
begin
  // Normalize the cursor positions such that cursor is always in the top left
  if (Cursor > Other) then begin
    TempI := Other.X;
    Other.X := Cursor.X;
    Cursor.X := TempI;

    TempS := Other.SelectedPart;
    Other.SelectedPart := Cursor.SelectedPart;
    Cursor.SelectedPart := TempS;
  end;
  if (Cursor.Y > Other.Y) then begin
    TempI := Other.Y;
    Other.Y := Cursor.Y;
    Cursor.Y := TempI;
  end;
end;

procedure TTrackerGrid.EraseSelection;
var
  X: TSelectionPos;
  R: Integer;
begin
  NormalizeCursors;

  for R := Cursor.Y to Other.Y do begin
    X := Cursor;
    X.Y := R;
    while X <= Other do begin
      ClearAt(X);
      IncSelectionPos(X);
    end;
  end;
end;

procedure TTrackerGrid.InputNote(Key: Word);
var
  Temp: Integer;
begin
  with Patterns[Cursor.X]^[Cursor.Y] do
    if Key = VK_DELETE then
      Note := NO_NOTE
    else begin
      Keybindings.TryGetData(Key, Temp);
      if Temp <> 0 then Note := Temp;
    end;
end;

procedure TTrackerGrid.InputInstrument(Key: Word);
var
  Temp: Nibble;
begin
  with Patterns[Cursor.X]^[Cursor.Y] do
    if Key = VK_DELETE then Instrument := 0
    else if KeycodeToHexNumber(Key, Temp) and (Temp <= 9) then
      Instrument := ((Instrument mod 10) * 10) + Temp;
end;

procedure TTrackerGrid.InputVolume(Key: Word);
begin

end;

procedure TTrackerGrid.InputEffectCode(Key: Word);
begin
  with Patterns[Cursor.X]^[Cursor.Y] do
    if Key = VK_DELETE then begin
      EffectCode := 0;
      EffectParams.Value := 0;
    end
    else KeycodeToHexNumber(Key, EffectCode);
end;

procedure TTrackerGrid.InputEffectParams(Key: Word);
var
  Temp: Nibble;
begin
  with Patterns[Cursor.X]^[Cursor.Y] do
    if Key = VK_DELETE then begin
      EffectCode := 0;
      EffectParams.Value := 0;
    end
    else if not DigitInputting then begin
      if KeycodeToHexNumber(Key, Temp) then begin
        EffectParams.Param2 := EffectParams.Param1;
        EffectParams.Param1 := Temp;
        DigitInputting := True;
      end;
    end
    else if KeycodeToHexNumber(Key, Temp) then begin
      EffectParams.Param2 := Temp;
      DigitInputting := False;
    end;
end;

procedure TTrackerGrid.RenderRow(Row: Integer);
var
  I: Integer;
begin
  with Canvas do begin
    Brush.Style := bsClear;
    Pen.Color := RGBToColor(58, 52, 39);

    for I := 0 to High(Patterns) do
      RenderCell(Patterns[I]^[Row]);
  end;
end;

procedure TTrackerGrid.RenderCell(const Cell: TCell);
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

    if (Cell.EffectCode <> 0) or (Cell.EffectParams.Value <> 0) then begin
      Font.Color := GetEffectColor(Cell.EffectCode);
      TextOut(
        PenPos.X,
        PenPos.Y,
        IntToHex(Cell.EffectCode, 1)
          + IntToHex(Cell.EffectParams.Param1, 1)
          + IntToHex(Cell.EffectParams.Param2, 1)
      );
    end
    else begin
      Font.Color := clGray;
      TextOut(PenPos.X, PenPos.Y, '...');
    end;

    TextOut(PenPos.X, PenPos.Y, ' ');
  end;
end;

procedure TTrackerGrid.SetHighlightedRow(Row: Integer);
begin
  FHighlightedRow := Row;
  if FHighlightedRow >= 0 then Invalidate;
end;

procedure TTrackerGrid.SetSelectionGridRect(R: TRect);
begin
  Cursor.X := R.Left;
  Other.X := R.Right;
  Cursor.Y := R.Top;
  Other.Y := R.Bottom;
end;

function TTrackerGrid.GetSelectionGridRect: TRect;
begin
  Result := TRect.Create(
    TPoint.Create(Cursor.X, Cursor.Y),
    TPoint.Create(Other.X, Other.Y),
    True
  );
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
    cpEffectCode: begin
      CharLeft := 9;
      CharRight := 10;
    end;
    cpEffectParams: begin
      CharLeft := 10;
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
  Result.X := Trunc((X/Width)*NUM_COLUMNS);
  Result.Y := Trunc((Y/Height)*NUM_ROWS);

  X := (min(X, width-1) mod ColumnWidth);
  X := Trunc((X/ColumnWidth)*13);
  case X of
    0..3: Result.SelectedPart := cpNote;
    4..5: Result.SelectedPart := cpInstrument;
    6..8: Result.SelectedPart := cpVolume;
    9:   Result.SelectedPart := cpEffectCode;
    10..12: Result.SelectedPart := cpEffectParams;
  end;
end;

function TTrackerGrid.KeycodeToHexNumber(Key: Word; out Num: Nibble): Boolean;
begin
  Result := True;

  case Key of
    VK_0: Num := $0;
    VK_1: Num := $1;
    VK_2: Num := $2;
    VK_3: Num := $3;
    VK_4: Num := $4;
    VK_5: Num := $5;
    VK_6: Num := $6;
    VK_7: Num := $7;
    VK_8: Num := $8;
    VK_9: Num := $9;
    VK_A: Num := $A;
    VK_B: Num := $B;
    VK_C: Num := $C;
    VK_D: Num := $D;
    VK_E: Num := $E;
    VK_F: Num := $F;
    else Result := False;
  end;
end;

function TTrackerGrid.KeycodeToHexNumber(Key: Word; out Num: Integer): Boolean;
var
  X: Nibble;
begin
  Result := KeycodeToHexNumber(Key, X);
  Num := X;
end;

function TTrackerGrid.GetAt(SelectionPos: TSelectionPos): Integer;
begin
  with Patterns[SelectionPos.X]^[SelectionPos.Y] do
    case SelectionPos.SelectedPart of
      cpNote: Result := Note;
      cpInstrument: Result := Instrument;
      cpVolume:;
      cpEffectCode: Result := EffectCode;
      cpEffectParams: Result := EffectParams.Value;
    end;
end;

procedure TTrackerGrid.SetAt(SelectionPos: TSelectionPos; Value: Integer);
begin
  with Patterns[SelectionPos.X]^[SelectionPos.Y] do
    case SelectionPos.SelectedPart of
      cpNote: Note := Value;
      cpInstrument: Instrument := Value;
      cpVolume:;
      cpEffectCode: EffectCode := Value;
      cpEffectParams: EffectParams.Value := Value;
    end;
end;

procedure TTrackerGrid.ClearAt(SelectionPos: TSelectionPos);
begin
  with Patterns[SelectionPos.X]^[SelectionPos.Y] do
    case SelectionPos.SelectedPart of
      cpNote: Note := NO_NOTE;
      cpInstrument: Instrument := 0;
      cpVolume:;
      cpEffectCode: EffectCode := 0;
      cpEffectParams: EffectParams.Value := 0;
    end;
end;

procedure TTrackerGrid.LoadPattern(Idx: Integer; Pat: PPattern);
begin
  Patterns[Idx] := Pat;
  Invalidate;
end;

end.

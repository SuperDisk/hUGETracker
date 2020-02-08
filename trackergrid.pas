unit TrackerGrid;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, Graphics, Constants, LCLType, math, LCLIntf,
  LMessages, HugeDatatypes, ClipboardUtils, gdeque, gstack, utils, effecteditor;

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
  UNDO_STACK_SIZE = 100;

type
  TPatternGrid = array[0..3] of PPattern;

  { TSavedPattern }

  TSavedPattern = record
    PatternNumber: Integer;
    Pattern: TPattern;
  end;

  TUndoRedoAction = array[0..3] of TSavedPattern;
  TUndoDeque = TDeque<TUndoRedoAction>;
  TRedoStack = TStack<TUndoRedoAction>;

  { TSelectionEnumerator }

  TSelectionEnumerator = record
  private
    function GetCurrent: TSelectionRow;
  public
    Grid: TPatternGrid;
    Row: Integer;
    Cursor, Other: TSelectionPos;

    constructor Create(Grid: TPatternGrid; Cursor, Other: TSelectionPos);
    function MoveNext: Boolean;
    property Current: TSelectionRow read GetCurrent;
    function GetEnumerator: TSelectionEnumerator;
  end;

  { TTrackerGrid }

  TTrackerGrid = class(TCustomControl)
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  private
    procedure PerformPaste(Paste: TSelection);
    procedure PerformCopy;
    procedure DoPaste(var Msg: TLMessage); message LM_PASTE;
    procedure DoCopy(var Msg: TLMessage); message LM_COPY;
    procedure DoCut(var Msg: TLMessage); message LM_CUT;
    procedure SaveUndoState;

    procedure RenderSelectedArea;
    procedure ClampCursors;
    procedure NormalizeCursors;

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

    procedure ChangeFontSize;
  private
    PatternMap: TPatternMap;
    Patterns: TPatternGrid;
    PatternNumbers: array[0..3] of Integer;

    CharHeight: Integer;
    CharWidth: Integer;

    MouseButtonDown: Boolean;
    Selecting: Boolean;

    Performed: TUndoDeque;
    Recall: TRedoStack;

    FHighlightedRow: Integer;
    FFontSize: Integer;
    procedure SetFontSize(AValue: Integer);
  public
    Cursor, Other: TSelectionPos;
    ColumnWidth, RowHeight: Integer;

    SelectedInstrument, SelectedOctave, Step: Integer;

    property HighlightedRow: Integer read FHighlightedRow write SetHighlightedRow;
    property SelectionGridRect: TRect read GetSelectionGridRect write SetSelectionGridRect;
    property FontSize: Integer read FFontSize write SetFontSize;
    procedure LoadPattern(Idx: Integer; PatternNumber: Integer);

    function GetAt(SelectionPos: TSelectionPos): Integer;
    procedure SetAt(SelectionPos: TSelectionPos; Value: Integer);
    procedure ClearAt(SelectionPos: TSelectionPos);

    procedure SelectAll;
    procedure SelectColumn;
    procedure EraseSelection;
    procedure DoUndo;
    procedure DoRedo;
    procedure DoRepeatPaste;
    procedure TransposeSelection(Semitones: Integer);
    procedure OpenEffectEditor;

    constructor Create(
      AOwner: TComponent;
      Parent: TWinControl;
      PatternMap: TPatternMap); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TSelectionEnumerator }

constructor TSelectionEnumerator.Create(Grid: TPatternGrid; Cursor,
  Other: TSelectionPos);
begin
  Self.Grid := Grid;
  Self.Row := Cursor.Y-1;
  Self.Cursor := Cursor;
  Self.Other := Other;
end;

function TSelectionEnumerator.GetCurrent: TSelectionRow;
var
  X, I: Integer;
begin
  // Very ugly function
  SetLength(Result, (Other.X-Cursor.X)+1);

  Result[0].Cell := Grid[Cursor.X]^[Row];
  if Cursor.X <> Other.X then
    Result[0].Parts := [Cursor.SelectedPart..High(TCellPart)]
  else
    Result[0].Parts := [Cursor.SelectedPart..Other.SelectedPart];

  I := 1;
  for X := Cursor.X+1 to Other.X - 1 do begin
    Result[I].Cell := Grid[X]^[Row];
    Result[I].Parts := [Low(TCellPart)..High(TCellPart)];
    Inc(I);
  end;

  if Cursor.X <> Other.X then begin
    Result[High(Result)].Cell := Grid[Other.X]^[Row];
    Result[High(Result)].Parts := [Low(TCellPart)..Other.SelectedPart];
  end;
end;

function TSelectionEnumerator.MoveNext: Boolean;
begin
  Result := True;
  Inc(Row);
  if Row > Other.Y then Result := False;
end;

function TSelectionEnumerator.GetEnumerator: TSelectionEnumerator;
begin
  Result := Self;
end;

{ TTrackerGrid }

constructor TTrackerGrid.Create(
  AOwner: TComponent;
  Parent: TWinControl;
  PatternMap: TPatternMap);
begin
  inherited Create(AOwner);

  FFontSize := 12;

  Self.PatternMap := PatternMap;

  Performed := TUndoDeque.Create;
  Recall := TRedoStack.Create;

  DoubleBuffered := True;
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csDoubleClicks];
  Self.Parent := Parent;

  ChangeFontSize;

  SelectedInstrument := 0;
  SelectedOctave := 0;
  Step := 0;
end;

destructor TTrackerGrid.Destroy;
begin
  inherited Destroy;

  Performed.Free;
  Recall.Free;
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
  if (Button = mbRight) and Selecting then Exit;

  if Button = mbLeft then
    MouseButtonDown := True;
  Selecting := False;

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

  NormalizeCursors;

  MouseButtonDown := False;
end;

procedure TTrackerGrid.DblClick;
begin
  OpenEffectEditor;

  NormalizeCursors;
  MouseButtonDown := False;
  Invalidate;
end;

procedure TTrackerGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if Key in [VK_CONTROL, VK_SHIFT] then Exit;
  if Key in [VK_DELETE] then Exit;

  case Key of
    VK_UP: Dec(Cursor.Y);
    VK_DOWN: Inc(Cursor.Y);
    VK_LEFT: DecSelectionPos(Cursor);
    VK_RIGHT: IncSelectionPos(Cursor);
    else
      if (not (ssCtrl in Shift)) and (not (ssShift in Shift)) then
        case Cursor.SelectedPart of
          cpNote:         InputNote(Key);
          cpInstrument:   InputInstrument(Key);
          cpVolume:       InputVolume(Key);
          cpEffectCode:   InputEffectCode(Key);
          cpEffectParams: InputEffectParams(Key);
        end;
  end;

  if (not (ssCtrl in Shift)) and (not (ssShift in Shift)) then
    Other := Cursor;
  Selecting := ssShift in Shift;

  ClampCursors;

  Invalidate
end;

procedure TTrackerGrid.PerformPaste(Paste: TSelection);
procedure OverlayCell(var Cell1: TCell; const Cell2: TSelectedCell);
begin
  if cpNote in Cell2.Parts then Cell1.Note := Cell2.Cell.Note;
  if cpInstrument in Cell2.Parts then Cell1.Instrument := Cell2.Cell.Instrument;
  if cpEffectCode in Cell2.Parts then Cell1.EffectCode:=Cell2.Cell.EffectCode;
  if cpEffectParams in Cell2.Parts then
    Cell1.EffectParams.Value := Cell2.Cell.EffectParams.Value;
end;
var
  X, Y: Integer;
begin
  try
    for Y := 0 to High(Paste) do begin
      if Cursor.Y+Y > High(TPattern) then Break;
      for X := 0 to High(Paste[Y]) do begin
        if Cursor.X+X > High(Patterns) then Break;
        OverlayCell(Patterns[Cursor.X + X]^[Cursor.Y + Y], Paste[Y, X]);
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

  Invalidate;
  SaveUndoState;
end;

procedure TTrackerGrid.DoPaste(var Msg: TLMessage);
begin
  PerformPaste(GetPastedCells);

  Invalidate;
  SaveUndoState;
end;

procedure TTrackerGrid.DoCopy(var Msg: TLMessage);
var
  Selection: TSelection;
  R: Integer;
  Rect: TRect;
  SelRow: TSelectionRow;
begin
  NormalizeCursors;

  Rect := SelectionGridRect;
  SetLength(Selection, Rect.Height+1);
  R := 0;
  for SelRow in TSelectionEnumerator.Create(Patterns, Cursor, Other) do begin
    Selection[R] := SelRow;
    Inc(R)
  end;

  CopyCells(Selection);
end;

procedure TTrackerGrid.DoCut(var Msg: TLMessage);
begin
  DoCopy(Msg);
  EraseSelection;
end;

procedure TTrackerGrid.SaveUndoState;
var
  Actn: TUndoRedoAction;
  I: Integer;
begin
  // First, clear out the recall stack
  while not Recall.IsEmpty do
    Recall.Pop;

  // Create the state we need to save
  Actn := Default(TUndoRedoAction);
  for I := Low(Patterns) to High(Patterns) do
    with Actn[I] do begin
      Pattern := Patterns[I]^;
      PatternNumber := PatternNumbers[I];
    end;

  // Push it into the performed queue
  Performed.PushFront(Actn);
  while Performed.Size > UNDO_STACK_SIZE do
    Performed.PopBack;
end;

procedure TTrackerGrid.DoUndo;
var
  OldState, NewState: TUndoRedoAction;
  I: Integer;
begin
  if Performed.IsEmpty then Exit;

  OldState := Performed.Front;
  Performed.PopFront;
  NewState := Performed.Front;

  Recall.Push(OldState);

  for I := Low(NewState) to High(NewState) do begin
    LoadPattern(I, NewState[I].PatternNumber);
    Patterns[I]^ := NewState[I].Pattern;
  end;

  Invalidate;
end;

procedure TTrackerGrid.DoRedo;
var
  State: TUndoRedoAction;
  I: Integer;
begin
  if Recall.IsEmpty then Exit;

  State := Recall.Top;
  Recall.Pop;

  Performed.PushFront(State);

  for I := Low(State) to High(State) do begin
    LoadPattern(I, State[I].PatternNumber);
    Patterns[I]^ := State[I].Pattern;
  end;

  Invalidate;
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
  Cursor.Y := EnsureRange(Cursor.Y, Low(TPattern), High(TPattern));
  Cursor.X := EnsureRange(Cursor.X, Low(TPattern), High(TPattern));
  Other.Y := EnsureRange(Other.Y, Low(TPattern), High(TPattern));
  Other.X := EnsureRange(Other.X, Low(TPattern), High(TPattern));
end;

procedure TTrackerGrid.NormalizeCursors;
var
  TempI: Integer;
  TempS: TCellPart;
begin
  ClampCursors;

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

procedure TTrackerGrid.TransposeSelection(Semitones: Integer);
var
  C, R: Integer;
begin
  NormalizeCursors;

  for R := Cursor.Y to Other.Y do
    for C := Cursor.X to Other.X do
      if Patterns[C]^[R].Note <> NO_NOTE then
        Patterns[C]^[R].Note :=
          EnsureRange(Patterns[C]^[R].Note + Semitones, LOWEST_NOTE, HIGHEST_NOTE);

  Invalidate;
  SaveUndoState;
end;

procedure TTrackerGrid.OpenEffectEditor;
begin
  inherited DblClick;

  frmEffectEditor.Cell := @Patterns[Cursor.X]^[Cursor.Y];
  frmEffectEditor.ShowModal;
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

  Invalidate;
  SaveUndoState;
end;

procedure TTrackerGrid.InputNote(Key: Word);
var
  Temp: Integer;
begin
  Temp := -1;

  with Patterns[Cursor.X]^[Cursor.Y] do
    if Key = VK_DELETE then
      Note := NO_NOTE
    else begin
      if Keybindings.TryGetData(Key, Temp) then begin
        Note := Min(HIGHEST_NOTE, Temp+(SelectedOctave*12));
        Instrument := SelectedInstrument;

        Inc(Cursor.Y, Step);
        ClampCursors;

        {if Instrument > 0 then
          SendMessage(frmTracker.Handle, LM_PREVIEW_NOTE, Note, Instrument);}
      end;
    end;

  Invalidate;
  SaveUndoState;
end;

procedure TTrackerGrid.InputInstrument(Key: Word);
var
  Temp: Nibble;
begin
  with Patterns[Cursor.X]^[Cursor.Y] do begin
    if Key = VK_DELETE then Instrument := 0
    else if KeycodeToHexNumber(Key, Temp) and InRange(Temp, 0, 9) then
      Instrument := ((Instrument mod 10) * 10) + Temp;

    Instrument := EnsureRange(Instrument, 0, 15);
  end;

  Invalidate;
  SaveUndoState;
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

  Invalidate;
  SaveUndoState;
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
    else if KeycodeToHexNumber(Key, Temp) then
      EffectParams.Value := ((EffectParams.Value mod $10) * $10) + Temp;

  Invalidate;
  SaveUndoState;
end;

procedure TTrackerGrid.RenderRow(Row: Integer);
var
  I: Integer;
begin
  with Canvas do begin
    Brush.Style := bsClear;
    Pen.Color := RGBToColor(58, 52, 39);

    for I := 0 to High(Patterns) do
      if Assigned(Patterns[I]) then
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
//var
  //R: TRect;
  //OldRow: Integer;
begin
  //OldRow := FHighlightedRow;
  FHighlightedRow := Row;

  Invalidate

  // An optimization to redraw the pattern faster. This function gets called
  // a lot (every time the FD callback happens) so it needs to be fast. It can
  // sort of lag out the main thread if it just does a full invalidate.

  // For some reason it's flakey whether or not it works, so we're just doing
  // a full invalidate right now...

  {R.Left:=0;
  R.Width:=ColumnWidth*4;
  R.Height:=RowHeight*2;

  if InRange(OldRow, Low(TPattern), High(TPattern)) then begin
    RenderRow(OldRow);
    R.Top:=(OldRow-1)*RowHeight;
    InvalidateRect(Self.Handle, @R, True);
  end;

  R.Left:=0;
  R.Width:=ColumnWidth*4;
  R.Height:=RowHeight*2;

  if InRange(Row, Low(TPattern), High(TPattern)) then begin
    RenderRow(Row);
    R.Top:=(Row-1)*RowHeight;
    InvalidateRect(Self.Handle, @R, True);
  end;}
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
var
  OrigX: Integer;
begin
  OrigX := X;
  X := EnsureRange(Width-1, 0, X);
  Y := EnsureRange(Height-1, 0, Y);

  Result.X := Trunc((X/Width)*NUM_COLUMNS);
  Result.Y := Trunc((Y/Height)*NUM_ROWS);

  if OrigX <= 0 then
    Result.SelectedPart := cpNote
  else begin
    X := X mod ColumnWidth;
    X := Trunc((X/ColumnWidth)*13);
    case X of
      0..3: Result.SelectedPart := cpNote;
      4..5: Result.SelectedPart := cpInstrument;
      6..8: Result.SelectedPart := cpVolume;
      9:   Result.SelectedPart := cpEffectCode;
      10..12: Result.SelectedPart := cpEffectParams;
    end;
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

procedure TTrackerGrid.ChangeFontSize;
begin
  // Kind of a hack
  with Canvas.Font do begin
    Name := 'PixeliteTTF';
    Size := FontSize;
    ColumnWidth := GetTextWidth('C-5 01v64C01 ');
    RowHeight := GetTextHeight('C-5 01v64C01 ');

    CharHeight := RowHeight;
    CharWidth := GetTextWidth('C');
  end;

  Width := ColumnWidth*4;
  Height := RowHeight*64;
end;

procedure TTrackerGrid.SetFontSize(AValue: Integer);
begin
  if FFontSize=AValue then Exit;
  FFontSize:=AValue;

  ChangeFontSize;
  Invalidate;
end;

function TTrackerGrid.GetAt(SelectionPos: TSelectionPos): Integer;
begin
  Result := 0;

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

procedure TTrackerGrid.SelectAll;
begin
  Cursor.X := 0;
  Cursor.Y := 0;
  Cursor.SelectedPart := cpNote;
  Other.X := High(Patterns);
  Other.Y := High(TPattern);
  Other.SelectedPart := cpEffectParams;

  Invalidate;
end;

procedure TTrackerGrid.SelectColumn;
begin
  Cursor.Y := 0;
  Other.Y := High(TPattern);
  Cursor.SelectedPart := Low(TCellPart);
  Other.SelectedPart := High(TCellPart);

  Invalidate;
end;

procedure TTrackerGrid.LoadPattern(Idx: Integer; PatternNumber: Integer);
begin
  Patterns[Idx] := PatternMap.GetOrCreateNew(PatternNumber);
  PatternNumbers[Idx] := PatternNumber; // Ugh
  Invalidate;
end;

end.


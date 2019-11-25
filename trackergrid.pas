unit TrackerGrid;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, Graphics, Constants, LCLType, math,
  LCLIntf, LMessages, ClipboardUtils, HugeDatatypes;

const
  clMisc = clMaroon;
  clPitch = clYellow;
  clVolume = clGreen;
  clPan = clTeal;
  clSong = clRed;

type
  TCellPart = (cpNote, cpInstrument, cpVolume, cpEffect);

  TSelectionPos = record
    X, Y: Integer;
    SelectedPart: set of TCellPart
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

  public
    Cursor, Other: TSelectionPos;
    SelectedRow: Integer;
  end;

const
  NotePosition = 0;
  InstrumentPosition = 3;
  VolumePosition = 5;
  EffectPosition = 8;

implementation

{ TTrackerGrid }

constructor TTrackerGrid.Create(AOwner: TComponent; Parent: TWinControl);
var
  X, Y: Integer;
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents,
      csDoubleClicks];
  Self.Parent := Parent;

  // Kind of a hack
  with Canvas.Font do begin
    Name := 'Pixelite';
    Size := 12;
    ColumnWidth := GetTextWidth('C-501v64C01');
    RowHeight := GetTextHeight('C-501v64C01');

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
      if I = SelectedRow then begin
        Brush.Color := RGBToColor(169, 153, 122);
        FillRect(0, I*RowHeight, Width, (I+1)*RowHeight);
      end;

      MoveTo(0, I*RowHeight);
      RenderRow(I);
    end;
  end;

  {Canvas.Pen.Color := RGBToColor(169, 153, 122);
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
  Canvas.Rectangle(R);}

  RenderSelectedArea;
end;

procedure TTrackerGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  SelectedColumn: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  MouseButtonDown := True;
  Selecting := False;

  if not (csDesigning in ComponentState) and CanFocus then
    SetFocus;

  SelectedRow := Round((Y/Height)*63);
  SelectedColumn := Round((X/Width)*4);
  with Cursor do begin
    X := SelectedColumn;
    Y := SelectedRow;
    SelectedPart := [];
  end;

  ClampCursors;
  Other := Cursor;

  Invalidate;
end;

procedure TTrackerGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  SelectedRow, SelectedColumn: Integer;
begin
  inherited MouseMove(Shift, X, Y);

  if MouseButtonDown then begin
    Selecting := True;
    SelectedRow := Round((Y/Height)*63);
    SelectedColumn := Round((X/Width)*4);
    with Other do begin
      X := SelectedColumn;
      Y := SelectedRow;
      SelectedPart := [];
    end;

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

  Selecting := False;

  case Key of
    VK_UP: Cursor.Y -= 1;
    VK_DOWN: Cursor.Y += 1;
    VK_LEFT: Cursor.X -= 1;
    VK_RIGHT: Cursor.X += 1;
    else begin
      with Cells[Cursor.X, Cursor.Y] do begin
        Instrument := Random(20);
        EffectCode := Random($F);
        EffectParams := Random($FF);
        Note := NoteMap.Keys[Random(NoteMap.Count)];
      end;
    end;
  end;

  ClampCursors;
  SelectedRow := Cursor.Y;

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
  if Selecting then begin
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
  end
  else
    with Cursor do
      BitBlt(
        Canvas.Handle,
        Cursor.X*ColumnWidth,
        Cursor.Y*RowHeight,
        ColumnWidth,
        RowHeight,
        Canvas.Handle,
        Cursor.X*ColumnWidth,
        Cursor.Y*RowHeight,
        DSTINVERT);
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
      Font.Color := clBlue;
      TextOut(PenPos.X, PenPos.Y, NoteString);
    end
    else begin
      Font.Color := clGray;
      TextOut(PenPos.X, PenPos.Y, '...');
    end;

    if Cell.Instrument <> 0 then begin
      Font.Color := clTeal;
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
  end;
end;

function TTrackerGrid.GetEffectColor(EffectCode: Integer): TColor;
begin
  Result := clBlack;
  case EffectCode of
    $0: Result := clMisc;
    $1: Result := clPitch;
    $2: Result := clPitch;
    $3: Result := clPitch;
    $4: Result := clPitch;
    $5: Result := clSong;
    $6: Result := clMisc;
    $7: Result := clMisc;
    $8: Result := clPan;
    $9: Result := clSong;
    $A: Result := clVolume;
    $B: Result := clSong;
    $C: Result := clVolume;
    $D: Result := clSong;
    $E: Result := clMisc;
    $F: Result := clSong;
  end;
end;

function TTrackerGrid.SelectionsToRect(S1, S2: TSelectionPos): TRect;
begin
  Result := TRect.Create(S1.X*ColumnWidth, S1.Y*RowHeight, S1.X*ColumnWidth + ColumnWidth, S1.Y*RowHeight + RowHeight);
  Result.Union(TRect.Create(S2.X*ColumnWidth, S2.Y*RowHeight, S2.X*ColumnWidth + ColumnWidth, S2.Y*RowHeight + RowHeight));
end;

end.


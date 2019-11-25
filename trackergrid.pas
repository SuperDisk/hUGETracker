unit TrackerGrid;

{$mode delphi}

interface

uses
  Classes, SysUtils, Controls, Graphics, Constants, LCLType, math,
  LCLIntf, LMessages, ClipboardUtils, Clipbrd, HugeDatatypes;

const
  clMisc = clMaroon;
  clPitch = clYellow;
  clVolume = clGreen;
  clPan = clTeal;
  clSong = clRed;

type
  TCellPart = (cpNote, cpInstrument, cpEffect);

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
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure DoPaste(var Msg: TLMessage); message LM_PASTE;
    procedure DoCopy(var Msg: TLMessage); message LM_COPY;
    procedure DoCut(var Msg: TLMessage); message LM_CUT;

  private
    Cells: array[0..3, 0..63] of TCell;

    ColumnWidth: Integer;
    RowHeight: Integer;
    CharHeight: Integer;
    CharWidth: Integer;

    CursorCell, OtherCell: TCell;

    procedure RenderRow(Row: Integer);
    procedure RenderCell(Cell: TCell);

    function GetEffectColor(EffectCode: Integer): TColor;
    function CellIndexToRect(X, Y: Integer): TRect;

  public
    Cursor, Other: TSelectionPos;
    SelectedRow: Integer;
  end;

implementation

function TTrackerGrid.CellIndexToRect(X, Y: Integer): TRect;
begin
  Result.Left := X*ColumnWidth;
  Result.Top := Y*RowHeight;
  Result.Width := ColumnWidth;
  Result.Height := RowHeight;
end;

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
  P: TRect;
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

  P := CellIndexToRect(Cursor.X, Cursor.Y);
  with Cursor do begin
    BitBlt(
      Canvas.Handle,
      P.Left,
      P.Top,
      P.Width,
      P.Height,
      Canvas.Handle,
      P.Left,
      P.Top,
      DSTINVERT);
  end;
end;

procedure TTrackerGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  SelectedColumn: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if not (csDesigning in ComponentState) and CanFocus then
    SetFocus;

  SelectedRow := Round((Y/Height)*63);
  SelectedColumn := Round((X/Width)*4);
  with Cursor do begin
    X := SelectedColumn;
    Y := SelectedRow;
    SelectedPart := [];
  end;
  CursorCell := Cells[SelectedColumn, SelectedRow];
  Invalidate;
end;

procedure TTrackerGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  case Key of
    VK_UP: SelectedRow -= 1;
    VK_DOWN: SelectedRow += 1;
  end;

  SelectedRow := Min(63, Max(0, SelectedRow));

  Invalidate
end;

procedure TTrackerGrid.DoPaste(var Msg: TLMessage);
var
  C: TCell;
  X: TModplugClipboardCell;
  ClipboardBytes: array of Byte;
begin
  ClipboardBytes := TEncoding.UTF8.GetBytes(Clipboard.AsText);
  Move(ClipboardBytes, X, sizeof(X));
  C := ModplugToHuge(X);
end;

procedure TTrackerGrid.DoCopy(var Msg: TLMessage);
begin
  //writeln('copying');
end;

procedure TTrackerGrid.DoCut(var Msg: TLMessage);
begin
  //writeln('cutting');
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
      TextOut(PenPos.X, PenPos.Y, IntToStr(Cell.Instrument));
    end
    else begin
      Font.Color := clGray;
      TextOut(PenPos.X, PenPos.Y, '..');
    end;

    //Font.Color := clDark; //clGreen;
    Font.Color := clGray;
    TextOut(PenPos.X, PenPos.Y, '...');

    if Cell.EffectCode <> 0 then begin
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

end.


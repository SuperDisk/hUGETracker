unit SFXGrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TrackerGrid, hUGEDatatypes, Math, Graphics, Constants,
  LCLType;

type

  { TSFXGrid }

  TSFXGrid = class(TTrackerGrid)
    procedure Paint; override;
    procedure ChangeFontSize; override;
    function MousePosToSelection(X, Y: Integer): TSelectionPos; override;
    procedure OpenEffectEditor; override;
    procedure RenderRow(Row: Integer); override;
    function SelectionToRect(Selection: TSelectionPos): TRect; override;
    procedure ClampCursors; override;

    procedure RenderLenCell(Cell: TCell);
    procedure RenderDutyCell(Cell: TCell);
    procedure RenderNoiseCell(Cell: TCell);
    procedure InputInstrument(Key: Word); override;
    procedure InputVolume(Key: Word); override;
    procedure InputEffectCode(Key: Word); override;
    procedure InputEffectParams(Key: Word); override;

    public
      LenColumnWidth: Integer;
  end;

implementation

{ TSFXGrid }

procedure TSFXGrid.Paint;
var
  I: Integer;
  R: TRect;
begin
  with Canvas do begin
    Brush.Color := TrackerGrid.clBackground;
    Clear;

    for I := 0 to High(TPattern) do begin
      if (I mod 4) = 0 then
        Brush.Color := clLineFour;
      if (I mod 16) = 0 then
        Brush.Color := clLineSixteen;
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
  Canvas.Pen.Color := clDividers;
  Canvas.Pen.Width := 1;

  for I := 0 to NumColumns do begin
    if I = 0 then
      R := TRect.Create(0, 0, LenColumnWidth, Height)
    else
      R := TRect.Create(LenColumnWidth+(ColumnWidth*(I-1)), 0, LenColumnWidth+(ColumnWidth*(I-1))+ColumnWidth, Height);
    Canvas.Rectangle(R);
  end;

  RenderSelectedArea;
  if DraggingSelection then
    Canvas.DrawFocusRect(SelectionsToRect(DragSelCursor, DragSelOther));
end;

procedure TSFXGrid.ChangeFontSize;
begin
  // Kind of a hack
  with Canvas.Font do begin
    Name := 'PixeliteTTF';
    Size := FontSize;
    ColumnWidth := GetTextWidth('C-5 . . . ');
    RowHeight := GetTextHeight('C-5 . . . ');
    LenColumnWidth := GetTextWidth('END ');

    CharHeight := RowHeight;
    CharWidth := GetTextWidth('C');
  end;

  Width := LenColumnWidth + (ColumnWidth*(NumColumns-1));
  Height := RowHeight*NumRows;
end;

function TSFXGrid.MousePosToSelection(X, Y: Integer): TSelectionPos;
begin
  X := EnsureRange(X, 0, Width-1);
  Y := EnsureRange(Y, 0, Height-1);

  if X < LenColumnWidth then begin
    Result.X := 0;
    Result.Y := Trunc((Y/Height)*NumRows);
    Result.SelectedPart := cpEffectParams;
  end
  else begin
    Dec(X, LenColumnWidth);
    Result.X := Trunc((X/(Width-LenColumnWidth))*(NumColumns-1))+1;
    Result.Y := Trunc((Y/Height)*NumRows);

    X := X mod ColumnWidth;
    X := Trunc((X/ColumnWidth)*10);
    case X of
      0..3: Result.SelectedPart := cpNote;
      4..5: Result.SelectedPart := cpInstrument;
      6..7: Result.SelectedPart := cpVolume;
      8..9:   Result.SelectedPart := cpEffectCode;
      //10..12: Result.SelectedPart := cpEffectParams;
    end;
  end;
end;

procedure TSFXGrid.OpenEffectEditor;
begin
end;

procedure TSFXGrid.RenderRow(Row: Integer);
var
  I: Integer;
begin
  with Canvas do begin
    Brush.Style := bsClear;
    Pen.Color := RGBToColor(58, 52, 39);

    for I := 0 to High(Patterns) do begin
      if Assigned(Patterns[I]) then
        case I of
          0: RenderLenCell(Patterns[I]^[Row]);
          1: RenderDutyCell(Patterns[I]^[Row]);
          2: RenderNoiseCell(Patterns[I]^[Row]);
        end;
    end;
  end;
end;

function TSFXGrid.SelectionToRect(Selection: TSelectionPos): TRect;
var
  CharLeft, CharRight: Integer;
begin
  if Selection.X = 0 then begin
    Result.Left := 0;
    Result.Right := 3*CharWidth;
  end else begin
    case Selection.SelectedPart of
        cpNote: begin
          CharLeft := 0;
          CharRight := 3;
        end;
        cpInstrument: begin
          CharLeft := 4;
          CharRight := 5;
        end;
        cpVolume: begin
          CharLeft := 6;
          CharRight := 7;
        end;
        cpEffectCode: begin
          CharLeft := 8;
          CharRight := 9;
        end;
        cpEffectParams: begin
          CharLeft := 8;
          CharRight := 9;
        end;
      end;

    Result.Left := LenColumnWidth + ((Selection.X-1)*ColumnWidth) + (CharLeft*CharWidth);
    Result.Right := LenColumnWidth + ((Selection.X-1)*ColumnWidth) + (CharRight*CharWidth);
  end;

  Result.Top := (Selection.Y*RowHeight);
  Result.Bottom := (Selection.Y*RowHeight) + RowHeight;
end;

procedure TSFXGrid.ClampCursors;
begin
  inherited ClampCursors;
end;

procedure TSFXGrid.RenderLenCell(Cell: TCell);
begin
  with Canvas do begin
    Font.Color := clBlack;

    if Cell.EffectParams.Value = 0 then
      TextOut(PenPos.X, PenPos.Y, 'END')
    else
      TextOut(PenPos.X, PenPos.Y, ' '+Format('%0.2d',[Cell.EffectParams.Value]));

    TextOut(PenPos.X, PenPos.Y, ' ');
  end;
end;

procedure TSFXGrid.RenderDutyCell(Cell: TCell);
var
  NoteString: ShortString;
begin
  with Canvas do begin
    if NoteMap.TryGetData(Cell.Note, NoteString) then begin
      Font.Color := clNote;
      TextOut(PenPos.X, PenPos.Y, NoteString);
    end
    else begin
      Font.Color := clDots;
      if Cell.Note = NO_NOTE then
        TextOut(PenPos.X, PenPos.Y, '...')
      else
        TextOut(PenPos.X, PenPos.Y, '???');
    end;

    TextOut(PenPos.X, PenPos.Y, ' ');

    Font.Color := clFxMisc;
    TextOut(PenPos.X, PenPos.Y, HexStr(Cell.Instrument, 1));

    TextOut(PenPos.X, PenPos.Y, ' ');

    Font.Color := clFxVolume;
    TextOut(PenPos.X, PenPos.Y, HexStr(Cell.Volume, 1));

    TextOut(PenPos.X, PenPos.Y, ' ');

    Font.Color := clFxPan;
    case Cell.EffectCode of
      $FF: TextOut(PenPos.X, PenPos.Y, 'M');
      $F0: TextOut(PenPos.X, PenPos.Y, 'L');
      $0F: TextOut(PenPos.X, PenPos.Y, 'R');
      else TextOut(PenPos.X, PenPos.Y, '?');
    end;

    TextOut(PenPos.X, PenPos.Y, ' ');
  end;
end;

procedure TSFXGrid.RenderNoiseCell(Cell: TCell);
var
  NoteString: ShortString;
begin
  with Canvas do begin
    if NoteMap.TryGetData(Cell.Note, NoteString) then begin
      Font.Color := clNote;
      TextOut(PenPos.X, PenPos.Y, NoteString);
    end
    else begin
      Font.Color := clDots;
      if Cell.Note = NO_NOTE then
        TextOut(PenPos.X, PenPos.Y, '...')
      else
        TextOut(PenPos.X, PenPos.Y, '???');
    end;

    TextOut(PenPos.X, PenPos.Y, ' ');

    Font.Color := clFxMisc;
    TextOut(PenPos.X, PenPos.Y, HexStr(Cell.Instrument, 1));

    TextOut(PenPos.X, PenPos.Y, ' ');

    Font.Color := clFxVolume;
    TextOut(PenPos.X, PenPos.Y, HexStr(Cell.Volume, 1));

    TextOut(PenPos.X, PenPos.Y, ' ');

    Font.Color := clFxPan;
    case Cell.EffectCode of
      $FF: TextOut(PenPos.X, PenPos.Y, 'M');
      $F0: TextOut(PenPos.X, PenPos.Y, 'L');
      $0F: TextOut(PenPos.X, PenPos.Y, 'R');
      else TextOut(PenPos.X, PenPos.Y, '?');
    end;

    TextOut(PenPos.X, PenPos.Y, ' ');
  end;
end;

procedure TSFXGrid.InputInstrument(Key: Word);
var
  Temp: Nibble;
begin
  BeginUndoAction;
  with Patterns[Cursor.X]^[Cursor.Y] do begin
    if Key = VK_DELETE then Instrument := 0
    else if KeycodeToHexNumber(Key, Temp) then
      Instrument := Temp;
  end;

  Invalidate;
  EndUndoAction;
end;

procedure TSFXGrid.InputVolume(Key: Word);
var
  Temp: Nibble;
begin
  BeginUndoAction;
  with Patterns[Cursor.X]^[Cursor.Y] do begin
    if Key = VK_DELETE then Volume := 0
    else if KeycodeToHexNumber(Key, Temp) then
      Volume := Temp;
  end;

  Invalidate;
  EndUndoAction;
end;

procedure TSFXGrid.InputEffectCode(Key: Word);
begin
  BeginUndoAction;

  with Patterns[Cursor.X]^[Cursor.Y] do begin
    case Key of
      VK_L: EffectCode := $F0;
      VK_R: EffectCode := $0F;
      VK_M: EffectCode := $FF;
    end;
  end;

  Invalidate;
  EndUndoAction;
end;

procedure TSFXGrid.InputEffectParams(Key: Word);
var
  Temp: Nibble;
begin
  BeginUndoAction;
  with Patterns[Cursor.X]^[Cursor.Y] do begin
    if Key = VK_DELETE then Instrument := 0
    else if KeycodeToHexNumber(Key, Temp) and InRange(Temp, 0, 9) then
      EffectParams.Value := ((EffectParams.Value mod 10) * 10) + Temp;
  end;

  Invalidate;
  EndUndoAction;
end;

end.


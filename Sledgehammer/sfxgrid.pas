unit SFXGrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TrackerGrid, hUGEDatatypes, Math, Graphics;

type

  { TSFXGrid }

  TSFXGrid = class(TTrackerGrid)
    procedure Paint; override;
    procedure ChangeFontSize; override;
    function MousePosToSelection(X, Y: Integer): TSelectionPos; override;
    procedure OpenEffectEditor; override;
    procedure RenderRow(Row: Integer); override;

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
    ColumnWidth := GetTextWidth('C-5 01v64C01 ');
    RowHeight := GetTextHeight('C-5 01v64C01 ');
    LenColumnWidth := GetTextWidth('END');

    CharHeight := RowHeight;
    CharWidth := GetTextWidth('C');
  end;

  Width := LenColumnWidth + (ColumnWidth*(NumColumns-1));
  Height := RowHeight*NumRows;
end;

function TSFXGrid.MousePosToSelection(X, Y: Integer): TSelectionPos;
begin
  X := EnsureRange(Width-1, 0, X);
  Y := EnsureRange(Height-1, 0, Y);

  if X < LenColumnWidth then begin
    Result.X := 0;
    Result.Y := Trunc((Y/Height)*NumRows);
    Result.SelectedPart := cpNote;
  end
  else begin
    Dec(X, LenColumnWidth);
    Result.X := Trunc((X/Width)*NumColumns)+1;
    Result.Y := Trunc((Y/Height)*NumRows);

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

    for I := 0 to High(Patterns) do
      if Assigned(Patterns[I]) then
        RenderCell(Patterns[I]^[Row]);
  end;
end;

end.


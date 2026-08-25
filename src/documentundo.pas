unit DocumentUndo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Grids, ComCtrls, Song, HugeDatatypes, TrackerGrid,
  UndoManager;

type
  PSong = ^TSong;

  { TDocumentUndoSession

    Binds a generic undo manager to the song's main pattern grid and order
    grid. All document-specific snapshots, bookmarks, and view restoration
    live here rather than in the form or in TUndoManager. }

  TDocumentUndoSession = class
  private
    FSong: PSong;
    FOrderGrid: TStringGrid;
    FPatternGrid: TTrackerGrid;
    FPageControl: TPageControl;
    FPatternPage: TTabSheet;
    FManager: TUndoManager;
    FPatternTarget: TPatternUndoTarget;
    FUpdatingOrderGrid: Boolean;
    FLoadedOrderIndex: Integer;
    FOrderDepth: Integer;
    FOrderBefore: TOrder;
    FOrderIndexBefore: Integer;

    function OrderGridRowToIndex(GridRow: Integer): Integer; inline;
    function OrderIndexToGridRow(OrderIndex: Integer): Integer; inline;
    procedure BeginOrderChange;
    procedure EndOrderChange;
    procedure ApplyOrder(const Order: TOrder; OrderIndex: Integer);
    procedure SelectOrder(OrderIndex: Integer);
    procedure ActivatePatternPage;
  public
    constructor Create(Song: PSong; OrderGrid: TStringGrid;
      PageControl: TPageControl; PatternPage: TTabSheet);
    destructor Destroy; override;

    procedure AttachPatternGrid(Grid: TTrackerGrid);
    procedure Reset;

    function CurrentOrderIndex: Integer;
    function CurrentPatternSetID: Integer;

    procedure LoadCurrentPatterns;
    procedure LoadOrderGrid;
    procedure SyncOrderFromGrid;
    procedure OrderGridSelectionChanged;
    procedure OrderGridRowsDeleted;

    procedure InsertNewPatternSet;
    procedure InsertDefaultPatternSet;
    procedure DeleteCurrentOrder;
    procedure InsertCurrentPatternSet;
    procedure InsertCloneOfCurrentPatternSet;
    procedure ReplaceCurrentWithNewPatternSet;

    property Manager: TUndoManager read FManager;
    property UpdatingOrderGrid: Boolean read FUpdatingOrderGrid;
  end;

implementation

type
  TDocumentPatternBookmark = class(TPatternUndoBookmark)
  public
    PatternSetID: Integer;
    OrderIndex: Integer;
  end;

  { TDocumentPatternUndoTarget }

  TDocumentPatternUndoTarget = class(TPatternUndoTarget)
  private
    FSession: TDocumentUndoSession;
  public
    constructor Create(Session: TDocumentUndoSession);
    function CaptureBookmark(const PatternNumbers: array of Integer):
      TPatternUndoBookmark; override;
    procedure Apply(const State: TPatternUndoState;
      Bookmark: TPatternUndoBookmark); override;
  end;

  { TOrderEditUndoAction }

  TOrderEditUndoAction = class(TUndoAction)
  private
    FSession: TDocumentUndoSession;
    FBefore, FAfter: TOrder;
    FBeforeIndex, FAfterIndex: Integer;
  public
    constructor Create(Session: TDocumentUndoSession;
      const BeforeOrder, AfterOrder: TOrder; BeforeIndex, AfterIndex: Integer);
    procedure Undo; override;
    procedure Redo; override;
  end;

function CloneOrder(const Source: TOrder): TOrder;
begin
  Result := nil;
  SetLength(Result, Length(Source));
  if Length(Source) > 0 then
    Move(Source[0], Result[0], Length(Source) * SizeOf(Source[0]));
end;

function OrdersEqual(const Left, Right: TOrder): Boolean;
begin
  Result := Length(Left) = Length(Right);
  if Result and (Length(Left) > 0) then
    Result := CompareMem(@Left[0], @Right[0],
      Length(Left) * SizeOf(Left[0]));
end;

{ TDocumentPatternUndoTarget }

constructor TDocumentPatternUndoTarget.Create(Session: TDocumentUndoSession);
begin
  inherited Create;
  FSession := Session;
end;

function TDocumentPatternUndoTarget.CaptureBookmark(
  const PatternNumbers: array of Integer): TPatternUndoBookmark;
var
  DocumentBookmark: TDocumentPatternBookmark;
  PatternSet: TPatternSet;
  Channel: TChannel;
  OrderIndex, PatternSetID: Integer;
begin
  if not InRange(FSession.FLoadedOrderIndex, 0,
    Length(FSession.FSong^.Order) - 1) then
    raise Exception.Create('Pattern grid has no order row');

  OrderIndex := FSession.FLoadedOrderIndex;
  PatternSetID := FSession.FSong^.Order[OrderIndex];
  PatternSet := GetPatternSet(FSession.FSong^, PatternSetID);
  for Channel := Low(TChannel) to High(TChannel) do
    if PatternNumbers[Ord(Channel)] <> PatternSet.PatternKeys[Channel] then
      raise Exception.Create('Pattern grid is out of sync');

  DocumentBookmark := TDocumentPatternBookmark.Create;
  DocumentBookmark.OrderIndex := OrderIndex;
  DocumentBookmark.PatternSetID := PatternSetID;
  Result := DocumentBookmark;
end;

procedure TDocumentPatternUndoTarget.Apply(const State: TPatternUndoState;
  Bookmark: TPatternUndoBookmark);
var
  DocumentBookmark: TDocumentPatternBookmark;
  I: Integer;
begin
  DocumentBookmark := TDocumentPatternBookmark(Bookmark);
  if not InRange(DocumentBookmark.OrderIndex, 0,
    Length(FSession.FSong^.Order) - 1)
  or (FSession.FSong^.Order[DocumentBookmark.OrderIndex] <>
    DocumentBookmark.PatternSetID) then
    raise Exception.Create('Undo history is out of sync');

  for I := Low(State.Patterns) to High(State.Patterns) do
    FSession.FSong^.Patterns.GetOrCreateNew(State.Patterns[I].PatternNumber)^ :=
      State.Patterns[I].Pattern;

  FSession.SelectOrder(DocumentBookmark.OrderIndex);
  for I := Low(State.Patterns) to High(State.Patterns) do
    FSession.FPatternGrid.LoadPattern(I, State.Patterns[I].PatternNumber);
  FSession.FLoadedOrderIndex := DocumentBookmark.OrderIndex;
  FSession.FPatternGrid.RestoreSelection(State.Cursor, State.Other);
  FSession.ActivatePatternPage;
end;

{ TOrderEditUndoAction }

constructor TOrderEditUndoAction.Create(Session: TDocumentUndoSession;
  const BeforeOrder, AfterOrder: TOrder; BeforeIndex, AfterIndex: Integer);
begin
  inherited Create;
  FSession := Session;
  FBefore := CloneOrder(BeforeOrder);
  FAfter := CloneOrder(AfterOrder);
  FBeforeIndex := BeforeIndex;
  FAfterIndex := AfterIndex;
end;

procedure TOrderEditUndoAction.Undo;
begin
  FSession.ApplyOrder(FBefore, FBeforeIndex);
end;

procedure TOrderEditUndoAction.Redo;
begin
  FSession.ApplyOrder(FAfter, FAfterIndex);
end;

{ TDocumentUndoSession }

constructor TDocumentUndoSession.Create(Song: PSong; OrderGrid: TStringGrid;
  PageControl: TPageControl; PatternPage: TTabSheet);
begin
  inherited Create;
  FSong := Song;
  FOrderGrid := OrderGrid;
  FPageControl := PageControl;
  FPatternPage := PatternPage;
  FManager := TUndoManager.Create;
  FPatternTarget := TDocumentPatternUndoTarget.Create(Self);
  FLoadedOrderIndex := -1;
end;

destructor TDocumentUndoSession.Destroy;
begin
  // Actions refer to this session and its target, so discard them first.
  FManager.Free;
  FPatternTarget.Free;
  inherited;
end;

procedure TDocumentUndoSession.AttachPatternGrid(Grid: TTrackerGrid);
begin
  FPatternGrid := Grid;
  FPatternGrid.UndoManager := FManager;
  FPatternGrid.UndoTarget := FPatternTarget;
end;

procedure TDocumentUndoSession.Reset;
begin
  FManager.Reset;
  SetLength(FOrderBefore, 0);
end;

function TDocumentUndoSession.OrderGridRowToIndex(GridRow: Integer): Integer;
begin
  Result := GridRow - FOrderGrid.FixedRows;
end;

function TDocumentUndoSession.OrderIndexToGridRow(OrderIndex: Integer): Integer;
begin
  Result := OrderIndex + FOrderGrid.FixedRows;
end;

function TDocumentUndoSession.CurrentOrderIndex: Integer;
begin
  Result := OrderGridRowToIndex(FOrderGrid.Row);
end;

function TDocumentUndoSession.CurrentPatternSetID: Integer;
begin
  Result := StrToInt(FOrderGrid.Cells[0, FOrderGrid.Row]);
end;

procedure TDocumentUndoSession.ActivatePatternPage;
begin
  FPageControl.ActivePage := FPatternPage;
end;

procedure TDocumentUndoSession.SelectOrder(OrderIndex: Integer);
begin
  FUpdatingOrderGrid := True;
  try
    FOrderGrid.Row := OrderIndexToGridRow(OrderIndex);
  finally
    FUpdatingOrderGrid := False;
  end;
end;

procedure TDocumentUndoSession.LoadCurrentPatterns;
var
  Channel: TChannel;
  PatternSetID: Integer;
  PatternSet: TPatternSet;
begin
  if FUpdatingOrderGrid then Exit;
  if not InRange(FOrderGrid.Row, FOrderGrid.FixedRows,
    FOrderGrid.RowCount - 1) then Exit;

  PatternSetID := CurrentPatternSetID;
  PatternSet := EnsurePatternSet(FSong^, PatternSetID);
  for Channel := Low(TChannel) to High(TChannel) do
    FPatternGrid.LoadPattern(Ord(Channel), PatternSet.PatternKeys[Channel]);
  FLoadedOrderIndex := CurrentOrderIndex;
end;

procedure TDocumentUndoSession.LoadOrderGrid;
var
  R: Integer;
begin
  if Length(FSong^.Order) = 0 then
    raise Exception.Create('Song has no order rows');

  FUpdatingOrderGrid := True;
  try
    FOrderGrid.Clean([gzNormal]);
    FOrderGrid.RowCount := Length(FSong^.Order) + FOrderGrid.FixedRows;
    for R := 0 to Length(FSong^.Order) - 1 do
      FOrderGrid.Cells[0, OrderIndexToGridRow(R)] :=
        IntToStr(FSong^.Order[R]);
  finally
    FUpdatingOrderGrid := False;
  end;
end;

procedure TDocumentUndoSession.BeginOrderChange;
begin
  if FOrderDepth = 0 then begin
    FOrderBefore := CloneOrder(FSong^.Order);
    FOrderIndexBefore := CurrentOrderIndex;
  end;
  Inc(FOrderDepth);
end;

procedure TDocumentUndoSession.EndOrderChange;
var
  BeforeIndex, AfterIndex: Integer;
begin
  if FOrderDepth <= 0 then
    raise Exception.Create('Unbalanced order undo transaction');

  Dec(FOrderDepth);
  if FOrderDepth <> 0 then Exit;
  if OrdersEqual(FOrderBefore, FSong^.Order) then Exit;

  BeforeIndex := FOrderIndexBefore;
  AfterIndex := CurrentOrderIndex;
  FManager.Commit(TOrderEditUndoAction.Create(Self, FOrderBefore,
    FSong^.Order, BeforeIndex, AfterIndex));
end;

procedure TDocumentUndoSession.SyncOrderFromGrid;
var
  R: Integer;
  NewOrder: TOrder;
  OwnChange: Boolean;
begin
  if FUpdatingOrderGrid then Exit;
  if FOrderGrid.RowCount <= FOrderGrid.FixedRows then
    raise Exception.Create('Song has no order rows');

  NewOrder := nil;
  OwnChange := FOrderDepth = 0;
  if OwnChange then BeginOrderChange;
  try
    SetLength(NewOrder, FOrderGrid.RowCount - FOrderGrid.FixedRows);
    for R := 0 to Length(NewOrder) - 1 do
      NewOrder[R] := StrToInt(
        FOrderGrid.Cells[0, OrderIndexToGridRow(R)]);
    for R := 0 to Length(NewOrder) - 1 do
      EnsurePatternSet(FSong^, NewOrder[R]);
    FSong^.Order := NewOrder;
  finally
    if OwnChange then EndOrderChange;
  end;
end;

procedure TDocumentUndoSession.ApplyOrder(const Order: TOrder;
  OrderIndex: Integer);
begin
  FSong^.Order := CloneOrder(Order);
  LoadOrderGrid;
  SelectOrder(OrderIndex);
  LoadCurrentPatterns;
  ActivatePatternPage;
end;

procedure TDocumentUndoSession.OrderGridSelectionChanged;
begin
  if not FUpdatingOrderGrid then
    LoadCurrentPatterns;
end;

procedure TDocumentUndoSession.OrderGridRowsDeleted;
begin
  if FUpdatingOrderGrid then Exit;

  if FOrderGrid.RowCount <= FOrderGrid.FixedRows then
    raise Exception.Create('Cannot delete the final order row');
  FOrderGrid.Row := EnsureRange(FOrderGrid.Row, FOrderGrid.FixedRows,
    FOrderGrid.RowCount - 1);
  SyncOrderFromGrid;
  LoadCurrentPatterns;
end;

procedure TDocumentUndoSession.InsertNewPatternSet;
var
  PatternSetID: Integer;
begin
  BeginOrderChange;
  try
    PatternSetID := CreatePatternSet(FSong^);
    FOrderGrid.InsertRowWithValues(FOrderGrid.Row + 1,
      [IntToStr(PatternSetID)]);
    FOrderGrid.Row := FOrderGrid.Row + 1;
    SyncOrderFromGrid;
  finally
    EndOrderChange;
  end;
  LoadCurrentPatterns;
end;

procedure TDocumentUndoSession.InsertDefaultPatternSet;
begin
  BeginOrderChange;
  try
    FOrderGrid.InsertRowWithValues(FOrderGrid.Row + 1, ['0']);
    FOrderGrid.Row := FOrderGrid.Row + 1;
    SyncOrderFromGrid;
  finally
    EndOrderChange;
  end;
  LoadCurrentPatterns;
end;

procedure TDocumentUndoSession.DeleteCurrentOrder;
begin
  BeginOrderChange;
  try
    if (FOrderGrid.Row >= FOrderGrid.FixedRows)
    and (FOrderGrid.RowCount > FOrderGrid.FixedRows + 1) then
      FOrderGrid.DeleteRow(FOrderGrid.Row);
    SyncOrderFromGrid;
  finally
    EndOrderChange;
  end;
  LoadCurrentPatterns;
end;

procedure TDocumentUndoSession.InsertCurrentPatternSet;
var
  PatternSetID: Integer;
begin
  BeginOrderChange;
  try
    PatternSetID := FSong^.Order[CurrentOrderIndex];
    FOrderGrid.InsertRowWithValues(FOrderGrid.Row + 1,
      [IntToStr(PatternSetID)]);
    FOrderGrid.Row := FOrderGrid.Row + 1;
    SyncOrderFromGrid;
  finally
    EndOrderChange;
  end;
  LoadCurrentPatterns;
end;

procedure TDocumentUndoSession.InsertCloneOfCurrentPatternSet;
var
  PatternSetID: Integer;
begin
  BeginOrderChange;
  try
    PatternSetID := ClonePatternSet(FSong^,
      FSong^.Order[CurrentOrderIndex]);
    FOrderGrid.InsertRowWithValues(FOrderGrid.Row + 1,
      [IntToStr(PatternSetID)]);
    FOrderGrid.Row := FOrderGrid.Row + 1;
    SyncOrderFromGrid;
  finally
    EndOrderChange;
  end;
  LoadCurrentPatterns;
end;

procedure TDocumentUndoSession.ReplaceCurrentWithNewPatternSet;
begin
  if FOrderGrid.Row < FOrderGrid.FixedRows then Exit;

  BeginOrderChange;
  try
    FOrderGrid.Cells[0, FOrderGrid.Row] :=
      IntToStr(CreatePatternSet(FSong^));
    SyncOrderFromGrid;
  finally
    EndOrderChange;
  end;
  LoadCurrentPatterns;
end;

end.

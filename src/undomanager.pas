unit UndoManager;

{$mode delphi}

interface

uses
  gdeque, gstack;

const
  UNDO_STACK_SIZE = 100;

type
  { TUndoAction }

  TUndoAction = class
  public
    procedure Undo; virtual; abstract;
    procedure Redo; virtual; abstract;
  end;

  TUndoActionDeque = TDeque<TUndoAction>;
  TRedoActionStack = TStack<TUndoAction>;

  { TUndoManager

    Owns committed actions. Actions contain all state and behavior necessary to
    undo and redo themselves; the manager deliberately knows nothing about the
    document or controls which produced them. }

  TUndoManager = class
  private
    Performed: TUndoActionDeque;
    Recall: TRedoActionStack;
    procedure ClearPerformed;
    procedure ClearRecall;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Commit(Action: TUndoAction);
    procedure Undo;
    procedure Redo;
    procedure Reset;
  end;

implementation

constructor TUndoManager.Create;
begin
  inherited;
  Performed := TUndoActionDeque.Create;
  Recall := TRedoActionStack.Create;
end;

destructor TUndoManager.Destroy;
begin
  Reset;

  // Work around the empty-TDeque destructor bug in the FPC version used by
  // the project. This can go once that compiler is no longer supported.
  Performed.PushFront(nil);
  Performed.Free;
  Recall.Free;
  inherited;
end;

procedure TUndoManager.ClearPerformed;
var
  Action: TUndoAction;
begin
  while not Performed.IsEmpty do begin
    Action := Performed.Front;
    Performed.PopFront;
    Action.Free;
  end;
end;

procedure TUndoManager.ClearRecall;
var
  Action: TUndoAction;
begin
  while not Recall.IsEmpty do begin
    Action := Recall.Top;
    Recall.Pop;
    Action.Free;
  end;
end;

procedure TUndoManager.Commit(Action: TUndoAction);
var
  Discarded: TUndoAction;
begin
  ClearRecall;
  Performed.PushFront(Action);
  while Performed.Size > UNDO_STACK_SIZE do begin
    Discarded := Performed.Back;
    Performed.PopBack;
    Discarded.Free;
  end;
end;

procedure TUndoManager.Undo;
var
  Action: TUndoAction;
begin
  if Performed.IsEmpty then Exit;

  Action := Performed.Front;
  Action.Undo;
  Performed.PopFront;
  Recall.Push(Action);
end;

procedure TUndoManager.Redo;
var
  Action: TUndoAction;
begin
  if Recall.IsEmpty then Exit;

  Action := Recall.Top;
  Action.Redo;
  Recall.Pop;
  Performed.PushFront(Action);
end;

procedure TUndoManager.Reset;
begin
  ClearPerformed;
  ClearRecall;
end;

end.

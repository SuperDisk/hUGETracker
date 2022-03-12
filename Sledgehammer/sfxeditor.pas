unit SFXEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  ComCtrls, Menus, SFXGrid, hUGESettings, options, HugeDatatypes, Keymap, About,
  Constants;

type

  { TSFXEditor1 }

  TSFXEditor1 = class(TForm)
    HeaderControl1: THeaderControl;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    Panel1: TPanel;
    RowNumberStringGrid: TStringGrid;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);

    procedure RecreateRowNumbers;
    procedure RecreateSFXGrid;
    procedure CreateKeymap;

    procedure BlankSFXPattern(P: PPattern);
  private
    SFXGrid: TSFXGrid;
    SFXPatterns: TPatternMap;
  public

  end;

var
  SFXEditor1: TSFXEditor1;

implementation

{$R *.lfm}

{ TSFXEditor1 }

procedure TSFXEditor1.FormCreate(Sender: TObject);
begin
  SFXPatterns := TPatternMap.Create;

  RecreateSFXGrid;
  RecreateRowNumbers;
  CreateKeymap;

  BlankSFXPattern(SFXPatterns.GetOrCreateNew(0));
  BlankSFXPattern(SFXPatterns.GetOrCreateNew(1));
  BlankSFXPattern(SFXPatterns.GetOrCreateNew(2));

  SFXGrid.LoadPattern(0, 0);
  SFXGrid.LoadPattern(1, 1);
  SFXGrid.LoadPattern(2, 2);
end;

procedure TSFXEditor1.CreateKeymap;
var
  StringGrid: TStringGrid;
begin
  if not TrackerSettings.UseCustomKeymap then
    LoadDefaultKeybindings
  else begin
    StringGrid := TStringGrid.Create(nil); // UGH!
    try
      StringGrid.SaveOptions := soAll;
      StringGrid.LoadFromFile('custom_keymap.km');
      LoadCustomKeybindings(StringGrid);
    finally
      StringGrid.Free;
    end;
  end;
end;

procedure TSFXEditor1.BlankSFXPattern(P: PPattern);
var
  I: Integer;
begin
  for I := Low(P^) to High(P^) do begin
    P^[I] := Default(TCell);
    P^[I].EffectCode := $FF;
    P^[I].Note := NO_NOTE;
  end;
end;

procedure TSFXEditor1.MenuItem7Click(Sender: TObject);
begin
  frmOptions.ShowModal;
end;

procedure TSFXEditor1.MenuItem8Click(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TSFXEditor1.RecreateRowNumbers;
var
  I: Integer;
begin
  RowNumberStringGrid.Clean;
  // Add the row numbers to the string grid
  for I := 0 to RowNumberStringGrid.RowCount-1 do begin
    if TrackerSettings.DisplayRowNumbersAsHex then begin
      RowNumberStringGrid.Cells[0, I] := IntToHex(I, 2);
    end
    else begin
      RowNumberStringGrid.Cells[0, I] := IntToStr(I);
    end;
  end;
end;

procedure TSFXEditor1.RecreateSFXGrid;
var
  I: Integer;
begin
  if Assigned(SFXGrid) then SFXGrid.Free;

  SFXGrid := TSFXGrid.Create(Self, ScrollBox1, SFXPatterns, 3, 32);

  //TrackerGrid.OnResize:=@OnTrackerGridResize;
  //TrackerGrid.OnCursorOutOfBounds:=@OnTrackerGridCursorOutOfBounds;
  //TrackerGrid.FontSize := TrackerSettings.PatternEditorFontSize;
  SFXGrid.Left := RowNumberStringGrid.Left + RowNumberStringGrid.Width;
  //TrackerGrid.PopupMenu := TrackerGridPopup;
  RowNumberStringGrid.DefaultRowHeight := SFXGrid.RowHeight;

  // Fix the size of the channel headers
  HeaderControl1.Sections.Items[1].Width := SFXGrid.LenColumnWidth;
  for I := 2 to HeaderControl1.Sections.Count-1 do
    HeaderControl1.Sections.Items[I].Width := SFXGrid.ColumnWidth;
end;

end.


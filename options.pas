unit options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, Grids,
  LCLProc, Buttons, ExtCtrls, Constants, Keymap, hUGESettings;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ColorButton1: TColorButton;
    ColorButton10: TColorButton;
    ColorButton11: TColorButton;
    ColorButton12: TColorButton;
    ColorButton13: TColorButton;
    ColorButton14: TColorButton;
    ColorButton2: TColorButton;
    ColorButton3: TColorButton;
    ColorButton4: TColorButton;
    ColorButton5: TColorButton;
    ColorButton6: TColorButton;
    ColorButton7: TColorButton;
    ColorButton8: TColorButton;
    ColorButton9: TColorButton;
    ColorDialog1: TColorDialog;
    DisplayHexRowNumbersCheck: TCheckBox;
    CustomizationGroupBox: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OptionsGroupBox: TGroupBox;
    Panel1: TPanel;
    PreviewWhenPlacingCheck: TCheckBox;
    PreviewWhenBumpingCheck: TCheckBox;
    KeymapCheckbox: TCheckBox;
    KeymapGroupBox: TGroupBox;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ScopesCheck: TCheckBox;
    Label1: TLabel;
    FontSizeSpinner: TSpinEdit;
    KeyMapStringGrid: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure KeymapCheckboxChange(Sender: TObject);
    procedure KeyMapStringGridValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
  private

  public

  end;

var
  frmOptions: TfrmOptions;

implementation

{$R *.lfm}

{ TfrmOptions }

procedure TfrmOptions.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmOptions.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    KeyMapStringGrid.LoadFromFile(OpenDialog1.FileName);
end;

procedure TfrmOptions.Button3Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    KeyMapStringGrid.SaveToFile(SaveDialog1.FileName);
end;

procedure TfrmOptions.Button4Click(Sender: TObject);
begin
  if KeyMapStringGrid.Row > 0 then
    KeyMapStringGrid.DeleteRow(KeyMapStringGrid.Row);
end;

procedure TfrmOptions.Button5Click(Sender: TObject);
begin
  KeyMapStringGrid.InsertRowWithValues(KeyMapStringGrid.RowCount, ['', '']);
end;

procedure TfrmOptions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  KeyMapStringGrid.SaveToFile('custom_keymap.km');
  TrackerSettings.PatternEditorFontSize := FontSizeSpinner.Value;
  TrackerSettings.UseScopes := ScopesCheck.Checked;
  TrackerSettings.UseCustomKeymap := KeymapCheckbox.Checked;
  TrackerSettings.PreviewWhenPlacing := PreviewWhenPlacingCheck.Checked;
  TrackerSettings.PreviewWhenBumping := PreviewWhenBumpingCheck.Checked;
  TrackerSettings.DisplayRowNumbersAsHex := DisplayHexRowNumbersCheck.Checked;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  KeyMapStringGrid.SaveOptions := soAll;

  if FileExists('custom_keymap.km') then
    KeyMapStringGrid.LoadFromFile('custom_keymap.km');

  FontSizeSpinner.Value := TrackerSettings.PatternEditorFontSize;
  ScopesCheck.Checked := TrackerSettings.UseScopes;
  KeymapCheckbox.Checked := TrackerSettings.UseCustomKeymap;
  PreviewWhenPlacingCheck.Checked := TrackerSettings.PreviewWhenPlacing;
  PreviewWhenBumpingCheck.Checked := TrackerSettings.PreviewWhenBumping;
  DisplayHexRowNumbersCheck.Checked := TrackerSettings.DisplayRowNumbersAsHex;
end;

procedure TfrmOptions.KeymapCheckboxChange(Sender: TObject);
begin
  KeymapGroupBox.Enabled := KeymapCheckbox.Checked;
end;

procedure TfrmOptions.KeyMapStringGridValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
begin
  if aCol = 0 then begin // shortcut
    if TextToShortCut(NewValue) = 0 then
      NewValue := '';
  end;

  if aCol = 1 then begin // note
    if NoteToCodeMap.IndexOf(NewValue) = -1 then
      NewValue := 'C-3';
  end;
end;

end.


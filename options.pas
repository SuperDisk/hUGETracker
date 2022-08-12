unit options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, Grids,
  LCLProc, Buttons, ExtCtrls, ComCtrls, CheckLst, Constants, Keymap,
  hUGESettings, hUGEDataTypes, TrackerGrid;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    UseCustomKeymapCheckbox: TCheckBox;
    CheckListBox1: TCheckListBox;
    NoteTextColorButton: TColorButton;
    OpenDialog2: TOpenDialog;
    OptionsPageControl: TPageControl;
    Panel1: TPanel;
    SaveDialog2: TSaveDialog;
    SelectedColorButton: TColorButton;
    FourthRowColorButton: TColorButton;
    SixteenthRowColorButton: TColorButton;
    DotsColorButton: TColorButton;
    DividersColorButton: TColorButton;
    InstrumentTextColorButton: TColorButton;
    MiscEffectTextColorButton: TColorButton;
    PitchEffectTextColorButton: TColorButton;
    KeyboardTabSheet: TTabSheet;
    GeneralTabSheet: TTabSheet;
    CustomiztaionTabSheet: TTabSheet;
    VolumeEffectTextColorButton: TColorButton;
    PanningEffectTextColorButton: TColorButton;
    SongEffectTextColorButton: TColorButton;
    BackgroundColorButton: TColorButton;
    HighlightedColorButton: TColorButton;
    ColorDialog1: TColorDialog;
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
    SampleTrackerGridPanel: TPanel;
    KeymapGroupBox: TGroupBox;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    FontSizeSpinner: TSpinEdit;
    KeyMapStringGrid: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FontSizeSpinnerChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure KeymapCheckboxChange(Sender: TObject);
    procedure KeyMapStringGridValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
    procedure NoteTextColorButtonColorChanged(Sender: TObject);
  private
    SamplePatternMap: TPatternMap;
    SampleTrackerGrid: TTrackerGrid;
    LoadingColors: Boolean;

    procedure RecreateTrackerGrid;
    procedure UpdateTrackerGridColors;
    procedure SaveColorsToFile(Filename: String);
    procedure LoadColorsFromFile(Filename: String);
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

procedure TfrmOptions.Button6Click(Sender: TObject);
begin
  NoteTextColorButton.ButtonColor := $007F4A00;
  InstrumentTextColorButton.ButtonColor := $007F7F00;
  MiscEffectTextColorButton.ButtonColor := $003F3F7C;
  PitchEffectTextColorButton.ButtonColor := $00006262;
  VolumeEffectTextColorButton.ButtonColor := $00007F26;
  PanningEffectTextColorButton.ButtonColor := $007F7F00;
  SongEffectTextColorButton.ButtonColor := $0000007F;
  BackgroundColorButton.ButtonColor := $00D0DBE1;
  HighlightedColorButton.ButtonColor := $007A99A9;
  SelectedColorButton.ButtonColor := $009EB4C0;
  FourthRowColorButton.ButtonColor := $00C3D1D8;
  SixteenthRowColorButton.ButtonColor := $00B5C5CE;
  DotsColorButton.ButtonColor := clGray;
  DividersColorButton.ButtonColor := $00ABB7BC;

  UpdateTrackerGridColors;
end;

procedure TfrmOptions.Button7Click(Sender: TObject);
begin
  if OpenDialog2.Execute then
    LoadColorsFromFile(OpenDialog2.FileName);
end;

procedure TfrmOptions.Button8Click(Sender: TObject);
begin
  if SaveDialog2.Execute then
    SaveColorsToFile(SaveDialog2.FileName);
end;

procedure TfrmOptions.FontSizeSpinnerChange(Sender: TObject);
begin
  if Visible then // Don't do this while the form is being constructed.
    RecreateTrackerGrid;
end;

procedure TfrmOptions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  KeyMapStringGrid.SaveToFile('custom_keymap.km');
  SaveColorsToFile('color_scheme.col');

  TrackerSettings.PatternEditorFontSize := FontSizeSpinner.Value;
  TrackerSettings.UseScopes := CheckListBox1.Checked[0];
  TrackerSettings.UseCustomKeymap := UseCustomKeymapCheckbox.Checked;
  TrackerSettings.PreviewWhenPlacing := CheckListBox1.Checked[1];
  TrackerSettings.PreviewWhenBumping := CheckListBox1.Checked[2];
  TrackerSettings.DisplayRowNumbersAsHex := CheckListBox1.Checked[3];
  TrackerSettings.DisplayOrderRowNumbersAsHex := CheckListBox1.Checked[4];
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
var
  SamplePattern: PPattern;
  I: Integer;
begin
  KeyMapStringGrid.SaveOptions := soAll;

  if FileExists('custom_keymap.km') then
    KeyMapStringGrid.LoadFromFile('custom_keymap.km');

  if FileExists('color_scheme.col') then
    LoadColorsFromFile('color_scheme.col');

  FontSizeSpinner.Value := TrackerSettings.PatternEditorFontSize;
  CheckListBox1.Checked[0] := TrackerSettings.UseScopes;
  UseCustomKeymapCheckbox.Checked := TrackerSettings.UseCustomKeymap;
  CheckListBox1.Checked[1] := TrackerSettings.PreviewWhenPlacing;
  CheckListBox1.Checked[2] := TrackerSettings.PreviewWhenBumping;
  CheckListBox1.Checked[3] := TrackerSettings.DisplayRowNumbersAsHex;
  CheckListBox1.Checked[4] := TrackerSettings.DisplayOrderRowNumbersAsHex;

  SamplePatternMap := TPatternMap.Create;
  SamplePattern := SamplePatternMap.GetOrCreateNew(0);

  for I := $0 to $F do begin
    SamplePattern^[I*2].Note := I;
    SamplePattern^[I*2].Instrument := Random(15);
    SamplePattern^[I*2].EffectCode := I;
    SamplePattern^[I*2].EffectParams.Value := Random($FF);
  end;

  RecreateTrackerGrid;
end;

procedure TfrmOptions.KeymapCheckboxChange(Sender: TObject);
begin
  KeymapGroupBox.Enabled := UseCustomKeymapCheckbox.Checked;
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

procedure TfrmOptions.NoteTextColorButtonColorChanged(Sender: TObject);
begin
  if not LoadingColors then
    UpdateTrackerGridColors;
end;

procedure TfrmOptions.RecreateTrackerGrid;
var
  I: Integer;
begin
  if Assigned(SampleTrackerGrid) then
    SampleTrackerGrid.Free;

  SampleTrackerGrid := TTrackerGrid.Create(Self, SampleTrackerGridPanel, SamplePatternMap, 4);
  SampleTrackerGrid.FontSize := FontSizeSpinner.Value;
  for I := 0 to 3 do
    SampleTrackerGrid.LoadPattern(I, 0);

  SampleTrackerGridPanel.Invalidate;
end;

procedure TfrmOptions.UpdateTrackerGridColors;
begin
  clNote := NoteTextColorButton.ButtonColor;
  clInstrument := InstrumentTextColorButton.ButtonColor;

  clFxMisc := MiscEffectTextColorButton.ButtonColor;
  clFxPitch := PitchEffectTextColorButton.ButtonColor;
  clFxVolume := VolumeEffectTextColorButton.ButtonColor;
  clFxPan := PanningEffectTextColorButton.ButtonColor;
  clFxSong := SongEffectTextColorButton.ButtonColor;

  clBackground := BackgroundColorButton.ButtonColor;
  clHighlighted := HighlightedColorButton.ButtonColor;
  clSelected := SelectedColorButton.ButtonColor;

  clLineFour := FourthRowColorButton.ButtonColor;
  clLineSixteen := SixteenthRowColorButton.ButtonColor;

  clDots := DotsColorButton.ButtonColor;
  clDividers := DividersColorButton.ButtonColor;

  SampleTrackerGridPanel.Invalidate;
end;

procedure TfrmOptions.SaveColorsToFile(Filename: String);
var
  F: file of TColor;
begin
  try
    AssignFile(F, Filename);
    Rewrite(F);

    Write(F, clNote);
    Write(F, clInstrument);

    Write(F, clFxMisc);
    Write(F, clFxPitch);
    Write(F, clFxVolume);
    Write(F, clFxPan);
    Write(F, clFxSong);

    Write(F, clBackground);
    Write(F, clHighlighted);
    Write(F, clSelected);

    Write(F, clLineFour);
    Write(F, clLineSixteen);

    Write(F, clDots);
    Write(F, clDividers);

    CloseFile(F);
  except
    on E: Exception do begin
      ShowMessage('Couldn''t save colors to ' + Filename);
    end;
  end;
end;

procedure TfrmOptions.LoadColorsFromFile(Filename: String);
var
  F: file of TColor;
begin
  LoadingColors := True;
  try
    AssignFile(F, Filename);
    Reset(F);

    Read(F, clNote);
    Read(F, clInstrument);

    Read(F, clFxMisc);
    Read(F, clFxPitch);
    Read(F, clFxVolume);
    Read(F, clFxPan);
    Read(F, clFxSong);

    Read(F, clBackground);
    Read(F, clHighlighted);
    Read(F, clSelected);

    Read(F, clLineFour);
    Read(F, clLineSixteen);

    Read(F, clDots);
    Read(F, clDividers);

    CloseFile(F);
  except
    on E: Exception do begin
      ShowMessage('Couldn''t load colors from ' + Filename);
    end;
  end;

  NoteTextColorButton.ButtonColor := clNote;
  InstrumentTextColorButton.ButtonColor := clInstrument;

  MiscEffectTextColorButton.ButtonColor := clFxMisc;
  PitchEffectTextColorButton.ButtonColor := clFxPitch;
  VolumeEffectTextColorButton.ButtonColor := clFxVolume;
  PanningEffectTextColorButton.ButtonColor := clFxPan;
  SongEffectTextColorButton.ButtonColor := clFxSong;

  BackgroundColorButton.ButtonColor := clBackground;
  HighlightedColorButton.ButtonColor := clHighlighted;
  SelectedColorButton.ButtonColor := clSelected;

  FourthRowColorButton.ButtonColor := clLineFour;
  SixteenthRowColorButton.ButtonColor := clLineSixteen;

  DotsColorButton.ButtonColor := clDots;
  DividersColorButton.ButtonColor := clDividers;

  LoadingColors := False;

  SampleTrackerGridPanel.Invalidate;
end;

end.


unit Tracker;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Menus, Spin, StdCtrls, ActnList, StdActns, SynEdit, SynHighlighterAny,
  FileUtil, math, Instruments, Waves, Song, Utils, Constants,
  sound, vars, machine, about_hugetracker, TrackerGrid, lclintf, lmessages,
  Buttons, Grids, DBCtrls, HugeDatatypes, LCLType, Clipbrd, RackCtls, Codegen,
  SymParser, options, bgrabitmap, effecteditor, RenderToWave,
  modimport, mainloop, strutils, Types, Keymap, hUGESettings;

// TODO: Move to config file?
const
  MaintainerEmail = 'yux50000@hotmail.com';
  MaintainerDiscord = 'SuperDisk#5726';
  clGameboyBlack = TColor($211807);
  clGameboyMidGreen = TColor($6CC086);
// TODO: Move somewhere else...
  LM_FD = LM_USER + 0;

type
  { TfrmTracker }

  TfrmTracker = class(TForm)
    IncrementCurrentInstrumentAction: TAction;
    DecrementCurrentInstrumentAction: TAction;
    GotoGeneralAction: TAction;
    GotoPatternsAction: TAction;
    GotoInstrumentsAction: TAction;
    GotoWavesAction: TAction;
    GotoCommentsAction: TAction;
    GotoRoutinesAction: TAction;
    DeleteRowAction: TAction;
    DeleteRowForAllAction: TAction;
    InsertRowForAllAction: TAction;
    InsertRowAction: TAction;
    MenuItem39: TMenuItem;
    ExportCMenuItem: TMenuItem;
    MenuItem40: TMenuItem;
    ExportAsmMenuItem: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem54: TMenuItem;
    NoiseMacroPaintbox: TPaintBox;
    Panel6: TPanel;
    GBDKCSaveDialog: TSaveDialog;
    RGBDSAsmSaveDialog: TSaveDialog;
    StopAction: TAction;
    PlayOrderAction: TAction;
    PlayCursorAction: TAction;
    PlayStartAction: TAction;
    ShortcutsActionList: TActionList;
    Button1: TButton;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    PlayWaveWhileDrawingCheckbox: TCheckBox;
    Duty1Visualizer: TPaintBox;
    Duty2Visualizer: TPaintBox;
    HexWaveEdit: TEdit;
    InstrumentExportButton: TButton;
    InstrumentImportButton: TButton;
    InstrumentComboBox: TComboBox;
    CutAction: TAction;
    ImageList2: TImageList;
    GBSaveDialog: TSaveDialog;
    Label13: TLabel;
    MenuItem10: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    FontSizeToggleMenuItem: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    NoiseVisualizer: TPaintBox;
    MODOpenDialog: TOpenDialog;
    RowNumberStringGrid: TStringGrid;
    WaveEditPopup: TPopupMenu;
    SynAnySyn1: TSynAnySyn;
    WavSaveDialog: TSaveDialog;
    ToolButton10: TToolButton;
    TrackerPopupEditEffect: TMenuItem;
    TrackerPopupTransposeOctaveUp: TMenuItem;
    TrackerPopupTransposeOctaveDown: TMenuItem;
    TrackerPopupTransposeSemiUp: TMenuItem;
    MenuItem30: TMenuItem;
    TrackerPopupTransposeSemiDown: TMenuItem;
    TrackerPopupUndo: TMenuItem;
    TrackerPopupRedo: TMenuItem;
    TrackerPopupSelectAll: TMenuItem;
    TrackerPopupSelectChannel: TMenuItem;
    TrackerPopupCut: TMenuItem;
    MenuItem29: TMenuItem;
    TrackerPopupCopy: TMenuItem;
    TrackerPopupPaste: TMenuItem;
    TrackerPopupErase: TMenuItem;
    TrackerPopupFloodPaste: TMenuItem;
    MenuItem8: TMenuItem;
    NoteHaltTimer: TTimer;
    TrackerGridPopup: TPopupMenu;
    StartVolTrackbar: TTrackBar;
    EnvChangeTrackbar: TTrackBar;
    LengthTrackbar: TTrackBar;
    SweepSizeTrackbar: TTrackBar;
    ToolButton5: TToolButton;
    WaveSaveDialog: TSaveDialog;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    LEDMeter1: TLEDMeter;
    LEDMeter2: TLEDMeter;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    OrderEditStringGrid: TStringGrid;
    EditCut1: TEditCut;
    HeaderControl1: THeaderControl;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    OctaveSpinEdit: TSpinEdit;
    GBSSaveDialog: TSaveDialog;
    StepSpinEdit: TSpinEdit;
    OscilloscopeUpdateTimer: TTimer;
    ExportGBSButton: TToolButton;
    ToolButton3: TToolButton;
    ToolButton9: TToolButton;
    Panel3: TPanel;
    PasteAction: TAction;
    CopyAction: TAction;
    HelpLookupManual: TAction;
    MenuBarActionList: TActionList;
    EditCopy1: TEditCopy;
    EditDelete1: TEditDelete;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    ExportWaveButton: TButton;
    FileExit1: TFileExit;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    FlowPanel1: TFlowPanel;
    ImageList1: TImageList;
    ImportWaveButton: TButton;
    CommentMemo: TMemo;
    Label20: TLabel;
    Label21: TLabel;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    DebugShiteButton: TMenuItem;
    N3: TMenuItem;
    N2: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    OrderEditPopup: TPopupMenu;
    InstrumentSaveDialog: TSaveDialog;
    ScrollBox1: TScrollBox;
    TicksPerRowSpinEdit: TSpinEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    PanicToolButton: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ExportGBButton: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    WaveEditNumberSpinner: TSpinEdit;
    WaveEditGroupBox: TGroupBox;
    Label19: TLabel;
    DebugPlayNoteButton: TMenuItem;
    WaveEditPaintBox: TPaintBox;
    RoutineNumberSpinner: TSpinEdit;
    Label17: TLabel;
    EnvelopePaintbox: TPaintBox;
    Label18: TLabel;
    Panel5: TPanel;
    SongEdit: TEdit;
    ArtistEdit: TEdit;
    SongInformationGroupbox: TGroupBox;
    Label15: TLabel;
    Label16: TLabel;
    SevenBitCounterCheckbox: TCheckBox;
    SweepTimeCombobox: TComboBox;
    RoutineSynedit: TSynEdit;
    WavesTabSheet: TTabSheet;
    WaveformCombobox: TComboBox;
    WaveVisualizer: TPaintBox;
    WaveVolumeCombobox: TComboBox;
    SweepDirectionCombobox: TComboBox;
    DutyCombobox: TComboBox;
    SquareGroupBox: TGroupBox;
    WaveGroupBox: TGroupBox;
    NoiseGroupBox: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LengthEnabledCheckbox: TCheckBox;
    InstrumentTypeCombobox: TComboBox;
    DirectionComboBox: TComboBox;
    InstrumentNameEdit: TEdit;
    InstrumentGroupBox: TGroupBox;
    EnvelopeGroupBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    SampleSongsMenuItem: TMenuItem;
    MenuItem5: TMenuItem;
    PageControl1: TPageControl;
    PaintBox1: TPaintBox;
    WavePaintbox: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    InstrumentNumberSpinner: TSpinEdit;
    Panel4: TPanel;
    Splitter1: TSplitter;
    PatternTabSheet: TTabSheet;
    InstrumentTabSheet: TTabSheet;
    GeneralTabSheet: TTabSheet;
    CommentsTabSheet: TTabSheet;
    RoutinesTabSheet: TTabSheet;
    StatusBar1: TStatusBar;
    TreeView1: TTreeView;
    TrackerGrid: TTrackerGrid;
    procedure Button1Click(Sender: TObject);
    procedure DecrementCurrentInstrumentActionExecute(Sender: TObject);
    procedure DeleteRowActionExecute(Sender: TObject);
    procedure DeleteRowActionUpdate(Sender: TObject);
    procedure DeleteRowForAllActionExecute(Sender: TObject);
    procedure DeleteRowForAllActionUpdate(Sender: TObject);
    procedure Duty1VisualizerClick(Sender: TObject);
    procedure ExportCMenuItemClick(Sender: TObject);
    procedure FileSaveAs1BeforeExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure GotoCommentsActionExecute(Sender: TObject);
    procedure GotoGeneralActionExecute(Sender: TObject);
    procedure GotoInstrumentsActionExecute(Sender: TObject);
    procedure GotoPatternsActionExecute(Sender: TObject);
    procedure GotoRoutinesActionExecute(Sender: TObject);
    procedure GotoWavesActionExecute(Sender: TObject);
    procedure HexWaveEditEditingDone(Sender: TObject);
    procedure IncrementCurrentInstrumentActionExecute(Sender: TObject);
    procedure InsertRowActionExecute(Sender: TObject);
    procedure InsertRowActionUpdate(Sender: TObject);
    procedure InsertRowForAllActionExecute(Sender: TObject);
    procedure InsertRowForAllActionUpdate(Sender: TObject);
    procedure MenuItem37Click(Sender: TObject);
    procedure MenuItem38Click(Sender: TObject);
    procedure ExportAsmMenuItemClick(Sender: TObject);
    procedure NoiseMacroPaintboxMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure NoiseMacroPaintboxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure NoiseMacroPaintboxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NoiseMacroPaintboxPaint(Sender: TObject);
    procedure OrderEditStringGridCellProcess(Sender: TObject; aCol,
      aRow: Integer; processType: TCellProcessType; var aValue: string);
    procedure OrderEditStringGridValidateEntry(sender: TObject; aCol,
      aRow: Integer; const OldValue: string; var NewValue: String);
    procedure PlayCursorActionExecute(Sender: TObject);
    procedure PlayOrderActionExecute(Sender: TObject);
    procedure ScrollBox1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ScrollBox1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure InstrumentComboBoxChange(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure CutActionExecute(Sender: TObject);
    procedure Duty1VisualizerPaint(Sender: TObject);
    procedure Duty2VisualizerPaint(Sender: TObject);
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure HeaderControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HeaderControl1SectionClick(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection);
    procedure HeaderControl1SectionResize(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection);
    procedure HelpLookupManualExecute(Sender: TObject);
    procedure ArtistEditChange(Sender: TObject);
    procedure CommentMemoChange(Sender: TObject);
    procedure DirectionComboBoxChange(Sender: TObject);
    procedure DutyComboboxChange(Sender: TObject);
    procedure EnvChangeSpinnerChange(Sender: TObject);
    procedure EnvelopePaintboxPaint(Sender: TObject);
    procedure ExportWaveButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ImportWaveButtonClick(Sender: TObject);
    procedure InstrumentExportButtonClick(Sender: TObject);
    procedure InstrumentImportButtonClick(Sender: TObject);
    procedure InstrumentNameEditChange(Sender: TObject);
    procedure InstrumentNumberSpinnerChange(Sender: TObject);
    procedure LengthSpinnerChange(Sender: TObject);
    procedure DebugPlayNoteButtonClick(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem31Click(Sender: TObject);
    procedure MenuItem33Click(Sender: TObject);
    procedure MenuItem34Click(Sender: TObject);
    procedure OnIncrementValueBy1Click(Sender: TObject);
    procedure OnDecrementValueBy1Click(Sender: TObject);
    procedure OnIncrementValueBy10Click(Sender: TObject);
    procedure OnDecrementValueBy10Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure NoiseVisualizerPaint(Sender: TObject);
    procedure NoteHaltTimerTimer(Sender: TObject);
    procedure OrderEditStringGridAfterSelection(Sender: TObject; aCol,
      aRow: Integer);
    procedure OrderEditStringGridColRowDeleted(Sender: TObject;
      IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure OrderEditStringGridColRowExchanged(Sender: TObject;
      IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure OrderEditStringGridColRowInserted(Sender: TObject;
      IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure OrderEditStringGridDblClick(Sender: TObject);
    procedure OrderEditStringGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PanicToolButtonClick(Sender: TObject);
    procedure PasteActionExecute(Sender: TObject);
    procedure OctaveSpinEditChange(Sender: TObject);
    procedure RoutineNumberSpinnerChange(Sender: TObject);
    procedure RoutineSyneditChange(Sender: TObject);
    procedure PlayStartActionExecute(Sender: TObject);
    procedure StepSpinEditChange(Sender: TObject);
    procedure StopActionExecute(Sender: TObject);
    procedure TicksPerRowSpinEditChange(Sender: TObject);
    procedure OscilloscopeUpdateTimerTimer(Sender: TObject);
    procedure ExportGBSButtonClick(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure ExportGBButtonClick(Sender: TObject);
    procedure TrackerPopupCopyClick(Sender: TObject);
    procedure TrackerPopupCutClick(Sender: TObject);
    procedure TrackerPopupEditEffectClick(Sender: TObject);
    procedure TrackerPopupEraseClick(Sender: TObject);
    procedure TrackerPopupFloodPasteClick(Sender: TObject);
    procedure TrackerPopupPasteClick(Sender: TObject);
    procedure TrackerPopupRedoClick(Sender: TObject);
    procedure TrackerPopupSelectAllClick(Sender: TObject);
    procedure TrackerPopupSelectChannelClick(Sender: TObject);
    procedure TrackerPopupTransposeOctaveDownClick(Sender: TObject);
    procedure TrackerPopupTransposeOctaveUpClick(Sender: TObject);
    procedure TrackerPopupTransposeSemiDownClick(Sender: TObject);
    procedure TrackerPopupTransposeSemiUpClick(Sender: TObject);
    procedure TrackerPopupUndoClick(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure WaveEditNumberSpinnerChange(Sender: TObject);
    procedure WaveEditPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WaveEditPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure WaveEditPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WaveEditPaintBoxPaint(Sender: TObject);
    procedure InstrumentTypeComboboxChange(Sender: TObject);
    procedure LengthEnabledCheckboxChange(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure SevenBitCounterCheckboxChange(Sender: TObject);
    procedure SongEditChange(Sender: TObject);
    procedure StartVolSpinnerChange(Sender: TObject);
    procedure SweepDirectionComboboxChange(Sender: TObject);
    procedure SweepSizeSpinnerChange(Sender: TObject);
    procedure SweepTimeComboboxChange(Sender: TObject);
    procedure WaveformComboboxChange(Sender: TObject);
    procedure WavePaintboxPaint(Sender: TObject);
    procedure WaveVisualizerPaint(Sender: TObject);
    procedure WaveVolumeComboboxChange(Sender: TObject);
  private
    Song: TSong;
    CurrentInstrument: ^TInstrument;
    CurrentWave: ^TWave;
    CurrentInstrumentBank: TInstrumentType;
    LoadedFileName: String;

    PreviewingInstrument: Integer;
    DrawingWave, DrawingMacro: Boolean;
    Playing: Boolean;
    LoadingFile: Boolean;
    SaveSucceeded: Boolean;
    ScopesOn: Boolean;

    {PatternsNode, }
    WaveInstrumentsNode,
    NoiseInstrumentsNode,
    DutyInstrumentsNode,
    WavesNode,
    RoutinesNode: TTreeNode;

    VisualizerBuffer: TBGRABitmap;

    InFDCallback: Boolean;

    procedure CustomExceptionHandler(Sender: TObject; E: Exception);

    procedure ChangeToSquare;
    procedure ChangeToWave;
    procedure ChangeToNoise;
    procedure LoadInstrument(Bank: TInstrumentType; Instr: Integer);
    procedure LoadWave(Wave: Integer);
    procedure LoadSong(Filename: String);
    procedure ReloadPatterns;
    procedure CopyOrderGridToOrderMatrix;
    procedure CopyOrderMatrixToOrderGrid;
    procedure CopyWaveIntoWaveRam(Wave: Integer);
    function ConvertWaveToHexString(Wave: Integer): String;

    function RenderPreviewROM: Boolean;
    function RenderSongToFile(Filename: String; Mode: TExportMode = emNormal): Boolean;

    function GetPreviewReady: Boolean;
    procedure GetROMReady(ROM: String);
    procedure HaltPlayback;
    procedure FDCallback;

    procedure OnFD(var Msg: TLMessage); message LM_FD;
    procedure OnSampleSongMenuItemClicked(Sender: TObject);

    procedure CreateKeymap;
    procedure RecreateTrackerGrid;
    procedure UpdateUIAfterLoad(FileName: String = '');
    procedure UpdateWindowTitle;
    procedure UpdateHexWaveTextbox;
    function CheckUnsavedChanges: Boolean;

    procedure DrawWaveform(PB: TPaintBox; Wave: TWave);
    procedure DrawEnvelope(PB: TPaintBox);
    procedure DrawVizualizer(PB: TPaintBox; Channel: Integer);

    procedure PreviewInstrument(Note: Integer; Instr: Integer; SquareOnCh2: Boolean = False); overload;
    procedure PreviewInstrument(Note: Integer; Instr: TInstrument; SquareOnCh2: Boolean = False); overload;
    procedure PreviewC5;
    procedure Panic;
  public
    procedure OnTrackerGridResize(Sender: TObject);
    procedure OnTrackerGridCursorOutOfBounds;
  end;

var
  frmTracker: TfrmTracker;

implementation

{$R *.lfm}

{ TfrmTracker }

procedure TfrmTracker.UpdateUIAfterLoad(FileName: String = '');
var
  I: Integer;
begin
  HaltPlayback;

  LoadingFile := True; // HACK!!!!!
  LoadInstrument(itSquare, 1);
  LoadWave(0);

  RoutineSynedit.Text := Song.Routines[0];
  RoutineNumberSpinner.Value := 0;

  SongEdit.Text := Song.Name;
  ArtistEdit.Text := Song.Artist;
  CommentMemo.Text := Song.Comment;

  TicksPerRowSpinEdit.Value := Song.TicksPerRow;

  LoadedFilename := FileName;
  UpdateWindowTitle;

  RecreateTrackerGrid;
  CopyOrderMatrixToOrderGrid;

  while InstrumentComboBox.Items.Count > 1 do
    InstrumentComboBox.Items.Delete(1);

  for I := Low(Song.Instruments.Duty) to High(song.Instruments.Duty) do
    InstrumentComboBox.Items.Add('Square '+IntToStr(I)+': '+Song.Instruments.Duty[ModInst(I)].Name);
  for I := Low(Song.Instruments.Wave) to High(song.Instruments.Wave) do
    InstrumentComboBox.Items.Add('Wave '+IntToStr(I)+': '+Song.Instruments.Wave[ModInst(I)].Name);
  for I := Low(Song.Instruments.Noise) to High(song.Instruments.Noise) do
    InstrumentComboBox.Items.Add('Noise '+IntToStr(I)+': '+Song.Instruments.Noise[ModInst(I)].Name);

  InstrumentComboBox.ItemIndex := 0;
  //TrackerGrid.SelectedInstrument := 1; //ModInst(InstrumentComboBox.ItemIndex);

  for I := Low(TInstrumentBank) to High(TInstrumentBank) do begin
    DutyInstrumentsNode.Items[I-1].Text := IntToStr(I)+': '+Song.Instruments.Duty[I].Name;
    WaveInstrumentsNode.Items[I-1].Text := IntToStr(I)+': '+Song.Instruments.Wave[I].Name;
    NoiseInstrumentsNode.Items[I-1].Text := IntToStr(I)+': '+Song.Instruments.Noise[I].Name;
  end;

  LoadingFile := False; // HACK!!!!

  OrderEditStringGrid.Row := 1;
  ReloadPatterns;

  PageControl1.ActivePageIndex := 0;
end;

procedure TfrmTracker.UpdateWindowTitle;
var
  S: String;
begin
  S := 'hUGETracker';

  if Trim(LoadedFileName) <> '' then
    S += ' - '+LoadedFilename;

  if Trim(Song.Name) <> '' then begin
    S += ' - [';
    if Trim(Song.Artist) <> '' then
      S += Song.Artist+' - ';

    S += Song.Name+']';
  end;

  Self.Caption := S;
end;

procedure TfrmTracker.UpdateHexWaveTextbox;
begin
  HexWaveEdit.Text := ConvertWaveToHexString(WaveEditNumberSpinner.Value);
end;

function TfrmTracker.CheckUnsavedChanges: Boolean;
begin
  Result := True;

  case MessageDlg('Save?', 'Do you want to save your work before closing this file?',
  mtWarning, [mbYes, mbNo, mbCancel], 0) of
    mrYes: begin
      FileSaveAs1.Execute;
      Exit(SaveSucceeded)
    end;
    mrNo: Exit(True);
    mrCancel: Exit(False)
  end;
end;

procedure TfrmTracker.DrawWaveform(PB: TPaintBox; Wave: TWave);
var
  Interval{, HInterval}: Integer;
  I: Integer;
  W, H : Integer;
begin
  W := PB.Width;
  H := PB.Height;

  Interval := W div 31;
  //HInterval := H div $10;
  With PB.Canvas do begin
    Brush.Color := clGameboyBlack;
    Clear;

    {Pen.Width := 1;
    Pen.Color := StringToColor('$103800');
    for I := 0 to 32 do
      Line(I*Interval, 0, I*Interval, H);

    for I := 0 to $10 do
      Line(0, I*HInterval, W, I*HInterval);}

    Brush.Color := clGameboyMidGreen;
    Pen.Color := clGameboyMidGreen;
    Pen.Width := 2;
    MoveTo(0, H);
    for I := Low(Wave) to High(Wave) do
      LineTo(I*Interval, Trunc((Wave[I]/$F)*H));
    LineTo(W, Trunc((Wave[0]/$F)*H));
  end;
end;


procedure TfrmTracker.DrawEnvelope(PB: TPaintBox);
var
  Interval, HInterval: Integer;
  I: Integer;
  W, H, V, S, L: Integer;
begin
  W := PB.Width;
  H := PB.Height-3;
  V := Min(CurrentInstrument^.InitialVolume,15);
  S := Min(CurrentInstrument^.VolSweepAmount,7)*2;
  case CurrentInstrument^.LengthEnabled of
    True:   L := (64-CurrentInstrument^.Length) div 2;
    False:  L := 400;
  end;
  Interval := 1 {W div 260};
  HInterval := H div 15;

  With PB.Canvas do begin
    Brush.Color := clGameboyBlack;
    Clear;
    Pen.Color := clGameboyMidGreen;
    Pen.Width := 3;
    MoveTo(0, H-V*HInterval); // Start volume

    if S{weep} = 0 then begin
       LineTo(L*Interval, H-V*HInterval); // no sweep
       LineTo(L*Interval, H);
       LineTo(W, H);
    end
    else begin
      case CurrentInstrument^.VolSweepDirection of
        Down: begin  // Envelope down, check length cut
           For I := 0 to V do
              LineTo( Min(I*S,L)*Interval, H-(V-I)*HInterval);
           LineTo(W, H);
           LineTo(Trunc(L*Interval), H);
           end;
        Up: begin    // Envelope up, check length cut
            For I := 0 to 15-V do
                LineTo( Min(I*S,L)*Interval, (15-V-I)*HInterval+9);
            LineTo(L*Interval, H-15*HInterval);
            LineTo(L*Interval, H);
            LineTo(W, H);
            end;
        end;
    end;
    // Width = 64th/sec sweep or 256th/sec length,
    // Envelope = sweep length(0-7 * volume(0-15) * 1/64. max 105 (1.64 sec)
    // Length = Length(0-63) / 1/256, max .25 sec.
    // for simplicity, Sweep is mult 2, length is div 2, both 1/128 sec.
  end;
end;

procedure TfrmTracker.DrawVizualizer(PB: TPaintBox; Channel: Integer);
var
  I, J, X: Integer;
begin
  // Thanks to Handoko on the Lazarus forums for this repainting tip!
  // https://forum.lazarus.freepascal.org/index.php/topic,48231.msg347235.html#msg347235
  with VisualizerBuffer.Canvas do begin
    MoveTo(0, PB.Height div 2);

    Brush.Color := clGameboyBlack;
    Clear;

    Pen.Color := clGray;
    Rectangle(0, 0, PB.Width, PB.Height);

    Pen.Width := 2;

    if not snd[Channel].ChannelOFF then begin
      Pen.Color := clGameboyMidGreen;

      X := 0;
      I := 0;
      J := SampleBuffers[Channel].Cursor;
      while I < SAMPLE_BUFFER_SIZE-1 do begin
        LineTo(X, (PB.Height div 2) +
          Trunc((SampleBuffers[Channel].BufferL[J]/512)*PB.Height) +
          Trunc((SampleBuffers[Channel].BufferR[J]/512)*PB.Height));
        J := (J+1) mod SAMPLE_BUFFER_SIZE;

        X := Trunc((I/SAMPLE_BUFFER_SIZE)*PB.Width);
        Inc(I, 2);
      end;
    end
    else begin
      Pen.Color := TColor($0C0094); // TODO: what color is this?
      Line(0, 0, PB.Width, PB.Height);
      Line(0, PB.Height, PB.Width, 0);
    end;
  end;

  VisualizerBuffer.Draw(PB.Canvas, 0, 0, True);
end;

procedure TfrmTracker.PreviewInstrument(Note: Integer; Instr: Integer;
  SquareOnCh2: Boolean = False);
begin
  PreviewInstrument(Note, Song.Instruments.All[Instr], SquareOnCh2);
end;

procedure TfrmTracker.PreviewInstrument(Note: Integer; Instr: TInstrument;
  SquareOnCh2: Boolean = False);
var
  Regs: TRegisters;
  I, Addr, Freq: Integer;
begin
  Freq := NotesToFreqs.KeyData[Note];

  LockPlayback;
  with Instr do
  begin
    case Type_ of
      itSquare: begin
        Regs := SquareInstrumentToRegisters(Freq, True, Instr);
        if SquareOnCh2 then begin
          Spokeb(NR21, Regs.NR11);
          Spokeb(NR22, Regs.NR12);
          Spokeb(NR23, Regs.NR13);
          Spokeb(NR24, Regs.NR14);
        end
        else begin
          Spokeb(NR10, Regs.NR10);
          Spokeb(NR11, Regs.NR11);
          Spokeb(NR12, Regs.NR12);
          Spokeb(NR13, Regs.NR13);
          Spokeb(NR14, Regs.NR14);
        end;
      end;
      itWave: begin
        CopyWaveIntoWaveRam(Waveform);
        Regs := WaveInstrumentToRegisters(Freq, True, Instr);

        Spokeb(NR30, Regs.NR30);
        Spokeb(NR31, Regs.NR31);
        Spokeb(NR32, Regs.NR32);
        Spokeb(NR33, Regs.NR33);
        Spokeb(NR34, Regs.NR34)
      end;
      itNoise: begin
        Regs := NoiseInstrumentToRegisters(Freq, True, Instr);
        if Instr.LengthEnabled then
          Regs.NR41 := Regs.NR41 or %01000000;
        if Instr.CounterStep = swSeven then
          Regs.NR41 := Regs.NR41 or %10000000;

        Addr := SymbolAddress(SYM_HALT_INSTR);
        Spokeb(Addr, Regs.NR42);
        Spokeb(Addr+1, Regs.NR41);

        for I := 0 to 5 do begin
          Spokeb(Addr+2+I, Byte(Instr.NoiseMacro[I]));
        end;

        PokeSymbol(SYM_HALT_MACRO_INDEX, 1);
        PokeSymbol(SYM_HALT_NOTE, Note);
      end;
    end;
  end;
  UnlockPlayback;
end;

procedure TfrmTracker.PreviewC5;
begin
  if not LoadingFile then begin
    PreviewInstrument(C_5, UnmodInst(CurrentInstrumentBank, InstrumentNumberSpinner.Value));
    NoteHaltTimer.Enabled := False;
    NoteHaltTimer.Enabled := True
  end;
end;

procedure TfrmTracker.Panic;
begin
  LockPlayback;
  // Silence CH1
  Spokeb(NR12, 0);
  Spokeb(NR14, %10000000);

  // Silence CH2
  Spokeb(NR22, 0);
  Spokeb(NR24, %10000000);

  // Silence CH3
  Spokeb(NR30, 0);

  // Silence CH4
  Spokeb(NR42, 0);
  Spokeb(NR44, %10000000);
  UnlockPlayback;
end;

procedure TfrmTracker.OnTrackerGridResize(Sender: TObject);
var
  I: Integer;
begin
  // Fix the size of the channel headers
  for I := 1 to HeaderControl1.Sections.Count-1 do
    HeaderControl1.Sections.Items[I].Width := TrackerGrid.ColumnWidth;
end;

procedure TfrmTracker.OnTrackerGridCursorOutOfBounds;
begin
  if  (TrackerGrid.Cursor.Y > High(TPattern))
  and (OrderEditStringGrid.Row < OrderEditStringGrid.RowCount-1) then begin
    OrderEditStringGrid.Row := OrderEditStringGrid.Row+1;
    TrackerGrid.Cursor.Y := Low(TPattern);
  end;

  if  (TrackerGrid.Cursor.Y < Low(TPattern))
  and (OrderEditStringGrid.Row > 1) then begin
    OrderEditStringGrid.Row := OrderEditStringGrid.Row-1;
    TrackerGrid.Cursor.Y := High(TPattern);
  end;
end;

procedure TfrmTracker.LoadWave(Wave: Integer);
begin
  CurrentWave := @Song.Waves[Wave];
  WaveEditPaintBox.Invalidate;
  UpdateHexWaveTextbox;
end;

procedure TfrmTracker.LoadSong(Filename: String);
var
  Stream: TStream;
begin
  DestroySong(Song);

  stream := TFileStream.Create(FileName, fmOpenRead);
  try
    ReadSongFromStream(stream, Song);
  finally
    stream.Free;
  end;
  FileSaveAs1.Dialog.FileName := FileName;

  UpdateUIAfterLoad(Filename);
end;

procedure TfrmTracker.ReloadPatterns;
var
  I: Integer;
  OrderNum: Integer;
begin
  if LoadingFile then Exit;

  for I := 0 to 3 do begin
    OrderNum := 0;
    TryStrToInt(
      OrderEditStringGrid.Cells[I+1, OrderEditStringGrid.Row],
      OrderNum
    );
    TrackerGrid.LoadPattern(I, OrderNum);
  end;

  CopyOrderGridToOrderMatrix;
end;

procedure TfrmTracker.CopyOrderGridToOrderMatrix;
var
  C, R, OrderNum: Integer;
begin
  // Copy the data in the grid to the song's order matrix. Not super efficient
  // but it's fast enough
  with OrderEditStringGrid do begin
    for C := 0 to 3 do begin
      SetLength(Song.OrderMatrix[C], RowCount);
      for R := 0 to RowCount-2 do begin
        OrderNum := 0;
        TryStrToInt(Cells[C+1, R+1], OrderNum);
        Song.OrderMatrix[C, R] := OrderNum;
      end;
    end;
  end;
end;

procedure TfrmTracker.CopyOrderMatrixToOrderGrid;

function IntToStr_(X: Integer): String;
begin
  if X = 0 then Result := '' else Result := IntToStr(X);
end;

var
  MaxRows, C, R: Integer;
begin
  MaxRows := -1;
  OrderEditStringGrid.Clean([gzNormal]);

  for C := Low(TOrderMatrix) to High(TOrderMatrix) do
    if Length(Song.OrderMatrix[C]) > MaxRows then
      MaxRows := Length(Song.OrderMatrix[C]);

  OrderEditStringGrid.RowCount := MaxRows;

  for C := Low(Song.OrderMatrix) to High(Song.OrderMatrix) do begin
    for R := 0 to High(Song.OrderMatrix[C])-1 do
      OrderEditStringGrid.Cells[C+1, R+1] := IntToStr_(Song.OrderMatrix[C, R]);
  end;

  // Show zeroes where they belong
  for R := 1 to OrderEditStringGrid.RowCount-1 do
    if Trim(OrderEditStringGrid.Rows[R].Text) <> '' then
      for C := 1 to OrderEditStringGrid.ColCount-1 do
        if OrderEditStringGrid.Cells[C, R] = '' then
          OrderEditStringGrid.Cells[C, R] := '0';
end;

procedure TfrmTracker.CopyWaveIntoWaveRam(Wave: Integer);
var
  NewWaveform: T4bitWave;
  I: Integer;
begin
  NewWaveform := ConvertWaveform(Song.Waves[Wave]);
  for I := Low(NewWaveform) to High(NewWaveform) do
    Spokeb(AUD3_WAVE_RAM+I, NewWaveform[I]);
end;

function TfrmTracker.ConvertWaveToHexString(Wave: Integer): String;
var
  I: Integer;
begin
  Result := '';
  for I := Low(CurrentWave^) to High(CurrentWave^) do
    Result += hexStr(CurrentWave^[I], 1);
end;

function TfrmTracker.RenderPreviewROM: Boolean;
begin
  Result := RenderSongToFile('preview.gb', emPreview);
end;

function TfrmTracker.RenderSongToFile(Filename: String; Mode: TExportMode = emNormal): Boolean;
begin
  Result := False;
  try
    AssembleSong(Song, Filename, Mode);
    Result := True;
  except
    on E: ECodegenRenameException do begin
      MessageDlg('Error!',
        'Couldn''t create file at ' + Filename +
        '. Make sure your path is correct!',
        mtError,
        [mbOK],
        0);
    end;
    on E: EAssemblyException do begin
      MessageDlg(
        'Error!',
        'There was an error assembling the song for playback.' + LineEnding +
        LineEnding + E.ProgramName + ' output:' + LineEnding + E.Message +
        LineEnding + LineEnding +
        'Please report this issue on the hUGETracker GitHub issues page, and ' +
        'post your song file!',
        mtError,
        [mbOK],
        0);

      {$ifdef PRODUCTION}
      OpenURL('https://github.com/SuperDisk/hUGETracker/issues');
      {$endif}
    end;
  end;
end;

function TfrmTracker.GetPreviewReady: Boolean;
var
  I: Integer;
begin
  if RenderPreviewROM then begin
    LockPlayback;

    // Load the new symbol table
    ParseSymFile('render/preview.sym');

    // Start emulation on the rendered preview binary
    GetROMReady('render/preview.gb');

    for I := 1 to 4 do
      snd[I].ChannelOFF := HeaderControl1.Sections[I].ImageIndex = 0;

    Result := True;
  end
  else Result := False;
end;

procedure TfrmTracker.GetROMReady(ROM: String);
begin
  z80_reset;
  ResetSound;
  enablesound;

  vars.FDCallback := @Self.FDCallback;

  load(ROM);
end;

procedure TfrmTracker.HaltPlayback;
begin
  Playing := False;

  TrackerGrid.HighlightedRow := -1;
  ToolButton2.ImageIndex := 74;

  LockPlayback;
  GetROMReady('halt.gb');
  ParseSymFile('halt.sym');
  UnlockPlayback;
end;

procedure TfrmTracker.FDCallback;
begin
  PostMessage(frmTracker.Handle, LM_FD, 0, 0);
end;

procedure TfrmTracker.OnFD(var Msg: TLMessage);
begin
  InFDCallback := True; // HACK!!!!!!!!!
  TrackerGrid.HighlightedRow := PeekSymbol(SYM_ROW);
  OrderEditStringGrid.Row := (PeekSymbol(SYM_CURRENT_ORDER) div 2) + 1;
  InFDCallback := False; // HACK!!!!!!!!
end;

procedure TfrmTracker.OnSampleSongMenuItemClicked(Sender: TObject);
begin
  LoadSong('Sample Songs/'+TMenuItem(Sender).Caption);
end;

procedure TfrmTracker.CreateKeymap;
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

procedure TfrmTracker.RecreateTrackerGrid;
var
  I: Integer;
begin
  if Assigned(TrackerGrid) then TrackerGrid.Free;
  TrackerGrid := TTrackerGrid.Create(Self, ScrollBox1, Song.Patterns);
  TrackerGrid.OnResize:=@OnTrackerGridResize;
  TrackerGrid.OnCursorOutOfBounds:=@OnTrackerGridCursorOutOfBounds;
  TrackerGrid.Left := RowNumberStringGrid.Left + RowNumberStringGrid.Width;
  TrackerGrid.FontSize := TrackerSettings.PatternEditorFontSize;
  RowNumberStringGrid.DefaultRowHeight := TrackerGrid.RowHeight;
  ScopesOn := TrackerSettings.UseScopes;

  // Fix the size of the channel headers
  for I := 1 to HeaderControl1.Sections.Count-1 do
    HeaderControl1.Sections.Items[I].Width := TrackerGrid.ColumnWidth;

  TrackerGrid.PopupMenu := TrackerGridPopup;
end;

procedure TfrmTracker.LoadInstrument(Bank: TInstrumentType; Instr: Integer);
var
  CI: ^TInstrument;
begin
  CurrentInstrumentBank := Bank;
  case Bank of
    itSquare: CurrentInstrument := @Song.Instruments.Duty[Instr];
    itWave:   CurrentInstrument := @Song.Instruments.Wave[Instr];
    itNoise:  CurrentInstrument := @Song.Instruments.Noise[Instr];
  end;
  CI := CurrentInstrument;

  InstrumentTypeComboBox.ItemIndex := Integer(CurrentInstrumentBank);
  InstrumentNumberSpinner.Value := Instr;
  InstrumentNameEdit.Text := CI^.Name;
  LengthEnabledCheckbox.Checked := CI^.LengthEnabled;
  LengthTrackbar.Position := CI^.Length;

  case CI^.Type_ of
    itSquare: begin
      InstrumentTypeComboBox.Text := 'Square';
      ChangeToSquare;
    end;
    itWave: begin
      InstrumentTypeComboBox.Text := 'Wave';
      ChangeToWave;
    end;
    itNoise: begin
      InstrumentTypeComboBox.Text := 'Noise';
      ChangeToNoise;
    end;
  end;

  StartVolTrackbar.Position := CI^.InitialVolume;
  case CI^.VolSweepDirection of
    Up: DirectionComboBox.Text := 'Up';
    Down: DirectionComboBox.Text := 'Down';
  end;
  EnvChangeTrackbar.Position := CI^.VolSweepAmount;

  case CI^.Type_ of
    itSquare: begin
      SweepTimeCombobox.ItemIndex := CI^.SweepTime;
      case CI^.SweepIncDec of
        Up: SweepDirectionCombobox.Text := 'Up';
        Down: SweepDirectionCombobox.Text := 'Down';
      end;
      SweepSizeTrackbar.Position := CI^.SweepShift;
      DutyCombobox.ItemIndex := CI^.Duty;
    end;

    itWave: begin
      WaveVolumeCombobox.ItemIndex := CI^.OutputLevel;
      WaveformCombobox.ItemIndex := CI^.Waveform;
    end;

    itNoise: begin
      //ShiftClockTrackbar.Position := CI^.ShiftClockFreq;
      //DivRatioTrackbar.Position := CI^.DividingRatio;
      SevenBitCounterCheckbox.Checked := CI^.CounterStep = swSeven;
    end;
  end;

  WavePaintbox.Invalidate;
  EnvelopePaintBox.Invalidate;
  NoiseMacroPaintbox.Invalidate;
end;

procedure TfrmTracker.ChangeToSquare;
begin
  EnvelopeGroupBox.Visible := True;
  SquareGroupBox.Visible := True;
  WaveGroupBox.Visible := False;
  NoiseGroupBox.Visible := False;

  LengthTrackbar.Max := 63;
end;

procedure TfrmTracker.ChangeToWave;
begin
  EnvelopeGroupBox.Visible := False;
  SquareGroupBox.Visible := False;
  WaveGroupBox.Visible := True;
  NoiseGroupBox.Visible := False;

  LengthTrackbar.Max := 255;
end;

procedure TfrmTracker.ChangeToNoise;
begin
  EnvelopeGroupBox.Visible := True;
  SquareGroupBox.Visible := False;
  WaveGroupBox.Visible := False;
  NoiseGroupBox.Visible := True;

  LengthTrackbar.Max := 63;
end;

procedure TfrmTracker.WavePaintboxPaint(Sender: TObject);
begin
  if not WaveGroupBox.Enabled then begin
    with WavePaintbox.Canvas do begin
      Brush.Color := clGameboyBlack;
      Clear;
    end;
  end
  else if WaveformCombobox.ItemIndex > -1 then
    DrawWaveform(WavePaintbox, Song.Waves[WaveformCombobox.ItemIndex]);
end;

procedure TfrmTracker.WaveVisualizerPaint(Sender: TObject);
begin
    DrawVizualizer(WaveVisualizer, 3);
end;

procedure TfrmTracker.WaveVolumeComboboxChange(Sender: TObject);
begin
  CurrentInstrument^.OutputLevel := WaveVolumeCombobox.ItemIndex;
end;

procedure TfrmTracker.CustomExceptionHandler(Sender: TObject; E: Exception);
var
  Stream: TStream;
  G: TGUID;
  OutName: String;
begin
  CreateGuid(G);
  OutName := 'BACKUP_'+GUIDToString(G)+'.uge';

  stream := TFileStream.Create(OutName, fmCreate);
  try
    WriteSongToStream(stream, Song);
  finally
    stream.Free;
  end;

  MessageDlg('Error',
    'An exception in hUGETracker has occured.' + LineEnding +
    'Your song has been backed up to the hUGETracker folder, so don''t worry!' + LineEnding+LineEnding+
    'Please report this issue on GitHub, or contact me directly via Discord or Email.'+LineEnding+LineEnding+
    'Email: '+MaintainerEmail+LineEnding+
    'Discord: '+MaintainerDiscord+LineEnding+LineEnding+
    'The error is: '+E.ClassName+' with message '+E.Message,
    mtError,
    [mbOK], 0);

  Application.Terminate;
end;

procedure TfrmTracker.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Brush.Color := clBlack;
  PaintBox1.Canvas.Pen.Color := clBlack;
  PaintBox1.Canvas.Clear;
  if WaveformCombobox.ItemIndex > -1 then
    DrawWaveform(PaintBox1, Song.Waves[WaveformCombobox.ItemIndex]);
end;

procedure TfrmTracker.SevenBitCounterCheckboxChange(Sender: TObject);
begin
  if SevenBitCounterCheckbox.Checked then
    CurrentInstrument^.CounterStep := swSeven
  else
    CurrentInstrument^.CounterStep := swFifteen;
end;

procedure TfrmTracker.SongEditChange(Sender: TObject);
begin
  Song.Name := SongEdit.Text;
  UpdateWindowTitle;
end;

procedure TfrmTracker.StartVolSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.InitialVolume := Round(StartVolTrackbar.Position);
  EnvelopePaintBox.Invalidate;
end;

procedure TfrmTracker.SweepDirectionComboboxChange(Sender: TObject);
begin
  case SweepDirectionComboBox.Text of
    'Up': CurrentInstrument^.SweepIncDec := Up;
    'Down': CurrentInstrument^.SweepIncDec := Down;
  end;
  EnvelopePaintBox.Invalidate;
end;

procedure TfrmTracker.SweepSizeSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.SweepShift := Round(SweepSizeTrackbar.Position);
end;

procedure TfrmTracker.SweepTimeComboboxChange(Sender: TObject);
begin
  CurrentInstrument^.SweepTime := SweepTimeCombobox.ItemIndex;
end;

procedure TfrmTracker.WaveformComboboxChange(Sender: TObject);
begin
  CurrentInstrument^.Waveform:=WaveformCombobox.ItemIndex;
  WavePaintbox.Invalidate;
end;

procedure TfrmTracker.InstrumentTypeComboboxChange(Sender: TObject);
begin
  case InstrumentTypeCombobox.ItemIndex of
    0: CurrentInstrumentBank := itSquare;
    1: CurrentInstrumentBank := itWave;
    2: CurrentInstrumentBank := itNoise;
  end;
  LoadInstrument(CurrentInstrumentBank, InstrumentNumberSpinner.Value);
end;

procedure TfrmTracker.FormCreate(Sender: TObject);
var
  PUI: PtrUint;
  SampleSongs: TStringList;
  S: String;
  MenuItem: TMenuItem;
  I: Integer;
begin
  {if Screen.Fonts.IndexOf('PixeliteTTF') = -1 then
    MessageDlg('Warning', 'You don''t have the Pixelite font installed. '+
    'On Windows, this probably means you didn''t extract hUGETracker before running it, '+
    'so please extract the .zip and run again! On Linux, you need to manually install the '+
    'font file, so please install PixeliteTTF.ttf and relaunch. Thanks!',
    mtWarning, [mbOk], 0);}

  if (not FileExists('PixeliteTTF.ttf'))
  or (not FileExists('halt.gb'))
  or (not DirectoryExists('hUGEDriver')) then begin
    MessageDlg('Error',
      'hUGETracker can''t load a required file which comes with '+
      'the tracker. This likely means that you haven''t extracted the program ' +
      'before running it. Please do so, and relaunch. Thanks!',
      mtError, [mbOk], 0);
    Application.Terminate;
  end;

  {$ifdef PRODUCTION}
  Application.OnException := @CustomExceptionHandler;
  {$endif}

  VisualizerBuffer := TBGRABitmap.Create(Duty1Visualizer.Width, Duty1Visualizer.Height);

  PreviewingInstrument := -1;

  // Fetch the tree items
  with TreeView1 do begin
    //PatternsNode := Items[0];
    DutyInstrumentsNode := Items[0].Items[0];
    WaveInstrumentsNode := Items[0].Items[1];
    NoiseInstrumentsNode := Items[0].Items[2];

    // TODO: Find out how to actually do this... WTF?
    WavesNode := Items[4];
    RoutinesNode := Items[5];
  end;

  for PUI := 1 to 15 do begin
    TreeView1.Items.AddChild(DutyInstrumentsNode, IntToStr(PUI)+':').Data := {%H-}Pointer(PUI);
    TreeView1.Items.AddChild(WaveInstrumentsNode, IntToStr(PUI)+':').Data := {%H-}Pointer(PUI);
    TreeView1.Items.AddChild(NoiseInstrumentsNode, IntToStr(PUI)+':').Data := {%H-}Pointer(PUI);
  end;

  for PUI := 0 to 15 do begin
    TreeView1.Items.AddChild(WavesNode, 'Wave '+IntToStr(PUI)).Data := {%H-}Pointer(PUI);
    TreeView1.Items.AddChild(RoutinesNode, 'Routine '+IntToStr(PUI)).Data := {%H-}Pointer(PUI);
  end;

  InitializeSong(Song);
  LoadDefaultInstruments(Song);

  // Create pattern editor control
  RecreateTrackerGrid;
  CreateKeymap;

  // Initialize ticks per row
  Song.TicksPerRow := TicksPerRowSpinEdit.Value;

  LoadInstrument(itSquare, 1);
  LoadWave(0);

  // Initialize order table (InitializeSong creates the default order table)
  CopyOrderMatrixToOrderGrid;

  // Manually resize the fixed column in the order editor
  OrderEditStringGrid.ColWidths[0]:=50;

  // Get the emulator ready to make sound...
  EnableSound;
  HaltPlayback;
  StartPlayback;

  // Start the Oscilloscope repaint timer
  OscilloscopeUpdateTimer.Enabled := ScopesOn;
  Panel2.Visible := ScopesOn;

  // Switch to general tab sheet
  PageControl1.ActivePageIndex := 0;

  // Add the row numbers to the string grid
  for I := 0 to RowNumberStringGrid.RowCount-1 do
    RowNumberStringGrid.Cells[0, I] := IntToStr(I);

  {$ifdef DEVELOPMENT}
  DebugShiteButton.Visible := True;
  DebugPlayNoteButton.Visible := True;
  {$endif}

  // Load sample songs list
  SampleSongs := FindAllFiles('./Sample Songs/', '*.uge', False);
  if SampleSongs.Count > 0 then
    for S in SampleSongs do begin
      MenuItem := TMenuItem.Create(MainMenu1);
      MenuItem.Caption := ExtractFileName(S);
      MenuItem.OnClick := @OnSampleSongMenuItemClicked;
      SampleSongsMenuItem.Add(MenuItem);
    end
  else
    SampleSongsMenuItem.Enabled := False;

  // If a command line param was passed, try to open it
  if FileExists(ParamStr(1)) and (ExtractFileExt(ParamStr(1)) = '.uge') then begin
    LoadSong(ParamStr(1));
    UpdateUIAfterLoad(ParamStr(1));
  end
  else
    UpdateUIAfterLoad;
end;

procedure TfrmTracker.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Note: Integer;
  Freq: Integer;
begin
  // Guard conditions
  if TrackerGrid.Cursor.SelectedPart <> cpNote then Exit;
  if Shift <> [] then Exit;
  if not Keybindings.TryGetData(Key, Note) then Exit;
  if not TrackerSettings.PreviewWhenPlacing then Exit;

  Inc(Note, OctaveSpinEdit.Value*12);
  EnsureRange(Note, LOWEST_NOTE, HIGHEST_NOTE);

  if PreviewingInstrument <> Note then
    PreviewingInstrument := -1;

  if (PreviewingInstrument > -1) or
     (not (ActiveControl = TrackerGrid)) or
     (InstrumentComboBox.ItemIndex <= 0)
  then exit;

  if not NotesToFreqs.TryGetData(Note, Freq) then Exit;

  case TrackerGrid.Cursor.X of
    0..1: InstrumentComboBox.ItemIndex := UnmodInst(itSquare, TrackerGrid.SelectedInstrument);
    2:    InstrumentComboBox.ItemIndex := UnmodInst(itWave, TrackerGrid.SelectedInstrument);
    3:    InstrumentComboBox.ItemIndex := UnmodInst(itNoise, TrackerGrid.SelectedInstrument);
  end;

  if TrackerGrid.Cursor.X = 1 then
    PreviewInstrument(Note, InstrumentComboBox.ItemIndex, True)
  else
    PreviewInstrument(Note, InstrumentComboBox.ItemIndex, False);

  PreviewingInstrument := Note;
end;

procedure TfrmTracker.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if PreviewingInstrument <> -1 then
    Panic;
  PreviewingInstrument := -1;
end;

procedure TfrmTracker.ImportWaveButtonClick(Sender: TObject);
var
  F: file of TWave;
begin
  OpenDialog1.Filter := 'hUGETracker waves|*.ugw';
  if OpenDialog1.Execute then begin
    assignfile(F, OpenDialog1.FileName);
    reset(F);
    Read(F, Song.Waves[WaveEditNumberSpinner.Value]);
    CloseFile(F);
    WaveEditPaintBox.Invalidate;
  end;
end;

procedure TfrmTracker.ExportWaveButtonClick(Sender: TObject);
var F: file of TWave;
begin
  if WaveSaveDialog.Execute then begin
    AssignFile(F, WaveSaveDialog.FileName);
    Rewrite(F);
    Write(F, CurrentWave^);
    CloseFile(F);
  end;
end;

procedure TfrmTracker.InstrumentExportButtonClick(Sender: TObject);
var
  F: file of TInstrument;
begin
  if InstrumentSaveDialog.Execute then begin
    AssignFile(F, InstrumentSaveDialog.FileName);
    Rewrite(F);
    Write(F, CurrentInstrument^);
    CloseFile(F);
  end;
end;

procedure TfrmTracker.InstrumentImportButtonClick(Sender: TObject);
var
  F: file of TInstrument;
begin
  OpenDialog1.Filter := 'hUGETracker instruments|*.ugi';
  if OpenDialog1.Execute then begin
    AssignFile(F, OpenDialog1.FileName);
    Reset(F);
    Read(F, CurrentInstrument^);
    CloseFile(F);
  end;
end;

procedure TfrmTracker.DirectionComboBoxChange(Sender: TObject);
begin
  case DirectionComboBox.Text of
    'Up': CurrentInstrument^.VolSweepDirection := Up;
    'Down': CurrentInstrument^.VolSweepDirection := Down;
  end;
  EnvelopePaintBox.Invalidate;
end;

procedure TfrmTracker.CommentMemoChange(Sender: TObject);
begin
  Song.Comment := CommentMemo.Text;
end;

procedure TfrmTracker.ArtistEditChange(Sender: TObject);
begin
  Song.Artist := ArtistEdit.Text;
  UpdateWindowTitle;
end;

procedure TfrmTracker.HelpLookupManualExecute(Sender: TObject);
  procedure OpenPage(Page: Integer);
  begin
    OpenURL('file:///'+ExpandFileName('./Manual.pdf')+'#page='+IntToStr(Page));
  end;
begin
  // TODO: This is really brittle. We need to find some way to export PDF
  // such that headers are exported as named destinations.
  {case LastActiveControl of
    TreeView1: OpenPage(13);

  end;}
end;

procedure TfrmTracker.FileSaveAs1Accept(Sender: TObject);
var
  Stream: TStream;
begin
  stream := TFileStream.Create(FileSaveAs1.Dialog.FileName, fmCreate);
  try
    WriteSongToStream(stream, Song);
    SaveSucceeded := True;
    LoadedFileName := FileSaveAs1.Dialog.FileName;
    UpdateWindowTitle;
  finally
    stream.Free;
  end;
end;

procedure TfrmTracker.FileOpen1Accept(Sender: TObject);
begin
  LoadSong(FileOpen1.Dialog.FileName);
  UpdateUIAfterLoad(FileOpen1.Dialog.FileName);
end;

procedure TfrmTracker.HeaderControl1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SelectedSection: THeaderSection;
  P: TPoint;
  I: Integer;
begin
  // Hack

  if Button = mbRight then begin
    P.X := X;
    P.Y := Y;

    for I := 1 to 4 do
      HeaderControl1.Sections[I].ImageIndex := 0;

    SelectedSection := HeaderControl1.Sections[HeaderControl1.GetSectionAt(P)];
    SelectedSection.ImageIndex := 1;

    for I := 1 to 4 do
      snd[I].ChannelOFF := HeaderControl1.Sections[I].ImageIndex = 0;
  end;
end;

procedure TfrmTracker.HeaderControl1SectionClick(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  if Section.OriginalIndex = 0 then Exit;

  Section.ImageIndex := (Section.ImageIndex + 1) mod 2;
  snd[Section.OriginalIndex].ChannelOFF := Section.ImageIndex = 0;
end;

procedure TfrmTracker.HeaderControl1SectionResize(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  // Prevent resizing these
  // TODO: Create a subclass of them that doesn't allow resizing maybe?
  if Section.OriginalIndex > 0 then Section.Width := TrackerGrid.ColumnWidth
  else Section.Width := RowNumberStringGrid.Width;
end;

procedure TfrmTracker.CutActionExecute(Sender: TObject);
begin
  PostMessage(Screen.ActiveControl.Handle, LM_CUT, 0, 0);
end;

procedure TfrmTracker.Duty1VisualizerPaint(Sender: TObject);
begin
  DrawVizualizer(Duty1Visualizer, 1);
end;

procedure TfrmTracker.Duty2VisualizerPaint(Sender: TObject);
begin
  DrawVizualizer(Duty2Visualizer, 2);
end;

procedure TfrmTracker.CopyActionExecute(Sender: TObject);
begin
  PostMessage(Screen.ActiveControl.Handle, LM_COPY, 0, 0);
end;

procedure TfrmTracker.InstrumentComboBoxChange(Sender: TObject);
begin
  TrackerGrid.SelectedInstrument := ModInst(InstrumentComboBox.ItemIndex);
end;

procedure TfrmTracker.FileSaveAs1BeforeExecute(Sender: TObject);
begin
  SaveSucceeded := False;
end;

procedure TfrmTracker.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CheckUnsavedChanges;
end;

procedure TfrmTracker.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  if Length(FileNames) >= 1 then begin
    if not (ExtractFileExt(FileNames[0]) = '.uge') or (not CheckUnsavedChanges) then
      Exit;

    LoadSong(FileNames[0]);
  end;
end;

procedure TfrmTracker.GotoGeneralActionExecute(Sender: TObject);
begin
  PageControl1.TabIndex := 0;
end;

procedure TfrmTracker.GotoPatternsActionExecute(Sender: TObject);
begin
  PageControl1.TabIndex := 1;
end;

procedure TfrmTracker.GotoInstrumentsActionExecute(Sender: TObject);
begin
  PageControl1.TabIndex := 2;
end;

procedure TfrmTracker.GotoWavesActionExecute(Sender: TObject);
begin
  PageControl1.TabIndex := 3;
end;

procedure TfrmTracker.GotoCommentsActionExecute(Sender: TObject);
begin
  PageControl1.TabIndex := 4;
end;

procedure TfrmTracker.GotoRoutinesActionExecute(Sender: TObject);
begin
  PageControl1.TabIndex := 5;
end;

procedure TfrmTracker.HexWaveEditEditingDone(Sender: TObject);
var
  S: String;
  I: Integer;
begin
  S := DelSpace(HexWaveEdit.Text);
  if S.Length < High(TWave)+1 then Exit;

  try
    for I := Low(TWave) to High(TWave) do
      CurrentWave^[I] := Hex2Dec(S.Substring(I,1));
    HexWaveEdit.Text := S;
    WaveEditPaintBox.Invalidate;
  except
    on EConvertError do Exit;
  end;
end;

procedure TfrmTracker.IncrementCurrentInstrumentActionExecute(Sender: TObject);
begin
  InstrumentComboBox.ItemIndex := EnsureRange(InstrumentComboBox.ItemIndex+1, 0, InstrumentComboBox.Items.Count-1);
  TrackerGrid.SelectedInstrument := ModInst(InstrumentComboBox.ItemIndex);
end;

procedure TfrmTracker.InsertRowActionExecute(Sender: TObject);
begin
  TrackerGrid.InsertRowInPatternAtCursor(TrackerGrid.Cursor.X);
end;

procedure TfrmTracker.InsertRowActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ActiveControl = TrackerGrid;
end;

procedure TfrmTracker.InsertRowForAllActionExecute(Sender: TObject);
begin
  TrackerGrid.InsertRowInAllAtCursor;
end;

procedure TfrmTracker.InsertRowForAllActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ActiveControl = TrackerGrid;
end;

procedure TfrmTracker.MenuItem37Click(Sender: TObject);
begin
  Clipboard.AsText := ConvertWaveToHexString(WaveEditNumberSpinner.Value);
end;

procedure TfrmTracker.MenuItem38Click(Sender: TObject);
begin
  HexWaveEdit.Text := Clipboard.AsText;
  HexWaveEditEditingDone(nil);
end;

procedure TfrmTracker.ExportAsmMenuItemClick(Sender: TObject);
var
  S: String;
begin
  S := InputBox('Song descriptor', 'Enter the song descriptor (must be a valid RGBDS symbol):', 'song_descriptor');
  if RGBDSAsmSaveDialog.Execute then
    RenderSongToRGBDSAsm(
      Song,
      S,
      RGBDSAsmSaveDialog.FileName
    );
end;

procedure TfrmTracker.NoiseMacroPaintboxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DrawingMacro := True;
end;

procedure TfrmTracker.NoiseMacroPaintboxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  MacroIdx, Val: Integer;
begin
  if DrawingMacro then begin
    MacroIdx := EnsureRange(Trunc((X/NoiseMacroPaintbox.Width) * 6), 0, 5);
    Val := 32-EnsureRange(Trunc((Y/NoiseMacroPaintbox.Height) * 63), 0, 63);

    CurrentInstrument^.NoiseMacro[MacroIdx] := Val;
    NoiseMacroPaintbox.Invalidate;
  end;
end;

procedure TfrmTracker.NoiseMacroPaintboxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DrawingMacro := False;
end;

procedure TfrmTracker.NoiseMacroPaintboxPaint(Sender: TObject);
var
  BarWidth, BarStep: Double;
  I: Integer;
  Rect: TRect;

  function FormatMacroNumber(Val: Integer): string;
  begin
    if Val = 0 then Exit('')
    else if Val > 0 then Exit('+'+IntToStr(Val))
    else Exit(IntToStr(Val));
  end;

begin
  BarWidth := (NoiseMacroPaintbox.Width / 6); // TODO: change this constant?
  BarStep := (NoiseMacroPaintbox.Height / 63);
  with NoiseMacroPaintbox.Canvas do begin
    Brush.Color := clGameboyBlack;
    Clear;

    Brush.Color := clGameboyMidGreen;
    Pen.Color := clGameboyBlack;
    for I := Low(TNoiseMacro) to High(TNoiseMacro) do begin
      Rect := TRect.Create(
        Trunc(I*BarWidth),
        NoiseMacroPaintbox.Height div 2,
        Trunc((I+1)*BarWidth),
        Ceil((NoiseMacroPaintbox.Height / 2) - (CurrentInstrument^.NoiseMacro[I] * BarStep))
      );
      FillRect(Rect);
      TextOut(Rect.Left + Trunc(Rect.Width/2.5), Rect.Top + (Rect.Height div 2), FormatMacroNumber(CurrentInstrument^.NoiseMacro[I]));
    end;
  end;
end;

procedure TfrmTracker.OrderEditStringGridCellProcess(Sender: TObject; aCol,
  aRow: Integer; processType: TCellProcessType; var aValue: string);
begin
  // Somewhat of a hack, to prevent accidentally pasting crap into the
  // order editor.
  if processType = cpPaste then
    aValue := OrderEditStringGrid.Cells[aCol, aRow];
end;

procedure TfrmTracker.OrderEditStringGridValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
var
  _: Integer;
begin
  if not TryStrToInt(NewValue, _) then NewValue := OldValue;
end;

procedure TfrmTracker.PlayCursorActionExecute(Sender: TObject);
begin
  if GetPreviewReady then begin
    Playing := True;

    PokeSymbol(SYM_CURRENT_ORDER, 2*(OrderEditStringGrid.Row-1));
    PokeSymbol(SYM_ROW, TrackerGrid.Cursor.Y);

    UnlockPlayback;
  end
end;

procedure TfrmTracker.PlayOrderActionExecute(Sender: TObject);
begin
  if GetPreviewReady then begin
    Playing := True;

    PokeSymbol(SYM_CURRENT_ORDER, 2*(OrderEditStringGrid.Row-1));
    PokeSymbol(SYM_ROW, 0);

    UnlockPlayback;
  end
end;

procedure TfrmTracker.ScrollBox1MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then begin
    Handled := True;
    if ssShift in Shift then
      TrackerGrid.IncrementSelection(-12, -10, -0, -0, -$10)
    else
      TrackerGrid.IncrementSelection(-1, -1, -0, -0, -1);
  end
  else
    Handled := False;
end;

procedure TfrmTracker.ScrollBox1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then begin
    Handled := True;
    if ssShift in Shift then
      TrackerGrid.IncrementSelection(12, 10, 0, 0, $10)
    else
      TrackerGrid.IncrementSelection(1, 1, 0, 0, 1);
  end
  else
    Handled := False;
end;

procedure TfrmTracker.FormShow(Sender: TObject);
begin
  // HACK: This needs to be done due to a scaling bug in the LCL.
  RecreateTrackerGrid;
  ReloadPatterns
end;

procedure TfrmTracker.Button1Click(Sender: TObject);
begin
  PreviewC5;
end;

procedure TfrmTracker.DecrementCurrentInstrumentActionExecute(Sender: TObject);
begin
  InstrumentComboBox.ItemIndex := EnsureRange(InstrumentComboBox.ItemIndex-1, 0, InstrumentComboBox.Items.Count-1);
  TrackerGrid.SelectedInstrument := ModInst(InstrumentComboBox.ItemIndex);
end;

procedure TfrmTracker.DeleteRowActionExecute(Sender: TObject);
begin
  TrackerGrid.DeleteRowInPatternAtCursor(TrackerGrid.Cursor.X);
end;

procedure TfrmTracker.DeleteRowActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ActiveControl = TrackerGrid;
end;

procedure TfrmTracker.DeleteRowForAllActionExecute(Sender: TObject);
begin
  TrackerGrid.DeleteRowInAllAtCursor;
end;

procedure TfrmTracker.DeleteRowForAllActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ActiveControl = TrackerGrid;
end;

procedure TfrmTracker.Duty1VisualizerClick(Sender: TObject);
var
  Section: THeaderSection;
begin
  if Sender = Duty1Visualizer then Section := HeaderControl1.Sections.Items[1]
  else if Sender = Duty2Visualizer then Section := HeaderControl1.Sections.Items[2]
  else if Sender = WaveVisualizer then Section := HeaderControl1.Sections.Items[3]
  else {if Sender = NoiseVisualizer then} Section := HeaderControl1.Sections.Items[4];

  Section.ImageIndex := (Section.ImageIndex + 1) mod 2;
  snd[Section.OriginalIndex].ChannelOFF := Section.ImageIndex = 0;
end;

procedure TfrmTracker.ExportCMenuItemClick(Sender: TObject);
var
  S: String;
begin
  S := InputBox('Song descriptor', 'Enter the song descriptor (must be a valid C symbol):', 'song_descriptor');
  if GBDKCSaveDialog.Execute then
    RenderSongToGBDKC(
      Song,
      S,
      GBDKCSaveDialog.FileName
    );
end;

procedure TfrmTracker.PasteActionExecute(Sender: TObject);
begin
  PostMessage(Screen.ActiveControl.Handle, LM_PASTE, 0, 0);
end;

procedure TfrmTracker.OctaveSpinEditChange(Sender: TObject);
begin
  TrackerGrid.SelectedOctave := OctaveSpinEdit.Value;
end;

procedure TfrmTracker.RoutineNumberSpinnerChange(Sender: TObject);
begin
  RoutineSynedit.Text := Song.Routines[RoutineNumberSpinner.Value];
end;

procedure TfrmTracker.RoutineSyneditChange(Sender: TObject);
begin
  Song.Routines[RoutineNumberSpinner.Value] := RoutineSynedit.Text;
end;

procedure TfrmTracker.PlayStartActionExecute(Sender: TObject);
begin
  if GetPreviewReady then begin
    Playing := True;
    UnlockPlayback;
  end;
end;

procedure TfrmTracker.StepSpinEditChange(Sender: TObject);
begin
  TrackerGrid.Step := StepSpinEdit.Value;
end;

procedure TfrmTracker.StopActionExecute(Sender: TObject);
begin
  TrackerGrid.HighlightedRow := -1;
  HaltPlayback;
end;

procedure TfrmTracker.DutyComboboxChange(Sender: TObject);
begin
  CurrentInstrument^.Duty := DutyComboBox.ItemIndex;
end;

procedure TfrmTracker.EnvChangeSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.VolSweepAmount := Round(EnvChangeTrackbar.Position);
  EnvelopePaintBox.Invalidate;
end;

procedure TfrmTracker.EnvelopePaintboxPaint(Sender: TObject);
begin
  with EnvelopePaintbox.Canvas do begin
    Brush.Color := clBlack;
    Clear;
  end;
  DrawEnvelope(EnvelopePaintBox);
end;

procedure TfrmTracker.InstrumentNameEditChange(Sender: TObject);
var
  S: String;
begin
  S := IntToStr(InstrumentNumberSpinner.Value) + ': ' + InstrumentNameEdit.Text;
  CurrentInstrument^.Name := InstrumentNameEdit.Text;
  case CurrentInstrumentBank of
    itSquare: begin
      InstrumentComboBox.Items[(0*15) + InstrumentNumberSpinner.Value] := 'Square '+S;
      DutyInstrumentsNode.Items[InstrumentNumberSpinner.Value-1].Text := S;
    end;
    itWave: begin
      InstrumentComboBox.Items[(1*15) + InstrumentNumberSpinner.Value] := 'Wave '+S;
      WaveInstrumentsNode.Items[InstrumentNumberSpinner.Value-1].Text := S;
    end;
    itNoise: begin
      InstrumentComboBox.Items[(2*15) + InstrumentNumberSpinner.Value] := 'Noise '+S;
      NoiseInstrumentsNode.Items[InstrumentNumberSpinner.Value-1].Text := S;
    end;
  end;
end;

procedure TfrmTracker.InstrumentNumberSpinnerChange(Sender: TObject);
begin
  LoadInstrument(CurrentInstrumentBank, InstrumentNumberSpinner.Value);
end;

procedure TfrmTracker.LengthSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.Length := Round(LengthTrackbar.Position);
  EnvelopePaintBox.Invalidate;
end;

procedure TfrmTracker.DebugPlayNoteButtonClick(Sender: TObject);
begin
  PreviewInstrument(C_5, InstrumentNumberSpinner.Value);
  //PreviewInstrument(NotesToFreqs.KeyData[C_5], InstrumentComboBox.ItemIndex);
end;

procedure TfrmTracker.MenuItem11Click(Sender: TObject);
begin
  TrackerGrid.DoUndo;
end;

procedure TfrmTracker.MenuItem12Click(Sender: TObject);
begin
  if CheckUnsavedChanges then
    FileOpen1.Execute;
end;

procedure TfrmTracker.MenuItem14Click(Sender: TObject);
begin
  if CheckUnsavedChanges then
    Application.Terminate;
end;

procedure TfrmTracker.MenuItem17Click(Sender: TObject);
var
  X, Highest: Integer;
begin
  Highest := Song.Patterns.MaxKey;

  with OrderEditStringGrid do
    InsertRowWithValues(
      Row,
      ['',
      IntToStr(Highest),
      IntToStr(Highest+1),
      IntToStr(Highest+2),
      IntToStr(Highest+3)]
    );

  for X := 0 to 3 do
    Song.Patterns.CreateNewPattern(Highest+X);

  ReloadPatterns;
end;

procedure TfrmTracker.MenuItem18Click(Sender: TObject);
begin
  with OrderEditStringGrid do
    InsertRowWithValues(Row, ['', '0', '0', '0', '0']);

  ReloadPatterns;
end;

procedure TfrmTracker.MenuItem19Click(Sender: TObject);
begin
  if OrderEditStringGrid.RowCount > 2 then
    OrderEditStringGrid.DeleteRow(OrderEditStringGrid.Row);

  ReloadPatterns;
end;

procedure TfrmTracker.MenuItem21Click(Sender: TObject);
begin
  with OrderEditStringGrid do begin
    InsertRowWithValues(Row, ['', '0', '0', '0', '0']);
    Rows[Row-1] := Rows[Row];
  end;

  ReloadPatterns;
end;

procedure TfrmTracker.MenuItem22Click(Sender: TObject);
var
  X, Highest: Integer;
begin
  Highest := Song.Patterns.MaxKey;

  with OrderEditStringGrid do begin
    InsertRowWithValues(
      Row,
      ['',
      IntToStr(Highest),
      IntToStr(Highest+1),
      IntToStr(Highest+2),
      IntToStr(Highest+3)]
    );

    for X := 0 to 3 do
      Song.Patterns.CreateNewPattern(Highest+X)^ :=
        Song.Patterns.KeyData[StrToInt(Rows[Row][X+1])]^;
  end;

  ReloadPatterns;
end;

procedure TfrmTracker.MenuItem26Click(Sender: TObject);
begin
  frmOptions.ShowModal;

  TrackerGrid.FontSize := TrackerSettings.PatternEditorFontSize;
  RowNumberStringGrid.DefaultRowHeight := TrackerGrid.RowHeight;

  ScopesOn := TrackerSettings.UseScopes;

  OscilloscopeUpdateTimer.Enabled := ScopesOn;
  Panel2.Visible := ScopesOn;

  CreateKeymap
end;

procedure TfrmTracker.MenuItem31Click(Sender: TObject);
begin
  TrackerGrid.InterpolateSelection;
end;

procedure TfrmTracker.MenuItem33Click(Sender: TObject);
begin
  if not CheckUnsavedChanges then Exit;

  FileSaveAs1.Dialog.FileName := '';
  FileOpen1.Dialog.FileName := '';

  DestroySong(Song);
  InitializeSong(Song);
  LoadDefaultInstruments(Song);

  UpdateUIAfterLoad;
end;

procedure TfrmTracker.MenuItem34Click(Sender: TObject);
var
  Stream: TStream;
begin
  if not CheckUnsavedChanges then Exit;

  if MODOpenDialog.Execute then begin
    DestroySong(Song);

    Stream := TFileStream.Create(MODOpenDialog.FileName, fmOpenRead);
    Song := LoadSongFromModStream(Stream);

    Stream.Free;

    UpdateUIAfterLoad(MODOpenDialog.FileName);
  end;
end;

procedure TfrmTracker.OnIncrementValueBy1Click(Sender: TObject);
begin
  TrackerGrid.IncrementSelection(1, 1, 0, 0, 1);
end;

procedure TfrmTracker.OnDecrementValueBy1Click(Sender: TObject);
begin
  TrackerGrid.IncrementSelection(-1, -1, 0, 0, -1);
end;

procedure TfrmTracker.OnIncrementValueBy10Click(Sender: TObject);
begin
  TrackerGrid.IncrementSelection(12, 10, 0, 0, $10);
end;

procedure TfrmTracker.OnDecrementValueBy10Click(Sender: TObject);
begin
  TrackerGrid.IncrementSelection(-12, -10, 0, 0, -$10);
end;

procedure TfrmTracker.MenuItem5Click(Sender: TObject);
begin
  frmAboutHugetracker.Show;
end;

procedure TfrmTracker.MenuItem8Click(Sender: TObject);
begin
  TrackerGrid.DoRedo;
end;

procedure TfrmTracker.NoiseVisualizerPaint(Sender: TObject);
begin
  DrawVizualizer(NoiseVisualizer, 4);
end;

procedure TfrmTracker.NoteHaltTimerTimer(Sender: TObject);
begin
  Panic;
  NoteHaltTimer.Enabled := False;
end;

procedure TfrmTracker.OrderEditStringGridAfterSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  if OrderEditStringGrid.Row > -1 then
    ReloadPatterns;

  if (not InFDCallback) and Playing then begin // Hacky solution, but probably the best there is.
    LockPlayback;
    PokeSymbol(SYM_NEXT_ORDER, OrderEditStringGrid.Row);
    PokeSymbol(SYM_ROW_BREAK, 1);
    UnlockPlayback;
  end
end;

procedure TfrmTracker.OrderEditStringGridColRowDeleted(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if OrderEditStringGrid.RowCount = 1 then
    with OrderEditStringGrid do
      InsertRowWithValues(Row, ['', '0', '0', '0', '0']);

  OrderEditStringGrid.Row := OrderEditStringGrid.Row-1;
  ReloadPatterns;
end;

procedure TfrmTracker.OrderEditStringGridColRowExchanged(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  ReloadPatterns;
end;

procedure TfrmTracker.OrderEditStringGridColRowInserted(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  ReloadPatterns;
end;

procedure TfrmTracker.OrderEditStringGridDblClick(Sender: TObject);
var
  Highest: Integer;
begin
  Highest := Song.Patterns.MaxKey;

  with OrderEditStringGrid do begin
    Cells[Col, Row] := IntToStr(Highest);
    TrackerGrid.LoadPattern(Col - 1, Highest);
  end;
end;

procedure TfrmTracker.OrderEditStringGridKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then begin
    // TODO: Make this stop the editor from showing
    OrderEditStringGrid.DeleteRow(OrderEditStringGrid.Row);
    ReloadPatterns;
  end;
end;

procedure TfrmTracker.PanicToolButtonClick(Sender: TObject);
begin
  Panic;
end;

procedure TfrmTracker.TicksPerRowSpinEditChange(Sender: TObject);
begin
  Song.TicksPerRow := TicksPerRowSpinEdit.Value;
end;

procedure TfrmTracker.OscilloscopeUpdateTimerTimer(Sender: TObject);
var
  Max1, Max2: Integer;
  I: Integer;
  Samp: Integer;
begin
  // HACK: Avoid LCL bug where modal windows can't close.
  if frmOptions.Visible
  or frmEffectEditor.Visible
  or frmRenderToWave.Visible then
    Exit;

  if Playing then
    OscilloscopeUpdateTimer.Interval := 25
  else
    OscilloscopeUpdateTimer.Interval := 200;

  Max1 := -1;
  Max2 := -1;
  for I := 0 to SAMPLE_BUFFER_SIZE do begin
    Samp := Abs(SampleBuffers[0].BufferL[I] div 2);
    if Samp > Max1 then Max1 := Samp;
    Samp := Abs(SampleBuffers[0].BufferR[I] div 2);
    if Samp > Max2 then Max2 := Samp;
  end;

  Duty1Visualizer.Invalidate;
  Duty2Visualizer.Invalidate;
  NoiseVisualizer.Invalidate;
  WaveVisualizer.Invalidate;

  LEDMeter1.Position := LEDMeter1.Position-2;
  LEDMeter2.Position := LEDMeter2.Position-2;

  Max1 := Trunc((Max1/512)*100);
  Max2 := Trunc((Max2/512)*100);
  if Max1 > LEDMeter1.Position then LEDMeter1.Position := Max1;
  if Max2 > LEDMeter2.Position then LEDMeter2.Position := Max2;
end;

procedure TfrmTracker.ExportGBSButtonClick(Sender: TObject);
begin
  if GBSSaveDialog.Execute then
    RenderSongToFile(GBSSaveDialog.FileName, emGBS);
end;

procedure TfrmTracker.ToolButton10Click(Sender: TObject);
begin
  if RenderPreviewROM then begin
    StopPlayback;
    ParseSymFile('render/preview.sym');
    frmRenderToWave.ShowModal;
    StartPlayback;
    HaltPlayback;
  end;
end;

procedure TfrmTracker.ExportGBButtonClick(Sender: TObject);
begin
  if GBSaveDialog.Execute then begin
    RenderSongToFile(GBSaveDialog.FileName);
  end;
end;

procedure TfrmTracker.TrackerPopupCopyClick(Sender: TObject);
begin
  SendMessage(TrackerGrid.Handle, LM_COPY, 0, 0)
end;

procedure TfrmTracker.TrackerPopupCutClick(Sender: TObject);
begin
  SendMessage(TrackerGrid.Handle, LM_CUT, 0, 0)
end;

procedure TfrmTracker.TrackerPopupEditEffectClick(Sender: TObject);
begin
  TrackerGrid.OpenEffectEditor;
end;

procedure TfrmTracker.TrackerPopupEraseClick(Sender: TObject);
begin
  TrackerGrid.EraseSelection
end;

procedure TfrmTracker.TrackerPopupFloodPasteClick(Sender: TObject);
begin
  TrackerGrid.DoRepeatPaste
end;

procedure TfrmTracker.TrackerPopupPasteClick(Sender: TObject);
begin
  SendMessage(TrackerGrid.Handle, LM_PASTE, 0, 0)
end;

procedure TfrmTracker.TrackerPopupRedoClick(Sender: TObject);
begin
  TrackerGrid.DoRedo
end;

procedure TfrmTracker.TrackerPopupSelectAllClick(Sender: TObject);
begin
  TrackerGrid.SelectAll
end;

procedure TfrmTracker.TrackerPopupSelectChannelClick(Sender: TObject);
begin
  TrackerGrid.SelectColumn
end;

procedure TfrmTracker.TrackerPopupTransposeOctaveDownClick(Sender: TObject);
begin
  TrackerGrid.TransposeSelection(-12)
  {if TrackerSettings.PreviewWhenBumping then
    ; // TODO: preview the note}
end;

procedure TfrmTracker.TrackerPopupTransposeOctaveUpClick(Sender: TObject);
begin
  TrackerGrid.TransposeSelection(12)
end;

procedure TfrmTracker.TrackerPopupTransposeSemiDownClick(Sender: TObject);
begin
  TrackerGrid.TransposeSelection(-1)
end;

procedure TfrmTracker.TrackerPopupTransposeSemiUpClick(Sender: TObject);
begin
  TrackerGrid.TransposeSelection(1)
end;

procedure TfrmTracker.TrackerPopupUndoClick(Sender: TObject);
begin
  TrackerGrid.DoUndo
end;

procedure TfrmTracker.TreeView1DblClick(Sender: TObject);
begin
  if not Assigned(TreeView1.Selected) then Exit;

  if TreeView1.Selected.Parent = DutyInstrumentsNode then begin
    LoadInstrument(itSquare, {%H-}PtrUInt(TreeView1.Selected.Data));
    PageControl1.ActivePage := InstrumentTabSheet;
  end;

  if TreeView1.Selected.Parent = WaveInstrumentsNode then begin
    LoadInstrument(itWave, {%H-}PtrUInt(TreeView1.Selected.Data));
    PageControl1.ActivePage := InstrumentTabSheet;
  end;

  if TreeView1.Selected.Parent = NoiseInstrumentsNode then begin
    LoadInstrument(itNoise, {%H-}PtrUInt(TreeView1.Selected.Data));
    PageControl1.ActivePage := InstrumentTabSheet;
  end;

  if TreeView1.Selected.Parent = WavesNode then begin
    WaveEditNumberSpinner.Value := {%H-}PtrUInt(TreeView1.Selected.Data);
    PageControl1.ActivePage := WavesTabSheet;
  end;

  if TreeView1.Selected.Parent = RoutinesNode then begin
    RoutineNumberSpinner.Value := {%H-}PtrUInt(TreeView1.Selected.Data);
    PageControl1.ActivePage := RoutinesTabSheet;
  end;
end;

procedure TfrmTracker.WaveEditNumberSpinnerChange(Sender: TObject);
begin
  LoadWave(WaveEditNumberSpinner.Value);
end;

procedure TfrmTracker.WaveEditPaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TestInstrument: TInstrument;
begin
  if Button = mbLeft then begin
    TestInstrument := Default(TInstrument);
    with TestInstrument do begin
      Type_ := itWave;
      OutputLevel := 1;
      Waveform := WaveEditNumberSpinner.Value;
      Length := 0;
      LengthEnabled := False;
    end;

    DrawingWave := True;
    if PlayWaveWhileDrawingCheckbox.Checked then
      PreviewInstrument(C_5, TestInstrument);
  end
end;

procedure TfrmTracker.WaveEditPaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Idx: Integer;
begin
  X := EnsureRange(X, 0, WaveEditPaintBox.Width);
  Y := EnsureRange(Y, 0, WaveEditPaintBox.Height);
  if DrawingWave then begin
    Idx := EnsureRange(Trunc((X / WaveEditPaintBox.Width)*High(TWave)), Low(TWave), High(TWave));
    CurrentWave^[Idx] := EnsureRange(Trunc((Y / WaveEditPaintBox.Height)*$F), 0, $F);
    UpdateHexWaveTextbox;
    WaveEditPaintBox.Invalidate;
    if PlayWaveWhileDrawingCheckbox.Checked then
      CopyWaveIntoWaveRam(WaveEditNumberSpinner.Value);
  end;
end;

procedure TfrmTracker.WaveEditPaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DrawingWave := False;
  Panic;
end;

procedure TfrmTracker.WaveEditPaintBoxPaint(Sender: TObject);
begin
  DrawWaveform(WaveEditPaintBox, CurrentWave^);
end;

procedure TfrmTracker.LengthEnabledCheckboxChange(Sender: TObject);
begin
  LengthTrackbar.Enabled := LengthEnabledCheckbox.Checked;
  CurrentInstrument^.LengthEnabled := LengthEnabledCheckbox.Checked;
  EnvelopePaintBox.Invalidate;
end;

end.


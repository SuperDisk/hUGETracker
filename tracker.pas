unit Tracker;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Menus, Spin, StdCtrls, ActnList, StdActns, SynEdit, SynHighlighterAny,
  FileUtil, math, Instruments, Waves, Song, EmulationThread, Utils, Constants,
  sound, vars, machine, about_hugetracker, TrackerGrid, lclintf, lmessages,
  Buttons, Grids, DBCtrls, HugeDatatypes, LCLType, RackCtls, Codegen, SymParser,
  options, IniFiles, bgrabitmap, effecteditor, RenderToWave, modimport;

type
  { TfrmTracker }

  TfrmTracker = class(TForm)
    Button1: TButton;
    Duty1Visualizer: TPaintBox;
    Duty2Visualizer: TPaintBox;
    InstrumentExportButton: TButton;
    InstrumentImportButton: TButton;
    InstrumentComboBox: TComboBox;
    CutAction: TAction;
    ImageList2: TImageList;
    GBSaveDialog: TSaveDialog;
    Label25: TLabel;
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
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem40: TMenuItem;
    NoiseVisualizer: TPaintBox;
    MODOpenDialog: TOpenDialog;
    Panel6: TPanel;
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
    ShiftClockTrackbar: TTrackBar;
    DivRatioTrackbar: TTrackBar;
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
    ActionList1: TActionList;
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
    RandomizeNoiseButton: TButton;
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
    Label14: TLabel;
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
    procedure Duty1VisualizerClick(Sender: TObject);
    procedure EditDelete1Execute(Sender: TObject);
    procedure FileSaveAs1BeforeExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure InstrumentComboBoxChange(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure CutActionExecute(Sender: TObject);
    procedure Duty1VisualizerPaint(Sender: TObject);
    procedure Duty2VisualizerPaint(Sender: TObject);
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
    procedure DivRatioSpinnerChange(Sender: TObject);
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
    procedure DebugShiteButtonClick(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure FontSizeToggleMenuItemClick(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem31Click(Sender: TObject);
    procedure MenuItem33Click(Sender: TObject);
    procedure MenuItem34Click(Sender: TObject);
    procedure MenuItem37Click(Sender: TObject);
    procedure MenuItem38Click(Sender: TObject);
    procedure MenuItem39Click(Sender: TObject);
    procedure MenuItem40Click(Sender: TObject);
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
    procedure OrderEditStringGridColRowMoved(Sender: TObject;
      IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure OrderEditStringGridDblClick(Sender: TObject);
    procedure OrderEditStringGridEditingDone(Sender: TObject);
    procedure OrderEditStringGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PanicToolButtonClick(Sender: TObject);
    procedure PasteActionExecute(Sender: TObject);
    procedure OctaveSpinEditChange(Sender: TObject);
    procedure RoutineNumberSpinnerChange(Sender: TObject);
    procedure RoutineSyneditChange(Sender: TObject);
    procedure ShiftClockSpinnerChange(Sender: TObject);
    procedure StepSpinEditChange(Sender: TObject);
    procedure TicksPerRowSpinEditChange(Sender: TObject);
    procedure OscilloscopeUpdateTimerTimer(Sender: TObject);
    procedure ExportGBSButtonClick(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
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
    procedure RandomizeNoiseButtonClick(Sender: TObject);
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

    EmulationThread: TThread;

    PreviewingInstrument: Integer;
    DrawingWave: Boolean;
    Playing: Boolean;
    LoadingFile: Boolean;
    SaveSucceeded: Boolean;

    {PatternsNode, }InstrumentsNode, WavesNode, RoutinesNode: TTreeNode;

    OptionsFile: TIniFile;

    VisualizerBuffer: TBGRABitmap;

    procedure ChangeToSquare;
    procedure ChangeToWave;
    procedure ChangeToNoise;
    procedure LoadInstrument(Instr: Integer);
    procedure LoadWave(Wave: Integer);
    procedure LoadSong(Filename: String);
    procedure ReloadPatterns;
    procedure CopyOrderGridToOrderMatrix;
    procedure CopyOrderMatrixToOrderGrid;
    function PreparePreview: Boolean;
    procedure ResetEmulationThread;

    procedure OnFD(var Msg: TLMessage); message LM_FD;
    procedure OnUndoOccured(var Msg: TLMessage); message LM_UNDO_OCCURED;
    procedure OnNotePlaced(var Msg: TLMessage); message LM_PREVIEW_NOTE;
    procedure OnSampleSongMenuItemClicked(Sender: TObject);

    procedure RecreateTrackerGrid;
    procedure UpdateUIAfterLoad;
    function CheckUnsavedChanges: Boolean;

    procedure DrawWaveform(PB: TPaintBox; Wave: TWave);
    procedure DrawVizualizer(PB: TPaintBox; Channel: Integer);

    procedure PreviewInstrument(Freq: Integer; Instr: Integer);
    procedure PreviewC5;
    procedure Panic;
  public
    procedure OnTrackerGridResize(Sender: TObject);

  end;

var
  frmTracker: TfrmTracker;

implementation

{$R *.lfm}

{ TfrmTracker }

procedure TfrmTracker.UpdateUIAfterLoad;
var
  I: Integer;
begin
  ResetEmulationThread;

  LoadingFile := True; // HACK!!!!!
  LoadInstrument(1);
  LoadWave(0);

  RoutineSynedit.Text := Song.Routines[0];
  RoutineNumberSpinner.Value := 0;

  SongEdit.Text := Song.Name;
  ArtistEdit.Text := Song.Artist;
  CommentMemo.Text := Song.Comment;

  TicksPerRowSpinEdit.Value := Song.TicksPerRow;

  RecreateTrackerGrid;
  CopyOrderMatrixToOrderGrid;

  for I := Low(Song.Instruments) to High(song.Instruments) do
    InstrumentComboBox.Items[I] := IntToStr(I)+': '+Song.Instruments[I].Name;

  for I := Low(Song.Instruments) to High(song.Instruments) do
    InstrumentsNode.Items[I-1].Text := IntToStr(I)+': '+Song.Instruments[I].Name;

  LoadingFile := False; // HACK!!!!

  OrderEditStringGrid.Row := 1;
  ReloadPatterns;

  PageControl1.ActivePageIndex := 0;
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

  Interval := W div 32;
  //HInterval := H div $10;
  With PB.Canvas do begin
    Brush.Color := clBlack;
    Clear;

    {Pen.Width := 1;
    Pen.Color := StringToColor('$103800');
    for I := 0 to 32 do
      Line(I*Interval, 0, I*Interval, H);

    for I := 0 to $10 do
      Line(0, I*HInterval, W, I*HInterval);}

    Brush.Color := clTeal;
    Pen.Color := clTeal;
    Pen.Width := 2;
    MoveTo(0, H);
    for I := 0 to 32 do
      LineTo(I*Interval, Trunc((Wave[I]/$F)*H));
    LineTo(W, Trunc((Wave[0]/$F)*H));
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

    Brush.Color := clBlack;
    Clear;

    Pen.Color := clGray;
    Rectangle(0, 0, PB.Width, PB.Height);

    Pen.Width := 2;

    if not snd[Channel].ChannelOFF then begin
      Pen.Color := clLime;

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
      Pen.Color := TColor($0C0094);
      Line(0, 0, PB.Width, PB.Height);
      Line(0, PB.Height, PB.Width, 0);
    end;
  end;
  VisualizerBuffer.Draw(PB.Canvas, 0, 0, True);
  //PB.Canvas.Draw(0, 0, VisualizerBuffer);
end;

procedure TfrmTracker.PreviewInstrument(Freq: Integer; Instr: Integer);
var
  Regs: TRegisters;
  I: Integer;
  NewWaveform: T4bitWave;
begin
  // TODO: Find some way to synchronize with the playback thread, so it doesn't
  // fail sometimes.

  with Song.Instruments[Instr] do
  begin
    case Type_ of
      itSquare: begin
        Regs := SquareInstrumentToRegisters(Freq, True, Song.Instruments[Instr]);
        Spokeb(NR10, Regs.NR10);
        Spokeb(NR11, Regs.NR11);
        Spokeb(NR12, Regs.NR12);
        Spokeb(NR13, Regs.NR13);
        Spokeb(NR14, Regs.NR14);
      end;
      itWave: begin
        NewWaveform := ConvertWaveform(Song.Waves[Waveform]);
        // Fill itWave RAM with the waveform
        for I := Low(NewWaveform) to High(NewWaveform) do
          Spokeb(AUD3_WAVE_RAM+I, NewWaveform[I]);

        Regs := WaveInstrumentToRegisters(Freq, True, Song.Instruments[Instr]);

        Spokeb(NR30, Regs.NR30);
        Spokeb(NR31, Regs.NR31);
        Spokeb(NR32, Regs.NR32);
        Spokeb(NR33, Regs.NR33);
        Spokeb(NR34, Regs.NR34)
      end;
      itNoise: begin
        Regs := NoiseInstrumentToRegisters(Freq, True, Song.Instruments[Instr]);
        Spokeb(NR41, Regs.NR41);
        Spokeb(NR42, Regs.NR42);
        Spokeb(NR43, Regs.NR43);
        Spokeb(NR44, Regs.NR44);
      end;
    end;
  end;
end;

procedure TfrmTracker.PreviewC5;
begin
  if not LoadingFile then begin
    PreviewInstrument(NotesToFreqs.Data[C_5], InstrumentNumberSpinner.Value);
    NoteHaltTimer.Enabled := False;
    NoteHaltTimer.Enabled := True
  end;
end;

procedure TfrmTracker.Panic;
begin
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
end;

procedure TfrmTracker.OnTrackerGridResize(Sender: TObject);
var
  Section: TCollectionItem;
begin
  // Fix the size of the channel headers
  for Section in HeaderControl1.Sections do
    (Section as THeaderSection).Width := TrackerGrid.ColumnWidth;
end;

procedure TfrmTracker.LoadWave(Wave: Integer);
begin
  CurrentWave := @Song.Waves[Wave];
  WaveEditPaintBox.Invalidate;
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

  UpdateUIAfterLoad;
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

function TfrmTracker.PreparePreview: Boolean;
var
  I: Integer;
begin
  if RenderPreviewROM(Song) then begin
    // Load the new symbol table
    SymbolTable := ParseSymFile('hUGEDriver/preview.sym');

    // Start emulation on the rendered preview binary
    EmulationThread.Free;
    EmulationThread := TEmulationThread.Create('hUGEDriver/preview.gb');
    PokeSymbol(SYM_TICKS_PER_ROW, Song.TicksPerRow);
    for I := 0 to 3 do
      snd[I+1].ChannelOFF := HeaderControl1.Sections[I].ImageIndex = 0;

    Result := True;
  end
  else Result := False;
end;

procedure TfrmTracker.ResetEmulationThread;
begin
  Playing := False;

  SymbolTable := nil;

  TrackerGrid.HighlightedRow := -1;
  ToolButton2.ImageIndex := 74;

  EmulationThread.Free;
  EmulationThread := TEmulationThread.Create('halt.gb');
  EmulationThread.Start;
end;

procedure TfrmTracker.OnFD(var Msg: TLMessage);
begin
  TrackerGrid.HighlightedRow := PeekSymbol(SYM_ROW);
  OrderEditStringGrid.Row := (PeekSymbol(SYM_CURRENT_ORDER) div 2) + 1;
end;

procedure TfrmTracker.OnUndoOccured(var Msg: TLMessage);
begin
  with OrderEditStringGrid do
    {if (Msg.lParamhi <> StrToInt(Rows[Row][0])) or
       (Msg.lParamhi <> StrToInt(Rows[Row][1])) or
       (Msg.lParamhi <> StrToInt(Rows[Row][2])) or
       (Msg.lParamhi <> StrToInt(Rows[Row][3])) then}
         Row := -1;
end;

procedure TfrmTracker.OnNotePlaced(var Msg: TLMessage);
begin
  PreviewInstrument(NotesToFreqs.KeyData[msg.wParam], msg.lParam);
end;

procedure TfrmTracker.OnSampleSongMenuItemClicked(Sender: TObject);
begin
  LoadSong('Sample Songs/'+TMenuItem(Sender).Caption);
end;

procedure TfrmTracker.RecreateTrackerGrid;
var
  Section: TCollectionItem;
begin
  if Assigned(TrackerGrid) then TrackerGrid.Free;
  TrackerGrid := TTrackerGrid.Create(Self, ScrollBox1, Song.Patterns);
  TrackerGrid.OnResize:=@OnTrackerGridResize;

  TrackerGrid.FontSize := OptionsFile.ReadInteger('hUGETracker', 'fontsize', 12);

  // Fix the size of the channel headers
  for Section in HeaderControl1.Sections do
    (Section as THeaderSection).Width := TrackerGrid.ColumnWidth;

  TrackerGrid.PopupMenu := TrackerGridPopup;
end;

procedure TfrmTracker.LoadInstrument(Instr: Integer);
var
  CI: ^TInstrument;
begin
  CurrentInstrument := @Song.Instruments[Instr];
  CI := CurrentInstrument;

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
      ShiftClockTrackbar.Position := CI^.ShiftClockFreq;
      DivRatioTrackbar.Position := CI^.DividingRatio;
      SevenBitCounterCheckbox.Checked := CI^.CounterStep = Seven;
    end;
  end;

  WavePaintbox.Invalidate;
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
      Brush.Color := clBlack;
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
    CurrentInstrument^.CounterStep := Seven
  else
    CurrentInstrument^.CounterStep := Fifteen;
end;

procedure TfrmTracker.SongEditChange(Sender: TObject);
begin
  Song.Name := SongEdit.Text;
end;

procedure TfrmTracker.StartVolSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.InitialVolume := Round(StartVolTrackbar.Position);
end;

procedure TfrmTracker.SweepDirectionComboboxChange(Sender: TObject);
begin
  case SweepDirectionComboBox.Text of
    'Up': CurrentInstrument^.SweepIncDec := Up;
    'Down': CurrentInstrument^.SweepIncDec := Down;
  end;
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
  case InstrumentTypeComboBox.Text of
    'Square': begin
      CurrentInstrument^.Type_ := itSquare;
      ChangeToSquare
    end;
    'Wave': begin
      CurrentInstrument^.Type_ := itWave;
      ChangeToWave
    end;
    'Noise': begin
      CurrentInstrument^.Type_ := itNoise;
      ChangeToNoise
    end;
  end;
end;

procedure TfrmTracker.RandomizeNoiseButtonClick(Sender: TObject);
begin
  ShiftClockTrackbar.Position := 0;
  DivRatioTrackbar.Position := Random(Round(DivRatioTrackbar.Max));
  SevenBitCounterCheckbox.Checked := Random <= 0.5;
  LengthEnabledCheckbox.Checked := Random <= 0.5;
  LengthTrackbar.Position := Random(Round(LengthTrackbar.Max));

  PreviewInstrument(NotesToFreqs.KeyData[RandomRange(LOWEST_NOTE, HIGHEST_NOTE)],
    InstrumentNumberSpinner.Value);
end;

procedure TfrmTracker.FormCreate(Sender: TObject);
var
  PUI: PtrUint;
  SampleSongs: TStringList;
  S: String;
  MenuItem: TMenuItem;
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
    Halt;
  end;

  VisualizerBuffer := TBGRABitmap.Create(Duty1Visualizer.Width, Duty1Visualizer.Height);

  ReturnNilIfGrowHeapFails := False;
  PreviewingInstrument := -1;
  OptionsFile := TINIFile.Create('options.ini');

  InitializeSong(Song);

  // Create pattern editor control
  RecreateTrackerGrid;

  // Initialize ticks per row
  Song.TicksPerRow := TicksPerRowSpinEdit.Value;

  LoadInstrument(1);
  LoadWave(0);

  // Fetch the tree items
  with TreeView1 do begin
    //PatternsNode := Items[0];
    InstrumentsNode := Items[0];
    WavesNode := Items[1];
    RoutinesNode := Items[2];
  end;

  for PUI := 1 to 15 do
    TreeView1.Items.AddChild(InstrumentsNode, IntToStr(PUI)+':').Data := {%H-}Pointer(PUI);

  for PUI := 0 to 15 do begin
    TreeView1.Items.AddChild(WavesNode, 'Wave '+IntToStr(PUI)).Data := {%H-}Pointer(PUI);
    TreeView1.Items.AddChild(RoutinesNode, 'Routine '+IntToStr(PUI)).Data := {%H-}Pointer(PUI);
  end;

  // Initialize order table
  {for I := 0 to 3 do begin
    OrderEditStringGrid.Cells[I+1, 1] := IntToStr(I);
    TrackerGrid.LoadPattern(I, I);
  end;}
  //CopyOrderGridToOrderMatrix;
  CopyOrderMatrixToOrderGrid;

  // Manually resize the fixed column in the order editor
  OrderEditStringGrid.ColWidths[0]:=50;

  // Get the emulator ready to make sound...
  EmulationThread := TEmulationThread.Create('halt.gb');
  EmulationThread.Start;
  ResetEmulationThread; //TODO: remove this hack

  // Start the Oscilloscope repaint timer
  OscilloscopeUpdateTimer.Enabled := True;

  // Switch to general tab sheet
  PageControl1.ActivePageIndex := 0;

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
  if FileExists(ParamStr(1)) and (ExtractFileExt(ParamStr(1)) = '.uge') then
    LoadSong(ParamStr(1));
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

  Inc(Note, OctaveSpinEdit.Value*12);
  EnsureRange(Note, LOWEST_NOTE, HIGHEST_NOTE);

  if PreviewingInstrument <> Note then
    PreviewingInstrument := -1;

  if (PreviewingInstrument > -1) or
     (not (ActiveControl = TrackerGrid)) or
     (InstrumentComboBox.ItemIndex <= 0)
  then exit;

  if not NotesToFreqs.TryGetData(Note, Freq) then Exit;

  PreviewInstrument(Freq, InstrumentComboBox.ItemIndex);
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
end;

procedure TfrmTracker.CommentMemoChange(Sender: TObject);
begin
  Song.Comment := CommentMemo.Text;
end;

procedure TfrmTracker.ArtistEditChange(Sender: TObject);
begin
  Song.Artist := ArtistEdit.Text;
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
  finally
    stream.Free;
  end;
end;

procedure TfrmTracker.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TfrmTracker.FileOpen1Accept(Sender: TObject);
begin
  LoadSong(FileOpen1.Dialog.FileName);
end;

procedure TfrmTracker.HeaderControl1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SelectedSection: THeaderSection;
  Section: TCollectionItem;
  P: TPoint;
  I: Integer;
begin
  // Hack

  if Button = mbRight then begin
    P.X := X;
    P.Y := Y;

    for Section in HeaderControl1.Sections do
      (Section as THeaderSection).ImageIndex := 0;

    SelectedSection := HeaderControl1.Sections[HeaderControl1.GetSectionAt(P)];
    SelectedSection.ImageIndex := 1;

    for I := 0 to 3 do
      snd[I+1].ChannelOFF := HeaderControl1.Sections[I].ImageIndex = 0;
  end;
end;

procedure TfrmTracker.HeaderControl1SectionClick(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  Section.ImageIndex := (Section.ImageIndex + 1) mod 2;
  snd[Section.OriginalIndex+1].ChannelOFF := Section.ImageIndex = 0;
end;

procedure TfrmTracker.HeaderControl1SectionResize(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  // Prevent resizing these
  // TODO: Create a subclass of them that doesn't allow resizing maybe?
  Section.Width := TrackerGrid.ColumnWidth;
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
  TrackerGrid.SelectedInstrument := InstrumentComboBox.ItemIndex;
end;

procedure TfrmTracker.EditDelete1Execute(Sender: TObject);
begin

end;

procedure TfrmTracker.FileSaveAs1BeforeExecute(Sender: TObject);
begin
  SaveSucceeded := False;
end;

procedure TfrmTracker.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CheckUnsavedChanges;
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

procedure TfrmTracker.Duty1VisualizerClick(Sender: TObject);
var
  Section: THeaderSection;
begin
  if Sender = Duty1Visualizer then Section := HeaderControl1.Sections.Items[0]
  else if Sender = Duty2Visualizer then Section := HeaderControl1.Sections.Items[1]
  else if Sender = WaveVisualizer then Section := HeaderControl1.Sections.Items[2]
  else {if Sender = NoiseVisualizer then} Section := HeaderControl1.Sections.Items[3];

  Section.ImageIndex := (Section.ImageIndex + 1) mod 2;
  snd[Section.OriginalIndex+1].ChannelOFF := Section.ImageIndex = 0;
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

procedure TfrmTracker.ShiftClockSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.ShiftClockFreq := Round(ShiftClockTrackbar.Position);
end;

procedure TfrmTracker.StepSpinEditChange(Sender: TObject);
begin
  TrackerGrid.Step := StepSpinEdit.Value;
end;

procedure TfrmTracker.DivRatioSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.DividingRatio := Round(DivRatioTrackbar.Position);
end;

procedure TfrmTracker.DutyComboboxChange(Sender: TObject);
begin
  CurrentInstrument^.Duty := DutyComboBox.ItemIndex;
end;

procedure TfrmTracker.EnvChangeSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.VolSweepAmount := Round(EnvChangeTrackbar.Position);
end;

procedure TfrmTracker.EnvelopePaintboxPaint(Sender: TObject);
begin
  with EnvelopePaintbox.Canvas do begin
    Brush.Color := clBlack;
    Clear;
  end;
end;

procedure TfrmTracker.InstrumentNameEditChange(Sender: TObject);
begin
  CurrentInstrument^.Name := InstrumentNameEdit.Text;
  InstrumentComboBox.Items[InstrumentNumberSpinner.Value] :=
    IntToStr(InstrumentNumberSpinner.Value) + ': ' + InstrumentNameEdit.Text;
  InstrumentsNode.Items[InstrumentNumberSpinner.Value-1].Text :=
    InstrumentComboBox.Items[InstrumentNumberSpinner.Value];
end;

procedure TfrmTracker.InstrumentNumberSpinnerChange(Sender: TObject);
begin
  LoadInstrument(InstrumentNumberSpinner.Value);
end;

procedure TfrmTracker.LengthSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.Length := Round(LengthTrackbar.Position);
end;

procedure TfrmTracker.DebugPlayNoteButtonClick(Sender: TObject);
begin
  PreviewInstrument(C_5, InstrumentNumberSpinner.Value);
  //PreviewInstrument(NotesToFreqs.KeyData[C_5], InstrumentComboBox.ItemIndex);
end;

procedure TfrmTracker.DebugShiteButtonClick(Sender: TObject);
begin
  // If a command line param was passed, try to open it
  if FileExists(ParamStr(0)) then
    LoadSong(ParamStr(0));
end;

procedure TfrmTracker.MenuItem10Click(Sender: TObject);
begin

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

procedure TfrmTracker.MenuItem13Click(Sender: TObject);
begin

end;

procedure TfrmTracker.MenuItem14Click(Sender: TObject);
begin
  if CheckUnsavedChanges then
    Halt;
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

procedure TfrmTracker.FontSizeToggleMenuItemClick(Sender: TObject);
begin

end;

procedure TfrmTracker.MenuItem24Click(Sender: TObject);
var
  Report: TSongUsageReport;
  SL: TStringList;
  Iter: Report.UnusedPatterns.TIterator;
begin
  Report := GetUsageReport(Song);
  SL := TStringList.Create;
  SL.Delimiter := ',';

  Iter := Report.UnusedPatterns.Iterator;
  if Iter <> nil then
    repeat
      SL.Add(IntToStr(Iter.GetData))
    until not Iter.Next;

  Iter.Free;

  if SL.Text = '' then
    MessageDlg('Usage report',
      Format('%d unused instruments'+LineEnding+
             '%d unused routines'+LineEnding+
             '%d unused patterns'+LineEnding,
             [INSTRUMENTS_COUNT - Report.UsedInstruments.Size,
              ROUTINES_COUNT - Report.UsedRoutines.size,
              Song.Patterns.Count - Report.UsedPatterns.Size]),
             mtInformation, [mbOK], 0)
  else
    if MessageDlg('Usage report',
          Format('%d unused instruments'+LineEnding+
                 '%d unused routines'+LineEnding+
                 '%d unused patterns'+LineEnding+LineEnding+
                 'Patterns ' + SL.DelimitedText+' are unused.'+LineEnding+
                 'Do you want to remove the unused patterns?',
                 [INSTRUMENTS_COUNT - Report.UsedInstruments.Size,
                  ROUTINES_COUNT - Report.UsedRoutines.size,
                  Song.Patterns.Count - Report.UsedPatterns.Size]),
                 mtInformation, [mbYes, mbNo], 0) = mrYes
    then begin
      Iter := Report.UnusedPatterns.Iterator;
      if Iter <> nil then
        repeat
          Song.Patterns.DeletePattern(Iter.GetData);
        until not Iter.Next;
    end;

  SL.Free;
  FreeUsageReport(Report);
end;

procedure TfrmTracker.MenuItem26Click(Sender: TObject);
begin
  frmOptions.FontSizeSpinner.Value := TrackerGrid.FontSize;
  frmOptions.ShowModal;
  TrackerGrid.FontSize := frmOptions.FontSizeSpinner.Value;

  OptionsFile.WriteInteger('hUGETracker', 'fontsize', TrackerGrid.FontSize);
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

    UpdateUIAfterLoad;
  end;
end;

procedure TfrmTracker.MenuItem37Click(Sender: TObject);
begin
  TrackerGrid.IncrementSelection(1, 1, 0, 0, 1);
end;

procedure TfrmTracker.MenuItem38Click(Sender: TObject);
begin
  TrackerGrid.IncrementSelection(-1, -1, 0, 0, -1);
end;

procedure TfrmTracker.MenuItem39Click(Sender: TObject);
begin
  TrackerGrid.IncrementSelection(12, 10, 0, 0, $10);
end;

procedure TfrmTracker.MenuItem40Click(Sender: TObject);
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

procedure TfrmTracker.OrderEditStringGridColRowMoved(Sender: TObject;
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

procedure TfrmTracker.OrderEditStringGridEditingDone(Sender: TObject);
var
  Temp: Integer;
begin
  // TODO: Fix this hack!
  // For some reason OnValidateEntry is giving bad pointers
  // for its NewValue and OldValue params. This is the workaround for now.
  with OrderEditStringGrid do begin
    if not TryStrToInt(Cells[Col, Row], Temp) then
      Cells[Col, Row] := ''
    else;
  end;
  ReloadPatterns;
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
    RenderSongToFile(Song, GBSSaveDialog.FileName, emGBS);
end;

procedure TfrmTracker.ToolButton10Click(Sender: TObject);
begin
  EmulationThread.Terminate;
  if RenderPreviewROM(Song) then begin
    SymbolTable := ParseSymFile('hUGEDriver/preview.sym');
    frmRenderToWave.ShowModal;
    ResetEmulationThread;
  end;
end;

procedure TfrmTracker.ToolButton2Click(Sender: TObject);
begin
  if PreparePreview then begin
    Playing := True;

    PokeSymbol(SYM_CURRENT_ORDER, 2*(OrderEditStringGrid.Row-1));
    PokeSymbol(SYM_ROW, TrackerGrid.Cursor.Y);

    EmulationThread.Start;
  end
end;

procedure TfrmTracker.ToolButton3Click(Sender: TObject);
begin
  if PreparePreview then begin
    Playing := True;
    EmulationThread.Start;
  end;
end;

procedure TfrmTracker.ToolButton4Click(Sender: TObject);
begin
  TrackerGrid.HighlightedRow := -1;
  ResetEmulationThread;
end;

procedure TfrmTracker.ExportGBButtonClick(Sender: TObject);
begin
  if GBSaveDialog.Execute then begin
    RenderSongToFile(Song, GBSaveDialog.FileName);
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
  if TreeView1.Selected.Parent = InstrumentsNode then begin
    InstrumentNumberSpinner.Value := {%H-}PtrUInt(TreeView1.Selected.Data);
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
begin
  DrawingWave := True;
end;

procedure TfrmTracker.WaveEditPaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Idx: Integer;
begin
  if DrawingWave then begin
    Idx := Trunc((X / WaveEditPaintBox.Width)*32);
    CurrentWave^[Idx] := EnsureRange(Trunc((Y / WaveEditPaintBox.Height)*$F), 0, $F);
    WaveEditPaintBox.Invalidate;
  end;
end;

procedure TfrmTracker.WaveEditPaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DrawingWave := False;
end;

procedure TfrmTracker.WaveEditPaintBoxPaint(Sender: TObject);
begin
  DrawWaveform(WaveEditPaintBox, CurrentWave^);
end;

procedure TfrmTracker.LengthEnabledCheckboxChange(Sender: TObject);
begin
  LengthTrackbar.Enabled := LengthEnabledCheckbox.Checked;
  CurrentInstrument^.LengthEnabled := LengthEnabledCheckbox.Checked;
end;

end.


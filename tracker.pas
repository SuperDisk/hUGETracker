unit Tracker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Menus, Spin, StdCtrls, ActnList, StdActns, SynEdit, math, Instruments, Waves,
  Song, EmulationThread, Utils, Constants, sound, vars, machine,
  about_hugetracker, TrackerGrid, lclintf, lmessages, Buttons, Grids, DBCtrls,
  ECProgressBar, HugeDatatypes, LCLType, Codegen, SymParser;

type
  { TfrmTracker }

  TfrmTracker = class(TForm)
    CutAction: TAction;
    ImageList2: TImageList;
    OrderEditStringGrid: TStringGrid;
    EditCut1: TEditCut;
    HeaderControl1: THeaderControl;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
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
    ExportButton: TButton;
    FileExit1: TFileExit;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    FlowPanel1: TFlowPanel;
    ImageList1: TImageList;
    ImportWaveButton: TButton;
    CommentMemo: TMemo;
    Label20: TLabel;
    Label21: TLabel;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    N3: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    OrderEditPopup: TPopupMenu;
    SaveDialog1: TSaveDialog;
    ScrollBox1: TScrollBox;
    TicksPerRowSpinEdit: TSpinEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    PanicToolButton: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    WaveEditNumberSpinner: TSpinEdit;
    WaveEditGroupBox: TGroupBox;
    Label19: TLabel;
    DebugButton: TMenuItem;
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
    WaveVolumeCombobox: TComboBox;
    SweepDirectionCombobox: TComboBox;
    DutyCombobox: TComboBox;
    SweepSizeSpinner: TECSpinPosition;
    LengthSpinner: TECSpinPosition;
    SquareGroupBox: TGroupBox;
    WaveGroupBox: TGroupBox;
    NoiseGroupBox: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
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
    MenuItem4: TMenuItem;
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
    StartVolSpinner: TECSpinPosition;
    EnvChangeSpinner: TECSpinPosition;
    NoiseFreqSpinner: TECSpinPosition;
    DivRatioSpinner: TECSpinPosition;
    TreeView1: TTreeView;
    TrackerGrid: TTrackerGrid;
    procedure CopyActionExecute(Sender: TObject);
    procedure CutActionExecute(Sender: TObject);
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
    procedure DivRatioSpinnerChange(Sender: TObject);
    procedure DutyComboboxChange(Sender: TObject);
    procedure EnvChangeSpinnerChange(Sender: TObject);
    procedure EnvelopePaintboxPaint(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ImportWaveButtonClick(Sender: TObject);
    procedure InstrumentNameEditChange(Sender: TObject);
    procedure InstrumentNumberSpinnerChange(Sender: TObject);
    procedure LengthSpinnerChange(Sender: TObject);
    procedure DebugButtonClick(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure NoiseFreqSpinnerChange(Sender: TObject);
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
    procedure TicksPerRowSpinEditChange(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
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
    procedure WaveVolumeComboboxChange(Sender: TObject);
  private
    Song: TSong;
    CurrentInstrument: ^TInstrument;
    CurrentWave: ^TWave;

    EmulationThread: TThread;

    PreviewingInstrument: Boolean;
    DrawingWave: Boolean;
    Playing: Boolean;
    LoadingFile: Boolean;

    PatternsNode, InstrumentsNode, WavesNode, RoutinesNode: TTreeNode;

    SymbolTable: TSymbolMap;

    procedure ChangeToSquare;
    procedure ChangeToWave;
    procedure ChangeToNoise;
    procedure LoadInstrument(Instr: Integer);
    procedure LoadWave(Wave: Integer);
    procedure ReloadPatterns;
    procedure CopyOrderGridToOrderMatrix;
    procedure CopyOrderMatrixToOrderGrid;
    function PeekSymbol(Symbol: String): Integer;
    procedure ResetEmulationThread;
    procedure OnFD;

    procedure RecreateTrackerGrid;
    procedure UpdateUIAfterLoad;

    procedure DrawWaveform(PB: TPaintBox; Wave: TWave);

    procedure PreviewInstrument(Freq: Integer; Instr: Integer);
  public

  end;

var
  frmTracker: TfrmTracker;

implementation

{$R *.lfm}

{ TfrmTracker }

procedure TfrmTracker.UpdateUIAfterLoad;
begin
  LoadingFile := True; // HACK!!!!!
  LoadInstrument(1);
  LoadWave(0);

  SongEdit.Text := Song.Name;
  ArtistEdit.Text := Song.Artist;
  CommentMemo.Text := Song.Comment;

  TicksPerRowSpinEdit.Value := Song.TicksPerRow;

  RecreateTrackerGrid;
  CopyOrderMatrixToOrderGrid;

  LoadingFile := False; // HACK!!!!

  OrderEditStringGrid.Row := 1;
  ReloadPatterns;
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

procedure TfrmTracker.PreviewInstrument(Freq: Integer; Instr: Integer);
var
  Regs: TRegisters;
  I: Integer;
  NewWaveform: T4bitWave;
begin
  with Song.Instruments[Instr] do
  begin
    case Type_ of
      Square: begin
        Regs := SquareInstrumentToRegisters(Freq, True, Song.Instruments[Instr]);
        Spokeb(NR10, Regs.NR10);
        Spokeb(NR11, Regs.NR11);
        Spokeb(NR12, Regs.NR12);
        Spokeb(NR13, Regs.NR13);
        Spokeb(NR14, Regs.NR14);
      end;
      Wave: begin
        Regs := WaveInstrumentToRegisters(Freq, True, Song.Instruments[Instr]);
        NewWaveform := ConvertWaveform(Song.Waves[Waveform]);
        // Fill Wave RAM with the waveform
        for I := 0 to 15 do
          Spokeb($FF30+I, NewWaveform[I]);

        Regs := WaveInstrumentToRegisters(Freq, True, Song.Instruments[Instr]);
        {writeln(inttobin(Regs.NR30, 8, 20));
        writeln(inttobin(Regs.NR31, 8, 20));
        writeln(inttobin(Regs.NR32, 8, 20));
        writeln(inttobin(Regs.NR33, 8, 20));
        writeln(inttobin(Regs.NR34, 8, 20));}

        Spokeb(NR30, Regs.NR30);
        Spokeb(NR31, Regs.NR31); // TODO: Figure out why this isn't working?
        Spokeb(NR32, Regs.NR32);
        Spokeb(NR33, Regs.NR33);
        Spokeb(NR34, Regs.NR34)
      end;
      Noise: begin
        Regs := NoiseInstrumentToRegisters(True, Song.Instruments[Instr]);
        Spokeb(NR41, Regs.NR41);
        Spokeb(NR42, Regs.NR42);
        Spokeb(NR43, Regs.NR43);
        Spokeb(NR44, Regs.NR44);
      end;
    end;
  end;
end;

procedure TfrmTracker.LoadWave(Wave: Integer);
begin
  CurrentWave := @Song.Waves[Wave];
  WaveEditPaintBox.Invalidate;
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

function TfrmTracker.PeekSymbol(Symbol: String): Integer;
begin
  if SymbolTable = nil then exit;
  Result := speekb(SymbolTable.KeyData[Symbol]);
end;

procedure TfrmTracker.ResetEmulationThread;
begin
  Playing := False;

  SymbolTable := nil;

  TrackerGrid.HighlightedRow := -1;
  ToolButton2.ImageIndex := 74;

  EmulationThread.Terminate;
  EmulationThread.WaitFor;
  EmulationThread.Free;
  EmulationThread := TEmulationThread.Create('halt.gb');
  EmulationThread.Start;
end;

procedure TfrmTracker.OnFD;
begin
  TrackerGrid.HighlightedRow:=PeekSymbol(SYM_ROW);
end;

procedure TfrmTracker.RecreateTrackerGrid;
begin
  if TrackerGrid <> nil then TrackerGrid.Free;
  TrackerGrid := TTrackerGrid.Create(Self, ScrollBox1, Song.Patterns);
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
  LengthSpinner.Position := CI^.Length;

  case CI^.Type_ of
    Square: begin
      InstrumentTypeComboBox.Text := 'Square';
      ChangeToSquare;
    end;
    Wave: begin
      InstrumentTypeComboBox.Text := 'Wave';
      ChangeToWave;
    end;
    Noise: begin
      InstrumentTypeComboBox.Text := 'Noise';
      ChangeToNoise;
    end;
  end;

  StartVolSpinner.Position := CI^.InitialVolume;
  case CI^.VolSweepDirection of
    Up: DirectionComboBox.Text := 'Up';
    Down: DirectionComboBox.Text := 'Down';
  end;
  EnvChangeSpinner.Position := CI^.VolSweepAmount;

  case CI^.Type_ of
    Square: begin
      SweepTimeCombobox.ItemIndex := CI^.SweepTime;
      case CI^.SweepIncDec of
        Up: SweepDirectionCombobox.Text := 'Up';
        Down: SweepDirectionCombobox.Text := 'Down';
      end;
      SweepSizeSpinner.Position := CI^.SweepShift;
      DutyCombobox.ItemIndex := CI^.Duty;
    end;

    Wave: begin
      WaveVolumeCombobox.ItemIndex := CI^.OutputLevel;
      WaveformCombobox.ItemIndex := CI^.Waveform;
    end;

    Noise: begin
      NoiseFreqSpinner.Position := CI^.ShiftClockFreq;
      DivRatioSpinner.Position := CI^.DividingRatio;
      SevenBitCounterCheckbox.Checked := CI^.CounterStep = Seven;
    end;
  end;
end;

procedure TfrmTracker.ChangeToSquare;
begin
  EnvelopeGroupBox.Enabled := True;
  SquareGroupBox.Enabled := True;
  WaveGroupBox.Enabled := False;
  NoiseGroupBox.Enabled := False;

  LengthSpinner.Max := 63;
end;

procedure TfrmTracker.ChangeToWave;
begin
  EnvelopeGroupBox.Enabled := False;
  SquareGroupBox.Enabled := False;
  WaveGroupBox.Enabled := True;
  NoiseGroupBox.Enabled := False;

  LengthSpinner.Max := 255;
end;

procedure TfrmTracker.ChangeToNoise;
begin
  EnvelopeGroupBox.Enabled := True;
  SquareGroupBox.Enabled := False;
  WaveGroupBox.Enabled := False;
  NoiseGroupBox.Enabled := True;

  LengthSpinner.Max := 63;
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
  CurrentInstrument^.InitialVolume := Round(StartVolSpinner.Position);
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
  CurrentInstrument^.SweepShift := Round(SweepSizeSpinner.Position);
end;

procedure TfrmTracker.SweepTimeComboboxChange(Sender: TObject);
begin
  CurrentInstrument^.SweepTime := SweepTimeCombobox.ItemIndex;
end;

procedure TfrmTracker.WaveformComboboxChange(Sender: TObject);
begin
  WavePaintbox.Invalidate;
end;

procedure TfrmTracker.InstrumentTypeComboboxChange(Sender: TObject);
begin
  case InstrumentTypeComboBox.Text of
    'Square': begin
      CurrentInstrument^.Type_ := Square;
      ChangeToSquare
    end;
    'Wave': begin
      CurrentInstrument^.Type_ := Wave;
      ChangeToWave
    end;
    'Noise': begin
      CurrentInstrument^.Type_ := Noise;
      ChangeToNoise
    end;
  end;
end;

procedure TfrmTracker.RandomizeNoiseButtonClick(Sender: TObject);
begin
  NoiseFreqSpinner.Position := Random(Round(NoiseFreqSpinner.Max));
  DivRatioSpinner.Position := Random(Round(DivRatioSpinner.Max));
  SevenBitCounterCheckbox.Checked := Random <= 0.5;
  LengthEnabledCheckbox.Checked := Random <= 0.5;
  LengthSpinner.Position := Random(Round(LengthSpinner.Max));

  PreviewInstrument(C_5, InstrumentNumberSpinner.Value);
end;

procedure TfrmTracker.FormCreate(Sender: TObject);
var
  I, J: Integer;
  Section: TCollectionItem;
begin
  ReturnNilIfGrowHeapFails := False;

  // Create pattern editor control
  Song.Patterns := TPatternMap.Create;
  RecreateTrackerGrid;

  // Fix the size of the channel headers
  for Section in HeaderControl1.Sections do
    (Section as THeaderSection).Width := TrackerGrid.ColumnWidth;

  FDCallback := @Self.OnFD;

  for I := Low(Song.Instruments) to High(Song.Instruments) do
    with Song.Instruments[I] do begin
      Type_ := Square;
      Length := 0;
      LengthEnabled := False;
      InitialVolume := 63;
      VolSweepDirection := Down;
      VolSweepAmount := 0;

      SweepTime := 0;
      SweepIncDec := Down;
      SweepShift := 0;

      Duty := 2;
    end;

  for I := Low(Song.Waves) to High(Song.Waves) do begin
    for J := 0 to 32 do
      Song.Waves[I][J] := random($F);
    WaveformCombobox.Items.Add('Wave #' + IntToStr(I));
  end;

  LoadInstrument(1);
  LoadWave(0);

  // Fetch the tree items
  with TreeView1 do begin
    PatternsNode := Items[0];
    InstrumentsNode := Items[1];
    WavesNode := Items[2];
    RoutinesNode := Items[3];
  end;

  // Initialize order table
  for I := 0 to 3 do begin
    OrderEditStringGrid.Cells[I+1, 1] := IntToStr(I);
    TrackerGrid.LoadPattern(I, I);
  end;
  CopyOrderGridToOrderMatrix;

  // Manually resize the fixed column in the order editor
  OrderEditStringGrid.ColWidths[0]:=50;

  // Get the emulator ready to make sound...
  EmulationThread := TEmulationThread.Create('halt.gb');
  EmulationThread.Start;
end;

procedure TfrmTracker.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Freq: Integer;
begin
    if PreviewingInstrument then exit;

    Keybindings.TryGetData(Key, Freq);
    if Freq <> 0 then
      PreviewInstrument(Freq, InstrumentNumberSpinner.Value);

    PreviewingInstrument := True;
end;

procedure TfrmTracker.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  PreviewingInstrument := False;

  // Silence channel 1
  Spokeb(NR12, 0);
  Spokeb(NR14, %10000000);
end;

procedure TfrmTracker.ImportWaveButtonClick(Sender: TObject);
var
  F: file of TWave;
begin
  OpenDialog1.DefaultExt := '*.pcm';
  if OpenDialog1.Execute then begin
    assignfile(F, OpenDialog1.FileName);
    reset(F);
    Read(F, Song.Waves[WaveEditNumberSpinner.Value]);
    CloseFile(F);
    WaveEditPaintBox.Invalidate;
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
begin

end;

procedure TfrmTracker.FileSaveAs1Accept(Sender: TObject);
var
  Stream: TStream;
begin
  stream := TFileStream.Create(FileSaveAs1.Dialog.FileName, fmCreate);
  try
    WriteSongToStream(stream, Song);
  finally
    stream.Free;
  end;
end;

procedure TfrmTracker.FileOpen1Accept(Sender: TObject);
var
  Stream: TStream;
begin
  stream := TFileStream.Create(FileOpen1.Dialog.FileName, fmOpenRead);
  try
    ReadSongFromStream(stream, Song);
  finally
    stream.Free;
  end;

  UpdateUIAfterLoad;
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

procedure TfrmTracker.CopyActionExecute(Sender: TObject);
begin
  PostMessage(Screen.ActiveControl.Handle, LM_COPY, 0, 0);
end;

procedure TfrmTracker.PasteActionExecute(Sender: TObject);
begin
  PostMessage(Screen.ActiveControl.Handle, LM_PASTE, 0, 0);
end;

procedure TfrmTracker.DivRatioSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.DividingRatio := Round(DivRatioSpinner.Position);
end;

procedure TfrmTracker.DutyComboboxChange(Sender: TObject);
begin
  CurrentInstrument^.Duty := DutyComboBox.ItemIndex;
end;

procedure TfrmTracker.EnvChangeSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.VolSweepAmount := Round(EnvChangeSpinner.Position);
end;

procedure TfrmTracker.EnvelopePaintboxPaint(Sender: TObject);
begin
  with EnvelopePaintbox.Canvas do begin
    Brush.Color := clBlack;
    Clear;
  end;
end;

procedure TfrmTracker.ExportButtonClick(Sender: TObject);
var F: file of TWave;
begin
  if SaveDialog1.Execute then begin
    AssignFile(F, OpenDialog1.FileName);
    Rewrite(F);
    Write(F, CurrentWave^);
    CloseFile(F);
  end;
end;

procedure TfrmTracker.InstrumentNameEditChange(Sender: TObject);
begin
  CurrentInstrument^.Name := InstrumentNameEdit.Text;
end;

procedure TfrmTracker.InstrumentNumberSpinnerChange(Sender: TObject);
begin
  LoadInstrument(InstrumentNumberSpinner.Value);
end;

procedure TfrmTracker.LengthSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.Length := Round(LengthSpinner.Position);
end;

procedure TfrmTracker.DebugButtonClick(Sender: TObject);
begin
  PreviewInstrument(C_5, InstrumentNumberSpinner.Value);
end;

procedure TfrmTracker.MenuItem16Click(Sender: TObject);
begin
  PostMessage(Screen.ActiveControl.Handle, LM_PASTE, 0, 0);
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
end;

procedure TfrmTracker.MenuItem18Click(Sender: TObject);
begin
  with OrderEditStringGrid do
    InsertRowWithValues(Row, ['', '0', '0', '0', '0']);
end;

procedure TfrmTracker.MenuItem19Click(Sender: TObject);
begin
  OrderEditStringGrid.DeleteRow(OrderEditStringGrid.Row);
end;

procedure TfrmTracker.MenuItem5Click(Sender: TObject);
var
  AboutForm: TfrmAboutHugeTracker;
begin
  AboutForm := TfrmAboutHugetracker.Create(Self);
  AboutForm.ShowModal;
  AboutForm.Free;
end;

procedure TfrmTracker.NoiseFreqSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.ShiftClockFreq := Round(NoiseFreqSpinner.Position);
end;

procedure TfrmTracker.OrderEditStringGridAfterSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  ReloadPatterns;
end;

procedure TfrmTracker.OrderEditStringGridColRowDeleted(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
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
  end;
end;

procedure TfrmTracker.PanicToolButtonClick(Sender: TObject);
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

procedure TfrmTracker.TicksPerRowSpinEditChange(Sender: TObject);
begin
  Song.TicksPerRow := TicksPerRowSpinEdit.Value;
end;

procedure TfrmTracker.ToolButton2Click(Sender: TObject);
begin
  if not Playing then begin
    if RenderPreviewROM(Song) then begin
      Playing := True;
      ToolButton2.ImageIndex := 75;

      // Load the new symbol table
      SymbolTable := ParseSymFile('hUGEDriver/preview.sym');

      // Start emulation on the rendered preview binary
      EmulationThread.Terminate;
      EmulationThread.WaitFor;
      EmulationThread.Free;
      EmulationThread := TEmulationThread.Create('hUGEDriver/preview.gb');
      EmulationThread.Start;
    end
    else ResetEmulationThread;
  end
  else ResetEmulationThread;
end;

procedure TfrmTracker.ToolButton4Click(Sender: TObject);
begin
  TrackerGrid.HighlightedRow := -1;
  ResetEmulationThread;
end;

procedure TfrmTracker.ToolButton5Click(Sender: TObject);
begin
  if SaveDialog1.Execute then begin
    RenderSongToFile(Song, SaveDialog1.FileName);
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
    CurrentWave^[Idx] :=
      Min($F,
        Max(0,
          Trunc((Y / WaveEditPaintBox.Height)*$F)));
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
  LengthSpinner.Enabled := LengthEnabledCheckbox.Checked;
  CurrentInstrument^.LengthEnabled := LengthEnabledCheckbox.Checked;
end;

end.


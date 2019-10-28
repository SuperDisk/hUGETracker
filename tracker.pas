unit Tracker;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Menus, Spin, StdCtrls, Grids, ValEdit, SynEdit, VirtualTrees, ECSlider,
  ECProgressBar, math, Instruments, Waves, Song, EmulationThread, Utils,
  Constants, StrUtils, mainloop, sound, vars, machine;

type
  { TfrmTracker }

  TfrmTracker = class(TForm)
    ExportButton: TButton;
    ImportWaveButton: TButton;
    CommentMemo: TMemo;
    Label20: TLabel;
    Label21: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpinEdit1: TSpinEdit;
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
    CoolBar1: TCoolBar;
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
    Panel3: TPanel;
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
    procedure NoiseFreqSpinnerChange(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
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

    procedure ChangeToSquare;
    procedure ChangeToWave;
    procedure ChangeToNoise;
    procedure LoadInstrument(Instr: Integer);
    procedure LoadWave(Wave: Integer);

    procedure DrawWaveform(PB: TPaintBox; Wave: TWave);

    procedure PreviewInstrument(Freq: Integer; Instr: Integer);
  public

  end;

var
  frmTracker: TfrmTracker;

implementation

{$R *.lfm}

{ TfrmTracker }

procedure TfrmTracker.DrawWaveform(PB: TPaintBox; Wave: TWave);
var
  Interval, HInterval: Integer;
  I: Integer;
  W, H : Integer;
begin
  W := PB.Width;
  H := PB.Height;

  Interval := W div 32;
  HInterval := H div $10;
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
      LineTo(I*Interval, Round((Wave[I]/$F)*H));
    LineTo(W, Round((Wave[0]/$F)*H));
  end;
end;

procedure TfrmTracker.PreviewInstrument(Freq: Integer; Instr: Integer);
var
  Regs: TRegisters;
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
begin
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

  // Get the emulator ready to make sound...
  EmulationThread := TEmulationThread.Create;
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
      PreviewInstrument(Keybindings[Key], InstrumentNumberSpinner.Value);

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
  F: file of byte;
begin
  OpenDialog1.DefaultExt := '*.pcm';
  if OpenDialog1.Execute then begin
    assignfile(F, OpenDialog1.FileName);
    reset(F);
    BlockRead(F, Song.Waves[WaveEditNumberSpinner.Value], 32);
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
var F: file of Byte;
begin
  if SaveDialog1.Execute then begin
    AssignFile(F, OpenDialog1.FileName);
    Rewrite(F);
    BlockWrite(F, CurrentWave^, 32);
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

procedure TfrmTracker.NoiseFreqSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.ShiftClockFreq := Round(NoiseFreqSpinner.Position);
end;

procedure TfrmTracker.SpinEdit1Change(Sender: TObject);
begin
  Song.TicksPerRow := SpinEdit1.Value;
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
    Idx := Round((X / WaveEditPaintBox.Width)*32);
    CurrentWave^[Idx] :=
      Min($F,
        Max(0,
          Round((Y / WaveEditPaintBox.Height)*$F)));
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


unit Tracker;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Menus, Spin, StdCtrls, SynEdit, VirtualTrees, ECSlider, ECProgressBar,
  instruments, song;

type
  { TfrmTracker }

  TfrmTracker = class(TForm)
    CommentMemo: TMemo;
    Label19: TLabel;
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
    procedure CommentMemoChange(Sender: TObject);
    procedure DirectionComboBoxChange(Sender: TObject);
    procedure DivRatioSpinnerChange(Sender: TObject);
    procedure DutyComboboxChange(Sender: TObject);
    procedure EnvChangeSpinnerChange(Sender: TObject);
    procedure EnvelopePaintboxPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InstrumentNameEditChange(Sender: TObject);
    procedure InstrumentNumberSpinnerChange(Sender: TObject);
    procedure LengthSpinnerChange(Sender: TObject);
    procedure NoiseFreqSpinnerChange(Sender: TObject);
    procedure RandomizeNoiseButtonClick(Sender: TObject);
    procedure InstrumentTypeComboboxChange(Sender: TObject);
    procedure LengthEnabledCheckboxChange(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure SevenBitCounterCheckboxChange(Sender: TObject);
    procedure StartVolSpinnerChange(Sender: TObject);
    procedure SweepDirectionComboboxChange(Sender: TObject);
    procedure SweepSizeSpinnerChange(Sender: TObject);
    procedure SweepTimeComboboxChange(Sender: TObject);
    procedure WavePaintboxPaint(Sender: TObject);
    procedure WaveVolumeComboboxChange(Sender: TObject);
  private
    Song: TSong;
    CurrentInstrument: ^TInstrument;

    procedure ChangeToSquare;
    procedure ChangeToWave;
    procedure ChangeToNoise;
    procedure LoadInstrument(Instr: Integer);
  public

  end;

var
  frmTracker: TfrmTracker;

implementation

{$R *.lfm}

{ TfrmTracker }

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
  with WavePaintbox.Canvas do begin
    Brush.Color := clBlack;
    Clear;
  end;
end;

procedure TfrmTracker.WaveVolumeComboboxChange(Sender: TObject);
begin
  CurrentInstrument^.OutputLevel := WaveVolumeCombobox.ItemIndex;
end;

procedure TfrmTracker.PaintBox1Paint(Sender: TObject);
begin
  with PaintBox1.Canvas do begin
    Brush.Color := clBlack;
    Clear;
  end;
end;

procedure TfrmTracker.SevenBitCounterCheckboxChange(Sender: TObject);
begin
  if SevenBitCounterCheckbox.Checked then
    CurrentInstrument^.CounterStep := Seven
  else
    CurrentInstrument^.CounterStep := Fifteen;
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
end;

procedure TfrmTracker.FormCreate(Sender: TObject);
var
  I: Integer;
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

  LoadInstrument(1);
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

procedure TfrmTracker.NoiseFreqSpinnerChange(Sender: TObject);
begin
  CurrentInstrument^.ShiftClockFreq := Round(NoiseFreqSpinner.Position);
end;

procedure TfrmTracker.LengthEnabledCheckboxChange(Sender: TObject);
begin
  LengthSpinner.Enabled := LengthEnabledCheckbox.Checked;
  CurrentInstrument^.LengthEnabled := LengthEnabledCheckbox.Checked;
end;

end.


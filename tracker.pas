unit Tracker;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Menus, Spin, StdCtrls, ExtendedNotebook;

type
  TInstrumentType = (Square, Wave, Noise);
  TDutyType = 0..3;
  TSweepType = (Up, Down);
  TStepWidth = (Fifteen, Seven);

  TInstrument = record
    //Type_: TInstrumentType;
    Length: Integer;
    // Highmask
    LengthEnabled: Boolean;

    InitialVolume: Integer;
    VolSweepDirection: TSweepType;
    VolSweepAmount: Integer;

    case Type_: TInstrumentType of
      Square: (
        // NR10
        SweepTime: Integer;
        SweepIncDec: TSweepType;
        SweepShift: Integer;

        // NR11
        Duty: TDutyType;
      );
      Wave: (
        // NR32
        OutputLevel: Integer;
        // Wave
        Waveform: Integer;
      );
      Noise: (
        // NR42
        ShiftClockFreq: Integer;
        CounterStep: TStepWidth;
        DividingRatio: Integer;
      );
  end;

  { TfrmTracker }

  TfrmTracker = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    DirectionComboBox1: TComboBox;
    DirectionComboBox2: TComboBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
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
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
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
    PaintBox2: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    InstrumentNumberSpinner: TSpinEdit;
    LengthSpinner: TSpinEdit;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    PatternTabSheet: TTabSheet;
    InstrumentTabSheet: TTabSheet;
    GeneralTabSheet: TTabSheet;
    CommentsTabSheet: TTabSheet;
    RoutinesTabSheet: TTabSheet;
    StatusBar1: TStatusBar;
    EnvelopeStartTrackbar: TTrackBar;
    EnvelopeSizeTrackbar: TTrackBar;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
    TreeView1: TTreeView;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
  private


  public

  end;

var
  frmTracker: TfrmTracker;

implementation

{$R *.lfm}

{ TfrmTracker }

procedure TfrmTracker.PaintBox2Paint(Sender: TObject);
begin
  with PaintBox2.Canvas do begin
    Brush.Color := clBlack;
    Clear;
  end;
end;

procedure TfrmTracker.PaintBox1Paint(Sender: TObject);
begin
  with PaintBox1.Canvas do begin
    Brush.Color := clBlack;
    Clear;
  end;
end;

end.


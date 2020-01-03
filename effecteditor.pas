unit effecteditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, hugeDatatypes;

type

  { TfrmEffectEditor }

  TfrmEffectEditor = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    LeftPanCheckGroup: TCheckGroup;
    RightPanCheckGroup: TCheckGroup;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    VibratoRateRadioGroup: TRadioGroup;
    DutyRadioGroup: TRadioGroup;
    TwoParamsTrackBar1: TTrackBar;
    TwoParamsTrackBar2: TTrackBar;
    VibratoDepthTrackBar: TTrackBar;
    LeftVolumeTrackBar: TTrackBar;
    RightVolumeTrackBar: TTrackBar;
    Value: TLabel;
    Notebook1: TNotebook;
    OneParamPage: TPage;
    MasterVolumePage: TPage;
    DutyCyclePage: TPage;
    PanningPage: TPage;
    OneParamTrackBar: TTrackBar;
    Value1: TLabel;
    Value2: TLabel;
    VibratoPage: TPage;
    TwoParamsPage: TPage;
    procedure ComboBox1Change(Sender: TObject);
    procedure OneParamTrackBarChange(Sender: TObject);
  private
    Cell: PCell;

    procedure LoadEffect;

    procedure fx0;
    procedure fx1;
    procedure fx2;
    procedure fx3;
    procedure fx4;
    procedure fx5;
    procedure fx6;
    procedure fx7;
    procedure fx8;
    procedure fx9;
    procedure fxA;
    procedure fxB;
    procedure fxC;
    procedure fxD;
    procedure fxE;
    procedure fxF;
  public
    constructor Create(Cell_: PCell); reintroduce;
  end;

var
  frmEffectEditor: TfrmEffectEditor;

implementation

{$R *.lfm}

{ TfrmEffectEditor }

procedure TfrmEffectEditor.ComboBox1Change(Sender: TObject);
begin
  Cell^.EffectCode := ComboBox1.ItemIndex;
  LoadEffect;
end;

procedure TfrmEffectEditor.OneParamTrackBarChange(Sender: TObject);
begin
  Cell^.EffectParams.Value := OneParamTrackBar.Position;
end;

procedure TfrmEffectEditor.LoadEffect;
begin
  case Cell^.EffectCode of
    $0: fx0;
    $1: fx1;
    $2: fx2;
    $3: fx3;
    $4: fx4;
    $5: fx5;
    $6: fx6;
    $7: fx7;
    $8: fx8;
    $9: fx9;
    $A: fxA;
    $B: fxB;
    $C: fxC;
    $D: fxD;
    $E: fxE;
    $F: fxF;
  end;
end;

procedure TfrmEffectEditor.fx0;
begin

end;

procedure TfrmEffectEditor.fx1;
begin

end;

procedure TfrmEffectEditor.fx2;
begin

end;

procedure TfrmEffectEditor.fx3;
begin

end;

procedure TfrmEffectEditor.fx4;
begin

end;

procedure TfrmEffectEditor.fx5;
begin

end;

procedure TfrmEffectEditor.fx6;
begin

end;

procedure TfrmEffectEditor.fx7;
begin

end;

procedure TfrmEffectEditor.fx8;
begin

end;

procedure TfrmEffectEditor.fx9;
begin

end;

procedure TfrmEffectEditor.fxA;
begin

end;

procedure TfrmEffectEditor.fxB;
begin

end;

procedure TfrmEffectEditor.fxC;
begin

end;

procedure TfrmEffectEditor.fxD;
begin

end;

procedure TfrmEffectEditor.fxE;
begin

end;

procedure TfrmEffectEditor.fxF;
begin

end;

constructor TfrmEffectEditor.Create(Cell_: PCell);
begin
  Self.Cell := Cell_;
  ComboBox1.ItemIndex := Cell^.EffectCode;
  LoadEffect;
end;

end.


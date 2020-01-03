unit effecteditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, hugeDatatypes;

type

  { TfrmEffectEditor }

  TfrmEffectEditor = class(TForm)
    Button1: TButton;
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
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure DutyRadioGroupSelectionChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure LeftPanCheckGroupItemClick(Sender: TObject; Index: integer);
    procedure OneParamTrackBarChange(Sender: TObject);
    procedure TwoParamsTrackBar1Change(Sender: TObject);
    procedure TwoParamsTrackBar2Change(Sender: TObject);
    procedure VibratoDepthTrackBarChange(Sender: TObject);
    procedure VibratoRateRadioGroupSelectionChanged(Sender: TObject);
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

implementation

{$R *.lfm}

{ TfrmEffectEditor }

procedure TfrmEffectEditor.ComboBox1Change(Sender: TObject);
begin
  Cell^.EffectCode := ComboBox1.ItemIndex;
  LoadEffect;
end;

procedure TfrmEffectEditor.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmEffectEditor.DutyRadioGroupSelectionChanged(Sender: TObject);
begin
  case DutyRadioGroup.ItemIndex of
    0: Cell^.EffectParams.Value := $00;
    1: Cell^.EffectParams.Value := $40;
    2: Cell^.EffectParams.Value := $80;
    3: Cell^.EffectParams.Value := $C0;
  end;
end;

procedure TfrmEffectEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  writeln('ok');
end;

procedure TfrmEffectEditor.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  ModalResult:=mrClose;
  writeln('hi');
end;

procedure TfrmEffectEditor.FormCreate(Sender: TObject);
begin
  ComboBox1.ItemIndex := Cell^.EffectCode;
  LoadEffect;
end;

procedure TfrmEffectEditor.LeftPanCheckGroupItemClick(Sender: TObject;
  Index: integer);
var
  Val: Integer;
begin
  Val := %00000000;
  if CheckBox4.Checked then Val := Val or %00001000;
  if CheckBox3.Checked then Val := Val or %00000100;
  if CheckBox2.Checked then Val := Val or %00000010;
  if CheckBox1.Checked then Val := Val or %00000001;

  if CheckBox8.Checked then Val := Val or %10000000;
  if CheckBox7.Checked then Val := Val or %01000000;
  if CheckBox6.Checked then Val := Val or %00100000;
  if CheckBox5.Checked then Val := Val or %00010000;

  Cell^.EffectParams.Value := Val;
end;

procedure TfrmEffectEditor.OneParamTrackBarChange(Sender: TObject);
begin
  Cell^.EffectParams.Value := OneParamTrackBar.Position;
end;

procedure TfrmEffectEditor.TwoParamsTrackBar1Change(Sender: TObject);
begin
  Cell^.EffectParams.Param1 := TwoParamsTrackBar1.Position;
  writeln('test');
end;

procedure TfrmEffectEditor.TwoParamsTrackBar2Change(Sender: TObject);
begin
  Cell^.EffectParams.Param1 := TwoParamsTrackBar2.Position;
end;

procedure TfrmEffectEditor.VibratoDepthTrackBarChange(Sender: TObject);
begin
  Cell^.EffectParams.Param2 := VibratoDepthTrackBar.Position;
end;

procedure TfrmEffectEditor.VibratoRateRadioGroupSelectionChanged(Sender: TObject
  );
begin
  case VibratoRateRadioGroup.ItemIndex of
    0: Cell^.EffectParams.Param1 := $0;
    1: Cell^.EffectParams.Param1 := $1;
    2: Cell^.EffectParams.Param1 := $3;
    3: Cell^.EffectParams.Param1 := $7;
    4: Cell^.EffectParams.Param1 := $F;
  end;
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
  Notebook1.PageIndex := 1;
end;

procedure TfrmEffectEditor.fx1;
begin
  Notebook1.PageIndex := 0;
end;

procedure TfrmEffectEditor.fx2;
begin
  Notebook1.PageIndex := 0;
end;

procedure TfrmEffectEditor.fx3;
begin
  Notebook1.PageIndex := 0;
end;

procedure TfrmEffectEditor.fx4;
begin
  Notebook1.PageIndex := 2;
end;

procedure TfrmEffectEditor.fx5;
begin
  Notebook1.PageIndex := 3;
end;

procedure TfrmEffectEditor.fx6;
begin
  Notebook1.PageIndex := 0;
end;

procedure TfrmEffectEditor.fx7;
begin
  Notebook1.PageIndex := 0;
end;

procedure TfrmEffectEditor.fx8;
begin
  Notebook1.PageIndex := 4;
end;

procedure TfrmEffectEditor.fx9;
begin
  Notebook1.PageIndex := 5;
end;

procedure TfrmEffectEditor.fxA;
begin
  Notebook1.PageIndex := 1;
end;

procedure TfrmEffectEditor.fxB;
begin
  Notebook1.PageIndex := 0;
end;

procedure TfrmEffectEditor.fxC;
begin
  Notebook1.PageIndex := 0;
end;

procedure TfrmEffectEditor.fxD;
begin
  Notebook1.PageIndex := 0;
end;

procedure TfrmEffectEditor.fxE;
begin
  Notebook1.PageIndex := 0;
end;

procedure TfrmEffectEditor.fxF;
begin
  Notebook1.PageIndex := 0;
end;

constructor TfrmEffectEditor.Create(Cell_: PCell);
begin
  inherited Create(nil);
  Self.Cell := Cell_;
  writeln('created');
end;

end.


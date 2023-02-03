unit findreplace;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, hugedatatypes, math, constants;

type

  { TfrmFindReplace }

  TfrmFindReplace = class(TForm)
    Button1: TButton;
    Button2: TButton;
    FindNoteCheckbox: TCheckBox;
    FindInstrCheckbox: TCheckBox;
    FindFXCodeCheckbox: TCheckBox;
    FindFXParamCheckbox: TCheckBox;
    ReplaceNoteCheckbox: TCheckBox;
    ReplaceInstrCheckbox: TCheckBox;
    ReplaceFXCodeCheckbox: TCheckBox;
    ReplaceFXParamCheckbox: TCheckBox;
    FindNoteLow: TComboBox;
    InstrReplaceOp: TComboBox;
    FXParamReplaceValue: TComboBox;
    NoteReplaceValue: TComboBox;
    FXCodeReplaceOp: TComboBox;
    FXParamReplaceOp: TComboBox;
    FindFXCodeLow: TComboBox;
    FindFXCodeHigh: TComboBox;
    FindFXParamLow: TComboBox;
    FindFXParamHigh: TComboBox;
    FindNoteHigh: TComboBox;
    NoteReplaceOp: TComboBox;
    FXCodeReplaceValue: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    FindInstrLow: TSpinEdit;
    FindInstrHigh: TSpinEdit;
    InstrReplaceValue: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    Patterns: ^TPatternMap;
  end;

var
  frmFindReplace: TfrmFindReplace;

implementation

{$R *.lfm}

{ TfrmFindReplace }

procedure TfrmFindReplace.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmFindReplace.FormCreate(Sender: TObject);
begin
  if not Assigned(Patterns) then Close;
end;

procedure TfrmFindReplace.Button1Click(Sender: TObject);
var
  I, J: Integer;
  PPat: PPattern;
  DoReplace: Boolean;

  procedure ReplaceOp(Op: Integer; var Data: Integer; Operand: Integer);
  begin
    case Op of
      0: begin
        Data := Operand;
      end;
      1: begin
        Inc(Data, Operand);
      end;
      2: begin
        Dec(Data, Operand);
      end;
    end;
  end;

  procedure ReplaceOp(Op: Integer; var Data: Byte; Operand: Integer);
  begin
    case Op of
      0: begin
        Data := Operand;
      end;
      1: begin
        Inc(Data, Operand);
      end;
      2: begin
        Dec(Data, Operand);
      end;
    end;
  end;

begin
  for I := 0 to Patterns^.Count do begin
    PPat := Patterns^.Data[I];

    for J := Low(TPattern) to High(TPattern) do begin
      DoReplace := True;

      if FindNoteCheckbox.Checked then
        DoReplace := DoReplace and InRange(PPat^[J].Note, 0, 0);

      if FindInstrCheckbox.Checked then
        DoReplace := DoReplace and InRange(PPat^[J].Instrument, 0, 0);

      if FindFXCodeCheckbox.Checked then
        DoReplace := DoReplace and InRange(PPat^[J].EffectCode, 0, 0);

      if FindFXParamCheckbox.Checked then
        DoReplace := DoReplace and InRange(PPat^[J].EffectParams.Value, 0, 0);

      if DoReplace then begin
        if ReplaceNoteCheckbox.Checked then
          ReplaceOp(NoteReplaceOp.ItemIndex, PPat^[J].Note, 0);

        if ReplaceInstrCheckbox.Checked then
          ReplaceOp(InstrReplaceOp.ItemIndex, PPat^[J].Instrument, 0);

        if ReplaceFXCodeCheckbox.Checked then
          ReplaceOp(FXCodeReplaceOp.ItemIndex, PPat^[J].EffectCode, 0);

        if ReplaceFXParamCheckbox.Checked then
          ReplaceOp(FXParamReplaceOp.ItemIndex, PPat^[J].EffectParams.Value, 0);
      end;
    end;
  end;
end;

end.


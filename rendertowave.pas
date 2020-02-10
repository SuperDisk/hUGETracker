unit RenderToWave;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ExtCtrls, Spin, ComCtrls, constants, process, fgl;

type
  TOrdersSeenSet = specialize TFPGMap<Integer, Boolean>;

  { EHaltingProblem }

  EHaltingProblem = class(Exception);

  { TfrmRenderToWave }

  TfrmRenderToWave = class(TForm)
    RenderButton: TButton;
    CancelButton: TButton;
    ComboBox1: TComboBox;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    ProgressBar1: TProgressBar;
    RadioGroup1: TRadioGroup;
    SecondsSpinEdit: TSpinEdit;
    OrderSpinEdit: TSpinEdit;
    LoopTimesSpinEdit: TSpinEdit;
    procedure CancelButtonClick(Sender: TObject);
    procedure RenderButtonClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FileNameEdit1AcceptFileName(Sender: TObject; var Value: String);
    procedure FileNameEdit1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    CurrentSeenPattern: Integer;
    TimesSeenTargetPattern: Integer;

    OrdersSeen: TOrdersSeenSet;
    Rendering: Boolean;
    CancelRequested: Boolean;

    procedure UpdateButtonEnabledStates;

    procedure ExportWaveToFile(Filename: String; Seconds: Integer); overload;
    procedure ExportWaveToFile(Filename: String; OrderNum: Integer; Loops: Integer); overload;

    procedure OrderCheckFD;
  public

  end;

var
  frmRenderToWave: TfrmRenderToWave;

implementation

uses sound, machine, mainloop, vars, symparser;

{$R *.lfm}

procedure TfrmRenderToWave.FileNameEdit1AcceptFileName(Sender: TObject;
  var Value: String);
begin
  UpdateButtonEnabledStates;
end;

procedure TfrmRenderToWave.RenderButtonClick(Sender: TObject);
begin
  ProgressBar1.Position := 0;
  Rendering := True;
  CancelRequested := False;
  UpdateButtonEnabledStates;

  try
    if RadioGroup1.ItemIndex = 0 then
      ExportWaveToFile(FileNameEdit1.FileName, SecondsSpinEdit.Value)
    else
      ExportWaveToFile(FileNameEdit1.FileName, OrderSpinEdit.Value, LoopTimesSpinEdit.Value);
  except
    on E: Exception do begin
      MessageDlg('Error!', 'Couldn''t write ' + FileNameEdit1.FileName + ' !' + LineEnding +
                            LineEnding + E.Message, mtError, [mbOk], '');
    end;
  end;

  ProgressBar1.Position := 0;
  Rendering := False;
  CancelRequested := False;
  UpdateButtonEnabledStates;
end;

procedure TfrmRenderToWave.CancelButtonClick(Sender: TObject);
begin
  CancelRequested := True;
  UpdateButtonEnabledStates;
end;

procedure TfrmRenderToWave.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0: FileNameEdit1.Filter := 'Wave Files|*.wav';
    1: FileNameEdit1.Filter := 'MP3 Files|*.mp3';
  end;
end;

procedure TfrmRenderToWave.FileNameEdit1Change(Sender: TObject);
begin
  UpdateButtonEnabledStates;
end;

procedure TfrmRenderToWave.FormShow(Sender: TObject);
begin
  ProgressBar1.Position:=0;
  FileNameEdit1.FileName:='';
  RadioGroup1.ItemIndex:=0;
  SecondsSpinEdit.Value:=0;
  OrderSpinEdit.Value:=0;
  LoopTimesSpinEdit.Value:=0;
end;

procedure TfrmRenderToWave.RadioGroup1Click(Sender: TObject);
begin
  Notebook1.PageIndex := RadioGroup1.ItemIndex;
end;

procedure TfrmRenderToWave.UpdateButtonEnabledStates;
begin
  RenderButton.Enabled := (FileNameEdit1.FileName <> '') and not Rendering;
  CancelButton.Enabled := Rendering and not CancelRequested;
end;

procedure TfrmRenderToWave.ExportWaveToFile(Filename: String; Seconds: Integer);
var
  CompletedCycles: QWord = 0;
  CyclesToDo: QWord;
  OutStream: TStream;
  Proc: TProcess;
begin
  CyclesToDo := (70224*60)*Seconds;
  ProgressBar1.Position := 0;

  z80_reset;
  ResetSound;
  enablesound;

  FDCallback := nil;
  load('hUGEDriver/preview.gb');
  chdir('hUGEDriver');

  if ComboBox1.ItemIndex = 0 then begin
    OutStream := TFileStream.Create(Filename, fmCreate);
  end
  else begin
    Proc := TProcess.Create(nil);
    Proc.Executable := 'lame';
    with Proc.Parameters do begin
        Add('-');
        Add(Filename);
    end;
    Proc.Options := [poUsePipes];
    Proc.Execute;

    OutStream := TMemoryStream.Create;
  end;

  try
    BeginWritingSoundToStream(OutStream);

    while (CompletedCycles < CyclesToDo) and not CancelRequested do begin
      Inc(CompletedCycles, z80_decode);
      ProgressBar1.Position := Trunc((CompletedCycles / CyclesToDo)*100);
      if Random(500) = 1 then Application.ProcessMessages;
    end;

    if CancelRequested then Exit;

  finally
    EndWritingSoundToStream;

    Chdir('..');

    try
      if ComboBox1.ItemIndex = 1 then begin
        OutStream.Seek(0, soFromBeginning);
        Proc.Input.CopyFrom(OutStream, OutStream.Size);
        Proc.CloseInput;
        Proc.WaitOnExit;
      end;
    finally
      if ComboBox1.ItemIndex = 1 then Proc.Free;
      OutStream.Free
    end;
  end;
end;

procedure TfrmRenderToWave.ExportWaveToFile(Filename: String;
  OrderNum: Integer; Loops: Integer);
var
  TimesToSeePattern: Integer;
begin
  RenderButton.Enabled:=False;

  TimesToSeePattern := (LoopTimesSpinEdit.Value+1);
  TimesSeenTargetPattern := 0;
  CurrentSeenPattern := -1;

  z80_reset;
  ResetSound;
  enablesound;

  //BeginWritingSoundToStream(Filename);

  FDCallback := @OrderCheckFD;

  load('hUGEDriver/preview.gb');
  SymbolTable := ParseSymFile('hUGEDriver/preview.sym');

  try
    while TimesSeenTargetPattern < TimesToSeePattern do begin
      z80_decode;
      ProgressBar1.Position := Trunc((TimesSeenTargetPattern / TimesToSeePattern)*100);
      if Random(500) = 1 then Application.ProcessMessages;
    end;
  except
    on E: EHaltingProblem do begin
      MessageDlg('Error!', E.Message, mtError, [mbOK], '');
    end;
  end;

  EndWritingSoundToStream;

  RenderButton.Enabled:=True;
end;

procedure TfrmRenderToWave.OrderCheckFD;
var
  Pat: Integer;
begin
  {if ((OrdersSeen.IndexOf(Pat) <> -1) and (OrdersSeen.KeyData[Pat])) and
     (OrdersSeen.IndexOf(OrderSpinEdit.Value) = -1) then
       raise EHaltingProblem.Create('The specified target order is never reached again in the song!');}

  Pat := (PeekSymbol(SYM_CURRENT_ORDER) div 2);
  if CurrentSeenPattern <> Pat then begin
    CurrentSeenPattern := Pat;

    if Pat = OrderSpinEdit.Value then
      Inc(TimesSeenTargetPattern);
  end;
end;

end.


unit RenderToWave;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ExtCtrls, Spin, ComCtrls, constants, process;

type
  TRenderFormat = (rfWave, rfMP3);

  { EHaltingProblem }

  EHaltingProblem = class(Exception);

  { TfrmRenderToWave }

  TfrmRenderToWave = class(TForm)
    RenderButton: TButton;
    CancelButton: TButton;
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
    PatternsSeen, CurrentSeenPattern, TimesSeenTargetPattern: Integer;

    Rendering: Boolean;
    CancelRequested: Boolean;

    procedure UpdateButtonEnabledStates;

    procedure ExportWaveToFile(Filename: String; Format: TRenderFormat);

    procedure RenderSeconds(Seconds: Integer);
    procedure RenderLoops(TargetOrder: Integer; Loops: Integer);

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
var
  RF: TRenderFormat;
begin
  ProgressBar1.Position := 0;
  Rendering := True;
  CancelRequested := False;
  UpdateButtonEnabledStates;

  case LowerCase(ExtractFileExt(FileNameEdit1.FileName)) of
    '.wav': RF := rfWave;
    '.mp3': RF := rfMP3;
    else RF := rfWave;
  end;

  try
    ExportWaveToFile(FileNameEdit1.FileName, RF);
  except
    on E: Exception do begin
      MessageDlg('Error!', 'Couldn''t write ' + FileNameEdit1.FileName + '!' + LineEnding +
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
  {case ComboBox1.ItemIndex of
    0: FileNameEdit1.Filter := 'Wave Files|*.wav';
    1: FileNameEdit1.Filter := 'MP3 Files|*.mp3';
  end;}
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

procedure TfrmRenderToWave.ExportWaveToFile(Filename: String; Format: TRenderFormat);
var
  OutStream: TStream;
  Proc: TProcess;
begin
  ProgressBar1.Position := 0;

  z80_reset;
  ResetSound;
  //enablesound;

  FDCallback := nil;
  load('hUGEDriver/preview.gb');
  chdir('hUGEDriver');

  if format = rfWave then begin
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

    // Choose which rendering time strategy to use
    case RadioGroup1.ItemIndex of
      0: RenderSeconds(SecondsSpinEdit.Value);
      1: RenderLoops(OrderSpinEdit.Value, LoopTimesSpinEdit.Value);
    end;

    if CancelRequested then Exit;

  finally
    EndWritingSoundToStream;

    Chdir('..');

    try
      if Format = rfMP3 then begin
        OutStream.Seek(0, soFromBeginning);
        Proc.Input.CopyFrom(OutStream, OutStream.Size);
        Proc.CloseInput;
        Proc.WaitOnExit;
      end;
    finally
      if Format = rfMP3 then Proc.Free;
      OutStream.Free
    end;
  end;
end;

procedure TfrmRenderToWave.RenderSeconds(Seconds: Integer);
var
  CompletedCycles: QWord = 0;
  CyclesToDo: QWord;
begin
  CyclesToDo := (70224*60)*Seconds;
  while (CompletedCycles < CyclesToDo) and not CancelRequested do begin
    Inc(CompletedCycles, z80_decode);
    ProgressBar1.Position := Trunc((CompletedCycles / CyclesToDo)*100);
    if Random(500) = 1 then Application.ProcessMessages;
  end;
end;

procedure TfrmRenderToWave.RenderLoops(TargetOrder: Integer; Loops: Integer);
var
  TimesToSeePattern: Integer;
  OldFD: TFDCallback;
  OrderCount: Integer;
begin
  OldFD := FDCallback;
  FDCallback := @OrderCheckFD;

  OrderCount := PeekSymbol(SYM_ORDER_COUNT) div 2;

  CurrentSeenPattern := -1;
  TimesSeenTargetPattern := 0;
  PatternsSeen := 0;
  TimesToSeePattern := (LoopTimesSpinEdit.Value+1);

  while (TimesSeenTargetPattern < TimesToSeePattern) and not CancelRequested do begin
    z80_decode;
    ProgressBar1.Position := Trunc((TimesSeenTargetPattern / TimesToSeePattern)*100);
    if Random(500) = 1 then Application.ProcessMessages;

    if (PatternsSeen > OrderCount) and (TimesSeenTargetPattern <= 1) then
      raise EHaltingProblem.Create('The specified loop point cannot be reached more than once!');
  end;

  FDCallback := OldFD;
end;

procedure TfrmRenderToWave.OrderCheckFD;
var
  Pat: Integer;
begin
  Pat := (PeekSymbol(SYM_CURRENT_ORDER) div 2);
  if CurrentSeenPattern <> Pat then begin
    Inc(PatternsSeen);
    CurrentSeenPattern := Pat;

    if Pat = OrderSpinEdit.Value then
      Inc(TimesSeenTargetPattern);
  end;
end;

end.


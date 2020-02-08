unit RenderToWave;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ExtCtrls, Spin, ComCtrls, constants;

type

  { TfrmRenderToWave }

  TfrmRenderToWave = class(TForm)
    Button1: TButton;
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
    procedure Button1Click(Sender: TObject);
    procedure FileNameEdit1AcceptFileName(Sender: TObject; var Value: String);
    procedure FileNameEdit1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    CurrentSeenPattern: Integer;
    TimesSeenTargetPattern: Integer;

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
  Button1.Enabled := (FileNameEdit1.FileName <> '');
end;

procedure TfrmRenderToWave.Button1Click(Sender: TObject);
begin
  if RadioGroup1.ItemIndex = 0 then
    ExportWaveToFile(FileNameEdit1.FileName, SecondsSpinEdit.Value)
  else
    ExportWaveToFile(FileNameEdit1.FileName, OrderSpinEdit.Value, LoopTimesSpinEdit.Value);
end;

procedure TfrmRenderToWave.FileNameEdit1Change(Sender: TObject);
begin
  Button1.Enabled := (FileNameEdit1.FileName <> '');
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

procedure TfrmRenderToWave.ExportWaveToFile(Filename: String; Seconds: Integer);
var
  CompletedCycles: QWord = 0;
  CyclesToDo: QWord;
begin
  CyclesToDo := (70224*60)*Seconds;
  ProgressBar1.Position := 0;

  z80_reset;
  ResetSound;
  enablesound;

  BeginWritingSoundToFile(Filename);

  FDCallback := nil;

  load('hUGEDriver/preview.gb');

  while CompletedCycles < CyclesToDo do begin
    Inc(CompletedCycles, z80_decode);
    ProgressBar1.Position := Trunc((CompletedCycles / CyclesToDo)*100);
    if Random(500) = 1 then Application.ProcessMessages;
  end;

  EndWritingSoundToFile;
end;

procedure TfrmRenderToWave.ExportWaveToFile(Filename: String;
  OrderNum: Integer; Loops: Integer);
var
  TimesToSeePattern: Integer;
begin
  TimesToSeePattern := (LoopTimesSpinEdit.Value+1);
  TimesSeenTargetPattern := 0;
  CurrentSeenPattern := -1;

  z80_reset;
  ResetSound;
  enablesound;

  BeginWritingSoundToFile(Filename);

  FDCallback := @OrderCheckFD;

  load('hUGEDriver/preview.gb');
  SymbolTable := ParseSymFile('hUGEDriver/preview.sym');

  while TimesSeenTargetPattern < TimesToSeePattern do begin
    z80_decode;
    ProgressBar1.Position := Trunc((TimesSeenTargetPattern / TimesToSeePattern)*100);
    if Random(500) = 1 then Application.ProcessMessages;
  end;

  EndWritingSoundToFile;
end;

procedure TfrmRenderToWave.OrderCheckFD;
var
  Pat: Integer;
begin
  Pat := (PeekSymbol(SYM_CURRENT_ORDER) div 2);
  if CurrentSeenPattern <> Pat then begin
    CurrentSeenPattern := Pat;

    if Pat = OrderSpinEdit.Value then
      Inc(TimesSeenTargetPattern);
  end;
end;

end.


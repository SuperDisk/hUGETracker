unit RenderToWave;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ExtCtrls, Spin, ComCtrls, Arrow, constants, process;

type
  { EInvalidOrder }

  EInvalidOrder = class(Exception);

  { TfrmRenderToWave }

  TfrmRenderToWave = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    PlayEntireSongRadioButton: TRadioButton;
    FromPositionRadioButton: TRadioButton;
    RenderButton: TButton;
    CancelButton: TButton;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    PlayEntireSongSpinEdit: TSpinEdit;
    FromPositionLowerSpinEdit: TSpinEdit;
    FromPositionUpperSpinEdit: TSpinEdit;
    procedure CancelButtonClick(Sender: TObject);
    procedure RenderButtonClick(Sender: TObject);
    procedure FileNameEdit1AcceptFileName(Sender: TObject; var Value: String);
    procedure FileNameEdit1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    SeenPatterns: array of Integer;
    StartOfSong, CurrentPattern: Integer;

    Rendering: Boolean;
    CancelRequested: Boolean;

    procedure UpdateButtonEnabledStates;

    procedure ExportWaveToFile(Filename: String);

    {procedure RenderSeconds(Seconds: Integer);
    procedure RenderLoops(TargetOrder: Integer; Loops: Integer);}
    procedure RenderEntireSong(Times: Integer);
    procedure RenderFromPosition(FromPos, ToPos: Integer);

    procedure OrderCheckCallback;
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
    ExportWaveToFile(FileNameEdit1.FileName);
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

procedure TfrmRenderToWave.FileNameEdit1Change(Sender: TObject);
begin
  UpdateButtonEnabledStates;
end;

procedure TfrmRenderToWave.FormShow(Sender: TObject);
begin
  ProgressBar1.Position:=0;
  FileNameEdit1.FileName:='';
  PlayEntireSongRadioButton.Checked:=True;
  PlayEntireSongSpinEdit.Value:=0;
  FromPositionLowerSpinEdit.Value:=0;
  FromPositionUpperSpinEdit.Value:=0;
end;

procedure TfrmRenderToWave.UpdateButtonEnabledStates;
begin
  RenderButton.Enabled := (FileNameEdit1.FileName <> '') and not Rendering;
  CancelButton.Enabled := Rendering and not CancelRequested;
end;

procedure TfrmRenderToWave.ExportWaveToFile(Filename: String);
var
  Proc: TProcess;
begin
  ProgressBar1.Position := 0;

  z80_reset;
  ResetSound;
  enablesound;

  FCCallback := nil;
  load('render/preview.gb');

  Proc := TProcess.Create(nil);
  Proc.Executable := 'ffmpeg';
  with Proc.Parameters do begin
    // HACK: to prevent ffmpeg from writing to stderr, we disable all output
    // This is needed because ffmpeg blocks unless you read what it writes
    Add('-nostats');
    Add('-loglevel');
    Add('0');

    Add('-sample_rate');
    Add(IntToStr(PlaybackFrequency));
    Add('-y');
    Add('-f');
    Add('f32le');
    Add('-channels');
    Add('2');
    Add('-i');
    Add('-');
    Add(Filename);
  end;
  Proc.Options := [poUsePipes, poNoConsole];
  Proc.Execute;

  try
    BeginWritingSoundToStream(Proc.Input);

    if PlayEntireSongRadioButton.Checked then
      RenderEntireSong(PlayEntireSongSpinEdit.Value)
    else if FromPositionRadioButton.Checked then
      RenderFromPosition(FromPositionLowerSpinEdit.Value, FromPositionUpperSpinEdit.Value);

    if CancelRequested then Exit;

  finally
    EndWritingSoundToStream;
    Proc.CloseInput;
    Proc.WaitOnExit;
    Proc.Free;
  end;
end;

procedure TfrmRenderToWave.RenderEntireSong(Times: Integer);
var
  OldFC: TCPUCallback;
begin
  OldFC := FCCallback;
  FCCallback := @OrderCheckCallback;

  SetLength(SeenPatterns, PeekSymbol(SYM_ORDER_COUNT) div 2);
  StartOfSong := -1;
  CurrentPattern := -1;

  repeat
    z80_decode;
    if Random(500) = 1 then Application.ProcessMessages;
  until ((StartOfSong <> -1) and (SeenPatterns[StartOfSong]-1 = PlayEntireSongSpinEdit.Value)) or CancelRequested;

  SetLength(SeenPatterns, 0);
  FCCallback := OldFC;
end;

procedure TfrmRenderToWave.RenderFromPosition(FromPos, ToPos: Integer);
var
  OldFC: TCPUCallback;
  StartPattern, TargetPattern: Integer;
  SawTargetPattern: Boolean;
begin
  OldFC := FCCallback;
  FCCallback := @OrderCheckCallback;

  SetLength(SeenPatterns, PeekSymbol(SYM_ORDER_COUNT) div 2);
  StartOfSong := -1;
  CurrentPattern := -1;
  StartPattern := FromPositionLowerSpinEdit.Value;
  TargetPattern := FromPositionUpperSpinEdit.Value;
  SawTargetPattern := False;

  if StartPattern > High(SeenPatterns) then
      raise EInvalidOrder.Create('The specified order doesn''t exist!');

  PokeSymbol(SYM_CURRENT_ORDER, 2*StartPattern);
  PokeSymbol(SYM_ROW, 0);

  repeat
    z80_decode;

    if CurrentPattern = TargetPattern then
      SawTargetPattern := True;

    if Random(500) = 1 then Application.ProcessMessages;
  until (StartOfSong <> -1) or
        (SawTargetPattern and (CurrentPattern <> TargetPattern)) or
        CancelRequested;

  SetLength(SeenPatterns, 0);
  FCCallback := OldFC;
end;

procedure TfrmRenderToWave.OrderCheckCallback;
var
  Pat: Integer;
begin
  Pat := (PeekSymbol(SYM_CURRENT_ORDER) div 2);

  if (SeenPatterns[Pat] <> 0) and (StartOfSong = -1) then
      StartOfSong := Pat;

  Inc(SeenPatterns[Pat]);
  CurrentPattern := Pat;
end;

end.


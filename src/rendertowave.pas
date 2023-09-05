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
    ComboBox1: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    PlayEntireSongRadioButton: TRadioButton;
    FromPositionRadioButton: TRadioButton;
    RenderButton: TButton;
    CancelButton: TButton;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    PlayEntireSongSpinEdit: TSpinEdit;
    FromPositionLowerSpinEdit: TSpinEdit;
    FromPositionUpperSpinEdit: TSpinEdit;
    procedure CancelButtonClick(Sender: TObject);
    procedure RenderButtonClick(Sender: TObject);
    procedure FileNameEdit1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    SeenPatterns: array of Integer;
    StartOfSong, CurrentPattern: Integer;

    Rendering: Boolean;
    CancelRequested: Boolean;

    function FilenameToFormatIndex: Integer;
    function GetFFMPEGFormat: String;
    procedure UpdateUI;

    procedure ExportWaveToFile(Filename: String);

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

procedure TfrmRenderToWave.RenderButtonClick(Sender: TObject);
begin
  Rendering := True;
  CancelRequested := False;
  UpdateUI;

  try
    ExportWaveToFile(FileNameEdit1.FileName);
  except
    on E: Exception do begin
      MessageDlg('Error!', 'Couldn''t write ' + FileNameEdit1.FileName + '!' + LineEnding +
                            LineEnding + E.Message, mtError, [mbOk], '');
    end;
  end;

  Rendering := False;
  CancelRequested := False;
  UpdateUI;
end;

procedure TfrmRenderToWave.CancelButtonClick(Sender: TObject);
begin
  CancelRequested := True;
  UpdateUI;
end;

procedure TfrmRenderToWave.FileNameEdit1Change(Sender: TObject);
begin
  ComboBox1.ItemIndex := FilenameToFormatIndex;
  UpdateUI;
end;

procedure TfrmRenderToWave.FormShow(Sender: TObject);
begin
  FileNameEdit1.FileName:='';
  PlayEntireSongRadioButton.Checked:=True;
  PlayEntireSongSpinEdit.Value:=1;
  FromPositionLowerSpinEdit.Value:=0;
  FromPositionUpperSpinEdit.Value:=0;
end;

function TfrmRenderToWave.FilenameToFormatIndex: Integer;
begin
  case LowerCase(ExtractFileExt(FileNameEdit1.FileName)) of
    '.wav': Result := 0;
    '.mp3': Result := 1;
    '.flac': Result := 2;
    else Result := 0;
  end;
end;

function TfrmRenderToWave.GetFFMPEGFormat: String;
begin
  case ComboBox1.ItemIndex of
    0: Result := 'wav';
    1: Result := 'mp3';
    2: Result := 'flac';
    else Result := 'wav';
  end;
end;

procedure TfrmRenderToWave.UpdateUI;
begin
  RenderButton.Enabled := (FileNameEdit1.FileName <> '') and not Rendering;
  CancelButton.Enabled := Rendering and not CancelRequested;
end;

procedure TfrmRenderToWave.ExportWaveToFile(Filename: String);
var
  Proc: TProcess;
begin
  z80_reset;
  ResetSound;
  enablesound;

  FCCallback := nil;
  load(ConcatPaths([CacheDir, 'render', 'preview.gb']));

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
    Add('-f');
    Add(GetFFMPEGFormat);
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

    Panel1.Caption := 'Ready';
  end;
end;

procedure TfrmRenderToWave.RenderEntireSong(Times: Integer);
var
  OldFC, OldFD: TCPUCallback;
begin
  OldFC := FCCallback;
  OldFD := FDCallback;
  FCCallback := @OrderCheckCallback;
  FDCallback := nil;

  SetLength(SeenPatterns, PeekSymbol(SYM_SONG_DESCRIPTOR, 4) div 2);
  StartOfSong := -1;
  CurrentPattern := -1;

  repeat
    z80_decode;
    if Random(500) = 1 then Application.ProcessMessages;
  until ((StartOfSong <> -1) and (SeenPatterns[StartOfSong]-1 = PlayEntireSongSpinEdit.Value)) or CancelRequested;

  SetLength(SeenPatterns, 0);
  FCCallback := OldFC;
  FDCallback := OldFD;
end;

procedure TfrmRenderToWave.RenderFromPosition(FromPos, ToPos: Integer);
var
  OldFC, OldFD: TCPUCallback;
  StartPattern, TargetPattern: Integer;
  SawTargetPattern: Boolean;
begin
  OldFC := FCCallback;
  OldFD := FDCallback;
  FCCallback := @OrderCheckCallback;
  FDCallback := nil;

  SetLength(SeenPatterns, PeekSymbol(SYM_SONG_DESCRIPTOR, 4) div 2);
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
  FDCallback := OldFD;
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
  Panel1.Caption := 'Rendering order '+IntToStr(CurrentPattern+1);
end;

end.


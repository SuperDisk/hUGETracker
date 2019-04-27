{+-----------------------------------------------------------------------------+
 | Author:      Christian Hackbart
 | Description: main Unit
 | Copyright (c) 2000 Christian Hackbart
 | Stand: 15.12.2000
 |
 | http://www.tu-ilmenau.de/~hackbart
 +----------------------------------------------------------------------------+}

unit UnitMain;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  lcl_out, vars, z80cpu, mainloop, machine, debugger, sound,
  StdCtrls, ExtCtrls, ComCtrls, Menus;

type

  { TfrmGameboy }

  TfrmGameboy = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Load1: TMenuItem;
    N3: TMenuItem;
    Message1: TMenuItem;
    N4: TMenuItem;
    Exit1: TMenuItem;
    CPU1: TMenuItem;
    PaintBox1: TPaintBox;
    Reset2: TMenuItem;
    Pause1: TMenuItem;
    N2: TMenuItem;
    LoadStat: TMenuItem;
    SaveStat: TMenuItem;
    N8: TMenuItem;
    Dump1: TMenuItem;
    MemoryMap1: TMenuItem;
    RamBank1: TMenuItem;
    ChrData1: TMenuItem;
    N1: TMenuItem;
    CGBVRAM1: TMenuItem;
    CGBWRAM1: TMenuItem;
    CGBCRAM1: TMenuItem;
    N9: TMenuItem;
    SGBVRAM1: TMenuItem;
    sgbtiledata1: TMenuItem;
    Option3: TMenuItem;
    AutoWait: TMenuItem;
    FrameSkip1: TMenuItem;
    N01: TMenuItem;
    N11: TMenuItem;
    N21: TMenuItem;
    N31: TMenuItem;
    N41: TMenuItem;
    N51: TMenuItem;
    Priority: TMenuItem;
    priority1: TMenuItem;
    priority2: TMenuItem;
    priority3: TMenuItem;
    N14: TMenuItem;
    priority4: TMenuItem;
    N12: TMenuItem;
    Video1: TMenuItem;
    x11: TMenuItem;
    x21: TMenuItem;
    x31: TMenuItem;
    x41: TMenuItem;
    N7: TMenuItem;
    full1: TMenuItem;
    fullscreen1: TMenuItem;
    full2: TMenuItem;
    full3: TMenuItem;
    full4: TMenuItem;
    Sound1: TMenuItem;
    Off: TMenuItem;
    N6: TMenuItem;
    Ch1: TMenuItem;
    Ch2: TMenuItem;
    Ch3: TMenuItem;
    Ch4: TMenuItem;
    Net1: TMenuItem;
    N15: TMenuItem;
    SerialIF1: TMenuItem;
    serial1: TMenuItem;
    serial2: TMenuItem;
    serial3: TMenuItem;
    Palette1: TMenuItem;
    preset11: TMenuItem;
    preset21: TMenuItem;
    preset31: TMenuItem;
    preset41: TMenuItem;
    preset51: TMenuItem;
    N10: TMenuItem;
    Setting1: TMenuItem;
    StatusBar: TStatusBar;
    OpenDialog: TOpenDialog;
    Debugger: TMenuItem;
    SaveDialog: TSaveDialog;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FileOpenClick(Sender: TObject);
    procedure KillClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FrameClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Pause(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure setPriority(Sender: TObject);
    procedure setResolution(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DebuggerClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure AutoWaitClick(Sender: TObject);
    procedure LoadStatClick(Sender: TObject);
    procedure SaveStatClick(Sender: TObject);
    procedure SetSound(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure SetAudioChannel(Sender: TObject);
  private
    { Private-Deklarationen}
    fullwindow: boolean;
    FDiff: integer; // Caption Height
    procedure Emulation(var msg: TMessage); message wm_user;
    procedure SetWindow;
    procedure LoadRom(Rom: String);

    procedure ShowCaption;
    procedure RemoveCaption;
  public
    { Public-Deklarationen}
    timervar: byte;
  end;

var
  frmGameboy: TfrmGameboy;

implementation

uses UnitMapview;

{$R *.lfm}


procedure TfrmGameboy.RemoveCaption;
begin
  FDiff := GetSystemMetrics(SM_CYCAPTION);
  SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_Style) and not WS_Caption);
  Height := Height - FDiff;
  Height := Height + FDiff;
end;

procedure TfrmGameboy.ShowCaption;
begin
  if Self.ComponentState = [csDestroying] then
    exit;
  SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_Style) + WS_Caption);
  Height := Height + FDiff;
  Height := Height - FDiff;
end;

procedure TfrmGameboy.FormCreate(Sender: TObject);
begin
  loadstat.Enabled := False;
  savestat.Enabled := False;

  f_stopped := True;
  randomize;
  Height := 144 * 2;
  Width := 160 * 2;

  StartLCL;

  make_pix;
  SetPriority(priority2); // normal Priority

  if assigned(cart) then
    freemem(cart, cartsize);
  z80_reset;

  SetAudioChannel(Self);
  enablesound;
end;

procedure TfrmGameboy.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if assigned(cart) then
    freemem(cart, cartsize);
  f_stopped := False;
  application.terminate;
end;

const
  CyclesPerFrame : Integer = 70224;
  TimePerFrame: Double = 1.0 / 60.0;

procedure TfrmGameboy.Emulation(var Msg: TMessage);
var
  li: Large_Integer;
  tickFreq, cycles: Integer;
  frameStart, frameEnd: Integer;
  frameElapsedInSec: Double;
begin
  lastTime := 0;

  cycles := 0;

  QueryPerformanceFrequency(@li);
  tickFreq := li.QuadPart;
  QueryPerformanceCounter(@li);
  frameStart := li.QuadPart;

  repeat
    application.ProcessMessages;

    // Pause
    if f_stopped then
      while f_stopped do
        application.ProcessMessages;

    while (cycles < CyclesPerFrame) do
      cycles += z80_decode;

    cycles -= CyclesPerFrame;

    repeat
      QueryPerformanceCounter(@li);
      frameEnd := li.QuadPart;

      frameElapsedInSec := (frameEnd - frameStart) / tickFreq;
    until (frameElapsedInSec > TimePerFrame) and (not SoundBufferTooFull);

    frameStart := frameEnd;
  until application.Terminated;
end;

procedure TfrmGameboy.LoadRom(Rom: String);
begin
  f_stopped := True;
  load(Rom);
  statusbar.Panels[2].Text := getcompany;
  statusbar.Panels[1].Text := speicher;
  statusbar.panels[0].Text := romname;
  z80_reset;
  z80_reset;
  f_stopped := False;
  gb_speed := 1;
  loadstat.Enabled := True;
  savestat.Enabled := True;
end;

procedure TfrmGameboy.FileOpenClick(Sender: TObject);
begin
  loadstat.Enabled := False;
  savestat.Enabled := False;

  opendialog.Filter :=
    'All Roms [*.gb;*.gbc;*.rom]|*.gb;*.gbc;*.rom|Gameboy [*.gb]|*.gb|Gameboy Color [*.gbc]|*.gbc|All Files [*.*]|*.*';

  if opendialog.Execute then LoadRom(opendialog.filename);
end;

procedure TfrmGameboy.KillClick(Sender: TObject);
var
  i: byte;
begin
  for i := 1 to 4 do
    z80_Reset;
end;

procedure TfrmGameboy.FormActivate(Sender: TObject);
begin
  onActivate := nil;
  repaint;
  with statusbar do
  begin
    Panels[2].Text := getcompany;
    Panels[1].Text := speicher;
    panels[0].Text := romname;
  end;
  z80_reset;
  z80_reset;
  f_stopped := True;
  gb_speed := 1;

  frmGameboy.DoubleBuffered := True;
  SetPaintBox(PaintBox1);
  postmessage(handle, wm_user, 0, 0);

  if ParamCount >= 1 then
    LoadRom(ParamStr(1));
end;

procedure TfrmGameboy.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmGameboy.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case key of
    Ord('Y'): k_b := 0;
    Ord('X'): k_a := 0;
    Ord('A'): k_start := 0;
    Ord('S'): k_select := 0;
    Ord('P'):
    begin
      f_stopped := not f_stopped;
      pause1.Checked := f_stopped;
    end;
    VK_UP: k_up := 0;
    VK_DOWN: k_down := 0;
    VK_LEFT: k_left := 0;
    VK_RIGHT: k_right := 0;
    VK_F1:
    begin
      ch1.Checked := not Ch1.Checked;
      snd[1].ChannelOFF := not ch1.Checked;
    end;
    VK_F2:
    begin
      ch2.Checked := not Ch2.Checked;
      snd[2].ChannelOFF := not ch2.Checked;
    end;
    VK_F3:
    begin
      ch3.Checked := not Ch3.Checked;
      snd[3].ChannelOFF := not ch3.Checked;
    end;
    VK_F4:
    begin
      ch4.Checked := not Ch4.Checked;
      snd[4].ChannelOFF := not ch4.Checked;
    end;
  end;
end;

procedure TfrmGameboy.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case KEY of
    Ord('Y'): k_b := 1;
    Ord('X'): k_a := 1;
    Ord('A'): k_start := 1;
    Ord('B'): k_select := 1;
    VK_UP: k_up := 1;
    VK_DOWN: k_down := 1;
    VK_LEFT: k_left := 1;
    VK_RIGHT: k_right := 1;
  end;
end;

procedure TfrmGameboy.FrameClick(Sender: TObject);
begin
  n01.Checked := False;
  n11.Checked := False;
  n21.Checked := False;
  n31.Checked := False;
  n41.Checked := False;
  n51.Checked := False;

  tmenuitem(Sender).Checked := True;
end;

procedure TfrmGameboy.PaintBox1Paint(Sender: TObject);
begin
  UpdateCanvas(PaintBox1.Width, PaintBox1.Height, PaintBox1.Canvas);
end;

procedure TfrmGameboy.Pause(Sender: TObject);
begin
  pause1.Checked := not pause1.Checked;
  f_stopped := pause1.Checked;
end;

procedure TfrmGameboy.ResetClick(Sender: TObject);
begin
  z80_reset;
end;

procedure TfrmGameboy.setPriority(Sender: TObject);
var
  ProcessID: DWORD;
  ProcessHandle: THandle;
  Priority: integer;
begin
{ HIGH_PRIORITY_CLASS,
  IDLE_PRIORITY_CLASS,
  NORMAL_PRIORITY_CLASS
  REALTIME_PRIORITY_CLASS}
{THREAD_PRIORITY_ABOVE_NORMAL,
 THREAD_PRIORITY_BELOW_NORMAL,
 THREAD_PRIORITY_HIGHEST,
 THREAD_PRIORITY_IDLE,
 THREAD_PRIORITY_LOWEST,
 THREAD_PRIORITY_NORMAL,
 THREAD_PRIORITY_TIME_CRITICAL}

  priority1.Checked := False;
  priority2.Checked := False;
  priority3.Checked := False;
  priority4.Checked := False;
  tmenuitem(Sender).Checked := True;

  if Sender = priority1 then
    priority := IDLE_PRIORITY_CLASS;
  if Sender = priority2 then
    priority := NORMAL_PRIORITY_CLASS;
  if Sender = priority3 then
    priority := HIGH_PRIORITY_CLASS;
  if Sender = priority4 then
    priority := REALTIME_PRIORITY_CLASS;

  ProcessID := GetCurrentProcessID;
  ProcessHandle := OpenProcess(PROCESS_SET_INFORMATION, False, ProcessID);
  SetPriorityClass(ProcessHandle, Priority);
{ ThreadHandle := GetCurrentThread;
  SetThreadPriority(ThreadHandle, THREAD_PRIORITY_HIGHEST);}
end;

procedure TfrmGameboy.setResolution(Sender: TObject);
begin
  // Resolution
  Width := 160;
  Height := 144;
  if Sender = x21 then
  begin
    Width := 160 * 2;
    Height := 144 * 2;
  end;
  if Sender = x31 then
  begin
    Width := 160 * 3;
    Height := 144 * 3;
  end;
  if Sender = x41 then
  begin
    Width := 160 * 4;
    Height := 144 * 4;
  end;
  Width := Width + 8;
  if not fullwindow then
    Height := Height + statusbar.Height + 46;
end;

procedure TfrmGameboy.SetWindow;
begin
  fullwindow := not fullwindow;
  if fullwindow then
  begin
    Menu := nil;
    statusbar.Hide;
    RemoveCaption;
  end
  else
  begin
    Menu := mainmenu;
    statusbar.Show;
    ShowCaption;
  end;
end;

procedure TfrmGameboy.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if button = mbRight then
    SetWindow
  else
  if (button = mbleft) and (fullwindow) then
  begin
    releasecapture;
    TWincontrol(Self).Perform(WM_syscommand, $F012, 0);
  end;
end;

procedure TfrmGameboy.DebuggerClick(Sender: TObject);
begin
  //frmdebug.show;
  frmMapview.Show;
  frmMapview.Update;
end;

procedure TfrmGameboy.FormResize(Sender: TObject);
begin
  if f_stopped then
    exit;
  statusbar.Panels[0].Text := IntToStr(Height) + ':' + IntToStr(Width);
end;

procedure TfrmGameboy.AutoWaitClick(Sender: TObject);
begin
  AutoWait.Checked := not AutoWait.Checked;
end;

procedure TfrmGameboy.LoadStatClick(Sender: TObject);
begin
  openDialog.filter := 'Savestate (*.gbs)|*.gbs';
  if opendialog.Execute then
    case ReadGameState(opendialog.filename) of
      1: ShowMessage('This ROM does not use a RAM...');
      2: ShowMessage('File not found...');
      3: ShowMessage('Not a valid Savegame...');
    end;
end;

procedure TfrmGameboy.SaveStatClick(Sender: TObject);
begin
  saveDialog.filter := 'Savestate (*.gbs)|*.gbs';
  if savedialog.Execute then
    SaveGameState(savedialog.filename);
end;

procedure TfrmGameboy.SetSound(Sender: TObject);
begin
  off.Checked := not off.Checked;
  if not off.Checked then
    enablesound
  else
    disablesound;
end;

procedure TfrmGameboy.TimerTimer(Sender: TObject);
begin
  Inc(timervar);
end;

procedure TfrmGameboy.SetAudioChannel(Sender: TObject);
begin
  if Sender = ch1 then
    ch1.Checked := not ch1.Checked
  else
  if Sender = ch2 then
    ch2.Checked := not ch2.Checked
  else
  if Sender = ch3 then
    ch3.Checked := not ch3.Checked
  else
  if Sender = ch4 then
    ch4.Checked := not ch4.Checked;

  snd[1].ChannelOFF := not CH1.Checked;
  snd[2].ChannelOFF := not CH2.Checked;
  snd[3].ChannelOFF := not CH3.Checked;
  snd[4].ChannelOFF := not CH4.Checked;
end;

end.

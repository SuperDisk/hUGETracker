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
  ddraw_out,dib_out,DirectDraw,vars,z80cpu,
  gfx,mainloop,machine, debugger,sound,
  StdCtrls, ExtCtrls,ComCtrls, Menus;


type
  TfrmGameboy = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Load1: TMenuItem;
    N3: TMenuItem;
    Message1: TMenuItem;
    N4: TMenuItem;
    Exit1: TMenuItem;
    CPU1: TMenuItem;
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
    N5: TMenuItem;
    Outputmode1: TMenuItem;
    mnuDirectdraw: TMenuItem;
    mnuDib: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FileOpenClick(Sender: TObject);
    procedure KillClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FrameClick(Sender: TObject);
    procedure Pause(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure setPriority(Sender: TObject);
    procedure setResolution(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DebuggerClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure AutoWaitClick(Sender: TObject);
    procedure LoadStatClick(Sender: TObject);
    procedure SaveStatClick(Sender: TObject);
    procedure SetSound(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure SetAudioChannel(Sender: TObject);
    procedure mnuOutputClick(Sender: TObject);
  private
    { Private-Deklarationen}
    fullwindow:Boolean;
    FDiff    : Integer; // Caption Height
    procedure Emulation(var msg:TMessage); message wm_user;
    procedure SetWindow;

    procedure ShowCaption;
    procedure RemoveCaption;
  public
    { Public-Deklarationen}
    timervar: byte;
    procedure RealSpeedEmulation;
    procedure _RealSpeedEmulation;
  end;

var
  frmGameboy: TfrmGameboy;

implementation

uses UnitDebug, UnitMapview;

{$R *.lfm}


procedure TfrmGameboy.RemoveCaption;
begin
  FDiff := GetSystemMetrics(SM_CYCAPTION);
  SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_Style) and not WS_Caption);
  Height:=Height - FDiff;
  Height:=Height + FDiff;
end;

procedure TfrmGameboy.ShowCaption;
begin
  If Self.ComponentState = [csDestroying] Then exit;
  SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_Style) + WS_Caption);
  Height:=Height + FDiff;
  Height:=Height - FDiff;
end;

procedure TfrmGameboy.FormCreate(Sender: TObject);
var li:Large_Integer;
begin
     loadstat.enabled:=False;
     savestat.enabled:=false;

     f_stopped:=True;
     randomize;
     Height:=144*2;
     width:=160*2;
     statbottom:=statusbar.Height;

     start_dib(Handle,32); //initialize DIB
     mnuDirectdraw.enabled:=(start_dx(Handle)=0); // is Directdraw?
     isddraw:=mnuDirectdraw.enabled;

     if mnuDirectdraw.enabled then
      begin
       mnuDirectDraw.checked:=true;
       mnuDib.checked:=false;
      end else
      begin
       mnuDirectDraw.checked:=false;
       mnuDib.checked:=true;
      end;

     make_pix;
     SetPriority(priority2); // normal Priority

     if assigned(cart) then freemem(cart,cartsize);
     z80_reset;
     // initialize PerfFreq

    QueryPerformanceFrequency(li.quadpart);
    perfFreq:=li.LowPart div 32;// shr 5;
    SetAudioChannel(Self);
    enablesound;
end;

procedure TfrmGameboy.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if assigned(cart) then freemem(cart,cartsize);
 f_stopped:=false;
 application.terminate;
end;

procedure TFrmGameboy.RealSpeedEmulation;
var li: Large_Integer;
    time,curTime: dword;
    curTimer: DWord;
begin
 if AutoWait.Checked then
 begin
  if lastTime=0 then
   begin
    QueryPerformanceCounter(li.quadpart);
    lastTime:=li.LowPart;
   end;
   QueryPerformanceCounter(li.quadpart);
   curTime:=li.LowPart;
   time:=curTime-lastTime;
   if time<perfFreq then
    begin
     while time<perfFreq do
      begin
       Sleep(10);
       QueryPerformanceCounter(li.quadpart);
       curTime:=li.LowPart;
       time:=curTime-lastTime;
      end;
      inc(lastTime,perfFreq);
    end else lastTime:=curTime;
  end;
end;

procedure TFrmGameboy._RealSpeedEmulation;
begin
 if AutoWait.Checked then
  while timervar=0 do application.ProcessMessages;
 timervar:=0;
end;


procedure TfrmGameboy.Emulation(Var Msg:TMessage);
var count: DWord;
    i:Byte;
    wasteTime: Byte;
    idx: Integer;
begin
 idx := 0;
 wasteTime := 0;

 lastTime:=0;count:=0;i:=0;

 repeat
  // Pause
   if f_stopped then
    while f_stopped do application.ProcessMessages;

   z80_decode;

   if count=0 then application.processmessages;
   count:=(count+1) mod 10000;

   if screen_updated>0 then // onpaint :)
    begin
     screen_updated:=0;
     if i=0 then RealSpeedEmulation;
     i:=(i+1) mod 31;
    end;
 until application.Terminated;
end;

procedure TfrmGameboy.FileOpenClick(Sender: TObject);
var i:integer;
    s:String;
begin
 loadstat.enabled:=False;
 savestat.enabled:=false;

 opendialog.Filter:='All Roms [*.gb;*.gbc;*.rom]|*.gb;*.gbc;*.rom|Gameboy [*.gb]|*.gb|Gameboy Color [*.gbc]|*.gbc|All Files [*.*]|*.*';

  if opendialog.Execute then
  begin
    f_stopped:=true;
    load(opendialog.filename);
    i:=0;
    statusbar.Panels[2].text:=getcompany;
    statusbar.Panels[1].text:=speicher;
    statusbar.panels[0].text:=romname;
    z80_reset;z80_reset;
    f_stopped:=false;gb_speed:=1;
    setzewindow(handle);
    loadstat.enabled:=true;
    savestat.enabled:=true;
  end;
end;

procedure TfrmGameboy.KillClick(Sender: TObject);
var i:Byte;
begin
 for i:=1 to 4 do z80_Reset;
end;

procedure TfrmGameboy.FormActivate(Sender: TObject);
begin
 onActivate:=nil;
 repaint;
 with statusbar do
  begin
   Panels[2].text:=getcompany;
   Panels[1].text:=speicher;
   panels[0].text:=romname;
  end;
 z80_reset;z80_reset;
 f_stopped:=true;gb_speed:=1;
 setzewindow(handle);
 postmessage(handle,wm_user,0,0);
end;

procedure TfrmGameboy.Exit1Click(Sender: TObject);
begin
 close;
end;

procedure TfrmGameboy.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case key of
  ord('Y'): k_b:=0;
  ord('X'): k_a:=0;
  ord('A'): k_start:=0;
  ord('S'): k_select:=0;
  ord('P'): begin
             f_stopped:=not f_stopped;
             pause1.Checked:=f_stopped;
            end;
  VK_UP: k_up:=0;
  VK_DOWN: k_down:=0;
  VK_LEFT: k_left:=0;
  VK_RIGHT: k_right:=0;
 end;
end;

procedure TfrmGameboy.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case KEY of
  ord('Y'): k_b:=1;
  ord('X'): k_a:=1;
  ord('A'): k_start:=1;
  ord('B'): k_select:=1;
   VK_UP: k_up:=1;
   VK_DOWN: k_down:=1;
   VK_LEFT: k_left:=1;
   VK_RIGHT: k_right:=1;
  end;
end;

procedure TfrmGameboy.FrameClick(Sender: TObject);
begin
 n01.checked:=false;n11.checked:=false;
 n21.checked:=false;n31.checked:=false;
 n41.checked:=false;n51.checked:=false;

 frameskip:=tmenuitem(sender).tag+1;
 tmenuitem(sender).checked:=true;

 if not isddraw then UpdateDIB(Window) else
                     UpdateDDraw(window);
end;

procedure TfrmGameboy.Pause(Sender: TObject);
begin
 pause1.checked:=not pause1.checked;
 f_stopped:=pause1.Checked;
end;

procedure TfrmGameboy.ResetClick(Sender: TObject);
begin
 z80_reset;
end;

procedure TfrmGameboy.setPriority(Sender: TObject);
var
	ProcessID : DWORD;
	ProcessHandle : THandle;
	ThreadHandle : THandle;
        Priority: Integer;
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

 priority1.checked:=false;priority2.checked:=false;
 priority3.checked:=false;priority4.checked:=false;
 tmenuitem(sender).checked:=true;

 if sender=priority1 then priority:=IDLE_PRIORITY_CLASS;
 if sender=priority2 then priority:=NORMAL_PRIORITY_CLASS;
 if sender=priority3 then priority:=HIGH_PRIORITY_CLASS;
 if sender=priority4 then priority:=REALTIME_PRIORITY_CLASS;

 ProcessID := GetCurrentProcessID;
 ProcessHandle := OpenProcess(PROCESS_SET_INFORMATION,false,ProcessID);
 SetPriorityClass(ProcessHandle, Priority);
{ ThreadHandle := GetCurrentThread;
  SetThreadPriority(ThreadHandle, THREAD_PRIORITY_HIGHEST);}
end;

procedure TfrmGameboy.setResolution(Sender: TObject);
begin
 // Resolution
 width := 160;height := 144;
 if sender=x21 then begin width := 160*2;height := 144*2;end;
 if sender=x31 then begin width := 160*3;height := 144*3;end;
 if sender=x41 then begin width := 160*4;height := 144*4;end;
 width:=width+8;
 if not fullwindow then height:=height+statusbar.height+46;
end;

procedure TfrmGameboy.SetWindow;
begin
 fullwindow:=not fullwindow;
 if fullwindow then
  begin
   Menu:=nil;
   statusbar.Hide;
   Statbottom:=0;
   RemoveCaption;
  end else
  begin
   Menu:=mainmenu;
   statusbar.show;
   StatBottom:=statusbar.height;
   ShowCaption;
  end;
end;

procedure TfrmGameboy.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if button=mbRight then SetWindow else
 If (button = mbleft) and (fullwindow) then
 begin
  releasecapture;
  TWincontrol (Self).perform (WM_syscommand, $F012, 0);
 end;
end;

procedure TfrmGameboy.DebuggerClick(Sender: TObject);
begin
 //frmdebug.show;
 frmMapview.show;
 frmMapview.Update;
end;

procedure TfrmGameboy.FormResize(Sender: TObject);
var oldskip:Byte;
begin
 if f_stopped then exit;
 oldskip:=frameskip;
 frameskip:=1;
 if not isddraw then UpdateDIB(Handle) else UpdateDDraw(Handle);
 frameskip:=oldskip;
 statusbar.Panels[0].text:=inttostr(height)+':'+inttostr(width);
end;

procedure TfrmGameboy.AutoWaitClick(Sender: TObject);
begin
 AutoWait.checked:= not AutoWait.checked; 
end;

procedure TfrmGameboy.LoadStatClick(Sender: TObject);
begin
 openDialog.filter:='Savestate (*.gbs)|*.gbs';
 if opendialog.execute then
  case ReadGameState(opendialog.filename) of
   1: showmessage('This ROM does not use a RAM...');
   2: showmessage('File not found...');
   3: showmessage('Not a valid Savegame...');
  end;
end;

procedure TfrmGameboy.SaveStatClick(Sender: TObject);
begin
 saveDialog.filter:='Savestate (*.gbs)|*.gbs';
 if savedialog.execute then SaveGameState(savedialog.filename);
end;

procedure TfrmGameboy.SetSound(Sender: TObject);
begin
 off.checked:=not off.checked;
 if not off.checked then enablesound else disablesound;
end;

procedure TfrmGameboy.TimerTimer(Sender: TObject);
begin
 inc(timervar);
end;

procedure TfrmGameboy.SetAudioChannel(Sender: TObject);
begin
 if sender=ch1 then ch1.checked:=not ch1.checked else
 if sender=ch2 then ch2.checked:=not ch2.checked else
 if sender=ch3 then ch3.checked:=not ch3.checked else
 if sender=ch4 then ch4.checked:=not ch4.checked;

 snd[1].ChannelOFF:=not CH1.checked;
 snd[2].ChannelOFF:=not CH2.checked;
 snd[3].ChannelOFF:=not CH3.checked;
 snd[4].ChannelOFF:=not CH4.checked;
end;

procedure TfrmGameboy.mnuOutputClick(Sender: TObject);
begin
 isddraw:=(sender=mnuDirectDraw);
 mnuDirectdraw.checked:=isddraw;
 mnuDib.checked:=not isddraw;
end;

end.


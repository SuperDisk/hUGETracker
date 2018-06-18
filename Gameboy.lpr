program Gameboy;

{$MODE Delphi}

uses
  directdraw in 'DirectX\directdraw.pas',
  dxCommon in 'DirectX\dxCommon.pas',
  ddraw_out in 'GPU\DDraw_out.pas',
  dib_out in 'GPU\dib_out.pas',
  vars in 'Global\Vars.pas',
  gfx in 'GPU\gfx.pas',
  z80Cpu in 'CPU\z80cpu.pas',
  machine in 'CPU\Machine.pas',
  debugger in 'Debugger\Debugger.pas',
  sound in 'Sound\Sound.pas',
  mainloop,
  Messages,
  Windows,
  SysUtils;

const
  game: string = '';

{$R *.res}

  procedure displaydebugger(h_wnd: HWND);
  var
    hdc: HWND;
    s: string;
  begin
    hdc := GetDC(h_wnd);
    SetBkMode(hdc, TRANSPARENT);
    SetTextColor(hdc, rgb(255, 255, 255));

    disassemble(s, pc.w);
    s := 'PC=' + inttohex(pc.w, 2) + ' - ' + s + #0;
    textout(hdc, 0, 0, @s[1], length(s) - 1);
    s := 'BC=' + inttohex(bc.w, 2) + ',DE=' + inttohex(de.w, 2) + ',HL=' + inttohex(hl.w, 2);
    textout(hdc, 0, 15, @s[1], length(s) - 1);
  end;

  procedure ShowMessage(Msg: string);
  begin
    msg := msg + #0;
    MessageBox(HWND_DESKTOP, @msg[1], 'Gameboy-Emulator', MB_OK or
      MB_ICONEXCLAMATION or MB_SYSTEMMODAL);
  end;

var
  g_bactive: boolean;

  function WindowProc(h_Wnd: HWND; aMSG: cardinal; wParam: cardinal;
    lParam: integer): integer; stdcall;
  var
    r: trect;
  begin
    case aMSG of
      // Pause if minimized
      WM_ACTIVATE:
      begin
        if HIWORD(wParam) = 0 then
          g_bActive := True
        else
          g_bActive := False;
        Result := 0;
        Exit;
      end;

      // allow user to move the window :)
      WM_NCHITTEST: if (DefWindowProc(h_wnd, aMsg, wParam, lParam) = HTCLIENT)
        then
        begin
          Result := HTCAPTION;
          exit;
        end;
      WM_CREATE:
      begin
        start_dx(h_Wnd);
        make_pix;

        z80_reset;
        setzewindow(h_Wnd);
        th := CreateThread(nil, 0, @main_loop, nil, 0, tid);
        Sleep(100);
        GetClientRect(h_Wnd, R);
        DW := 320 - (R.right - R.left);
        DH := 288 - (R.bottom - R.top);
        MoveWindow(h_Wnd, 50, 50, 320 + DW, 288 + DH, True);
        if (cart <> nil) then
          freemem(cart, cartsize);

        z80_reset;
        if paramcount > 0 then
          game := ParamStr(1);
        load(game);
        //enablesound;
        z80_reset;
        z80_reset;
        f_stopped := False;
      end;

      // Clean up and close the app
      WM_DESTROY:
      begin
        f_stopped := True;
        sleep(100);
        ReleaseAllObjects;
        PostQuitMessage(0);
        Result := 0;
        Exit;
      end;
      // Handle any non-accelerated key commands
      WM_KEYDOWN:
      begin
        case wParam of
          VK_ESCAPE,
          VK_F12:
          begin
            PostMessage(h_Wnd, WM_CLOSE, 0, 0);
            Result := 0;
            Exit;
          end;
          Ord('Y'), Ord('Z'): k_b := 0;
          Ord('X'): k_a := 0;
          Ord('A'): k_start := 0;
          Ord('S'): k_select := 0;
          Ord('R'): z80_reset;
          Ord('P'): f_stopped := not f_stopped;

          VK_UP: k_up := 0;
          VK_DOWN: k_down := 0;
          VK_LEFT: k_left := 0;
          VK_RIGHT: k_right := 0;

          VK_TAB: displayDebugger(h_Wnd);
          VK_F1: ShowMessage('M.o.G.E  (C) 2000 Christian Hackbart'#13 +
              'www.tu-ilmenau.de/~hackbart'#13#13 +
              'Keys:'#13 +
              'R - Reset,     P - Pause,'#13 +
              'S - Select,    A - Start,'#13 +
              'X/Y - Button A/B');
        end;
      end;
      // Turn off the cursor since this is a full-screen app
      VK_ESCAPE: PostMessage(h_Wnd, WM_DESTROY, 0, 0);
      WM_KEYUP:
        case wparam of
          Ord('Y'), Ord('Z'): k_b := 1;
          Ord('X'): k_a := 1;
          Ord('A'): k_start := 1;
          Ord('S'): k_select := 1;
          VK_UP: k_up := 1;
          VK_DOWN: k_down := 1;
          VK_LEFT: k_left := 1;
          VK_RIGHT: k_right := 1;
        end;
    end;
    Result := DefWindowProc(h_Wnd, aMSG, wParam, lParam);
  end;

  function WinMain(in1: thandle; winm: integer): integer;
  var
    mess: msg;
    wndcl: wndclass;
    h_Wnd: Hwnd;
  begin
    Result := mess.wParam;

    // Set up and register window class
    wndcl.style := CS_HREDRAW or CS_VREDRAW;
    wndcl.hbrBackground := HBRUSH(COLOR_WINDOW + 1);
    wndcl.lpfnWndProc := @WindowProc;
    wndcl.cbClsExtra := 0;
    wndcl.cbWndExtra := 0;
    wndcl.hInstance := in1;
    wndcl.hIcon := 0; // LoadIcon(in1, 'MAINICON');
    wndcl.hCursor := LoadCursor(0, IDC_ARROW);
    wndcl.hbrBackground := GetStockObject(BLACK_BRUSH);
    wndcl.lpszMenuName := nil;
    wndcl.lpszClassName := 'Gameboy';
    RegisterClass(wndcl);

    // Create a window
    h_Wnd := CreateWindowEx(//WS_EX_CLIENTEDGE,
      WS_EX_TOPMOST, 'Gameboy',
      'Gameboy', WS_POPUP or
      WS_THICKFRAME or WS_SYSMENU or WS_MAXIMIZEBOX or WS_MINIMIZEBOX,
      //WS_SIZEBOX or WS_OVERLAPPEDWINDOW or WS_VISIBLE,
      50, 50, 160 * 2, 144 * 2, 0,
      0, in1, nil);
    ShowWindow(h_Wnd, winm);
    UpdateWindow(h_Wnd);
    while GetMessage(mess, 0, 0, 0) do
      DispatchMessage(mess);
    f_stopped := True;
    thr_f := 0;
    Sleep(100);
  end;


begin
  if paramcount = 0 then
  begin
    ShowMessage('Usage:' + ParamStr(0) + ' [romname]');
    halt;
  end;
  if winmain(GetModuleHandle(nil), SW_SHOW) <> 0 then
    Exit;
end.

(************************************************************
Author: Deepak Shenoy
        shenoy@agnisoft.com
Copyright (C) 2000 Agni Software Pvt. Ltd.
All Rights Reserved.
http://www.agnisoft.com

Change Log:
11.09.2000
  Now works for Windows ME too. Thanks to Petri Mustonen <weilo@hem.passagen.se>
  and Mark Carrington <mark@mutantpenguin.net> for their help and code.
06.03.2000
  Bug fix on suggestion from Jean-Fabien Connault <cycocrew@worldnet.fr> (Thanks)
  Save Dialog on Win9x/NT would show "Open" as the caption if no title was
  assigned. Added DoExecute Procedure.

22.01.2000
  IsWin2000 fix on suggestion from Jean-Fabien Connault <cycocrew@worldnet.fr>
  - Major Version check should be >= 5
********************************************************)

unit AgOpenDialog;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CommDlg;

type
  TAgOpenDialog = class(TOpenDialog)
  protected
    FShowPlacesBar : boolean; // shows the left bar
    FInterceptor : Pointer;   // pointer to intercepting function
    function IsWin2000 : boolean;  // are we on Windows2000?
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  published
    property ShowPlacesBar : boolean read FShowPlacesBar write FShowPlacesBar;
  end;

  TAgSaveDialog = class(TAgOpenDialog)
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  end;


procedure Register;

type
  TOpenFileNameEx = packed record
    lStructSize: DWORD;
    hWndOwner: HWND;
    hInstance: HINST;
    lpstrFilter: PAnsiChar;
    lpstrCustomFilter: PAnsiChar;
    nMaxCustFilter: DWORD;
    nFilterIndex: DWORD;
    lpstrFile: PAnsiChar;
    nMaxFile: DWORD;
    lpstrFileTitle: PAnsiChar;
    nMaxFileTitle: DWORD;
    lpstrInitialDir: PAnsiChar;
    lpstrTitle: PAnsiChar;
    Flags: DWORD;
    nFileOffset: Word;
    nFileExtension: Word;
    lpstrDefExt: PAnsiChar;
    lCustData: LPARAM;
    lpfnHook: function(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): UINT stdcall;
    lpTemplateName: PAnsiChar;
    pvReserved : Pointer;
    dwReserved : DWORD;
    FlagsEx : DWORD;
  end;

const OFN_EX_NOPLACESBAR = 1;

  function GetOpenFileNameEx(var OpenFile: TOpenFilenameEx): Bool; stdcall;
  function GetSaveFileNameEx(var OpenFile: TOpenFilenameEx): Bool; stdcall;
implementation

{$R *.res}

function GetOpenFileNameEx;      external 'comdlg32.dll'  name 'GetOpenFileNameA';
function GetSaveFileNameEx;      external 'comdlg32.dll'  name 'GetSaveFileNameA';

procedure Register;
begin
  RegisterComponents('Samples', [TAgOpenDialog, TAgSaveDialog]);
end;

var
  CurInstanceShowPlacesBar : boolean;

function OpenInterceptor(var DialogData: TOpenFileName): Bool; stdcall;
var DialogDataEx : TOpenFileNameEx;
begin
   Move(DialogData, DialogDataEx, sizeof(DialogData));
   if CurInstanceShowPlacesBar then
     DialogDataEx.FlagsEx := 0
   else
     DialogDataEx.FlagsEx := OFN_EX_NOPLACESBAR;
   DialogDataEx.lStructSize := sizeof(TOpenFileNameEx); // size of new structure
   Result := GetOpenFileNameEx( DialogDataEx );
end;

function SaveInterceptor(var DialogData: TOpenFileName): Bool; stdcall;
var DialogDataEx : TOpenFileNameEx;
begin
   Move(DialogData, DialogDataEx, sizeof(DialogData));
   if CurInstanceShowPlacesBar then
     DialogDataEx.FlagsEx := 0
   else
     DialogDataEx.FlagsEx := 1;
   DialogDataEx.lStructSize := sizeof(TOpenFileNameEx); // size of new structure
   Result := GetSaveFileNameEx( DialogDataEx );
end;

{ TAgOpenDialog }

constructor TAgOpenDialog.Create(AOwner: TComponent);
begin
  inherited;
  FShowPlacesBar := TRUE;
  FInterceptor := @OpenInterceptor;
end;

function TAgOpenDialog.Execute: Boolean;
begin
  if IsWin2000 then
  begin
     CurInstanceShowPlacesBar := FShowPlacesBar;
     Result := True; //DoExecute(FInterceptor);
  end
  else
    Result := inherited Execute;
end;

// CHANGED - 11 Sep 2000
// Function returns true on Windows ME too. Should change the name.
function TAgOpenDialog.IsWin2000: boolean;
var ver : TOSVersionInfo;
begin
  Result := FALSE;
  ver.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if not GetVersionEx(ver ) then
    Exit;

   if ( ver.dwPlatformId=VER_PLATFORM_WIN32_NT) then
   begin // Detect Windows 2000
      if (ver.dwMajorVersion >= 5 ) then
      Result := TRUE;
   end
   else // Detect Windows ME
   if ((ver.dwPlatformId=VER_PLATFORM_WIN32_WINDOWS) and
       (ver.dwMajorVersion >= 4) and
       (ver.dwMinorVersion >= 90)
      ) then
      Result := TRUE;
end;


{ TAgSaveDialog }

constructor TAgSaveDialog.Create(AOwner: TComponent);
begin
  inherited;
  FInterceptor := @SaveInterceptor;
end;

function TAgSaveDialog.Execute: Boolean;
begin
  if IsWin2000 then
  begin
     Result := Inherited Execute; // TAgOpenDialog has the functionality
  end
  else
    Result := True; //DoExecute(@GetSaveFileName); // can't call inherited because
                                           // it will call opendialogs DoExecute 
end;

end.

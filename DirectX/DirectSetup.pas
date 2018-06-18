(*==========================================================================
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dsetup.h
 *  Content:    DirectXSetup, error codes and flags
 *
 *  DirectX 7.0 Delphi adaptation by Erik Unger
 *
 *  Modyfied: 05-Oct-99
 *
 *  Download: http://www.delphi-jedi.org/DelphiGraphics/
 *  E-Mail: Erik.Unger@gmx.at
 *
 ***************************************************************************)

unit DirectSetup;

{$MODE Delphi}

{$MINENUMSIZE 4}
{$ALIGN ON}

interface

uses
  Windows, SysUtils,
  DXCommon;

var
  DSetupDLL : HModule;

type
  PDLSVersion = ^TDLSVersion;
  TDLSVersion = packed record
    dwVersionMS: DWORD;
    dwVersionLS: DWORD;
  end;


const
  FOURCC_VERS : array[0..3] of Char = ('v','e','r','s');

// DSETUP Error Codes, must remain compatible with previous setup.
  DSETUPERR_SUCCESS_RESTART     = HResult(1);
  DSETUPERR_SUCCESS             = HResult(0);
  DSETUPERR_BADWINDOWSVERSION   = HResult(-1);
  DSETUPERR_SOURCEFILENOTFOUND  = HResult(-2);
  DSETUPERR_BADSOURCESIZE       = HResult(-3);
  DSETUPERR_BADSOURCETIME       = HResult(-4);
  DSETUPERR_NOCOPY              = HResult(-5);
  DSETUPERR_OUTOFDISKSPACE      = HResult(-6);
  DSETUPERR_CANTFINDINF         = HResult(-7);
  DSETUPERR_CANTFINDDIR         = HResult(-8);
  DSETUPERR_INTERNAL            = HResult(-9);
  DSETUPERR_NTWITHNO3D          = HResult(-10);  // REM: obsolete, you'll never see this
  DSETUPERR_UNKNOWNOS           = HResult(-11);
  DSETUPERR_USERHITCANCEL       = HResult(-12);
  DSETUPERR_NOTPREINSTALLEDONNT = HResult(-13);
  DSETUPERR_NEWERVERSION	= HResult(-14);  

// DSETUP flags. DirectX 5.0 apps should use these flags only.
  DSETUP_DDRAWDRV     = $00000008;   (* install DirectDraw Drivers           *)
  DSETUP_DSOUNDDRV    = $00000010;   (* install DirectSound Drivers          *)
  DSETUP_DXCORE       = $00010000;   (* install DirectX runtime              *)
  DSETUP_DIRECTX = DSETUP_DXCORE or DSETUP_DDRAWDRV or DSETUP_DSOUNDDRV;
  DSETUP_TESTINSTALL  = $00020000;   (* just test install, don't do anything *)
  DSETUP_USEROLDERFLAG= $02000000;   (* enable return DSETUPERR_NEWERVERSION *)
// Bug #22730
  DSETUP_NTINSTALL		= $00080000;   (* install on Win2K platform *)

// These OBSOLETE flags are here for compatibility with pre-DX5 apps only.
// They are present to allow DX3 apps to be recompiled with DX5 and still work.
// DO NOT USE THEM for DX5. They will go away in future DX releases.
  DSETUP_DDRAW         = $00000001; (* OBSOLETE. install DirectDraw           *)
  DSETUP_DSOUND        = $00000002; (* OBSOLETE. install DirectSound          *)
  DSETUP_DPLAY         = $00000004; (* OBSOLETE. install DirectPlay           *)
  DSETUP_DPLAYSP       = $00000020; (* OBSOLETE. install DirectPlay Providers *)
  DSETUP_DVIDEO        = $00000040; (* OBSOLETE. install DirectVideo          *)
  DSETUP_D3D           = $00000200; (* OBSOLETE. install Direct3D             *)
  DSETUP_DINPUT        = $00000800; (* OBSOLETE. install DirectInput          *)
  DSETUP_DIRECTXSETUP  = $00001000; (* OBSOLETE. install DirectXSetup DLL's   *)
  DSETUP_NOUI          = $00002000; (* OBSOLETE. install DirectX with NO UI   *)
  DSETUP_PROMPTFORDRIVERS = $10000000; (* OBSOLETE. prompt when replacing display/audio drivers *)
  DSETUP_RESTOREDRIVERS = $20000000;(* OBSOLETE. restore display/audio drivers *)

//******************************************************************
// DirectX Setup Callback mechanism
//******************************************************************

// DSETUP Message Info Codes, passed to callback as Reason parameter.
  DSETUP_CB_MSG_NOMESSAGE                 = 0;
  DSETUP_CB_MSG_CANTINSTALL_UNKNOWNOS     = 1;
  DSETUP_CB_MSG_CANTINSTALL_NT            = 2;
  DSETUP_CB_MSG_CANTINSTALL_BETA          = 3;
  DSETUP_CB_MSG_CANTINSTALL_NOTWIN32      = 4;
  DSETUP_CB_MSG_CANTINSTALL_WRONGLANGUAGE = 5;
  DSETUP_CB_MSG_CANTINSTALL_WRONGPLATFORM = 6;
  DSETUP_CB_MSG_PREINSTALL_NT             = 7;
  DSETUP_CB_MSG_NOTPREINSTALLEDONNT       = 8;
  DSETUP_CB_MSG_SETUP_INIT_FAILED         = 9;
  DSETUP_CB_MSG_INTERNAL_ERROR            = 10;
  DSETUP_CB_MSG_CHECK_DRIVER_UPGRADE      = 11;
  DSETUP_CB_MSG_OUTOFDISKSPACE            = 12;
  DSETUP_CB_MSG_BEGIN_INSTALL             = 13;
  DSETUP_CB_MSG_BEGIN_INSTALL_RUNTIME     = 14;
  DSETUP_CB_MSG_BEGIN_INSTALL_DRIVERS     = 15;
  DSETUP_CB_MSG_BEGIN_RESTORE_DRIVERS     = 16;
  DSETUP_CB_MSG_FILECOPYERROR             = 17;


  DSETUP_CB_UPGRADE_TYPE_MASK      = $000F;
  DSETUP_CB_UPGRADE_KEEP           = $0001;
  DSETUP_CB_UPGRADE_SAFE           = $0002;
  DSETUP_CB_UPGRADE_FORCE          = $0004;
  DSETUP_CB_UPGRADE_UNKNOWN        = $0008;

  DSETUP_CB_UPGRADE_HASWARNINGS    = $0100;
  DSETUP_CB_UPGRADE_CANTBACKUP     = $0200;

  DSETUP_CB_UPGRADE_DEVICE_ACTIVE  = $0800;

  DSETUP_CB_UPGRADE_DEVICE_DISPLAY = $1000;
  DSETUP_CB_UPGRADE_DEVICE_MEDIA   = $2000;


type
  PDSetup_CB_UpgradeInfo = ^TDSetup_CB_UpgradeInfo;
  TDSetup_CB_UpgradeInfo = record
    UpgradeFlags: DWORD;
  end;

  PDSetup_CB_FileCopyError = ^TDSetup_CB_FileCopyError;
  TDSetup_CB_FileCopyError = record
    dwError: DWORD;
  end;

//
// Data Structures
//
  PDirectXRegisterAppA = ^TDirectXRegisterAppA;
  TDirectXRegisterAppA = record
    dwSize: DWORD;
    dwFlags: DWORD;
    lpszApplicationName: PAnsiChar;
    lpGUID: PGUID;
    lpszFilename: PAnsiChar;
    lpszCommandLine: PAnsiChar;
    lpszPath: PAnsiChar;
    lpszCurrentDirectory: PAnsiChar;
  end;

  PDirectXRegisterApp2A = ^TDirectXRegisterApp2A;
  TDirectXRegisterApp2A = record
    dwSize: DWORD;
    dwFlags: DWORD;
    lpszApplicationName: PAnsiChar;
    lpGUID: PGUID;
    lpszFilename: PAnsiChar;
    lpszCommandLine: PAnsiChar;
    lpszPath: PAnsiChar;
    lpszCurrentDirectory: PAnsiChar;
    lpszLauncherName: PAnsiChar;
  end;

  PDirectXRegisterAppW = ^TDirectXRegisterAppW;
  TDirectXRegisterAppW = record
    dwSize: DWORD;
    dwFlags: DWORD;
    lpszApplicationName: PWideChar;
    lpGUID: PGUID;
    lpszFilename: PWideChar;
    lpszCommandLine: PWideChar;
    lpszPath: PWideChar;
    lpszCurrentDirectory: PWideChar;
  end;

  PDirectXRegisterApp2W = ^TDirectXRegisterApp2W;
  TDirectXRegisterApp2W = record
    dwSize: DWORD;
    dwFlags: DWORD;
    lpszApplicationName: PWideChar;
    lpGUID: PGUID;
    lpszFilename: PWideChar;
    lpszCommandLine: PWideChar;
    lpszPath: PWideChar;
    lpszCurrentDirectory: PWideChar;
    lpszLauncherName: PWideChar;
  end;

  PDirectXRegisterApp = ^TDirectXRegisterApp;
  PDirectXRegisterApp2 = ^TDirectXRegisterApp2;
{$IFDEF UNICODE}
  TDirectXRegisterApp = TDirectXRegisterAppW;
  TDirectXRegisterApp2 = TDirectXRegisterAppW2;
{$ELSE}
  TDirectXRegisterApp = TDirectXRegisterAppA;
  TDirectXRegisterApp2 = TDirectXRegisterApp2A;
{$ENDIF}

//
// API
//
var
  DirectXSetupW : function (hWnd: HWND; lpszRootPath: PWideChar; dwFlags: DWORD) : Integer; stdcall;
  DirectXSetupA : function (hWnd: HWND; lpszRootPath: PAnsiChar; dwFlags: DWORD) : Integer; stdcall;
  DirectXSetup : function (hWnd: HWND; lpszRootPath: PCharAW; dwFlags: DWORD) : Integer; stdcall;

  DirectXDeviceDriverSetupW : function (hWnd: HWND; lpszDriverClass: PWideChar;
     lpszDriverPath: PWideChar; dwFlags: DWORD) : Integer; stdcall;
  DirectXDeviceDriverSetupA : function (hWnd: HWND; lpszDriverClass: PAnsiChar;
     lpszDriverPath: PAnsiChar; dwFlags: DWORD) : Integer; stdcall;
  DirectXDeviceDriverSetup : function (hWnd: HWND; lpszDriverClass: PCharAW;
     lpszDriverPath: PCharAW; dwFlags: DWORD) : Integer; stdcall;

  DirectXRegisterApplicationW : function
     (hWnd: HWND; const lpDXRegApp: TDirectXRegisterAppW) : Integer; stdcall;
  DirectXRegisterApplicationA : function
     (hWnd: HWND; const lpDXRegApp: TDirectXRegisterAppA) : Integer; stdcall;
  DirectXRegisterApplication : function
     (hWnd: HWND; const lpDXRegApp: TDirectXRegisterApp) : Integer; stdcall;

  DirectXUnRegisterApplication : function
     (hWnd: HWND; const lpGUID: TGUID) : Integer; stdcall;

type
  TDSetup_Callback = function (Reason: DWORD; MsgType: DWORD; // Same as flags to MessageBox
      szMessage: PChar; szName: PChar; pInfo: Pointer) : DWORD; stdcall;

var
  DirectXSetupSetCallback : function (Callback: TDSetup_Callback) : Integer; stdcall;

  DirectXSetupGetVersion : function (out lpdwVersion, lpdwMinorVersion: DWORD) : Integer; stdcall;

implementation

(*==========================================================================
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dsetup.h
 *  Content:    DirectXSetup, error codes and flags
 ***************************************************************************)

procedure LoadDSetup;

  function RegGetStringValue(Hive: HKEY; const KeyName, ValueName: string): string;
  var EnvKey  : HKEY;
      Buf     : array[0..255] of char;
      BufSize : DWord;
      RegType : DWord;
      rc      : DWord;
  begin
    Result := '';
    BufSize := Sizeof(Buf);
    ZeroMemory(@Buf, BufSize);
    RegType := REG_SZ;
    try
      if (RegOpenKeyEx(Hive, PChar(KeyName), 0, KEY_READ, EnvKey) = ERROR_SUCCESS) then
      begin
        try
          if (ValueName = '') then rc := RegQueryValueEx(EnvKey, nil, nil, @RegType, @Buf, @BufSize)
            else rc := RegQueryValueEx(EnvKey, PChar(ValueName), nil, @RegType, @Buf, @BufSize);
          if rc = ERROR_SUCCESS then Result := string(Buf);
        finally
          RegCloseKey(EnvKey);
        end;
      end;
    finally
      RegCloseKey(Hive);
    end;
  end;


  function ExistFile(const FileName: string): Boolean;
  var hFile: THandle;
  begin
    hFile := CreateFile(PChar(FileName), 0, 0, nil, OPEN_EXISTING, 0, 0);
    Result := hFile <> INVALID_HANDLE_VALUE;
    if hFile = INVALID_HANDLE_VALUE then FileClose(hFile); { *Converted from CloseHandle* }
  end;

  function GetDSetupDLLPath : string;
  begin
     Result := RegGetStringValue(HKEY_LOCAL_MACHINE,
                                 'Software\Microsoft\Windows\CurrentVersion\Uninstall\DirectXDrivers',
                                 'UninstallString');
     if Result <> '' then
       Result := Copy(Result,1,Length(Result)-Length('dxsetup.exe')) + 'DSetup.dll';
  end;

begin
  DSetupDLL := LoadLibrary(PChar(GetDSetupDLLPath));

  DirectXSetupA := GetProcAddress(DSetupDLL,'DirectXSetupA');
  DirectXSetupW := GetProcAddress(DSetupDLL,'DirectXSetupW');
{$IFDEF UNICODE}
  DirectXSetup := DirectXSetupW;
{$ELSE}
  DirectXSetup := DirectXSetupA;
{$ENDIF}

  DirectXDeviceDriverSetupA :=
      GetProcAddress(DSetupDLL,'DirectXDeviceDriverSetupA');
  DirectXDeviceDriverSetupW :=
      GetProcAddress(DSetupDLL,'DirectXDeviceDriverSetupW');
{$IFDEF UNICODE}
  DirectXDeviceDriverSetup := DirectXDeviceDriverSetupW;
{$ELSE}
  DirectXDeviceDriverSetup := DirectXDeviceDriverSetupA;
{$ENDIF}

  DirectXRegisterApplicationA :=
       GetProcAddress(DSetupDLL,'DirectXRegisterApplicationA');
  DirectXRegisterApplicationW :=
       GetProcAddress(DSetupDLL,'DirectXRegisterApplicationW');
{$IFDEF UNICODE}
  DirectXRegisterApplication := DirectXRegisterApplicationW;
{$ELSE}
  DirectXRegisterApplication := DirectXRegisterApplicationA;
{$ENDIF}

  DirectXUnRegisterApplication :=
      GetProcAddress(DSetupDLL,'DirectXUnRegisterApplication');

  DirectXSetupSetCallback :=
      GetProcAddress(DSetupDLL,'DirectXSetupSetCallback');

  DirectXSetupGetVersion := GetProcAddress(DSetupDLL,'DirectXSetupGetVersion');

end;

initialization
begin
  if not IsNTandDelphiRunning then
  begin
    LoadDSetup;
  end;
end;

finalization
begin
  FreeLibrary(DSetupDLL);
end;

end.
 
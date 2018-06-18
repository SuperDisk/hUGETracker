unit DXCommon;

{$MODE Delphi}

interface

uses
  Windows, SysUtils;

type
{$IFDEF UNICODE}
  PCharAW = PWideChar;
{$ELSE}
  PCharAW = PAnsiChar;
{$ENDIF}
  
function IsNTandDelphiRunning : boolean;
function RegGetStringValue(Hive: HKEY; const KeyName, ValueName: string): string;
function ExistFile(const FileName: string): Boolean;

implementation

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
  if Result = true then FileClose(hFile); { *Converted from CloseHandle* }
end;


function IsNTandDelphiRunning : boolean;
var
  OSVersion  : TOSVersionInfo;
  ProgName   : array[0..255] of char;
begin
  OSVersion.dwOsVersionInfoSize := sizeof(OSVersion);
  GetVersionEx(OSVersion);
  ProgName[0] := #0;
  lstrcat(ProgName, PChar(ParamStr(0)));
  CharLowerBuff(ProgName, SizeOf(ProgName));
  // Not running in NT or program is not Delphi itself ?
  result := ( (OSVersion.dwPlatformID = VER_PLATFORM_WIN32_NT) and
              (Pos('delphi32.exe', string(ProgName)) > 0) );
end;


end.
 
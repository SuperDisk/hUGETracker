(*==========================================================================;
 *
 *  Copyright (C) 1996-1999 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dinput.h
 *  Content:    DirectInput include file
 *
 *  DirectX 7.0 Delphi adaptation by Erik Unger, input format
 *  variable structure & other fixups by Arne Schäpers (as)
 *
 *  Modified: 04-Jan-2000
 *
 *  Download: http://www.delphi-jedi.org/DelphiGraphics/
 *  E-Mail: Erik.Unger@gmx.at
 *          a.schaepers@digitalpublishing.de            
 *
 ***************************************************************************)

{ Some notes from as:
  1. DirectInput Enum callback functions which are documented with result
  type BOOL in the SDK had to be changed to result type integer because the debug
  version of DINPUT.DLL (which is the same for SDK versions 5.0, 5.2, 6.0, and 6.1)
  explicitely checks for two possible return values:

  0 - FALSE in C and in Delphi
  1 - TRUE in C, defined as DIENUM_CONTINUE

  In Delphi, TRUE means $FFFFFFFF (= -1) for the LongBool (= BOOL) data
  type, and AL = 1 (upper three bytes undefined) for the Boolean data type.
  The debug version of DINPUT.DLL will throw an external exception
  ("invalid return value for callback") when fed with either value.

  This change affects the following methods:
  EnumDevices, EnumObjects, EnumEffects, EnumCreatedEffectObjects

  2. Windows 98 and DX6 debug versions DInput and DSound

  Under Windows 98, the "debug" setup of the DirectX SDK 6.x skips DInput.DLL
  and DSound.DLL, i.e. makes you end up with the retail version of these two
  files without any notice.
  The debug versions of DInput.DLL and DSound.DLL can be found in the
  \extras\Win98\Win98Dbg folder of the SDK CD; they need to be installed
  "manually".

  This problem has been fixed with DX7 where the SDK installs the debug versions
  of DInput and DSound without any "manual" help.

}

unit DirectInput;

{$MODE Delphi}

{$MINENUMSIZE 4}
{$ALIGN ON}
                                                
interface

uses
  Windows,
  MMSystem;

var
  DInputDLL : HMODULE;

{$IFDEF DIRECTX3}
const DIRECTINPUT_VERSION = $0300;
{$ELSE}
const DIRECTINPUT_VERSION = $0700;
{$ENDIF}

function DIErrorString(Value: HResult) : string;

type
  TRefGUID = packed record
    case integer of
    1: (guid : PGUID);
    2: (dwFlags : DWORD);
  end;

(****************************************************************************
 *
 *      Class IDs
 *
 ****************************************************************************)
const
  CLSID_DirectInput: TGUID =
      (D1:$25E609E0;D2:$B259;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  CLSID_DirectInputDevice: TGUID =
      (D1:$25E609E1;D2:$B259;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));

(****************************************************************************
 *
 *      Predefined object types
 *
 ****************************************************************************)

  GUID_XAxis: TGUID =
      (D1:$A36D02E0;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_YAxis: TGUID =
      (D1:$A36D02E1;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_ZAxis: TGUID =
      (D1:$A36D02E2;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_RxAxis: TGUID =
      (D1:$A36D02F4;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_RyAxis: TGUID =
      (D1:$A36D02F5;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_RzAxis: TGUID =
      (D1:$A36D02E3;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_Slider: TGUID =
      (D1:$A36D02E4;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));

  GUID_Button: TGUID =
      (D1:$A36D02F0;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_Key: TGUID =
      (D1:$55728220;D2:$D33C;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));

  GUID_POV: TGUID =
      (D1:$A36D02F2;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));

  GUID_Unknown: TGUID =
      (D1:$A36D02F3;D2:$C9F3;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));

(****************************************************************************
 *
 *      Predefined product GUIDs
 *
 ****************************************************************************)

  GUID_SysMouse: TGUID =
      (D1:$6F1D2B60;D2:$D5A0;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_SysKeyboard: TGUID =
      (D1:$6F1D2B61;D2:$D5A0;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_Joystick: TGUID =
      (D1:$6F1D2B70;D2:$D5A0;D3:$11CF;D4:($BF,$C7,$44,$45,$53,$54,$00,$00));
  GUID_SysMouseEm: TGUID = '{6F1D2B80-D5A0-11CF-BFC7-444553540000}';
  GUID_SysMouseEm2: TGUID = '{6F1D2B81-D5A0-11CF-BFC7-444553540000}';
  GUID_SysKeyboardEm: TGUID = '{6F1D2B82-D5A0-11CF-BFC7-444553540000}';
  GUID_SysKeyboardEm2: TGUID = '{6F1D2B83-D5A0-11CF-BFC7-444553540000}';

(****************************************************************************
 *
 *      Predefined force feedback effects
 *
 ****************************************************************************)

  GUID_ConstantForce: TGUID =
      (D1:$13541C20;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_RampForce: TGUID =
      (D1:$13541C21;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_Square: TGUID =
      (D1:$13541C22;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_Sine: TGUID =
      (D1:$13541C23;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_Triangle: TGUID =
      (D1:$13541C24;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_SawtoothUp: TGUID =
      (D1:$13541C25;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_SawtoothDown: TGUID =
      (D1:$13541C26;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_Spring: TGUID =
      (D1:$13541C27;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_Damper: TGUID =
      (D1:$13541C28;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_Inertia: TGUID =
      (D1:$13541C29;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_Friction: TGUID =
      (D1:$13541C2A;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));
  GUID_CustomForce: TGUID =
      (D1:$13541C2B;D2:$8E33;D3:$11D0;D4:($9A,$D0,$00,$A0,$C9,$A0,$6E,$35));



(****************************************************************************
 *
 *      Interfaces and Structures...
 *
 ****************************************************************************)

(****************************************************************************
 *
 *      IDirectInputEffect
 *
 ****************************************************************************)

const
  DIEFT_ALL                   = $00000000;

  DIEFT_CONSTANTFORCE         = $00000001;
  DIEFT_RAMPFORCE             = $00000002;
  DIEFT_PERIODIC              = $00000003;
  DIEFT_CONDITION             = $00000004;
  DIEFT_CUSTOMFORCE           = $00000005;
  DIEFT_HARDWARE              = $000000FF;

  DIEFT_FFATTACK              = $00000200;
  DIEFT_FFFADE                = $00000400;
  DIEFT_SATURATION            = $00000800;
  DIEFT_POSNEGCOEFFICIENTS    = $00001000;
  DIEFT_POSNEGSATURATION      = $00002000;
  DIEFT_DEADBAND              = $00004000;
  DIEFT_STARTDELAY            = $00008000;

function DIEFT_GETTYPE(n: variant) : byte;

const
  DI_DEGREES                  =     100;
  DI_FFNOMINALMAX             =   10000;
  DI_SECONDS                  = 1000000;

type
  PDIConstantForce = ^TDIConstantForce;
  TDIConstantForce = packed record
    lMagnitude : LongInt;
  end;

  PDIRampForce = ^TDIRampForce;
  TDIRampForce = packed record
    lStart : LongInt;
    lEnd : LongInt;
  end;

  PDIPeriodic = ^TDIPeriodic;
  TDIPeriodic = packed record
    dwMagnitude : DWORD;
    lOffset : LongInt;
    dwPhase : DWORD;
    dwPeriod : DWORD;
  end;

  PDICondition = ^TDICondition;
  TDICondition = packed record
    lOffset : LongInt;
    lPositiveCoefficient : LongInt;
    lNegativeCoefficient : LongInt;
    dwPositiveSaturation : DWORD;
    dwNegativeSaturation : DWORD;
    lDeadBand : LongInt;
  end;

  PDICustomForce = ^TDICustomForce;
  TDICustomForce = packed record
    cChannels : DWORD;
    dwSamplePeriod : DWORD;
    cSamples : DWORD;
    rglForceData : PLongInt;
  end;

  PDIEnvelope = ^TDIEnvelope;
  TDIEnvelope = packed record
    dwSize : DWORD;                   (* sizeof(DIENVELOPE)   *)
    dwAttackLevel : DWORD;
    dwAttackTime : DWORD;             (* Microseconds         *)
    dwFadeLevel : DWORD;
    dwFadeTime : DWORD;               (* Microseconds         *)
  end;

  PDIEffect_DX5 = ^TDIEffect_DX5;
  TDIEffect_DX5 = packed record
    dwSize : DWORD;                   (* sizeof(DIEFFECT)     *)
    dwFlags : DWORD;                  (* DIEFF_*              *)
    dwDuration : DWORD;               (* Microseconds         *)
    dwSamplePeriod : DWORD;           (* Microseconds         *)
    dwGain : DWORD;
    dwTriggerButton : DWORD;          (* or DIEB_NOTRIGGER    *)
    dwTriggerRepeatInterval : DWORD;  (* Microseconds         *)
    cAxes : DWORD;                    (* Number of axes       *)
    rgdwAxes : PDWORD;                (* Array of axes        *)
    rglDirection : PLongInt;          (* Array of directions  *)
    lpEnvelope : PDIEnvelope;         (* Optional             *)
    cbTypeSpecificParams : DWORD;     (* Size of params       *)
    lpvTypeSpecificParams : pointer;  (* Pointer to params    *)
  end;

  PDIEffect_DX6 = ^TDIEffect_DX6;
  TDIEffect_DX6 = packed record
    dwSize : DWORD;                   (* sizeof(DIEFFECT)     *)
    dwFlags : DWORD;                  (* DIEFF_*              *)
    dwDuration : DWORD;               (* Microseconds         *)
    dwSamplePeriod : DWORD;           (* Microseconds         *)
    dwGain : DWORD;
    dwTriggerButton : DWORD;          (* or DIEB_NOTRIGGER    *)
    dwTriggerRepeatInterval : DWORD;  (* Microseconds         *)
    cAxes : DWORD;                    (* Number of axes       *)
    rgdwAxes : PDWORD;                (* Array of axes        *)
    rglDirection : PLongInt;          (* Array of directions  *)
    lpEnvelope : PDIEnvelope;         (* Optional             *)
    cbTypeSpecificParams : DWORD;     (* Size of params       *)
    lpvTypeSpecificParams : pointer;  (* Pointer to params    *)
    dwStartDelay: DWORD;              (* Microseconds         *)
  end;

  PDIEffect = ^TDIEffect;
{$IFDEF DIRECTX5}
  TDIEffect = TDIEffect_DX5;
{$ELSE}
  TDIEffect = TDIEffect_DX6;
{$ENDIF}

  PDIFileEffect = ^TDIFileEffect;
  TDIFileEffect = packed record
    dwSize : DWORD;
    GuidEffect: TGUID;
    lpDiEffect: PDIEffect;
    szFriendlyName : array [0..MAX_PATH-1] of AnsiChar;
  end;

const
  DIEFF_OBJECTIDS             = $00000001;
  DIEFF_OBJECTOFFSETS         = $00000002;
  DIEFF_CARTESIAN             = $00000010;
  DIEFF_POLAR                 = $00000020;
  DIEFF_SPHERICAL             = $00000040;

  DIEP_DURATION               = $00000001;
  DIEP_SAMPLEPERIOD           = $00000002;
  DIEP_GAIN                   = $00000004;
  DIEP_TRIGGERBUTTON          = $00000008;
  DIEP_TRIGGERREPEATINTERVAL  = $00000010;
  DIEP_AXES                   = $00000020;
  DIEP_DIRECTION              = $00000040;
  DIEP_ENVELOPE               = $00000080;
  DIEP_TYPESPECIFICPARAMS     = $00000100;
{$IFDEF DIRECTX5}
  DIEP_ALLPARAMS              = $000001FF;
{$ELSE}
  DIEP_STARTDELAY             = $00000200;
  DIEP_ALLPARAMS_DX5          = $000001FF;
  DIEP_ALLPARAMS              = $000003FF;
{$ENDIF}
  DIEP_START                  = $20000000;
  DIEP_NORESTART              = $40000000;
  DIEP_NODOWNLOAD             = $80000000;
  DIEB_NOTRIGGER              = $FFFFFFFF;

  DIES_SOLO                   = $00000001;
  DIES_NODOWNLOAD             = $80000000;

  DIEGES_PLAYING              = $00000001;
  DIEGES_EMULATED             = $00000002;


type
  PDIEffEscape = ^TDIEffEscape;
  TDIEffEscape = packed record
    dwSize : DWORD;
    dwCommand : DWORD;
    lpvInBuffer : pointer;
    cbInBuffer : DWORD;
    lpvOutBuffer : pointer;
    cbOutBuffer : DWORD;
  end;


//
// IDirectSoundCapture  // as: ???
//
  IDirectInputEffect = interface (IUnknown)
    ['{E7E1F7C0-88D2-11D0-9AD0-00A0C9A06E35}']
    (** IDirectInputEffect methods ***)
    function Initialize(hinst: THandle; dwVersion: DWORD;
        const rguid: TGUID) : HResult;  stdcall;
    function GetEffectGuid(var pguid: TGUID) : HResult;  stdcall;
    function GetParameters(var peff: TDIEffect; dwFlags: DWORD) : HResult;  stdcall;
    function SetParameters(var peff: TDIEffect; dwFlags: DWORD) : HResult;  stdcall;
    function Start(dwIterations: DWORD; dwFlags: DWORD) : HResult;  stdcall;
    function Stop : HResult;  stdcall;
    function GetEffectStatus(var pdwFlags : DWORD) : HResult;  stdcall;
    function Download : HResult;  stdcall;
    function Unload : HResult;  stdcall;
    function Escape(var pesc: TDIEffEscape) : HResult;  stdcall;
  end;

(****************************************************************************
 *
 *      IDirectInputDevice
 *
 ****************************************************************************)

const
  DIDEVTYPE_DEVICE = 1;
  DIDEVTYPE_MOUSE = 2;
  DIDEVTYPE_KEYBOARD = 3;
  DIDEVTYPE_JOYSTICK = 4;
  DIDEVTYPE_HID = $00010000;

  DIDEVTYPEMOUSE_UNKNOWN = 1;
  DIDEVTYPEMOUSE_TRADITIONAL = 2;
  DIDEVTYPEMOUSE_FINGERSTICK = 3;
  DIDEVTYPEMOUSE_TOUCHPAD = 4;
  DIDEVTYPEMOUSE_TRACKBALL = 5;

  DIDEVTYPEKEYBOARD_UNKNOWN = 0;
  DIDEVTYPEKEYBOARD_PCXT = 1;
  DIDEVTYPEKEYBOARD_OLIVETTI = 2;
  DIDEVTYPEKEYBOARD_PCAT = 3;
  DIDEVTYPEKEYBOARD_PCENH = 4;
  DIDEVTYPEKEYBOARD_NOKIA1050 = 5;
  DIDEVTYPEKEYBOARD_NOKIA9140 = 6;
  DIDEVTYPEKEYBOARD_NEC98 = 7;
  DIDEVTYPEKEYBOARD_NEC98LAPTOP = 8;
  DIDEVTYPEKEYBOARD_NEC98106 = 9;
  DIDEVTYPEKEYBOARD_JAPAN106 = 10;
  DIDEVTYPEKEYBOARD_JAPANAX = 11;
  DIDEVTYPEKEYBOARD_J3100 = 12;

  DIDEVTYPEJOYSTICK_UNKNOWN = 1;
  DIDEVTYPEJOYSTICK_TRADITIONAL = 2;
  DIDEVTYPEJOYSTICK_FLIGHTSTICK = 3;
  DIDEVTYPEJOYSTICK_GAMEPAD = 4;
  DIDEVTYPEJOYSTICK_RUDDER = 5;
  DIDEVTYPEJOYSTICK_WHEEL = 6;
  DIDEVTYPEJOYSTICK_HEADTRACKER = 7;

function GET_DIDEVICE_TYPE(dwDevType: variant) : byte;
function GET_DIDEVICE_SUBTYPE(dwDevType: variant) : byte;

type
  PDIDevCaps_DX3 = ^TDIDevCaps_DX3;
  TDIDevCaps_DX3 = packed record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwDevType: DWORD;
    dwAxes: DWORD;
    dwButtons: DWORD;
    dwPOVs: DWORD;
  end;

  PDIDevCaps_DX5 = ^TDIDevCaps_DX5;
  TDIDevCaps_DX5 = packed record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwDevType: DWORD;
    dwAxes: DWORD;
    dwButtons: DWORD;
    dwPOVs: DWORD;
    dwFFSamplePeriod: DWORD;
    dwFFMinTimeResolution: DWORD;
    dwFirmwareRevision: DWORD;
    dwHardwareRevision: DWORD;
    dwFFDriverVersion: DWORD;
  end;

  PDIDevCaps = ^TDIDevCaps;
{$IFDEF DIRECTX3}
  TDIDevCaps = TDIDevCaps_DX3;
{$ELSE}
  TDIDevCaps = TDIDevCaps_DX5;
{$ENDIF}

const
  DIDC_ATTACHED           = $00000001;
  DIDC_POLLEDDEVICE       = $00000002;
  DIDC_EMULATED           = $00000004;
  DIDC_POLLEDDATAFORMAT   = $00000008;
  DIDC_FORCEFEEDBACK      = $00000100;
  DIDC_FFATTACK           = $00000200;
  DIDC_FFFADE             = $00000400;
  DIDC_SATURATION         = $00000800;
  DIDC_POSNEGCOEFFICIENTS = $00001000;
  DIDC_POSNEGSATURATION   = $00002000;
  DIDC_DEADBAND           = $00004000;
  DIDC_STARTDELAY         = $00008000;
  DIDC_ALIAS              = $00010000;
  DIDC_PHANTOM            = $00020000;

  DIDFT_ALL = $00000000;

  DIDFT_RELAXIS = $00000001;
  DIDFT_ABSAXIS = $00000002;
  DIDFT_AXIS    = $00000003;

  DIDFT_PSHBUTTON = $00000004;
  DIDFT_TGLBUTTON = $00000008;
  DIDFT_BUTTON    = $0000000C;

  DIDFT_POV        = $00000010;
  DIDFT_COLLECTION = $00000040;
  DIDFT_NODATA     = $00000080;

  DIDFT_ANYINSTANCE = $00FFFF00;
  DIDFT_INSTANCEMASK = DIDFT_ANYINSTANCE;
function DIDFT_MAKEINSTANCE(n: variant) : DWORD;
function DIDFT_GETTYPE(n: variant) : byte;
function DIDFT_GETINSTANCE(n: variant) : DWORD;
const
  DIDFT_FFACTUATOR        = $01000000;
  DIDFT_FFEFFECTTRIGGER   = $02000000;
  DIDFT_OUTPUT            = $10000000;
  DIDFT_VENDORDEFINED     = $04000000;
  DIDFT_ALIAS             = $08000000;

function DIDFT_ENUMCOLLECTION(n: variant) : DWORD;
const
  DIDFT_NOCOLLECTION = $00FFFF00;



type
  PDIObjectDataFormat = ^TDIObjectDataFormat;
  TDIObjectDataFormat = packed record
    pguid: PGUID;
    dwOfs: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
  end;

  PDIDataFormat = ^TDIDataFormat;
  TDIDataFormat = packed record
    dwSize: DWORD;   
    dwObjSize: DWORD;
    dwFlags: DWORD;   
    dwDataSize: DWORD;   
    dwNumObjs: DWORD;   
    rgodf: PDIObjectDataFormat;
  end;

const
  DIDF_ABSAXIS = $00000001;
  DIDF_RELAXIS = $00000002;

type
  PDIDeviceObjectInstance_DX3A = ^TDIDeviceObjectInstance_DX3A;
  TDIDeviceObjectInstance_DX3A = packed record
    dwSize: DWORD;
    guidType: TGUID;
    dwOfs: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
    tszName: Array [0..MAX_PATH-1] of CHAR;
  end;

  PDIDeviceObjectInstance_DX3W = ^TDIDeviceObjectInstance_DX3W;
  TDIDeviceObjectInstance_DX3W = packed record
    dwSize: DWORD;
    guidType: TGUID;
    dwOfs: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
    tszName: Array [0..MAX_PATH-1] of WCHAR;
  end;

  PDIDeviceObjectInstance_DX3 = ^TDIDeviceObjectInstance_DX3;
{$IFDEF UNICODE}
  TDIDeviceObjectInstance_DX3 = TDIDeviceObjectInstance_DX3W;
{$ELSE}
  TDIDeviceObjectInstance_DX3 = TDIDeviceObjectInstance_DX3A;
{$ENDIF}

  PDIDeviceObjectInstance_DX5A = ^TDIDeviceObjectInstance_DX5A;
  TDIDeviceObjectInstance_DX5A = packed record
    dwSize: DWORD;
    guidType: TGUID;
    dwOfs: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
    tszName: Array [0..MAX_PATH-1] of CHAR;
    dwFFMaxForce: DWORD;
    dwFFForceResolution: DWORD;
    wCollectionNumber: WORD;
    wDesignatorIndex: WORD;
    wUsagePage: WORD;
    wUsage: WORD;
    dwDimension: DWORD;
    wExponent: WORD;
    wReserved: WORD;
  end;

  PDIDeviceObjectInstance_DX5W = ^TDIDeviceObjectInstance_DX5W;
  TDIDeviceObjectInstance_DX5W = packed record
    dwSize: DWORD;
    guidType: TGUID;
    dwOfs: DWORD;
    dwType: DWORD;
    dwFlags: DWORD;
    tszName: Array [0..MAX_PATH-1] of WCHAR;
    dwFFMaxForce: DWORD;
    dwFFForceResolution: DWORD;
    wCollectionNumber: WORD;
    wDesignatorIndex: WORD;
    wUsagePage: WORD;
    wUsage: WORD;
    dwDimension: DWORD;
    wExponent: WORD;
    wReserved: WORD;
  end;

  PDIDeviceObjectInstance_DX5 = ^TDIDeviceObjectInstance_DX5;
{$IFDEF UNICODE}
  TDIDeviceObjectInstance_DX5 = TDIDeviceObjectInstance_DX5W;
{$ELSE}
  TDIDeviceObjectInstance_DX5 = TDIDeviceObjectInstance_DX5A;
{$ENDIF}

  PDIDeviceObjectInstanceA = ^TDIDeviceObjectInstanceA;
  PDIDeviceObjectInstanceW = ^TDIDeviceObjectInstanceA;
  PDIDeviceObjectInstance = ^TDIDeviceObjectInstance;
{$IFDEF DIRECTX3}
  TDIDeviceObjectInstanceA = TDIDeviceObjectInstance_DX3A;
  TDIDeviceObjectInstanceW = TDIDeviceObjectInstance_DX3W;
  TDIDeviceObjectInstance = TDIDeviceObjectInstance_DX3;
{$ELSE}
  TDIDeviceObjectInstanceA = TDIDeviceObjectInstance_DX5A;
  TDIDeviceObjectInstanceW = TDIDeviceObjectInstance_DX5W;
  TDIDeviceObjectInstance = TDIDeviceObjectInstance_DX5;
{$ENDIF}

  // Bug fix (and deviation from the SDK). Callback *must* return
  // DIENUM_STOP (= 0) or DIENUM_CONTINUE (=1) in order to work
  // with the debug version of DINPUT.DLL
  TDIEnumDeviceObjectsCallbackA = function (
      var lpddoi: TDIDeviceObjectInstanceA; pvRef: Pointer): Integer; stdcall; // BOOL; stdcall;
  TDIEnumDeviceObjectsCallbackW = function (
      var lpddoi: TDIDeviceObjectInstanceW; pvRef: Pointer): Integer; stdcall; // BOOL; stdcall;
  TDIEnumDeviceObjectsCallback = function (
      var lpddoi: TDIDeviceObjectInstance; pvRef: Pointer): Integer; stdcall; // BOOL; stdcall;

  TDIEnumDeviceObjectsProc = function (
      var lpddoi: TDIDeviceObjectInstance; pvRef: Pointer): Integer; stdcall; // BOOL; stdcall;

const
  DIDOI_FFACTUATOR        = $00000001;
  DIDOI_FFEFFECTTRIGGER   = $00000002;
  DIDOI_POLLED            = $00008000;
  DIDOI_ASPECTPOSITION    = $00000100;
  DIDOI_ASPECTVELOCITY    = $00000200;
  DIDOI_ASPECTACCEL       = $00000300;
  DIDOI_ASPECTFORCE       = $00000400;
  DIDOI_ASPECTMASK        = $00000F00;
  DIDOI_GUIDISUSAGE       = $00010000;

type
  PDIPropHeader = ^TDIPropHeader;
  TDIPropHeader = packed record
    dwSize: DWORD;
    dwHeaderSize: DWORD;
    dwObj: DWORD;
    dwHow: DWORD;
  end;

const
  DIPH_DEVICE = 0;
  DIPH_BYOFFSET = 1;
  DIPH_BYID = 2;
  DIPH_BYUSAGE = 3;

function DIMAKEUSAGEDWORD(UsagePage, Usage: WORD) : DWORD;

type
  PDIPropDWord = ^TDIPropDWord;
  TDIPropDWord = packed record
    diph: TDIPropHeader;
    dwData: DWORD;
  end;

  PDIPropRange = ^TDIPropRange;
  TDIPropRange = packed record
    diph: TDIPropHeader;
    lMin: Longint;
    lMax: Longint;
  end;

const
  DIPROPRANGE_NOMIN = $80000000;
  DIPROPRANGE_NOMAX = $7FFFFFFF;

type
  PDIPropCal = ^TDIPropCal;
  TDIPropCal = packed record
    diph: TDIPropHeader;
    lMin:    LongInt;
    lCenter: LongInt;
    lMax:    LongInt;
  end;

  PDIPropGUIDAndPath = ^TDIPropGUIDAndPath;
  TDIPropGUIDAndPath = packed record
    diph: TDIPropHeader;
    guidClass: TGUID;
    wszPath: array [0..MAX_PATH-1] of WideChar;
  end;

  PDIPropString = ^TDIPropString;
  TDIPropString = packed record
    diph: TDIPropHeader;
    wsz: array [0..MAX_PATH-1] of WideChar;
  end;

type
  MAKEDIPROP = PGUID;

const
  DIPROP_BUFFERSIZE = MAKEDIPROP(1);

  DIPROP_AXISMODE = MAKEDIPROP(2);

  DIPROPAXISMODE_ABS = 0;
  DIPROPAXISMODE_REL = 1;

  DIPROP_GRANULARITY = MAKEDIPROP(3);

  DIPROP_RANGE = MAKEDIPROP(4);

  DIPROP_DEADZONE = MAKEDIPROP(5);

  DIPROP_SATURATION = MAKEDIPROP(6);

  DIPROP_FFGAIN = MAKEDIPROP(7);

  DIPROP_FFLOAD = MAKEDIPROP(8);

  DIPROP_AUTOCENTER = MAKEDIPROP(9);

  DIPROPAUTOCENTER_OFF = 0;
  DIPROPAUTOCENTER_ON = 1;

  DIPROP_CALIBRATIONMODE = MAKEDIPROP(10);

  DIPROPCALIBRATIONMODE_COOKED = 0;
  DIPROPCALIBRATIONMODE_RAW = 1;

  DIPROP_CALIBRATION      = MAKEDIPROP(11);

  DIPROP_GUIDANDPATH      = MAKEDIPROP(12);

  DIPROP_INSTANCENAME     = MAKEDIPROP(13);

  DIPROP_PRODUCTNAME      = MAKEDIPROP(14);

  DIPROP_JOYSTICKID       = MAKEDIPROP(15);

  DIPROP_GETPORTDISPLAYNAME       = MAKEDIPROP(16);


  DIPROP_ENABLEREPORTID       = MAKEDIPROP(17);


  DIPROP_GETPHYSICALRANGE            = MAKEDIPROP(18);

  DIPROP_GETLOGICALRANGE            = MAKEDIPROP(19);


type
  PDIDeviceObjectData = ^TDIDeviceObjectData;
  TDIDeviceObjectData = packed record
    dwOfs: DWORD;
    dwData: DWORD;
    dwTimeStamp: DWORD;
    dwSequence: DWORD;
  end;

const
  DIGDD_PEEK = $00000001;
{
#define DISEQUENCE_COMPARE(dwSequence1, cmp, dwSequence2) \
                         (int) ((dwSequence1) - (dwSequence2))  cmp 0
}

  DISCL_EXCLUSIVE  = $00000001;
  DISCL_NONEXCLUSIVE = $00000002;
  DISCL_FOREGROUND = $00000004;
  DISCL_BACKGROUND = $00000008;
  DISCL_NOWINKEY   = $00000010;


type

  PDIDeviceInstance_DX3A = ^TDIDeviceInstance_DX3A;
  TDIDeviceInstance_DX3A = packed record
    dwSize: DWORD;
    guidInstance: TGUID;
    guidProduct: TGUID;
    dwDevType: DWORD;
    tszInstanceName: Array [0..MAX_PATH-1] of AnsiChar;
    tszProductName: Array [0..MAX_PATH-1] of AnsiChar;
  end;

  PDIDeviceInstance_DX3W = ^TDIDeviceInstance_DX3W;
  TDIDeviceInstance_DX3W = packed record
    dwSize: DWORD;
    guidInstance: TGUID;
    guidProduct: TGUID;
    dwDevType: DWORD;
    tszInstanceName: Array [0..MAX_PATH-1] of WideChar;
    tszProductName: Array [0..MAX_PATH-1] of WideChar;
  end;

  PDIDeviceInstance_DX3 = ^TDIDeviceInstance_DX3;
{$IFDEF UNICODE}
  TDIDeviceInstance_DX3 = TDIDeviceInstance_DX3W;
{$ELSE}
  TDIDeviceInstance_DX3 = TDIDeviceInstance_DX3A;
{$ENDIF}

  PDIDeviceInstance_DX5A = ^TDIDeviceInstance_DX5A;
  TDIDeviceInstance_DX5A = packed record
    dwSize: DWORD;
    guidInstance: TGUID;
    guidProduct: TGUID;
    dwDevType: DWORD;
    tszInstanceName: Array [0..MAX_PATH-1] of AnsiChar;
    tszProductName: Array [0..MAX_PATH-1] of AnsiChar;
    guidFFDriver: TGUID;
    wUsagePage: WORD;
    wUsage: WORD;
  end;

  PDIDeviceInstance_DX5W = ^TDIDeviceInstance_DX5W;
  TDIDeviceInstance_DX5W = packed record
    dwSize: DWORD;
    guidInstance: TGUID;
    guidProduct: TGUID;
    dwDevType: DWORD;
    tszInstanceName: Array [0..MAX_PATH-1] of WideChar;
    tszProductName: Array [0..MAX_PATH-1] of WideChar;
    guidFFDriver: TGUID;
    wUsagePage: WORD;
    wUsage: WORD;
  end;

  PDIDeviceInstance_DX5 = ^TDIDeviceInstance_DX5;
{$IFDEF UNICODE}
  TDIDeviceInstance_DX5 = TDIDeviceInstance_DX5W;
{$ELSE}
  TDIDeviceInstance_DX5 = TDIDeviceInstance_DX5A;
{$ENDIF}

  PDIDeviceInstanceA = ^TDIDeviceInstanceA;
  PDIDeviceInstanceW = ^TDIDeviceInstanceW;
  PDIDeviceInstance = ^TDIDeviceInstance;
{$IFDEF DIRECTX3}
  TDIDeviceInstanceA = TDIDeviceInstance_DX3A;
  TDIDeviceInstanceW = TDIDeviceInstance_DX3W;
  TDIDeviceInstance = TDIDeviceInstance_DX3;
{$ELSE}
  TDIDeviceInstanceA = TDIDeviceInstance_DX5A;
  TDIDeviceInstanceW = TDIDeviceInstance_DX5W;
  TDIDeviceInstance = TDIDeviceInstance_DX5;
{$ENDIF}

  IDirectInputDeviceA = interface (IUnknown)
    ['{5944E680-C92E-11CF-BFC7-444553540000}']
    (*** IDirectInputDeviceA methods ***)
    function GetCapabilities(var lpDIDevCaps: TDIDevCaps) : HResult;  stdcall;
    function EnumObjects(lpCallback: TDIEnumDeviceObjectsCallbackA;
        pvRef: Pointer; dwFlags: DWORD) : HResult;  stdcall;
    function GetProperty(rguidProp: PGUID; var pdiph: TDIPropHeader) :
        HResult;  stdcall;
    function SetProperty(rguidProp: PGUID; const pdiph: TDIPropHeader) :
        HResult;  stdcall;
    function Acquire : HResult;  stdcall;
    function Unacquire : HResult;  stdcall;
    function GetDeviceState(cbData: DWORD; lpvData: Pointer) : HResult;  stdcall;
    function GetDeviceData(cbObjectData: DWORD; rgdod: PDIDeviceObjectData;
        var pdwInOut: DWORD; dwFlags: DWORD) : HResult;  stdcall;
    function SetDataFormat(var lpdf: TDIDataFormat) : HResult;  stdcall;
    function SetEventNotification(hEvent: THandle) : HResult;  stdcall;
    function SetCooperativeLevel(hwnd: HWND; dwFlags: DWORD) : HResult;  stdcall;
    function GetObjectInfo(var pdidoi: TDIDeviceObjectInstanceA; dwObj: DWORD;
        dwHow: DWORD) : HResult;  stdcall;
    function GetDeviceInfo(var pdidi: TDIDeviceInstanceA) : HResult;  stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD) : HResult;  stdcall;
    function Initialize(hinst: THandle; dwVersion: DWORD; const rguid: TGUID) : HResult;  stdcall;
  end;

  IDirectInputDeviceW = interface (IUnknown)
    ['{5944E681-C92E-11CF-BFC7-444553540000}']
    (*** IDirectInputDeviceW methods ***)
    function GetCapabilities(var lpDIDevCaps: TDIDevCaps) : HResult;  stdcall;
    function EnumObjects(lpCallback: TDIEnumDeviceObjectsCallbackW;
        pvRef: Pointer; dwFlags: DWORD) : HResult;  stdcall;
    function GetProperty(rguidProp: PGUID; var pdiph: TDIPropHeader) :
        HResult;  stdcall;
    function SetProperty(rguidProp: PGUID; var pdiph: TDIPropHeader) :
        HResult;  stdcall;
    function Acquire : HResult;  stdcall;
    function Unacquire : HResult;  stdcall;
    function GetDeviceState(cbData: DWORD; lpvData: Pointer) : HResult;  stdcall;
    function GetDeviceData(cbObjectData: DWORD; rgdod: PDIDeviceObjectData;
        var pdwInOut: DWORD; dwFlags: DWORD) : HResult;  stdcall;
    function SetDataFormat(var lpdf: TDIDataFormat) : HResult;  stdcall;
    function SetEventNotification(hEvent: THandle) : HResult;  stdcall;
    function SetCooperativeLevel(hwnd: HWND; dwFlags: DWORD) : HResult;  stdcall;
    function GetObjectInfo(var pdidoi: TDIDeviceObjectInstanceW; dwObj: DWORD;
        dwHow: DWORD) : HResult;  stdcall;
    function GetDeviceInfo(var pdidi: TDIDeviceInstanceW) : HResult;  stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD) : HResult;  stdcall;
    function Initialize(hinst: THandle; dwVersion: DWORD; const rguid: TGUID) : HResult;  stdcall;
  end;

{$IFDEF UNICODE}
  IDirectInputDevice = IDirectInputDeviceW;
{$ELSE}
  IDirectInputDevice = IDirectInputDeviceA;
{$ENDIF}

const
  DISFFC_RESET            = $00000001;
  DISFFC_STOPALL          = $00000002;
  DISFFC_PAUSE            = $00000004;
  DISFFC_CONTINUE         = $00000008;
  DISFFC_SETACTUATORSON   = $00000010;
  DISFFC_SETACTUATORSOFF  = $00000020;

  DIGFFS_EMPTY            = $00000001;
  DIGFFS_STOPPED          = $00000002;
  DIGFFS_PAUSED           = $00000004;
  DIGFFS_ACTUATORSON      = $00000010;
  DIGFFS_ACTUATORSOFF     = $00000020;
  DIGFFS_POWERON          = $00000040;
  DIGFFS_POWEROFF         = $00000080;
  DIGFFS_SAFETYSWITCHON   = $00000100;
  DIGFFS_SAFETYSWITCHOFF  = $00000200;
  DIGFFS_USERFFSWITCHON   = $00000400;
  DIGFFS_USERFFSWITCHOFF  = $00000800;
  DIGFFS_DEVICELOST       = $80000000;

type
  PDIEffectInfoA = ^TDIEffectInfoA;
  TDIEffectInfoA = packed record
    dwSize : DWORD;
    guid : TGUID;
    dwEffType : DWORD;
    dwStaticParams : DWORD;
    dwDynamicParams : DWORD;
    tszName : array [0..MAX_PATH-1] of CHAR;
  end;

  PDIEffectInfoW = ^TDIEffectInfoW;
  TDIEffectInfoW = packed record
    dwSize : DWORD;
    guid : TGUID;
    dwEffType : DWORD;
    dwStaticParams : DWORD;
    dwDynamicParams : DWORD;
    tszName : array [0..MAX_PATH-1] of WCHAR;
  end;

  PDIEffectInfo = ^TDIEffectInfo;
{$IFDEF UNICODE}
  TDIEffectInfo = TDIEffectInfoW;
{$ELSE}
  TDIEffectInfo = TDIEffectInfoA;
{$ENDIF}

const
  DISDD_CONTINUE          = $00000001;

  // Bug fix & deviation from the SDK: Must return DIENUM_STOP or
  // DIENUM_CONTINUE (=1) in order to work with the debug version of DINPUT.DLL
type
  TDIEnumEffectsCallbackA =
      function(var pdei: TDIEffectInfoA; pvRef: pointer): Integer; stdcall; // BOOL; stdcall;
  TDIEnumEffectsCallbackW =
      function(var pdei: TDIEffectInfoW; pvRef: pointer): Integer; stdcall; // BOOL; stdcall;
  TDIEnumEffectsCallback =
      function(var pdei: TDIEffectInfo; pvRef: pointer) : Integer; stdcall; // BOOL; stdcall;
  TDIEnumEffectsProc = TDIEnumEffectsCallback;

  TDIEnumCreatedEffectObjectsCallback =
      function(peff: IDirectInputEffect; pvRev: pointer): Integer; stdcall; // BOOL; stdcall;
  TDIEnumCreatedEffectObjectsProc = TDIEnumCreatedEffectObjectsCallback;

  IDirectInputDevice2A = interface (IDirectInputDeviceA)
    ['{5944E682-C92E-11CF-BFC7-444553540000}']
    (*** IDirectInputDevice2A methods ***)
    function CreateEffect(const rguid: TGUID; lpeff: PDIEffect;
        var ppdeff: IDirectInputEffect; punkOuter: IUnknown) : HResult;  stdcall;
    function EnumEffects(lpCallback: TDIEnumEffectsCallbackA;
        pvRef: pointer; dwEffType: DWORD) : HResult;  stdcall;
    function GetEffectInfo(pdei: TDIEffectInfoA; const rguid: TGUID) : HResult;  stdcall;
    function GetForceFeedbackState(var pdwOut: DWORD) : HResult;  stdcall;
    function SendForceFeedbackCommand(dwFlags: DWORD) : HResult;  stdcall;
    function EnumCreatedEffectObjects(lpCallback:
        TDIEnumCreatedEffectObjectsCallback;
        pvRef: pointer; fl: DWORD) : HResult;  stdcall;
    function Escape(var pesc: TDIEffEscape) : HResult;  stdcall;
    function Poll : HResult;  stdcall;
    function SendDeviceData
        (cbObjectData: DWORD; var rgdod: TDIDeviceObjectData;
        var pdwInOut: DWORD; fl: DWORD) : HResult;  stdcall;
  end;

  IDirectInputDevice2W = interface (IDirectInputDeviceW)
    ['{5944E683-C92E-11CF-BFC7-444553540000}']
    (*** IDirectInputDevice2W methods ***)
    function CreateEffect(const rguid: TGUID; lpeff: PDIEffect;
        var ppdeff: IDirectInputEffect; punkOuter: IUnknown) : HResult;  stdcall;
    function EnumEffects(lpCallback: TDIEnumEffectsCallbackW;
        pvRef: pointer; dwEffType: DWORD) : HResult;  stdcall;
    function GetEffectInfo(pdei: TDIEffectInfoW; const rguid: TGUID) : HResult;  stdcall;
    function GetForceFeedbackState(var pdwOut: DWORD) : HResult;  stdcall;
    function SendForceFeedbackCommand(dwFlags: DWORD) : HResult;  stdcall;
    function EnumCreatedEffectObjects(lpCallback:
        TDIEnumCreatedEffectObjectsCallback;
        pvRef: pointer; fl: DWORD) : HResult;  stdcall;
    function Escape(var pesc: TDIEffEscape) : HResult;  stdcall;
    function Poll : HResult;  stdcall;
    function SendDeviceData
        (cbObjectData: DWORD; var rgdod: TDIDeviceObjectData;
        var pdwInOut: DWORD; fl: DWORD) : HResult;  stdcall;
  end;

{$IFDEF UNICODE}
  IDirectInputDevice2 = IDirectInputDevice2W;
{$ELSE}
  IDirectInputDevice2 = IDirectInputDevice2A;
{$ENDIF}

const
  DIFEF_DEFAULT               = $00000000;
  DIFEF_INCLUDENONSTANDARD    = $00000001;
  DIFEF_MODIFYIFNEEDED	    	= $00000010;

///Weitermachen:  (as: nur die Deklarationen eingefüllt, die ich zum Testen gebraucht habe)

type
  TEnumEffectsInFileCallback = function(gaga, huhu: Integer): HResult;

type
  IDirectInputDevice7W = interface (IDirectInputDevice2W)
    ['{57D7C6BD-2356-11D3-8E9D-00C04F6844AE}']
    (*** IDirectInputDevice7A methods ***)
    function EnumEffectsInFile(const lpszFileName: PChar;
      pec: TEnumEffectsInFileCallback; pvRef: Pointer; dwFlags: DWord): HResult; stdcall;
    function WriteEffectToFile(const lpszFileName: PChar;
      dwEntries: DWord; const rgDIFileEft: PDIFileEffect; dwFlags: DWord): HResult; stdcall;
  end;

  IDirectInputDevice7A = interface (IDirectInputDevice2A)
    ['{57D7C6BC-2356-11D3-8E9D-00C04F6844AE}']
    function EnumEffectsInFile(const lpszFileName: PChar;
      pec: TEnumEffectsInFileCallback; pvRef: Pointer; dwFlags: DWord): HResult; stdcall;
    function WriteEffectToFile(const lpszFileName: PChar;
      dwEntries: DWord; const rgDIFileEft: PDIFileEffect; dwFlags: DWord): HResult; stdcall;
  end;

{$IFDEF UNICODE}
  IDirectInputDevice7 = IDirectInputDevice7W;
{$ELSE}
  IDirectInputDevice7 = IDirectInputDevice7A;
{$ENDIF}

(****************************************************************************
 *
 *      Mouse
 *
 ****************************************************************************)

type
  PDIMouseState = ^TDIMouseState;
  TDIMouseState = packed record
    lX: Longint;
    lY: Longint;
    lZ: Longint;
    rgbButtons: Array [0..3] of BYTE;  // up to 4 buttons
  end;

  PDIMouseState2 = ^TDIMouseState2;
  TDIMouseState2 = packed record
    lX: Longint;
    lY: Longint;
    lZ: Longint;
    rgbButtons: Array [0..7] of BYTE;  // up to 8 buttons
  end;

const
  DIMOFS_X       = 0;
  DIMOFS_Y       = 4;
  DIMOFS_Z       = 8;
  DIMOFS_BUTTON0 = 12;
  DIMOFS_BUTTON1 = 13;
  DIMOFS_BUTTON2 = 14;
  DIMOFS_BUTTON3 = 15;
  // DX7 supports up to 8 mouse buttons
  DIMOFS_BUTTON4 = DIMOFS_BUTTON0+4;
  DIMOFS_BUTTON5 = DIMOFS_BUTTON0+5;
  DIMOFS_BUTTON6 = DIMOFS_BUTTON0+6;
  DIMOFS_BUTTON7 = DIMOFS_BUTTON0+7;


const
  _c_dfDIMouse_Objects: array[0..6] of TDIObjectDataFormat = (
    (  pguid: @GUID_XAxis;
       dwOfs: DIMOFS_X;
       dwType: DIDFT_AXIS or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_YAxis;
       dwOfs: DIMOFS_Y;
       dwType: DIDFT_AXIS or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_ZAxis;
       dwOfs: DIMOFS_Z;
       dwType: $80000000 or DIDFT_AXIS or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIMOFS_BUTTON0;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIMOFS_BUTTON1;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIMOFS_BUTTON2;
       dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIMOFS_BUTTON3;
       dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0)
    );

  c_dfDIMouse: TDIDataFormat = (
    dwSize: Sizeof(c_dfDIMouse);              // $18
    dwObjSize: Sizeof(TDIObjectDataFormat);   // $10
    dwFlags: DIDF_RELAXIS;                    //
    dwDataSize: Sizeof(TDIMouseState);        // $10
    dwNumObjs: High(_c_dfDIMouse_Objects)+1;  // 7
    rgodf: @_c_dfDIMouse_Objects[Low(_c_dfDIMouse_Objects)]
  );


  _c_dfDIMouse2_Objects: array[0..10] of TDIObjectDataFormat = (
    (  pguid: @GUID_XAxis;
       dwOfs: DIMOFS_X;
       dwType: DIDFT_AXIS or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_YAxis;
       dwOfs: DIMOFS_Y;
       dwType: DIDFT_AXIS or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_ZAxis;
       dwOfs: DIMOFS_Z;
       dwType: $80000000 or DIDFT_AXIS or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIMOFS_BUTTON0;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIMOFS_BUTTON1;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIMOFS_BUTTON2;
       dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIMOFS_BUTTON3;
       dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    // fields introduced with IDirectInputDevice7.GetDeviceState       
    (  pguid: nil;
       dwOfs: DIMOFS_BUTTON4;
       dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIMOFS_BUTTON5;
       dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIMOFS_BUTTON6;
       dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIMOFS_BUTTON7;
       dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0)
    );

  c_dfDIMouse2: TDIDataFormat = (
    dwSize: Sizeof(c_dfDIMouse);              // $18
    dwObjSize: Sizeof(TDIObjectDataFormat);   // $10
    dwFlags: DIDF_RELAXIS;                    //
    dwDataSize: Sizeof(TDIMouseState2);        // $14
    dwNumObjs: High(_c_dfDIMouse_Objects)+1;  // 11
    rgodf: @_c_dfDIMouse2_Objects[Low(_c_dfDIMouse2_Objects)]
  );


(****************************************************************************
 *
 *      DirectInput keyboard scan codes
 *
 ****************************************************************************)

const
  DIK_ESCAPE          = $01;
  DIK_1               = $02;
  DIK_2               = $03;
  DIK_3               = $04;
  DIK_4               = $05;
  DIK_5               = $06;
  DIK_6               = $07;
  DIK_7               = $08;
  DIK_8               = $09;
  DIK_9               = $0A;
  DIK_0               = $0B;
  DIK_MINUS           = $0C;    (* - on main keyboard *)
  DIK_EQUALS          = $0D;
  DIK_BACK            = $0E;    (* backspace *)
  DIK_TAB             = $0F;
  DIK_Q               = $10;
  DIK_W               = $11;
  DIK_E               = $12;
  DIK_R               = $13;
  DIK_T               = $14;
  DIK_Y               = $15;
  DIK_U               = $16;
  DIK_I               = $17;
  DIK_O               = $18;
  DIK_P               = $19;
  DIK_LBRACKET        = $1A;
  DIK_RBRACKET        = $1B;
  DIK_RETURN          = $1C;    (* Enter on main keyboard *)
  DIK_LCONTROL        = $1D;
  DIK_A               = $1E;
  DIK_S               = $1F;
  DIK_D               = $20;
  DIK_F               = $21;
  DIK_G               = $22;
  DIK_H               = $23;
  DIK_J               = $24;
  DIK_K               = $25;
  DIK_L               = $26;
  DIK_SEMICOLON       = $27;
  DIK_APOSTROPHE      = $28;
  DIK_GRAVE           = $29;    (* accent grave *)
  DIK_LSHIFT          = $2A;
  DIK_BACKSLASH       = $2B;
  DIK_Z               = $2C;
  DIK_X               = $2D;
  DIK_C               = $2E;
  DIK_V               = $2F;
  DIK_B               = $30;
  DIK_N               = $31;
  DIK_M               = $32;
  DIK_COMMA           = $33;
  DIK_PERIOD          = $34;    (* . on main keyboard *)
  DIK_SLASH           = $35;    (* / on main keyboard *)
  DIK_RSHIFT          = $36;
  DIK_MULTIPLY        = $37;    (* * on numeric keypad *)
  DIK_LMENU           = $38;    (* left Alt *)
  DIK_SPACE           = $39;
  DIK_CAPITAL         = $3A;
  DIK_F1              = $3B;
  DIK_F2              = $3C;
  DIK_F3              = $3D;
  DIK_F4              = $3E;
  DIK_F5              = $3F;
  DIK_F6              = $40;
  DIK_F7              = $41;
  DIK_F8              = $42;
  DIK_F9              = $43;
  DIK_F10             = $44;
  DIK_NUMLOCK         = $45;
  DIK_SCROLL          = $46;    (* Scroll Lock *)
  DIK_NUMPAD7         = $47;
  DIK_NUMPAD8         = $48;
  DIK_NUMPAD9         = $49;
  DIK_SUBTRACT        = $4A;    (* - on numeric keypad *)
  DIK_NUMPAD4         = $4B;
  DIK_NUMPAD5         = $4C;
  DIK_NUMPAD6         = $4D;
  DIK_ADD             = $4E;    (* + on numeric keypad *)
  DIK_NUMPAD1         = $4F;
  DIK_NUMPAD2         = $50;
  DIK_NUMPAD3         = $51;
  DIK_NUMPAD0         = $52;
  DIK_DECIMAL         = $53;    (* . on numeric keypad *)
  // $54 to $56 unassigned
  DIK_F11             = $57;
  DIK_F12             = $58;
  // $59 to $63 unassigned
  DIK_F13             = $64;    (*                     (NEC PC98) *)
  DIK_F14             = $65;    (*                     (NEC PC98) *)
  DIK_F15             = $66;    (*                     (NEC PC98) *)
  // $67 to $6F unassigned
  DIK_KANA            = $70;    (* (Japanese keyboard)            *)
  DIK_CONVERT         = $79;    (* (Japanese keyboard)            *)
  DIK_NOCONVERT       = $7B;    (* (Japanese keyboard)            *)
  DIK_YEN             = $7D;    (* (Japanese keyboard)            *)
  DIK_NUMPADEQUALS    = $8D;    (* = on numeric keypad (NEC PC98) *)
  // $8E to $8F unassigned
  DIK_CIRCUMFLEX      = $90;    (* (Japanese keyboard)            *)
  DIK_AT              = $91;    (*                     (NEC PC98) *)
  DIK_COLON           = $92;    (*                     (NEC PC98) *)
  DIK_UNDERLINE       = $93;    (*                     (NEC PC98) *)
  DIK_KANJI           = $94;    (* (Japanese keyboard)            *)
  DIK_STOP            = $95;    (*                     (NEC PC98) *)
  DIK_AX              = $96;    (*                     (Japan AX) *)
  DIK_UNLABELED       = $97;    (*                        (J3100) *)
  // $98 to $99 unassigned
  DIK_NUMPADENTER     = $9C;    (* Enter on numeric keypad *)
  DIK_RCONTROL        = $9D;
  // $9E to $B2 unassigned
  DIK_NUMPADCOMMA     = $B3;    (* , on numeric keypad (NEC PC98) *)
  // $B4 unassigned
  DIK_DIVIDE          = $B5;    (* / on numeric keypad *)
  // $B6 unassigned
  DIK_SYSRQ           = $B7;
  DIK_RMENU           = $B8;    (* right Alt *)
  // $B9 to $C4 unassigned
  DIK_PAUSE           = $C5;    (* Pause (watch out - not realiable on some kbds) *)
  // $C6 unassigned
  DIK_HOME            = $C7;    (* Home on arrow keypad *)
  DIK_UP              = $C8;    (* UpArrow on arrow keypad *)
  DIK_PRIOR           = $C9;    (* PgUp on arrow keypad *)
  // $CA unassigned
  DIK_LEFT            = $CB;    (* LeftArrow on arrow keypad *)
  DIK_RIGHT           = $CD;    (* RightArrow on arrow keypad *)
  // $CF unassigned
  DIK_END             = $CF;    (* End on arrow keypad *)
  DIK_DOWN            = $D0;    (* DownArrow on arrow keypad *)
  DIK_NEXT            = $D1;    (* PgDn on arrow keypad *)
  DIK_INSERT          = $D2;    (* Insert on arrow keypad *)
  DIK_DELETE          = $D3;    (* Delete on arrow keypad *)
  DIK_LWIN            = $DB;    (* Left Windows key *)
  DIK_RWIN            = $DC;    (* Right Windows key *)
  DIK_APPS            = $DD;    (* AppMenu key *)
  // New with DX 6.1 & Win98
  DIK_POWER           = $DE;
  DIK_SLEEP           = $DF;
  // $E0 to $E2 unassigned
  // $E3 = Wake up ("translated" in German DInput to "Kielwasser" (ship's wake) ;-)

(*
 *  Alternate names for keys, to facilitate transition from DOS.
 *)
  DIK_BACKSPACE      = DIK_BACK    ;        (* backspace *)
  DIK_NUMPADSTAR     = DIK_MULTIPLY;        (* * on numeric keypad *)
  DIK_LALT           = DIK_LMENU   ;        (* left Alt *)
  DIK_CAPSLOCK       = DIK_CAPITAL ;        (* CapsLock *)
  DIK_NUMPADMINUS    = DIK_SUBTRACT;        (* - on numeric keypad *)
  DIK_NUMPADPLUS     = DIK_ADD     ;        (* + on numeric keypad *)
  DIK_NUMPADPERIOD   = DIK_DECIMAL ;        (* . on numeric keypad *)
  DIK_NUMPADSLASH    = DIK_DIVIDE  ;        (* / on numeric keypad *)
  DIK_RALT           = DIK_RMENU   ;        (* right Alt *)
  DIK_UPARROW        = DIK_UP      ;        (* UpArrow on arrow keypad *)
  DIK_PGUP           = DIK_PRIOR   ;        (* PgUp on arrow keypad *)
  DIK_LEFTARROW      = DIK_LEFT    ;        (* LeftArrow on arrow keypad *)
  DIK_RIGHTARROW     = DIK_RIGHT   ;        (* RightArrow on arrow keypad *)
  DIK_DOWNARROW      = DIK_DOWN    ;        (* DownArrow on arrow keypad *)
  DIK_PGDN           = DIK_NEXT    ;        (* PgDn on arrow keypad *)

(****************************************************************************
 *
 *      Keyboard
 *
 ****************************************************************************)


type
  TDIKeyboardState = array[0..255] of Byte;
(*
const
  _c_dfDIKeyboard_Objects: array[0..255] of TDIObjectDataFormat = (
    (  pguid: @GUID_Key;
       dwOfs: DIK_ESCAPE;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    // -------- top row (except function keys) on main kbd ------------
    (  pguid: @GUID_Key;
       dwOfs: DIK_1;  // "1" on main kbd, Offset 2
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_2;  // "2" on main kbd, Offset 3
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_3;  // "3" on main kbd, etc.
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_4;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_5;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_6;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_7;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_8;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_9;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_0;  // "0", main kbd
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_MINUS; // "-" on US kbds, "ß" on german kbds
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_EQUALS;  // "=" for US, "´" for german
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_BACK;  // backspace
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    // ----------- 2nd row -----------------------
    (  pguid: @GUID_Key;
       dwOfs: DIK_TAB;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_Q;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_W;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_E;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_R;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_T;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_Y;  // "Z" on german & french keyboards
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_U;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_I;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_O;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_P;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_LBRACKET;  // "Ü" on german keyboards
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_RBRACKET;  // "+" on german keyboards
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_RETURN;   // Enter on main kbd
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    // next row should really start with caps lock but doesn't ;-)
    // (DIK_CAPITAL is Offset $3A, i.e. after 4th row)
    (  pguid: @GUID_Key;
       dwOfs: DIK_LCONTROL;  // Left Ctrl (german kbds: "Strg")
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    // ----------- 3rd row ------------------------------
    (  pguid: @GUID_Key;
       dwOfs: DIK_A;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_S;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_D;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_F;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_G;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_H;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_J;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_K;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_L;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_SEMICOLON;  // "Ö" on german kbds
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_APOSTROPHE;  // "Ä" on german kbds
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_GRAVE; // accent grave, "'" on german kbds
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    // ---------------- 4th row -----------------------
    (  pguid: @GUID_Key;
       dwOfs: DIK_LSHIFT;  // left shift
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_BACKSLASH;  // "<" on german kbds
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_Z;     // "Y" on german kbds
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_X;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_C;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_V;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_B;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_N;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_M;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_COMMA;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_PERIOD;  // on main kbd
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_SLASH;  // "-" on german kbds
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_RSHIFT;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    // --- misc keys (bye, bye, order) ----------------
    (  pguid: @GUID_Key;
       dwOfs: DIK_MULTIPLY;  // on numeric keypad
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_LMENU;  // left ALT
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_SPACE;  // space bar
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_CAPITAL;   // caps lock (on main kbd, above LSHIFT)
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    // ---------- function keys -----------
    (  pguid: @GUID_Key;
       dwOfs: DIK_F1;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_F2;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_F3;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_F4;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_F5;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_F6;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_F7;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_F8;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_F9;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_F10;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    // ------- F11, F12 after numeric keypad (for "historical reasons" -- XT kbd)

    // --------- numeric keypad (mostly, that is) -----------
    (  pguid: @GUID_Key;
       dwOfs: DIK_NUMLOCK;   // numeric keypad
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_SCROLL;  // scroll lock
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_NUMPAD7;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_NUMPAD8;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_NUMPAD9;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_SUBTRACT;  // "-" on numeric keypad
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_NUMPAD4;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_NUMPAD5;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_NUMPAD6;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_ADD;   // "+" on numeric keypad
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_NUMPAD1;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_NUMPAD2;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_NUMPAD3;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_NUMPAD0;  // "0" or "Insert" on numeric keypad
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_DECIMAL;  // "." or "Del" on numeric keypad
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: $54;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    // "extended" function keys; F13 to F15 only on NEC PC98
    (  pguid: @GUID_Key;
       dwOfs: DIK_F11;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_F12;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    // -------------------------------------------------
    // a whole lot of keys for asian kbds only
    // -------------------------------------------------
    (  pguid: @GUID_Key;
       dwOfs: DIK_NUMPADENTER;  // Enter on numeric keypad
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_RCONTROL;        // right Ctrl on main kbd
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;   // "," on numeric keypad (NEC PC98 only)
       dwOfs: DIK_NUMPADCOMMA;
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_DIVIDE;   // "/" on numeric keypad
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_SYSRQ;   // "System request", "Druck/S-Abf" on german kbds
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_RMENU;  // right ALT
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_PAUSE;  // "Pause" - not reliable on some kbds
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),

    // ----------- arrow keypad -----------------
    (  pguid: @GUID_Key;
       dwOfs:   DIK_HOME;    // Home on arrow keypad 
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_UP;        // UpArrow on arrow keypad 
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_PRIOR;    // PgUp on arrow keypad
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_LEFT;    // LeftArrow on arrow keypad 
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_RIGHT;    // RightArrow on arrow keypad 
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_END;    // End on arrow keypad 
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_DOWN;    // DownArrow on arrow keypad
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_NEXT;    // PgDn on arrow keypad 
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_INSERT;    // Insert on arrow keypad 
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_DELETE;    // Delete on arrow keypad
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_LWIN;    // Left Windows key 
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_RWIN;    // Right Windows key 
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: DIK_APPS;    // AppMenu key
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    // -------- added with Win 98 / DirectX 6.1 ------------
    (  pguid: @GUID_Key;
       dwOfs: 222;    // Power on key
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: 223;    // Sleep key
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0),
    (  pguid: @GUID_Key;
       dwOfs: 227;   // Wake (up) key. The german "translation"
                     // reads "Kielwasser" (ship's wake) ;-)
       dwType: DIDFT_BUTTON or DIDFT_NOCOLLECTION;
       dwFlags: 0)
  );
*)

var  // set by initialization - I was simply too lazy
  _c_dfDIKeyboard_Objects: array[0..255] of TDIObjectDataFormat;
const
  c_dfDIKeyboard: TDIDataFormat = (
    dwSize: Sizeof(c_dfDIKeyboard);
    dwObjSize: Sizeof(TDIObjectDataFormat);
    dwFlags: DIDF_RELAXIS;
    dwDataSize: Sizeof(TDIKeyboardState);
    dwNumObjs: High(_c_dfDIKeyboard_Objects)+1;
    rgodf: @_c_dfDIKeyboard_Objects[Low(_c_dfDIKeyboard_Objects)]
  );

(****************************************************************************
 *
 *      Joystick
 *
 ****************************************************************************)


type
  PDIJoyState = ^TDIJoyState;
  TDIJoyState = packed record
    lX: Longint;   (* x-axis position              *)
    lY: Longint;   (* y-axis position              *)
    lZ: Longint;   (* z-axis position              *)
    lRx: Longint;   (* x-axis rotation              *)
    lRy: Longint;   (* y-axis rotation              *)
    lRz: Longint;   (* z-axis rotation              *)
    rglSlider: Array [0..1] of Longint;   (* extra axes positions         *)
    rgdwPOV: Array [0..3] of DWORD;   (* POV directions               *)
    rgbButtons: Array [0..31] of BYTE;   (* 32 buttons                   *)
  end;

  PDIJoyState2 = ^TDIJoyState2;
  TDIJoyState2 = packed record
    lX: Longint;   (* x-axis position              *)
    lY: Longint;   (* y-axis position              *)
    lZ: Longint;   (* z-axis position              *)
    lRx: Longint;   (* x-axis rotation              *)
    lRy: Longint;   (* y-axis rotation              *)
    lRz: Longint;   (* z-axis rotation              *)
    rglSlider: Array [0..1] of Longint;   (* extra axes positions         *)
    rgdwPOV: Array [0..3] of DWORD;   (* POV directions               *)
    rgbButtons: Array [0..127] of BYTE;   (* 128 buttons                  *)
    lVX: Longint;   (* x-axis velocity              *)
    lVY: Longint;   (* y-axis velocity              *)
    lVZ: Longint;   (* z-axis velocity              *)
    lVRx: Longint;   (* x-axis angular velocity      *)
    lVRy: Longint;   (* y-axis angular velocity      *)
    lVRz: Longint;   (* z-axis angular velocity      *)
    rglVSlider: Array [0..1] of Longint;   (* extra axes velocities        *)
    lAX: Longint;   (* x-axis acceleration          *)
    lAY: Longint;   (* y-axis acceleration          *)
    lAZ: Longint;   (* z-axis acceleration          *)
    lARx: Longint;   (* x-axis angular acceleration  *)
    lARy: Longint;   (* y-axis angular acceleration  *)
    lARz: Longint;   (* z-axis angular acceleration  *)
    rglASlider: Array [0..1] of Longint;   (* extra axes accelerations     *)
    lFX: Longint;   (* x-axis force                 *)
    lFY: Longint;   (* y-axis force                 *)
    lFZ: Longint;   (* z-axis force                 *)
    lFRx: Longint;   (* x-axis torque                *)
    lFRy: Longint;   (* y-axis torque                *)
    lFRz: Longint;   (* z-axis torque                *)
    rglFSlider: Array [0..1] of Longint;   (* extra axes forces            *)
  end;


function DIJOFS_SLIDER(n: variant) : variant;

function DIJOFS_POV(n: variant) : variant;

function DIJOFS_BUTTON(n: variant) : variant;
const
  DIJOFS_BUTTON_ = 48;

const
  DIJOFS_BUTTON0 = DIJOFS_BUTTON_ + 0;
  DIJOFS_BUTTON1 = DIJOFS_BUTTON_ + 1;
  DIJOFS_BUTTON2 = DIJOFS_BUTTON_ + 2;
  DIJOFS_BUTTON3 = DIJOFS_BUTTON_ + 3;
  DIJOFS_BUTTON4 = DIJOFS_BUTTON_ + 4;
  DIJOFS_BUTTON5 = DIJOFS_BUTTON_ + 5;
  DIJOFS_BUTTON6 = DIJOFS_BUTTON_ + 6;
  DIJOFS_BUTTON7 = DIJOFS_BUTTON_ + 7;
  DIJOFS_BUTTON8 = DIJOFS_BUTTON_ + 8;
  DIJOFS_BUTTON9 = DIJOFS_BUTTON_ + 9;
  DIJOFS_BUTTON10 = DIJOFS_BUTTON_ + 10;
  DIJOFS_BUTTON11 = DIJOFS_BUTTON_ + 11;
  DIJOFS_BUTTON12 = DIJOFS_BUTTON_ + 12;
  DIJOFS_BUTTON13 = DIJOFS_BUTTON_ + 13;
  DIJOFS_BUTTON14 = DIJOFS_BUTTON_ + 14;
  DIJOFS_BUTTON15 = DIJOFS_BUTTON_ + 15;
  DIJOFS_BUTTON16 = DIJOFS_BUTTON_ + 16;
  DIJOFS_BUTTON17 = DIJOFS_BUTTON_ + 17;
  DIJOFS_BUTTON18 = DIJOFS_BUTTON_ + 18;
  DIJOFS_BUTTON19 = DIJOFS_BUTTON_ + 19;
  DIJOFS_BUTTON20 = DIJOFS_BUTTON_ + 20;
  DIJOFS_BUTTON21 = DIJOFS_BUTTON_ + 21;
  DIJOFS_BUTTON22 = DIJOFS_BUTTON_ + 22;
  DIJOFS_BUTTON23 = DIJOFS_BUTTON_ + 23;
  DIJOFS_BUTTON24 = DIJOFS_BUTTON_ + 24;
  DIJOFS_BUTTON25 = DIJOFS_BUTTON_ + 25;
  DIJOFS_BUTTON26 = DIJOFS_BUTTON_ + 26;
  DIJOFS_BUTTON27 = DIJOFS_BUTTON_ + 27;
  DIJOFS_BUTTON28 = DIJOFS_BUTTON_ + 28;
  DIJOFS_BUTTON29 = DIJOFS_BUTTON_ + 29;
  DIJOFS_BUTTON30 = DIJOFS_BUTTON_ + 30;
  DIJOFS_BUTTON31 = DIJOFS_BUTTON_ + 31;


const
  DIJOFS_X  =0;
  DIJOFS_Y  =4;
  DIJOFS_Z  =8;
  DIJOFS_RX =12;
  DIJOFS_RY =16;
  DIJOFS_RZ =20;

  _c_dfDIJoystick_Objects: array[0..43] of TDIObjectDataFormat = (
    (  pguid: @GUID_XAxis;
       dwOfs: DIJOFS_X; dwType: $80000000 or DIDFT_AXIS or DIDFT_NOCOLLECTION; dwFlags: $100),
    (  pguid: @GUID_YAxis;
       dwOfs: DIJOFS_Y; dwType: $80000000 or DIDFT_AXIS or DIDFT_NOCOLLECTION; dwFlags: $100),
    (  pguid: @GUID_ZAxis;
       dwOfs: DIJOFS_Z; dwType: $80000000 or DIDFT_AXIS or DIDFT_NOCOLLECTION; dwFlags: $100),
    (  pguid: @GUID_RxAxis;
       dwOfs: DIJOFS_RX; dwType: $80000000 or DIDFT_AXIS or DIDFT_NOCOLLECTION; dwFlags: $100),
    (  pguid: @GUID_RyAxis;
       dwOfs: DIJOFS_RY; dwType: $80000000 or DIDFT_AXIS or DIDFT_NOCOLLECTION; dwFlags: $100),
    (  pguid: @GUID_RzAxis;
       dwOfs: DIJOFS_RZ; dwType: $80000000 or DIDFT_AXIS or DIDFT_NOCOLLECTION; dwFlags: $100),

    (  pguid: @GUID_Slider;  // 2 Sliders
       dwOfs: 24; dwType: $80000000 or DIDFT_AXIS or DIDFT_NOCOLLECTION; dwFlags: $100),
    (  pguid: @GUID_Slider;
       dwOfs: 28; dwType: $80000000 or DIDFT_AXIS or DIDFT_NOCOLLECTION; dwFlags: $100),

    (  pguid: @GUID_POV;  // 4 POVs (yes, really)
       dwOfs: 32; dwType: $80000000 or DIDFT_POV or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: @GUID_POV;
       dwOfs: 36; dwType: $80000000 or DIDFT_POV or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: @GUID_POV;
       dwOfs: 40; dwType: $80000000 or DIDFT_POV or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: @GUID_POV;
       dwOfs: 44; dwType: $80000000 or DIDFT_POV or DIDFT_NOCOLLECTION; dwFlags: 0),

    (  pguid: nil;   // Buttons
       dwOfs: DIJOFS_BUTTON0; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON1; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON2; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON3; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON4; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON5; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON6; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON7; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON8; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON9; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON10; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON11; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON12; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON13; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON14; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON15; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON16; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON17; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON18; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON19; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON20; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON21; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON22; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON23; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON24; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON25; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON26; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON27; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON28; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON29; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON30; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0),
    (  pguid: nil;
       dwOfs: DIJOFS_BUTTON31; dwType: $80000000 or DIDFT_BUTTON or DIDFT_NOCOLLECTION; dwFlags: 0)
  );

  c_dfDIJoystick: TDIDataFormat = (
    dwSize: Sizeof(c_dfDIJoystick);
    dwObjSize: Sizeof(TDIObjectDataFormat);  // $10
    dwFlags: DIDF_ABSAXIS;
    dwDataSize: SizeOf(TDIJoyState);         // $10
    dwNumObjs: High(_c_dfDIJoystick_Objects)+1;  // $2C
    rgodf: @_c_dfDIJoystick_Objects[Low(_c_dfDIJoystick_Objects)]
  );

var  // Set by initialization part -- didn't want to type in another 656 consts...
  _c_dfDIJoystick2_Objects: array[0..$A3] of TDIObjectDataFormat;
  { Elements $00..$2B: exact copy of _c_dfDIJoystick
    Elements $2C..$8B: more "buttons" with nil GUIDs
    remaining elements ($8B..$A2):
     $8C,$8D,$8E: X axis, Y axis, Z axis with dwFlags = $0200
     $8F,$90,$91: rX axis, rY axis, rZ axis with dwFlags = $0200
     $92, $93: Slider with dwFlags = $0200
     --------
     $94,$95,$96: X axis, Y axis, Z axis with dwFlags = $0300
     $97,$98,$99: rX axis, rY axis, rZ axis with dwFlags = $0300
     $9A,$9B: Slider with dwFlags = $0300
     --------
     $9C,$9D,$9E: X axis, Y axis, Z axis with dwFlags = $0400
     $9F, $A0, $A1: rX axis, rY axis, rZ axis with dwFlags = $0400
     $A2, $A3: Slider with dwFlags = $0400
  }
const
  c_dfDIJoystick2: TDIDataFormat = (
    dwSize: Sizeof(c_dfDIJoystick2);
    dwObjSize: Sizeof(TDIObjectDataFormat);
    dwFlags: DIDF_ABSAXIS;
    dwDataSize: SizeOf(TDIJoyState2);  // $110
    dwNumObjs: High(_c_dfDIJoystick2_Objects)+1;
    rgodf: @_c_dfDIJoystick2_Objects[Low(_c_dfDIJoystick2_Objects)]
  );

(****************************************************************************
 *
 *  IDirectInput
 *
 ****************************************************************************)


  DIENUM_STOP = 0;
  DIENUM_CONTINUE = 1;

type
  // as with the other enum functions: must rtn DIENUM_STOP or DIENUM_CONTINUE
  TDIEnumDevicesCallbackA = function (var lpddi: TDIDeviceInstanceA;
      pvRef: Pointer): Integer; stdcall;  // BOOL; stdcall;
  TDIEnumDevicesCallbackW = function (var lpddi: TDIDeviceInstanceW;
      pvRef: Pointer): Integer; stdcall;  // BOOL; stdcall;
  TDIEnumDevicesCallback = function (var lpddi: TDIDeviceInstance;
      pvRef: Pointer): Integer; stdcall; // BOOL; stdcall;
  TDIEnumDevicesProc = TDIEnumDevicesCallback;

const
  DIEDFL_ALLDEVICES       = $00000000;
  DIEDFL_ATTACHEDONLY     = $00000001;
  DIEDFL_FORCEFEEDBACK    = $00000100;

type

  IDirectInputW = interface (IUnknown)
    ['{89521361-AA8A-11CF-BFC7-444553540000}']
    (*** IDirectInputW methods ***)
    function CreateDevice(const rguid: TGUID; var lplpDirectInputDevice:
        IDirectInputDeviceW; pUnkOuter: IUnknown) : HResult;  stdcall;
    function EnumDevices(dwDevType: DWORD; lpCallback: TDIEnumDevicesCallbackW;
        pvRef: Pointer; dwFlags: DWORD) : HResult;  stdcall;
    function GetDeviceStatus(const rguidInstance: TGUID) : HResult;  stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD) : HResult;  stdcall;
    function Initialize(hinst: THandle; dwVersion: DWORD) : HResult;  stdcall;
  end;

  IDirectInputA = interface (IUnknown)
    ['{89521360-AA8A-11CF-BFC7-444553540000}']
    (*** IDirectInputA methods ***)
    function CreateDevice(const rguid: TGUID; var lplpDirectInputDevice:
        IDirectInputDeviceA; pUnkOuter: IUnknown) : HResult;  stdcall;
    function EnumDevices(dwDevType: DWORD; lpCallback: TDIEnumDevicesCallbackA;
        pvRef: Pointer; dwFlags: DWORD) : HResult;  stdcall;
    function GetDeviceStatus(const rguidInstance: TGUID) : HResult;  stdcall;
    function RunControlPanel(hwndOwner: HWND; dwFlags: DWORD) : HResult;  stdcall;
    function Initialize(hinst: THandle; dwVersion: DWORD) : HResult;  stdcall;
  end;

{$IFDEF UNICODE}
  IDirectInput = IDirectInputW;
{$ELSE}
  IDirectInput = IDirectInputA;
{$ENDIF}


  IDirectInput2W = interface (IDirectInputW)
    ['{5944E663-AA8A-11CF-BFC7-444553540000}']
    (*** IDirectInput2W methods ***)
    function FindDevice(const rguidClass: TGUID; ptszName: PWideChar; out pguidInstance: TGUID): HResult;  stdcall;
  end;

  IDirectInput2A = interface (IDirectInputA)
    ['{5944E662-AA8A-11CF-BFC7-444553540000}']
    (*** IDirectInput2A methods ***)
    function FindDevice(const rguidClass: TGUID; ptszName: PAnsiChar; out pguidInstance: TGUID): HResult;  stdcall;
  end;

{$IFDEF UNICODE}
  IDirectInput2 = IDirectInput2W;
{$ELSE}
  IDirectInput2 = IDirectInput2A;
{$ENDIF}


type
  IDirectInput7W = interface (IDirectInput2W)
    ['{9A4CB685-236D-11D3-8E9D-00C04F6844AE}']
    {*** IDirectInput7W methods ***}
    function CreateDeviceEx(const rguid, riid: TGUID; out lplpDirectInputDevice;
        pUnkOuter: IUnknown) : HResult; stdcall;
  end;

  IDirectInput7A = interface (IDirectInput2A)
    ['{9A4CB684-236D-11D3-8E9D-00C04F6844AE}']
    {*** IDirectInput7A methods ***}
    function CreateDeviceEx(const rguid, riid: TGUID; out lplpDirectInputDevice;
        pUnkOuter: IUnknown) : HResult; stdcall;
  end;

{$IFDEF UNICODE}
  IDirectInput7 = IDirectInput7W;
{$ELSE}
  IDirectInput7 = IDirectInput7A;
{$ENDIF}


var
  DirectInputCreateA : function (hinst: THandle; dwVersion: DWORD;
      out ppDI: IDirectInputA;
      punkOuter: IUnknown) : HResult; stdcall;
  DirectInputCreateW : function (hinst: THandle; dwVersion: DWORD;
      out ppDI: IDirectInputW;
      punkOuter: IUnknown) : HResult; stdcall;
  DirectInputCreate : function (hinst: THandle; dwVersion: DWORD;
      out ppDI: IDirectInput;
      punkOuter: IUnknown) : HResult; stdcall;

  DirectInputCreateEx : function (
      hinst: THandle;
      dwVersion: DWORD;
      const riidltf: TGUID;
      out ppvOut;
      punkOuter: IUnknown) : HResult; stdcall;

(****************************************************************************
 *
 *      Interfaces
 *
 ****************************************************************************)
type
  IID_IDirectInputW = IDirectInputW;
  IID_IDirectInputA = IDirectInputA;
  IID_IDirectInput = IDirectInput;

  IID_IDirectInput2W = IDirectInput2W;
  IID_IDirectInput2A = IDirectInput2A;
  IID_IDirectInput2 = IDirectInput2;

  IID_IDirectInput7W = IDirectInput7W;
  IID_IDirectInput7A = IDirectInput7A;
  IID_IDirectInput7 = IDirectInput7;

  IID_IDirectInputDeviceW = IDirectInputDeviceW;
  IID_IDirectInputDeviceA = IDirectInputDeviceA;
  IID_IDirectInputDevice = IDirectInputDevice;

  IID_IDirectInputDevice2W = IDirectInputDevice2W;
  IID_IDirectInputDevice2A = IDirectInputDevice2A;
  IID_IDirectInputDevice2 = IDirectInputDevice2;

  IID_IDirectInputEffect = IDirectInputEffect;

  IID_IDirectInputDevice7W = IDirectInputDevice7W;
  IID_IDirectInputDevice7A = IDirectInputDevice7A;
  IID_IDirectInputDevice7 = IDirectInputDevice7;

(****************************************************************************
 *
 *  Return Codes
 *
 ****************************************************************************)

(*
 *  The operation completed successfully.
 *)
const
  DI_OK = S_OK;

(*
 *  The device exists but is not currently attached.
 *)
  DI_NOTATTACHED = S_FALSE;

(*
 *  The device buffer overflowed.  Some input was lost.
 *)
  DI_BUFFEROVERFLOW = S_FALSE;

(*
 *  The change in device properties had no effect.
 *)
  DI_PROPNOEFFECT = S_FALSE;

(*
 *  The operation had no effect.
 *)
  DI_NOEFFECT = S_FALSE;

(*
 *  The device is a polled device.  As a result, device buffering
 *  will not collect any data and event notifications will not be
 *  signalled until GetDeviceState is called.
 *)
  DI_POLLEDDEVICE = $00000002;

(*
 *  The parameters of the effect were successfully updated by
 *  IDirectInputEffect::SetParameters, but the effect was not
 *  downloaded because the device is not exclusively acquired
 *  or because the DIEP_NODOWNLOAD flag was passed.
 *)
  DI_DOWNLOADSKIPPED = $00000003;

(*
 *  The parameters of the effect were successfully updated by
 *  IDirectInputEffect::SetParameters, but in order to change
 *  the parameters, the effect needed to be restarted.
 *)
  DI_EFFECTRESTARTED = $00000004;

(*
 *  The parameters of the effect were successfully updated by
 *  IDirectInputEffect::SetParameters, but some of them were
 *  beyond the capabilities of the device and were truncated.
 *)
  DI_TRUNCATED = $00000008;

(*
 *  Equal to DI_EFFECTRESTARTED | DI_TRUNCATED.
 *)
  DI_TRUNCATEDANDRESTARTED = $0000000C;

  SEVERITY_ERROR_FACILITY_WIN32 =
      HResult(SEVERITY_ERROR shl 31) or HResult(FACILITY_WIN32 shl 16);

(*
 *  The application requires a newer version of DirectInput.
 *)

  DIERR_OLDDIRECTINPUTVERSION = SEVERITY_ERROR_FACILITY_WIN32
      or ERROR_OLD_WIN_VERSION;

(*
 *  The application was written for an unsupported prerelease version
 *  of DirectInput.
 *)
  DIERR_BETADIRECTINPUTVERSION = SEVERITY_ERROR_FACILITY_WIN32
      or ERROR_RMODE_APP;

(*
 *  The object could not be created due to an incompatible driver version
 *  or mismatched or incomplete driver components.
 *)
  DIERR_BADDRIVERVER = SEVERITY_ERROR_FACILITY_WIN32
      or ERROR_BAD_DRIVER_LEVEL;

(*
 * The device or device instance or effect is not registered with DirectInput.
 *)
  DIERR_DEVICENOTREG = REGDB_E_CLASSNOTREG;

(*
 * The requested object does not exist.
 *)
  DIERR_NOTFOUND = SEVERITY_ERROR_FACILITY_WIN32
      or ERROR_FILE_NOT_FOUND;

(*
 * The requested object does not exist.
 *)
  DIERR_OBJECTNOTFOUND = SEVERITY_ERROR_FACILITY_WIN32
      or ERROR_FILE_NOT_FOUND;

(*
 * An invalid parameter was passed to the returning function,
 * or the object was not in a state that admitted the function
 * to be called.
 *)
  DIERR_INVALIDPARAM = E_INVALIDARG;

(*
 * The specified interface is not supported by the object
 *)
  DIERR_NOINTERFACE = E_NOINTERFACE;

(*
 * An undetermined error occured inside the DInput subsystem
 *)
  DIERR_GENERIC = E_FAIL;

(*
 * The DInput subsystem couldn't allocate sufficient memory to complete the
 * caller's request.
 *)
  DIERR_OUTOFMEMORY = E_OUTOFMEMORY;

(*
 * The function called is not supported at this time
 *)
  DIERR_UNSUPPORTED = E_NOTIMPL;

(*
 * This object has not been initialized
 *)
  DIERR_NOTINITIALIZED = SEVERITY_ERROR_FACILITY_WIN32
      or ERROR_NOT_READY;

(*
 * This object is already initialized
 *)
  DIERR_ALREADYINITIALIZED = SEVERITY_ERROR_FACILITY_WIN32
      or ERROR_ALREADY_INITIALIZED;

(*
 * This object does not support aggregation
 *)
  DIERR_NOAGGREGATION = CLASS_E_NOAGGREGATION;

(*
 * Another app has a higher priority level, preventing this call from
 * succeeding.
 *)
  DIERR_OTHERAPPHASPRIO = E_ACCESSDENIED;

(*
 * Access to the device has been lost.  It must be re-acquired.
 *)
  DIERR_INPUTLOST = SEVERITY_ERROR_FACILITY_WIN32
      or ERROR_READ_FAULT;

(*
 * The operation cannot be performed while the device is acquired.
 *)
  DIERR_ACQUIRED = SEVERITY_ERROR_FACILITY_WIN32
      or ERROR_BUSY;

(*
 * The operation cannot be performed unless the device is acquired.
 *)
  DIERR_NOTACQUIRED = SEVERITY_ERROR_FACILITY_WIN32
      or ERROR_INVALID_ACCESS;

(*
 * The specified property cannot be changed.
 *)
  DIERR_READONLY = E_ACCESSDENIED;

(*
 * The device already has an event notification associated with it.
 *)
  DIERR_HANDLEEXISTS = E_ACCESSDENIED;

(*
 * Data is not yet available.
 *)
  E_PENDING = HResult($80070007);

(*
 * Unable to IDirectInputJoyConfig_Acquire because the user
 * does not have sufficient privileges to change the joystick
 * configuration.
 *)
  DIERR_INSUFFICIENTPRIVS = HResult($80040200);

(*
 * The device is full.
 *)
  DIERR_DEVICEFULL = DIERR_INSUFFICIENTPRIVS + 1;

(*
 * Not all the requested information fit into the buffer.
 *)
  DIERR_MOREDATA = DIERR_INSUFFICIENTPRIVS + 2;

(*
 * The effect is not downloaded.
 *)
  DIERR_NOTDOWNLOADED = DIERR_INSUFFICIENTPRIVS + 3;

(*
 *  The device cannot be reinitialized because there are still effects
 *  attached to it.
 *)
  DIERR_HASEFFECTS = DIERR_INSUFFICIENTPRIVS + 4;

(*
 *  The operation cannot be performed unless the device is acquired
 *  in DISCL_EXCLUSIVE mode.
 *)
  DIERR_NOTEXCLUSIVEACQUIRED = DIERR_INSUFFICIENTPRIVS + 5;

(*
 *  The effect could not be downloaded because essential information
 *  is missing.  For example, no axes have been associated with the
 *  effect, or no type-specific information has been created.
 *)
  DIERR_INCOMPLETEEFFECT = DIERR_INSUFFICIENTPRIVS + 6;

(*
 *  Attempted to read buffered device data from a device that is
 *  not buffered.
 *)
  DIERR_NOTBUFFERED = DIERR_INSUFFICIENTPRIVS + 7;

(*
 *  An attempt was made to modify parameters of an effect while it is
 *  playing.  Not all hardware devices support altering the parameters
 *  of an effect while it is playing.
 *)
  DIERR_EFFECTPLAYING = DIERR_INSUFFICIENTPRIVS + 8;

(*
 *  The operation could not be completed because the device is not
 *  plugged in.
 *)
  DIERR_UNPLUGGED                = $80040209;

(*
 *  SendDeviceData failed because more information was requested
 *  to be sent than can be sent to the device.  Some devices have
 *  restrictions on how much data can be sent to them.  (For example,
 *  there might be a limit on the number of buttons that can be
 *  pressed at once.)
 *)
 DIERR_REPORTFULL                = $8004020A;


(****************************************************************************
 *
 *  Definitions for non-IDirectInput (VJoyD) features defined more recently
 *  than the current sdk files
 *
 ****************************************************************************)

(*
 * Flag to indicate that the dwReserved2 field of the JOYINFOEX structure
 * contains mini-driver specific data to be passed by VJoyD to the mini-
 * driver instead of doing a poll.
 *)
  JOY_PASSDRIVERDATA          = $10000000;

(*
 * Informs the joystick driver that the configuration has been changed
 * and should be reloaded from the registery.
 * dwFlags is reserved and should be set to zero
 *)

function joyConfigChanged(dwFlags: DWORD) : MMRESULT; stdcall;

const
(*
 * Hardware Setting indicating that the device is a headtracker
 *)
  JOY_HWS_ISHEADTRACKER       = $02000000;

(*
 * Hardware Setting indicating that the VxD is used to replace
 * the standard analog polling
 *)
  JOY_HWS_ISGAMEPORTDRIVER    = $04000000;

(*
 * Hardware Setting indicating that the driver needs a standard
 * gameport in order to communicate with the device.
 *)
  JOY_HWS_ISANALOGPORTDRIVER  = $08000000;

(*
 * Hardware Setting indicating that VJoyD should not load this
 * driver, it will be loaded externally and will register with
 * VJoyD of it's own accord.
 *)
  JOY_HWS_AUTOLOAD            = $10000000;

(*
 * Hardware Setting indicating that the driver acquires any 
 * resources needed without needing a devnode through VJoyD.
 *)
  JOY_HWS_NODEVNODE           = $20000000;

(*
 * Hardware Setting indicating that the device is a gameport bus
 *)
  JOY_HWS_ISGAMEPORTBUS       = $80000000;
  JOY_HWS_GAMEPORTBUSBUSY     = $00000001;

//from older Verion:
(*
 * Hardware Setting indicating that the VxD can be used as
 * a port 201h emulator.
 *)
  JOY_HWS_ISGAMEPORTEMULATOR  = $40000000;


(*
 * Usage Setting indicating that the settings are volatile and
 * should be removed if still present on a reboot.
 *)
  JOY_US_VOLATILE             = $00000008;

(****************************************************************************
 *
 *  Definitions for non-IDirectInput (VJoyD) features defined more recently
 *  than the current ddk files
 *
 ****************************************************************************)

(*
 * Poll type in which the do_other field of the JOYOEMPOLLDATA
 * structure contains mini-driver specific data passed from an app.
 *)
  JOY_OEMPOLL_PASSDRIVERDATA  = 7;

implementation

uses
  Forms,
  SysUtils;

function DIMAKEUSAGEDWORD(UsagePage, Usage: WORD) : DWORD;
begin
  Result := Usage or (UsagePage shl 16);
end;


function DIEFT_GETTYPE(n: variant) : byte;
begin
  Result := byte(n);
end;

function GET_DIDEVICE_TYPE(dwDevType: variant) : byte;
begin
  Result := byte(dwDevType);
end;

function GET_DIDEVICE_SUBTYPE(dwDevType: variant) : byte;
begin
  Result := hi(word(dwDevType));
end;

function DIDFT_MAKEINSTANCE(n: variant) : DWORD;
begin
  Result := word(n) shl 8;
end;

function DIDFT_GETTYPE(n: variant) : byte;
begin
  Result := byte(n);
end;

function DIDFT_GETINSTANCE(n: variant) : DWORD;
begin
  Result := word(n) shr 8;
end;

function DIDFT_ENUMCOLLECTION(n: variant) : DWORD;
begin
  Result := word(n) shl 8;
end;

function DIJOFS_SLIDER(n: variant) : variant;
begin
  Result := n * 4 + 24;
end;

function DIJOFS_POV(n: variant) : variant;
begin
  Result := n * 4 + 32;
end;

function DIJOFS_BUTTON(n: variant) : variant;
begin
  Result := 48 + n;
end;

function DIErrorString(Value: HResult) : string;
begin
  case Value of
    DI_OK: Result := 'The operation completed successfully.';
    S_FALSE: Result := '"The operation had no effect." or "The device buffer overflowed and some input was lost." or "The device exists but is not currently attached." or "The change in device properties had no effect."';
//    DI_BUFFEROVERFLOW: Result := 'The device buffer overflowed and some input was lost. This value is equal to the S_FALSE standard COM return value.';
    DI_DOWNLOADSKIPPED: Result := 'The parameters of the effect were successfully updated, but the effect could not be downloaded because the associated device was not acquired in exclusive mode.';
    DI_EFFECTRESTARTED: Result := 'The effect was stopped, the parameters were updated, and the effect was restarted.';
//    DI_NOEFFECT: Result := 'The operation had no effect. This value is equal to the S_FALSE standard COM return value.';
//    DI_NOTATTACHED: Result := 'The device exists but is not currently attached. This value is equal to the S_FALSE standard COM return value.';
    DI_POLLEDDEVICE: Result := 'The device is a polled device. As a result, device buffering will not collect any data and event notifications will not be signaled until the IDirectInputDevice2::Poll method is called.';
//    DI_PROPNOEFFECT: Result := 'The change in device properties had no effect. This value is equal to the S_FALSE standard COM return value.';
    DI_TRUNCATED: Result := 'The parameters of the effect were successfully updated, but some of them were beyond the capabilities of the device and were truncated to the nearest supported value.';
    DI_TRUNCATEDANDRESTARTED: Result := 'Equal to DI_EFFECTRESTARTED | DI_TRUNCATED.';
    DIERR_ACQUIRED: Result := 'The operation cannot be performed while the device is acquired.';
    DIERR_ALREADYINITIALIZED: Result := 'This object is already initialized';
    DIERR_BADDRIVERVER: Result := 'The object could not be created due to an incompatible driver version or mismatched or incomplete driver components.';
    DIERR_BETADIRECTINPUTVERSION: Result := 'The application was written for an unsupported prerelease version of DirectInput.';
    DIERR_DEVICEFULL: Result := 'The device is full.';
    DIERR_DEVICENOTREG: Result := 'The device or device instance is not registered with DirectInput. This value is equal to the REGDB_E_CLASSNOTREG standard COM return value.';
    DIERR_EFFECTPLAYING: Result := 'The parameters were updated in memory but were not downloaded to the device because the device does not support updating an effect while it is still playing.';
    DIERR_HASEFFECTS: Result := 'The device cannot be reinitialized because there are still effects attached to it.';
    DIERR_GENERIC: Result := 'An undetermined error occurred inside the DirectInput subsystem. This value is equal to the E_FAIL standard COM return value.';
//    DIERR_HANDLEEXISTS: Result := 'The device already has an event notification associated with it. This value is equal to the E_ACCESSDENIED standard COM return value.';
    DIERR_INCOMPLETEEFFECT: Result := 'The effect could not be downloaded because essential information is missing. For example, no axes have been associated with the effect, or no type-specific information has been supplied.';
    DIERR_INPUTLOST: Result := 'Access to the input device has been lost. It must be reacquired.';
    DIERR_INVALIDPARAM: Result := 'An invalid parameter was passed to the returning function, or the object was not in a state that permitted the function to be called. This value is equal to the E_INVALIDARG standard COM return value.';
    DIERR_MOREDATA: Result := 'Not all the requested information fitted into the buffer.';
    DIERR_NOAGGREGATION: Result := 'This object does not support aggregation.';
    DIERR_NOINTERFACE: Result := 'The specified interface is not supported by the object. This value is equal to the E_NOINTERFACE standard COM return value.';
    DIERR_NOTACQUIRED: Result := 'The operation cannot be performed unless the device is acquired.';
    DIERR_NOTBUFFERED: Result := 'The device is not buffered. Set the DIPROP_BUFFERSIZE property to enable buffering.';
    DIERR_NOTDOWNLOADED: Result := 'The effect is not downloaded.';
    DIERR_NOTEXCLUSIVEACQUIRED: Result := 'The operation cannot be performed unless the device is acquired in DISCL_EXCLUSIVE mode.';
    DIERR_NOTFOUND: Result := 'The requested object does not exist.';
    DIERR_NOTINITIALIZED: Result := 'This object has not been initialized.';
//    DIERR_OBJECTNOTFOUND: Result := 'The requested object does not exist.';
    DIERR_OLDDIRECTINPUTVERSION: Result := 'The application requires a newer version of DirectInput.';
    DIERR_OTHERAPPHASPRIO: Result := '"The device already has an event notification associated with it." or "The specified property cannot be changed." or "Another application has a higher priority level, preventing this call from succeeding. "';
    DIERR_OUTOFMEMORY: Result := 'The DirectInput subsystem could not allocate sufficient memory to complete the call. This value is equal to the E_OUTOFMEMORY standard COM return value.';
//    DIERR_READONLY: Result := 'The specified property cannot be changed. This value is equal to the E_ACCESSDENIED standard COM return value.';
    DIERR_UNSUPPORTED: Result := 'The function called is not supported at this time. This value is equal to the E_NOTIMPL standard COM return value.';
    E_PENDING: Result := 'Data is not yet available.';
    HResult($800405CC): Result := 'No more memory for effects of this kind (not documented)';
      else Result := Format('Unrecognized Error: $%x',[Value]);
  end;
end;

function joyConfigChanged(dwFlags: DWORD) : MMRESULT; external 'WinMM.dll';

function IsNTandDelphiRunning : boolean;
var
  OSVersion  : TOSVersionInfo;
begin
  OSVersion.dwOsVersionInfoSize := sizeof(OSVersion);
  GetVersionEx(OSVersion);
  // Not running in NT or program is not Delphi itself ?
  result := ( (OSVersion.dwPlatformID = VER_PLATFORM_WIN32_NT) and
     (Uppercase(ExtractFileName(Application.ExeName)) = 'DELPHI32.EXE') );
end;

procedure Init_c_dfDIKeyboard_Objects;  // XRef: Initialization
var x: Cardinal;
begin
  for x := 0 to 255 do
  with _c_dfDIKeyboard_Objects[x] do
  begin
    pGuid := @GUID_Key; dwOfs := x; dwFlags := 0;
    dwType := $80000000 or DIDFT_BUTTON or x shl 8;
  end;
end;

procedure Init_c_dfDIJoystick2_Objects;  // XRef: Initialization
var x,y, OfVal: Cardinal;
begin
  Move(_c_dfDIJoystick_Objects,_c_dfDIJoystick2_Objects,SizeOf(_c_dfDIJoystick_Objects));
  // all those empty "buttons"
  for x := $2C to $8B do
    Move(_c_dfDIJoystick_Objects[$2B],_c_dfDIJoystick2_Objects[x],SizeOf(TDIObjectDataFormat));
  for x := 0 to 2 do
  begin  // 3 more blocks of X axis..Sliders
    Move(_c_dfDIJoystick_Objects,_c_dfDIJoystick2_Objects[$8C+8*x],8*SizeOf(TDIObjectDataFormat));
    for y := 0 to 7 do _c_dfDIJoystick2_Objects[$8C+8*x+y].dwFlags := (x+1) shl 8;
  end;
  OfVal := _c_dfDIJoystick2_Objects[$2B].dwOfs+1;
  for x := $2C to $A3 do
  begin
    _c_dfDIJoystick2_Objects[x].dwOfs := OfVal;
    if x < $8C then Inc(OfVal) else Inc(OfVal,4);
  end;
end;


initialization
begin
  Init_c_dfDIKeyboard_Objects;  // set kbd GUIDs & flags
  Init_c_dfDIJoystick2_Objects;  // construct Joystick2 from Joystick fmt

  if not IsNTandDelphiRunning then
  begin
    DInputDLL := LoadLibrary('DInput.dll');

    DirectInputCreateA := GetProcAddress(DInputDLL,'DirectInputCreateA');
    DirectInputCreateW := GetProcAddress(DInputDLL,'DirectInputCreateW');
    // no A/W version
    DirectInputCreateEx := GetProcAddress(DInputDLL,'DirectInputCreateEx');
{$IFDEF UNICODE}
    DirectInputCreate := DirectInputCreateW;
{$ELSE}
    DirectInputCreate := DirectInputCreateA;
{$ENDIF}
  end;
end;


finalization
begin
  FreeLibrary(DInputDLL);
end;

end.



unit DirectPlay;

{$MODE Delphi}

(*==========================================================================;
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dplay.h dplobby.h
 *  Content:    DirectPlay include files
 *
 *  DirectX 7 Delphi adaptation by Erik Unger
 *
 *  Modified: 13-Jan-2000
 *
 *  Download: http://www.delphi-jedi.org/DelphiGraphics/
 *  E-Mail: Erik.Unger@gmx.at
 *
 ***************************************************************************)

interface

{$MINENUMSIZE 4}
{$ALIGN ON}

uses
  Windows;

var
  DPlayDLL : HMODULE = 0;

(*==========================================================================;
 *
 *  Copyright (C) 1994-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dplay.h
 *  Content:    DirectPlay include file
 *
 ***************************************************************************)

function DPErrorString(Value: HResult) : string;

type
{$IFDEF UNICODE}
  PCharAW = PWideChar;
{$ELSE}
  PCharAW = PAnsiChar;
{$ENDIF}

const
// {D1EB6D20-8923-11d0-9D97-00A0C90A43CB}
  CLSID_DirectPlay: TGUID =
      (D1:$d1eb6d20;D2:$8923;D3:$11d0;D4:($9d,$97,$00,$a0,$c9,$a,$43,$cb));

(*
 * GUIDS used by Service Providers shipped with DirectPlay
 * Use these to identify Service Provider returned by EnumConnections
 *)

// GUID for IPX service provider
// {685BC400-9D2C-11cf-A9CD-00AA006886E3}
  DPSPGUID_IPX: TGUID =
      (D1:$685bc400;D2:$9d2c;D3:$11cf;D4:($a9,$cd,$00,$aa,$00,$68,$86,$e3));

// GUID for TCP/IP service provider
// 36E95EE0-8577-11cf-960C-0080C7534E82
  DPSPGUID_TCPIP: TGUID =
      (D1:$36E95EE0;D2:$8577;D3:$11cf;D4:($96,$0c,$00,$80,$c7,$53,$4e,$82));

// GUID for Serial service provider
// {0F1D6860-88D9-11cf-9C4E-00A0C905425E}
  DPSPGUID_SERIAL: TGUID =
      (D1:$f1d6860;D2:$88d9;D3:$11cf;D4:($9c,$4e,$00,$a0,$c9,$05,$42,$5e));

// GUID for Modem service provider
// {44EAA760-CB68-11cf-9C4E-00A0C905425E}
  DPSPGUID_MODEM: TGUID =
      (D1:$44eaa760;D2:$cb68;D3:$11cf;D4:($9c,$4e,$00,$a0,$c9,$05,$42,$5e));


(****************************************************************************
 *
 * DirectPlay Structures
 *
 * Various structures used to invoke DirectPlay.
 *
 ****************************************************************************)

type
(*
 * TDPID
 * DirectPlay player and group ID
 *)
  TDPID = DWORD;
  PDPID = ^TDPID;


const
(*
 * DPID that system messages come from
 *)
  DPID_SYSMSG = 0;

(*
 * DPID representing all players in the session
 *)
  DPID_ALLPLAYERS = 0;

(*
 * DPID representing the server player
 *)
  DPID_SERVERPLAYER = 1;

(*
 * DPID representing the maximum ID in the range of DPID's reserved for
 * use by DirectPlay.
 *)
  DPID_RESERVEDRANGE = 100;

(*
 * The player ID is unknown (used with e.g. DPSESSION_NOMESSAGEID)
 *)
  DPID_UNKNOWN = $FFFFFFFF;

type
(*
 * DPCAPS
 * Used to obtain the capabilities of a DirectPlay object
 *)
  PDPCaps = ^TDPCaps;
  TDPCaps = packed record
    dwSize: DWORD;              // Size of structure, in bytes
    dwFlags: DWORD;             // DPCAPS_xxx flags
    dwMaxBufferSize: DWORD;     // Maximum message size, in bytes,  for this service provider
    dwMaxQueueSize: DWORD;      // Obsolete.
    dwMaxPlayers: DWORD;        // Maximum players/groups (local + remote)
    dwHundredBaud: DWORD;       // Bandwidth in 100 bits per second units;
                                // i.e. 24 is 2400, 96 is 9600, etc.
    dwLatency: DWORD;           // Estimated latency; 0 = unknown
    dwMaxLocalPlayers: DWORD;   // Maximum # of locally created players allowed
    dwHeaderLength: DWORD;      // Maximum header length, in bytes, on messages
                                // added by the service provider
    dwTimeout: DWORD;           // Service provider's suggested timeout value
                                // This is how long DirectPlay will wait for
                                // responses to system messages
  end;

const
(*
 * This DirectPlay object is the session host.  If the host exits the
 * session, another application will become the host and receive a
 * DPSYS_HOST system message.
 *)
  DPCAPS_ISHOST = $00000002;

(*
 * The service provider bound to this DirectPlay object can optimize
 * group messaging.
 *)
  DPCAPS_GROUPOPTIMIZED = $00000008;

(*
 * The service provider bound to this DirectPlay object can optimize
 * keep alives (see DPSESSION_KEEPALIVE)
 *)
  DPCAPS_KEEPALIVEOPTIMIZED = $00000010;

(*
 * The service provider bound to this DirectPlay object can optimize
 * guaranteed message delivery.
 *)
  DPCAPS_GUARANTEEDOPTIMIZED = $00000020;

(*
 * This DirectPlay object supports guaranteed message delivery.
 *)
  DPCAPS_GUARANTEEDSUPPORTED = $00000040;

(*
 * This DirectPlay object supports digital signing of messages.
 *)
  DPCAPS_SIGNINGSUPPORTED = $00000080;

(*
 * This DirectPlay object supports encryption of messages.
 *)
  DPCAPS_ENCRYPTIONSUPPORTED = $00000100;

(*
 * This DirectPlay player was created on this machine
 *)
  DPPLAYERCAPS_LOCAL = $00000800;

(*
 * Current Open settings supports all forms of Cancel
 *)
  DPCAPS_ASYNCCANCELSUPPORTED = $00001000;

(*
 * Current Open settings supports CancelAll, but not Cancel
 *)
  DPCAPS_ASYNCCANCELALLSUPPORTED = $00002000;

(*
 * Current Open settings supports Send Timeouts for sends
 *)
  DPCAPS_SENDTIMEOUTSUPPORTED = $00004000;

(*
 * Current Open settings supports send priority
 *)
  DPCAPS_SENDPRIORITYSUPPORTED = $00008000;

(*
 * Current Open settings supports DPSEND_ASYNC flag
 *)
  DPCAPS_ASYNCSUPPORTED = $00010000;

type
(*
 * TDPSessionDesc2
 * Used to describe the properties of a DirectPlay
 * session instance
 *)
  PDPSessionDesc2 = ^TDPSessionDesc2;
  TDPSessionDesc2 = packed record
    dwSize: DWORD;             // Size of structure
    dwFlags: DWORD;            // DPSESSION_xxx flags
    guidInstance: TGUID;       // ID for the session instance
    guidApplication: TGUID;    // GUID of the DirectPlay application.
                               // GUID_NULL for all applications.
    dwMaxPlayers: DWORD;       // Maximum # players allowed in session
    dwCurrentPlayers: DWORD;   // Current # players in session (read only)
    case integer of
      0 : (
    lpszSessionName: PCharAW;  // Name of the session
    lpszPassword: PCharAW;     // Password of the session (optional)
    dwReserved1: DWORD;        // Reserved for future MS use.
    dwReserved2: DWORD;
    dwUser1: DWORD;            // For use by the application
    dwUser2: DWORD;
    dwUser3: DWORD;
    dwUser4: DWORD;
      );
      1 : (
    lpszSessionNameA: PAnsiChar;   // Name of the session
    lpszPasswordA: PAnsiChar       // Password of the session (optional)
      );
      2 : (
    lpszSessionNameW: PWideChar;
    lpszPasswordW: PWideChar
      );
  end;

const
(*
 * Applications cannot create new players in this session.
 *)
  DPSESSION_NEWPLAYERSDISABLED = $00000001;

(*
 * If the DirectPlay object that created the session, the host,
 * quits, then the host will attempt to migrate to another
 * DirectPlay object so that new players can continue to be created
 * and new applications can join the session.
 *)
  DPSESSION_MIGRATEHOST = $00000004;

(*
 * This flag tells DirectPlay not to set the idPlayerTo and idPlayerFrom
 * fields in player messages.  This cuts two DWORD's off the message
 * overhead.
 *)
  DPSESSION_NOMESSAGEID = $00000008;

(*
 * This flag tells DirectPlay to not allow any new applications to
 * join the session.  Applications already in the session can still
 * create new players.
 *)
  DPSESSION_JOINDISABLED = $00000020;

(*
 * This flag tells DirectPlay to detect when remote players 
 * exit abnormally (e.g. their computer or modem gets unplugged)
 *)
  DPSESSION_KEEPALIVE = $00000040;

(*
 * This flag tells DirectPlay not to send a message to all players
 * when a players remote data changes
 *)
  DPSESSION_NODATAMESSAGES = $00000080;

(*
 * This flag indicates that the session belongs to a secure server
 * and needs user authentication
 *)
  DPSESSION_SECURESERVER = $00000100;

(*
 * This flag indicates that the session is private and requirs a password
 * for EnumSessions as well as Open.
 *)
  DPSESSION_PRIVATE = $00000200;

(*
 * This flag indicates that the session requires a password for joining.
 *)
  DPSESSION_PASSWORDREQUIRED = $00000400;

(*
 * This flag tells DirectPlay to route all messages through the server
 *)
  DPSESSION_MULTICASTSERVER = $00000800;

(*
 * This flag tells DirectPlay to only download information about the
 * DPPLAYER_SERVERPLAYER.
 *)
  DPSESSION_CLIENTSERVER = $00001000;

(*
 * This flag tells DirectPlay to use the protocol built into dplay
 * for reliability and statistics all the time.  When this bit is
 * set, only other sessions with this bit set can join or be joined.
 *)
  DPSESSION_DIRECTPLAYPROTOCOL = $00002000;

(*
 * This flag tells DirectPlay that preserving order of received
 * packets is not important, when using reliable delivery.  This
 * will allow messages to be indicated out of order if preceding
 * messages have not yet arrived.  Otherwise DPLAY will wait for
 * earlier messages before delivering later reliable messages.
 *)
  DPSESSION_NOPRESERVEORDER = $00004000;

  
(*
 * This flag tells DirectPlay to optimize communication for latency
 *)
  DPSESSION_OPTIMIZELATENCY = $00008000;

type
(*
 * TDPName
 * Used to hold the name of a DirectPlay entity
 * like a player or a group
 *)
  PDPName = ^TDPName;
  TDPName = packed record
    dwSize: DWORD;    // Size of structure
    dwFlags: DWORD;   // Not used. Must be zero.
    case Integer of
      0 : (
    lpszShortName : PCharAW; // The short or friendly name
    lpszLongName : PCharAW;  // The long or formal name
      );
      1 : (
    lpszShortNameA : PAnsiChar;
    lpszLongNameA : PAnsiChar;
      );
      2 : (
    lpszShortNameW : PWideChar;
    lpszLongNameW : PWideChar;
      );
  end;

(*
 * TDPCredentials
 * Used to hold the user name and password of a DirectPlay user
 *)

  PDPCredentials = ^TDPCredentials;
  TDPCredentials = packed record
    dwSize: DWORD;    // Size of structure
    dwFlags: DWORD;   // Not used. Must be zero.
    case Integer of
      0 : (
    lpszUsername: PCharAW;   // User name of the account
    lpszPassword: PCharAW;   // Password of the account
    lpszDomain:   PCharAW;   // Domain name of the account
      );
      1 : (
    lpszUsernameA: PAnsiChar;   // User name of the account
    lpszPasswordA: PAnsiChar;   // Password of the account
    lpszDomainA:   PAnsiChar;   // Domain name of the account
      );
      2 : (
    lpszUsernameW: PWideChar;   // User name of the account
    lpszPasswordW: PWideChar;   // Password of the account
    lpszDomainW:   PWideChar;   // Domain name of the account
      );
  end;

(*
 * TDPSecurityDesc
 * Used to describe the security properties of a DirectPlay
 * session instance
 *)
  PDPSecurityDesc = ^TDPSecurityDesc;
  TDPSecurityDesc = packed record
    dwSize: DWORD;                  // Size of structure
    dwFlags: DWORD;                 // Not used. Must be zero.
    case Integer of
      0 : (
    lpszSSPIProvider : PCharAW;  // SSPI provider name
    lpszCAPIProvider : PCharAW;  // CAPI provider name
    dwCAPIProviderType: DWORD;      // Crypto Service Provider type
    dwEncryptionAlgorithm: DWORD;   // Encryption Algorithm type
      );
      1 : (
    lpszSSPIProviderA : PAnsiChar;  // SSPI provider name
    lpszCAPIProviderA : PAnsiChar;  // CAPI provider name
      );
      2 : (
    lpszSSPIProviderW : PWideChar;  // SSPI provider name
    lpszCAPIProviderW : PWideChar;  // CAPI provider name
      );
  end;

(*
 * DPACCOUNTDESC
 * Used to describe a user membership account
 *)

  PDPAccountDesc = ^TDPAccountDesc;
  TDPAccountDesc = packed record
    dwSize: DWORD;    // Size of structure
    dwFlags: DWORD;   // Not used. Must be zero.
    case Integer of
      0 : (lpszAccountID : PCharAW);  // Account identifier
      1 : (lpszAccountIDA : PAnsiChar); 
      2 : (lpszAccountIDW : PWideChar);
  end;

(*
 * TDPLConnection
 * Used to hold all in the informaion needed to connect
 * an application to a session or create a session
 *)
  PDPLConnection = ^TDPLConnection;
  TDPLConnection = packed record
    dwSize: DWORD;                     // Size of this structure
    dwFlags: DWORD;                    // Flags specific to this structure
    lpSessionDesc: PDPSessionDesc2;    // Pointer to session desc to use on connect
    lpPlayerName: PDPName;             // Pointer to Player name structure
    guidSP: TGUID;                     // GUID of the DPlay SP to use
    lpAddress: Pointer;                // Address for service provider
    dwAddressSize: DWORD;              // Size of address data
  end;

(*
 * TDPChat
 * Used to hold the a DirectPlay chat message
 *)
  PDPChat = ^TDPChat;
  TDPChat = packed record
    dwSize: DWORD;
    dwFlags: DWORD;
    case Integer of
      0 : (lpszMessage : PCharAW);  // Message string
      1 : (lpszMessageA : PAnsiChar);
      2 : (lpszMessageW : PWideChar);
  end;

(*
 * TSGBuffer
 * Scatter Gather Buffer used for SendEx
 *)
  PSGBuffer = ^TSGBuffer;
  TSGBuffer = packed record
    len: UINT;
    pData: PUCHAR;
  end;

(****************************************************************************
 *
 * Prototypes for DirectPlay callback functions
 *
 ****************************************************************************)

(*
 * Callback for IDirectPlay2::EnumSessions
 *)
  TDPEnumSessionsCallback2 = function(lpThisSD: PDPSessionDesc2;
      var lpdwTimeOut: DWORD; dwFlags: DWORD; lpContext: Pointer) : BOOL; stdcall;

const
(*
 * This flag is set on the EnumSessions callback dwFlags parameter when
 * the time out has occurred. There will be no session data for this
 * callback. If *lpdwTimeOut is set to a non-zero value and the
 * EnumSessionsCallback function returns TRUE then EnumSessions will
 * continue waiting until the next timeout occurs. Timeouts are in
 * milliseconds.
 *)
  DPESC_TIMEDOUT = $00000001;

type
(*
 * Callback for IDirectPlay2.EnumPlayers
 *              IDirectPlay2.EnumGroups
 *              IDirectPlay2.EnumGroupPlayers
 *)
  TDPEnumPlayersCallback2 = function(DPID: TDPID; dwPlayerType: DWORD;
      const lpName: TDPName; dwFlags: DWORD; lpContext: Pointer) : BOOL; stdcall;


(*
 * ANSI callback for DirectPlayEnumerate
 * This callback prototype will be used if compiling
 * for ANSI strings
 *)
  TDPEnumDPCallbackA = function(const lpguidSP: TGUID; lpSPName: PAnsiChar;
      dwMajorVersion: DWORD; dwMinorVersion: DWORD; lpContext: Pointer) : BOOL; stdcall;

(*
 * Unicode callback for DirectPlayEnumerate
 * This callback prototype will be used if compiling
 * for Unicode strings
 *)
  TDPEnumDPCallbackW = function(const lpguidSP: TGUID; lpSPName: PWideChar;
      dwMajorVersion: DWORD; dwMinorVersion: DWORD; lpContext: Pointer) : BOOL; stdcall;

(*
 * Callback for DirectPlayEnumerate
 *)
{$IFDEF UNICODE}
  TDPEnumDPCallback = TDPEnumDPCallbackW;
{$ELSE}
  TDPEnumDPCallback = TDPEnumDPCallbackA;
{$ENDIF}

(*
 * Callback for IDirectPlay3(A/W).EnumConnections
 *)
  TDPEnumConnectionsCallback = function(const lpguidSP: TGUID;
      lpConnection: Pointer; dwConnectionSize: DWORD; const lpName: TDPName;
      dwFlags: DWORD; lpContext: Pointer) : BOOL; stdcall;

(*
 * API's
 *)

var
  DirectPlayEnumerate : function (lpEnumDPCallback: TDPEnumDPCallback;
      lpContext: Pointer) : HResult; stdcall;
  DirectPlayEnumerateA : function (lpEnumDPCallback: TDPEnumDPCallbackA;
      lpContext: Pointer) : HResult; stdcall;
  DirectPlayEnumerateW : function (lpEnumDPCallback: TDPEnumDPCallbackW;
      lpContext: Pointer) : HResult; stdcall;


(****************************************************************************
 *
 * IDirectPlay2 (and IDirectPlay2A) Interface
 *
 ****************************************************************************)

type
  IDirectPlay2AW = interface (IUnknown)
    (*** IDirectPlay2 methods ***)
    function AddPlayerToGroup(idGroup: TDPID; idPlayer: TDPID) : HResult; stdcall;
    function Close: HResult; stdcall;
    function CreateGroup(out lpidGroup: TDPID; lpGroupName: PDPName;
        lpData: Pointer; dwDataSize: DWORD; dwFlags: DWORD) : HResult; stdcall;
    function CreatePlayer(out lpidPlayer: TDPID; pPlayerName: PDPName;
        hEvent: THandle; lpData: Pointer; dwDataSize: DWORD; dwFlags: DWORD) : HResult; stdcall;
    function DeletePlayerFromGroup(idGroup: TDPID; idPlayer: TDPID) : HResult; stdcall;
    function DestroyGroup(idGroup: TDPID) : HResult; stdcall;
    function DestroyPlayer(idPlayer: TDPID) : HResult; stdcall;
    function EnumGroupPlayers(idGroup: TDPID; lpguidInstance: PGUID;
        lpEnumPlayersCallback2: TDPEnumPlayersCallback2; lpContext: Pointer;
        dwFlags: DWORD) : HResult; stdcall;
    function EnumGroups(lpguidInstance: PGUID; lpEnumPlayersCallback2:
        TDPEnumPlayersCallback2; lpContext: Pointer; dwFlags: DWORD) : HResult; stdcall;
    function EnumPlayers(lpguidInstance: PGUID; lpEnumPlayersCallback2:
        TDPEnumPlayersCallback2; lpContext: Pointer; dwFlags: DWORD) : HResult; stdcall;
    function EnumSessions(const lpsd: TDPSessionDesc2; dwTimeout: DWORD;
        lpEnumSessionsCallback2: TDPEnumSessionsCallback2; lpContext: Pointer;
        dwFlags: DWORD) : HResult; stdcall;
    function GetCaps(var lpDPCaps: TDPCaps; dwFlags: DWORD) : HResult; stdcall;
    function GetGroupData(idGroup: TDPID; lpData: Pointer; var lpdwDataSize: DWORD;
        dwFlags: DWORD) : HResult; stdcall;
    function GetGroupName(idGroup: TDPID; lpData: Pointer; var lpdwDataSize: DWORD) :
        HResult; stdcall;
    function GetMessageCount(idPlayer: TDPID; var lpdwCount: DWORD) : HResult; stdcall;
    function GetPlayerAddress(idPlayer: TDPID; lpAddress: Pointer;
        var lpdwAddressSize: DWORD) : HResult; stdcall;
    function GetPlayerCaps(idPlayer: TDPID; var lpPlayerCaps: TDPCaps;
        dwFlags: DWORD) : HResult; stdcall;
    function GetPlayerData(idPlayer: TDPID; lpData: Pointer; var lpdwDataSize: DWORD;
        dwFlags: DWORD) : HResult; stdcall;
    function GetPlayerName(idPlayer: TDPID; lpData: Pointer; var lpdwDataSize: DWORD) : HResult; stdcall;
    function GetSessionDesc(lpData: Pointer; var lpdwDataSize: DWORD) : HResult; stdcall;
    function Initialize(const lpGUID: TGUID) : HResult; stdcall;
    function Open(var lpsd: TDPSessionDesc2; dwFlags: DWORD) : HResult; stdcall;
    function Receive(var lpidFrom: TDPID; var lpidTo: TDPID; dwFlags: DWORD;
        lpData: Pointer; var lpdwDataSize: DWORD) : HResult; stdcall;
    function Send(idFrom: TDPID; lpidTo: TDPID; dwFlags: DWORD; var lpData;
        lpdwDataSize: DWORD) : HResult; stdcall;
    function SetGroupData(idGroup: TDPID; lpData: Pointer; dwDataSize: DWORD;
        dwFlags: DWORD) : HResult; stdcall;
    function SetGroupName(idGroup: TDPID; lpGroupName: PDPName;
        dwFlags: DWORD) : HResult; stdcall;
    function SetPlayerData(idPlayer: TDPID; lpData: Pointer; dwDataSize: DWORD;
        dwFlags: DWORD) : HResult; stdcall;
    function SetPlayerName(idPlayer: TDPID; lpPlayerName: PDPName;
        dwFlags: DWORD) : HResult; stdcall;
    function SetSessionDesc(var lpSessDesc: TDPSessionDesc2; dwFlags: DWORD) :
        HResult; stdcall;
  end;

  IDirectPlay2W = interface (IDirectPlay2AW)
    ['{2B74F7C0-9154-11CF-A9CD-00AA006886E3}']
  end;
  IDirectPlay2A = interface (IDirectPlay2AW)
    ['{9d460580-a822-11cf-960c-0080c7534e82}']
  end;

{$IFDEF UNICODE}
  IDirectPlay2 = IDirectPlay2W;
{$ELSE}
  IDirectPlay2 = IDirectPlay2A;
{$ENDIF}

(****************************************************************************
 *
 * IDirectPlay3 (and IDirectPlay3A) Interface
 *
 ****************************************************************************)

  IDirectPlay3AW = interface (IDirectPlay2AW)
    (*** IDirectPlay3 methods ***)
    function AddGroupToGroup(idParentGroup: TDPID; idGroup: TDPID) : HResult; stdcall;
    function CreateGroupInGroup(idParentGroup: TDPID; var lpidGroup: TDPID;
        lpGroupName: PDPName; lpData: Pointer; dwDataSize: DWORD;
        dwFlags: DWORD) : HResult; stdcall;
    function DeleteGroupFromGroup(idParentGroup: TDPID; idGroup: TDPID) : HResult; stdcall;
    function EnumConnections(lpguidApplication: PGUID;
        lpEnumCallback: TDPEnumConnectionsCallback; lpContext: Pointer;
        dwFlags: DWORD) : HResult; stdcall;
    function EnumGroupsInGroup(idGroup: TDPID; lpguidInstance: PGUID;
        lpEnumPlayersCallback2: TDPEnumPlayersCallback2; lpContext: Pointer;
        dwFlags: DWORD) : HResult; stdcall;
    function GetGroupConnectionSettings(dwFlags: DWORD; idGroup: TDPID;
        lpData: Pointer; var lpdwDataSize: DWORD) : HResult; stdcall;
    function InitializeConnection(lpConnection: Pointer; dwFlags: DWORD) : HResult; stdcall;
    function SecureOpen(var lpsd: TDPSessionDesc2; dwFlags: DWORD;
        var lpSecurity: TDPSecurityDesc; var lpCredentials: TDPCredentials) : HResult; stdcall;
    function SendChatMessage(idFrom: TDPID; idTo: TDPID; dwFlags: DWORD;
        var lpChatMessage: TDPChat) : HResult; stdcall;
    function SetGroupConnectionSettings(dwFlags: DWORD; idGroup: TDPID;
        var lpConnection: TDPLConnection) : HResult; stdcall;
    function StartSession(dwFlags: DWORD; idGroup: TDPID) : HResult; stdcall;
    function GetGroupFlags(idGroup: TDPID; out lpdwFlags: DWORD) : HResult; stdcall;
    function GetGroupParent(idGroup: TDPID; out lpidParent: TDPID) : HResult; stdcall;
    function GetPlayerAccount(idPlayer: TDPID; dwFlags: DWORD; var lpData;
        var lpdwDataSize: DWORD) : HResult; stdcall;
    function GetPlayerFlags(idPlayer: TDPID; out lpdwFlags: DWORD) : HResult; stdcall;
  end;


  IDirectPlay3W = interface (IDirectPlay3AW)
    ['{133EFE40-32DC-11D0-9CFB-00A0C90A43CB}']
  end;
  IDirectPlay3A = interface (IDirectPlay3AW)
    ['{133efe41-32dc-11d0-9cfb-00a0c90a43cb}']
  end;

{$IFDEF UNICODE}
  IDirectPlay3 = IDirectPlay3W;
{$ELSE}
  IDirectPlay3 = IDirectPlay3A;
{$ENDIF}


(****************************************************************************
 *
 * IDirectPlay4 (and IDirectPlay4A) Interface
 *
 ****************************************************************************)

  IDirectPlay4AW = interface (IDirectPlay3AW)
    (*** IDirectPlay4 methods ***)
    function GetGroupOwner(idGroup: TDPID; out idOwner: TDPID) : HResult; stdcall;
    function SetGroupOwner(idGroup: TDPID; idOwner: TDPID) : HResult; stdcall;
    function SendEx(idFrom: TDPID; idTo: TDPID; dwFlags: DWORD; lpData: Pointer;
        dwDataSize: DWORD; dwPriority: DWORD; dwTimeout: DWORD;
        lpContext: Pointer; lpdwMsgId: PDWORD) : HResult; stdcall;
    function GetMessageQueue(idFrom: TDPID; idTo: TDPID; dwFlags: DWORD;
        lpdwNumMsgs: PDWORD; lpdwNumBytes: PDWORD) : HResult; stdcall;
    function CancelMessage(dwMessageID: DWORD; dwFlags: DWORD) : HResult; stdcall;
    function CancelPriority(dwMinPriority: DWORD; dwMaxPriority: DWORD; dwFlags: DWORD) : HResult; stdcall;
  end;


  IDirectPlay4W = interface (IDirectPlay4AW)
    ['{0ab1c530-4745-11D1-a7a1-0000f803abfc}']
  end;
  IDirectPlay4A = interface (IDirectPlay4AW)
    ['{0ab1c531-4745-11D1-a7a1-0000f803abfc}']
  end;

{$IFDEF UNICODE}
  IDirectPlay4 = IDirectPlay4W;
{$ELSE}
  IDirectPlay4 = IDirectPlay4A;
{$ENDIF}


const
(****************************************************************************
 *
 * EnumConnections API flags
 *
 ****************************************************************************)

(*
 * Enumerate Service Providers
 *)
  DPCONNECTION_DIRECTPLAY = $00000001;

(*
 * Enumerate Lobby Providers
 *)
  DPCONNECTION_DIRECTPLAYLOBBY = $00000002;

(****************************************************************************
 *
 * EnumPlayers API flags
 *
 ****************************************************************************)

(*
 * Enumerate all players in the current session
 *)
  DPENUMPLAYERS_ALL = $00000000;
  DPENUMGROUPS_ALL = DPENUMPLAYERS_ALL;

(*
 * Enumerate only local (created by this application) players
 * or groups
 *)
  DPENUMPLAYERS_LOCAL = $00000008;
  DPENUMGROUPS_LOCAL = DPENUMPLAYERS_LOCAL;

(*
 * Enumerate only remote (non-local) players
 * or groups
 *)
  DPENUMPLAYERS_REMOTE = $00000010;
  DPENUMGROUPS_REMOTE = DPENUMPLAYERS_REMOTE;

(*
 * Enumerate groups along with the players
 *)
  DPENUMPLAYERS_GROUP = $00000020;

(*
 * Enumerate players or groups in another session 
 * (must supply lpguidInstance)
 *)
  DPENUMPLAYERS_SESSION = $00000080;
  DPENUMGROUPS_SESSION = DPENUMPLAYERS_SESSION;

(*
 * Enumerate server players
 *)
  DPENUMPLAYERS_SERVERPLAYER = $00000100;

(*
 * Enumerate spectator players
 *)
  DPENUMPLAYERS_SPECTATOR = $00000200;

(*
 * Enumerate shortcut groups
 *)
  DPENUMGROUPS_SHORTCUT = $00000400;

(*
 * Enumerate staging area groups
 *)
  DPENUMGROUPS_STAGINGAREA = $00000800;

(*
 * Enumerate hidden groups
 *)
  DPENUMGROUPS_HIDDEN = $00001000;

(*
 * Enumerate the group's owner
 *)
  DPENUMPLAYERS_OWNER = $00002000;

(****************************************************************************
 *
 * CreatePlayer API flags
 *
 ****************************************************************************)

(*
 * This flag indicates that this player should be designated
 * the server player. The app should specify this at CreatePlayer.
 *)
  DPPLAYER_SERVERPLAYER = DPENUMPLAYERS_SERVERPLAYER;

(*
 * This flag indicates that this player should be designated
 * a spectator. The app should specify this at CreatePlayer.
 *)
  DPPLAYER_SPECTATOR = DPENUMPLAYERS_SPECTATOR;

(*
 * This flag indicates that this player was created locally.
 * (returned from GetPlayerFlags)
 *)
  DPPLAYER_LOCAL = DPENUMPLAYERS_LOCAL;

(*
 * This flag indicates that this player is the group's owner
 * (Only returned in EnumGroupPlayers)
 *)
  DPPLAYER_OWNER = DPENUMPLAYERS_OWNER;

(****************************************************************************
 *
 * CreateGroup API flags
 *
 ****************************************************************************)

(*
 * This flag indicates that the StartSession can be called on the group.
 * The app should specify this at CreateGroup, or CreateGroupInGroup.
 *)
  DPGROUP_STAGINGAREA = DPENUMGROUPS_STAGINGAREA;

(*
 * This flag indicates that this group was created locally.
 * (returned from GetGroupFlags)
 *)
  DPGROUP_LOCAL = DPENUMGROUPS_LOCAL;

(*
 * This flag indicates that this group was created hidden.
 *)
  DPGROUP_HIDDEN = DPENUMGROUPS_HIDDEN;

(****************************************************************************
 *
 * EnumSessions API flags
 *
 ****************************************************************************)

(*
 * Enumerate sessions which can be joined
 *)
  DPENUMSESSIONS_AVAILABLE = $00000001;

(*
 * Enumerate all sessions even if they can't be joined.
 *)
  DPENUMSESSIONS_ALL = $00000002;

(*
 * Start an asynchronous enum sessions
 *)
  DPENUMSESSIONS_ASYNC = $00000010;

(*
 * Stop an asynchronous enum sessions
 *)
  DPENUMSESSIONS_STOPASYNC = $00000020;

(*
 * Enumerate sessions even if they require a password
 *)
  DPENUMSESSIONS_PASSWORDREQUIRED = $00000040;

(*
 * Return status about progress of enumeration instead of
 * showing any status dialogs.
 *)
  DPENUMSESSIONS_RETURNSTATUS = $00000080;

(****************************************************************************
 *
 * GetCaps and GetPlayerCaps API flags
 *
 ****************************************************************************)

(*
 * The latency returned should be for guaranteed message sending.
 * Default is non-guaranteed messaging.
 *)
  DPGETCAPS_GUARANTEED = $00000001;

(****************************************************************************
 *
 * GetGroupData, GetPlayerData API flags
 * Remote and local Group/Player data is maintained separately.
 * Default is DPGET_REMOTE.
 *
 ****************************************************************************)

(*
 * Get the remote data (set by any DirectPlay object in
 * the session using DPSET_REMOTE)
 *)
  DPGET_REMOTE = $00000000;

(*
 * Get the local data (set by this DirectPlay object 
 * using DPSET_LOCAL)
 *)
  DPGET_LOCAL = $00000001;

(****************************************************************************
 *
 * Open API flags
 *
 ****************************************************************************)

(*
 * Join the session that is described by the DPSESSIONDESC2 structure
 *)
  DPOPEN_JOIN = $00000001;

(*
 * Create a new session as described by the DPSESSIONDESC2 structure
 *)
  DPOPEN_CREATE = $00000002;

(*
 * Return status about progress of open instead of showing
 * any status dialogs.
 *)
  DPOPEN_RETURNSTATUS = DPENUMSESSIONS_RETURNSTATUS;

(****************************************************************************
 *
 * DPLCONNECTION flags
 *
 ****************************************************************************)

(*
 * This application should create a new session as
 * described by the DPSESIONDESC structure
 *)
  DPLCONNECTION_CREATESESSION = DPOPEN_CREATE;

(*
 * This application should join the session described by
 * the DPSESIONDESC structure with the lpAddress data
 *)
  DPLCONNECTION_JOINSESSION = DPOPEN_JOIN;

(****************************************************************************
 *
 * Receive API flags
 * Default is DPRECEIVE_ALL
 *
 ****************************************************************************)

(*
 * Get the first message in the queue
 *)
  DPRECEIVE_ALL = $00000001;

(*
 * Get the first message in the queue directed to a specific player 
 *)
  DPRECEIVE_TOPLAYER = $00000002;

(*
 * Get the first message in the queue from a specific player
 *)
  DPRECEIVE_FROMPLAYER = $00000004;

(*
 * Get the message but don't remove it from the queue
 *)
  DPRECEIVE_PEEK = $00000008;

(****************************************************************************
 *
 * Send API flags
 *
 ****************************************************************************)

(*
 * Send the message using a guaranteed send method.
 * Default is non-guaranteed.
 *)
  DPSEND_GUARANTEED = $00000001;

(*
 * This flag is obsolete. It is ignored by DirectPlay
 *)
  DPSEND_HIGHPRIORITY = $00000002;

(*
 * This flag is obsolete. It is ignored by DirectPlay
 *)
  DPSEND_OPENSTREAM = $00000008;

(*
 * This flag is obsolete. It is ignored by DirectPlay
 *)
  DPSEND_CLOSESTREAM = $00000010;

(*
 * Send the message digitally signed to ensure authenticity.
 *)
  DPSEND_SIGNED = $00000020;

(*
 * Send the message with encryption to ensure privacy.
 *)
  DPSEND_ENCRYPTED = $00000040;

(*
 * The message is a lobby system message
 *)
  DPSEND_LOBBYSYSTEMMESSAGE = $00000080;

(*
 * andyco - added this so we can make addforward async.
 * needs to be sanitized when we add / expose full async
 * support.  8/3/97.
 *)
  DPSEND_ASYNC = $00000200;

(*
 * When a message is completed, don't tell me.
 * by default the application is notified with a system message.
 *)
  DPSEND_NOSENDCOMPLETEMSG = $00000400;


(*
 * Maximum priority for sends available to applications
 *)
  DPSEND_MAX_PRI = $0000FFFF;
  DPSEND_MAX_PRIORITY = DPSEND_MAX_PRI;

(****************************************************************************
 *
 * SetGroupData, SetGroupName, SetPlayerData, SetPlayerName,
 * SetSessionDesc API flags.
 * Default is DPSET_REMOTE.
 *
 ****************************************************************************)

(* 
 * Propagate the data to all players in the session
 *)
  DPSET_REMOTE = $00000000;

(*
 * Do not propagate the data to other players
 *)
  DPSET_LOCAL = $00000001;

(*
 * Used with DPSET_REMOTE, use guaranteed message send to
 * propagate the data
 *)
  DPSET_GUARANTEED = $00000002;

(****************************************************************************
 *
 * GetMessageQueue API flags.
 * Default is DPMESSAGEQUEUE_SEND
 *
 ****************************************************************************)

(*
 * Get Send Queue - requires Service Provider Support
 *)
  DPMESSAGEQUEUE_SEND = $00000001;

(*
 * Get Receive Queue
 *)
  DPMESSAGEQUEUE_RECEIVE = $00000002;

(****************************************************************************
 *
 * Connect API flags
 *
 ****************************************************************************)

(*
 * Start an asynchronous connect which returns status codes
 *)
  DPCONNECT_RETURNSTATUS = DPENUMSESSIONS_RETURNSTATUS;

(****************************************************************************
 *
 * DirectPlay system messages and message data structures
 *
 * All system message come 'From' player DPID_SYSMSG.  To determine what type 
 * of message it is, cast the lpData from Receive to TDPMsg_Generic and check
 * the dwType member against one of the following DPSYS_xxx constants. Once
 * a match is found, cast the lpData to the corresponding of the DPMSG_xxx
 * structures to access the data of the message.
 *
 ****************************************************************************)

(*
 * A new player or group has been created in the session
 * Use TDPMsg_CreatePlayerOrGroup.  Check dwPlayerType to see if it
 * is a player or a group.
 *)
  DPSYS_CREATEPLAYERORGROUP = $0003;

(*
 * A player has been deleted from the session
 * Use TDPMsg_DestroyPlayerOrGroup
 *)
  DPSYS_DESTROYPLAYERORGROUP = $0005;

(*
 * A player has been added to a group
 * Use DPMSG_ADDPLAYERTOGROUP
 *)
  DPSYS_ADDPLAYERTOGROUP = $0007;

(*
 * A player has been removed from a group
 * Use DPMSG_DELETEPLAYERFROMGROUP
 *)
  DPSYS_DELETEPLAYERFROMGROUP = $0021;

(*
 * This DirectPlay object lost its connection with all the
 * other players in the session.
 * Use DPMSG_SESSIONLOST.
 *)
  DPSYS_SESSIONLOST = $0031;

(*
 * The current host has left the session.
 * This DirectPlay object is now the host.
 * Use DPMSG_HOST.
 *)
  DPSYS_HOST = $0101;

(*
 * The remote data associated with a player or
 * group has changed. Check dwPlayerType to see
 * if it is a player or a group
 * Use DPMSG_SETPLAYERORGROUPDATA
 *)
  DPSYS_SETPLAYERORGROUPDATA = $0102;

(*
 * The name of a player or group has changed.
 * Check dwPlayerType to see if it is a player
 * or a group.
 * Use TDPMsg_SetPlayerOrGroupName
 *)
  DPSYS_SETPLAYERORGROUPNAME = $0103;

(*
 * The session description has changed.
 * Use DPMSG_SETSESSIONDESC
 *)
  DPSYS_SETSESSIONDESC = $0104;

(*
 * A group has been added to a group
 * Use TDPMsg_AddGroupToGroup
 *)
  DPSYS_ADDGROUPTOGROUP = $0105;

(*
 * A group has been removed from a group
 * Use DPMsg_DeleteGroupFromGroup
 *)
  DPSYS_DELETEGROUPFROMGROUP = $0106;

(*
 * A secure player-player message has arrived.
 * Use DPMSG_SECUREMESSAGE
 *)
  DPSYS_SECUREMESSAGE = $0107;

(*
 * Start a new session.
 * Use DPMSG_STARTSESSION
 *)
  DPSYS_STARTSESSION = $0108;

(*
 * A chat message has arrived
 * Use DPMSG_CHAT
 *)
  DPSYS_CHAT = $0109;

(*
 * The owner of a group has changed
 * Use DPMSG_SETGROUPOWNER
 *)
  DPSYS_SETGROUPOWNER = $010A;

(*
 * An async send has finished, failed or been cancelled
 * Use DPMSG_SENDCOMPLETE
 *)
  DPSYS_SENDCOMPLETE = $010D;

(*
 * Used in the dwPlayerType field to indicate if it applies to a group
 * or a player
 *)
  DPPLAYERTYPE_GROUP = $00000000;
  DPPLAYERTYPE_PLAYER = $00000001;

type
(*
 * TDPMsg_Generic
 * Generic message structure used to identify the message type.
 *)
  PDPMsg_Generic = ^TDPMsg_Generic;
  TDPMsg_Generic = packed record
    dwType: DWORD;   // Message type
  end;

(*
 * TDPMsg_CreatePlayerOrGroup
 * System message generated when a new player or group
 * created in the session with information about it.
 *)
  PDPMsg_CreatePlayerOrGroup = ^TDPMsg_CreatePlayerOrGroup;
  TDPMsg_CreatePlayerOrGroup = packed record
    dwType: DWORD;             // Message type
    dwPlayerType: DWORD;       // Is it a player or group
    DPID: TDPID;               // ID of the player or group
    dwCurrentPlayers: DWORD;   // current # players & groups in session
    lpData: Pointer;           // pointer to remote data
    dwDataSize: DWORD;         // size of remote data
    dpnName: TDPName;           // structure with name info
                               // the following fields are only available when using
                               // the IDirectPlay3 interface or greater
    dpIdParent: TDPID;         // id of parent group
    dwFlags: DWORD;            // player or group flags
  end;

(*
 * TDPMsg_DestroyPlayerOrGroup
 * System message generated when a player or group is being
 * destroyed in the session with information about it.
 *)
  PDPMsg_DestroyPlayerOrGroup= ^TDPMsg_DestroyPlayerOrGroup;
  TDPMsg_DestroyPlayerOrGroup = packed record
    dwType: DWORD;             // Message type
    dwPlayerType: DWORD;       // Is it a player or group
    DPID: TDPID;                // player ID being deleted
    lpLocalData: Pointer;      // copy of players local data
    dwLocalDataSize: DWORD;    // sizeof local data
    lpRemoteData: Pointer;     // copy of players remote data
    dwRemoteDataSize: DWORD;   // sizeof remote data
                               // the following fields are only available when using
                               // the IDirectPlay3 interface or greater
    dpnName: TDPName;           // structure with name info
    dpIdParent: TDPID;          // id of parent group
    dwFlags: DWORD;            // player or group flags
  end;

(*
 * DPMSG_ADDPLAYERTOGROUP
 * System message generated when a player is being added
 * to a group.
 *)
  PDPMsg_AddPlayerToGroup = ^TDPMsg_AddPlayerToGroup;
  TDPMsg_AddPlayerToGroup = packed record
    dwType: DWORD;      // Message type
    dpIdGroup: TDPID;    // group ID being added to
    dpIdPlayer: TDPID;   // player ID being added
  end;

(*
 * DPMSG_DELETEPLAYERFROMGROUP
 * System message generated when a player is being
 * removed from a group
 *)
  PDPMsg_DeletePlayerFromGroup = ^TDPMsg_DeletePlayerFromGroup;
  TDPMsg_DeletePlayerFromGroup = TDPMsg_AddPlayerToGroup;

(*
 * TDPMsg_AddGroupToGroup
 * System message generated when a group is being added
 * to a group.
 *)
  PDPMsg_AddGroupToGroup = ^TDPMsg_AddGroupToGroup;
  TDPMsg_AddGroupToGroup = packed record
    dwType: DWORD;           // Message type
    dpIdParentGroup: TDPID;   // group ID being added to
    dpIdGroup: TDPID;         // group ID being added
  end;

(*
 * DPMsg_DeleteGroupFromGroup
 * System message generated when a GROUP is being
 * removed from a group
 *)
  PDPMsg_DeleteGroupFromGroup = ^TDPMsg_DeleteGroupFromGroup;
  TDPMsg_DeleteGroupFromGroup = TDPMsg_AddGroupToGroup;

(*
 * DPMSG_SETPLAYERORGROUPDATA
 * System message generated when remote data for a player or
 * group has changed.
 *)
  PDPMsg_SetPlayerOrGroupData = ^TDPMsg_SetPlayerOrGroupData;
  TDPMsg_SetPlayerOrGroupData = packed record
    dwType: DWORD;         // Message type
    dwPlayerType: DWORD;   // Is it a player or group
    DPID: TDPID;           // ID of player or group
    lpData: Pointer;       // pointer to remote data
    dwDataSize: DWORD;     // size of remote data
  end;

(*
 * DPMSG_SETPLAYERORGROUPNAME
 * System message generated when the name of a player or
 * group has changed.
 *)
  PDPMsg_SetPlayerOrGroupName = ^TDPMsg_SetPlayerOrGroupName;
  TDPMsg_SetPlayerOrGroupName = packed record
    dwType: DWORD;         // Message type
    dwPlayerType: DWORD;   // Is it a player or group
    DPID: TDPID;           // ID of player or group
    dpnName: TDPName;      // structure with new name info
  end;

(*
 * DPMSG_SETSESSIONDESC
 * System message generated when session desc has changed
 *)
  PDPMsg_SetSessionDesc = ^TDPMsg_SetSessionDesc;
  TDPMsg_SetSessionDesc = packed record
    dwType: DWORD;            // Message type
    dpDesc: TDPSessionDesc2;   // Session desc
  end;

(*
 * DPMSG_HOST
 * System message generated when the host has migrated to this
 * DirectPlay object.
 *
 *)
  PDPMsg_Host = ^TDPMsg_Host;
  TDPMsg_Host = TDPMsg_Generic;

(*
 * DPMSG_SESSIONLOST
 * System message generated when the connection to the session is lost.
 *
 *)
  PDPMsg_SessionLost = ^TDPMsg_SessionLost;
  TDPMsg_SessionLost = TDPMsg_Generic;

(*
 * DPMSG_SECUREMESSAGE
 * System message generated when a player requests a secure send
 *)
  PDPMsg_SecureMessage = ^TDPMsg_SecureMessage;
  TDPMsg_SecureMessage = packed record
    dwType: DWORD;       // Message Type
    dwFlags: DWORD;      // Signed/Encrypted
    dpIdFrom: TDPID;      // ID of Sending Player
    lpData: Pointer;     // Player message
    dwDataSize: DWORD;   // Size of player message
  end;

(*
 * DPMSG_STARTSESSION
 * System message containing all information required to
 * start a new session
 *)
  PDPMsg_StartSession = ^TDPMsg_StartSession;
  TDPMsg_StartSession = packed record
    dwType: DWORD;             // Message type
    lpConn: PDPLConnection;   // TDPLConnection structure
  end;

(*
 * DPMSG_CHAT
 * System message containing a chat message
 *)
  PDPMsg_Chat = ^TDPMsg_Chat;
  TDPMsg_Chat = packed record
    dwType: DWORD;        // Message type
    dwFlags: DWORD;       // Message flags
    idFromPlayer: TDPID;  // ID of the Sending Player
    idToPlayer: TDPID;    // ID of the To Player
    idToGroup: TDPID;     // ID of the To Group
    lpChat: PDPChat;      // Pointer to a structure containing the chat message
  end;

(*
 * DPMSG_SETGROUPOWNER
 * System message generated when the owner of a group has changed
 *)
  PDPMsg_SetGroupOwner = ^TDPMsg_SetGroupOwner;
  TDPMsg_SetGroupOwner = packed record
    dwType: DWORD;        // Message type
    idGroup: TDPID;       // ID of the group
    idNewOwner: TDPID;    // ID of the player that is the new owner
    idOldOwner: TDPID;    // ID of the player that used to be the owner
  end;

(*
 * DPMSG_SENDCOMPLETE
 * System message generated when finished with an Async Send message
 *
 * NOTE SENDPARMS has an overlay for DPMSG_SENDCOMPLETE, don't
 *                change this message w/o changing SENDPARMS.
 *)
  PDPMsg_SendComplete = ^TDPMsg_SendComplete;
  TDPMsg_SendComplete = packed record
    dwType: DWORD;        // Message type
    idFrom: TDPID;
    idTo: TDPID;
    dwFlags: DWORD;
    dwPriority: DWORD;
    dwTimeout: DWORD;
    lpvContext: Pointer;
    dwMsgID: DWORD;
    hr: HRESULT;
    dwSendTime: DWORD;
  end;

(****************************************************************************
 *
 * DIRECTPLAY ERRORS
 *
 * Errors are represented by negative values and cannot be combined.
 *
 ****************************************************************************)
const
  MAKE_DPHRESULT = HResult($88770000);

  DP_OK = S_OK;
  DPERR_ALREADYINITIALIZED = MAKE_DPHRESULT + 5;
  DPERR_ACCESSDENIED = MAKE_DPHRESULT + 10;
  DPERR_ACTIVEPLAYERS = MAKE_DPHRESULT + 20;
  DPERR_BUFFERTOOSMALL = MAKE_DPHRESULT + 30;
  DPERR_CANTADDPLAYER = MAKE_DPHRESULT + 40;
  DPERR_CANTCREATEGROUP = MAKE_DPHRESULT + 50;
  DPERR_CANTCREATEPLAYER = MAKE_DPHRESULT + 60;
  DPERR_CANTCREATESESSION = MAKE_DPHRESULT + 70;
  DPERR_CAPSNOTAVAILABLEYET = MAKE_DPHRESULT + 80;
  DPERR_EXCEPTION = MAKE_DPHRESULT + 90;
  DPERR_GENERIC = E_FAIL;
  DPERR_INVALIDFLAGS = MAKE_DPHRESULT + 120;
  DPERR_INVALIDOBJECT = MAKE_DPHRESULT + 130;
  DPERR_INVALIDPARAM = E_INVALIDARG;
  DPERR_INVALIDPARAMS = DPERR_INVALIDPARAM;
  DPERR_INVALIDPLAYER = MAKE_DPHRESULT + 150;
  DPERR_INVALIDGROUP = MAKE_DPHRESULT + 155;
  DPERR_NOCAPS = MAKE_DPHRESULT + 160;
  DPERR_NOCONNECTION = MAKE_DPHRESULT + 170;
  DPERR_NOMEMORY = E_OUTOFMEMORY;
  DPERR_OUTOFMEMORY = DPERR_NOMEMORY;
  DPERR_NOMESSAGES = MAKE_DPHRESULT + 190;
  DPERR_NONAMESERVERFOUND = MAKE_DPHRESULT + 200;
  DPERR_NOPLAYERS = MAKE_DPHRESULT + 210;
  DPERR_NOSESSIONS = MAKE_DPHRESULT + 220;
  DPERR_PENDING = E_PENDING;
  DPERR_SENDTOOBIG = MAKE_DPHRESULT + 230;
  DPERR_TIMEOUT = MAKE_DPHRESULT + 240;
  DPERR_UNAVAILABLE = MAKE_DPHRESULT + 250;
  DPERR_UNSUPPORTED = E_NOTIMPL;
  DPERR_BUSY = MAKE_DPHRESULT + 270;
  DPERR_USERCANCEL = MAKE_DPHRESULT + 280;
  DPERR_NOINTERFACE = E_NOINTERFACE;
  DPERR_CANNOTCREATESERVER = MAKE_DPHRESULT + 290;
  DPERR_PLAYERLOST = MAKE_DPHRESULT + 300;
  DPERR_SESSIONLOST = MAKE_DPHRESULT + 310;
  DPERR_UNINITIALIZED = MAKE_DPHRESULT + 320;
  DPERR_NONEWPLAYERS = MAKE_DPHRESULT + 330;
  DPERR_INVALIDPASSWORD = MAKE_DPHRESULT + 340;
  DPERR_CONNECTING = MAKE_DPHRESULT + 350;
  DPERR_CONNECTIONLOST = MAKE_DPHRESULT + 360;
  DPERR_UNKNOWNMESSAGE = MAKE_DPHRESULT + 370;
  DPERR_CANCELFAILED = MAKE_DPHRESULT + 380;
  DPERR_INVALIDPRIORITY = MAKE_DPHRESULT + 390;
  DPERR_NOTHANDLED = MAKE_DPHRESULT + 400;
  DPERR_CANCELLED = MAKE_DPHRESULT + 410;
  DPERR_ABORTED = MAKE_DPHRESULT + 420;


  DPERR_BUFFERTOOLARGE = MAKE_DPHRESULT + 1000;
  DPERR_CANTCREATEPROCESS = MAKE_DPHRESULT + 1010;
  DPERR_APPNOTSTARTED = MAKE_DPHRESULT + 1020;
  DPERR_INVALIDINTERFACE = MAKE_DPHRESULT + 1030;
  DPERR_NOSERVICEPROVIDER = MAKE_DPHRESULT + 1040;
  DPERR_UNKNOWNAPPLICATION = MAKE_DPHRESULT + 1050;
  DPERR_NOTLOBBIED = MAKE_DPHRESULT + 1070;
  DPERR_SERVICEPROVIDERLOADED = MAKE_DPHRESULT + 1080;
  DPERR_ALREADYREGISTERED = MAKE_DPHRESULT + 1090;
  DPERR_NOTREGISTERED = MAKE_DPHRESULT + 1100;

//
// Security related errors
//
  DPERR_AUTHENTICATIONFAILED = MAKE_DPHRESULT + 2000;
  DPERR_CANTLOADSSPI = MAKE_DPHRESULT + 2010;
  DPERR_ENCRYPTIONFAILED = MAKE_DPHRESULT + 2020;
  DPERR_SIGNFAILED = MAKE_DPHRESULT + 2030;
  DPERR_CANTLOADSECURITYPACKAGE = MAKE_DPHRESULT + 2040;
  DPERR_ENCRYPTIONNOTSUPPORTED = MAKE_DPHRESULT + 2050;
  DPERR_CANTLOADCAPI = MAKE_DPHRESULT + 2060;
  DPERR_NOTLOGGEDIN = MAKE_DPHRESULT + 2070;
  DPERR_LOGONDENIED = MAKE_DPHRESULT + 2080;

(****************************************************************************
 *
 * 	dplay 1.0 obsolete structures + interfaces
 *	Included for compatibility only. New apps should
 *	use IDirectPlay2
 *
 ****************************************************************************)

  DPOPEN_OPENSESSION = DPOPEN_JOIN;
  DPOPEN_CREATESESSION = DPOPEN_CREATE;

  DPENUMSESSIONS_PREVIOUS = $00000004;

  DPENUMPLAYERS_PREVIOUS = $00000004;

  DPSEND_GUARANTEE = DPSEND_GUARANTEED;
  DPSEND_TRYONCE = $00000004;

  DPCAPS_NAMESERVICE = $00000001;
  DPCAPS_NAMESERVER = DPCAPS_ISHOST;
  DPCAPS_GUARANTEED = $00000004;

  DPLONGNAMELEN = 52;
  DPSHORTNAMELEN = 20;
  DPSESSIONNAMELEN = 32;
  DPPASSWORDLEN = 16;
  DPUSERRESERVED = 16;

  DPSYS_ADDPLAYER = $0003;
  DPSYS_DELETEPLAYER = $0005;

  DPSYS_DELETEGROUP = $0020;
  DPSYS_DELETEPLAYERFROMGRP = $0021;
  DPSYS_CONNECT = $484b;

type
  PDPMsg_AddPlayer = ^TDPMsg_AddPlayer;
  TDPMsg_AddPlayer = packed record
    dwType: DWORD;
    dwPlayerType: DWORD;
    DPID: TDPID;
    szLongName: array[0..DPLONGNAMELEN-1] of Char;
    szShortName: array[0..DPSHORTNAMELEN-1] of Char;
    dwCurrentPlayers: DWORD;
  end;

  PDPMsg_AddGroup = ^TDPMsg_AddGroup;
  TDPMsg_AddGroup = TDPMsg_AddPlayer;

  PDPMsg_GroupAdd = ^TDPMsg_GroupAdd;
  TDPMsg_GroupAdd = packed record
    dwType: DWORD;
    dpIdGroup: TDPID;
    dpIdPlayer: TDPID;
  end;

  PDPMsg_GroupDelete = ^TDPMsg_GroupDelete;
  TDPMsg_GroupDelete = TDPMsg_GroupAdd;

  PDPMsg_DeletePlayer = ^TDPMsg_DeletePlayer;
  TDPMsg_DeletePlayer = packed record
    dwType: DWORD;
    DPID: TDPID;
  end;

  TDPEnumPlayersCallback = function(dpId: TDPID; lpFriendlyName: PChar;
      lpFormalName: PChar; dwFlags: DWORD; lpContext: Pointer) : BOOL; stdcall;

  PDPSessionDesc = ^TDPSessionDesc;
  TDPSessionDesc = packed record
    dwSize: DWORD;
    guidSession: TGUID;
    dwSession: DWORD;
    dwMaxPlayers: DWORD;
    dwCurrentPlayers: DWORD;
    dwFlags: DWORD;
    szSessionName: Array [0..DPSESSIONNAMELEN-1] of char;
    szUserField: Array [0..DPUSERRESERVED-1] of char;
    dwReserved1: DWORD;
    szPassword: Array [0..DPPASSWORDLEN-1] of char;
    dwReserved2: DWORD;
    dwUser1: DWORD;
    dwUser2: DWORD;
    dwUser3: DWORD;
    dwUser4: DWORD;
  end;

  TDPEnumSessionsCallback = function(const lpDPSessionDesc: TDPSessionDesc;
      lpContext: Pointer; var lpdwTimeOut: DWORD; dwFlags: DWORD) : BOOL; stdcall;

type
  IDirectPlay = interface (IUnknown)
    ['{5454e9a0-db65-11ce-921c-00aa006c4972}']
    (*** IDirectPlay methods ***)
    function AddPlayerToGroup(pidGroup: TDPID; pidPlayer: TDPID) : HResult; stdcall;
    function Close: HResult; stdcall;
    function CreatePlayer(out lppidID: TDPID; lpPlayerFriendlyName: PChar;
        lpPlayerFormalName: PChar; lpEvent: PHandle) : HResult; stdcall;
    function CreateGroup(out lppidID: TDPID; lpGroupFriendlyName: PChar;
        lpGroupFormalName: PChar) : HResult; stdcall;
    function DeletePlayerFromGroup(pidGroup: TDPID; pidPlayer: TDPID) : HResult; stdcall;
    function DestroyPlayer(pidID: TDPID) : HResult; stdcall;
    function DestroyGroup(pidID: TDPID) : HResult; stdcall;
    function EnableNewPlayers(bEnable: BOOL) : HResult; stdcall;
    function EnumGroupPlayers(pidGroupPID: TDPID; lpEnumPlayersCallback:
        TDPEnumPlayersCallback; lpContext: Pointer; dwFlags: DWORD) : HResult; stdcall;
    function EnumGroups(dwSessionID: DWORD; lpEnumPlayersCallback:
        TDPEnumPlayersCallback; lpContext: Pointer; dwFlags: DWORD) : HResult; stdcall;
    function EnumPlayers(dwSessionId: DWORD; lpEnumPlayersCallback:
        TDPEnumPlayersCallback; lpContext: Pointer; dwFlags: DWORD) : HResult; stdcall;
    function EnumSessions(var lpSDesc: TDPSessionDesc; dwTimeout: DWORD;
        lpEnumSessionsCallback: TDPEnumSessionsCallback; lpContext: Pointer;
        dwFlags: DWORD) : HResult; stdcall;
    function GetCaps(var lpDPCaps: TDPCaps) : HResult; stdcall;
    function GetMessageCount(pidID: TDPID; var lpdwCount: DWORD) : HResult; stdcall;
    function GetPlayerCaps(pidID: TDPID; var lpDPPlayerCaps: TDPCaps) : HResult; stdcall;
    function GetPlayerName(pidID: TDPID; lpPlayerFriendlyName: PChar;
        var lpdwFriendlyNameLength: DWORD; lpPlayerFormalName: PChar;
        var lpdwFormalNameLength: DWORD) : HResult; stdcall;
    function Initialize(const lpGUID: TGUID) : HResult; stdcall;
    function Open(var lpSDesc: TDPSessionDesc) : HResult; stdcall;
    function Receive(var lppidFrom, lppidTo: TDPID; dwFlags: DWORD;
        var lpvBuffer; var lpdwSize: DWORD) : HResult; stdcall;
    function SaveSession(lpSessionName: PChar) : HResult; stdcall;
    function Send(pidFrom: TDPID; pidTo: TDPID; dwFlags: DWORD;
        var lpvBuffer; dwBuffSize: DWORD) : HResult; stdcall;
    function SetPlayerName(pidID: TDPID; lpPlayerFriendlyName: PChar;
        lpPlayerFormalName: PChar) : HResult; stdcall;
  end;

(*
 * GUIDS used by DirectPlay objects
 *)
  IID_IDirectPlay2W = IDirectPlay2W;
  IID_IDirectPlay2A = IDirectPlay2A;
  IID_IDirectPlay2 =  IDirectPlay2;

  IID_IDirectPlay3W = IDirectPlay3W;
  IID_IDirectPlay3A = IDirectPlay3A;
  IID_IDirectPlay3 =  IDirectPlay3;

  IID_IDirectPlay4W = IDirectPlay4W;
  IID_IDirectPlay4A = IDirectPlay4A;
  IID_IDirectPlay4 =  IDirectPlay4;

  IID_IDirectPlay = IDirectPlay;

var
  DirectPlayCreate : function (lpGUID: PGUID; out lplpDP: IDirectPlay;
      pUnk: IUnknown) : HResult; stdcall;

(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dplobby.h
 *  Content:    DirectPlayLobby include file
 ***************************************************************************)

(*
 * GUIDS used by DirectPlay objects
 *)

const
(* {2FE8F810-B2A5-11d0-A787-0000F803ABFC} *)
  CLSID_DirectPlayLobby: TGUID =
      (D1:$2fe8f810;D2:$b2a5;D3:$11d0;D4:($a7,$87,$00,$00,$f8,$3,$ab,$fc));

(****************************************************************************
 *
 * IDirectPlayLobby Structures
 *
 * Various structures used to invoke DirectPlayLobby.
 *
 ****************************************************************************)

type
(*
 * TDPLAppInfo
 * Used to hold information about a registered DirectPlay
 * application
 *)
  PDPLAppInfo = ^TDPLAppInfo;
  TDPLAppInfo = packed record
    dwSize: DWORD;            // Size of this structure
    guidApplication: TGUID;   // GUID of the Application
    case Integer of           // Pointer to the Application Name
      0: (lpszAppName: PCharAW);
      1: (lpszAppNameW: PWideChar);
      3: (lpszAppNameA: PChar);
  end;

(*
 * TDPCompoundAddressElement
 *
 * An array of these is passed to CreateCompoundAddresses()
 *)
  PDPCompoundAddressElement = ^TDPCompoundAddressElement;
  TDPCompoundAddressElement = packed record
    guidDataType: TGUID;
    dwDataSize: DWORD;
    lpData: Pointer;
  end;                                 

(*
 * TDPApplicationDesc
 * Used to register a DirectPlay application
 *)
  PDPApplicationDesc = ^TDPApplicationDesc;
  TDPApplicationDesc = packed record
    dwSize: DWORD;
    dwFlags: DWORD;
    case integer of
      0 : (lpszApplicationName: PCharAW;
           guidApplication: TGUID;
           lpszFilename: PCharAW;
           lpszCommandLine: PCharAW;
           lpszPath: PCharAW;
           lpszCurrentDirectory: PCharAW;
           lpszDescriptionA: PAnsiChar;
           lpszDescriptionW: PWideChar);
      1 : (lpszApplicationNameA: PAnsiChar;
           filler1: TGUID;
           lpszFilenameA: PAnsiChar;
           lpszCommandLineA: PAnsiChar;
           lpszPathA: PAnsiChar;
           lpszCurrentDirectoryA: PAnsiChar);
      2 : (lpszApplicationNameW: PWideChar;
           filler2: TGUID;
           lpszFilenameW: PWideChar;
           lpszCommandLineW: PWideChar;
           lpszPathW: PWideChar;
           lpszCurrentDirectoryW: PWideChar);
  end;

(*
 * TDPApplicationDesc2
 * Used to register a DirectPlay application
 *)
  PDPApplicationDesc2 = ^TDPApplicationDesc2;
  TDPApplicationDesc2 = packed record
    dwSize: DWORD;
    dwFlags: DWORD;
    case integer of
      0 : (lpszApplicationName: PCharAW;
           guidApplication: TGUID;
           lpszFilename: PCharAW;
           lpszCommandLine: PCharAW;
           lpszPath: PCharAW;
           lpszCurrentDirectory: PCharAW;
           lpszDescriptionA: PAnsiChar;
           lpszDescriptionW: PWideChar;
           lpszAppLauncherName: PCharAW);
      1 : (lpszApplicationNameA: PAnsiChar;
           filler1: TGUID;
           lpszFilenameA: PAnsiChar;
           lpszCommandLineA: PAnsiChar;
           lpszPathA: PAnsiChar;
           lpszCurrentDirectoryA: PAnsiChar;
           filler3: PChar;
           filler4: PChar;
           lpszAppLauncherNameA: PAnsiChar);
      2 : (lpszApplicationNameW: PWideChar;
           filler2: TGUID;
           lpszFilenameW: PWideChar;
           lpszCommandLineW: PWideChar;
           lpszPathW: PWideChar;
           lpszCurrentDirectoryW: PWideChar;
           filler5: PChar;
           filler6: PChar;
           lpszAppLauncherNameW: PWideChar);
  end;


(****************************************************************************
 *
 * Enumeration Method Callback Prototypes
 *
 ****************************************************************************)

(*
 * Callback for EnumAddress()
 *)
  TDPEnumAdressCallback = function(const guidDataType: TGUID;
      dwDataSize: DWORD; lpData: Pointer; lpContext: Pointer) : BOOL; stdcall;

(*
 * Callback for EnumAddressTypes()
 *)
  TDPLEnumAddressTypesCallback = function(const guidDataType: TGUID;
      lpContext: Pointer; dwFlags: DWORD) : BOOL; stdcall;

(*
 * Callback for EnumLocalApplications()
 *)
  TDPLEnumLocalApplicationsCallback = function(const lpAppInfo: TDPLAppInfo;
      lpContext: Pointer; dwFlags: DWORD) : BOOL; stdcall;

(****************************************************************************
 *
 * IDirectPlayLobby (and IDirectPlayLobbyA) Interface
 *
 ****************************************************************************)

type
  IDirectPlayLobbyAW = interface (IUnknown)
    (*** IDirectPlayLobby methods ***)
    function Connect(dwFlags: DWORD; out lplpDP: IDirectPlay2;
        pUnk: IUnknown) : HResult; stdcall;
    function CreateAddress(const guidSP, guidDataType: TGUID; var lpData;
        dwDataSize: DWORD; var lpAddress; var lpdwAddressSize: DWORD) : HResult; stdcall;
    function EnumAddress(lpEnumAddressCallback: TDPEnumAdressCallback;
        var lpAddress; dwAddressSize: DWORD; lpContext : Pointer) : HResult; stdcall;
    function EnumAddressTypes(lpEnumAddressTypeCallback:
        TDPLEnumAddressTypesCallback; const guidSP: TGUID; lpContext: Pointer;
        dwFlags: DWORD) : HResult; stdcall;
    function EnumLocalApplications(lpEnumLocalAppCallback: TDPLEnumLocalApplicationsCallback;
        lpContext: Pointer; dwFlags: DWORD) : HResult; stdcall;
    function GetConnectionSettings(dwAppID: DWORD; lpData: PDPLConnection;
        var lpdwDataSize: DWORD) : HResult; stdcall;
    function ReceiveLobbyMessage(dwFlags: DWORD; dwAppID: DWORD;
        var lpdwMessageFlags: DWORD; lpData: Pointer; var lpdwDataSize: DWORD) : HResult; stdcall;
    function RunApplication(dwFlags: DWORD; var lpdwAppId: DWORD;
        const lpConn: TDPLConnection; hReceiveEvent: THandle) : HResult; stdcall;
    function SendLobbyMessage(dwFlags: DWORD; dwAppID: DWORD; const lpData;
        dwDataSize: DWORD) : HResult; stdcall;
    function SetConnectionSettings(dwFlags: DWORD; dwAppID: DWORD;
        var lpConn: TDPLConnection) : HResult; stdcall;
    function SetLobbyMessageEvent(dwFlags: DWORD; dwAppID: DWORD;
        hReceiveEvent: THandle) : HResult; stdcall;
  end;

  IDirectPlayLobbyW = interface (IDirectPlayLobbyAW)
    ['{AF465C71-9588-11CF-A020-00AA006157AC}']
  end;
  IDirectPlayLobbyA = interface (IDirectPlayLobbyAW)
    ['{26C66A70-B367-11cf-A024-00AA006157AC}']
  end;

{$IFDEF UNICODE}
  IDirectPlayLobby = IDirectPlayLobbyW;
{$ELSE}
  IDirectPlayLobby = IDirectPlayLobbyA;
{$ENDIF}


(****************************************************************************
 *
 * IDirectPlayLobby2 (and IDirectPlayLobby2A) Interface
 *
 ****************************************************************************)

  IDirectPlayLobby2AW = interface(IDirectPlayLobbyAW)
    (*** IDirectPlayLobby2 methods ***)
    function CreateCompoundAddress(const lpElements: TDPCompoundAddressElement;
        dwElementCount: DWORD; lpAddress: Pointer; var lpdwAddressSize: DWORD) : HResult; stdcall;
  end;

  IDirectPlayLobby2W = interface (IDirectPlayLobby2AW)
    ['{0194C220-A303-11D0-9C4F-00A0C905425E}']
  end;
  IDirectPlayLobby2A = interface (IDirectPlayLobby2AW)
    ['{1BB4AF80-A303-11d0-9C4F-00A0C905425E}']
  end;

{$IFDEF UNICODE}
  IDirectPlayLobby2 = IDirectPlayLobby2W;
{$ELSE}
  IDirectPlayLobby2 = IDirectPlayLobby2A;
{$ENDIF}

(****************************************************************************
 *
 * IDirectPlayLobby3 (and IDirectPlayLobby3A) Interface
 *
 ****************************************************************************)

  IDirectPlayLobby3AW = interface(IDirectPlayLobby2AW)
    (*** IDirectPlayLobby3 methods ***)
    function ConnectEx(dwFlags: DWORD; const riid: TGUID;
        out lplpDP; pUnk: IUnknown) : HResult; stdcall;
    function RegisterApplication(dwFlags: DWORD;
        var lpAppDesc: TDPApplicationDesc) : HResult; stdcall;
    function UnregisterApplication(dwFlags: DWORD;
         const guidApplication: TGUID) : HResult; stdcall;
    function WaitForConnectionSettings(dwFlags: DWORD) : HResult; stdcall;
	end;

  IDirectPlayLobby3W = interface (IDirectPlayLobby3AW)
    ['{2DB72490-652C-11d1-A7A8-0000F803ABFC}']
  end;
  IDirectPlayLobby3A = interface (IDirectPlayLobby3AW)
    ['{2DB72491-652C-11d1-A7A8-0000F803ABFC}']
  end;

{$IFDEF UNICODE}
  IDirectPlayLobby3 = IDirectPlayLobby3W;
{$ELSE}
  IDirectPlayLobby3 = IDirectPlayLobby3A;
{$ENDIF}

  IID_IDirectPlayLobbyW =  IDirectPlayLobbyW;
  IID_IDirectPlayLobbyA =  IDirectPlayLobbyA;
  IID_IDirectPlayLobby =   IDirectPlayLobby;

  IID_IDirectPlayLobby2W = IDirectPlayLobby2W;
  IID_IDirectPlayLobby2A = IDirectPlayLobby2A;
  IID_IDirectPlayLobby2 =  IDirectPlayLobby2;

  IID_IDirectPlayLobby3W = IDirectPlayLobby3W;
  IID_IDirectPlayLobby3A = IDirectPlayLobby3A;
  IID_IDirectPlayLobby3 =  IDirectPlayLobby3;

(****************************************************************************
 *
 * DirectPlayLobby API Prototypes
 *
 ****************************************************************************)

var
  DirectPlayLobbyCreateW : function (lpguidSP: PGUID; out lplpDPL:
      IDirectPlayLobbyW; lpUnk: IUnknown; lpData: Pointer; dwDataSize: DWORD) : HResult; stdcall;
  DirectPlayLobbyCreateA : function (lpguidSP: PGUID; out lplpDPL:
      IDirectPlayLobbyA; lpUnk: IUnknown; lpData: Pointer; dwDataSize: DWORD) : HResult; stdcall;
  DirectPlayLobbyCreate : function (lpguidSP: PGUID; out lplpDPL:
      IDirectPlayLobby; lpUnk: IUnknown; lpData: Pointer; dwDataSize: DWORD) : HResult; stdcall;

const
(****************************************************************************
 *
 * DirectPlayLobby Flags
 *
 ****************************************************************************)

(*
 *  This flag is used by IDirectPlayLobby.WaitForConnectionSettings to
 *  cancel a current wait that is in progress.
 *)
 DPLWAIT_CANCEL = $00000001;

(*
 *	This is a message flag used by ReceiveLobbyMessage.  It can be
 *	returned in the dwMessageFlags parameter to indicate a message from
 *	the system.
 *)
  DPLMSG_SYSTEM = $00000001;

(*
 *	This is a message flag used by ReceiveLobbyMessage and SendLobbyMessage.
 *  It is used to indicate that the message is a standard lobby message.
 *  TDPLMsg_SetProperty, TDPLMsg_SetPropertyResponse, TDPLMsg_GetProperty,
 *	TDPLMsg_GetPropertyResponse
 *)
  DPLMSG_STANDARD = $00000002;

type
(****************************************************************************
 *
 * DirectPlayLobby messages and message data structures
 *
 * All system messages have a dwMessageFlags value of DPLMSG_SYSTEM returned
 * from a call to ReceiveLobbyMessage.
 *
 * All standard messages have a dwMessageFlags value of DPLMSG_STANDARD returned
 * from a call to ReceiveLobbyMessage.
 *
 ****************************************************************************)

(*
 * TDPLMsg_Generic
 * Generic message structure used to identify the message type.
 *)
  PDPLMsg_Generic = ^TDPLMsg_Generic;
  TDPLMsg_Generic = packed record
    dwType: DWORD;   // Message type
  end;

(*
 * TDPLMsg_SystemMessage
 * Generic message format for all system messages --
 * DPLSYS_CONNECTIONSETTINGSREAD, DPLSYS_DPLYCONNECTSUCCEEDED,
 * DPLSYS_DPLAYCONNECTFAILED, DPLSYS_APPTERMINATED, DPLSYS_NEWCONNECTIONSETTINGS
 *)
  PDPLMsg_SystemMessage = ^TDPLMsg_SystemMessage;
  TDPLMsg_SystemMessage = packed record
    dwType: DWORD;         // Message type
    guidInstance: TGUID;    // Instance GUID of the dplay session the message corresponds to
  end;

(*
 *  TDPLMsg_SetProperty
 *  Standard message sent by an application to a lobby to set a
 *  property
 *)
  PDPLMsg_SetProperty = ^TDPLMsg_SetProperty;
  TDPLMsg_SetProperty = packed record
    dwType: DWORD;                           // Message type
    dwRequestID: DWORD;                      // Request ID (DPL_NOCONFIRMATION if no confirmation desired)
    guidPlayer: TGUID;                       // Player GUID
    guidPropertyTag: TGUID;                  // Property GUID
    dwDataSize: DWORD;                       // Size of data
    dwPropertyData: array[0..0] of DWORD;    // Buffer containing data
  end;

const
  DPL_NOCONFIRMATION = 0;

type
(*
 *  TDPLMsg_SetPropertyResponse
 *  Standard message returned by a lobby to confirm a
 *  TDPLMsg_SetProperty message.
 *)
  PDPLMsg_SetPropertyResponse = ^TDPLMsg_SetPropertyResponse;
  TDPLMsg_SetPropertyResponse = packed record
    dwType: DWORD;            // Message type
    dwRequestID: DWORD;       // Request ID
    guidPlayer: TGUID;        // Player GUID
    guidPropertyTag: TGUID;   // Property GUID
    hr: HResult;              // Return Code
  end;

(*
 *  TDPLMsg_GetProperty
 *  Standard message sent by an application to a lobby to request
 *	the current value of a property
 *)
  PDPLMsg_GetProperty = ^TDPLMsg_GetProperty;
  TDPLMsg_GetProperty = packed record
    dwType: DWORD;            // Message type
    dwRequestID: DWORD;       // Request ID
    guidPlayer: TGUID;        // Player GUID
    guidPropertyTag: TGUID;   // Property GUID
  end;
  LPDPLMSG_GETPROPERTY = ^TDPLMsg_GetProperty;

(*
 *  TDPLMsg_GetPropertyResponse
 *  Standard message returned by a lobby in response to a
 *	TDPLMsg_GetProperty message.
 *)
  PDPLMsg_GetPropertyResponse = ^TDPLMsg_GetPropertyResponse;
  TDPLMsg_GetPropertyResponse = packed record
    dwType: DWORD;                           // Message type
    dwRequestID: DWORD;                      // Request ID
    guidPlayer: TGUID;                       // Player GUID
    guidPropertyTag: TGUID;                  // Property GUID
    hr: HResult;                             // Return Code
    dwDataSize: DWORD;                       // Size of data
    dwPropertyData: array[0..0] of DWORD;    // Buffer containing data
  end;

(*
 *  TDPLMsg_NewSessionHost
 *  Standard message returned by a lobby in response to a
 *  the session host migrating to a new client
 *)
  PDPLMsg_NewSessionHost = ^TDPLMsg_NewSessionHost;
  TDPLMsg_NewSessionHost = packed record
    dwType: DWORD;            // Message type
    guidInstance: TGUID;      // Property GUID
  end;

const
(******************************************
 *
 *	DirectPlay Lobby message dwType values
 *
 *****************************************)

(*
 *  The application has read the connection settings.
 *  It is now O.K. for the lobby client to release
 *  its IDirectPlayLobby interface.
 *)
  DPLSYS_CONNECTIONSETTINGSREAD = $00000001;

(*
 *  The application's call to DirectPlayConnect failed
 *)
  DPLSYS_DPLAYCONNECTFAILED = $00000002;

(*
 *  The application has created a DirectPlay session.
 *)
  DPLSYS_DPLAYCONNECTSUCCEEDED = $00000003;

(*
 *  The application has terminated.
 *)
  DPLSYS_APPTERMINATED = $00000004;

(*
 *  The message is a TDPLMsg_SetProperty message.
 *)
  DPLSYS_SETPROPERTY = $00000005;

(*
 *  The message is a TDPLMsg_SetPropertyResponse message.
 *)
  DPLSYS_SETPROPERTYRESPONSE = $00000006;

(*
 *  The message is a TDPLMsg_GetProperty message.
 *)
  DPLSYS_GETPROPERTY = $00000007;

(*
 *  The message is a TDPLMsg_GetPropertyResponse message.
 *)
  DPLSYS_GETPROPERTYRESPONSE = $00000008;

(*
 *  The message is a TDPLMsg_NewSessionHost message.
 *)
  DPLSYS_NEWSESSIONHOST = $00000009;

(*
 *  New connection settings are available.
 *)
  DPLSYS_NEWCONNECTIONSETTINGS = $0000000A;

(****************************************************************************
 *
 * DirectPlay defined property GUIDs and associated data structures
 *
 ****************************************************************************)

(*
 * DPLPROPERTY_MessagesSupported
 *
 * Request whether the lobby supports standard.  Lobby with respond with either
 * TRUE or FALSE or may not respond at all.
 * 
 * Property data is a single BOOL with TRUE or FALSE
 *)
// {762CCDA1-D916-11d0-BA39-00C04FD7ED67}
  DPLPROPERTY_MessagesSupported: TGUID =
      (D1:$762ccda1;D2:$d916;D3:$11d0;D4:($ba,$39,$00,$c0,$4f,$d7,$ed,$67));

(*
 * DPLPROPERTY_LobbyGuid
 *
 * Request the GUID that identifies the lobby software that the application
 * is communicating with.
 *
 * Property data is a single GUID.
 *)
// {F56920A0-D218-11d0-BA39-00C04FD7ED67}
  DPLPROPERTY_LobbyGuid: TGUID =
      (D1:$F56920A0;D2:$D218;D3:$11d0;D4:($ba,$39,$00,$c0,$4f,$d7,$ed,$67));

(*
 * DPLPROPERTY_PlayerGuid
 *
 * Request the GUID that identifies the player on this machine for sending
 * property data back to the lobby.
 *
 * Property data is the DPLDATA_PLAYERDATA structure
 *)
// {B4319322-D20D-11d0-BA39-00C04FD7ED67}
  DPLPROPERTY_PlayerGuid: TGUID =
      (D1:$b4319322;D2:$d20d;D3:$11d0;D4:($ba,$39,$00,$c0,$4f,$d7,$ed,$67));

type
(*
 * TDPLData_PlayerGUID
 *
 * Data structure to hold the GUID of the player and player creation flags
 * from the lobby.
 *)
  PDPLData_PlayerGUID = ^TDPLData_PlayerGUID;
  TDPLData_PlayerGUID = packed record
    guidPlayer: TGUID;
    dwPlayerFlags: DWORD;
  end;

const
(*
 * DPLPROPERTY_PlayerScore
 *
 * Used to send an array of long integers to the lobby indicating the 
 * score of a player.
 *
 * Property data is the TDPLData_PlayerScore structure.
 *)
// {48784000-D219-11d0-BA39-00C04FD7ED67}
  DPLPROPERTY_PlayerScore: TGUID =
      (D1:$48784000;D2:$d219;D3:$11d0;D4:($ba,$39,$00,$c0,$4f,$d7,$ed,$67));

type
(*
 * TDPLData_PlayerScore
 *
 * Data structure to hold an array of long integers representing a player score.
 * Application must allocate enough memory to hold all the scores.
 *)
  PDPLData_PlayerScore = ^TDPLData_PlayerScore;
  TDPLData_PlayerScore = packed record
    dwScoreCount: DWORD;
    Score: array[0..0] of LongInt;
  end;

(****************************************************************************
 *
 * DirectPlay Address ID's
 *
 ****************************************************************************)

(* DirectPlay Address
 *
 * A DirectPlay address consists of multiple chunks of data, each tagged
 * with a GUID signifying the type of data in the chunk. The chunk also
 * has a length so that unknown chunk types can be skipped.
 *
 * The EnumAddress() function is used to parse these address data chunks.
 *)

(*
 * TDPAddress
 *
 * Header for block of address data elements
 *)
  PDPAddress = ^TDPAddress;
  TDPAddress = packed record
    guidDataType: TGUID;
    dwDataSize: DWORD;
  end;

const
(*
 * DPAID_TotalSize
 *
 * Chunk is a DWORD containing size of entire TDPAddress structure
 *)

// {1318F560-912C-11d0-9DAA-00A0C90A43CB}
  DPAID_TotalSize: TGUID =
      (D1:$1318f560;D2:$912c;D3:$11d0;D4:($9d,$aa,$00,$a0,$c9,$a,$43,$cb));

(*
 * DPAID_ServiceProvider
 *
 * Chunk is a GUID describing the service provider that created the chunk.
 * All addresses must contain this chunk.
 *)

// {07D916C0-E0AF-11cf-9C4E-00A0C905425E}
  DPAID_ServiceProvider: TGUID =
      (D1:$7d916c0;D2:$e0af;D3:$11cf;D4:($9c,$4e,$00,$a0,$c9,$5,$42,$5e));

(*
 * DPAID_LobbyProvider
 *
 * Chunk is a GUID describing the lobby provider that created the chunk.
 * All addresses must contain this chunk.
 *)

// {59B95640-9667-11d0-A77D-0000F803ABFC}
  DPAID_LobbyProvider: TGUID =
      (D1:$59b95640;D2:$9667;D3:$11d0;D4:($a7,$7d,$00,$00,$f8,$3,$ab,$fc));

(*
 * DPAID_Phone and DPAID_PhoneW
 *
 * Chunk is a string containing a phone number (i.e. "1-800-555-1212")
 * in ANSI or UNICODE format
 *)

// {78EC89A0-E0AF-11cf-9C4E-00A0C905425E}
  DPAID_Phone: TGUID =
      (D1:$78ec89a0;D2:$e0af;D3:$11cf;D4:($9c,$4e,$00,$a0,$c9,$5,$42,$5e));

// {BA5A7A70-9DBF-11d0-9CC1-00A0C905425E}
  DPAID_PhoneW: TGUID =
      (D1:$ba5a7a70;D2:$9dbf;D3:$11d0;D4:($9c,$c1,$00,$a0,$c9,$5,$42,$5e));

(*
 * DPAID_Modem and DPAID_ModemW
 *
 * Chunk is a string containing a modem name registered with TAPI
 * in ANSI or UNICODE format
 *)

// {F6DCC200-A2FE-11d0-9C4F-00A0C905425E}
  DPAID_Modem: TGUID =
      (D1:$f6dcc200;D2:$a2fe;D3:$11d0;D4:($9c,$4f,$00,$a0,$c9,$5,$42,$5e));

// {01FD92E0-A2FF-11d0-9C4F-00A0C905425E}
  DPAID_ModemW: TGUID =
      (D1:$1fd92e0;D2:$a2ff;D3:$11d0;D4:($9c,$4f,$00,$a0,$c9,$5,$42,$5e));

(*
 * DPAID_Inet and DPAID_InetW
 *
 * Chunk is a string containing a TCP/IP host name or an IP address
 * (i.e. "dplay.microsoft.com" or "137.55.100.173") in ANSI or UNICODE format
 *)

// {C4A54DA0-E0AF-11cf-9C4E-00A0C905425E}
  DPAID_INet: TGUID =
      (D1:$c4a54da0;D2:$e0af;D3:$11cf;D4:($9c,$4e,$00,$a0,$c9,$5,$42,$5e));

// {E63232A0-9DBF-11d0-9CC1-00A0C905425E}
  DPAID_INetW: TGUID =
      (D1:$e63232a0;D2:$9dbf;D3:$11d0;D4:($9c,$c1,$00,$a0,$c9,$5,$42,$5e));

(*
 * DPAID_InetPort
 *
 * Chunk is the port number used for creating the apps TCP and UDP sockets.
 * WORD value (i.e. 47624)
 *)

// {E4524541-8EA5-11d1-8A96-006097B01411}
  DPAID_INetPort: TGUID =
      (D1:$e4524541;D2:$8ea5;D3:$11d1;D4:($8a,$96,$00,$60,$97,$b0,$14,$11));

//@@BEGIN_MSINTERNAL
(*
 * DPAID_MaxMessageSize
 *
 * Tells DPLAY what the maximum allowed message size is.  Enables SPs to
 *	combat Denial of Service attacks
 *)

 // this terrible hack is needed so the SP can work with the Elmer build.
 // it can be removed when the MSINTERNAL stuff is removed
{$DEFINE MAXMSGSIZEGUIDDEFINED}

// {F5D09980-F0C4-11d1-8326-006097B01411}
  DPAID_MaxMessageSize: TGUID =
      (D1:$f5d09980;D2:$f0c4;D3:$11d1;D4:($83,$26,$00,$60,$97,$b0,$14,$11));
//@@END_MSINTERNAL

(*
 * TDPComPortAddress
 *
 * Used to specify com port settings. The constants that define baud rate,
 * stop bits and parity are defined in WINBASE.H. The constants for flow
 * control are given below.
 *)

  DPCPA_NOFLOW       = 0;           // no flow control
  DPCPA_XONXOFFFLOW  = 1;           // software flow control
  DPCPA_RTSFLOW      = 2;           // hardware flow control with RTS
  DPCPA_DTRFLOW      = 3;           // hardware flow control with DTR
  DPCPA_RTSDTRFLOW   = 4;           // hardware flow control with RTS and DTR

type
  PDPComPortAddress = ^TDPComPortAddress;
  TDPComPortAddress = packed record
    dwComPort: DWORD;       // COM port to use (1-4)
    dwBaudRate: DWORD;      // baud rate (100-256k)
    dwStopBits: DWORD;      // no. stop bits (1-2)
    dwParity: DWORD;        // parity (none, odd, even, mark)
    dwFlowControl: DWORD;   // flow control (none, xon/xoff, rts, dtr)
  end;

const
(*
 * DPAID_ComPort
 *
 * Chunk contains a TDPComPortAddress structure defining the serial port.
 *)

// {F2F0CE00-E0AF-11cf-9C4E-00A0C905425E}
  DPAID_ComPort: TGUID =
      (D1:$f2f0ce00;D2:$e0af;D3:$11cf;D4:($9c,$4e,$00,$a0,$c9,$5,$42,$5e));

(****************************************************************************
 *
 * 	dplobby 1.0 obsolete definitions
 *	Included for compatibility only.
 *
 ****************************************************************************)

  DPLAD_SYSTEM = DPLMSG_SYSTEM;
 

implementation

uses
  DXCommon;

(*==========================================================================;
 *
 *  Copyright (C) 1994-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dplay.h
 *  Content:    DirectPlay include file
 *
 ***************************************************************************)

function DPErrorString(Value: HResult) : string;
begin
  case Value of
    CLASS_E_NOAGGREGATION: Result := 'A non-NULL value was passed for the pUnkOuter parameter in DirectPlayCreate, DirectPlayLobbyCreate, or IDirectPlayLobby2::Connect.';
    DPERR_ACCESSDENIED: Result := 'The session is full or an incorrect password was supplied.';
    DPERR_ACTIVEPLAYERS: Result := 'The requested operation cannot be performed because there are existing active players.';
    DPERR_ALREADYINITIALIZED: Result := 'This object is already initialized.';
    DPERR_APPNOTSTARTED: Result := 'The application has not been started yet.';
    DPERR_AUTHENTICATIONFAILED: Result := 'The password or credentials supplied could not be authenticated.';
    DPERR_BUFFERTOOLARGE: Result := 'The data buffer is too large to store.';
    DPERR_BUSY: Result := 'A message cannot be sent because the transmission medium is busy.';
    DPERR_BUFFERTOOSMALL: Result := 'The supplied buffer is not large enough to contain the requested data.';
    DPERR_CANTADDPLAYER: Result := 'The player cannot be added to the session.';
    DPERR_CANTCREATEGROUP: Result := 'A new group cannot be created.';
    DPERR_CANTCREATEPLAYER: Result := 'A new player cannot be created.';
    DPERR_CANTCREATEPROCESS: Result := 'Cannot start the application.';
    DPERR_CANTCREATESESSION: Result := 'A new session cannot be created.';
    DPERR_CANTLOADCAPI: Result := 'No credentials were supplied and the CryptoAPI package (CAPI) to use for cryptography services cannot be loaded.';
    DPERR_CANTLOADSECURITYPACKAGE: Result := 'The software security package cannot be loaded.';
    DPERR_CANTLOADSSPI: Result := 'No credentials were supplied and the software security package (SSPI) that will prompt for credentials cannot be loaded.';
    DPERR_CAPSNOTAVAILABLEYET: Result := 'The capabilities of the DirectPlay object have not been determined yet. This error will occur if the DirectPlay object is implemented on a connectivity solution that requires polling to determine available bandwidth and latency.';
    DPERR_CONNECTING: Result := 'The method is in the process of connecting to the network. The application should keep calling the method until it returns DP_OK, indicating successful completion, or it returns a different error.';
    DPERR_ENCRYPTIONFAILED: Result := 'The requested information could not be digitally encrypted. Encryption is used for message privacy. This error is only relevant in a secure session.';
    DPERR_EXCEPTION: Result := 'An exception occurred when processing the request.';
    DPERR_GENERIC: Result := 'An undefined error condition occurred.';
//    DPERR_INVALIDCREDENTIALS: Result := 'The credentials supplied (as to IDirectPlay3::SecureOpen) were not valid.';
    DPERR_INVALIDFLAGS: Result := 'The flags passed to this method are invalid.';
    DPERR_INVALIDGROUP: Result := 'The group ID is not recognized as a valid group ID for this game session.';
    DPERR_INVALIDINTERFACE: Result := 'The interface parameter is invalid.';
    DPERR_INVALIDOBJECT: Result := 'The DirectPlay object pointer is invalid.';
    DPERR_INVALIDPARAMS: Result := 'One or more of the parameters passed to the method are invalid.';
    DPERR_INVALIDPASSWORD: Result := 'An invalid password was supplied when attempting to join a session that requires a password.';
    DPERR_INVALIDPLAYER: Result := 'The player ID is not recognized as a valid player ID for this game session.';
    DPERR_LOGONDENIED: Result := 'The session could not be opened because credentials are required and either no credentials were supplied or the credentials were invalid.';
    DPERR_NOCAPS: Result := 'The communication link that DirectPlay is attempting to use is not capable of this function.';
    DPERR_NOCONNECTION: Result := 'No communication link was established.';
    DPERR_NOINTERFACE: Result := 'The interface is not supported.';
    DPERR_NOMESSAGES: Result := 'There are no messages in the receive queue.';
    DPERR_NONAMESERVERFOUND: Result := 'No name server (host) could be found or created. A host must exist to create a player.';
    DPERR_NONEWPLAYERS: Result := 'The session is not accepting any new players.';
    DPERR_NOPLAYERS: Result := 'There are no active players in the session.';
    DPERR_NOSESSIONS: Result := 'There are no existing sessions for this game.';
    DPERR_NOTLOBBIED: Result := 'Returned by the IDirectPlayLobby2::Connect method if the application was not started by using the IDirectPlayLobby2::RunApplication method or if there is no DPLCONNECTION structure currently initialized for this DirectPlayLobby object.';
    DPERR_NOTLOGGEDIN: Result := 'An action cannot be performed because a player or client application is not logged in. Returned by the IDirectPlay3::Send method when the client application tries to send a secure message without being logged in.';
    DPERR_OUTOFMEMORY: Result := 'There is insufficient memory to perform the requested operation.';
    DPERR_PLAYERLOST: Result := 'A player has lost the connection to the session.';
    DPERR_SENDTOOBIG: Result := 'The message being sent by the IDirectPlay3::Send method is too large.';
    DPERR_SESSIONLOST: Result := 'The connection to the session has been lost.';
    DPERR_SIGNFAILED: Result := 'The requested information could not be digitally signed. Digital signatures are used to establish the authenticity of messages.';
    DPERR_TIMEOUT: Result := 'The operation could not be completed in the specified time.';
    DPERR_UNAVAILABLE: Result := 'The requested function is not available at this time.';
    DPERR_UNINITIALIZED: Result := 'The requested object has not been initialized.';
    DPERR_UNKNOWNAPPLICATION: Result := 'An unknown application was specified.';
    DPERR_UNSUPPORTED: Result := 'The function is not available in this implementation. Returned from IDirectPlay3::GetGroupConnectionSettings and IDirectPlay3::SetGroupConnectionSettings if they are called from a session that is not a lobby session.';
    DPERR_USERCANCEL: Result := 'Can be returned in two ways. 1) The user canceled the connection process during a call to the IDirectPlay3::Open method. 2) The user clicked Cancel in one of the DirectPlay service provider dialog boxes during a call to IDirectPlay3::EnumSessions.';
    else Result := 'Unrecognized Error';
  end;
end;

initialization
begin
  if not IsNTandDelphiRunning then
  begin
    DPlayDLL := LoadLibrary('DPlayX.dll');

    DirectPlayEnumerateA := GetProcAddress(DPlayDLL,'DirectPlayEnumerateA');
    DirectPlayEnumerateW := GetProcAddress(DPlayDLL,'DirectPlayEnumerateW');
  {$IFDEF UNICODE}
    DirectPlayEnumerate := DirectPlayEnumerateW;
  {$ELSE}
    DirectPlayEnumerate := DirectPlayEnumerateA;
  {$ENDIF}

    DirectPlayCreate := GetProcAddress(DPlayDLL,'DirectPlayCreate');

//  File:       dplay.h

    DirectPlayLobbyCreateW := GetProcAddress(DPlayDLL,'DirectPlayLobbyCreateW');
    DirectPlayLobbyCreateA := GetProcAddress(DPlayDLL,'DirectPlayLobbyCreateA');
  {$IFDEF UNICODE}
    DirectPlayLobbyCreate := DirectPlayLobbyCreateW;
  {$ELSE}
    DirectPlayLobbyCreate := DirectPlayLobbyCreateA;
  {$ENDIF}

  end;
end;

finalization
begin
  if DPlayDLL <> 0 then FreeLibrary(DPlayDLL);
end;

end.

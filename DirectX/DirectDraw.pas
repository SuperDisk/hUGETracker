(*==========================================================================;
 *
 *  Copyright (C) 1994-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  Files:	ddraw.h dvp.h
 *  Content:	DirectDraw and DirectDrawVideoPort include files
 *
 *  DirectX 7.0 Delphi adaptation by Erik Unger
 *
 *  Modified: 29-Jan-2000
 *
 *  Download: http://www.delphi-jedi.org/DelphiGraphics/
 *  E-Mail: Erik.Unger@gmx.at
 *
 *
 ***************************************************************************)

unit DirectDraw;

{$MODE Delphi}

interface

{$MINENUMSIZE 4}
{$ALIGN ON}

uses
  Windows;

var
  DDrawDLL : HMODULE = 0;

function DDErrorString(Value: HResult) : string;

function MAKEFOURCC(ch0, ch1, ch2, ch3: Char) : DWORD;

(*
 * FOURCC codes for DX compressed-texture pixel formats
 *)
const
  FOURCC_DXT1 = 'DXT1';
  FOURCC_DXT2 = 'DXT2';
  FOURCC_DXT3 = 'DXT3';
  FOURCC_DXT4 = 'DXT4';
  FOURCC_DXT5 = 'DXT5';

(*
 * GUIDS used by DirectDraw objects
 *)
const
  CLSID_DirectDraw: TGUID = '{D7B70EE0-4340-11CF-B063-0020AFC2CD35}';
  CLSID_DirectDraw7: TGUID = '{3c305196-50db-11d3-9cfe-00c04fd930c5}';
  CLSID_DirectDrawClipper: TGUID = '{593817A0-7DB3-11CF-A2DE-00AA00b93356}';

const
  DD_ROP_SPACE = (256 div 32);       // space required to store ROP array

  MAX_DDDEVICEID_STRING	= 512;

(*
 * Flags for the IDirectDraw4::GetDeviceIdentifier method
 *)

(*
 * This flag causes GetDeviceIdentifier to return information about the host (typically 2D) adapter in a system equipped
 * with a stacked secondary 3D adapter. Such an adapter appears to the application as if it were part of the
 * host adapter, but is typically physcially located on a separate card. The stacked secondary's information is
 * returned when GetDeviceIdentifier's dwFlags field is zero, since this most accurately reflects the qualities
 * of the DirectDraw object involved.
 *)
  DDGDI_GETHOSTIDENTIFIER         = $00000001;

(*============================================================================
 *
 * DirectDraw Structures
 *
 * Various structures used to invoke DirectDraw.
 *
 *==========================================================================*)

var
  NilGUID : TGUID absolute 0;


type
  TRefGUID = packed record
    case integer of
    1: (guid : PGUID);
    2: (dwFlags : DWORD);
  end;

  IDirectDraw = interface;
  IDirectDraw2 = interface;
  IDirectDraw4 = interface;
  IDirectDraw7 = interface;
  IDirectDrawSurface = interface;
  IDirectDrawSurface2 = interface;
  IDirectDrawSurface3 = interface;
  IDirectDrawSurface4 = interface;
  IDirectDrawSurface7 = interface;

  IDirectDrawPalette = interface;
  IDirectDrawClipper = interface;
  IDirectDrawColorControl = interface;
  IDirectDrawGammaControl = interface;

(*
 * Generic pixel format with 8-bit RGB and alpha components
 *)
  PDDARGB = ^TDDARGB;
  TDDARGB = packed record
    blue:     BYTE;
    green:    BYTE;
    red:      BYTE;
    alpha:    BYTE;
  end;

(*
 * This version of the structure remains for backwards source compatibility.
 * The DDARGB structure is the one that should be used for all DirectDraw APIs.
 *)
  PDDRGBA = ^TDDRGBA;
  TDDRGBA = packed record
    red   : BYTE;
    green : BYTE;
    blue  : BYTE;
    alpha : BYTE;
  end;

(*
 * TDDColorKey
 *)
  PDDColorKey = ^TDDColorKey;
  TDDColorKey = packed record
    dwColorSpaceLowValue: DWORD;   // low boundary of color space that is to
                                   // be treated as Color Key, inclusive
    dwColorSpaceHighValue: DWORD;  // high boundary of color space that is
                                   // to be treated as Color Key, inclusive
  end;

// Delphi 5 can't handle interface in variant records
// so we have to use pointers instead (which can be type-casted into interfaces):

{$IFDEF VER130}
  PDirectDrawSurface = Pointer;              
{$ELSE}
  PDirectDrawSurface = IDirectDrawSurface;
{$ENDIF}

(*
 * TDDBltFX
 * Used to pass override information to the DIRECTDRAWSURFACE callback Blt.
 *)
  PDDBltFX = ^TDDBltFX;
  TDDBltFX = packed record
    dwSize                        : DWORD;     // size of structure
    dwDDFX                        : DWORD;     // FX operations
    dwROP                         : DWORD;     // Win32 raster operations
    dwDDROP                       : DWORD;     // Raster operations new for DirectDraw
    dwRotationAngle               : DWORD;     // Rotation angle for blt
    dwZBufferOpCode               : DWORD;     // ZBuffer compares
    dwZBufferLow                  : DWORD;     // Low limit of Z buffer
    dwZBufferHigh                 : DWORD;     // High limit of Z buffer
    dwZBufferBaseDest             : DWORD;     // Destination base value
    dwZDestConstBitDepth          : DWORD;     // Bit depth used to specify Z constant for destination
    case integer of
    0: (
      dwZDestConst                : DWORD      // Constant to use as Z buffer for dest
     );
    1: (
      //lpDDSZBufferDest            : PDirectDrawSurface; // Surface to use as Z buffer for dest
      dwZSrcConstBitDepth         : DWORD;     // Bit depth used to specify Z constant for source
      case integer of
      0: (
        dwZSrcConst               : DWORD;     // Constant to use as Z buffer for src
       );
      1: (
        //lpDDSZBufferSrc           : PDirectDrawSurface; // Surface to use as Z buffer for src
        dwAlphaEdgeBlendBitDepth  : DWORD;     // Bit depth used to specify constant for alpha edge blend
        dwAlphaEdgeBlend          : DWORD;     // Alpha for edge blending
        dwReserved                : DWORD;
        dwAlphaDestConstBitDepth  : DWORD;     // Bit depth used to specify alpha constant for destination
        case integer of
        0: (
          dwAlphaDestConst        : DWORD;     // Constant to use as Alpha Channel
         );
        1: (
          //lpDDSAlphaDest          : PDirectDrawSurface; // Surface to use as Alpha Channel
          dwAlphaSrcConstBitDepth : DWORD;     // Bit depth used to specify alpha constant for source
          case integer of
          0: (
            dwAlphaSrcConst       : DWORD;     // Constant to use as Alpha Channel
          );
          1: (
            //lpDDSAlphaSrc         : PDirectDrawSurface; // Surface to use as Alpha Channel
            case integer of
            0: (
              dwFillColor         : DWORD;     // color in RGB or Palettized
            );
            1: (
              dwFillDepth         : DWORD;     // depth value for z-buffer
            );
            2: (
              dwFillPixel         : DWORD;     // pixel value
            );
            3: (
              //lpDDSPattern        : PDirectDrawSurface; // Surface to use as pattern
              ddckDestColorkey    : TDDColorKey; // DestColorkey override
              ddckSrcColorkey     : TDDColorKey; // SrcColorkey override
            )
        )
      )
    )
  )
  end;

(*
 * TDDSCaps
 *)
  PDDSCaps = ^TDDSCaps;
  TDDSCaps = packed record
    dwCaps: DWORD;         // capabilities of surface wanted
  end;

(*
 * TDDOSCaps
 *)
  PDDOSCaps = ^TDDOSCaps;
  TDDOSCaps = packed record
    dwCaps: DWORD;         // capabilities of surface wanted
  end;

(*
 * This structure is used internally by DirectDraw.
 *)
  PDDSCapsEx = ^TDDSCapsEx;
  TDDSCapsEx = packed record
    dwCaps2 : DWORD;
    dwCaps3 : DWORD;
    dwCaps4 : DWORD;
  end;

(*
 * TDDSCaps2
 *)
  PDDSCaps2 = ^TDDSCaps2;
  TDDSCaps2 = packed record
    dwCaps: DWORD;         // capabilities of surface wanted
    dwCaps2 : DWORD;
    dwCaps3 : DWORD;
    dwCaps4 : DWORD;
  end;

(*
 * TDDCaps
 *)
(*
 * This structure is the TDDCaps structure as it was in version 2 and 3 of Direct X.
 * It is present for back compatability.
 *)
  PDDCaps_DX3 = ^TDDCaps_DX3;
  TDDCaps_DX3 = packed record
    dwSize: DWORD;                 // size of the DDDRIVERCAPS structure
    dwCaps: DWORD;                 // driver specific capabilities
    dwCaps2: DWORD;                // more driver specific capabilites
    dwCKeyCaps: DWORD;             // color key capabilities of the surface
    dwFXCaps: DWORD;               // driver specific stretching and effects capabilites
    dwFXAlphaCaps: DWORD;          // alpha driver specific capabilities
    dwPalCaps: DWORD;              // palette capabilities
    dwSVCaps: DWORD;               // stereo vision capabilities
    dwAlphaBltConstBitDepths: DWORD;       // DDBD_2,4,8
    dwAlphaBltPixelBitDepths: DWORD;       // DDBD_1,2,4,8
    dwAlphaBltSurfaceBitDepths: DWORD;     // DDBD_1,2,4,8
    dwAlphaOverlayConstBitDepths: DWORD;   // DDBD_2,4,8
    dwAlphaOverlayPixelBitDepths: DWORD;   // DDBD_1,2,4,8
    dwAlphaOverlaySurfaceBitDepths: DWORD; // DDBD_1,2,4,8
    dwZBufferBitDepths: DWORD;             // DDBD_8,16,24,32
    dwVidMemTotal: DWORD;          // total amount of video memory
    dwVidMemFree: DWORD;           // amount of free video memory
    dwMaxVisibleOverlays: DWORD;   // maximum number of visible overlays
    dwCurrVisibleOverlays: DWORD;  // current number of visible overlays
    dwNumFourCCCodes: DWORD;       // number of four cc codes
    dwAlignBoundarySrc: DWORD;     // source rectangle alignment
    dwAlignSizeSrc: DWORD;         // source rectangle byte size
    dwAlignBoundaryDest: DWORD;    // dest rectangle alignment
    dwAlignSizeDest: DWORD;        // dest rectangle byte size
    dwAlignStrideAlign: DWORD;     // stride alignment
    dwRops: Array [0..DD_ROP_SPACE-1] of DWORD;   // ROPS supported
    ddsCaps: TDDSCaps;             // TDDSCaps structure has all the general capabilities
    dwMinOverlayStretch: DWORD;    // minimum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMaxOverlayStretch: DWORD;    // maximum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMinLiveVideoStretch: DWORD;  // minimum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMaxLiveVideoStretch: DWORD;  // maximum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMinHwCodecStretch: DWORD;    // minimum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMaxHwCodecStretch: DWORD;    // maximum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwReserved1: DWORD;            // reserved
    dwReserved2: DWORD;            // reserved
    dwReserved3: DWORD;            // reserved
    dwSVBCaps: DWORD;              // driver specific capabilities for System->Vmem blts
    dwSVBCKeyCaps: DWORD;          // driver color key capabilities for System->Vmem blts
    dwSVBFXCaps: DWORD;            // driver FX capabilities for System->Vmem blts
    dwSVBRops: Array [0..DD_ROP_SPACE-1] of DWORD;// ROPS supported for System->Vmem blts
    dwVSBCaps: DWORD;              // driver specific capabilities for Vmem->System blts
    dwVSBCKeyCaps: DWORD;          // driver color key capabilities for Vmem->System blts
    dwVSBFXCaps: DWORD;            // driver FX capabilities for Vmem->System blts
    dwVSBRops: Array [0..DD_ROP_SPACE-1] of DWORD;// ROPS supported for Vmem->System blts
    dwSSBCaps: DWORD;              // driver specific capabilities for System->System blts
    dwSSBCKeyCaps: DWORD;          // driver color key capabilities for System->System blts
    dwSSBFXCaps: DWORD;            // driver FX capabilities for System->System blts
    dwSSBRops: Array [0..DD_ROP_SPACE-1] of DWORD;// ROPS supported for System->System blts
    dwReserved4 : DWORD;
    dwReserved5 : DWORD;
    dwReserved6 : DWORD;
  end;

(*
 * This structure is the TDDCaps structure as it was in version 5 of Direct X.
 * It is present for back compatability.
 *)
  PDDCaps_DX5 = ^TDDCaps_DX5;
  TDDCaps_DX5 = packed record
    dwSize: DWORD;                 // size of the DDDRIVERCAPS structure
    dwCaps: DWORD;                 // driver specific capabilities
    dwCaps2: DWORD;                // more driver specific capabilites
    dwCKeyCaps: DWORD;             // color key capabilities of the surface
    dwFXCaps: DWORD;               // driver specific stretching and effects capabilites
    dwFXAlphaCaps: DWORD;          // alpha driver specific capabilities
    dwPalCaps: DWORD;              // palette capabilities
    dwSVCaps: DWORD;               // stereo vision capabilities
    dwAlphaBltConstBitDepths: DWORD;       // DDBD_2,4,8
    dwAlphaBltPixelBitDepths: DWORD;       // DDBD_1,2,4,8
    dwAlphaBltSurfaceBitDepths: DWORD;     // DDBD_1,2,4,8
    dwAlphaOverlayConstBitDepths: DWORD;   // DDBD_2,4,8
    dwAlphaOverlayPixelBitDepths: DWORD;   // DDBD_1,2,4,8
    dwAlphaOverlaySurfaceBitDepths: DWORD; // DDBD_1,2,4,8
    dwZBufferBitDepths: DWORD;             // DDBD_8,16,24,32
    dwVidMemTotal: DWORD;          // total amount of video memory
    dwVidMemFree: DWORD;           // amount of free video memory
    dwMaxVisibleOverlays: DWORD;   // maximum number of visible overlays
    dwCurrVisibleOverlays: DWORD;  // current number of visible overlays
    dwNumFourCCCodes: DWORD;       // number of four cc codes
    dwAlignBoundarySrc: DWORD;     // source rectangle alignment
    dwAlignSizeSrc: DWORD;         // source rectangle byte size
    dwAlignBoundaryDest: DWORD;    // dest rectangle alignment
    dwAlignSizeDest: DWORD;        // dest rectangle byte size
    dwAlignStrideAlign: DWORD;     // stride alignment
    dwRops: Array [0..DD_ROP_SPACE-1] of DWORD;   // ROPS supported
    ddsCaps: TDDSCaps;             // TDDSCaps structure has all the general capabilities
    dwMinOverlayStretch: DWORD;    // minimum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMaxOverlayStretch: DWORD;    // maximum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMinLiveVideoStretch: DWORD;  // minimum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMaxLiveVideoStretch: DWORD;  // maximum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMinHwCodecStretch: DWORD;    // minimum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMaxHwCodecStretch: DWORD;    // maximum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwReserved1: DWORD;            // reserved
    dwReserved2: DWORD;            // reserved
    dwReserved3: DWORD;            // reserved
    dwSVBCaps: DWORD;              // driver specific capabilities for System->Vmem blts
    dwSVBCKeyCaps: DWORD;          // driver color key capabilities for System->Vmem blts
    dwSVBFXCaps: DWORD;            // driver FX capabilities for System->Vmem blts
    dwSVBRops: Array [0..DD_ROP_SPACE-1] of DWORD;// ROPS supported for System->Vmem blts
    dwVSBCaps: DWORD;              // driver specific capabilities for Vmem->System blts
    dwVSBCKeyCaps: DWORD;          // driver color key capabilities for Vmem->System blts
    dwVSBFXCaps: DWORD;            // driver FX capabilities for Vmem->System blts
    dwVSBRops: Array [0..DD_ROP_SPACE-1] of DWORD;// ROPS supported for Vmem->System blts
    dwSSBCaps: DWORD;              // driver specific capabilities for System->System blts
    dwSSBCKeyCaps: DWORD;          // driver color key capabilities for System->System blts
    dwSSBFXCaps: DWORD;            // driver FX capabilities for System->System blts
    dwSSBRops: Array [0..DD_ROP_SPACE-1] of DWORD;// ROPS supported for System->System blts
    // Members added for DX5:
    dwMaxVideoPorts: DWORD;	   // maximum number of usable video ports
    dwCurrVideoPorts: DWORD;	   // current number of video ports used
    dwSVBCaps2: DWORD;		   // more driver specific capabilities for System->Vmem blts
    dwNLVBCaps: DWORD;		   // driver specific capabilities for non-local->local vidmem blts
    dwNLVBCaps2: DWORD;		   // more driver specific capabilities non-local->local vidmem blts
    dwNLVBCKeyCaps: DWORD;	   // driver color key capabilities for non-local->local vidmem blts
    dwNLVBFXCaps: DWORD;	   // driver FX capabilities for non-local->local blts
    dwNLVBRops: Array [0..DD_ROP_SPACE-1] of DWORD; // ROPS supported for non-local->local blts
  end;

  PDDCaps_DX6 = ^TDDCaps_DX6;
  TDDCaps_DX6 = packed record
    dwSize: DWORD;                 // size of the DDDRIVERCAPS structure
    dwCaps: DWORD;                 // driver specific capabilities
    dwCaps2: DWORD;                // more driver specific capabilites
    dwCKeyCaps: DWORD;             // color key capabilities of the surface
    dwFXCaps: DWORD;               // driver specific stretching and effects capabilites
    dwFXAlphaCaps: DWORD;          // alpha driver specific capabilities
    dwPalCaps: DWORD;              // palette capabilities
    dwSVCaps: DWORD;               // stereo vision capabilities
    dwAlphaBltConstBitDepths: DWORD;       // DDBD_2,4,8
    dwAlphaBltPixelBitDepths: DWORD;       // DDBD_1,2,4,8
    dwAlphaBltSurfaceBitDepths: DWORD;     // DDBD_1,2,4,8
    dwAlphaOverlayConstBitDepths: DWORD;   // DDBD_2,4,8
    dwAlphaOverlayPixelBitDepths: DWORD;   // DDBD_1,2,4,8
    dwAlphaOverlaySurfaceBitDepths: DWORD; // DDBD_1,2,4,8
    dwZBufferBitDepths: DWORD;             // DDBD_8,16,24,32
    dwVidMemTotal: DWORD;          // total amount of video memory
    dwVidMemFree: DWORD;           // amount of free video memory
    dwMaxVisibleOverlays: DWORD;   // maximum number of visible overlays
    dwCurrVisibleOverlays: DWORD;  // current number of visible overlays
    dwNumFourCCCodes: DWORD;       // number of four cc codes
    dwAlignBoundarySrc: DWORD;     // source rectangle alignment
    dwAlignSizeSrc: DWORD;         // source rectangle byte size
    dwAlignBoundaryDest: DWORD;    // dest rectangle alignment
    dwAlignSizeDest: DWORD;        // dest rectangle byte size
    dwAlignStrideAlign: DWORD;     // stride alignment
    dwRops: Array [0..DD_ROP_SPACE-1] of DWORD;   // ROPS supported
    ddsOldCaps: TDDSCaps;          // Was dssCaps: TDDSCaps. ddsCaps is of type TDDScaps2 for DX6
    dwMinOverlayStretch: DWORD;    // minimum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMaxOverlayStretch: DWORD;    // maximum overlay stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMinLiveVideoStretch: DWORD;  // minimum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMaxLiveVideoStretch: DWORD;  // maximum live video stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMinHwCodecStretch: DWORD;    // minimum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwMaxHwCodecStretch: DWORD;    // maximum hardware codec stretch factor multiplied by 1000, eg 1000 == 1.0, 1300 == 1.3
    dwReserved1: DWORD;            // reserved
    dwReserved2: DWORD;            // reserved
    dwReserved3: DWORD;            // reserved
    dwSVBCaps: DWORD;              // driver specific capabilities for System->Vmem blts
    dwSVBCKeyCaps: DWORD;          // driver color key capabilities for System->Vmem blts
    dwSVBFXCaps: DWORD;            // driver FX capabilities for System->Vmem blts
    dwSVBRops: Array [0..DD_ROP_SPACE-1] of DWORD;// ROPS supported for System->Vmem blts
    dwVSBCaps: DWORD;              // driver specific capabilities for Vmem->System blts
    dwVSBCKeyCaps: DWORD;          // driver color key capabilities for Vmem->System blts
    dwVSBFXCaps: DWORD;            // driver FX capabilities for Vmem->System blts
    dwVSBRops: Array [0..DD_ROP_SPACE-1] of DWORD;// ROPS supported for Vmem->System blts
    dwSSBCaps: DWORD;              // driver specific capabilities for System->System blts
    dwSSBCKeyCaps: DWORD;          // driver color key capabilities for System->System blts
    dwSSBFXCaps: DWORD;            // driver FX capabilities for System->System blts
    dwSSBRops: Array [0..DD_ROP_SPACE-1] of DWORD;// ROPS supported for System->System blts
    // Members added for DX5:
    dwMaxVideoPorts: DWORD;	   // maximum number of usable video ports
    dwCurrVideoPorts: DWORD;	   // current number of video ports used
    dwSVBCaps2: DWORD;		   // more driver specific capabilities for System->Vmem blts
    dwNLVBCaps: DWORD;		   // driver specific capabilities for non-local->local vidmem blts
    dwNLVBCaps2: DWORD;		   // more driver specific capabilities non-local->local vidmem blts
    dwNLVBCKeyCaps: DWORD;	   // driver color key capabilities for non-local->local vidmem blts
    dwNLVBFXCaps: DWORD;	   // driver FX capabilities for non-local->local blts
    dwNLVBRops: Array [0..DD_ROP_SPACE-1] of DWORD; // ROPS supported for non-local->local blts
    // Members added for DX6 release
    ddsCaps : TDDSCaps2 ;          // Surface Caps
  end;

  TDDCaps_DX7 = TDDCaps_DX6;
  
  PDDCaps = ^TDDCaps;

{$IFDEF DIRECTX3}
  TDDCaps = TDDCaps_DX3;
{$ELSE}
  {$IFDEF DIRECTX5}
    TDDCaps = TDDCaps_DX5;
  {$ELSE}
    {$IFDEF DIRECTX6}
      TDDCaps = TDDCaps_DX6;
    {$ELSE}
      TDDCaps = TDDCaps_DX7;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}




(*
 * TDDPixelFormat
 *)
  PDDPixelFormat_DX5 = ^TDDPixelFormat_DX5;
  TDDPixelFormat_DX5 = packed record
    dwSize: DWORD;                 // size of structure
    dwFlags: DWORD;                // pixel format flags
    dwFourCC: DWORD;               // (FOURCC code)
    case Integer of
    0: (
      dwZBufferBitDepth: DWORD;      // how many bits for z buffers
     );
    1: (
      dwAlphaBitDepth: DWORD;        // how many bits for alpha channels
     );
    2: (
      dwRGBBitCount: DWORD;          // how many bits per pixel
      dwRBitMask: DWORD;             // mask for red bit
      dwGBitMask: DWORD;             // mask for green bits
      dwBBitMask: DWORD;             // mask for blue bits
      dwRGBAlphaBitMask: DWORD;      // mask for alpha channel
     );
    3: (
      dwYUVBitCount: DWORD;          // how many bits per pixel
      dwYBitMask: DWORD;             // mask for Y bits
      dwUBitMask: DWORD;             // mask for U bits
      dwVBitMask: DWORD;             // mask for V bits
      case Integer of
      0: (
        dwYUVAlphaBitMask: DWORD;      // mask for alpha channel
       );
      1: (
        dwRGBZBitMask: DWORD;
       );
      2: (
        dwYUVZBitMask: DWORD;
       );
     );
  end;

  PDDPixelFormat_DX6 = ^TDDPixelFormat_DX6;
  TDDPixelFormat_DX6 = packed record
    dwSize: DWORD;                 // size of structure
    dwFlags: DWORD;                // pixel format flags
    dwFourCC: DWORD;               // (FOURCC code)
    case Integer of
      1: (
          dwRGBBitCount : DWORD;  // how many bits per pixel
          dwRBitMask : DWORD;  // mask for red bit
          dwGBitMask : DWORD;  // mask for green bits
          dwBBitMask : DWORD;  // mask for blue bits
          dwRGBAlphaBitMask : DWORD; // mask for alpha channel
          );
      2: (
          dwYUVBitCount : DWORD;  // how many bits per pixel
          dwYBitMask : DWORD;  // mask for Y bits
          dwUBitMask : DWORD;  // mask for U bits
          dwVBitMask : DWORD;  // mask for V bits
          dwYUVAlphaBitMask : DWORD; // mask for alpha channel
          );
      3: (
          dwZBufferBitDepth : DWORD; // how many total bits/pixel in z buffer (including any stencil bits)
          dwStencilBitDepth : DWORD; // how many stencil bits (note: dwZBufferBitDepth-dwStencilBitDepth is total Z-only bits)
          dwZBitMask : DWORD;  // mask for Z bits
          dwStencilBitMask : DWORD; // mask for stencil bits
          dwLuminanceAlphaBitMask : DWORD;// mask for alpha channel
          );
      4: (
          dwAlphaBitDepth : DWORD; // how many bits for alpha channels
          dwLuminanceBitMask : DWORD; // mask for luminance bits
          dwBumpDvBitMask : DWORD;        // mask for bump map V delta bits
          dwBumpLuminanceBitMask : DWORD; // mask for luminance in bump map
          dwRGBZBitMask : DWORD;  // mask for Z channel
          );
      5: (
           dwLuminanceBitCount : DWORD; // how many bits per pixel
           dwBumpDuBitMask : DWORD;       // mask for bump map U delta bits
           Fill1, Fill2    : DWORD;
           dwYUVZBitMask   : DWORD;  // mask for Z channel
         );
      6: ( dwBumpBitCount  : DWORD;         // how many bits per "buxel", total
         );
  end;

  TDDPixelFormat_DX3 = TDDPixelFormat_DX5;
  TDDPixelFormat_DX7 = TDDPixelFormat_DX6;

  PDDPixelFormat = ^TDDPixelFormat;
{$IFDEF DIRECTX3}
  TDDPixelFormat = TDDPixelFormat_DX3;
{$ELSE}
  {$IFDEF DIRECTX5}
    TDDPixelFormat = TDDPixelFormat_DX5;
  {$ELSE}
    {$IFDEF DIRECTX6}
      TDDPixelFormat = TDDPixelFormat_DX6;
    {$ELSE}
      TDDPixelFormat = TDDPixelFormat_DX7;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

(*
 * TDDOverlayFX
 *)
  PDDOverlayFX = ^TDDOverlayFX;
  TDDOverlayFX = packed record
    dwSize: DWORD;                         // size of structure
    dwAlphaEdgeBlendBitDepth: DWORD;       // Bit depth used to specify constant for alpha edge blend
    dwAlphaEdgeBlend: DWORD;               // Constant to use as alpha for edge blend
    dwReserved: DWORD;
    dwAlphaDestConstBitDepth: DWORD;       // Bit depth used to specify alpha constant for destination
    case Integer of
    0: (
      dwAlphaDestConst: DWORD;               // Constant to use as alpha channel for dest
      dwAlphaSrcConstBitDepth: DWORD;        // Bit depth used to specify alpha constant for source
      dwAlphaSrcConst: DWORD;                // Constant to use as alpha channel for src
      dckDestColorkey: TDDColorKey;                // DestColorkey override
      dckSrcColorkey: TDDColorKey;                 // DestColorkey override
      dwDDFX: DWORD;                         // Overlay FX
      dwFlags: DWORD;                        // flags
     );
    1: (
      //lpDDSAlphaDest: PDirectDrawSurface;     // Surface to use as alpha channel for dest
      filler: DWORD;
      //lpDDSAlphaSrc: PDirectDrawSurface;      // Surface to use as alpha channel for src
     );
  end;

(*
 * TDDBltBatch: BltBatch entry structure
 *)
  PDDBltBatch = ^TDDBltBatch;
  TDDBltBatch = packed record
    lprDest: PRect;
    lpDDSSrc: IDirectDrawSurface;
    lprSrc: PRect;
    dwFlags: DWORD;
    lpDDBltFx: TDDBltFX;
  end;

(*
 * TDDGammaRamp
 *)
  PDDGammaRamp = ^TDDGammaRamp;
  TDDGammaRamp = packed record
    red   : array[0..255] of WORD;
    green : array[0..255] of WORD;
    blue  : array[0..255] of WORD;
  end;

(*
 *  This is the structure within which DirectDraw returns data about the current graphics driver and chipset
 *)

  PDDDeviceIdentifier = ^TDDDeviceIdentifier;
  TDDDeviceIdentifier = packed record
    //
    // These elements are for presentation to the user only. They should not be used to identify particular
    // drivers, since this is unreliable and many different strings may be associated with the same
    // device, and the same driver from different vendors.
    //
    szDriver: array[0..MAX_DDDEVICEID_STRING-1] of Char;
    szDescription: array[0..MAX_DDDEVICEID_STRING-1] of Char;

    //
    // This element is the version of the DirectDraw/3D driver. It is legal to do <, > comparisons
    // on the whole 64 bits. Caution should be exercised if you use this element to identify problematic
    // drivers. It is recommended that guidDeviceIdentifier is used for this purpose.
    //
    // This version has the form:
    //  wProduct = HIWORD(liDriverVersion.HighPart)
    //  wVersion = LOWORD(liDriverVersion.HighPart)
    //  wSubVersion = HIWORD(liDriverVersion.LowPart)
    //  wBuild = LOWORD(liDriverVersion.LowPart)
    //
    liDriverVersion: TLargeInteger;     // Defined for applications and other 32 bit components

    //
    // These elements can be used to identify particular chipsets. Use with extreme caution.
    //   dwVendorId     Identifies the manufacturer. May be zero if unknown.
    //   dwDeviceId     Identifies the type of chipset. May be zero if unknown.
    //   dwSubSysId     Identifies the subsystem, typically this means the particular board. May be zero if unknown.
    //   dwRevision     Identifies the revision level of the chipset. May be zero if unknown.
    //
    dwVendorId: DWORD;
    dwDeviceId: DWORD;
    dwSubSysId: DWORD;
    dwRevision: DWORD;

    //
    // This element can be used to check changes in driver/chipset. This GUID is a unique identifier for the
    // driver/chipset pair. Use this element if you wish to track changes to the driver/chipset in order to
    // reprofile the graphics subsystem.
    // This element can also be used to identify particular problematic drivers.
    //
    guidDeviceIdentifier: TGUID;
  end;

  PDDDeviceIdentifier2 = ^TDDDeviceIdentifier2;
  TDDDeviceIdentifier2 = packed record
    //
    // These elements are for presentation to the user only. They should not be used to identify particular
    // drivers, since this is unreliable and many different strings may be associated with the same
    // device, and the same driver from different vendors.
    //
    szDriver: array[0..MAX_DDDEVICEID_STRING-1] of Char;
    szDescription: array[0..MAX_DDDEVICEID_STRING-1] of Char;

    //
    // This element is the version of the DirectDraw/3D driver. It is legal to do <, > comparisons
    // on the whole 64 bits. Caution should be exercised if you use this element to identify problematic
    // drivers. It is recommended that guidDeviceIdentifier is used for this purpose.
    //
    // This version has the form:
    //  wProduct = HIWORD(liDriverVersion.HighPart)
    //  wVersion = LOWORD(liDriverVersion.HighPart)
    //  wSubVersion = HIWORD(liDriverVersion.LowPart)
    //  wBuild = LOWORD(liDriverVersion.LowPart)
    //
    liDriverVersion: TLargeInteger;     // Defined for applications and other 32 bit components

    //
    // These elements can be used to identify particular chipsets. Use with extreme caution.
    //   dwVendorId     Identifies the manufacturer. May be zero if unknown.
    //   dwDeviceId     Identifies the type of chipset. May be zero if unknown.
    //   dwSubSysId     Identifies the subsystem, typically this means the particular board. May be zero if unknown.
    //   dwRevision     Identifies the revision level of the chipset. May be zero if unknown.
    //
    dwVendorId: DWORD;
    dwDeviceId: DWORD;
    dwSubSysId: DWORD;
    dwRevision: DWORD;

    //
    // This element can be used to check changes in driver/chipset. This GUID is a unique identifier for the
    // driver/chipset pair. Use this element if you wish to track changes to the driver/chipset in order to
    // reprofile the graphics subsystem.
    // This element can also be used to identify particular problematic drivers.
    //
    guidDeviceIdentifier: TGUID;

    (*
     * This element is used to determine the Windows Hardware Quality Lab (WHQL)
     * certification level for this driver/device pair.
     *)
    dwWHQLLevel: DWORD;
  end;

(*
 * callbacks
 *)
  TClipperCallback = function(lpDDClipper: IDirectDrawClipper; hWnd: HWND;
      Code: DWORD; lpContext: Pointer): HResult; stdcall;
  TSurfacesStreamingCallback = function(Arg: DWORD): HResult; stdcall;

(*
 * TDDSurfaceDesc
 *)
  PDDSurfaceDesc_DX5 = ^TDDSurfaceDesc_DX5;
  TDDSurfaceDesc_DX5 = packed record
    dwSize: DWORD;                 // size of the TDDSurfaceDesc structure
    dwFlags: DWORD;                // determines what fields are valid
    dwHeight: DWORD;               // height of surface to be created
    dwWidth: DWORD;                // width of input surface
    case Integer of
    0: (
      dwLinearSize : DWORD;       // unused at the moment
     );
    1: (
      lPitch: LongInt;                 // distance to start of next line (return value only)
      dwBackBufferCount: DWORD;      // number of back buffers requested
      case Integer of
      0: (
        dwMipMapCount: DWORD;          // number of mip-map levels requested
        dwAlphaBitDepth: DWORD;        // depth of alpha buffer requested
        dwReserved: DWORD;             // reserved
        lpSurface: Pointer;              // pointer to the associated surface memory
        ddckCKDestOverlay: TDDColorKey;      // color key for destination overlay use
        ddckCKDestBlt: TDDColorKey;          // color key for destination blt use
        ddckCKSrcOverlay: TDDColorKey;       // color key for source overlay use
        ddckCKSrcBlt: TDDColorKey;           // color key for source blt use
        ddpfPixelFormat: TDDPixelFormat_DX5; // pixel format description of the surface
        ddsCaps: TDDSCaps;                // direct draw surface capabilities
       );
      1: (
        dwZBufferBitDepth: DWORD;      // depth of Z buffer requested
       );
      2: (
        dwRefreshRate: DWORD;          // refresh rate (used when display mode is described)
       );
     );
  end;

  PDDSurfaceDesc_DX6 = ^TDDSurfaceDesc_DX6;
  TDDSurfaceDesc_DX6 = packed record
    dwSize: DWORD;                 // size of the TDDSurfaceDesc structure
    dwFlags: DWORD;                // determines what fields are valid
    dwHeight: DWORD;               // height of surface to be created
    dwWidth: DWORD;                // width of input surface
    case Integer of
    0: (
      dwLinearSize : DWORD;       // unused at the moment
     );
    1: (
      lPitch: LongInt;                 // distance to start of next line (return value only)
      dwBackBufferCount: DWORD;      // number of back buffers requested
      case Integer of
      0: (
        dwMipMapCount: DWORD;          // number of mip-map levels requested
        dwAlphaBitDepth: DWORD;        // depth of alpha buffer requested
        dwReserved: DWORD;             // reserved
        lpSurface: Pointer;              // pointer to the associated surface memory
        ddckCKDestOverlay: TDDColorKey;      // color key for destination overlay use
        ddckCKDestBlt: TDDColorKey;          // color key for destination blt use
        ddckCKSrcOverlay: TDDColorKey;       // color key for source overlay use
        ddckCKSrcBlt: TDDColorKey;           // color key for source blt use
        ddpfPixelFormat: TDDPixelFormat_DX6; // pixel format description of the surface
        ddsCaps: TDDSCaps;                // direct draw surface capabilities
       );
      1: (
        dwZBufferBitDepth: DWORD;      // depth of Z buffer requested
       );
      2: (
        dwRefreshRate: DWORD;          // refresh rate (used when display mode is described)
       );
     );
  end;

  PDDSurfaceDesc = ^TDDSurfaceDesc;
{$IFDEF DIRECTX5}
  TDDSurfaceDesc = TDDSurfaceDesc_DX5;
{$ELSE}
  TDDSurfaceDesc = TDDSurfaceDesc_DX6;
{$ENDIF}


(*
 * TDDSurfaceDesc2
 *)
  PDDSurfaceDesc2 = ^TDDSurfaceDesc2;
  TDDSurfaceDesc2 = packed record
    dwSize: DWORD;                 // size of the TDDSurfaceDesc structure
    dwFlags: DWORD;                // determines what fields are valid
    dwHeight: DWORD;               // height of surface to be created
    dwWidth: DWORD;                // width of input surface
    case Integer of
    0: (
      lPitch : LongInt;                  // distance to start of next line (return value only)
     );
    1: (
      dwLinearSize : DWORD;              // Formless late-allocated optimized surface size
      dwBackBufferCount: DWORD;          // number of back buffers requested
      case Integer of
      0: (
        dwMipMapCount: DWORD;            // number of mip-map levels requested
        dwAlphaBitDepth: DWORD;          // depth of alpha buffer requested
        dwReserved: DWORD;               // reserved
        lpSurface: Pointer;              // pointer to the associated surface memory
        ddckCKDestOverlay: TDDColorKey;  // color key for destination overlay use
        ddckCKDestBlt: TDDColorKey;      // color key for destination blt use
        ddckCKSrcOverlay: TDDColorKey;   // color key for source overlay use
        ddckCKSrcBlt: TDDColorKey;       // color key for source blt use
        ddpfPixelFormat: TDDPixelFormat; // pixel format description of the surface
        ddsCaps: TDDSCaps2;              // direct draw surface capabilities
        dwTextureStage: DWORD;           // stage in multitexture cascade
       );
      1: (
        dwRefreshRate: DWORD;          // refresh rate (used when display mode is described)
       );
     );
  end;

(*
 * TDDOptSurfaceDesc
 *)

  PDDOptSurfaceDesc = ^TDDOptSurfaceDesc;
  TDDOptSurfaceDesc = packed record
    dwSize : DWORD;             // size of the DDOPTSURFACEDESC structure
    dwFlags : DWORD;            // determines what fields are valid
    ddSCaps : TDDSCaps2;        // Common caps like: Memory type
    ddOSCaps : TDDOSCaps;       // Common caps like: Memory type
    guid : TGUID;               // Compression technique GUID
    dwCompressionRatio : DWORD; // Compression ratio
  end;

(*
 * DDCOLORCONTROL
 *)
  PDDColorControl = ^TDDColorControl;
  TDDColorControl = packed record
    dwSize: DWORD;
    dwFlags: DWORD;
    lBrightness: LongInt;
    lContrast: LongInt;
    lHue: LongInt;
    lSaturation: LongInt;
    lSharpness: LongInt;
    lGamma: LongInt;
    lColorEnable: LongInt;
    dwReserved1: DWORD;
  end;

(*
 * callbacks
 *)

{$IFNDEF WINNT}
  TDDEnumModesCallback = function (const lpDDSurfaceDesc: TDDSurfaceDesc;
      lpContext: Pointer) : HResult; stdcall;
  TDDEnumModesCallback2 = function (const lpDDSurfaceDesc: TDDSurfaceDesc2;
      lpContext: Pointer) : HResult; stdcall;
  TDDEnumSurfacesCallback = function (lpDDSurface: IDirectDrawSurface;
      const lpDDSurfaceDesc: TDDSurfaceDesc; lpContext: Pointer) : HResult; stdcall;
  TDDEnumSurfacesCallback2 = function (lpDDSurface: IDirectDrawSurface4;
      const lpDDSurfaceDesc: TDDSurfaceDesc2; lpContext: Pointer) : HResult; stdcall;
  TDDEnumSurfacesCallback7 = function (lpDDSurface: IDirectDrawSurface7;
      const lpDDSurfaceDesc: TDDSurfaceDesc2; lpContext: Pointer) : HResult; stdcall;
{$ENDIF}

(*
 * INTERACES FOLLOW:
 *      IDirectDraw
 *      IDirectDrawClipper
 *      IDirectDrawPalette
 *      IDirectDrawSurface
 *)

(*
 * IDirectDraw
 *)

  IDirectDraw = interface (IUnknown)
    ['{6C14DB80-A733-11CE-A521-0020AF0BE560}']
    (*** IDirectDraw methods ***)
    function Compact: HResult; stdcall;
    function CreateClipper (dwFlags: DWORD;
        out lplpDDClipper: IDirectDrawClipper;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function CreatePalette (dwFlags: DWORD; lpColorTable: pointer;
        out lplpDDPalette: IDirectDrawPalette;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function CreateSurface (var lpDDSurfaceDesc: TDDSurfaceDesc;
        out lplpDDSurface: IDirectDrawSurface;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function DuplicateSurface (lpDDSurface: IDirectDrawSurface;
        out lplpDupDDSurface: IDirectDrawSurface) : HResult; stdcall;
    function EnumDisplayModes (dwFlags: DWORD;
        lpDDSurfaceDesc: PDDSurfaceDesc; lpContext: Pointer;
        lpEnumModesCallback: TDDEnumModesCallback) : HResult; stdcall;
    function EnumSurfaces (dwFlags: DWORD; const lpDDSD: TDDSurfaceDesc;
        lpContext: Pointer; lpEnumCallback: TDDEnumSurfacesCallback) :
        HResult; stdcall;
    function FlipToGDISurface: HResult; stdcall;
    function GetCaps (lpDDDriverCaps: PDDCaps; lpDDHELCaps: PDDCaps) : HResult; stdcall;
    function GetDisplayMode (out lpDDSurfaceDesc: TDDSurfaceDesc) : HResult; stdcall;
    function GetFourCCCodes (var lpNumCodes: DWORD; lpCodes: PDWORD) : HResult; stdcall;
    function GetGDISurface (out lplpGDIDDSSurface: IDirectDrawSurface) :
        HResult; stdcall;
    function GetMonitorFrequency (out lpdwFrequency: DWORD) : HResult; stdcall;
    function GetScanLine (out lpdwScanLine: DWORD) : HResult; stdcall;
    function GetVerticalBlankStatus (out lpbIsInVB: BOOL) : HResult; stdcall;
    function Initialize (lpGUID: PGUID) : HResult; stdcall;
    function RestoreDisplayMode: HResult; stdcall;
    function SetCooperativeLevel (hWnd: HWND; dwFlags: DWORD) : HResult; stdcall;
    (*** Warning!  SetDisplayMode differs between DirectDraw 1 and DirectDraw 2 ***)
    function SetDisplayMode (dwWidth: DWORD; dwHeight: DWORD;
        dwBpp: DWORD) : HResult; stdcall;
    function WaitForVerticalBlank (dwFlags: DWORD; hEvent: THandle) :
        HResult; stdcall;
  end;

  IDirectDraw2 = interface (IUnknown)
    ['{B3A6F3E0-2B43-11CF-A2DE-00AA00B93356}']
    (*** IDirectDraw methods ***)
    function Compact: HResult; stdcall;
    function CreateClipper (dwFlags: DWORD;
        out lplpDDClipper: IDirectDrawClipper;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function CreatePalette (dwFlags: DWORD; lpColorTable: pointer;
        out lplpDDPalette: IDirectDrawPalette;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function CreateSurface (var lpDDSurfaceDesc: TDDSurfaceDesc;
        out lplpDDSurface: IDirectDrawSurface;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function DuplicateSurface (lpDDSurface: IDirectDrawSurface;
        out lplpDupDDSurface: IDirectDrawSurface) : HResult; stdcall;
    function EnumDisplayModes (dwFlags: DWORD;
        lpDDSurfaceDesc: PDDSurfaceDesc; lpContext: Pointer;
        lpEnumModesCallback: TDDEnumModesCallback) : HResult; stdcall;
    function EnumSurfaces (dwFlags: DWORD; var lpDDSD: TDDSurfaceDesc;
        lpContext: Pointer; lpEnumCallback: TDDEnumSurfacesCallback) :
        HResult; stdcall;
    function FlipToGDISurface: HResult; stdcall;
    function GetCaps (lpDDDriverCaps: PDDCaps; lpDDHELCaps: PDDCaps) : HResult; stdcall;
    function GetDisplayMode (out lpDDSurfaceDesc: TDDSurfaceDesc) : HResult; stdcall;
    function GetFourCCCodes (var lpNumCodes: DWORD; lpCodes: PDWORD) : HResult; stdcall;
    function GetGDISurface (out lplpGDIDDSSurface: IDirectDrawSurface) : HResult; stdcall;
    function GetMonitorFrequency (out lpdwFrequency: DWORD) : HResult; stdcall;
    function GetScanLine (out lpdwScanLine: DWORD) : HResult; stdcall;
    function GetVerticalBlankStatus (out lpbIsInVB: BOOL) : HResult; stdcall;
    function Initialize (lpGUID: PGUID) : HResult; stdcall;
    function RestoreDisplayMode: HResult; stdcall;
    function SetCooperativeLevel (hWnd: HWND; dwFlags: DWORD) : HResult; stdcall;
(*** Warning!  SetDisplayMode differs between DirectDraw 1 and DirectDraw 2 ***)
    function SetDisplayMode (dwWidth: DWORD; dwHeight: DWORD; dwBPP: DWORD;
        dwRefreshRate: DWORD; dwFlags: DWORD) : HResult; stdcall;
    function WaitForVerticalBlank (dwFlags: DWORD; hEvent: THandle) :
        HResult; stdcall;
    (*** Added in the v2 interface ***)
    function GetAvailableVidMem (var lpDDSCaps: TDDSCaps;
        out lpdwTotal, lpdwFree: DWORD) : HResult; stdcall;
  end;

  IDirectDraw4 = interface (IUnknown)
    ['{9c59509a-39bd-11d1-8c4a-00c04fd930c5}']
    (*** IDirectDraw methods ***)
    function Compact: HResult; stdcall;
    function CreateClipper (dwFlags: DWORD;
        out lplpDDClipper: IDirectDrawClipper;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function CreatePalette (dwFlags: DWORD; lpColorTable: pointer;
        out lplpDDPalette: IDirectDrawPalette;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function CreateSurface (const lpDDSurfaceDesc: TDDSurfaceDesc2;
        out lplpDDSurface: IDirectDrawSurface4;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function DuplicateSurface (lpDDSurface: IDirectDrawSurface4;
        out lplpDupDDSurface: IDirectDrawSurface4) : HResult; stdcall;
    function EnumDisplayModes (dwFlags: DWORD;
        lpDDSurfaceDesc: PDDSurfaceDesc2; lpContext: Pointer;
        lpEnumModesCallback: TDDEnumModesCallback2) : HResult; stdcall;
    function EnumSurfaces (dwFlags: DWORD; const lpDDSD: TDDSurfaceDesc2;
        lpContext: Pointer; lpEnumCallback: TDDEnumSurfacesCallback2) :
        HResult; stdcall;
    function FlipToGDISurface: HResult; stdcall;
    function GetCaps (lpDDDriverCaps: PDDCaps; lpDDHELCaps: PDDCaps) : HResult; stdcall;
    function GetDisplayMode (out lpDDSurfaceDesc: TDDSurfaceDesc2) : HResult; stdcall;
    function GetFourCCCodes (var lpNumCodes: DWORD; lpCodes: PDWORD) : HResult; stdcall;
    function GetGDISurface (out lplpGDIDDSSurface: IDirectDrawSurface4) :
        HResult; stdcall;
    function GetMonitorFrequency (out lpdwFrequency: DWORD) : HResult; stdcall;
    function GetScanLine (out lpdwScanLine: DWORD) : HResult; stdcall;
    function GetVerticalBlankStatus (out lpbIsInVB: BOOL) : HResult; stdcall;
    function Initialize (lpGUID: PGUID) : HResult; stdcall;
    function RestoreDisplayMode: HResult; stdcall;
    function SetCooperativeLevel (hWnd: HWND; dwFlags: DWORD) : HResult; stdcall;
(*** Warning!  SetDisplayMode differs between DirectDraw 1 and DirectDraw 2 ***)
    function SetDisplayMode (dwWidth: DWORD; dwHeight: DWORD; dwBPP: DWORD;
        dwRefreshRate: DWORD; dwFlags: DWORD) : HResult; stdcall;
    function WaitForVerticalBlank (dwFlags: DWORD; hEvent: THandle) :
        HResult; stdcall;
    (*** Added in the v2 interface ***)
    function GetAvailableVidMem (const lpDDSCaps: TDDSCaps2;
        out lpdwTotal, lpdwFree: DWORD) : HResult; stdcall;
    (*** Added in the V4 Interface ***)
    function GetSurfaceFromDC (hdc : Windows.HDC;
        out lpDDS4: IDirectDrawSurface4) : HResult; stdcall;
    function RestoreAllSurfaces : HResult; stdcall;
    function TestCooperativeLevel : HResult; stdcall;
    function GetDeviceIdentifier (out lpdddi: TDDDeviceIdentifier;
        dwFlags: DWORD) : HResult; stdcall;
  end;

  IDirectDraw7 = interface (IUnknown)
    ['{15e65ec0-3b9c-11d2-b92f-00609797ea5b}']
    (*** IDirectDraw methods ***)
    function Compact: HResult; stdcall;
    function CreateClipper (dwFlags: DWORD;
        out lplpDDClipper: IDirectDrawClipper;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function CreatePalette (dwFlags: DWORD; lpColorTable: pointer;
        out lplpDDPalette: IDirectDrawPalette;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function CreateSurface (const lpDDSurfaceDesc: TDDSurfaceDesc2;
        out lplpDDSurface: IDirectDrawSurface7;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function DuplicateSurface (lpDDSurface: IDirectDrawSurface7;
        out lplpDupDDSurface: IDirectDrawSurface7) : HResult; stdcall;
    function EnumDisplayModes (dwFlags: DWORD;
        lpDDSurfaceDesc: PDDSurfaceDesc2; lpContext: Pointer;
        lpEnumModesCallback: TDDEnumModesCallback2) : HResult; stdcall;
    function EnumSurfaces (dwFlags: DWORD; const lpDDSD: TDDSurfaceDesc2;
        lpContext: Pointer; lpEnumCallback: TDDEnumSurfacesCallback7) :
        HResult; stdcall;
    function FlipToGDISurface: HResult; stdcall;
    function GetCaps (lpDDDriverCaps: PDDCaps; lpDDHELCaps: PDDCaps) : HResult; stdcall;
    function GetDisplayMode (out lpDDSurfaceDesc: TDDSurfaceDesc2) : HResult; stdcall;
    function GetFourCCCodes (var lpNumCodes: DWORD; lpCodes: PDWORD) : HResult; stdcall;
    function GetGDISurface (out lplpGDIDDSSurface: IDirectDrawSurface7) :
        HResult; stdcall;
    function GetMonitorFrequency (out lpdwFrequency: DWORD) : HResult; stdcall;
    function GetScanLine (out lpdwScanLine: DWORD) : HResult; stdcall;
    function GetVerticalBlankStatus (out lpbIsInVB: BOOL) : HResult; stdcall;
    function Initialize (lpGUID: PGUID) : HResult; stdcall;
    function RestoreDisplayMode: HResult; stdcall;
    function SetCooperativeLevel (hWnd: HWND; dwFlags: DWORD) : HResult; stdcall;
    function SetDisplayMode (dwWidth: DWORD; dwHeight: DWORD; dwBPP: DWORD;
        dwRefreshRate: DWORD; dwFlags: DWORD) : HResult; stdcall;
    function WaitForVerticalBlank (dwFlags: DWORD; hEvent: THandle) :
        HResult; stdcall;
    (*** Added in the v2 interface ***)
    function GetAvailableVidMem (const lpDDSCaps: TDDSCaps2;
        out lpdwTotal, lpdwFree: DWORD) : HResult; stdcall;
    (*** Added in the V4 Interface ***)
    function GetSurfaceFromDC (hdc : Windows.HDC;
        out lpDDS: IDirectDrawSurface7) : HResult; stdcall;
    function RestoreAllSurfaces : HResult; stdcall;
    function TestCooperativeLevel : HResult; stdcall;
    function GetDeviceIdentifier (out lpdddi: TDDDeviceIdentifier2;
        dwFlags: DWORD) : HResult; stdcall;
    function StartModeTest(const lpModesToTest; dwNumEntries, dwFlags: DWORD) : HResult; stdcall;
    function EvaluateMode(dwFlags: DWORD; out pSecondsUntilTimeout: DWORD) : HResult; stdcall;
  end;



(*
 * IDirectDrawPalette
 *)

  IDirectDrawPalette = interface (IUnknown)
    ['{6C14DB84-A733-11CE-A521-0020AF0BE560}']
    (*** IDirectDrawPalette methods ***)
    function GetCaps (out lpdwCaps: DWORD) : HResult; stdcall;
    function GetEntries (dwFlags: DWORD; dwBase: DWORD; dwNumEntries: DWORD;
        lpEntries: pointer) : HResult; stdcall;
    function Initialize (lpDD: IDirectDraw; dwFlags: DWORD;
        lpDDColorTable: pointer) : HResult; stdcall;
    function SetEntries (dwFlags: DWORD; dwStartingEntry: DWORD;
        dwCount: DWORD; lpEntries: pointer) : HResult; stdcall;
  end;

(*
 * IDirectDrawClipper
 *)

  IDirectDrawClipper = interface (IUnknown)
    ['{6C14DB85-A733-11CE-A521-0020AF0BE560}']
    (*** IDirectDrawClipper methods ***)
    function GetClipList (lpRect: PRect; lpClipList: PRgnData;
        var lpdwSize: DWORD) : HResult; stdcall;
    function GetHWnd (out lphWnd: HWND) : HResult; stdcall;
    function Initialize (lpDD: IDirectDraw; dwFlags: DWORD) : HResult; stdcall;
    function IsClipListChanged (out lpbChanged: BOOL) : HResult; stdcall;
    function SetClipList (lpClipList: PRgnData; dwFlags: DWORD) : HResult; stdcall;
    function SetHWnd (dwFlags: DWORD; hWnd: HWND) : HResult; stdcall;
  end;

(*
 * IDirectDrawSurface and related interfaces
 *)

  IDirectDrawSurface = interface (IUnknown)
    ['{6C14DB81-A733-11CE-A521-0020AF0BE560}']
    (*** IDirectDrawSurface methods ***)
    function AddAttachedSurface (lpDDSAttachedSurface: IDirectDrawSurface) :
        HResult; stdcall;
    function AddOverlayDirtyRect (const lpRect: TRect) : HResult; stdcall;
    function Blt (lpDestRect: PRect;
        lpDDSrcSurface: IDirectDrawSurface; lpSrcRect: PRect;
        dwFlags: DWORD; lpDDBltFx: PDDBltFX) : HResult; stdcall;
    function BltBatch (const lpDDBltBatch: TDDBltBatch; dwCount: DWORD;
        dwFlags: DWORD) : HResult; stdcall;
    function BltFast (dwX: DWORD; dwY: DWORD;
        lpDDSrcSurface: IDirectDrawSurface; lpSrcRect: PRect;
        dwTrans: DWORD) : HResult; stdcall;
    function DeleteAttachedSurface (dwFlags: DWORD;
        lpDDSAttachedSurface: IDirectDrawSurface) : HResult; stdcall;
    function EnumAttachedSurfaces (lpContext: Pointer;
        lpEnumSurfacesCallback: TDDEnumSurfacesCallback) : HResult; stdcall;
    function EnumOverlayZOrders (dwFlags: DWORD; lpContext: Pointer;
        lpfnCallback: TDDEnumSurfacesCallback) : HResult; stdcall;
    function Flip (lpDDSurfaceTargetOverride: IDirectDrawSurface;
        dwFlags: DWORD) : HResult; stdcall;
    function GetAttachedSurface (var lpDDSCaps: TDDSCaps;
        (*out*)var lplpDDAttachedSurface: IDirectDrawSurface) : HResult; stdcall;
    function GetBltStatus (dwFlags: DWORD) : HResult; stdcall;
    function GetCaps (out lpDDSCaps: TDDSCaps) : HResult; stdcall;
    function GetClipper (out lplpDDClipper: IDirectDrawClipper) : HResult; stdcall;
    function GetColorKey (dwFlags: DWORD; out lpDDColorKey: TDDColorKey) :
        HResult; stdcall;
    function GetDC (out lphDC: HDC) : HResult; stdcall;
    function GetFlipStatus (dwFlags: DWORD) : HResult; stdcall;
    function GetOverlayPosition (out lplX, lplY: LongInt) : HResult; stdcall;
    function GetPalette (out lplpDDPalette: IDirectDrawPalette) : HResult; stdcall;
    function GetPixelFormat (out lpDDPixelFormat: TDDPixelFormat) : HResult; stdcall;
    function GetSurfaceDesc (out lpDDSurfaceDesc: TDDSurfaceDesc) : HResult; stdcall;
    function Initialize (lpDD: IDirectDraw;
        out lpDDSurfaceDesc: TDDSurfaceDesc) : HResult; stdcall;
    function IsLost: HResult; stdcall;
    function Lock (lpDestRect: PRect; out lpDDSurfaceDesc:
        TDDSurfaceDesc; dwFlags: DWORD; hEvent: THandle) : HResult; stdcall;
    function ReleaseDC (hDC: Windows.HDC) : HResult; stdcall;
    function _Restore: HResult; stdcall;
    function SetClipper (lpDDClipper: IDirectDrawClipper) : HResult; stdcall;
    function SetColorKey (dwFlags: DWORD; lpDDColorKey: PDDColorKey) :
        HResult; stdcall;
    function SetOverlayPosition (lX, lY: LongInt) : HResult; stdcall;
    function SetPalette (lpDDPalette: IDirectDrawPalette) : HResult; stdcall;
    function Unlock (lpSurfaceData: Pointer) : HResult; stdcall;
    function UpdateOverlay (lpSrcRect: PRect;
        lpDDDestSurface: IDirectDrawSurface; lpDestRect: PRect;
        dwFlags: DWORD; lpDDOverlayFx: PDDOverlayFX) : HResult; stdcall;
    function UpdateOverlayDisplay (dwFlags: DWORD) : HResult; stdcall;
    function UpdateOverlayZOrder (dwFlags: DWORD;
        lpDDSReference: IDirectDrawSurface) : HResult; stdcall;
  end;

(*
 * IDirectDrawSurface2 and related interfaces
 *)

  IDirectDrawSurface2 = interface (IUnknown)
    ['{57805885-6eec-11cf-9441-a82303c10e27}']
    (*** IDirectDrawSurface methods ***)
    function AddAttachedSurface (lpDDSAttachedSurface: IDirectDrawSurface2) :
        HResult; stdcall;
    function AddOverlayDirtyRect (const lpRect: TRect) : HResult; stdcall;
    function Blt (lpDestRect: PRect;
        lpDDSrcSurface: IDirectDrawSurface2; lpSrcRect: PRect;
        dwFlags: DWORD; lpDDBltFx: PDDBltFX) : HResult; stdcall;
    function BltBatch (const lpDDBltBatch: TDDBltBatch; dwCount: DWORD;
        dwFlags: DWORD) : HResult; stdcall;
    function BltFast (dwX: DWORD; dwY: DWORD;
        lpDDSrcSurface: IDirectDrawSurface2; lpSrcRect: PRect;
        dwTrans: DWORD) : HResult; stdcall;
    function DeleteAttachedSurface (dwFlags: DWORD;
        lpDDSAttachedSurface: IDirectDrawSurface2) : HResult; stdcall;
    function EnumAttachedSurfaces (lpContext: Pointer;
        lpEnumSurfacesCallback: TDDEnumSurfacesCallback) : HResult; stdcall;
    function EnumOverlayZOrders (dwFlags: DWORD; lpContext: Pointer;
        lpfnCallback: TDDEnumSurfacesCallback) : HResult; stdcall;
    function Flip (lpDDSurfaceTargetOverride: IDirectDrawSurface2;
        dwFlags: DWORD) : HResult; stdcall;
    function GetAttachedSurface (var lpDDSCaps: TDDSCaps;
        out lplpDDAttachedSurface: IDirectDrawSurface2) : HResult; stdcall;
    function GetBltStatus (dwFlags: DWORD) : HResult; stdcall;
    function GetCaps (out lpDDSCaps: TDDSCaps) : HResult; stdcall;
    function GetClipper (out lplpDDClipper: IDirectDrawClipper) : HResult; stdcall;
    function GetColorKey (dwFlags: DWORD; out lpDDColorKey: TDDColorKey) :
        HResult; stdcall;
    function GetDC (out lphDC: HDC) : HResult; stdcall;
    function GetFlipStatus (dwFlags: DWORD) : HResult; stdcall;
    function GetOverlayPosition (out lplX, lplY: LongInt) : HResult; stdcall;
    function GetPalette (out lplpDDPalette: IDirectDrawPalette) : HResult; stdcall;
    function GetPixelFormat (out lpDDPixelFormat: TDDPixelFormat) : HResult; stdcall;
    function GetSurfaceDesc (out lpDDSurfaceDesc: TDDSurfaceDesc) : HResult; stdcall;
    function Initialize (lpDD: IDirectDraw;
        out lpDDSurfaceDesc: TDDSurfaceDesc) : HResult; stdcall;
    function IsLost: HResult; stdcall;
    function Lock (lpDestRect: PRect;
        out lpDDSurfaceDesc: TDDSurfaceDesc; dwFlags: DWORD;
        hEvent: THandle) : HResult; stdcall;
    function ReleaseDC (hDC: Windows.HDC) : HResult; stdcall;
    function _Restore: HResult; stdcall;
    function SetClipper (lpDDClipper: IDirectDrawClipper) : HResult; stdcall;
    function SetColorKey (dwFlags: DWORD; lpDDColorKey: PDDColorKey) :
        HResult; stdcall;
    function SetOverlayPosition (lX, lY: LongInt) : HResult; stdcall;
    function SetPalette (lpDDPalette: IDirectDrawPalette) : HResult; stdcall;
    function Unlock (lpSurfaceData: Pointer) : HResult; stdcall;
    function UpdateOverlay (lpSrcRect: PRect;
        lpDDDestSurface: IDirectDrawSurface2; lpDestRect: PRect;
        dwFlags: DWORD; lpDDOverlayFx: PDDOverlayFX) : HResult; stdcall;
    function UpdateOverlayDisplay (dwFlags: DWORD) : HResult; stdcall;
    function UpdateOverlayZOrder (dwFlags: DWORD;
        lpDDSReference: IDirectDrawSurface2) : HResult; stdcall;
    (*** Added in the v2 interface ***)
    function GetDDInterface (var lplpDD: IDirectDraw) : HResult; stdcall;
    function PageLock (dwFlags: DWORD) : HResult; stdcall;
    function PageUnlock (dwFlags: DWORD) : HResult; stdcall;
  end;

  IDirectDrawSurface3 = interface (IUnknown)
    ['{DA044E00-69B2-11D0-A1D5-00AA00B8DFBB}']
    (*** IDirectDrawSurface methods ***)
    function AddAttachedSurface (lpDDSAttachedSurface: IDirectDrawSurface3) :
        HResult; stdcall;
    function AddOverlayDirtyRect (const lpRect: TRect) : HResult; stdcall;
    function Blt (lpDestRect: PRect;
        lpDDSrcSurface: IDirectDrawSurface3; lpSrcRect: PRect;
        dwFlags: DWORD; lpDDBltFx: PDDBltFX) : HResult; stdcall;
    function BltBatch (const lpDDBltBatch: TDDBltBatch; dwCount: DWORD;
        dwFlags: DWORD) : HResult; stdcall;
    function BltFast (dwX: DWORD; dwY: DWORD;
        lpDDSrcSurface: IDirectDrawSurface3; lpSrcRect: PRect;
        dwTrans: DWORD) : HResult; stdcall;
    function DeleteAttachedSurface (dwFlags: DWORD;
        lpDDSAttachedSurface: IDirectDrawSurface3) : HResult; stdcall;
    function EnumAttachedSurfaces (lpContext: Pointer;
        lpEnumSurfacesCallback: TDDEnumSurfacesCallback) : HResult; stdcall;
    function EnumOverlayZOrders (dwFlags: DWORD; lpContext: Pointer;
        lpfnCallback: TDDEnumSurfacesCallback) : HResult; stdcall;
    function Flip (lpDDSurfaceTargetOverride: IDirectDrawSurface3;
        dwFlags: DWORD) : HResult; stdcall;
    function GetAttachedSurface (var lpDDSCaps: TDDSCaps;
        out lplpDDAttachedSurface: IDirectDrawSurface3) : HResult; stdcall;
    function GetBltStatus (dwFlags: DWORD) : HResult; stdcall;
    function GetCaps (out lpDDSCaps: TDDSCaps) : HResult; stdcall;
    function GetClipper (out lplpDDClipper: IDirectDrawClipper) : HResult; stdcall;
    function GetColorKey (dwFlags: DWORD; out lpDDColorKey: TDDColorKey) :
        HResult; stdcall;
    function GetDC (out lphDC: HDC) : HResult; stdcall;
    function GetFlipStatus (dwFlags: DWORD) : HResult; stdcall;
    function GetOverlayPosition (out lplX, lplY: LongInt) : HResult; stdcall;
    function GetPalette (out lplpDDPalette: IDirectDrawPalette) : HResult; stdcall;
    function GetPixelFormat (out lpDDPixelFormat: TDDPixelFormat) : HResult; stdcall;
    function GetSurfaceDesc (out lpDDSurfaceDesc: TDDSurfaceDesc) : HResult; stdcall;
    function Initialize (lpDD: IDirectDraw;
        out lpDDSurfaceDesc: TDDSurfaceDesc) : HResult; stdcall;
    function IsLost: HResult; stdcall;
    function Lock (lpDestRect: PRect;
        out lpDDSurfaceDesc: TDDSurfaceDesc; dwFlags: DWORD;
        hEvent: THandle) : HResult; stdcall;
    function ReleaseDC (hDC: Windows.HDC) : HResult; stdcall;
    function _Restore: HResult; stdcall;
    function SetClipper (lpDDClipper: IDirectDrawClipper) : HResult; stdcall;
    function SetColorKey (dwFlags: DWORD; lpDDColorKey: PDDColorKey) :
        HResult; stdcall;
    function SetOverlayPosition (lX, lY: LongInt) : HResult; stdcall;
    function SetPalette (lpDDPalette: IDirectDrawPalette) : HResult; stdcall;
    function Unlock (lpSurfaceData: Pointer) : HResult; stdcall;
    function UpdateOverlay (lpSrcRect: PRect;
        lpDDDestSurface: IDirectDrawSurface3; lpDestRect: PRect;
        dwFlags: DWORD; lpDDOverlayFx: PDDOverlayFX) : HResult; stdcall;
    function UpdateOverlayDisplay (dwFlags: DWORD) : HResult; stdcall;
    function UpdateOverlayZOrder (dwFlags: DWORD;
        lpDDSReference: IDirectDrawSurface3) : HResult; stdcall;
    (*** Added in the v2 interface ***)
    function GetDDInterface (out lplpDD: IDirectDraw) : HResult; stdcall;
    function PageLock (dwFlags: DWORD) : HResult; stdcall;
    function PageUnlock (dwFlags: DWORD) : HResult; stdcall;
    (*** Added in the V3 interface ***)
    function SetSurfaceDesc(const lpddsd: TDDSurfaceDesc; dwFlags: DWORD) : HResult; stdcall;
  end;

(*
 * IDirectDrawSurface4 and related interfaces
 *)
  IDirectDrawSurface4 = interface (IUnknown)
    ['{0B2B8630-AD35-11D0-8EA6-00609797EA5B}']
    (*** IDirectDrawSurface methods ***)
    function AddAttachedSurface (lpDDSAttachedSurface: IDirectDrawSurface4) :
        HResult; stdcall;
    function AddOverlayDirtyRect (const lpRect: TRect) : HResult; stdcall;
    function Blt (lpDestRect: PRect;
        lpDDSrcSurface: IDirectDrawSurface4; lpSrcRect: PRect;
        dwFlags: DWORD; lpDDBltFx: PDDBltFX) : HResult; stdcall;
    function BltBatch (const lpDDBltBatch: TDDBltBatch; dwCount: DWORD;
        dwFlags: DWORD) : HResult; stdcall;
    function BltFast (dwX: DWORD; dwY: DWORD;
        lpDDSrcSurface: IDirectDrawSurface4; lpSrcRect: PRect;
        dwTrans: DWORD) : HResult; stdcall;
    function DeleteAttachedSurface (dwFlags: DWORD;
        lpDDSAttachedSurface: IDirectDrawSurface4) : HResult; stdcall;
    function EnumAttachedSurfaces (lpContext: Pointer;
        lpEnumSurfacesCallback: TDDEnumSurfacesCallback2) : HResult; stdcall;
    function EnumOverlayZOrders (dwFlags: DWORD; lpContext: Pointer;
        lpfnCallback: TDDEnumSurfacesCallback2) : HResult; stdcall;
    function Flip (lpDDSurfaceTargetOverride: IDirectDrawSurface4;
        dwFlags: DWORD) : HResult; stdcall;
    function GetAttachedSurface (const lpDDSCaps: TDDSCaps2;
        out lplpDDAttachedSurface: IDirectDrawSurface4) : HResult; stdcall;
    function GetBltStatus (dwFlags: DWORD) : HResult; stdcall;
    function GetCaps (out lpDDSCaps: TDDSCaps2) : HResult; stdcall;
    function GetClipper (out lplpDDClipper: IDirectDrawClipper) : HResult; stdcall;
    function GetColorKey (dwFlags: DWORD; out lpDDColorKey: TDDColorKey) :
        HResult; stdcall;
    function GetDC (out lphDC: HDC) : HResult; stdcall;
    function GetFlipStatus (dwFlags: DWORD) : HResult; stdcall;
    function GetOverlayPosition (out lplX, lplY: LongInt) : HResult; stdcall;
    function GetPalette (out lplpDDPalette: IDirectDrawPalette) : HResult; stdcall;
    function GetPixelFormat (out lpDDPixelFormat: TDDPixelFormat) : HResult; stdcall;
    function GetSurfaceDesc (out lpDDSurfaceDesc: TDDSurfaceDesc2) : HResult; stdcall;
    function Initialize (lpDD: IDirectDraw;
        out lpDDSurfaceDesc: TDDSurfaceDesc2) : HResult; stdcall;
    function IsLost: HResult; stdcall;
    function Lock (lpDestRect: PRect;
        out lpDDSurfaceDesc: TDDSurfaceDesc2; dwFlags: DWORD;
        hEvent: THandle) : HResult; stdcall;
    function ReleaseDC (hDC: Windows.HDC) : HResult; stdcall;
    function _Restore: HResult; stdcall;
    function SetClipper (lpDDClipper: IDirectDrawClipper) : HResult; stdcall;
    function SetColorKey (dwFlags: DWORD; lpDDColorKey: PDDColorKey) :
        HResult; stdcall;
    function SetOverlayPosition (lX, lY: LongInt) : HResult; stdcall;
    function SetPalette (lpDDPalette: IDirectDrawPalette) : HResult; stdcall;
    function Unlock (lpRect: PRect) : HResult; stdcall;
    function UpdateOverlay (lpSrcRect: PRect;
        lpDDDestSurface: IDirectDrawSurface4; lpDestRect: PRect;
        dwFlags: DWORD; lpDDOverlayFx: PDDOverlayFX) : HResult; stdcall;
    function UpdateOverlayDisplay (dwFlags: DWORD) : HResult; stdcall;
    function UpdateOverlayZOrder (dwFlags: DWORD;
        lpDDSReference: IDirectDrawSurface4) : HResult; stdcall;
    (*** Added in the v2 interface ***)
    function GetDDInterface (out lplpDD: IUnknown) : HResult; stdcall;
    function PageLock (dwFlags: DWORD) : HResult; stdcall;
    function PageUnlock (dwFlags: DWORD) : HResult; stdcall;
    (*** Added in the V3 interface ***)
    function SetSurfaceDesc(const lpddsd2: TDDSurfaceDesc2; dwFlags: DWORD) : HResult; stdcall;
    (*** Added in the v4 interface ***)
    function SetPrivateData(const guidTag: TGUID; lpData: pointer;
        cbSize: DWORD; dwFlags: DWORD) : HResult; stdcall;
    function GetPrivateData(const guidTag: TGUID; lpBuffer: pointer;
        var lpcbBufferSize: DWORD) : HResult; stdcall;
    function FreePrivateData(const guidTag: TGUID) : HResult; stdcall;
    function GetUniquenessValue(out lpValue: DWORD) : HResult; stdcall;
    function ChangeUniquenessValue : HResult; stdcall;
  end;

  IDirectDrawSurface7 = interface (IUnknown)
    ['{06675a80-3b9b-11d2-b92f-00609797ea5b}']
    (*** IDirectDrawSurface methods ***)
    function AddAttachedSurface (lpDDSAttachedSurface: IDirectDrawSurface7) :
        HResult; stdcall;
    function AddOverlayDirtyRect (const lpRect: TRect) : HResult; stdcall;
    function Blt (lpDestRect: PRect;
        lpDDSrcSurface: IDirectDrawSurface7; lpSrcRect: PRect;
        dwFlags: DWORD; lpDDBltFx: PDDBltFX) : HResult; stdcall;
    function BltBatch (const lpDDBltBatch: TDDBltBatch; dwCount: DWORD;
        dwFlags: DWORD) : HResult; stdcall;
    function BltFast (dwX: DWORD; dwY: DWORD;
        lpDDSrcSurface: IDirectDrawSurface7; lpSrcRect: PRect;
        dwTrans: DWORD) : HResult; stdcall;
    function DeleteAttachedSurface (dwFlags: DWORD;
        lpDDSAttachedSurface: IDirectDrawSurface7) : HResult; stdcall;
    function EnumAttachedSurfaces (lpContext: Pointer;
        lpEnumSurfacesCallback: TDDEnumSurfacesCallback2) : HResult; stdcall;
    function EnumOverlayZOrders (dwFlags: DWORD; lpContext: Pointer;
        lpfnCallback: TDDEnumSurfacesCallback2) : HResult; stdcall;
    function Flip (lpDDSurfaceTargetOverride: IDirectDrawSurface7;
        dwFlags: DWORD) : HResult; stdcall;
    function GetAttachedSurface (const lpDDSCaps: TDDSCaps2;
        out lplpDDAttachedSurface: IDirectDrawSurface7) : HResult; stdcall;
    function GetBltStatus (dwFlags: DWORD) : HResult; stdcall;
    function GetCaps (out lpDDSCaps: TDDSCaps2) : HResult; stdcall;
    function GetClipper (out lplpDDClipper: IDirectDrawClipper) : HResult; stdcall;
    function GetColorKey (dwFlags: DWORD; out lpDDColorKey: TDDColorKey) :
        HResult; stdcall;
    function GetDC (out lphDC: HDC) : HResult; stdcall;
    function GetFlipStatus (dwFlags: DWORD) : HResult; stdcall;
    function GetOverlayPosition (out lplX, lplY: LongInt) : HResult; stdcall;
    function GetPalette (out lplpDDPalette: IDirectDrawPalette) : HResult; stdcall;
    function GetPixelFormat (out lpDDPixelFormat: TDDPixelFormat) : HResult; stdcall;
    function GetSurfaceDesc (out lpDDSurfaceDesc: TDDSurfaceDesc2) : HResult; stdcall;
    function Initialize (lpDD: IDirectDraw;
        out lpDDSurfaceDesc: TDDSurfaceDesc2) : HResult; stdcall;
    function IsLost: HResult; stdcall;
    function Lock (lpDestRect: PRect;
        out lpDDSurfaceDesc: TDDSurfaceDesc2; dwFlags: DWORD;
        hEvent: THandle) : HResult; stdcall;
    function ReleaseDC (hDC: Windows.HDC) : HResult; stdcall;
    function _Restore: HResult; stdcall;
    function SetClipper (lpDDClipper: IDirectDrawClipper) : HResult; stdcall;
    function SetColorKey (dwFlags: DWORD; lpDDColorKey: PDDColorKey) :
        HResult; stdcall;
    function SetOverlayPosition (lX, lY: LongInt) : HResult; stdcall;
    function SetPalette (lpDDPalette: IDirectDrawPalette) : HResult; stdcall;
    function Unlock (lpRect: PRect) : HResult; stdcall;
    function UpdateOverlay (lpSrcRect: PRect;
        lpDDDestSurface: IDirectDrawSurface7; lpDestRect: PRect;
        dwFlags: DWORD; lpDDOverlayFx: PDDOverlayFX) : HResult; stdcall;
    function UpdateOverlayDisplay (dwFlags: DWORD) : HResult; stdcall;
    function UpdateOverlayZOrder (dwFlags: DWORD;
        lpDDSReference: IDirectDrawSurface7) : HResult; stdcall;
    (*** Added in the v2 interface ***)
    function GetDDInterface (out lplpDD: IUnknown) : HResult; stdcall;
    function PageLock (dwFlags: DWORD) : HResult; stdcall;
    function PageUnlock (dwFlags: DWORD) : HResult; stdcall;
    (*** Added in the V3 interface ***)
    function SetSurfaceDesc(const lpddsd2: TDDSurfaceDesc2; dwFlags: DWORD) : HResult; stdcall;
    (*** Added in the v4 interface ***)
    function SetPrivateData(const guidTag: TGUID; lpData: pointer;
        cbSize: DWORD; dwFlags: DWORD) : HResult; stdcall;
    function GetPrivateData(const guidTag: TGUID; lpBuffer: pointer;
        var lpcbBufferSize: DWORD) : HResult; stdcall;
    function FreePrivateData(const guidTag: TGUID) : HResult; stdcall;
    function GetUniquenessValue(out lpValue: DWORD) : HResult; stdcall;
    function ChangeUniquenessValue : HResult; stdcall;
    (*** Moved Texture7 methods here ***)
    function SetPriority(dwPriority: DWORD) : HResult; stdcall;
    function GetPriority(out lpdwPriority: DWORD) : HResult; stdcall;
    function SetLOD(dwMaxLOD: DWORD) : HResult; stdcall;
    function GetLOD(out lpdwMaxLOD: DWORD) : HResult; stdcall;
  end;

  IDirectDrawColorControl = interface (IUnknown)
    ['{4B9F0EE0-0D7E-11D0-9B06-00A0C903A3B8}']
    function GetColorControls(out lpColorControl: TDDColorControl) : HResult; stdcall;
    function SetColorControls(const lpColorControl: TDDColorControl) : HResult; stdcall;
  end;

(*
 * IDirectDrawGammaControl
 *)
  IDirectDrawGammaControl = interface (IUnknown)
    ['{69C11C3E-B46B-11D1-AD7A-00C04FC29B4E}']
    function GetGammaRamp (dwFlags: DWORD; out lpRampData: TDDGammaRamp)
        : HResult; stdcall;
    function SetGammaRamp (dwFlags: DWORD; const lpRampData: TDDGammaRamp)
        : HResult; stdcall;
  end;

type
  IID_IDirectDraw = IDirectDraw;
  IID_IDirectDraw2 = IDirectDraw2;
  IID_IDirectDraw4 = IDirectDraw4;
  IID_IDirectDraw7 = IDirectDraw7;
  IID_IDirectDrawSurface = IDirectDrawSurface;
  IID_IDirectDrawSurface2 = IDirectDrawSurface2;
  IID_IDirectDrawSurface3 = IDirectDrawSurface3;
  IID_IDirectDrawSurface4 = IDirectDrawSurface4;
  IID_IDirectDrawSurface7 = IDirectDrawSurface7;

  IID_IDirectDrawPalette = IDirectDrawPalette;
  IID_IDirectDrawClipper = IDirectDrawClipper;
  IID_IDirectDrawColorControl = IDirectDrawColorControl;
  IID_IDirectDrawGammaControl = IDirectDrawGammaControl;

const  
(*
 * ddsCaps field is valid.
 *)
  DDSD_CAPS               = $00000001;     // default

(*
 * dwHeight field is valid.
 *)
  DDSD_HEIGHT             = $00000002;

(*
 * dwWidth field is valid.
 *)
  DDSD_WIDTH              = $00000004;

(*
 * lPitch is valid.
 *)
  DDSD_PITCH              = $00000008;

(*
 * dwBackBufferCount is valid.
 *)
  DDSD_BACKBUFFERCOUNT    = $00000020;

(*
 * dwZBufferBitDepth is valid.  (shouldnt be used in DDSURFACEDESC2)
 *)
  DDSD_ZBUFFERBITDEPTH    = $00000040;

(*
 * dwAlphaBitDepth is valid.
 *)
   DDSD_ALPHABITDEPTH      = $00000080;

(*
 * lpSurface is valid.
 *)
  DDSD_LPSURFACE	   = $00000800;

(*
 * ddpfPixelFormat is valid.
 *)
  DDSD_PIXELFORMAT        = $00001000;

(*
 * ddckCKDestOverlay is valid.
 *)
  DDSD_CKDESTOVERLAY      = $00002000;

(*
 * ddckCKDestBlt is valid.
 *)
  DDSD_CKDESTBLT          = $00004000;

(*
 * ddckCKSrcOverlay is valid.
 *)
  DDSD_CKSRCOVERLAY       = $00008000;

(*
 * ddckCKSrcBlt is valid.
 *)
  DDSD_CKSRCBLT           = $00010000;

(*
 * dwMipMapCount is valid.
 *)
  DDSD_MIPMAPCOUNT        = $00020000;

 (*
  * dwRefreshRate is valid
  *)
  DDSD_REFRESHRATE        = $00040000;

(*
 * dwLinearSize is valid
 *)
  DDSD_LINEARSIZE	  = $00080000;

(*
 * dwTextureStage is valid
 *)
  DDSD_TEXTURESTAGE       = $00100000;

(*
 * All input fields are valid.
 *)
  DDSD_ALL		  = $001ff9ee;


(*
 * guid field is valid.
 *)
  DDOSD_GUID                  = $00000001;

(*
 * dwCompressionRatio field is valid.
 *)
  DDOSD_COMPRESSION_RATIO     = $00000002;

(*
 * ddSCaps field is valid.
 *)
  DDOSD_SCAPS                 = $00000004;

(*
 * ddOSCaps field is valid.
 *)
  DDOSD_OSCAPS                = $00000008;

(*
 * All input fields are valid.
 *)
  DDOSD_ALL                   = $0000000f;

(*
 * The surface's optimized pixelformat is compressed
 *)
  DDOSDCAPS_OPTCOMPRESSED			= $00000001;

(*
 * The surface's optimized pixelformat is reordered
 *)
  DDOSDCAPS_OPTREORDERED			= $00000002;

(*
 * The opt surface is a monolithic mipmap
 *)
  DDOSDCAPS_MONOLITHICMIPMAP		= $00000004;

(*
 * The valid Surf caps:
 *   DDSCAPS_SYSTEMMEMORY  	= $00000800;
 *   DDSCAPS_VIDEOMEMORY        = $00004000;
 *   DDSCAPS_LOCALVIDMEM        = $10000000;
 *   DDSCAPS_NONLOCALVIDMEM     = $20000000;
 *)
  DDOSDCAPS_VALIDSCAPS         	= $30004800;

(*
 * The valid OptSurf caps
 *)
  DDOSDCAPS_VALIDOSCAPS         	= $00000007;


(*
 * DDCOLORCONTROL
 *)

(*
 * lBrightness field is valid.
 *)
  DDCOLOR_BRIGHTNESS		= $00000001;

(*
 * lContrast field is valid.
 *)
  DDCOLOR_CONTRAST		= $00000002;

(*
 * lHue field is valid.
 *)
  DDCOLOR_HUE			= $00000004;

(*
 * lSaturation field is valid.
 *)
  DDCOLOR_SATURATION		= $00000008;

(*
 * lSharpness field is valid.
 *)
  DDCOLOR_SHARPNESS		= $00000010;

(*
 * lGamma field is valid.
 *)
  DDCOLOR_GAMMA			= $00000020;

(*
 * lColorEnable field is valid.
 *)
  DDCOLOR_COLORENABLE		= $00000040;



(*============================================================================
 *
 * Direct Draw Capability Flags
 *
 * These flags are used to describe the capabilities of a given Surface.
 * All flags are bit flags.
 *
 *==========================================================================*)

(****************************************************************************
 *
 * DIRECTDRAWSURFACE CAPABILITY FLAGS
 *
 ****************************************************************************)
(*
 * This bit currently has no meaning.
 *)
  DDSCAPS_RESERVED1                       = $00000001;

(*
 * Indicates that this surface contains alpha-only information.
 * (To determine if a surface is RGBA/YUVA, the pixel format must be
 * interrogated.)
 *)
  DDSCAPS_ALPHA                           = $00000002;

(*
 * Indicates that this surface is a backbuffer.  It is generally
 * set by CreateSurface when the DDSCAPS_FLIP capability bit is set.
 * It indicates that this surface is THE back buffer of a surface
 * flipping structure.  DirectDraw supports N surfaces in a
 * surface flipping structure.  Only the surface that immediately
 * precedeces the DDSCAPS_FRONTBUFFER has this capability bit set.
 * The other surfaces are identified as back buffers by the presence
 * of the DDSCAPS_FLIP capability, their attachment order, and the
 * absence of the DDSCAPS_FRONTBUFFER and DDSCAPS_BACKBUFFER
 * capabilities.  The bit is sent to CreateSurface when a standalone
 * back buffer is being created.  This surface could be attached to
 * a front buffer and/or back buffers to form a flipping surface
 * structure after the CreateSurface call.  See AddAttachments for
 * a detailed description of the behaviors in this case.
 *)
  DDSCAPS_BACKBUFFER                      = $00000004;

(*
 * Indicates a complex surface structure is being described.  A
 * complex surface structure results in the creation of more than
 * one surface.  The additional surfaces are attached to the root
 * surface.  The complex structure can only be destroyed by
 * destroying the root.
 *)
  DDSCAPS_COMPLEX                         = $00000008;

(*
 * Indicates that this surface is a part of a surface flipping structure.
 * When it is passed to CreateSurface the DDSCAPS_FRONTBUFFER and
 * DDSCAP_BACKBUFFER bits are not set.  They are set by CreateSurface
 * on the resulting creations.  The dwBackBufferCount field in the
 * TDDSurfaceDesc structure must be set to at least 1 in order for
 * the CreateSurface call to succeed.  The DDSCAPS_COMPLEX capability
 * must always be set with creating multiple surfaces through CreateSurface.
 *)
  DDSCAPS_FLIP                            = $00000010;

(*
 * Indicates that this surface is THE front buffer of a surface flipping
 * structure.  It is generally set by CreateSurface when the DDSCAPS_FLIP
 * capability bit is set.
 * If this capability is sent to CreateSurface then a standalonw front buffer
 * is created.  This surface will not have the DDSCAPS_FLIP capability.
 * It can be attached to other back buffers to form a flipping structure.
 * See AddAttachments for a detailed description of the behaviors in this
 * case.
 *)
  DDSCAPS_FRONTBUFFER                     = $00000020;

(*
 * Indicates that this surface is any offscreen surface that is not an overlay,
 * texture, zbuffer, front buffer, back buffer, or alpha surface.  It is used
 * to identify plain vanilla surfaces.
 *)
  DDSCAPS_OFFSCREENPLAIN                  = $00000040;

(*
 * Indicates that this surface is an overlay.  It may or may not be directly visible
 * depending on whether or not it is currently being overlayed onto the primary
 * surface.  DDSCAPS_VISIBLE can be used to determine whether or not it is being
 * overlayed at the moment.
 *)
  DDSCAPS_OVERLAY                         = $00000080;

(*
 * Indicates that unique DirectDrawPalette objects can be created and
 * attached to this surface.
 *)
  DDSCAPS_PALETTE                         = $00000100;

(*
 * Indicates that this surface is the primary surface.  The primary
 * surface represents what the user is seeing at the moment.
 *)
  DDSCAPS_PRIMARYSURFACE                  = $00000200;

(*
 * This flag used to be DDSCAPS_PRIMARYSURFACELEFT, which is now
 * obsolete.
 *)
  DDSCAPS_RESERVED3              = $00000400;
(*
 * Indicates that this surface is the primary surface for the left eye.
 * The primary surface for the left eye represents what the user is seeing
 * at the moment with the users left eye.  When this surface is created the
 * DDSCAPS_PRIMARYSURFACE represents what the user is seeing with the users
 * right eye.
 *)
  DDSCAPS_PRIMARYSURFACELEFT = DDSCAPS_RESERVED3;

(*
 * Indicates that this surface memory was allocated in system memory
 *)
  DDSCAPS_SYSTEMMEMORY                    = $00000800;

(*
 * Indicates that this surface can be used as a 3D texture.  It does not
 * indicate whether or not the surface is being used for that purpose.
 *)
  DDSCAPS_TEXTURE                         = $00001000;

(*
 * Indicates that a surface may be a destination for 3D rendering.  This
 * bit must be set in order to query for a Direct3D Device Interface
 * from this surface.
 *)
  DDSCAPS_3DDEVICE                        = $00002000;

(*
 * Indicates that this surface exists in video memory.
 *)
  DDSCAPS_VIDEOMEMORY                     = $00004000;

(*
 * Indicates that changes made to this surface are immediately visible.
 * It is always set for the primary surface and is set for overlays while
 * they are being overlayed and texture maps while they are being textured.
 *)
  DDSCAPS_VISIBLE                         = $00008000;

(*
 * Indicates that only writes are permitted to the surface.  Read accesses
 * from the surface may or may not generate a protection fault, but the
 * results of a read from this surface will not be meaningful.  READ ONLY.
 *)
  DDSCAPS_WRITEONLY                       = $00010000;

(*
 * Indicates that this surface is a z buffer. A z buffer does not contain
 * displayable information.  Instead it contains bit depth information that is
 * used to determine which pixels are visible and which are obscured.
 *)
  DDSCAPS_ZBUFFER                         = $00020000;

(*
 * Indicates surface will have a DC associated long term
 *)
  DDSCAPS_OWNDC                           = $00040000;

(*
 * Indicates surface should be able to receive live video
 *)
  DDSCAPS_LIVEVIDEO                       = $00080000;

(*
 * Indicates surface should be able to have a stream decompressed
 * to it by the hardware.
 *)
  DDSCAPS_HWCODEC                         = $00100000;

(*
 * Surface is a ModeX surface.
 *
 *)
  DDSCAPS_MODEX                           = $00200000;

(*
 * Indicates surface is one level of a mip-map. This surface will
 * be attached to other DDSCAPS_MIPMAP surfaces to form the mip-map.
 * This can be done explicitly, by creating a number of surfaces and
 * attaching them with AddAttachedSurface or by implicitly by CreateSurface.
 * If this bit is set then DDSCAPS_TEXTURE must also be set.
 *)
  DDSCAPS_MIPMAP                          = $00400000;

(*
 * This bit is reserved. It should not be specified.
 *)
  DDSCAPS_RESERVED2                       = $00800000;

(*
 * Indicates that memory for the surface is not allocated until the surface
 * is loaded (via the Direct3D texture Load() function).
 *)
  DDSCAPS_ALLOCONLOAD                     = $04000000;

(*
 * Indicates that the surface will recieve data from a video port.
 *)
  DDSCAPS_VIDEOPORT		          = $08000000;

(*
 * Indicates that a video memory surface is resident in true, local video
 * memory rather than non-local video memory. If this flag is specified then
 * so must DDSCAPS_VIDEOMEMORY. This flag is mutually exclusive with
 * DDSCAPS_NONLOCALVIDMEM.
 *)
  DDSCAPS_LOCALVIDMEM                     = $10000000;

(*
 * Indicates that a video memory surface is resident in non-local video
 * memory rather than true, local video memory. If this flag is specified
 * then so must DDSCAPS_VIDEOMEMORY. This flag is mutually exclusive with
 * DDSCAPS_LOCALVIDMEM.
 *)
  DDSCAPS_NONLOCALVIDMEM                  = $20000000;

(*
 * Indicates that this surface is a standard VGA mode surface, and not a
 * ModeX surface. (This flag will never be set in combination with the
 * DDSCAPS_MODEX flag).
 *)
  DDSCAPS_STANDARDVGAMODE                 = $40000000;

(*
 * Indicates that this surface will be an optimized surface. This flag is
 * currently only valid in conjunction with the DDSCAPS_TEXTURE flag. The surface
 * will be created without any underlying video memory until loaded.
 *)
  DDSCAPS_OPTIMIZED                       = $80000000;



(*
 * Indicates that this surface will receive data from a video port using
 * the de-interlacing hardware.  This allows the driver to allocate memory
 * for any extra buffers that may be required.  The DDSCAPS_VIDEOPORT and
 * DDSCAPS_OVERLAY flags must also be set.
 *)
  DDSCAPS2_HARDWAREDEINTERLACE            = $00000002;

(*
 * Indicates to the driver that this surface will be locked very frequently
 * (for procedural textures, dynamic lightmaps, etc). Surfaces with this cap
 * set must also have DDSCAPS_TEXTURE. This cap cannot be used with
 * DDSCAPS2_HINTSTATIC and DDSCAPS2_OPAQUE.
 *)
  DDSCAPS2_HINTDYNAMIC 			= $00000004;

(*
 * Indicates to the driver that this surface can be re-ordered/retiled on
 * load. This operation will not change the size of the texture. It is
 * relatively fast and symmetrical, since the application may lock these
 * bits (although it will take a performance hit when doing so). Surfaces
 * with this cap set must also have DDSCAPS_TEXTURE. This cap cannot be
 * used with DDSCAPS2_HINTDYNAMIC and DDSCAPS2_OPAQUE.
 *)
  DDSCAPS2_HINTSTATIC 			= $00000008;

(*
 * Indicates that the client would like this texture surface to be managed by the
 * DirectDraw/Direct3D runtime. Surfaces with this cap set must also have
 * DDSCAPS_TEXTURE and DDSCAPS_SYSTEMMEMORY.
 *)
  DDSCAPS2_TEXTUREMANAGE                  = $00000010;

(*
 * These bits are reserved for internal use *)
  DDSCAPS2_RESERVED1                      = $00000020;
  DDSCAPS2_RESERVED2                      = $00000040;

(*
 * Indicates to the driver that this surface will never be locked again.
 * The driver is free to optimize this surface via retiling and actual compression.
 * All calls to Lock() or Blts from this surface will fail. Surfaces with this
 * cap set must also have DDSCAPS_TEXTURE. This cap cannot be used with
 * DDSCAPS2_HINTDYNAMIC and DDSCAPS2_HINTSTATIC.
 *)
  DDSCAPS2_OPAQUE                         = $00000080;

(*
 * Applications should set this bit at CreateSurface time to indicate that they
 * intend to use antialiasing. Only valid if DDSCAPS_3DDEVICE is also set.
 *)
  DDSCAPS2_HINTANTIALIASING               = $00000100;

(*
 * This flag is used at CreateSurface time to indicate that this set of
 * surfaces is a cubic environment map
 *)
  DDSCAPS2_CUBEMAP                        = $00000200;

(*
 * These flags preform two functions:
 * - At CreateSurface time, they define which of the six cube faces are
 *   required by the application.
 * - After creation, each face in the cubemap will have exactly one of these
 *   bits set.
 *)
  DDSCAPS2_CUBEMAP_POSITIVEX              = $00000400;
  DDSCAPS2_CUBEMAP_NEGATIVEX              = $00000800;
  DDSCAPS2_CUBEMAP_POSITIVEY              = $00001000;
  DDSCAPS2_CUBEMAP_NEGATIVEY              = $00002000;
  DDSCAPS2_CUBEMAP_POSITIVEZ              = $00004000;
  DDSCAPS2_CUBEMAP_NEGATIVEZ              = $00008000;

(*
 * This macro may be used to specify all faces of a cube map at CreateSurface time
 *)
  DDSCAPS2_CUBEMAP_ALLFACES = ( DDSCAPS2_CUBEMAP_POSITIVEX or
                                DDSCAPS2_CUBEMAP_NEGATIVEX or
                                DDSCAPS2_CUBEMAP_POSITIVEY or
                                DDSCAPS2_CUBEMAP_NEGATIVEY or
                                DDSCAPS2_CUBEMAP_POSITIVEZ or
                                DDSCAPS2_CUBEMAP_NEGATIVEZ );


(*
 * This flag is an additional flag which is present on mipmap sublevels from DX7 onwards
 * It enables easier use of GetAttachedSurface rather than EnumAttachedSurfaces for surface
 * constructs such as Cube Maps, wherein there are more than one mipmap surface attached
 * to the root surface.
 * This caps bit is ignored by CreateSurface
 *)
  DDSCAPS2_MIPMAPSUBLEVEL                 = $00010000;

(* This flag indicates that the texture should be managed by D3D only *)
  DDSCAPS2_D3DTEXTUREMANAGE               = $00020000;

(* This flag indicates that the managed surface can be safely lost *)
  DDSCAPS2_DONOTPERSIST                   = $00040000;

(* indicates that this surface is part of a stereo flipping chain *)
  DDSCAPS2_STEREOSURFACELEFT              = $00080000;



 (****************************************************************************
 *
 * DIRECTDRAW DRIVER CAPABILITY FLAGS
 *
 ****************************************************************************)

(*
 * Display hardware has 3D acceleration.
 *)
  DDCAPS_3D                       = $00000001;

(*
 * Indicates that DirectDraw will support only dest rectangles that are aligned
 * on DIRECTDRAWCAPS.dwAlignBoundaryDest boundaries of the surface, respectively.
 * READ ONLY.
 *)
  DDCAPS_ALIGNBOUNDARYDEST        = $00000002;

(*
 * Indicates that DirectDraw will support only source rectangles  whose sizes in
 * BYTEs are DIRECTDRAWCAPS.dwAlignSizeDest multiples, respectively.  READ ONLY.
 *)
  DDCAPS_ALIGNSIZEDEST            = $00000004;
(*
 * Indicates that DirectDraw will support only source rectangles that are aligned
 * on DIRECTDRAWCAPS.dwAlignBoundarySrc boundaries of the surface, respectively.
 * READ ONLY.
 *)
  DDCAPS_ALIGNBOUNDARYSRC         = $00000008;

(*
 * Indicates that DirectDraw will support only source rectangles  whose sizes in
 * BYTEs are DIRECTDRAWCAPS.dwAlignSizeSrc multiples, respectively.  READ ONLY.
 *)
  DDCAPS_ALIGNSIZESRC             = $00000010;

(*
 * Indicates that DirectDraw will create video memory surfaces that have a stride
 * alignment equal to DIRECTDRAWCAPS.dwAlignStride.  READ ONLY.
 *)
  DDCAPS_ALIGNSTRIDE              = $00000020;

(*
 * Display hardware is capable of blt operations.
 *)
  DDCAPS_BLT                      = $00000040;

(*
 * Display hardware is capable of asynchronous blt operations.
 *)
  DDCAPS_BLTQUEUE                 = $00000080;

(*
 * Display hardware is capable of color space conversions during the blt operation.
 *)
  DDCAPS_BLTFOURCC                = $00000100;

(*
 * Display hardware is capable of stretching during blt operations.
 *)
  DDCAPS_BLTSTRETCH               = $00000200;

(*
 * Display hardware is shared with GDI.
 *)
  DDCAPS_GDI                      = $00000400;

(*
 * Display hardware can overlay.
 *)
  DDCAPS_OVERLAY                  = $00000800;

(*
 * Set if display hardware supports overlays but can not clip them.
 *)
  DDCAPS_OVERLAYCANTCLIP          = $00001000;

(*
 * Indicates that overlay hardware is capable of color space conversions during
 * the overlay operation.
 *)
  DDCAPS_OVERLAYFOURCC            = $00002000;

(*
 * Indicates that stretching can be done by the overlay hardware.
 *)
  DDCAPS_OVERLAYSTRETCH           = $00004000;

(*
 * Indicates that unique DirectDrawPalettes can be created for DirectDrawSurfaces
 * other than the primary surface.
 *)
  DDCAPS_PALETTE                  = $00008000;

(*
 * Indicates that palette changes can be syncd with the veritcal refresh.
 *)
  DDCAPS_PALETTEVSYNC             = $00010000;

(*
 * Display hardware can return the current scan line.
 *)
  DDCAPS_READSCANLINE             = $00020000;

(*
 * Display hardware has stereo vision capabilities.  DDSCAPS_PRIMARYSURFACELEFT
 * can be created.
 *)
  DDCAPS_STEREOVIEW               = $00040000;

(*
 * Display hardware is capable of generating a vertical blank interrupt.
 *)
  DDCAPS_VBI                      = $00080000;

(*
 * Supports the use of z buffers with blt operations.
 *)
  DDCAPS_ZBLTS                    = $00100000;

(*
 * Supports Z Ordering of overlays.
 *)
  DDCAPS_ZOVERLAYS                = $00200000;

(*
 * Supports color key
 *)
  DDCAPS_COLORKEY                 = $00400000;

(*
 * Supports alpha surfaces
 *)
  DDCAPS_ALPHA                    = $00800000;

(*
 * colorkey is hardware assisted(DDCAPS_COLORKEY will also be set)
 *)
  DDCAPS_COLORKEYHWASSIST         = $01000000;

(*
 * no hardware support at all
 *)
  DDCAPS_NOHARDWARE               = $02000000;

(*
 * Display hardware is capable of color fill with bltter
 *)
  DDCAPS_BLTCOLORFILL             = $04000000;

(*
 * Display hardware is bank switched, and potentially very slow at
 * random access to VRAM.
 *)
  DDCAPS_BANKSWITCHED             = $08000000;

(*
 * Display hardware is capable of depth filling Z-buffers with bltter
 *)
  DDCAPS_BLTDEPTHFILL             = $10000000;

(*
 * Display hardware is capable of clipping while bltting.
 *)
  DDCAPS_CANCLIP                  = $20000000;

(*
 * Display hardware is capable of clipping while stretch bltting.
 *)
  DDCAPS_CANCLIPSTRETCHED         = $40000000;

(*
 * Display hardware is capable of bltting to or from system memory
 *)
  DDCAPS_CANBLTSYSMEM             = $80000000;


 (****************************************************************************
 *
 * MORE DIRECTDRAW DRIVER CAPABILITY FLAGS (dwCaps2)
 *
 ****************************************************************************)

(*
 * Display hardware is certified
 *)
  DDCAPS2_CERTIFIED               = $00000001;

(*
 * Driver cannot interleave 2D operations (lock and blt) to surfaces with
 * Direct3D rendering operations between calls to BeginScene() and EndScene()
 *)
  DDCAPS2_NO2DDURING3DSCENE       = $00000002;

(*
 * Display hardware contains a video port
 *)
  DDCAPS2_VIDEOPORT	          = $00000004;

(*
 * The overlay can be automatically flipped according to the video port
 * VSYNCs, providing automatic doubled buffered display of video port
 * data using an overlay
 *)
  DDCAPS2_AUTOFLIPOVERLAY	  = $00000008;

(*
 * Overlay can display each field of interlaced data individually while
 * it is interleaved in memory without causing jittery artifacts.
 *)
  DDCAPS2_CANBOBINTERLEAVED	= $00000010;

(*
 * Overlay can display each field of interlaced data individually while
 * it is not interleaved in memory without causing jittery artifacts.
 *)
  DDCAPS2_CANBOBNONINTERLEAVED	= $00000020;

(*
 * The overlay surface contains color controls (brightness, sharpness, etc.)
 *)
  DDCAPS2_COLORCONTROLOVERLAY	= $00000040;

(*
 * The primary surface contains color controls (gamma, etc.)
 *)
  DDCAPS2_COLORCONTROLPRIMARY	= $00000080;

(*
 * RGBZ -> RGB supported for 16:16 RGB:Z
 *)
  DDCAPS2_CANDROPZ16BIT		= $00000100;

(*
 * Driver supports non-local video memory.
 *)
  DDCAPS2_NONLOCALVIDMEM          = $00000200;

(*
 * Dirver supports non-local video memory but has different capabilities for
 * non-local video memory surfaces. If this bit is set then so must
 * DDCAPS2_NONLOCALVIDMEM.
 *)
  DDCAPS2_NONLOCALVIDMEMCAPS      = $00000400;

(*
 * Driver neither requires nor prefers surfaces to be pagelocked when performing
 * blts involving system memory surfaces
 *)
  DDCAPS2_NOPAGELOCKREQUIRED      = $00000800;

(*
 * Driver can create surfaces which are wider than the primary surface
 *)
  DDCAPS2_WIDESURFACES            = $00001000;

(*
 * Driver supports bob without using a video port by handling the
 * DDFLIP_ODD and DDFLIP_EVEN flags specified in Flip.
 *)
  DDCAPS2_CANFLIPODDEVEN          = $00002000;

(*
 * Driver supports bob using hardware
 *)
  DDCAPS2_CANBOBHARDWARE          = $00004000;

(*
 * Driver supports bltting any FOURCC surface to another surface of the same FOURCC
 *)
  DDCAPS2_COPYFOURCC              = $00008000;


(*
 * Driver supports loadable gamma ramps for the primary surface
 *)
  DDCAPS2_PRIMARYGAMMA            = $00020000;

(*
 * Driver can render in windowed mode.
 *)
  DDCAPS2_CANRENDERWINDOWED       = $00080000;

(*
 * A calibrator is available to adjust the gamma ramp according to the
 * physical display properties so that the result will be identical on
 * all calibrated systems.
 *)
  DDCAPS2_CANCALIBRATEGAMMA       = $00100000;

(*
 * Indicates that the driver will respond to DDFLIP_INTERVALn flags
 *)
  DDCAPS2_FLIPINTERVAL            = $00200000;

(*
 * Indicates that the driver will respond to DDFLIP_NOVSYNC
 *)
   DDCAPS2_FLIPNOVSYNC             = $00400000;

(*
 * Driver supports management of video memory, if this flag is ON,
 * driver manages the texture if requested with DDSCAPS2_TEXTUREMANAGE on
 * DirectX manages the texture if this flag is OFF and surface has DDSCAPS2_TEXTUREMANAGE on
 *)
  DDCAPS2_CANMANAGETEXTURE        = $00800000;

(*
 * The Direct3D texture manager uses this cap to decide whether to put managed
 * surfaces in non-local video memory. If the cap is set, the texture manager will
 * put managed surfaces in non-local vidmem. Drivers that cannot texture from
 * local vidmem SHOULD NOT set this cap.
 *)
  DDCAPS2_TEXMANINNONLOCALVIDMEM  = $01000000;

(*
 * Indicates that the driver supports DX7 type of stereo in at least one mode (which may
 * not necessarily be the current mode). Applications should use IDirectDraw7 (or higher)
 * ::EnumDisplayModes and check the DDSURFACEDESC.ddsCaps.dwCaps2 field for the presence of
 * DDSCAPS2_STEREOSURFACELEFT to check if a particular mode supports stereo. The application
 * can also use IDirectDraw7(or higher)::GetDisplayMode to check the current mode.
 *)
  DDCAPS2_STEREO                  = $02000000;

(*
 * This caps bit is intended for internal DirectDraw use.
 * -It is only valid if DDCAPS2_NONLOCALVIDMEMCAPS is set.
 * -If this bit is set, then DDCAPS_CANBLTSYSMEM MUST be set by the driver (and
 *  all the assoicated system memory blt caps must be correct).
 * -It implies that the system->video blt caps in DDCAPS also apply to system to
 *  nonlocal blts. I.e. the dwSVBCaps, dwSVBCKeyCaps, dwSVBFXCaps and dwSVBRops
 *  members of DDCAPS (DDCORECAPS) are filled in correctly.
 * -Any blt from system to nonlocal memory that matches these caps bits will
 *  be passed to the driver.
 *
 * NOTE: This is intended to enable the driver itself to do efficient reordering
 * of textures. This is NOT meant to imply that hardware can write into AGP memory.
 * This operation is not currently supported.
 *)
  DDCAPS2_SYSTONONLOCAL_AS_SYSTOLOCAL   = $04000000;

(****************************************************************************
 *
 * DIRECTDRAW FX ALPHA CAPABILITY FLAGS
 *
 ****************************************************************************)

(*
 * Supports alpha blending around the edge of a source color keyed surface.
 * For Blt.
 *)
  DDFXALPHACAPS_BLTALPHAEDGEBLEND         = $00000001;

(*
 * Supports alpha information in the pixel format.  The bit depth of alpha
 * information in the pixel format can be 1,2,4, or 8.  The alpha value becomes
 * more opaque as the alpha value increases.  (0 is transparent.)
 * For Blt.
 *)
  DDFXALPHACAPS_BLTALPHAPIXELS            = $00000002;

(*
 * Supports alpha information in the pixel format.  The bit depth of alpha 
 * information in the pixel format can be 1,2,4, or 8.  The alpha value 
 * becomes more transparent as the alpha value increases.  (0 is opaque.) 
 * This flag can only be set if DDCAPS_ALPHA is set.
 * For Blt.
 *)
  DDFXALPHACAPS_BLTALPHAPIXELSNEG         = $00000004;

(*
 * Supports alpha only surfaces.  The bit depth of an alpha only surface can be
 * 1,2,4, or 8.  The alpha value becomes more opaque as the alpha value increases.
 * (0 is transparent.)
 * For Blt.
 *)
  DDFXALPHACAPS_BLTALPHASURFACES          = $00000008;

(*
 * The depth of the alpha channel data can range can be 1,2,4, or 8.
 * The NEG suffix indicates that this alpha channel becomes more transparent
 * as the alpha value increases. (0 is opaque.)  This flag can only be set if
 * DDCAPS_ALPHA is set.
 * For Blt.
 *)
  DDFXALPHACAPS_BLTALPHASURFACESNEG       = $00000010;

(*
 * Supports alpha blending around the edge of a source color keyed surface.
 * For Overlays.
 *)
  DDFXALPHACAPS_OVERLAYALPHAEDGEBLEND     = $00000020;

(*
 * Supports alpha information in the pixel format.  The bit depth of alpha
 * information in the pixel format can be 1,2,4, or 8.  The alpha value becomes
 * more opaque as the alpha value increases.  (0 is transparent.)
 * For Overlays.
 *)
  DDFXALPHACAPS_OVERLAYALPHAPIXELS        = $00000040;

(*
 * Supports alpha information in the pixel format.  The bit depth of alpha 
 * information in the pixel format can be 1,2,4, or 8.  The alpha value 
 * becomes more transparent as the alpha value increases.  (0 is opaque.) 
 * This flag can only be set if DDCAPS_ALPHA is set.
 * For Overlays.
 *)
  DDFXALPHACAPS_OVERLAYALPHAPIXELSNEG     = $00000080;

(*
 * Supports alpha only surfaces.  The bit depth of an alpha only surface can be
 * 1,2,4, or 8.  The alpha value becomes more opaque as the alpha value increases.
 * (0 is transparent.)
 * For Overlays.
 *)
  DDFXALPHACAPS_OVERLAYALPHASURFACES      = $00000100;

(*
 * The depth of the alpha channel data can range can be 1,2,4, or 8.  
 * The NEG suffix indicates that this alpha channel becomes more transparent
 * as the alpha value increases. (0 is opaque.)  This flag can only be set if
 * DDCAPS_ALPHA is set.
 * For Overlays.
 *)
  DDFXALPHACAPS_OVERLAYALPHASURFACESNEG   = $00000200;

(****************************************************************************
 *
 * DIRECTDRAW FX CAPABILITY FLAGS
 *
 ****************************************************************************)

(*
 * Uses arithmetic operations to stretch and shrink surfaces during blt
 * rather than pixel doubling techniques.  Along the Y axis.
 *)
  DDFXCAPS_BLTARITHSTRETCHY       = $00000020;

(*
 * Uses arithmetic operations to stretch during blt
 * rather than pixel doubling techniques.  Along the Y axis. Only
 * works for x1, x2, etc.
 *)
  DDFXCAPS_BLTARITHSTRETCHYN      = $00000010;

(*
 * Supports mirroring left to right in blt.
 *)
  DDFXCAPS_BLTMIRRORLEFTRIGHT     = $00000040;

(*
 * Supports mirroring top to bottom in blt.
 *)
  DDFXCAPS_BLTMIRRORUPDOWN        = $00000080;

(*
 * Supports arbitrary rotation for blts.
 *)
  DDFXCAPS_BLTROTATION            = $00000100;

(*
 * Supports 90 degree rotations for blts.
 *)
   DDFXCAPS_BLTROTATION90          = $00000200;

(*
 * DirectDraw supports arbitrary shrinking of a surface along the
 * x axis (horizontal direction) for blts.
 *)
  DDFXCAPS_BLTSHRINKX             = $00000400;

(*
 * DirectDraw supports integer shrinking (1x,2x,) of a surface
 * along the x axis (horizontal direction) for blts.
 *)
  DDFXCAPS_BLTSHRINKXN            = $00000800;

(*
 * DirectDraw supports arbitrary shrinking of a surface along the
 * y axis (horizontal direction) for blts.  
 *)
  DDFXCAPS_BLTSHRINKY             = $00001000;

(*
 * DirectDraw supports integer shrinking (1x,2x,) of a surface
 * along the y axis (vertical direction) for blts.
 *)
  DDFXCAPS_BLTSHRINKYN            = $00002000;

(*
 * DirectDraw supports arbitrary stretching of a surface along the
 * x axis (horizontal direction) for blts.
 *)
  DDFXCAPS_BLTSTRETCHX            = $00004000;

(*
 * DirectDraw supports integer stretching (1x,2x,) of a surface
 * along the x axis (horizontal direction) for blts.
 *)
  DDFXCAPS_BLTSTRETCHXN           = $00008000;

(*
 * DirectDraw supports arbitrary stretching of a surface along the
 * y axis (horizontal direction) for blts.  
 *)
  DDFXCAPS_BLTSTRETCHY            = $00010000;

(*
 * DirectDraw supports integer stretching (1x,2x,) of a surface
 * along the y axis (vertical direction) for blts.  
 *)
  DDFXCAPS_BLTSTRETCHYN           = $00020000;

(*
 * Uses arithmetic operations to stretch and shrink surfaces during 
 * overlay rather than pixel doubling techniques.  Along the Y axis 
 * for overlays.
 *)
  DDFXCAPS_OVERLAYARITHSTRETCHY   = $00040000;

(*
 * Uses arithmetic operations to stretch surfaces during 
 * overlay rather than pixel doubling techniques.  Along the Y axis 
 * for overlays. Only works for x1, x2, etc.
 *)
  DDFXCAPS_OVERLAYARITHSTRETCHYN  = $00000008;

(*
 * DirectDraw supports arbitrary shrinking of a surface along the
 * x axis (horizontal direction) for overlays.
 *)
  DDFXCAPS_OVERLAYSHRINKX         = $00080000;

(*
 * DirectDraw supports integer shrinking (1x,2x,) of a surface
 * along the x axis (horizontal direction) for overlays.
 *)
  DDFXCAPS_OVERLAYSHRINKXN        = $00100000;

(*
 * DirectDraw supports arbitrary shrinking of a surface along the
 * y axis (horizontal direction) for overlays.  
 *)
  DDFXCAPS_OVERLAYSHRINKY         = $00200000;

(*
 * DirectDraw supports integer shrinking (1x,2x,) of a surface
 * along the y axis (vertical direction) for overlays.  
 *)
  DDFXCAPS_OVERLAYSHRINKYN        = $00400000;

(*
 * DirectDraw supports arbitrary stretching of a surface along the
 * x axis (horizontal direction) for overlays.
 *)
  DDFXCAPS_OVERLAYSTRETCHX        = $00800000;

(*
 * DirectDraw supports integer stretching (1x,2x,) of a surface
 * along the x axis (horizontal direction) for overlays.
 *)
  DDFXCAPS_OVERLAYSTRETCHXN       = $01000000;

(*
 * DirectDraw supports arbitrary stretching of a surface along the
 * y axis (horizontal direction) for overlays.  
 *)
  DDFXCAPS_OVERLAYSTRETCHY        = $02000000;

(*
 * DirectDraw supports integer stretching (1x,2x,) of a surface
 * along the y axis (vertical direction) for overlays.  
 *)
  DDFXCAPS_OVERLAYSTRETCHYN       = $04000000;

(*
 * DirectDraw supports mirroring of overlays across the vertical axis
 *)
  DDFXCAPS_OVERLAYMIRRORLEFTRIGHT = $08000000;

(*
 * DirectDraw supports mirroring of overlays across the horizontal axis
 *)
  DDFXCAPS_OVERLAYMIRRORUPDOWN    = $10000000;

(*
 * Driver can do alpha blending for blits.
 *)
  DDFXCAPS_BLTALPHA 		= $00000001;

(*
 * Driver can do geometric transformations (or warps) for blits.
 *)
  DDFXCAPS_BLTTRANSFORM		= $00000002;

(*
 * Driver can do surface-reconstruction filtering for warped blits.
 *)
  DDFXCAPS_BLTFILTER 	       = DDFXCAPS_BLTARITHSTRETCHY;

(*
 * Driver can do alpha blending for overlays.
 *)
  DDFXCAPS_OVERLAYALPHA 		= $00000004;

(*
 * Driver can do geometric transformations (or warps) for overlays.
 *)
  DDFXCAPS_OVERLAYTRANSFORM 	= $20000000;

(*
 * Driver can do surface-reconstruction filtering for warped overlays.
 *)
  DDFXCAPS_OVERLAYFILTER 	      = DDFXCAPS_OVERLAYARITHSTRETCHY;

(****************************************************************************
 *
 * DIRECTDRAW STEREO VIEW CAPABILITIES
 *
 ****************************************************************************)

(*
 * This flag used to be DDSVCAPS_ENIGMA, which is now obsolete
 * The stereo view is accomplished via enigma encoding.
 *)
  DDSVCAPS_RESERVED1                 = $00000001;
  DDSVCAPS_ENIGMA                 = DDSVCAPS_RESERVED1;

(*
 * This flag used to be DDSVCAPS_FLICKER, which is now obsolete
 * The stereo view is accomplished via high frequency flickering.
 *)
  DDSVCAPS_RESERVED2                = $00000002;
  DDSVCAPS_FLICKER                = DDSVCAPS_RESERVED2;

(*
 * This flag used to be DDSVCAPS_REDBLUE, which is now obsolete
 * The stereo view is accomplished via red and blue filters applied
 * to the left and right eyes.  All images must adapt their colorspaces
 * for this process.
 *)
  DDSVCAPS_RESERVED3                = $00000004;
  DDSVCAPS_REDBLUE                = DDSVCAPS_RESERVED3;

(*
 * This flag used to be DDSVCAPS_SPLIT, which is now obsolete
 * The stereo view is accomplished with split screen technology.
 *)
  DDSVCAPS_RESERVED4                  = $00000008;
  DDSVCAPS_SPLIT                  = DDSVCAPS_RESERVED4;

(*
 * The stereo view is accomplished with switching technology
 *)
  DDSVCAPS_STEREOSEQUENTIAL       = $00000010;

(****************************************************************************
 *
 * DIRECTDRAWPALETTE CAPABILITIES
 *
 ****************************************************************************)

(*
 * Index is 4 bits.  There are sixteen color entries in the palette table.
 *)
  DDPCAPS_4BIT                    = $00000001;

(*
 * Index is onto a 8 bit color index.  This field is only valid with the
 * DDPCAPS_1BIT, DDPCAPS_2BIT or DDPCAPS_4BIT capability and the target
 * surface is in 8bpp. Each color entry is one byte long and is an index
 * into destination surface's 8bpp palette.
 *)
  DDPCAPS_8BITENTRIES             = $00000002;

(*
 * Index is 8 bits.  There are 256 color entries in the palette table.
 *)
  DDPCAPS_8BIT                    = $00000004;

(*
 * Indicates that this DIRECTDRAWPALETTE should use the palette color array
 * passed into the lpDDColorArray parameter to initialize the DIRECTDRAWPALETTE
 * object.
 * This flag is obsolete. DirectDraw always initializes the color array from
 * the lpDDColorArray parameter. The definition remains for source-level
 * compatibility.
 *)
  DDPCAPS_INITIALIZE              = $00000008;

(*
 * This palette is the one attached to the primary surface.  Changing this
 * table has immediate effect on the display unless DDPSETPAL_VSYNC is specified
 * and supported.
 *)
  DDPCAPS_PRIMARYSURFACE          = $00000010;

(*
 * This palette is the one attached to the primary surface left.  Changing
 * this table has immediate effect on the display for the left eye unless
 * DDPSETPAL_VSYNC is specified and supported.
 *)
  DDPCAPS_PRIMARYSURFACELEFT      = $00000020;

(*
 * This palette can have all 256 entries defined
 *)
  DDPCAPS_ALLOW256                = $00000040;

(*
 * This palette can have modifications to it synced with the monitors
 * refresh rate.
 *)
  DDPCAPS_VSYNC                   = $00000080;

(*
 * Index is 1 bit.  There are two color entries in the palette table.
 *)
  DDPCAPS_1BIT                    = $00000100;

(*
 * Index is 2 bit.  There are four color entries in the palette table.
 *)
  DDPCAPS_2BIT                    = $00000200;

(*
 * The peFlags member of PALETTEENTRY denotes an 8 bit alpha value
 *)
  DDPCAPS_ALPHA			= $00000400;

(****************************************************************************
 *
 * DIRECTDRAWPALETTE SETENTRY CONSTANTS
 *
 ****************************************************************************)


(****************************************************************************
 *
 * DIRECTDRAWPALETTE GETENTRY CONSTANTS
 *
 ****************************************************************************)

(* 0 is the only legal value *)

(****************************************************************************
 *
 * DIRECTDRAWSURFACE SETPALETTE CONSTANTS
 *
 ****************************************************************************)

(*
 * The passed pointer is an IUnknown ptr. The cbData argument to SetPrivateData
 * must be set to sizeof(IUnknown^). DirectDraw will call AddRef through this
 * pointer and Release when the private data is destroyed. This includes when
 * the surface or palette is destroyed before such priovate data is destroyed.
 *)
  DDSPD_IUNKNOWNPOINTER           = $00000001;

(*
 * Private data is only valid for the current state of the object,
 * as determined by the uniqueness value.
 *)
  DDSPD_VOLATILE                  = $00000002;

(****************************************************************************
 *
 * DIRECTDRAWSURFACE SETPALETTE CONSTANTS
 *
 ****************************************************************************)


(****************************************************************************
 *
 * DIRECTDRAW BITDEPTH CONSTANTS
 *
 * NOTE:  These are only used to indicate supported bit depths.   These
 * are flags only, they are not to be used as an actual bit depth.   The
 * absolute numbers 1, 2, 4, 8, 16, 24 and 32 are used to indicate actual
 * bit depths in a surface or for changing the display mode.
 *
 ****************************************************************************)

(*
 * 1 bit per pixel.
 *)
  DDBD_1                  = $00004000;

(*
 * 2 bits per pixel.
 *)
  DDBD_2                  = $00002000;

(*
 * 4 bits per pixel.
 *)
  DDBD_4                  = $00001000;

(*
 * 8 bits per pixel.
 *)
  DDBD_8                  = $00000800;

(*
 * 16 bits per pixel.
 *)
  DDBD_16                 = $00000400;

(*
 * 24 bits per pixel.
 *)
  DDBD_24                 = $00000200;

(*
 * 32 bits per pixel.
 *)
  DDBD_32                 = $00000100;

(****************************************************************************
 *
 * DIRECTDRAWSURFACE SET/GET COLOR KEY FLAGS
 *
 ****************************************************************************)

(*
 * Set if the structure contains a color space.  Not set if the structure
 * contains a single color key.
 *)
  DDCKEY_COLORSPACE       = $00000001;

(*
 * Set if the structure specifies a color key or color space which is to be
 * used as a destination color key for blt operations.
 *)
  DDCKEY_DESTBLT          = $00000002;

(*
 * Set if the structure specifies a color key or color space which is to be
 * used as a destination color key for overlay operations.
 *)
  DDCKEY_DESTOVERLAY      = $00000004;

(*
 * Set if the structure specifies a color key or color space which is to be
 * used as a source color key for blt operations.
 *)
  DDCKEY_SRCBLT           = $00000008;

(*
 * Set if the structure specifies a color key or color space which is to be
 * used as a source color key for overlay operations.
 *)
  DDCKEY_SRCOVERLAY       = $00000010;


(****************************************************************************
 *
 * DIRECTDRAW COLOR KEY CAPABILITY FLAGS
 *
 ****************************************************************************)

(*
 * Supports transparent blting using a color key to identify the replaceable 
 * bits of the destination surface for RGB colors.
 *)
  DDCKEYCAPS_DESTBLT                      = $00000001;

(*
 * Supports transparent blting using a color space to identify the replaceable
 * bits of the destination surface for RGB colors.
 *)
  DDCKEYCAPS_DESTBLTCLRSPACE              = $00000002;

(*
 * Supports transparent blting using a color space to identify the replaceable
 * bits of the destination surface for YUV colors.
 *)
  DDCKEYCAPS_DESTBLTCLRSPACEYUV           = $00000004;

(*
 * Supports transparent blting using a color key to identify the replaceable
 * bits of the destination surface for YUV colors.
 *)
  DDCKEYCAPS_DESTBLTYUV                   = $00000008;

(*
 * Supports overlaying using colorkeying of the replaceable bits of the surface
 * being overlayed for RGB colors.
 *)
  DDCKEYCAPS_DESTOVERLAY                  = $00000010;

(*
 * Supports a color space as the color key for the destination for RGB colors.
 *)
  DDCKEYCAPS_DESTOVERLAYCLRSPACE          = $00000020;

(*
 * Supports a color space as the color key for the destination for YUV colors.
 *)
  DDCKEYCAPS_DESTOVERLAYCLRSPACEYUV       = $00000040;

(*
 * Supports only one active destination color key value for visible overlay
 * surfaces.
 *)
  DDCKEYCAPS_DESTOVERLAYONEACTIVE         = $00000080;

(*
 * Supports overlaying using colorkeying of the replaceable bits of the
 * surface being overlayed for YUV colors.
 *)
  DDCKEYCAPS_DESTOVERLAYYUV               = $00000100;

(*
 * Supports transparent blting using the color key for the source with
 * this surface for RGB colors.
 *)
  DDCKEYCAPS_SRCBLT                       = $00000200;

(*
 * Supports transparent blting using a color space for the source with
 * this surface for RGB colors.
 *)
  DDCKEYCAPS_SRCBLTCLRSPACE               = $00000400;

(*
 * Supports transparent blting using a color space for the source with
 * this surface for YUV colors.
 *)
  DDCKEYCAPS_SRCBLTCLRSPACEYUV            = $00000800;

(*
 * Supports transparent blting using the color key for the source with
 * this surface for YUV colors.
 *)
  DDCKEYCAPS_SRCBLTYUV                    = $00001000;

(*
 * Supports overlays using the color key for the source with this
 * overlay surface for RGB colors.
 *)
  DDCKEYCAPS_SRCOVERLAY                   = $00002000;

(*
 * Supports overlays using a color space as the source color key for
 * the overlay surface for RGB colors.
 *)
  DDCKEYCAPS_SRCOVERLAYCLRSPACE           = $00004000;

(*
 * Supports overlays using a color space as the source color key for
 * the overlay surface for YUV colors.
 *)
  DDCKEYCAPS_SRCOVERLAYCLRSPACEYUV        = $00008000;

(*
 * Supports only one active source color key value for visible
 * overlay surfaces.
 *)
  DDCKEYCAPS_SRCOVERLAYONEACTIVE          = $00010000;

(*
 * Supports overlays using the color key for the source with this
 * overlay surface for YUV colors.
 *)
  DDCKEYCAPS_SRCOVERLAYYUV                = $00020000;

(*
 * there are no bandwidth trade-offs for using colorkey with an overlay
 *)
  DDCKEYCAPS_NOCOSTOVERLAY                = $00040000;


(****************************************************************************
 *
 * DIRECTDRAW PIXELFORMAT FLAGS
 *
 ****************************************************************************)

(*
 * The surface has alpha channel information in the pixel format.
 *)
  DDPF_ALPHAPIXELS                        = $00000001;

(*
 * The pixel format contains alpha only information
 *)
  DDPF_ALPHA                              = $00000002;

(*
 * The FourCC code is valid.
 *)
  DDPF_FOURCC                             = $00000004;

(*
 * The surface is 4-bit color indexed.
 *)
  DDPF_PALETTEINDEXED4                    = $00000008;

(*
 * The surface is indexed into a palette which stores indices
 * into the destination surface's 8-bit palette.
 *)
  DDPF_PALETTEINDEXEDTO8                  = $00000010;

(*
 * The surface is 8-bit color indexed.
 *)
  DDPF_PALETTEINDEXED8                    = $00000020;

(*
 * The RGB data in the pixel format structure is valid.
 *)
  DDPF_RGB                                = $00000040;

(*
 * The surface will accept pixel data in the format specified
 * and compress it during the write.
 *)
  DDPF_COMPRESSED                         = $00000080;

(*
 * The surface will accept RGB data and translate it during
 * the write to YUV data.  The format of the data to be written
 * will be contained in the pixel format structure.  The DDPF_RGB
 * flag will be set.
 *)
  DDPF_RGBTOYUV                           = $00000100;

(*
 * pixel format is YUV - YUV data in pixel format struct is valid
 *)
  DDPF_YUV                                = $00000200;

(*
 * pixel format is a z buffer only surface
 *)
  DDPF_ZBUFFER                            = $00000400;

(*
 * The surface is 1-bit color indexed.
 *)
  DDPF_PALETTEINDEXED1                    = $00000800;

(*
 * The surface is 2-bit color indexed.
 *)
  DDPF_PALETTEINDEXED2                    = $00001000;

(*
 * The surface contains Z information in the pixels
 *)
  DDPF_ZPIXELS				= $00002000;

(*
 * The surface contains stencil information along with Z
 *)
  DDPF_STENCILBUFFER			= $00004000;

(*
 * Premultiplied alpha format -- the color components have been
 * premultiplied by the alpha component.
 *)
  DDPF_ALPHAPREMULT 			= $00008000;


(*
 * Luminance data in the pixel format is valid.
 * Use this flag for luminance-only or luminance+alpha surfaces,
 * the bit depth is then ddpf.dwLuminanceBitCount.
 *)
  DDPF_LUMINANCE                          = $00020000;

(*
 * Luminance data in the pixel format is valid.
 * Use this flag when hanging luminance off bumpmap surfaces,
 * the bit mask for the luminance portion of the pixel is then
 * ddpf.dwBumpLuminanceBitMask
 *)
  DDPF_BUMPLUMINANCE                      = $00040000;

(*
 * Bump map dUdV data in the pixel format is valid.
 *)
  DDPF_BUMPDUDV                           = $00080000;

(*===========================================================================
 *
 *
 * DIRECTDRAW CALLBACK FLAGS
 *
 *
 *==========================================================================*)

(****************************************************************************
 *
 * DIRECTDRAW ENUMSURFACES FLAGS
 *
 ****************************************************************************)

(*
 * Enumerate all of the surfaces that meet the search criterion.
 *)
  DDENUMSURFACES_ALL                      = $00000001;

(*
 * A search hit is a surface that matches the surface description.
 *)
  DDENUMSURFACES_MATCH                    = $00000002;

(*
 * A search hit is a surface that does not match the surface description.
 *)
  DDENUMSURFACES_NOMATCH                  = $00000004;

(*
 * Enumerate the first surface that can be created which meets the search criterion.
 *)
  DDENUMSURFACES_CANBECREATED             = $00000008;

(*
 * Enumerate the surfaces that already exist that meet the search criterion.
 *)
  DDENUMSURFACES_DOESEXIST                = $00000010;

(****************************************************************************
 *
 * DIRECTDRAW SETDISPLAYMODE FLAGS
 *
 ****************************************************************************)

(*
 * The desired mode is a standard VGA mode
 *)
  DDSDM_STANDARDVGAMODE                   = $00000001;

(****************************************************************************
 *
 * DIRECTDRAW ENUMDISPLAYMODES FLAGS
 *
 ****************************************************************************)

(*
 * Enumerate Modes with different refresh rates.  EnumDisplayModes guarantees
 * that a particular mode will be enumerated only once.  This flag specifies whether
 * the refresh rate is taken into account when determining if a mode is unique.
 *)
  DDEDM_REFRESHRATES                      = $00000001;

(*
 * Enumerate VGA modes. Specify this flag if you wish to enumerate supported VGA
 * modes such as mode 0x13 in addition to the usual ModeX modes (which are always
 * enumerated if the application has previously called SetCooperativeLevel with the
 * DDSCL_ALLOWMODEX flag set).
 *)
  DDEDM_STANDARDVGAMODES                  = $00000002;


(****************************************************************************
 *
 * DIRECTDRAW SETCOOPERATIVELEVEL FLAGS
 *
 ****************************************************************************)

(*
 * Exclusive mode owner will be responsible for the entire primary surface.
 * GDI can be ignored. used with DD
 *)
  DDSCL_FULLSCREEN                        = $00000001;

(*
 * allow CTRL_ALT_DEL to work while in fullscreen exclusive mode
 *)
  DDSCL_ALLOWREBOOT                       = $00000002;

(*
 * prevents DDRAW from modifying the application window.
 * prevents DDRAW from minimize/restore the application window on activation.
 *)
  DDSCL_NOWINDOWCHANGES                   = $00000004;

(*
 * app wants to work as a regular Windows application
 *)
  DDSCL_NORMAL                            = $00000008;

(*
 * app wants exclusive access
 *)
  DDSCL_EXCLUSIVE                         = $00000010;


(*
 * app can deal with non-windows display modes
 *)
  DDSCL_ALLOWMODEX                        = $00000040;

(*
 * this window will receive the focus messages
 *)
  DDSCL_SETFOCUSWINDOW                    = $00000080;

(*
 * this window is associated with the DDRAW object and will
 * cover the screen in fullscreen mode
 *)
  DDSCL_SETDEVICEWINDOW                   = $00000100;

(*
 * app wants DDRAW to create a window to be associated with the
 * DDRAW object
 *)
  DDSCL_CREATEDEVICEWINDOW                = $00000200;

(*
 * App explicitly asks DDRAW/D3D to be multithread safe. This makes D3D
 * take the global crtisec more frequently.
 *)
  DDSCL_MULTITHREADED                     = $00000400;

(*
 * App hints that it would like to keep the FPU set up for optimal Direct3D
 * performance (single precision and exceptions disabled) so Direct3D
 * does not need to explicitly set the FPU each time
 *)
  DDSCL_FPUSETUP                          = $00000800;

(*
 * App specifies that it needs either double precision FPU or FPU exceptions
 * enabled. This makes Direct3D explicitly set the FPU state eah time it is
 * called. Setting the flag will reduce Direct3D performance. The flag is
 * assumed by default in DirectX 6 and earlier. See also DDSCL_FPUSETUP
 *)
  DDSCL_FPUPRESERVE                          = $00001000;

(****************************************************************************
 *
 * DIRECTDRAW BLT FLAGS
 *
 ****************************************************************************)

(*
 * Use the alpha information in the pixel format or the alpha channel surface
 * attached to the destination surface as the alpha channel for this blt.
 *)
  DDBLT_ALPHADEST                         = $00000001;

(*
 * Use the dwConstAlphaDest field in the TDDBltFX structure as the alpha channel
 * for the destination surface for this blt.
 *)
  DDBLT_ALPHADESTCONSTOVERRIDE            = $00000002;

(*
 * The NEG suffix indicates that the destination surface becomes more
 * transparent as the alpha value increases. (0 is opaque)
 *)
  DDBLT_ALPHADESTNEG                      = $00000004;

(*
 * Use the lpDDSAlphaDest field in the TDDBltFX structure as the alpha
 * channel for the destination for this blt.
 *)
  DDBLT_ALPHADESTSURFACEOVERRIDE          = $00000008;

(*
 * Use the dwAlphaEdgeBlend field in the TDDBltFX structure as the alpha channel
 * for the edges of the image that border the color key colors.
 *)
  DDBLT_ALPHAEDGEBLEND                    = $00000010;

(*
 * Use the alpha information in the pixel format or the alpha channel surface
 * attached to the source surface as the alpha channel for this blt.
 *)
  DDBLT_ALPHASRC                          = $00000020;

(*
 * Use the dwConstAlphaSrc field in the TDDBltFX structure as the alpha channel
 * for the source for this blt.
 *)
  DDBLT_ALPHASRCCONSTOVERRIDE             = $00000040;

(*
 * The NEG suffix indicates that the source surface becomes more transparent
 * as the alpha value increases. (0 is opaque)
 *)
  DDBLT_ALPHASRCNEG                       = $00000080;

(*
 * Use the lpDDSAlphaSrc field in the TDDBltFX structure as the alpha channel
 * for the source for this blt. 
 *)
  DDBLT_ALPHASRCSURFACEOVERRIDE           = $00000100;

(*
 * Do this blt asynchronously through the FIFO in the order received.  If
 * there is no room in the hardware FIFO fail the call.
 *)
  DDBLT_ASYNC                             = $00000200;

(*
 * Uses the dwFillColor field in the TDDBltFX structure as the RGB color
 * to fill the destination rectangle on the destination surface with.
 *)
  DDBLT_COLORFILL                         = $00000400;

(*
 * Uses the dwDDFX field in the TDDBltFX structure to specify the effects
 * to use for the blt.
 *)
  DDBLT_DDFX                              = $00000800;

(*
 * Uses the dwDDROPS field in the TDDBltFX structure to specify the ROPS
 * that are not part of the Win32 API.
 *)
  DDBLT_DDROPS                            = $00001000;

(*
 * Use the color key associated with the destination surface.
 *)
  DDBLT_KEYDEST                           = $00002000;

(*
 * Use the dckDestColorkey field in the TDDBltFX structure as the color key
 * for the destination surface.
 *)
  DDBLT_KEYDESTOVERRIDE                   = $00004000;

(*
 * Use the color key associated with the source surface.
 *)
  DDBLT_KEYSRC                            = $00008000;

(*
 * Use the dckSrcColorkey field in the TDDBltFX structure as the color key
 * for the source surface.
 *)
  DDBLT_KEYSRCOVERRIDE                    = $00010000;

(*
 * Use the dwROP field in the TDDBltFX structure for the raster operation
 * for this blt.  These ROPs are the same as the ones defined in the Win32 API.
 *)
  DDBLT_ROP                               = $00020000;

(*
 * Use the dwRotationAngle field in the TDDBltFX structure as the angle
 * (specified in 1/100th of a degree) to rotate the surface.
 *)
  DDBLT_ROTATIONANGLE                     = $00040000;

(*
 * Z-buffered blt using the z-buffers attached to the source and destination
 * surfaces and the dwZBufferOpCode field in the TDDBltFX structure as the
 * z-buffer opcode.
 *)
  DDBLT_ZBUFFER                           = $00080000;

(*
 * Z-buffered blt using the dwConstDest Zfield and the dwZBufferOpCode field
 * in the TDDBltFX structure as the z-buffer and z-buffer opcode respectively
 * for the destination.
 *)
  DDBLT_ZBUFFERDESTCONSTOVERRIDE          = $00100000;

(*
 * Z-buffered blt using the lpDDSDestZBuffer field and the dwZBufferOpCode
 * field in the TDDBltFX structure as the z-buffer and z-buffer opcode
 * respectively for the destination.
 *)
  DDBLT_ZBUFFERDESTOVERRIDE               = $00200000;

(*
 * Z-buffered blt using the dwConstSrcZ field and the dwZBufferOpCode field
 * in the TDDBltFX structure as the z-buffer and z-buffer opcode respectively
 * for the source.
 *)
  DDBLT_ZBUFFERSRCCONSTOVERRIDE           = $00400000;

(*
 * Z-buffered blt using the lpDDSSrcZBuffer field and the dwZBufferOpCode
 * field in the TDDBltFX structure as the z-buffer and z-buffer opcode
 * respectively for the source.
 *)
   DDBLT_ZBUFFERSRCOVERRIDE                = $00800000;

(*
 * wait until the device is ready to handle the blt
 * this will cause blt to not return DDERR_WASSTILLDRAWING
 *)
  DDBLT_WAIT                              = $01000000;

(*
 * Uses the dwFillDepth field in the TDDBltFX structure as the depth value
 * to fill the destination rectangle on the destination Z-buffer surface
 * with.
 *)
  DDBLT_DEPTHFILL                         = $02000000;

(*
 * wait until the device is ready to handle the blt
 * this will cause blt to not return DDERR_WASSTILLDRAWING
 *)
  DDBLT_DONOTWAIT                         = $08000000;

(****************************************************************************
 *
 * BLTFAST FLAGS
 *
 ****************************************************************************)

  DDBLTFAST_NOCOLORKEY                    = $00000000;
  DDBLTFAST_SRCCOLORKEY                   = $00000001;
  DDBLTFAST_DESTCOLORKEY                  = $00000002;
  DDBLTFAST_WAIT                          = $00000010;
  DDBLTFAST_DONOTWAIT                     = $00000020;

(****************************************************************************
 *
 * FLIP FLAGS
 *
 ****************************************************************************)


  DDFLIP_WAIT                          = $00000001;

(*
 * Indicates that the target surface contains the even field of video data.
 * This flag is only valid with an overlay surface.
 *)
  DDFLIP_EVEN                          = $00000002;

(*
 * Indicates that the target surface contains the odd field of video data.
 * This flag is only valid with an overlay surface.
 *)
  DDFLIP_ODD                           = $00000004;

(*
 * Causes DirectDraw to perform the physical flip immediately and return
 * to the application. Typically, what was the front buffer but is now the back
 * buffer will still be visible (depending on timing) until the next vertical
 * retrace. Subsequent operations involving the two flipped surfaces will
 * not check to see if the physical flip has finished (i.e. will not return
 * DDERR_WASSTILLDRAWING for that reason (but may for other reasons)).
 * This allows an application to perform Flips at a higher frequency than the
 * monitor refresh rate, but may introduce visible artifacts.
 * Only effective if DDCAPS2_FLIPNOVSYNC is set. If that bit is not set,
 * DDFLIP_NOVSYNC has no effect.
 *)
  DDFLIP_NOVSYNC                       = $00000008;


(*
 * Flip Interval Flags. These flags indicate how many vertical retraces to wait between
 * each flip. The default is one. DirectDraw will return DDERR_WASSTILLDRAWING for each
 * surface involved in the flip until the specified number of vertical retraces has
 * ocurred. Only effective if DDCAPS2_FLIPINTERVAL is set. If that bit is not set,
 * DDFLIP_INTERVALn has no effect.
 *)

(*
 * DirectDraw will flip on every other vertical sync
 *)
  DDFLIP_INTERVAL2                     = $02000000;


(*
 * DirectDraw will flip on every third vertical sync
 *)
  DDFLIP_INTERVAL3                     = $03000000;


(*
 * DirectDraw will flip on every fourth vertical sync
 *)
  DDFLIP_INTERVAL4                     = $04000000;

(*
 * DirectDraw will flip and display a main stereo surface
 *)
  DDFLIP_STEREO                        = $00000010;

(*
 * On IDirectDrawSurface7 and higher interfaces, the default is DDFLIP_WAIT. If you wish
 * to override the default and use time when the accelerator is busy (as denoted by
 * the DDERR_WASSTILLDRAWING return code) then use DDFLIP_DONOTWAIT.
 *)
  DDFLIP_DONOTWAIT                     = $00000020;

(****************************************************************************
 *
 * DIRECTDRAW SURFACE OVERLAY FLAGS
 *
 ****************************************************************************)

(*
 * Use the alpha information in the pixel format or the alpha channel surface
 * attached to the destination surface as the alpha channel for the
 * destination overlay.
 *)
  DDOVER_ALPHADEST                        = $00000001;

(*
 * Use the dwConstAlphaDest field in the TDDOverlayFX structure as the
 * destination alpha channel for this overlay.
 *)
  DDOVER_ALPHADESTCONSTOVERRIDE           = $00000002;

(*
 * The NEG suffix indicates that the destination surface becomes more
 * transparent as the alpha value increases.
 *)
  DDOVER_ALPHADESTNEG                     = $00000004;

(*
 * Use the lpDDSAlphaDest field in the TDDOverlayFX structure as the alpha
 * channel destination for this overlay.
 *)
  DDOVER_ALPHADESTSURFACEOVERRIDE         = $00000008;

(*
 * Use the dwAlphaEdgeBlend field in the TDDOverlayFX structure as the alpha
 * channel for the edges of the image that border the color key colors.
 *)
  DDOVER_ALPHAEDGEBLEND                   = $00000010;

(*
 * Use the alpha information in the pixel format or the alpha channel surface
 * attached to the source surface as the source alpha channel for this overlay.
 *)
  DDOVER_ALPHASRC                         = $00000020;

(*
 * Use the dwConstAlphaSrc field in the TDDOverlayFX structure as the source
 * alpha channel for this overlay.
 *)
  DDOVER_ALPHASRCCONSTOVERRIDE            = $00000040;

(*
 * The NEG suffix indicates that the source surface becomes more transparent
 * as the alpha value increases.
 *)
  DDOVER_ALPHASRCNEG                      = $00000080;

(*
 * Use the lpDDSAlphaSrc field in the TDDOverlayFX structure as the alpha channel
 * source for this overlay.
 *)
  DDOVER_ALPHASRCSURFACEOVERRIDE          = $00000100;

(*
 * Turn this overlay off.
 *)
  DDOVER_HIDE                             = $00000200;

(*
 * Use the color key associated with the destination surface.
 *)
  DDOVER_KEYDEST                          = $00000400;

(*
 * Use the dckDestColorkey field in the TDDOverlayFX structure as the color key
 * for the destination surface
 *)
  DDOVER_KEYDESTOVERRIDE                  = $00000800;

(*
 * Use the color key associated with the source surface.
 *)
  DDOVER_KEYSRC                           = $00001000;

(*
 * Use the dckSrcColorkey field in the TDDOverlayFX structure as the color key
 * for the source surface.
 *)
  DDOVER_KEYSRCOVERRIDE                   = $00002000;

(*
 * Turn this overlay on.
 *)
  DDOVER_SHOW                             = $00004000;

(*
 * Add a dirty rect to an emulated overlayed surface.
 *)
  DDOVER_ADDDIRTYRECT                     = $00008000;

(*
 * Redraw all dirty rects on an emulated overlayed surface.
 *)
  DDOVER_REFRESHDIRTYRECTS                = $00010000;

(*
 * Redraw the entire surface on an emulated overlayed surface.
 *)
  DDOVER_REFRESHALL                      = $00020000;

(*
 * Use the overlay FX flags to define special overlay FX
 *)
  DDOVER_DDFX                             = $00080000;

(*
 * Autoflip the overlay when ever the video port autoflips
 *)
  DDOVER_AUTOFLIP                      	  = $00100000;

(*
 * Display each field of video port data individually without
 * causing any jittery artifacts
 *)
  DDOVER_BOB                       	  = $00200000;

(*
 * Indicates that bob/weave decisions should not be overridden by other
 * interfaces.
 *)
  DDOVER_OVERRIDEBOBWEAVE		  = $00400000;

(*
 * Indicates that the surface memory is composed of interleaved fields.
 *)
  DDOVER_INTERLEAVED			  = $00800000;

(*
 * Indicates that bob will be performed using hardware rather than
 * software or emulated.
 *)
  DDOVER_BOBHARDWARE		       	= $01000000;

(*
 * Indicates that overlay FX structure contains valid ARGB scaling factors.
 *)
  DDOVER_ARGBSCALEFACTORS                 = $02000000;

(*
 * Indicates that ARGB scaling factors can be degraded to fit driver capabilities.
 *)
  DDOVER_DEGRADEARGBSCALING               = $04000000;

(****************************************************************************
 *
 * DIRECTDRAWSURFACE LOCK FLAGS
 *
 ****************************************************************************)

(*
 * The default.  Set to indicate that Lock should return a valid memory pointer
 * to the top of the specified rectangle.  If no rectangle is specified then a
 * pointer to the top of the surface is returned.
 *)
  DDLOCK_SURFACEMEMORYPTR                 = $00000000;    // = default

(*
 * Set to indicate that Lock should wait until it can obtain a valid memory
 * pointer before returning.  If this bit is set, Lock will never return
 * DDERR_WASSTILLDRAWING.
 *)
  DDLOCK_WAIT                             = $00000001;

(*
 * Set if an event handle is being passed to Lock.  Lock will trigger the event
 * when it can return the surface memory pointer requested.
 *)
  DDLOCK_EVENT                            = $00000002;

(*
 * Indicates that the surface being locked will only be read from.
 *)
  DDLOCK_READONLY                         = $00000010;

(*
 * Indicates that the surface being locked will only be written to
 *)
  DDLOCK_WRITEONLY                        = $00000020;

(*
 * Indicates that a system wide lock should not be taken when this surface
 * is locked. This has several advantages (cursor responsiveness, ability
 * to call more Windows functions, easier debugging) when locking video
 * memory surfaces. However, an application specifying this flag must
 * comply with a number of conditions documented in the help file.
 * Furthermore, this flag cannot be specified when locking the primary.
 *)
  DDLOCK_NOSYSLOCK                        = $00000800;

(*
 * Used only with Direct3D Vertex Buffer Locks. Indicates that no vertices
 * that were referred to in Draw*PrimtiveVB calls since the start of the
 * frame (or the last lock without this flag) will be modified during the
 * lock. This can be useful when one is only appending data to the vertex
 * buffer
 *)
  DDLOCK_NOOVERWRITE                      = $00001000;

(*
 * Indicates that no assumptions will be made about the contents of the
 * surface or vertex buffer during this lock.
 * This enables two things:
 * -    Direct3D or the driver may provide an alternative memory
 *      area as the vertex buffer. This is useful when one plans to clear the
 *      contents of the vertex buffer and fill in new data.
 * -    Drivers sometimes store surface data in a re-ordered format.
 *      When the application locks the surface, the driver is forced to un-re-order
 *      the surface data before allowing the application to see the surface contents.
 *      This flag is a hint to the driver that it can skip the un-re-ordering process
 *      since the application plans to overwrite every single pixel in the surface
 *      or locked rectangle (and so erase any un-re-ordered pixels anyway).
 *      Applications should always set this flag when they intend to overwrite the entire
 *      surface or locked rectangle.
 *)
  DDLOCK_DISCARDCONTENTS                  = $00002000;
 (*
  * DDLOCK_OKTOSWAP is an older, less informative name for DDLOCK_DISCARDCONTENTS
  *)
  DDLOCK_OKTOSWAP                         = $00002000;

(*
 * On IDirectDrawSurface7 and higher interfaces, the default is DDLOCK_WAIT. If you wish
 * to override the default and use time when the accelerator is busy (as denoted by
 * the DDERR_WASSTILLDRAWING return code) then use DDLOCK_DONOTWAIT.
 *)
  DDLOCK_DONOTWAIT                        = $00004000;


(****************************************************************************
 *
 * DIRECTDRAWSURFACE PAGELOCK FLAGS
 *
 ****************************************************************************)

(*
 * No flags defined at present
 *)


(****************************************************************************
 *
 * DIRECTDRAWSURFACE PAGEUNLOCK FLAGS
 *
 ****************************************************************************)

(*
 * No flags defined at present
 *)


(****************************************************************************
 *
 * DIRECTDRAWSURFACE BLT FX FLAGS
 *
 ****************************************************************************)

(*
 * If stretching, use arithmetic stretching along the Y axis for this blt.
 *)
  DDBLTFX_ARITHSTRETCHY                   = $00000001;

(*
 * Do this blt mirroring the surface left to right.  Spin the
 * surface around its y-axis.
 *)
  DDBLTFX_MIRRORLEFTRIGHT                 = $00000002;

(*
 * Do this blt mirroring the surface up and down.  Spin the surface
 * around its x-axis.
 *)
  DDBLTFX_MIRRORUPDOWN                    = $00000004;

(*
 * Schedule this blt to avoid tearing.
 *)
  DDBLTFX_NOTEARING                       = $00000008;

(*
 * Do this blt rotating the surface one hundred and eighty degrees.
 *)
  DDBLTFX_ROTATE180                       = $00000010;

(*
 * Do this blt rotating the surface two hundred and seventy degrees.
 *)
  DDBLTFX_ROTATE270                       = $00000020;

(*
 * Do this blt rotating the surface ninety degrees.
 *)
  DDBLTFX_ROTATE90                        = $00000040;

(*
 * Do this z blt using dwZBufferLow and dwZBufferHigh as  range values
 * specified to limit the bits copied from the source surface.
 *)
  DDBLTFX_ZBUFFERRANGE                    = $00000080;

(*
 * Do this z blt adding the dwZBufferBaseDest to each of the sources z values
 * before comparing it with the desting z values.
 *)
  DDBLTFX_ZBUFFERBASEDEST                 = $00000100;

(****************************************************************************
 *
 * DIRECTDRAWSURFACE OVERLAY FX FLAGS
 *
 ****************************************************************************)

(*
 * If stretching, use arithmetic stretching along the Y axis for this overlay.
 *)
  DDOVERFX_ARITHSTRETCHY                  = $00000001;

(*
 * Mirror the overlay across the vertical axis
 *)
  DDOVERFX_MIRRORLEFTRIGHT                = $00000002;

(*
 * Mirror the overlay across the horizontal axis
 *)
  DDOVERFX_MIRRORUPDOWN                   = $00000004;

(****************************************************************************
 *
 * Flags for dwDDFX member of DDSPRITEFX structure
 *
 ****************************************************************************)
(*
 * Use affine transformation matrix in fTransform member.
 *)
  DDSPRITEFX_AFFINETRANSFORM		= $00000001;

(*
 * Use RGBA scaling factors in ddrgbaScaleFactors member.
 *)
  DDSPRITEFX_RGBASCALING			= $00000002;

(*
 * Degrade RGBA scaling factors to accommodate driver's capabilities.
 *)
  DDSPRITEFX_DEGRADERGBASCALING		= $00000004;

(*
 * Do bilinear filtering of stretched or warped sprite.
 *)
  DDSPRITEFX_BILINEARFILTER     	  	= $00000008;

(*
 * Do "blur" filtering of stretched or warped sprite.
 *)
  DDSPRITEFX_BLURFILTER 	      	 	= $00000010;

(*
 * Do "flat" filtering of stretched or warped sprite.
 *)
  DDSPRITEFX_FLATFILTER 	      		= $00000020;

(*
 * Degrade filtering operation to accommodate driver's capabilities.
 *)
  DDSPRITEFX_DEGRADEFILTER 	      	= $00000040;

(****************************************************************************
 *
 * DIRECTDRAW WAITFORVERTICALBLANK FLAGS
 *
 ****************************************************************************)

(*
 * return when the vertical blank interval begins
 *)
  DDWAITVB_BLOCKBEGIN                     = $00000001;

(*
 * set up an event to trigger when the vertical blank begins
 *)
  DDWAITVB_BLOCKBEGINEVENT                = $00000002;

(*
 * return when the vertical blank interval ends and display begins
 *)
  DDWAITVB_BLOCKEND                       = $00000004;

(****************************************************************************
 *
 * DIRECTDRAW GETFLIPSTATUS FLAGS
 *
 ****************************************************************************)

(*
 * is it OK to flip now?
 *)
  DDGFS_CANFLIP                   = $00000001;

(*
 * is the last flip finished?
 *)
  DDGFS_ISFLIPDONE                = $00000002;

(****************************************************************************
 *
 * DIRECTDRAW GETBLTSTATUS FLAGS
 *
 ****************************************************************************)

(*
 * is it OK to blt now?
 *)
  DDGBS_CANBLT                    = $00000001;

(*
 * is the blt to the surface finished?
 *)
  DDGBS_ISBLTDONE                 = $00000002;


(****************************************************************************
 *
 * DIRECTDRAW ENUMOVERLAYZORDER FLAGS
 *
 ****************************************************************************)

(*
 * Enumerate overlays back to front.
 *)
  DDENUMOVERLAYZ_BACKTOFRONT      = $00000000;

(*
 * Enumerate overlays front to back
 *)
  DDENUMOVERLAYZ_FRONTTOBACK      = $00000001;

(****************************************************************************
 *
 * DIRECTDRAW UPDATEOVERLAYZORDER FLAGS
 *
 ****************************************************************************)

(*
 * Send overlay to front
 *)
  DDOVERZ_SENDTOFRONT             = $00000000;

(*
 * Send overlay to back
 *)
  DDOVERZ_SENDTOBACK              = $00000001;

(*
 * Move Overlay forward
 *)
  DDOVERZ_MOVEFORWARD             = $00000002;

(*
 * Move Overlay backward
 *)
  DDOVERZ_MOVEBACKWARD            = $00000003;

(*
 * Move Overlay in front of relative surface
 *)
  DDOVERZ_INSERTINFRONTOF         = $00000004;

(*
 * Move Overlay in back of relative surface
 *)
  DDOVERZ_INSERTINBACKOF          = $00000005;

(****************************************************************************
 *
 * DIRECTDRAW SETGAMMARAMP FLAGS
 *
 ****************************************************************************)

(*
 * Request calibrator to adjust the gamma ramp according to the physical
 * properties of the display so that the result should appear identical
 * on all systems.
 *)
  DDSGR_CALIBRATE                        = $00000001;

(****************************************************************************
 *
 * DIRECTDRAW STARTMODETEST FLAGS
 *
 ****************************************************************************)

(*
 * Indicates that the mode being tested has passed
 *)
 DDSMT_ISTESTREQUIRED                   = $00000001;


(****************************************************************************
 *
 * DIRECTDRAW EVALUATEMODE FLAGS
 *
 ****************************************************************************)

(*
 * Indicates that the mode being tested has passed
 *)
 DDEM_MODEPASSED                        = $00000001;

(*
 * Indicates that the mode being tested has failed
 *)
 DDEM_MODEFAILED                        = $00000002;

(*===========================================================================
 *
 *
 * DIRECTDRAW RETURN CODES
 *
 * The return values from DirectDraw Commands and Surface that return an HResult
 * are codes from DirectDraw concerning the results of the action
 * requested by DirectDraw.
 *
 *==========================================================================*)

(*
 * Status is OK
 *
 * Issued by: DirectDraw Commands and all callbacks
 *)
  DD_OK                                   = 0;
  DD_FALSE                                = S_FALSE;

(****************************************************************************
 *
 * DIRECTDRAW ENUMCALLBACK RETURN VALUES
 *
 * EnumCallback returns are used to control the flow of the DIRECTDRAW and
 * DIRECTDRAWSURFACE object enumerations.   They can only be returned by
 * enumeration callback routines.
 *
 ****************************************************************************)

(*
 * stop the enumeration
 *)
  DDENUMRET_CANCEL                        = 0;

(*
 * continue the enumeration
 *)
  DDENUMRET_OK                            = 1;

(****************************************************************************
 *
 * DIRECTDRAW ERRORS
 *
 * Errors are represented by negative values and cannot be combined.
 *
 ****************************************************************************)

  _FACDD = $876;
  MAKE_DDHRESULT = HResult(1 shl 31) or HResult(_FACDD shl 16);


(*
 * This object is already initialized
 *)
  DDERR_ALREADYINITIALIZED                = MAKE_DDHRESULT + 5;

(*
 * This surface can not be attached to the requested surface.
 *)
  DDERR_CANNOTATTACHSURFACE               = MAKE_DDHRESULT + 10;

(*
 * This surface can not be detached from the requested surface.
 *)
  DDERR_CANNOTDETACHSURFACE               = MAKE_DDHRESULT + 20;

(*
 * Support is currently not available.
 *)
  DDERR_CURRENTLYNOTAVAIL                 = MAKE_DDHRESULT + 40;

(*
 * An exception was encountered while performing the requested operation
 *)
  DDERR_EXCEPTION                         = MAKE_DDHRESULT + 55;

(*
 * Generic failure.
 *)
  DDERR_GENERIC                           = E_FAIL;

(*
 * Height of rectangle provided is not a multiple of reqd alignment
 *)
  DDERR_HEIGHTALIGN                       = MAKE_DDHRESULT + 90;

(*
 * Unable to match primary surface creation request with existing
 * primary surface.
 *)
  DDERR_INCOMPATIBLEPRIMARY               = MAKE_DDHRESULT + 95;

(*
 * One or more of the caps bits passed to the callback are incorrect.
 *)
  DDERR_INVALIDCAPS                       = MAKE_DDHRESULT + 100;

(*
 * DirectDraw does not support provided Cliplist.
 *)
  DDERR_INVALIDCLIPLIST                   = MAKE_DDHRESULT + 110;

(*
 * DirectDraw does not support the requested mode
 *)
  DDERR_INVALIDMODE                       = MAKE_DDHRESULT + 120;

(*
 * DirectDraw received a pointer that was an invalid DIRECTDRAW object.
 *)
  DDERR_INVALIDOBJECT                     = MAKE_DDHRESULT + 130;

(*
 * One or more of the parameters passed to the callback function are
 * incorrect.
 *)
  DDERR_INVALIDPARAMS                     = E_INVALIDARG;

(*
 * pixel format was invalid as specified
 *)
  DDERR_INVALIDPIXELFORMAT                = MAKE_DDHRESULT + 145;

(*
 * Rectangle provided was invalid.
 *)
  DDERR_INVALIDRECT                       = MAKE_DDHRESULT + 150;

(*
 * Operation could not be carried out because one or more surfaces are locked
 *)
  DDERR_LOCKEDSURFACES                    = MAKE_DDHRESULT + 160;

(*
 * There is no 3D present.
 *)
  DDERR_NO3D                              = MAKE_DDHRESULT + 170;

(*
 * Operation could not be carried out because there is no alpha accleration
 * hardware present or available.
 *)
  DDERR_NOALPHAHW                         = MAKE_DDHRESULT + 180;

(*
 * Operation could not be carried out because there is no stereo
 * hardware present or available.
 *)
  DDERR_NOSTEREOHARDWARE          = MAKE_DDHRESULT + 181;

(*
 * Operation could not be carried out because there is no hardware
 * present which supports stereo surfaces
 *)
  DDERR_NOSURFACELEFT             = MAKE_DDHRESULT + 182;

(*
 * no clip list available
 *)
  DDERR_NOCLIPLIST                        = MAKE_DDHRESULT + 205;

(*
 * Operation could not be carried out because there is no color conversion
 * hardware present or available.
 *)
  DDERR_NOCOLORCONVHW                     = MAKE_DDHRESULT + 210;

(*
 * Create function called without DirectDraw object method SetCooperativeLevel
 * being called.
 *)
  DDERR_NOCOOPERATIVELEVELSET             = MAKE_DDHRESULT + 212;

(*
 * Surface doesn't currently have a color key
 *)
  DDERR_NOCOLORKEY                        = MAKE_DDHRESULT + 215;

(*
 * Operation could not be carried out because there is no hardware support
 * of the dest color key.
 *)
  DDERR_NOCOLORKEYHW                      = MAKE_DDHRESULT + 220;

(*
 * No DirectDraw support possible with current display driver
 *)
  DDERR_NODIRECTDRAWSUPPORT               = MAKE_DDHRESULT + 222;

(*
 * Operation requires the application to have exclusive mode but the
 * application does not have exclusive mode.
 *)
  DDERR_NOEXCLUSIVEMODE                   = MAKE_DDHRESULT + 225;

(*
 * Flipping visible surfaces is not supported.
 *)
  DDERR_NOFLIPHW                          = MAKE_DDHRESULT + 230;

(*
 * There is no GDI present.
 *)
  DDERR_NOGDI                             = MAKE_DDHRESULT + 240;

(*
 * Operation could not be carried out because there is no hardware present
 * or available.
 *)
  DDERR_NOMIRRORHW                        = MAKE_DDHRESULT + 250;

(*
 * Requested item was not found
 *)
  DDERR_NOTFOUND                          = MAKE_DDHRESULT + 255;

(*
 * Operation could not be carried out because there is no overlay hardware
 * present or available.
 *)
  DDERR_NOOVERLAYHW                       = MAKE_DDHRESULT + 260;

(*
 * Operation could not be carried out because the source and destination
 * rectangles are on the same surface and overlap each other.
 *)
  DDERR_OVERLAPPINGRECTS       		= MAKE_DDHRESULT + 270;

(*
 * Operation could not be carried out because there is no appropriate raster
 * op hardware present or available.
 *)
  DDERR_NORASTEROPHW                      = MAKE_DDHRESULT + 280;

(*
 * Operation could not be carried out because there is no rotation hardware
 * present or available.
 *)
  DDERR_NOROTATIONHW                      = MAKE_DDHRESULT + 290;

(*
 * Operation could not be carried out because there is no hardware support
 * for stretching
 *)
  DDERR_NOSTRETCHHW                       = MAKE_DDHRESULT + 310;

(*
 * DirectDrawSurface is not in 4 bit color palette and the requested operation
 * requires 4 bit color palette.
 *)
  DDERR_NOT4BITCOLOR                      = MAKE_DDHRESULT + 316;

(*
 * DirectDrawSurface is not in 4 bit color index palette and the requested
 * operation requires 4 bit color index palette.
 *)
  DDERR_NOT4BITCOLORINDEX                 = MAKE_DDHRESULT + 317;

(*
 * DirectDraw Surface is not in 8 bit color mode and the requested operation
 * requires 8 bit color.
 *)
  DDERR_NOT8BITCOLOR                      = MAKE_DDHRESULT + 320;

(*
 * Operation could not be carried out because there is no texture mapping
 * hardware present or available.
 *)
  DDERR_NOTEXTUREHW                       = MAKE_DDHRESULT + 330;

(*
 * Operation could not be carried out because there is no hardware support
 * for vertical blank synchronized operations.
 *)
  DDERR_NOVSYNCHW                         = MAKE_DDHRESULT + 335;

(*
 * Operation could not be carried out because there is no hardware support
 * for zbuffer blting.
 *)
  DDERR_NOZBUFFERHW                       = MAKE_DDHRESULT + 340;

(*
 * Overlay surfaces could not be z layered based on their BltOrder because
 * the hardware does not support z layering of overlays.
 *)
  DDERR_NOZOVERLAYHW                      = MAKE_DDHRESULT + 350;

(*
 * The hardware needed for the requested operation has already been
 * allocated.
 *)
  DDERR_OUTOFCAPS                         = MAKE_DDHRESULT + 360;

(*
 * DirectDraw does not have enough memory to perform the operation.
 *)
  DDERR_OUTOFMEMORY                       = E_OUTOFMEMORY;

(*
 * DirectDraw does not have enough memory to perform the operation.
 *)
  DDERR_OUTOFVIDEOMEMORY                  = MAKE_DDHRESULT + 380;

(*
 * hardware does not support clipped overlays
 *)
  DDERR_OVERLAYCANTCLIP                   = MAKE_DDHRESULT + 382;

(*
 * Can only have ony color key active at one time for overlays
 *)
  DDERR_OVERLAYCOLORKEYONLYONEACTIVE      = MAKE_DDHRESULT + 384;

(*
 * Access to this palette is being refused because the palette is already
 * locked by another thread.
 *)
  DDERR_PALETTEBUSY                       = MAKE_DDHRESULT + 387;

(*
 * No src color key specified for this operation.
 *)
  DDERR_COLORKEYNOTSET                    = MAKE_DDHRESULT + 400;

(*
 * This surface is already attached to the surface it is being attached to.
 *)
  DDERR_SURFACEALREADYATTACHED            = MAKE_DDHRESULT + 410;

(*
 * This surface is already a dependency of the surface it is being made a
 * dependency of.
 *)
  DDERR_SURFACEALREADYDEPENDENT           = MAKE_DDHRESULT + 420;

(*
 * Access to this surface is being refused because the surface is already
 * locked by another thread.
 *)
  DDERR_SURFACEBUSY                       = MAKE_DDHRESULT + 430;

(*
 * Access to this surface is being refused because no driver exists
 * which can supply a pointer to the surface.
 * This is most likely to happen when attempting to lock the primary
 * surface when no DCI provider is present.
 * Will also happen on attempts to lock an optimized surface.
 *)
  DDERR_CANTLOCKSURFACE                   = MAKE_DDHRESULT + 435;

(*
 * Access to Surface refused because Surface is obscured.
 *)
  DDERR_SURFACEISOBSCURED                 = MAKE_DDHRESULT + 440;

(*
 * Access to this surface is being refused because the surface is gone.
 * The DIRECTDRAWSURFACE object representing this surface should
 * have Restore called on it.
 *)
  DDERR_SURFACELOST                       = MAKE_DDHRESULT + 450;

(*
 * The requested surface is not attached.
 *)
  DDERR_SURFACENOTATTACHED                = MAKE_DDHRESULT + 460;

(*
 * Height requested by DirectDraw is too large.
 *)
  DDERR_TOOBIGHEIGHT                      = MAKE_DDHRESULT + 470;

(*
 * Size requested by DirectDraw is too large --  The individual height and
 * width are OK.
 *)
  DDERR_TOOBIGSIZE                        = MAKE_DDHRESULT + 480;

(*
 * Width requested by DirectDraw is too large.
 *)
  DDERR_TOOBIGWIDTH                       = MAKE_DDHRESULT + 490;

(*
 * Action not supported.
 *)
  DDERR_UNSUPPORTED                       = E_NOTIMPL;

(*
 * FOURCC format requested is unsupported by DirectDraw
 *)
  DDERR_UNSUPPORTEDFORMAT                 = MAKE_DDHRESULT + 510;

(*
 * Bitmask in the pixel format requested is unsupported by DirectDraw
 *)
  DDERR_UNSUPPORTEDMASK                   = MAKE_DDHRESULT + 520;

(*
 * The specified stream contains invalid data
 *)
  DDERR_INVALIDSTREAM                     = MAKE_DDHRESULT + 521;

(*
 * vertical blank is in progress
 *)
  DDERR_VERTICALBLANKINPROGRESS           = MAKE_DDHRESULT + 537;

(*
 * Informs DirectDraw that the previous Blt which is transfering information
 * to or from this Surface is incomplete.
 *)
  DDERR_WASSTILLDRAWING                   = MAKE_DDHRESULT + 540;

(*
 * The specified surface type requires specification of the COMPLEX flag
 *)
  DDERR_DDSCAPSCOMPLEXREQUIRED            = MAKE_DDHRESULT + 542;

(*
 * Rectangle provided was not horizontally aligned on reqd. boundary
 *)
  DDERR_XALIGN                            = MAKE_DDHRESULT + 560;

(*
 * The GUID passed to DirectDrawCreate is not a valid DirectDraw driver
 * identifier.
 *)
  DDERR_INVALIDDIRECTDRAWGUID             = MAKE_DDHRESULT + 561;

(*
 * A DirectDraw object representing this driver has already been created
 * for this process.
 *)
  DDERR_DIRECTDRAWALREADYCREATED          = MAKE_DDHRESULT + 562;

(*
 * A hardware only DirectDraw object creation was attempted but the driver
 * did not support any hardware.
 *)
  DDERR_NODIRECTDRAWHW                    = MAKE_DDHRESULT + 563;

(*
 * this process already has created a primary surface
 *)
  DDERR_PRIMARYSURFACEALREADYEXISTS       = MAKE_DDHRESULT + 564;

(*
 * software emulation not available.
 *)
  DDERR_NOEMULATION                       = MAKE_DDHRESULT + 565;

(*
 * region passed to Clipper::GetClipList is too small.
 *)
  DDERR_REGIONTOOSMALL                    = MAKE_DDHRESULT + 566;

(*
 * an attempt was made to set a clip list for a clipper objec that
 * is already monitoring an hwnd.
 *)
  DDERR_CLIPPERISUSINGHWND                = MAKE_DDHRESULT + 567;

(*
 * No clipper object attached to surface object
 *)
  DDERR_NOCLIPPERATTACHED                 = MAKE_DDHRESULT + 568;

(*
 * Clipper notification requires an HWND or
 * no HWND has previously been set as the CooperativeLevel HWND.
 *)
  DDERR_NOHWND                            = MAKE_DDHRESULT + 569;

(*
 * HWND used by DirectDraw CooperativeLevel has been subclassed,
 * this prevents DirectDraw from restoring state.
 *)
  DDERR_HWNDSUBCLASSED                    = MAKE_DDHRESULT + 570;

(*
 * The CooperativeLevel HWND has already been set.
 * It can not be reset while the process has surfaces or palettes created.
 *)
  DDERR_HWNDALREADYSET                    = MAKE_DDHRESULT + 571;

(*
 * No palette object attached to this surface.
 *)
  DDERR_NOPALETTEATTACHED                 = MAKE_DDHRESULT + 572;

(*
 * No hardware support for 16 or 256 color palettes.
 *)
  DDERR_NOPALETTEHW                       = MAKE_DDHRESULT + 573;

(*
 * If a clipper object is attached to the source surface passed into a
 * BltFast call.
 *)
  DDERR_BLTFASTCANTCLIP                   = MAKE_DDHRESULT + 574;

(*
 * No blter.
 *)
  DDERR_NOBLTHW                           = MAKE_DDHRESULT + 575;

(*
 * No DirectDraw ROP hardware.
 *)
  DDERR_NODDROPSHW                        = MAKE_DDHRESULT + 576;

(*
 * returned when GetOverlayPosition is called on a hidden overlay
 *)
  DDERR_OVERLAYNOTVISIBLE                 = MAKE_DDHRESULT + 577;

(*
 * returned when GetOverlayPosition is called on a overlay that UpdateOverlay
 * has never been called on to establish a destionation.
 *)
  DDERR_NOOVERLAYDEST                     = MAKE_DDHRESULT + 578;

(*
 * returned when the position of the overlay on the destionation is no longer
 * legal for that destionation.
 *)
  DDERR_INVALIDPOSITION                   = MAKE_DDHRESULT + 579;

(*
 * returned when an overlay member is called for a non-overlay surface
 *)
  DDERR_NOTAOVERLAYSURFACE                = MAKE_DDHRESULT + 580;

(*
 * An attempt was made to set the cooperative level when it was already
 * set to exclusive.
 *)
  DDERR_EXCLUSIVEMODEALREADYSET           = MAKE_DDHRESULT + 581;

(*
 * An attempt has been made to flip a surface that is not flippable.
 *)
  DDERR_NOTFLIPPABLE                      = MAKE_DDHRESULT + 582;

(*
 * Can't duplicate primary & 3D surfaces, or surfaces that are implicitly
 * created.
 *)
  DDERR_CANTDUPLICATE                     = MAKE_DDHRESULT + 583;

(*
 * Surface was not locked.  An attempt to unlock a surface that was not
 * locked at all, or by this process, has been attempted.
 *)
  DDERR_NOTLOCKED                         = MAKE_DDHRESULT + 584;

(*
 * Windows can not create any more DCs, or a DC was requested for a paltte-indexed
 * surface when the surface had no palette AND the display mode was not palette-indexed
 * (in this case DirectDraw cannot select a proper palette into the DC)
 *)
  DDERR_CANTCREATEDC                      = MAKE_DDHRESULT + 585;

(*
 * No DC was ever created for this surface.
 *)
  DDERR_NODC                              = MAKE_DDHRESULT + 586;

(*
 * This surface can not be restored because it was created in a different
 * mode.
 *)
  DDERR_WRONGMODE                         = MAKE_DDHRESULT + 587;

(*
 * This surface can not be restored because it is an implicitly created
 * surface.
 *)
  DDERR_IMPLICITLYCREATED                 = MAKE_DDHRESULT + 588;

(*
 * The surface being used is not a palette-based surface
 *)
  DDERR_NOTPALETTIZED                     = MAKE_DDHRESULT + 589;

(*
 * The display is currently in an unsupported mode
 *)
  DDERR_UNSUPPORTEDMODE                   = MAKE_DDHRESULT + 590;

(*
 * Operation could not be carried out because there is no mip-map
 * texture mapping hardware present or available.
 *)
  DDERR_NOMIPMAPHW                        = MAKE_DDHRESULT + 591;

(*
 * The requested action could not be performed because the surface was of
 * the wrong type.
 *)
  DDERR_INVALIDSURFACETYPE                = MAKE_DDHRESULT + 592;

(*
 * Device does not support optimized surfaces, therefore no video memory optimized surfaces
 *)
  DDERR_NOOPTIMIZEHW                      = MAKE_DDHRESULT + 600;

(*
 * Surface is an optimized surface, but has not yet been allocated any memory
 *)
  DDERR_NOTLOADED                         = MAKE_DDHRESULT + 601;

(*
 * Attempt was made to create or set a device window without first setting
 * the focus window
 *)
  DDERR_NOFOCUSWINDOW                     = MAKE_DDHRESULT + 602;

(*
 * Attempt was made to set a palette on a mipmap sublevel
 *)
  DDERR_NOTONMIPMAPSUBLEVEL               = MAKE_DDHRESULT + 603;

(*
 * A DC has already been returned for this surface. Only one DC can be
 * retrieved per surface.
 *)
  DDERR_DCALREADYCREATED                  = MAKE_DDHRESULT + 620;

(*
 * An attempt was made to allocate non-local video memory from a device
 * that does not support non-local video memory.
 *)
  DDERR_NONONLOCALVIDMEM                  = MAKE_DDHRESULT + 630;

(*
 * The attempt to page lock a surface failed.
 *)
  DDERR_CANTPAGELOCK                      = MAKE_DDHRESULT + 640;

(*
 * The attempt to page unlock a surface failed.
 *)
  DDERR_CANTPAGEUNLOCK                    = MAKE_DDHRESULT + 660;

(*
 * An attempt was made to page unlock a surface with no outstanding page locks.
 *)
  DDERR_NOTPAGELOCKED                     = MAKE_DDHRESULT + 680;

(*
 * There is more data available than the specified buffer size could hold
 *)
  DDERR_MOREDATA         			= MAKE_DDHRESULT + 690;

(*
 * The data has expired and is therefore no longer valid.
 *)
  DDERR_EXPIRED                           = MAKE_DDHRESULT + 691;

(*
 * The mode test has finished executing.
 *)
 DDERR_TESTFINISHED                      = MAKE_DDHRESULT + 692;

(*
 * The mode test has switched to a new mode.
 *)
 DDERR_NEWMODE                           = MAKE_DDHRESULT + 693;

(*
 * D3D has not yet been initialized.
 *)
 DDERR_D3DNOTINITIALIZED                 = MAKE_DDHRESULT + 694;

(*
 * The video port is not active
 *)
  DDERR_VIDEONOTACTIVE   			= MAKE_DDHRESULT + 695;

(*
 * The monitor does not have EDID data.
 *)
 DDERR_NOMONITORINFORMATION             = MAKE_DDHRESULT + 696;

(*
 * The driver does not enumerate display mode refresh rates.
 *)
 DDERR_NODRIVERSUPPORT                  = MAKE_DDHRESULT + 697;

(*
 * Surfaces created by one direct draw device cannot be used directly by
 * another direct draw device.
 *)
  DDERR_DEVICEDOESNTOWNSURFACE   		= MAKE_DDHRESULT + 699;

(*
 * An attempt was made to invoke an interface member of a DirectDraw object
 * created by CoCreateInstance() before it was initialized.
 *)
  DDERR_NOTINITIALIZED                    = CO_E_NOTINITIALIZED;

(* Alpha bit depth constants *)

(*
 * API's
 *)

type
  HMonitor = THandle;

  TDDEnumCallbackA = function (lpGUID: PGUID; lpDriverDescription: PAnsiChar;
      lpDriverName: PAnsiChar; lpContext: Pointer) : BOOL; stdcall;
  TDDEnumCallbackW = function (lpGUID: PGUID; lpDriverDescription: PWideChar;
      lpDriverName: PWideChar; lpContext: Pointer) : BOOL; stdcall;
{$IFDEF UNICODE}
  TDDEnumCallback = TDDEnumCallbackW;
{$ELSE}
  TDDEnumCallback = TDDEnumCallbackA;
{$ENDIF}

  TDDEnumCallbackExA = function (lpGUID: PGUID; lpDriverDescription: PAnsiChar;
      lpDriverName: PAnsiChar; lpContext: Pointer; Monitor: HMonitor) : BOOL;
      stdcall;
  TDDEnumCallbackExW = function (lpGUID: PGUID; lpDriverDescription: PWideChar;
      lpDriverName: PWideChar; lpContext: Pointer; Monitor: HMonitor) : BOOL;
      stdcall;
      
{$IFDEF UNICODE}
  TDDEnumCallbackEx = TDDEnumCallbackExW;
{$ELSE}
  TDDEnumCallbackEx = TDDEnumCallbackExA;
{$ENDIF}

var
  DirectDrawEnumerateA : function (lpCallback: TDDEnumCallbackA;
       lpContext: Pointer) : HResult; stdcall;
  DirectDrawEnumerateW : function (lpCallback: TDDEnumCallbackW;
       lpContext: Pointer) : HResult; stdcall;
  DirectDrawEnumerate : function (lpCallback: TDDEnumCallback;
       lpContext: Pointer) : HResult; stdcall;

  DirectDrawEnumerateExA : function (lpCallback: TDDEnumCallbackExA;
       lpContext: Pointer; dwFlags: DWORD) : HResult; stdcall;
  DirectDrawEnumerateExW : function (lpCallback: TDDEnumCallbackExW;
       lpContext: Pointer; dwFlags: DWORD) : HResult; stdcall;
  DirectDrawEnumerateEx : function (lpCallback: TDDEnumCallbackEx;
       lpContext: Pointer; dwFlags: DWORD) : HResult; stdcall;

  DirectDrawCreate : function (lpGUID: PGUID;
       out lplpDD: IDirectDraw;
       pUnkOuter: IUnknown) : HResult; stdcall;
  DirectDrawCreateEx : function  (lpGUID: PGUID;
       out lplpDD: IDirectDraw7; const iid: TGUID; 
       pUnkOuter: IUnknown) : HResult; stdcall;
  DirectDrawCreateClipper : function (dwFlags: DWORD;
       out lplpDDClipper: IDirectDrawClipper;
       pUnkOuter: IUnknown) : HResult; stdcall;

const
(*
 * Flags for DirectDrawEnumerateEx
 * DirectDrawEnumerateEx supercedes DirectDrawEnumerate. You must use GetProcAddress to
 * obtain a function pointer (of type LPDIRECTDRAWENUMERATEEX) to DirectDrawEnumerateEx.
 * By default, only the primary display device is enumerated.
 * DirectDrawEnumerate is equivalent to DirectDrawEnumerate(,,DDENUM_NONDISPLAYDEVICES)
 *)

(*
 * This flag causes enumeration of any GDI display devices which are part of
 * the Windows Desktop
 *)
  DDENUM_ATTACHEDSECONDARYDEVICES     = $00000001;

(*
 * This flag causes enumeration of any GDI display devices which are not
 * part of the Windows Desktop
 *)
  DDENUM_DETACHEDSECONDARYDEVICES     = $00000002;

(*
 * This flag causes enumeration of non-display devices
 *)
  DDENUM_NONDISPLAYDEVICES            = $00000004;

  REGSTR_KEY_DDHW_DESCRIPTION = 'Description';
  REGSTR_KEY_DDHW_DRIVERNAME  = 'DriverName';
  REGSTR_PATH_DDHW            = 'Hardware\DirectDrawDrivers';

  DDCREATE_HARDWAREONLY       = $00000001;
  DDCREATE_EMULATIONONLY      = $00000002;

(*
 * Macros for interpretting DDEVICEIDENTIFIER2.dwWHQLLevel
 *)
function GET_WHQL_YEAR(dwWHQLLevel: DWORD) : DWORD;
function GET_WHQL_MONTH(dwWHQLLevel: DWORD) : DWORD;
function GET_WHQL_DAY(dwWHQLLevel: DWORD) : DWORD;


(*==========================================================================;
 *
 *  Copyright (C) 1996-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:	dvp.h
 *  Content:	DirectDrawVideoPort include file
 *
 ***************************************************************************)

const
(*
 * GUIDS used by DirectDrawVideoPort objects
 *)
  DDVPTYPE_E_HREFH_VREFH: TGUID =
      (D1:$54F39980;D2:$DA60;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));
  DDVPTYPE_E_HREFH_VREFL: TGUID =
      (D1:$92783220;D2:$DA60;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));
  DDVPTYPE_E_HREFL_VREFH: TGUID =
      (D1:$A07A02E0;D2:$DA60;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));
  DDVPTYPE_E_HREFL_VREFL: TGUID =
      (D1:$E09C77E0;D2:$DA60;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));
  DDVPTYPE_CCIR656: TGUID =
      (D1:$FCA326A0;D2:$DA60;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));
  DDVPTYPE_BROOKTREE: TGUID =
      (D1:$1352A560;D2:$DA61;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));
  DDVPTYPE_PHILIPS: TGUID =
      (D1:$332CF160;D2:$DA61;D3:$11CF;D4:($9B,$06,$00,$A0,$C9,$03,$A3,$B8));

(*
 * GUIDS used to describe connections
 *)

(*============================================================================
 *
 * DirectDraw Structures
 *
 * Various structures used to invoke DirectDraw.
 *
 *==========================================================================*)

type

(*
 * TDDVideoPortConnect
 *)
  PDDVideoPortConnect = ^TDDVideoPortConnect;
  TDDVideoPortConnect = packed record
    dwSize: DWORD;        // size of the TDDVideoPortConnect structure
    dwPortWidth: DWORD;   // Width of the video port
    guidTypeID: TGUID;    // Description of video port connection
    dwFlags: DWORD;       // Connection flags
    dwReserved1: DWORD;   // Reserved, set to zero.
  end;

(*
 * TDDVideoPortCaps
 *)
  PDDVideoPortCaps = ^TDDVideoPortCaps;
  TDDVideoPortCaps = packed record
    dwSize: DWORD;                          // size of the TDDVideoPortCaps structure
    dwFlags: DWORD;                         // indicates which fields contain data
    dwMaxWidth: DWORD;                      // max width of the video port field
    dwMaxVBIWidth: DWORD;                   // max width of the VBI data
    dwMaxHeight: DWORD;                     // max height of the video port field
    dwVideoPortID: DWORD;                   // Video port ID (0 - (dwMaxVideoPorts -1))
    dwCaps: DWORD;                          // Video port capabilities
    dwFX: DWORD;                            // More video port capabilities
    dwNumAutoFlipSurfaces: DWORD;           // Number of autoflippable surfaces
    dwAlignVideoPortBoundary: DWORD;        // Byte restriction of placement within the surface
    dwAlignVideoPortPrescaleWidth: DWORD;   // Byte restriction of width after prescaling
    dwAlignVideoPortCropBoundary: DWORD;    // Byte restriction of left cropping
    dwAlignVideoPortCropWidth: DWORD;       // Byte restriction of cropping width
    dwPreshrinkXStep: DWORD;                // Width can be shrunk in steps of 1/x
    dwPreshrinkYStep: DWORD;                // Height can be shrunk in steps of 1/x
    dwNumVBIAutoFlipSurfaces: DWORD;        // Number of VBI autoflippable surfaces
    dwNumPreferredAutoflip: DWORD;	// Optimal number of autoflippable surfaces for hardware
    wNumFilterTapsX: WORD;              // Number of taps the prescaler uses in the X direction (0 - no prescale, 1 - replication, etc.)
    wNumFilterTapsY: WORD;              // Number of taps the prescaler uses in the Y direction (0 - no prescale, 1 - replication, etc.)
  end;

const
(*
 * The dwMaxWidth and dwMaxVBIWidth members are valid
 *)
  DDVPD_WIDTH = $00000001;

(*
 * The dwMaxHeight member is valid
 *)
  DDVPD_HEIGHT = $00000002;

(*
 * The dwVideoPortID member is valid
 *)
  DDVPD_ID = $00000004;

(*
 * The dwCaps member is valid
 *)
  DDVPD_CAPS = $00000008;

(*
 * The dwFX member is valid
 *)
  DDVPD_FX = $00000010;

(*
 * The dwNumAutoFlipSurfaces member is valid
 *)
  DDVPD_AUTOFLIP = $00000020;

(*
 * All of the alignment members are valid
 *)
  DDVPD_ALIGN = $00000040;

(*
 * The dwNumPreferredAutoflip member is valid
 *)
  DDVPD_PREFERREDAUTOFLIP = $00000080;

(*
 * The wNumFilterTapsX and wNumFilterTapsY fields are valid
 *)
  DDVPD_FILTERQUALITY     = $00000100;

type
(*
 * TDDVideoPortDesc
 *)
  PDDVideoPortDesc = ^TDDVideoPortDesc;
  TDDVideoPortDesc = packed record
    dwSize: DWORD;                       // size of the TDDVideoPortDesc structure
    dwFieldWidth: DWORD;                 // width of the video port field
    dwVBIWidth: DWORD;                   // width of the VBI data
    dwFieldHeight: DWORD;                // height of the video port field
    dwMicrosecondsPerField: DWORD;       // Microseconds per video field
    dwMaxPixelsPerSecond: DWORD;         // Maximum pixel rate per second
    dwVideoPortID: DWORD;                // Video port ID (0 - (dwMaxVideoPorts -1))
    dwReserved1: DWORD;                  // Reserved for future use - set to zero
    VideoPortType: TDDVideoPortConnect;  // Description of video port connection
    dwReserved2: DWORD;                  // Reserved for future use - set to zero
    dwReserved3: DWORD;                  // Reserved for future use - set to zero
  end;

(*
 * TDDVideoPortInfo
 *)
  PDDVideoPortInfo = ^TDDVideoPortInfo;
  TDDVideoPortInfo = packed record
    dwSize: DWORD;                            // Size of the structure
    dwOriginX: DWORD;                         // Placement of the video data within the surface.
    dwOriginY: DWORD;                         // Placement of the video data within the surface.
    dwVPFlags: DWORD;                         // Video port options
    rCrop: TRect;                             // Cropping rectangle (optional).
    dwPrescaleWidth: DWORD;                   // Determines pre-scaling/zooming in the X direction (optional).
    dwPrescaleHeight: DWORD;                  // Determines pre-scaling/zooming in the Y direction (optional).
    lpddpfInputFormat: PDDPixelFormat;       // Video format written to the video port
    lpddpfVBIInputFormat: PDDPixelFormat;    // Input format of the VBI data
    lpddpfVBIOutputFormat: PDDPixelFormat;   // Output format of the data
    dwVBIHeight: DWORD;                       // Specifies the number of lines of data within the vertical blanking interval.
    dwReserved1: DWORD;                       // Reserved for future use - set to zero
    dwReserved2: DWORD;                       // Reserved for future use - set to zero
  end;

(*
 * TDDVideoPortBandWidth
 *)
  PDDVideoPortBandWidth = ^TDDVideoPortBandWidth;
  TDDVideoPortBandWidth = packed record
    dwSize: DWORD;                 // Size of the structure
    dwCaps: DWORD;
    dwOverlay: DWORD;              // Zoom factor at which overlay is supported
    dwColorkey: DWORD;             // Zoom factor at which overlay w/ colorkey is supported
    dwYInterpolate: DWORD;         // Zoom factor at which overlay w/ Y interpolation is supported
    dwYInterpAndColorkey: DWORD;   // Zoom factor at which ovelray w/ Y interpolation and colorkeying is supported
    dwReserved1: DWORD;            // Reserved for future use - set to zero
    dwReserved2: DWORD;            // Reserved for future use - set to zero
  end;

(*
 * TDDVideoPortStatus
 *)
  PDDVideoPortStatus = ^TDDVideoPortStatus;
  TDDVideoPortStatus = record
    dwSize: DWORD;                       // Size of the structure
    bInUse: BOOL;                        // TRUE if video port is currently being used
    dwFlags: DWORD;                      // Currently not used
    dwReserved1: DWORD;                  // Reserved for future use
    VideoPortType: TDDVideoPortConnect;  // Information about the connection
    dwReserved2: DWORD;                  // Reserved for future use
    dwReserved3: DWORD;                  // Reserved for future use
  end;

const
(*============================================================================
 *
 * Video Port Flags
 *
 * All flags are bit flags.
 *
 *==========================================================================*)

(****************************************************************************
 *
 * VIDEOPORT TDDVideoPortConnect FLAGS
 *
 ****************************************************************************)

(*
 * When this is set by the driver and passed to the client, this
 * indicates that the video port is capable of double clocking the data.
 * When this is set by the client, this indicates that the video port
 * should enable double clocking.  This flag is only valid with external
 * syncs.
 *)
  DDVPCONNECT_DOUBLECLOCK = $00000001;

(*
 * When this is set by the driver and passed to the client, this
 * indicates that the video port is capable of using an external VACT
 * signal. When this is set by the client, this indicates that the
 * video port should use the external VACT signal.
 *)
  DDVPCONNECT_VACT = $00000002;

(*
 * When this is set by the driver and passed to the client, this
 * indicates that the video port is capable of treating even fields
 * like odd fields and visa versa.  When this is set by the client,
 * this indicates that the video port should treat even fields like odd
 * fields.
 *)
  DDVPCONNECT_INVERTPOLARITY = $00000004;

(*
 * Indicates that any data written to the video port during the VREF
 * period will not be written into the frame buffer. This flag is read only.
 *)
  DDVPCONNECT_DISCARDSVREFDATA = $00000008;

(*
 * When this is set be the driver and passed to the client, this
 * indicates that the device will write half lines into the frame buffer
 * if half lines are provided by the decoder.  If this is set by the client,
 * this indicates that the decoder will be supplying half lines.
 *)
  DDVPCONNECT_HALFLINE = $00000010;

(*
 * Indicates that the signal is interlaced. This flag is only
 * set by the client.
 *)
  DDVPCONNECT_INTERLACED = $00000020;

(*
 * Indicates that video port is shareable and that this video port
 * will use the even fields.  This flag is only set by the client.
 *)
  DDVPCONNECT_SHAREEVEN = $00000040;

(*
 * Indicates that video port is shareable and that this video port
 * will use the odd fields.  This flag is only set by the client.
 *)
  DDVPCONNECT_SHAREODD = $00000080;

(****************************************************************************
 *
 * VIDEOPORT TDDVideoPortDesc CAPS
 *
 ****************************************************************************)

(*
 * Flip can be performed automatically to avoid tearing.
 *)
  DDVPCAPS_AUTOFLIP = $00000001;

(*
 * Supports interlaced video
 *)
  DDVPCAPS_INTERLACED = $00000002;

(*
 * Supports non-interlaced video
 *)
  DDVPCAPS_NONINTERLACED = $00000004;

(*
 * Indicates that the device can return whether the current field
 * of an interlaced signal is even or odd.
 *)
  DDVPCAPS_READBACKFIELD = $00000008;

(*
 * Indicates that the device can return the current line of video
 * being written into the frame buffer.
 *)
  DDVPCAPS_READBACKLINE = $00000010;

(*
 * Allows two gen-locked video streams to share a single video port,
 * where one stream uses the even fields and the other uses the odd
 * fields. Separate parameters (including address, scaling,
 * cropping, etc.) are maintained for both fields.)
 *)
  DDVPCAPS_SHAREABLE = $00000020;

(*
 * Even fields of video can be automatically discarded.
 *)
  DDVPCAPS_SKIPEVENFIELDS = $00000040;

(*
 * Odd fields of video can be automatically discarded.
 *)
  DDVPCAPS_SKIPODDFIELDS = $00000080;

(*
 * Indicates that the device is capable of driving the graphics
 * VSYNC with the video port VSYNC.
 *)
  DDVPCAPS_SYNCMASTER = $00000100;

(*
 * Indicates that data within the vertical blanking interval can
 * be written to a different surface.
 *)
  DDVPCAPS_VBISURFACE = $00000200;

(*
 * Indicates that the video port can perform color operations
 * on the incoming data before it is written to the frame buffer.
 *)
  DDVPCAPS_COLORCONTROL = $00000400;

(*
 * Indicates that the video port can accept VBI data in a different
 * width or format than the regular video data.
 *)
  DDVPCAPS_OVERSAMPLEDVBI = $00000800;

(*
 * Indicates that the video port can write data directly to system memory
 *)
  DDVPCAPS_SYSTEMMEMORY = $00001000;

(*
 * Indicates that the VBI and video portions of the video stream can
 * be controlled by an independent processes.
 *)
  DDVPCAPS_VBIANDVIDEOINDEPENDENT	= $00002000;

(*
 * Indicates that the video port contains high quality hardware
 * de-interlacing hardware that should be used instead of the
 * bob/weave algorithms.
 *)
  DDVPCAPS_HARDWAREDEINTERLACE		= $00004000;

(****************************************************************************
 *
 * VIDEOPORT TDDVideoPortDesc FX
 *
 ****************************************************************************)

(*
 * Limited cropping is available to crop out the vertical interval data.
 *)
  DDVPFX_CROPTOPDATA = $00000001;

(*
 * Incoming data can be cropped in the X direction before it is written
 * to the surface.
 *)
  DDVPFX_CROPX = $00000002;

(*
 * Incoming data can be cropped in the Y direction before it is written
 * to the surface.
 *)
  DDVPFX_CROPY = $00000004;

(*
 * Supports interleaving interlaced fields in memory.
 *)
  DDVPFX_INTERLEAVE = $00000008;

(*
 * Supports mirroring left to right as the video data is written
 * into the frame buffer.
 *)
  DDVPFX_MIRRORLEFTRIGHT = $00000010;

(*
 * Supports mirroring top to bottom as the video data is written
 * into the frame buffer.
 *)
  DDVPFX_MIRRORUPDOWN = $00000020;

(*
 * Data can be arbitrarily shrunk in the X direction before it
 * is written to the surface.
 *)
  DDVPFX_PRESHRINKX = $00000040;

(*
 * Data can be arbitrarily shrunk in the Y direction before it
 * is written to the surface.
 *)
  DDVPFX_PRESHRINKY = $00000080;

(*
 * Data can be binary shrunk (1/2, 1/4, 1/8, etc.) in the X
 * direction before it is written to the surface.
 *)
  DDVPFX_PRESHRINKXB = $00000100;

(*
 * Data can be binary shrunk (1/2, 1/4, 1/8, etc.) in the Y
 * direction before it is written to the surface.
 *)
  DDVPFX_PRESHRINKYB = $00000200;

(*
 * Data can be shrunk in increments of 1/x in the X direction
 * (where X is specified in the TDDVideoPortCaps.dwPreshrinkXStep)
 * before it is written to the surface.
 *)
  DDVPFX_PRESHRINKXS = $00000400;

(*
 * Data can be shrunk in increments of 1/x in the Y direction
 * (where X is specified in the TDDVideoPortCaps.dwPreshrinkYStep)
 * before it is written to the surface.
 *)
  DDVPFX_PRESHRINKYS = $00000800;

(*
 * Data can be arbitrarily stretched in the X direction before
 * it is written to the surface.
 *)
  DDVPFX_PRESTRETCHX = $00001000;

(*
 * Data can be arbitrarily stretched in the Y direction before
 * it is written to the surface.
 *)
  DDVPFX_PRESTRETCHY = $00002000;

(*
 * Data can be integer stretched in the X direction before it is
 * written to the surface.
 *)
  DDVPFX_PRESTRETCHXN = $00004000;

(*
 * Data can be integer stretched in the Y direction before it is
 * written to the surface.
 *)
  DDVPFX_PRESTRETCHYN = $00008000;

(*
 * Indicates that data within the vertical blanking interval can
 * be converted independently of the remaining video data.
 *)
  DDVPFX_VBICONVERT = $00010000;

(*
 * Indicates that scaling can be disabled for data within the
 * vertical blanking interval.
 *)
  DDVPFX_VBINOSCALE = $00020000;

(*
 * Indicates that the video data can ignore the left and right
 * cropping coordinates when cropping oversampled VBI data.
 *)
  DDVPFX_IGNOREVBIXCROP = $00040000;

(*
 * Indicates that interleaving can be disabled for data within the
 * vertical blanking interval.
 *)
  DDVPFX_VBINOINTERLEAVE     = $00080000;

(****************************************************************************
 *
 * VIDEOPORT TDDVideoPortInfo FLAGS
 *
 ****************************************************************************)

(*
 * Perform automatic flipping.   Auto-flipping is performed between
 * the overlay surface that was attached to the video port using
 * IDirectDrawVideoPort::AttachSurface and the overlay surfaces that
 * are attached to the surface via the IDirectDrawSurface::AttachSurface
 * method.  The flip order is the order in which the overlay surfaces
 * were. attached.
 *)
  DDVP_AUTOFLIP = $00000001;

(*
 * Perform conversion using the ddpfOutputFormat information.
 *)
  DDVP_CONVERT = $00000002;

(*
 * Perform cropping using the specified rectangle.
 *)
  DDVP_CROP = $00000004;

(*
 * Indicates that interlaced fields should be interleaved in memory.
 *)
  DDVP_INTERLEAVE = $00000008;

(*
 * Indicates that the data should be mirrored left to right as it's
 * written into the frame buffer.
 *)
  DDVP_MIRRORLEFTRIGHT = $00000010;

(*
 * Indicates that the data should be mirrored top to bottom as it's
 * written into the frame buffer.
 *)
  DDVP_MIRRORUPDOWN = $00000020;

(*
 * Perform pre-scaling/zooming based on the pre-scale parameters.
 *)
  DDVP_PRESCALE = $00000040;

(*
 * Ignore input of even fields.
 *)
  DDVP_SKIPEVENFIELDS = $00000080;

(*
 * Ignore input of odd fields.
 *)
  DDVP_SKIPODDFIELDS = $00000100;

(*
 * Drive the graphics VSYNCs using the video port VYSNCs.
 *)
  DDVP_SYNCMASTER = $00000200;

(*
 * The ddpfVBIOutputFormatFormat member contains data that should be used
 * to convert the data within the vertical blanking interval.
 *)
  DDVP_VBICONVERT = $00000400;

(*
 * Indicates that data within the vertical blanking interval
 * should not be scaled.
 *)
  DDVP_VBINOSCALE = $00000800;

(*
 * Indicates that these bob/weave decisions should not be
 * overriden by other interfaces.
 *)
  DDVP_OVERRIDEBOBWEAVE = $00001000;

(*
 * Indicates that the video data should ignore the left and right
 * cropping coordinates when cropping the VBI data.
 *)
  DDVP_IGNOREVBIXCROP = $00002000;

(*
 * Indicates that interleaving can be disabled for data within the
 * vertical blanking interval.
 *)
  DDVP_VBINOINTERLEAVE			= $00004000;

(*
 * Indicates that the video port should use the hardware
 * de-interlacing hardware.
 *)
  DDVP_HARDWAREDEINTERLACE		= $00008000;

(****************************************************************************
 *
 * DIRIRECTDRAWVIDEOPORT GETINPUTFORMAT/GETOUTPUTFORMAT FLAGS
 *
 ****************************************************************************)

(*
 * Return formats for the video data
 *)
  DDVPFORMAT_VIDEO = $00000001;

(*
 * Return formats for the VBI data
 *)
  DDVPFORMAT_VBI = $00000002;

(****************************************************************************
 *
 * DIRIRECTDRAWVIDEOPORT SETTARGETSURFACE FLAGS
 *
 ****************************************************************************)

(*
 * Surface should receive video data (and VBI data if a surface
 * is not explicitly attached for that purpose)
 *)
  DDVPTARGET_VIDEO = $00000001;

(*
 * Surface should receive VBI data
 *)
  DDVPTARGET_VBI = $00000002;

(****************************************************************************
 *
 * DIRIRECTDRAWVIDEOPORT WAITFORSYNC FLAGS
 *
 ****************************************************************************)

(*
 * Waits until the beginning of the next VSYNC
 *)
  DDVPWAIT_BEGIN = $00000001;

(*
 * Waits until the end of the next/current VSYNC
 *)
  DDVPWAIT_END = $00000002;

(*
 * Waits until the beginning of the specified line
 *)
  DDVPWAIT_LINE = $00000003;

(****************************************************************************
 *
 * DIRECTDRAWVIDEOPORT FLIP FLAGS
 *
 ****************************************************************************)

(*
 * Flips the normal video surface
 *)
  DDVPFLIP_VIDEO = $00000001;

(*
 * Flips the VBI surface
 *)
  DDVPFLIP_VBI = $00000002;

(****************************************************************************
 *
 * DIRIRECTDRAWVIDEOPORT GETVIDEOSIGNALSTATUS VALUES
 *
 ****************************************************************************)

(*
 * No video signal is present at the video port
 *)
  DDVPSQ_NOSIGNAL = $00000001;

(*
 * A valid video signal is present at the video port
 *)
  DDVPSQ_SIGNALOK = $00000002;

(****************************************************************************
 *
 * VIDEOPORTBANDWIDTH Flags
 *
 ****************************************************************************)

(*
 * The specified height/width refer to the size of the video port data
 * written into memory, after prescaling has occured.
 *)
  DDVPB_VIDEOPORT = $00000001;

(*
 * The specified height/width refer to the source size of the overlay.
 *)
  DDVPB_OVERLAY = $00000002;

(*
 * This is a query for the device to return which caps this device requires.
 *)
  DDVPB_TYPE = $00000004;

(****************************************************************************
 *
 * VIDEOPORTBANDWIDTH Caps
 *
 ****************************************************************************)

(*
 * The bandwidth for this device is dependant on the overlay source size.
 *)
  DDVPBCAPS_SOURCE = $00000001;

(*
 * The bandwidth for this device is dependant on the overlay destination
 * size.
 *)
  DDVPBCAPS_DESTINATION = $00000002;

(****************************************************************************
 *
 * DDVIDEOPORTCONTAINER CreateVideoPort flags
 *
 ****************************************************************************)

(*
 * The process only wants to control the VBI portion of the video stream.
 *)
  DDVPCREATE_VBIONLY			= $00000001;

(*
 * The process only wants to control the non-VBI (video) portion of
 * the video stream.
 *)
  DDVPCREATE_VIDEOONLY			= $00000002;

(****************************************************************************
 *
 * DDVIDEOPORTSTATUS flags
 *
 ****************************************************************************)

(*
 * The video port interface is only controlling the VBI portion of the
 * video stream
 *)
  DDVPSTATUS_VBIONLY			= $00000001;

(*
 * The video port interface is only controlling the video portion of the
 * video stream
 *)
  DDVPSTATUS_VIDEOONLY			= $00000002;


type
(*
 * API's
 *)

  TDDEnumVideoCallback = function (lpTDDVideoPortCaps: PDDVideoPortCaps;
      lpContext: Pointer) : HResult; stdcall;

(*
 * INTERACES FOLLOW:
 *	IDirectDrawVideoPort
 *	IVideoPort
 *)


(*
 * IDirectDrawVideoPort
 *)
  IDirectDrawVideoPort = interface (IUnknown)
    ['{B36D93E0-2B43-11CF-A2DE-00AA00B93356}']
    (*** IDirectDrawVideoPort methods ***)
    function Flip(lpDDSurface: IDirectDrawSurface; dwFlags: DWORD) : HResult; stdcall;
    function GetBandwidthInfo(var lpddpfFormat: TDDPixelFormat;
        dwWidth: DWORD; dwHeight: DWORD; dwFlags: DWORD;
        var lpBandwidth: TDDVideoPortBandWidth) : HResult; stdcall;
    function GetColorControls(var lpColorControl: TDDColorControl) : HResult; stdcall;
    function GetInputFormats(var lpNumFormats: DWORD; var lpFormats:
        TDDPixelFormat; dwFlags: DWORD) : HResult; stdcall;
    function GetOutputFormats(var lpInputFormat: TDDPixelFormat;
        var lpNumFormats: DWORD; lpFormats: PDDPixelFormat; dwFlags: DWORD)
        : HResult; stdcall;
    function GetFieldPolarity(var lpbVideoField: BOOL) : HResult; stdcall;
    function GetVideoLine(var lpdwLine: DWORD) : HResult; stdcall;
    function GetVideoSignalStatus(varlpdwStatus: DWORD) : HResult; stdcall;
    function SetColorControls(var lpColorControl: TDDColorControl) : HResult; stdcall;
    function SetTargetSurface(lpDDSurface: IDirectDrawSurface; dwFlags: DWORD) :
        HResult; stdcall;
    function StartVideo(var lpVideoInfo: TDDVideoPortInfo) : HResult; stdcall;
    function StopVideo: HResult; stdcall;
    function UpdateVideo(var lpVideoInfo: TDDVideoPortInfo) : HResult; stdcall;
    function WaitForSync(dwFlags: DWORD; dwLine: DWORD; dwTimeout: DWORD) :
        HResult; stdcall;
  end;

(*
 * IDirectDrawVideoPortContainer
 *)
  IDDVideoPortContainer = interface (IUnknown)
    ['{6C142760-A733-11CE-A521-0020AF0BE560}']
    (*** IDDVideoPortContainer methods ***)
    function CreateVideoPort(dwFlags: DWORD; var lpTDDVideoPortDesc:
        TDDVideoPortDesc; var lplpDDVideoPort: IDirectDrawVideoPort;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function EnumVideoPorts(dwFlags: DWORD;
        lpTDDVideoPortCaps: PDDVideoPortCaps; lpContext: Pointer;
        lpEnumVideoCallback: TDDEnumVideoCallback) : HResult; stdcall;
    function GetVideoPortConnectInfo(dwPortId: DWORD; var lpNumEntries: DWORD;
        lpConnectInfo: PDDVideoPortConnect) : HResult; stdcall;
    function QueryVideoPortStatus(dwPortId: DWORD;
        var lpVPStatus: TDDVideoPortStatus) : HResult; stdcall;
  end;

  IID_IDDVideoPortContainer = IDDVideoPortContainer;
  IID_IDirectDrawVideoPort = IDirectDrawVideoPort;

implementation

uses
  DXCommon;

{
#define GET_WHQL_YEAR( dwWHQLLevel ) \
    ( (dwWHQLLevel) / 0x10000 )
#define GET_WHQL_MONTH( dwWHQLLevel ) \
    ( ( (dwWHQLLevel) / 0x100 ) & 0x00ff )
#define GET_WHQL_DAY( dwWHQLLevel ) \
    ( (dwWHQLLevel) & 0xff )
}
function GET_WHQL_YEAR(dwWHQLLevel: DWORD) : DWORD;
begin
  Result := (dwWHQLLevel) div $10000;
end;

function GET_WHQL_MONTH(dwWHQLLevel: DWORD) : DWORD;
begin
  Result := ( (dwWHQLLevel) div $100 ) and $00ff;
end;

function GET_WHQL_DAY(dwWHQLLevel: DWORD) : DWORD;
begin
  Result := (dwWHQLLevel) and $ff;
end;


function MAKEFOURCC(ch0, ch1, ch2, ch3: Char) : DWORD;
begin
  Result := DWORD(byte(ch0) shl 0) or
            DWORD(byte(ch1) shl 8) or
            DWORD(byte(ch2) shl 16) or
            DWORD(byte(ch3) shl 24);
end;

function DDErrorString(Value: HResult) : string;
begin
  case Value of
    DD_OK: Result := 'The request completed successfully.';
    DDERR_ALREADYINITIALIZED: Result := 'This object is already initialized.';
    DDERR_BLTFASTCANTCLIP: Result := ' if a clipper object is attached to the source surface passed into a BltFast call.';
    DDERR_CANNOTATTACHSURFACE: Result := 'This surface can not be attached to the requested surface.';
    DDERR_CANNOTDETACHSURFACE: Result := 'This surface can not be detached from the requested surface.';
    DDERR_CANTCREATEDC: Result := 'Windows can not create any more DCs.';
    DDERR_CANTDUPLICATE: Result := 'Cannot duplicate primary & 3D surfaces, or surfaces that are implicitly created.';
    DDERR_CLIPPERISUSINGHWND: Result := 'An attempt was made to set a cliplist for a clipper object that is already monitoring an hwnd.';
    DDERR_COLORKEYNOTSET: Result := 'No src color key specified for this operation.';
    DDERR_CURRENTLYNOTAVAIL: Result := 'Support is currently not available.';
    DDERR_DIRECTDRAWALREADYCREATED: Result := 'A DirectDraw object representing this driver has already been created for this process.';
    DDERR_EXCEPTION: Result := 'An exception was encountered while performing the requested operation.';
    DDERR_EXCLUSIVEMODEALREADYSET: Result := 'An attempt was made to set the cooperative level when it was already set to exclusive.';
    DDERR_GENERIC: Result := 'Generic failure.';
    DDERR_HEIGHTALIGN: Result := 'Height of rectangle provided is not a multiple of reqd alignment.';
    DDERR_HWNDALREADYSET: Result := 'The CooperativeLevel HWND has already been set. It can not be reset while the process has surfaces or palettes created.';
    DDERR_HWNDSUBCLASSED: Result := 'HWND used by DirectDraw CooperativeLevel has been subclassed, this prevents DirectDraw from restoring state.';
    DDERR_IMPLICITLYCREATED: Result := 'This surface can not be restored because it is an implicitly created surface.';
    DDERR_INCOMPATIBLEPRIMARY: Result := 'Unable to match primary surface creation request with existing primary surface.';
    DDERR_INVALIDCAPS: Result := 'One or more of the caps bits passed to the callback are incorrect.';
    DDERR_INVALIDCLIPLIST: Result := 'DirectDraw does not support the provided cliplist.';
    DDERR_INVALIDDIRECTDRAWGUID: Result := 'The GUID passed to DirectDrawCreate is not a valid DirectDraw driver identifier.';
    DDERR_INVALIDMODE: Result := 'DirectDraw does not support the requested mode.';
    DDERR_INVALIDOBJECT: Result := 'DirectDraw received a pointer that was an invalid DIRECTDRAW object.';
    DDERR_INVALIDPARAMS: Result := 'One or more of the parameters passed to the function are incorrect.';
    DDERR_INVALIDPIXELFORMAT: Result := 'The pixel format was invalid as specified.';
    DDERR_INVALIDPOSITION: Result := 'Returned when the position of the overlay on the destination is no longer legal for that destination.';
    DDERR_INVALIDRECT: Result := 'Rectangle provided was invalid.';
    DDERR_LOCKEDSURFACES: Result := 'Operation could not be carried out because one or more surfaces are locked.';
    DDERR_NO3D: Result := 'There is no 3D present.';
    DDERR_NOALPHAHW: Result := 'Operation could not be carried out because there is no alpha accleration hardware present or available.';
    DDERR_NOBLTHW: Result := 'No blitter hardware present.';
    DDERR_NOCLIPLIST: Result := 'No cliplist available.';
    DDERR_NOCLIPPERATTACHED: Result := 'No clipper object attached to surface object.';
    DDERR_NOCOLORCONVHW: Result := 'Operation could not be carried out because there is no color conversion hardware present or available.';
    DDERR_NOCOLORKEY: Result := 'Surface does not currently have a color key';
    DDERR_NOCOLORKEYHW: Result := 'Operation could not be carried out because there is no hardware support of the destination color key.';
    DDERR_NOCOOPERATIVELEVELSET: Result := 'Create function called without DirectDraw object method SetCooperativeLevel being called.';
    DDERR_NODC: Result := 'No DC was ever created for this surface.';
    DDERR_NODDROPSHW: Result := 'No DirectDraw ROP hardware.';
    DDERR_NODIRECTDRAWHW: Result := 'A hardware-only DirectDraw object creation was attempted but the driver did not support any hardware.';
    DDERR_NOEMULATION: Result := 'Software emulation not available.';
    DDERR_NOEXCLUSIVEMODE: Result := 'Operation requires the application to have exclusive mode but the application does not have exclusive mode.';
    DDERR_NOFLIPHW: Result := 'Flipping visible surfaces is not supported.';
    DDERR_NOGDI: Result := 'There is no GDI present.';
    DDERR_NOHWND: Result := 'Clipper notification requires an HWND or no HWND has previously been set as the CooperativeLevel HWND.';
    DDERR_NOMIRRORHW: Result := 'Operation could not be carried out because there is no hardware present or available.';
    DDERR_NOOVERLAYDEST: Result := 'Returned when GetOverlayPosition is called on an overlay that UpdateOverlay has never been called on to establish a destination.';
    DDERR_NOOVERLAYHW: Result := 'Operation could not be carried out because there is no overlay hardware present or available.';
    DDERR_NOPALETTEATTACHED: Result := 'No palette object attached to this surface.';
    DDERR_NOPALETTEHW: Result := 'No hardware support for 16 or 256 color palettes.';
    DDERR_NORASTEROPHW: Result := 'Operation could not be carried out because there is no appropriate raster op hardware present or available.';
    DDERR_NOROTATIONHW: Result := 'Operation could not be carried out because there is no rotation hardware present or available.';
    DDERR_NOSTRETCHHW: Result := 'Operation could not be carried out because there is no hardware support for stretching.';
    DDERR_NOT4BITCOLOR: Result := 'DirectDrawSurface is not in 4 bit color palette and the requested operation requires 4 bit color palette.';
    DDERR_NOT4BITCOLORINDEX: Result := 'DirectDrawSurface is not in 4 bit color index palette and the requested operation requires 4 bit color index palette.';
    DDERR_NOT8BITCOLOR: Result := 'DirectDrawSurface is not in 8 bit color mode and the requested operation requires 8 bit color.';
    DDERR_NOTAOVERLAYSURFACE: Result := 'Returned when an overlay member is called for a non-overlay surface.';
    DDERR_NOTEXTUREHW: Result := 'Operation could not be carried out because there is no texture mapping hardware present or available.';
    DDERR_NOTFLIPPABLE: Result := 'An attempt has been made to flip a surface that is not flippable.';
    DDERR_NOTFOUND: Result := 'Requested item was not found.';
    DDERR_NOTLOCKED: Result := 'Surface was not locked.  An attempt to unlock a surface that was not locked at all, or by this process, has been attempted.';
    DDERR_NOTPALETTIZED: Result := 'The surface being used is not a palette-based surface.';
    DDERR_NOVSYNCHW: Result := 'Operation could not be carried out because there is no hardware support for vertical blank synchronized operations.';
    DDERR_NOZBUFFERHW: Result := 'Operation could not be carried out because there is no hardware support for zbuffer blitting.';
    DDERR_NOZOVERLAYHW: Result := 'Overlay surfaces could not be z layered based on their BltOrder because the hardware does not support z layering of overlays.';
    DDERR_OUTOFCAPS: Result := 'The hardware needed for the requested operation has already been allocated.';
    DDERR_OUTOFMEMORY: Result := 'DirectDraw does not have enough memory to perform the operation.';
    DDERR_OUTOFVIDEOMEMORY: Result := 'DirectDraw does not have enough memory to perform the operation.';
    DDERR_OVERLAYCANTCLIP: Result := 'The hardware does not support clipped overlays.';
    DDERR_OVERLAYCOLORKEYONLYONEACTIVE: Result := 'Can only have ony color key active at one time for overlays.';
    DDERR_OVERLAYNOTVISIBLE: Result := 'Returned when GetOverlayPosition is called on a hidden overlay.';
    DDERR_PALETTEBUSY: Result := 'Access to this palette is being refused because the palette is already locked by another thread.';
    DDERR_PRIMARYSURFACEALREADYEXISTS: Result := 'This process already has created a primary surface.';
    DDERR_REGIONTOOSMALL: Result := 'Region passed to Clipper::GetClipList is too small.';
    DDERR_SURFACEALREADYATTACHED: Result := 'This surface is already attached to the surface it is being attached to.';
    DDERR_SURFACEALREADYDEPENDENT: Result := 'This surface is already a dependency of the surface it is being made a dependency of.';
    DDERR_SURFACEBUSY: Result := 'Access to this surface is being refused because the surface is already locked by another thread.';
    DDERR_SURFACEISOBSCURED: Result := 'Access to surface refused because the surface is obscured.';
    DDERR_SURFACELOST: Result := 'Access to this surface is being refused because the surface memory is gone. The DirectDrawSurface object representing this surface should have Restore called on it.';
    DDERR_SURFACENOTATTACHED: Result := 'The requested surface is not attached.';
    DDERR_TOOBIGHEIGHT: Result := 'Height requested by DirectDraw is too large.';
    DDERR_TOOBIGSIZE: Result := 'Size requested by DirectDraw is too large, but the individual height and width are OK.';
    DDERR_TOOBIGWIDTH: Result := 'Width requested by DirectDraw is too large.';
    DDERR_UNSUPPORTED: Result := 'Action not supported.';
    DDERR_UNSUPPORTEDFORMAT: Result := 'FOURCC format requested is unsupported by DirectDraw.';
    DDERR_UNSUPPORTEDMASK: Result := 'Bitmask in the pixel format requested is unsupported by DirectDraw.';
    DDERR_VERTICALBLANKINPROGRESS: Result := 'Vertical blank is in progress.';
    DDERR_WASSTILLDRAWING: Result := 'Informs DirectDraw that the previous Blt which is transfering information to or from this Surface is incomplete.';
    DDERR_WRONGMODE: Result := 'This surface can not be restored because it was created in a different mode.';
    DDERR_XALIGN: Result := 'Rectangle provided was not horizontally aligned on required boundary.';
    // new:
    DDERR_OVERLAPPINGRECTS: Result := 'Operation could not be carried out because the source and destination rectangles are on the same surface and overlap each other.';
    DDERR_INVALIDSTREAM: Result := 'The specified stream contains invalid data';
    DDERR_UNSUPPORTEDMODE: Result := 'The display is currently in an unsupported mode';
    DDERR_NOMIPMAPHW: Result := 'Operation could not be carried out because there is no mip-map texture mapping hardware present or available.';
    DDERR_INVALIDSURFACETYPE: Result := 'The requested action could not be performed because the surface was of the wrong type.';
    DDERR_NOOPTIMIZEHW: Result := 'Device does not support optimized surfaces, therefore no video memory optimized surfaces';
    DDERR_NOTLOADED: Result := 'Surface is an optimized surface, but has not yet been allocated any memory';
    DDERR_NOFOCUSWINDOW: Result := 'Attempt was made to create or set a device window without first setting the focus window';
    DDERR_DCALREADYCREATED: Result := 'A DC has already been returned for this surface. Only one DC can be retrieved per surface.';
    DDERR_NONONLOCALVIDMEM: Result := 'An attempt was made to allocate non-local video memory from a device that does not support non-local video memory.';
    DDERR_CANTPAGELOCK: Result := 'The attempt to page lock a surface failed.';
    DDERR_CANTPAGEUNLOCK: Result := 'The attempt to page unlock a surface failed.';
    DDERR_NOTPAGELOCKED: Result := 'An attempt was made to page unlock a surface with no outstanding page locks.';
    DDERR_MOREDATA: Result := 'There is more data available than the specified buffer size could hold';
    DDERR_EXPIRED: Result := 'The data has expired and is therefore no longer valid.';
    DDERR_VIDEONOTACTIVE: Result := 'The video port is not active';
    DDERR_DEVICEDOESNTOWNSURFACE: Result := 'Surfaces created by one direct draw device cannot be used directly by another direct draw device.';
    DDERR_NOTINITIALIZED: Result := 'An attempt was made to invoke an interface member of a DirectDraw object created by CoCreateInstance() before it was initialized.';
    else Result := 'Unrecognized Error';
  end;
end;

initialization
begin
  if not IsNTandDelphiRunning then
  begin
    DDrawDLL := LoadLibrary('DDraw.dll');
    DirectDrawEnumerateA := GetProcAddress(DDrawDLL,'DirectDrawEnumerateA');
    DirectDrawEnumerateW := GetProcAddress(DDrawDLL,'DirectDrawEnumerateW');
{$IFDEF UNICODE}
    DirectDrawEnumerate := DirectDrawEnumerateW;
{$ELSE}
    DirectDrawEnumerate := DirectDrawEnumerateA;
{$ENDIF}

    DirectDrawEnumerateExA := GetProcAddress(DDrawDLL,'DirectDrawEnumerateExA');
    DirectDrawEnumerateExW := GetProcAddress(DDrawDLL,'DirectDrawEnumerateExW');
{$IFDEF UNICODE}
    DirectDrawEnumerateEx := DirectDrawEnumerateExW;
{$ELSE}
    DirectDrawEnumerateEx := DirectDrawEnumerateExA;
{$ENDIF}

    DirectDrawCreate := GetProcAddress(DDrawDLL,'DirectDrawCreate');
    DirectDrawCreateEx := GetProcAddress(DDrawDLL,'DirectDrawCreateEx');
    DirectDrawCreateClipper := GetProcAddress(DDrawDLL,'DirectDrawCreateClipper');
{$IFDEF WINNT}
    NtDirectDrawCreate := GetProcAddress(DDrawDLL,'NtDirectDrawCreate');
{$ENDIF}
  end;
end;

finalization
begin
  if DDrawDLL <> 0 then FreeLibrary(DDrawDLL);
end;

end.


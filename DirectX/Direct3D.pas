(*==========================================================================;
 *
 *  Copyright (C) 1995-1998 Microsoft Corporation.  All Rights Reserved.
 *
 *  Files:   d3dtypes.h d3dcaps.h d3d.h
 *
 *  DirectX 7.0 Delphi adaptation by Erik Unger
 *
 *  Modyfied: 19-Feb-2000
 *
 *  Download: http://www.delphi-jedi.org/DelphiGraphics/
 *  E-Mail: Erik.Unger@gmx.at
 *
 ***************************************************************************)

unit Direct3D;

{$MODE Delphi}

{$MINENUMSIZE 4}
{$ALIGN ON}

interface

uses
  Windows,
  DirectDraw;

(* TD3DValue is the fundamental Direct3D fractional data type *)

type
  TRefClsID = TGUID;

type
  TD3DValue = Single;
  TD3DFixed = LongInt;
  float = TD3DValue;
  PD3DColor = ^TD3DColor;
  TD3DColor = DWORD;

function D3DVal(val: variant) : float;
function D3DDivide(a,b: double) : float;
function D3DMultiply(a,b: double) : float;

(*
 * Format of CI colors is
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |    alpha      |         color index           |   fraction    |
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *)

// #define CI_GETALPHA(ci)    ((ci) >> 24)
function CI_GETALPHA(ci: DWORD) : DWORD;

// #define CI_GETINDEX(ci)    (((ci) >> 8) & 0xffff)
function CI_GETINDEX(ci: DWORD) : DWORD;

// #define CI_GETFRACTION(ci) ((ci) & 0xff)
function CI_GETFRACTION(ci: DWORD) : DWORD;

// #define CI_ROUNDINDEX(ci)  CI_GETINDEX((ci) + 0x80)
function CI_ROUNDINDEX(ci: DWORD) : DWORD;

// #define CI_MASKALPHA(ci)   ((ci) & 0xffffff)
function CI_MASKALPHA(ci: DWORD) : DWORD;

// #define CI_MAKE(a, i, f)    (((a) << 24) | ((i) << 8) | (f))
function CI_MAKE(a,i,f: DWORD) : DWORD;

(*
 * Format of RGBA colors is
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |    alpha      |      red      |     green     |     blue      |
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *)

// #define RGBA_GETALPHA(rgb)      ((rgb) >> 24)
function RGBA_GETALPHA(rgb: TD3DColor) : DWORD;

// #define RGBA_GETRED(rgb)        (((rgb) >> 16) & 0xff)
function RGBA_GETRED(rgb: TD3DColor) : DWORD;

// #define RGBA_GETGREEN(rgb)      (((rgb) >> 8) & 0xff)
function RGBA_GETGREEN(rgb: TD3DColor) : DWORD;

// #define RGBA_GETBLUE(rgb)       ((rgb) & 0xff)
function RGBA_GETBLUE(rgb: TD3DColor) : DWORD;

// #define RGBA_MAKE(r, g, b, a)   ((TD3DColor) (((a) << 24) | ((r) << 16) | ((g) << 8) | (b)))
function RGBA_MAKE(r, g, b, a: DWORD) : TD3DColor;

(* D3DRGB and D3DRGBA may be used as initialisers for D3DCOLORs
 * The float values must be in the range 0..1
 *)

// #define D3DRGB(r, g, b) \
//     (0xff000000L | (((long)((r) * 255)) << 16) | (((long)((g) * 255)) << 8) | (long)((b) * 255))
function D3DRGB(r, g, b: float) : TD3DColor;

// #define D3DRGBA(r, g, b, a) \
//     (  (((long)((a) * 255)) << 24) | (((long)((r) * 255)) << 16) \
//     |   (((long)((g) * 255)) << 8) | (long)((b) * 255) \
//    )
function D3DRGBA(r, g, b, a: float) : TD3DColor;

(*
 * Format of RGB colors is
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |    ignored    |      red      |     green     |     blue      |
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *)

// #define RGB_GETRED(rgb)         (((rgb) >> 16) & 0xff)
function RGB_GETRED(rgb: TD3DColor) : DWORD;

// #define RGB_GETGREEN(rgb)       (((rgb) >> 8) & 0xff)
function RGB_GETGREEN(rgb: TD3DColor) : DWORD;

// #define RGB_GETBLUE(rgb)        ((rgb) & 0xff)
function RGB_GETBLUE(rgb: TD3DColor) : DWORD;

// #define RGBA_SETALPHA(rgba, x) (((x) << 24) | ((rgba) & 0x00ffffff))
function RGBA_SETALPHA(rgba: TD3DColor; x: DWORD) : TD3DColor;

// #define RGB_MAKE(r, g, b)       ((TD3DColor) (((r) << 16) | ((g) << 8) | (b)))
function RGB_MAKE(r, g, b: DWORD) : TD3DColor;

// #define RGBA_TORGB(rgba)       ((TD3DColor) ((rgba) & 0xffffff))
function RGBA_TORGB(rgba: TD3DColor) : TD3DColor;

// #define RGB_TORGBA(rgb)        ((TD3DColor) ((rgb) | 0xff000000))
function RGB_TORGBA(rgb: TD3DColor) : TD3DColor;

(*
 * Flags for Enumerate functions
 *)
const

(*
 * Stop the enumeration
 *)

  D3DENUMRET_CANCEL                        = DDENUMRET_CANCEL;

(*
 * Continue the enumeration
 *)

  D3DENUMRET_OK                            = DDENUMRET_OK;

type
  TD3DValidateCallback = function (lpUserArg: Pointer;
      dwOffset: DWORD): HResult; stdcall;
  TD3DEnumTextureFormatsCallback = function (var lpDdsd: TDDSurfaceDesc;
      lpContext: Pointer): HResult; stdcall;
  TD3DEnumPixelFormatsCallback = function (var lpDDPixFmt: TDDPixelFormat;
      lpContext: Pointer): HResult; stdcall;


  PD3DMaterialHandle = ^TD3DMaterialHandle;
  TD3DMaterialHandle = DWORD;

  PD3DTextureHandle = ^TD3DTextureHandle;
  TD3DTextureHandle = DWORD;

  PD3DMatrixHandle = ^TD3DMatrixHandle;
  TD3DMatrixHandle = DWORD;

  PD3DColorValue = ^TD3DColorValue;
  TD3DColorValue = packed record
    case Integer of
    0: (
      r: TD3DValue;
      g: TD3DValue;
      b: TD3DValue;
      a: TD3DValue;
     );
    1: (
      dvR: TD3DValue;
      dvG: TD3DValue;
      dvB: TD3DValue;
      dvA: TD3DValue;
     );
  end;

  PD3DRect = ^TD3DRect;
  TD3DRect = packed record
    case Integer of
    0: (
      x1: LongInt;
      y1: LongInt;
      x2: LongInt;
      y2: LongInt;
     );
    1: (
      lX1: LongInt;
      lY1: LongInt;
      lX2: LongInt;
      lY2: LongInt;
     );
     2: (
       a: array[0..3] of LongInt;
     );
  end;

  PD3DVector = ^TD3DVector;
  TD3DVector = packed record
    case Integer of
    0: (
      x: TD3DValue;
      y: TD3DValue;
      z: TD3DValue;
     );
    1: (
      dvX: TD3DValue;
      dvY: TD3DValue;
      dvZ: TD3DValue;
     );
  end;

(******************************************************************
 *                                                                *
 *   D3DVec.inl                                                   *
 *                                                                *
 *   Float-valued 3D vector class for Direct3D.                   *
 *                                                                *
 *   Copyright (c) 1996-1998 Microsoft Corp. All rights reserved. *
 *                                                                *
 ******************************************************************)

    // Addition and subtraction
  function VectorAdd(const v1, v2: TD3DVector) : TD3DVector;
  function VectorSub(const v1, v2: TD3DVector) : TD3DVector;
    // Scalar multiplication and division
  function VectorMulS(const v: TD3DVector; s: TD3DValue) : TD3DVector;
  function VectorDivS(const v: TD3DVector; s: TD3DValue) : TD3DVector;
    // Memberwise multiplication and division
  function VectorMul(const v1, v2: TD3DVector) : TD3DVector;
  function VectorDiv(const v1, v2: TD3DVector) : TD3DVector;
    // Vector dominance
  function VectorSmaller(v1, v2: TD3DVector) : boolean;
  function VectorSmallerEquel(v1, v2: TD3DVector) : boolean;
    // Bitwise equality
  function VectorEquel(v1, v2: TD3DVector) : boolean;
    // Length-related functions
  function VectorSquareMagnitude(v: TD3DVector) : TD3DValue;
  function VectorMagnitude(v: TD3DVector) : TD3DValue;
    // Returns vector with same direction and unit length
  function VectorNormalize(const v: TD3DVector) : TD3DVector;
    // Return min/max component of the input vector
  function VectorMin(v: TD3DVector) : TD3DValue;
  function VectorMax(v: TD3DVector) : TD3DValue;
    // Return memberwise min/max of input vectors
  function VectorMinimize(const v1, v2: TD3DVector) : TD3DVector;
  function VectorMaximize(const v1, v2: TD3DVector) : TD3DVector;
    // Dot and cross product
  function VectorDotProduct(v1, v2: TD3DVector) : TD3DValue;
  function VectorCrossProduct(const v1, v2: TD3DVector) : TD3DVector;

type
(*
 * Vertex data types supported in an ExecuteBuffer.
 *)

(*
 * Homogeneous vertices
 *)

  PD3DHVertex = ^TD3DHVertex;
  TD3DHVertex = packed record
    dwFlags: DWORD;        (* Homogeneous clipping flags *)
    case Integer of
    0: (
      hx: TD3DValue;
      hy: TD3DValue;
      hz: TD3DValue;
     );
    1: (
      dvHX: TD3DValue;
      dvHY: TD3DValue;
      dvHZ: TD3DValue;
     );
  end;

(*
 * Transformed/lit vertices
 *)

  PD3DTLVertex = ^TD3DTLVertex;
  TD3DTLVertex = packed record
    case Integer of
    0: (
      sx: TD3DValue;             (* Screen coordinates *)
      sy: TD3DValue;
      sz: TD3DValue;
      rhw: TD3DValue;            (* Reciprocal of homogeneous w *)
      color: TD3DColor;          (* Vertex color *)
      specular: TD3DColor;       (* Specular component of vertex *)
      tu: TD3DValue;             (* Texture coordinates *)
      tv: TD3DValue;
     );
    1: (
      dvSX: TD3DValue;
      dvSY: TD3DValue;
      dvSZ: TD3DValue;
      dvRHW: TD3DValue;
      dcColor: TD3DColor;
      dcSpecular: TD3DColor;
      dvTU: TD3DValue;
      dvTV: TD3DValue;
     );
  end;

(*
 * Untransformed/lit vertices
 *)

  PD3DLVertex = ^TD3DLVertex;
  TD3DLVertex = packed record
    case Integer of
    0: (
      x: TD3DValue;             (* Homogeneous coordinates *)
      y: TD3DValue;
      z: TD3DValue;
      dwReserved: DWORD;
      color: TD3DColor;         (* Vertex color *)
      specular: TD3DColor;      (* Specular component of vertex *)
      tu: TD3DValue;            (* Texture coordinates *)
      tv: TD3DValue;
     );
    1: (
      dvX: TD3DValue;
      dvY: TD3DValue;
      dvZ: TD3DValue;
      UNIONFILLER1d: DWORD;
      dcColor: TD3DColor;
      dcSpecular: TD3DColor;
      dvTU: TD3DValue;
      dvTV: TD3DValue;
     );
  end;

(*
 * Untransformed/unlit vertices
 *)

  PD3DVertex = ^TD3DVertex;
  TD3DVertex = packed record
    case Integer of
    0: (
      x: TD3DValue;             (* Homogeneous coordinates *)
      y: TD3DValue;
      z: TD3DValue;
      nx: TD3DValue;            (* Normal *)
      ny: TD3DValue;
      nz: TD3DValue;
      tu: TD3DValue;            (* Texture coordinates *)
      tv: TD3DValue;
     );
    1: (
      dvX: TD3DValue;
      dvY: TD3DValue;
      dvZ: TD3DValue;
      dvNX: TD3DValue;
      dvNY: TD3DValue;
      dvNZ: TD3DValue;
      dvTU: TD3DValue;
      dvTV: TD3DValue;
     );
  end;

(*
 * Matrix, viewport, and tranformation structures and definitions.
 *)

  PD3DMatrix = ^TD3DMatrix;
  TD3DMatrix = packed record
    case integer of
      0 : (_11, _12, _13, _14: TD3DValue;
           _21, _22, _23, _24: TD3DValue;
           _31, _32, _33, _34: TD3DValue;
           _41, _42, _43, _44: TD3DValue);
      1 : (m : array [0..3, 0..3] of TD3DValue);
  end;

  PD3DViewport = ^TD3DViewport;
  TD3DViewport = packed record
    dwSize: DWORD;
    dwX: DWORD;
    dwY: DWORD;                (* Top left *)
    dwWidth: DWORD;
    dwHeight: DWORD;           (* Dimensions *)
    dvScaleX: TD3DValue;       (* Scale homogeneous to screen *)
    dvScaleY: TD3DValue;       (* Scale homogeneous to screen *)
    dvMaxX: TD3DValue;         (* Min/max homogeneous x coord *)
    dvMaxY: TD3DValue;         (* Min/max homogeneous y coord *)
    dvMinZ: TD3DValue;
    dvMaxZ: TD3DValue;         (* Min/max homogeneous z coord *)
  end;

  PD3DViewport2 = ^TD3DViewport2;
  TD3DViewport2 = packed record
    dwSize: DWORD;
    dwX: DWORD;
    dwY: DWORD;                (* Viewport Top left *)
    dwWidth: DWORD;
    dwHeight: DWORD;           (* Viewport Dimensions *)
    dvClipX: TD3DValue;	       (* Top left of clip volume *)
    dvClipY: TD3DValue;
    dvClipWidth: TD3DValue;    (* Clip Volume Dimensions *)
    dvClipHeight: TD3DValue;
    dvMinZ: TD3DValue;         (* Min/max of clip Volume *)
    dvMaxZ: TD3DValue;
  end;

  PD3DViewport7 = ^TD3DViewport7;
  TD3DViewport7 = packed record
    dwX: DWORD;
    dwY: DWORD;                (* Viewport Top left *)
    dwWidth: DWORD;
    dwHeight: DWORD;           (* Viewport Dimensions *)
    dvMinZ: TD3DValue;         (* Min/max of clip Volume *)
    dvMaxZ: TD3DValue;
  end;

(*
 * Values for clip fields.
 *)

const
// Max number of user clipping planes, supported in D3D.
  D3DMAXUSERCLIPPLANES  = 32;

// These bits could be ORed together to use with D3DRENDERSTATE_CLIPPLANEENABLE
//
  D3DCLIPPLANE0 = (1 shl 0);
  D3DCLIPPLANE1 = (1 shl 1);
  D3DCLIPPLANE2 = (1 shl 2);
  D3DCLIPPLANE3 = (1 shl 3);
  D3DCLIPPLANE4 = (1 shl 4);
  D3DCLIPPLANE5 = (1 shl 5);

const
  D3DCLIP_LEFT                            = $00000001;
  D3DCLIP_RIGHT                           = $00000002;
  D3DCLIP_TOP                             = $00000004;
  D3DCLIP_BOTTOM                          = $00000008;
  D3DCLIP_FRONT                           = $00000010;
  D3DCLIP_BACK                            = $00000020;
  D3DCLIP_GEN0                            = $00000040;
  D3DCLIP_GEN1                            = $00000080;
  D3DCLIP_GEN2                            = $00000100;
  D3DCLIP_GEN3                            = $00000200;
  D3DCLIP_GEN4                            = $00000400;
  D3DCLIP_GEN5                            = $00000800;

(*
 * Values for d3d status.
 *)

  D3DSTATUS_CLIPUNIONLEFT                 = D3DCLIP_LEFT;
  D3DSTATUS_CLIPUNIONRIGHT                = D3DCLIP_RIGHT;
  D3DSTATUS_CLIPUNIONTOP                  = D3DCLIP_TOP;
  D3DSTATUS_CLIPUNIONBOTTOM               = D3DCLIP_BOTTOM;
  D3DSTATUS_CLIPUNIONFRONT                = D3DCLIP_FRONT;
  D3DSTATUS_CLIPUNIONBACK                 = D3DCLIP_BACK;
  D3DSTATUS_CLIPUNIONGEN0                 = D3DCLIP_GEN0;
  D3DSTATUS_CLIPUNIONGEN1                 = D3DCLIP_GEN1;
  D3DSTATUS_CLIPUNIONGEN2                 = D3DCLIP_GEN2;
  D3DSTATUS_CLIPUNIONGEN3                 = D3DCLIP_GEN3;
  D3DSTATUS_CLIPUNIONGEN4                 = D3DCLIP_GEN4;
  D3DSTATUS_CLIPUNIONGEN5                 = D3DCLIP_GEN5;

  D3DSTATUS_CLIPINTERSECTIONLEFT          = $00001000;
  D3DSTATUS_CLIPINTERSECTIONRIGHT         = $00002000;
  D3DSTATUS_CLIPINTERSECTIONTOP           = $00004000;
  D3DSTATUS_CLIPINTERSECTIONBOTTOM        = $00008000;
  D3DSTATUS_CLIPINTERSECTIONFRONT         = $00010000;
  D3DSTATUS_CLIPINTERSECTIONBACK          = $00020000;
  D3DSTATUS_CLIPINTERSECTIONGEN0          = $00040000;
  D3DSTATUS_CLIPINTERSECTIONGEN1          = $00080000;
  D3DSTATUS_CLIPINTERSECTIONGEN2          = $00100000;
  D3DSTATUS_CLIPINTERSECTIONGEN3          = $00200000;
  D3DSTATUS_CLIPINTERSECTIONGEN4          = $00400000;
  D3DSTATUS_CLIPINTERSECTIONGEN5          = $00800000;
  D3DSTATUS_ZNOTVISIBLE                   = $01000000;
(* Do not use 0x80000000 for any status flags in future as it is reserved *)

  D3DSTATUS_CLIPUNIONALL = (
            D3DSTATUS_CLIPUNIONLEFT or
            D3DSTATUS_CLIPUNIONRIGHT or
            D3DSTATUS_CLIPUNIONTOP or
            D3DSTATUS_CLIPUNIONBOTTOM or
            D3DSTATUS_CLIPUNIONFRONT or
            D3DSTATUS_CLIPUNIONBACK or
            D3DSTATUS_CLIPUNIONGEN0 or
            D3DSTATUS_CLIPUNIONGEN1 or
            D3DSTATUS_CLIPUNIONGEN2 or
            D3DSTATUS_CLIPUNIONGEN3 or
            D3DSTATUS_CLIPUNIONGEN4 or
            D3DSTATUS_CLIPUNIONGEN5);

  D3DSTATUS_CLIPINTERSECTIONALL = (
            D3DSTATUS_CLIPINTERSECTIONLEFT or
            D3DSTATUS_CLIPINTERSECTIONRIGHT or
            D3DSTATUS_CLIPINTERSECTIONTOP or
            D3DSTATUS_CLIPINTERSECTIONBOTTOM or
            D3DSTATUS_CLIPINTERSECTIONFRONT or
            D3DSTATUS_CLIPINTERSECTIONBACK or
            D3DSTATUS_CLIPINTERSECTIONGEN0 or
            D3DSTATUS_CLIPINTERSECTIONGEN1 or
            D3DSTATUS_CLIPINTERSECTIONGEN2 or
            D3DSTATUS_CLIPINTERSECTIONGEN3 or
            D3DSTATUS_CLIPINTERSECTIONGEN4 or
            D3DSTATUS_CLIPINTERSECTIONGEN5);

  D3DSTATUS_DEFAULT = (
            D3DSTATUS_CLIPINTERSECTIONALL or
            D3DSTATUS_ZNOTVISIBLE);

(*
 * Options for direct transform calls
 *)

  D3DTRANSFORM_CLIPPED       = $00000001;
  D3DTRANSFORM_UNCLIPPED     = $00000002;

type
  PD3DTransformData = ^TD3DTransformData;
  TD3DTransformData = packed record
    dwSize: DWORD;
    lpIn: Pointer;             (* Input vertices *)
    dwInSize: DWORD;           (* Stride of input vertices *)
    lpOut: Pointer;            (* Output vertices *)
    dwOutSize: DWORD;          (* Stride of output vertices *)
    lpHOut: ^TD3DHVertex;       (* Output homogeneous vertices *)
    dwClip: DWORD;             (* Clipping hint *)
    dwClipIntersection: DWORD;
    dwClipUnion: DWORD;        (* Union of all clip flags *)
    drExtent: TD3DRect;         (* Extent of transformed vertices *)
  end;

(*
 * Structure defining position and direction properties for lighting.
 *)

  PD3DLightingElement = ^TD3DLightingElement;
  TD3DLightingElement = packed record
    dvPosition: TD3DVector;           (* Lightable point in model space *)
    dvNormal: TD3DVector;             (* Normalised unit vector *)
  end;

(*
 * Structure defining material properties for lighting.
 *)

  PD3DMaterial = ^TD3DMaterial;
  TD3DMaterial = packed record
    dwSize: DWORD;
    case Integer of
    0: (
      diffuse: TD3DColorValue;        (* Diffuse color RGBA *)
      ambient: TD3DColorValue;        (* Ambient color RGB *)
      specular: TD3DColorValue;       (* Specular 'shininess' *)
      emissive: TD3DColorValue;       (* Emissive color RGB *)
      power: TD3DValue;               (* Sharpness if specular highlight *)
      hTexture: TD3DTextureHandle;    (* Handle to texture map *)
      dwRampSize: DWORD;
     );
    1: (
      dcvDiffuse: TD3DColorValue;
      dcvAmbient: TD3DColorValue;
      dcvSpecular: TD3DColorValue;
      dcvEmissive: TD3DColorValue;
      dvPower: TD3DValue;
     );
  end;

  PD3DMaterial7 = ^TD3DMaterial7;
  TD3DMaterial7 = packed record
    case Integer of
    0: (
      diffuse: TD3DColorValue;        (* Diffuse color RGBA *)
      ambient: TD3DColorValue;        (* Ambient color RGB *)
      specular: TD3DColorValue;       (* Specular 'shininess' *)
      emissive: TD3DColorValue;       (* Emissive color RGB *)
      power: TD3DValue;               (* Sharpness if specular highlight *)
     );
    1: (
      dcvDiffuse: TD3DColorValue;
      dcvAmbient: TD3DColorValue;
      dcvSpecular: TD3DColorValue;
      dcvEmissive: TD3DColorValue;
      dvPower: TD3DValue;
     );
  end;

  PD3DLightType = ^TD3DLightType;
  TD3DLightType = (
    D3DLIGHT_INVALID_0,
    D3DLIGHT_POINT,
    D3DLIGHT_SPOT,
    D3DLIGHT_DIRECTIONAL,
// Note: The following light type (D3DLIGHT_PARALLELPOINT)
// is no longer supported from D3D for DX7 onwards.
    D3DLIGHT_PARALLELPOINT,
    D3DLIGHT_GLSPOT);

(*
 * Structure defining a light source and its properties.
 *)

  PD3DLight = ^TD3DLight;
  TD3DLight = packed record
    dwSize: DWORD;
    dltType: TD3DLightType;     (* Type of light source *)
    dcvColor: TD3DColorValue;   (* Color of light *)
    dvPosition: TD3DVector;     (* Position in world space *)
    dvDirection: TD3DVector;    (* Direction in world space *)
    dvRange: TD3DValue;         (* Cutoff range *)
    dvFalloff: TD3DValue;       (* Falloff *)
    dvAttenuation0: TD3DValue;  (* Constant attenuation *)
    dvAttenuation1: TD3DValue;  (* Linear attenuation *)
    dvAttenuation2: TD3DValue;  (* Quadratic attenuation *)
    dvTheta: TD3DValue;         (* Inner angle of spotlight cone *)
    dvPhi: TD3DValue;           (* Outer angle of spotlight cone *)
  end;

  PD3DLight7 = ^TD3DLight7;
  TD3DLight7 = packed record
    dltType: TD3DLightType;     (* Type of light source *)
    dcvDiffuse: TD3DColorValue; (* Diffuse color of light *)
    dcvSpecular: TD3DColorValue;(* Specular color of light *)
    dcvAmbient: TD3DColorValue; (* Ambient color of light *)
    dvPosition: TD3DVector;     (* Position in world space *)
    dvDirection: TD3DVector;    (* Direction in world space *)
    dvRange: TD3DValue;         (* Cutoff range *)
    dvFalloff: TD3DValue;       (* Falloff *)
    dvAttenuation0: TD3DValue;  (* Constant attenuation *)
    dvAttenuation1: TD3DValue;  (* Linear attenuation *)
    dvAttenuation2: TD3DValue;  (* Quadratic attenuation *)
    dvTheta: TD3DValue;         (* Inner angle of spotlight cone *)
    dvPhi: TD3DValue;           (* Outer angle of spotlight cone *)
  end;

(*
 * Structure defining a light source and its properties.
 *)

(* flags bits *)
const
  D3DLIGHT_ACTIVE		  	= $00000001;
  D3DLIGHT_NO_SPECULAR  = $00000002;
  D3DLIGHT_ALL = D3DLIGHT_ACTIVE or D3DLIGHT_ACTIVE;

(* maximum valid light range *)
  D3DLIGHT_RANGE_MAX		= 1.8439088915e+18; //sqrt(FLT_MAX);

type
  PD3DLight2 = ^TD3DLight2;
  TD3DLight2 = packed record
    dwSize: DWORD;
    dltType: TD3DLightType;     (* Type of light source *)
    dcvColor: TD3DColorValue;   (* Color of light *)
    dvPosition: TD3DVector;     (* Position in world space *)
    dvDirection: TD3DVector;    (* Direction in world space *)
    dvRange: TD3DValue;         (* Cutoff range *)
    dvFalloff: TD3DValue;       (* Falloff *)
    dvAttenuation0: TD3DValue;  (* Constant attenuation *)
    dvAttenuation1: TD3DValue;  (* Linear attenuation *)
    dvAttenuation2: TD3DValue;  (* Quadratic attenuation *)
    dvTheta: TD3DValue;         (* Inner angle of spotlight cone *)
    dvPhi: TD3DValue;           (* Outer angle of spotlight cone *)
    dwFlags: DWORD;
  end;

  PD3DLightData = ^TD3DLightData;
  TD3DLightData = packed record
    dwSize: DWORD;
    lpIn: ^TD3DLightingElement;   (* Input positions and normals *)
    dwInSize: DWORD;             (* Stride of input elements *)
    lpOut: ^TD3DTLVertex;         (* Output colors *)
    dwOutSize: DWORD;            (* Stride of output colors *)
  end;

(*
 * Before DX5, these values were in an enum called
 * TD3DColorModel. This was not correct, since they are
 * bit flags. A driver can surface either or both flags
 * in the dcmColorModel member of D3DDEVICEDESC.
 *)

type
  TD3DColorModel = DWORD;
 
const
  D3DCOLOR_MONO = 1;
  D3DCOLOR_RGB  = 2;

(*
 * Options for clearing
 *)

const
  D3DCLEAR_TARGET            = $00000001; (* Clear target surface *)
  D3DCLEAR_ZBUFFER           = $00000002; (* Clear target z buffer *)
  D3DCLEAR_STENCIL           = $00000004; (* Clear stencil planes *)

(*
 * Execute buffers are allocated via Direct3D.  These buffers may then
 * be filled by the application with instructions to execute along with
 * vertex data.
 *)

(*
 * Supported op codes for execute instructions.
 *)

type
  PD3DOpcode = ^TD3DOpcode;
  TD3DOpcode = (
    D3DOP_INVALID_0,
    D3DOP_POINT,
    D3DOP_LINE,
    D3DOP_TRIANGLE,
    D3DOP_MATRIXLOAD,
    D3DOP_MATRIXMULTIPLY,
    D3DOP_STATETRANSFORM,
    D3DOP_STATELIGHT,
    D3DOP_STATERENDER,
    D3DOP_PROCESSVERTICES,
    D3DOP_TEXTURELOAD,
    D3DOP_EXIT,
    D3DOP_BRANCHFORWARD,
    D3DOP_SPAN,
    D3DOP_SETSTATUS);

  PD3DInstruction = ^TD3DInstruction;
  TD3DInstruction = packed record
    bOpcode: BYTE;   (* Instruction opcode *)
    bSize: BYTE;     (* Size of each instruction data unit *)
    wCount: WORD;    (* Count of instruction data units to follow *)
  end;

(*
 * Structure for texture loads
 *)

  PD3DTextureLoad = ^TD3DTextureLoad;
  TD3DTextureLoad = packed record
    hDestTexture: TD3DTextureHandle;
    hSrcTexture: TD3DTextureHandle;
  end;

(*
 * Structure for picking
 *)

  PD3DPickRecord = ^TD3DPickRecord;
  TD3DPickRecord = packed record
    bOpcode: BYTE;
    bPad: BYTE;
    dwOffset: DWORD;
    dvZ: TD3DValue;
  end;

(*
 * The following defines the rendering states which can be set in the
 * execute buffer.
 *)

  PD3DShadeMode = ^TD3DShadeMode;
  TD3DShadeMode = (
    D3DSHADE_INVALID_0,
    D3DSHADE_FLAT,
    D3DSHADE_GOURAUD,
    D3DSHADE_PHONG);

  PD3DFillMode = ^TD3DFillMode;
  TD3DFillMode = (
    D3DFILL_INVALID_0,
    D3DFILL_POINT,
    D3DFILL_WIREFRAME,
    D3DFILL_SOLID);

  PD3DLinePattern = ^TD3DLinePattern;
  TD3DLinePattern = packed record
    wRepeatFactor: WORD;
    wLinePattern: WORD;
  end;

  PD3DTextureFilter = ^TD3DTextureFilter;
  TD3DTextureFilter = (
    D3DFILTER_INVALID_0,
    D3DFILTER_NEAREST,
    D3DFILTER_LINEAR,
    D3DFILTER_MIPNEAREST,
    D3DFILTER_MIPLINEAR,
    D3DFILTER_LINEARMIPNEAREST,
    D3DFILTER_LINEARMIPLINEAR);

  PD3DBlend = ^TD3DBlend;
  TD3DBlend = (
    D3DBLEND_INVALID_0,
    D3DBLEND_ZERO,
    D3DBLEND_ONE,
    D3DBLEND_SRCCOLOR,
    D3DBLEND_INVSRCCOLOR,
    D3DBLEND_SRCALPHA,
    D3DBLEND_INVSRCALPHA,
    D3DBLEND_DESTALPHA,
    D3DBLEND_INVDESTALPHA,
    D3DBLEND_DESTCOLOR,
    D3DBLEND_INVDESTCOLOR,
    D3DBLEND_SRCALPHASAT,
    D3DBLEND_BOTHSRCALPHA,
    D3DBLEND_BOTHINVSRCALPHA);

  PD3DTextureBlend = ^TD3DTextureBlend;
  TD3DTextureBlend = (
    D3DTBLEND_INVALID_0,
    D3DTBLEND_DECAL,
    D3DTBLEND_MODULATE,
    D3DTBLEND_DECALALPHA,
    D3DTBLEND_MODULATEALPHA,
    D3DTBLEND_DECALMASK,
    D3DTBLEND_MODULATEMASK,
    D3DTBLEND_COPY,
    D3DTBLEND_ADD);

  PD3DTextureAddress = ^TD3DTextureAddress;
  TD3DTextureAddress = (
    D3DTADDRESS_INVALID_0,
    D3DTADDRESS_WRAP,
    D3DTADDRESS_MIRROR,
    D3DTADDRESS_CLAMP,
    D3DTADDRESS_BORDER);

  PD3DCull = ^TD3DCull;
  TD3DCull = (
    D3DCULL_INVALID_0,
    D3DCULL_NONE,
    D3DCULL_CW,
    D3DCULL_CCW);

  PD3DCmpFunc = ^TD3DCmpFunc;
  TD3DCmpFunc = (
    D3DCMP_INVALID_0,
    D3DCMP_NEVER,
    D3DCMP_LESS,
    D3DCMP_EQUAL,
    D3DCMP_LESSEQUAL,
    D3DCMP_GREATER,
    D3DCMP_NOTEQUAL,
    D3DCMP_GREATEREQUAL,
    D3DCMP_ALWAYS);

  PD3DStencilOp = ^TD3DStencilOp;
  TD3DStencilOp = (
    D3DSTENCILOP_INVALID_0,
    D3DSTENCILOP_KEEP,
    D3DSTENCILOP_ZERO,
    D3DSTENCILOP_REPLACE,
    D3DSTENCILOP_INCRSAT,
    D3DSTENCILOP_DECRSAT,
    D3DSTENCILOP_INVERT,
    D3DSTENCILOP_INCR,
    D3DSTENCILOP_DECR);
    
  PD3DFogMode = ^TD3DFogMode;
  TD3DFogMode = (
    D3DFOG_NONE,
    D3DFOG_EXP,
    D3DFOG_EXP2,
    D3DFOG_LINEAR);

  PD3DZBufferType = ^TD3DZBufferType;
  TD3DZBufferType = (
    D3DZB_FALSE,
    D3DZB_TRUE,   // Z buffering
    D3DZB_USEW);  // W buffering

  PD3DAntialiasMode = ^TD3DAntialiasMode;
  TD3DAntialiasMode = (
    D3DANTIALIAS_NONE,
    D3DANTIALIAS_SORTDEPENDENT,
    D3DANTIALIAS_SORTINDEPENDENT);

// Vertex types supported by Direct3D
  PD3DVertexType = ^TD3DVertexType;
  TD3DVertexType = (
    D3DVT_INVALID_0,
    D3DVT_VERTEX,
    D3DVT_LVERTEX,
    D3DVT_TLVERTEX);

// Primitives supported by draw-primitive API
  PD3DPrimitiveType = ^TD3DPrimitiveType;
  TD3DPrimitiveType = (
    D3DPT_INVALID_0,
    D3DPT_POINTLIST,
    D3DPT_LINELIST,
    D3DPT_LINESTRIP,
    D3DPT_TRIANGLELIST,
    D3DPT_TRIANGLESTRIP,
    D3DPT_TRIANGLEFAN);

(*
 * Amount to add to a state to generate the override for that state.
 *)

const
  D3DSTATE_OVERRIDE_BIAS          = 256;

(*
 * A state which sets the override flag for the specified state type.
 *)

function D3DSTATE_OVERRIDE(StateType: DWORD) : DWORD;

type
  PD3DTransformStateType = ^TD3DTransformStateType;
  TD3DTransformStateType = DWORD;
const
  D3DTRANSFORMSTATE_WORLD         = 1;
  D3DTRANSFORMSTATE_VIEW          = 2;
  D3DTRANSFORMSTATE_PROJECTION    = 3;
  D3DTRANSFORMSTATE_WORLD1        = 4;  // 2nd matrix to blend
  D3DTRANSFORMSTATE_WORLD2        = 5;  // 3rd matrix to blend
  D3DTRANSFORMSTATE_WORLD3        = 6;  // 4th matrix to blend
  D3DTRANSFORMSTATE_TEXTURE0      = 16;
  D3DTRANSFORMSTATE_TEXTURE1      = 17;
  D3DTRANSFORMSTATE_TEXTURE2      = 18;
  D3DTRANSFORMSTATE_TEXTURE3      = 19;
  D3DTRANSFORMSTATE_TEXTURE4      = 20;
  D3DTRANSFORMSTATE_TEXTURE5      = 21;
  D3DTRANSFORMSTATE_TEXTURE6      = 22;
  D3DTRANSFORMSTATE_TEXTURE7      = 23;

type
  PD3DLightStateType = ^TD3DLightStateType;
  TD3DLightStateType = (
    D3DLIGHTSTATE_INVALID_0,
    D3DLIGHTSTATE_MATERIAL,
    D3DLIGHTSTATE_AMBIENT,
    D3DLIGHTSTATE_COLORMODEL,
    D3DLIGHTSTATE_FOGMODE,
    D3DLIGHTSTATE_FOGSTART,
    D3DLIGHTSTATE_FOGEND,
    D3DLIGHTSTATE_FOGDENSITY,
    D3DLIGHTSTATE_COLORVERTEX);

  PD3DRenderStateType = ^TD3DRenderStateType;
  TD3DRenderStateType = DWORD;
const
    D3DRENDERSTATE_ANTIALIAS          = 2;    (* D3DANTIALIASMODE *)
    D3DRENDERSTATE_TEXTUREPERSPECTIVE = 4;    (* TRUE for perspective correction *)
    D3DRENDERSTATE_ZENABLE            = 7;    (* D3DZBUFFERTYPE (or TRUE/FALSE for legacy) *)
    D3DRENDERSTATE_FILLMODE           = 8;    (* D3DFILL_MODE        *)
    D3DRENDERSTATE_SHADEMODE          = 9;    (* D3DSHADEMODE *)
    D3DRENDERSTATE_LINEPATTERN        = 10;   (* D3DLINEPATTERN *)
    D3DRENDERSTATE_ZWRITEENABLE       = 14;   (* TRUE to enable z writes *)
    D3DRENDERSTATE_ALPHATESTENABLE    = 15;   (* TRUE to enable alpha tests *)
    D3DRENDERSTATE_LASTPIXEL          = 16;   (* TRUE for last-pixel on lines *)
    D3DRENDERSTATE_SRCBLEND           = 19;   (* D3DBLEND *)
    D3DRENDERSTATE_DESTBLEND          = 20;   (* D3DBLEND *)
    D3DRENDERSTATE_CULLMODE           = 22;   (* D3DCULL *)
    D3DRENDERSTATE_ZFUNC              = 23;   (* D3DCMPFUNC *)
    D3DRENDERSTATE_ALPHAREF           = 24;   (* D3DFIXED *)
    D3DRENDERSTATE_ALPHAFUNC          = 25;   (* D3DCMPFUNC *)
    D3DRENDERSTATE_DITHERENABLE       = 26;   (* TRUE to enable dithering *)
    D3DRENDERSTATE_ALPHABLENDENABLE   = 27;   (* TRUE to enable alpha blending *)
    D3DRENDERSTATE_FOGENABLE          = 28;   (* TRUE to enable fog blending *)
    D3DRENDERSTATE_SPECULARENABLE     = 29;   (* TRUE to enable specular *)
    D3DRENDERSTATE_ZVISIBLE           = 30;   (* TRUE to enable z checking *)
    D3DRENDERSTATE_STIPPLEDALPHA      = 33;   (* TRUE to enable stippled alpha (RGB device only) *)
    D3DRENDERSTATE_FOGCOLOR           = 34;   (* D3DCOLOR *)
    D3DRENDERSTATE_FOGTABLEMODE       = 35;   (* D3DFOGMODE *)
    D3DRENDERSTATE_FOGSTART           = 36;   (* Fog start (for both vertex and pixel fog) *)
    D3DRENDERSTATE_FOGEND             = 37;   (* Fog end      *)
    D3DRENDERSTATE_FOGDENSITY         = 38;   (* Fog density  *)
    D3DRENDERSTATE_EDGEANTIALIAS      = 40;   (* TRUE to enable edge antialiasing *)
    D3DRENDERSTATE_COLORKEYENABLE     = 41;   (* TRUE to enable source colorkeyed textures *)
    D3DRENDERSTATE_ZBIAS              = 47;   (* LONG Z bias *)
    D3DRENDERSTATE_RANGEFOGENABLE     = 48;   (* Enables range-based fog *)

    D3DRENDERSTATE_STENCILENABLE      = 52;   (* BOOL enable/disable stenciling *)
    D3DRENDERSTATE_STENCILFAIL        = 53;   (* D3DSTENCILOP to do if stencil test fails *)
    D3DRENDERSTATE_STENCILZFAIL       = 54;   (* D3DSTENCILOP to do if stencil test passes and Z test fails *)
    D3DRENDERSTATE_STENCILPASS        = 55;   (* D3DSTENCILOP to do if both stencil and Z tests pass *)
    D3DRENDERSTATE_STENCILFUNC        = 56;   (* D3DCMPFUNC fn.  Stencil Test passes if ((ref & mask) stencilfn (stencil & mask)) is true *)
    D3DRENDERSTATE_STENCILREF         = 57;   (* Reference value used in stencil test *)
    D3DRENDERSTATE_STENCILMASK        = 58;   (* Mask value used in stencil test *)
    D3DRENDERSTATE_STENCILWRITEMASK   = 59;   (* Write mask applied to values written to stencil buffer *)
    D3DRENDERSTATE_TEXTUREFACTOR      = 60;   (* D3DCOLOR used for multi-texture blend *)

    (*
     * 128 values [128; 255] are reserved for texture coordinate wrap flags.
     * These are constructed with the D3DWRAP_U and D3DWRAP_V macros. Using
     * a flags word preserves forward compatibility with texture coordinates
     * that are >2D.
     *)
    D3DRENDERSTATE_WRAP0              = 128;  (* wrap for 1st texture coord. set *)
    D3DRENDERSTATE_WRAP1              = 129;  (* wrap for 2nd texture coord. set *)
    D3DRENDERSTATE_WRAP2              = 130;  (* wrap for 3rd texture coord. set *)
    D3DRENDERSTATE_WRAP3              = 131;  (* wrap for 4th texture coord. set *)
    D3DRENDERSTATE_WRAP4              = 132;  (* wrap for 5th texture coord. set *)
    D3DRENDERSTATE_WRAP5              = 133;  (* wrap for 6th texture coord. set *)
    D3DRENDERSTATE_WRAP6              = 134;  (* wrap for 7th texture coord. set *)
    D3DRENDERSTATE_WRAP7              = 135;  (* wrap for 8th texture coord. set *)
    D3DRENDERSTATE_CLIPPING            = 136;
    D3DRENDERSTATE_LIGHTING            = 137;
    D3DRENDERSTATE_EXTENTS             = 138;
    D3DRENDERSTATE_AMBIENT             = 139;
    D3DRENDERSTATE_FOGVERTEXMODE       = 140;
    D3DRENDERSTATE_COLORVERTEX         = 141;
    D3DRENDERSTATE_LOCALVIEWER         = 142;
    D3DRENDERSTATE_NORMALIZENORMALS    = 143;
    D3DRENDERSTATE_COLORKEYBLENDENABLE = 144;
    D3DRENDERSTATE_DIFFUSEMATERIALSOURCE    = 145;
    D3DRENDERSTATE_SPECULARMATERIALSOURCE   = 146;
    D3DRENDERSTATE_AMBIENTMATERIALSOURCE    = 147;
    D3DRENDERSTATE_EMISSIVEMATERIALSOURCE   = 148;
    D3DRENDERSTATE_VERTEXBLEND              = 151;
    D3DRENDERSTATE_CLIPPLANEENABLE          = 152;

//
// retired renderstates - not supported for DX7 interfaces
//
    D3DRENDERSTATE_TEXTUREHANDLE      = 1;    (* Texture handle for legacy interfaces (Texture;Texture2) *)
    D3DRENDERSTATE_TEXTUREADDRESS     = 3;    (* D3DTEXTUREADDRESS  *)
    D3DRENDERSTATE_WRAPU              = 5;    (* TRUE for wrapping in u *)
    D3DRENDERSTATE_WRAPV              = 6;    (* TRUE for wrapping in v *)
    D3DRENDERSTATE_MONOENABLE         = 11;   (* TRUE to enable mono rasterization *)
    D3DRENDERSTATE_ROP2               = 12;   (* ROP2 *)
    D3DRENDERSTATE_PLANEMASK          = 13;   (* DWORD physical plane mask *)
    D3DRENDERSTATE_TEXTUREMAG         = 17;   (* D3DTEXTUREFILTER *)
    D3DRENDERSTATE_TEXTUREMIN         = 18;   (* D3DTEXTUREFILTER *)
    D3DRENDERSTATE_TEXTUREMAPBLEND    = 21;   (* D3DTEXTUREBLEND *)
    D3DRENDERSTATE_SUBPIXEL           = 31;   (* TRUE to enable subpixel correction *)
    D3DRENDERSTATE_SUBPIXELX          = 32;   (* TRUE to enable correction in X only *)
    D3DRENDERSTATE_STIPPLEENABLE      = 39;   (* TRUE to enable stippling *)
    D3DRENDERSTATE_BORDERCOLOR        = 43;   (* Border color for texturing w/border *)
    D3DRENDERSTATE_TEXTUREADDRESSU    = 44;   (* Texture addressing mode for U coordinate *)
    D3DRENDERSTATE_TEXTUREADDRESSV    = 45;   (* Texture addressing mode for V coordinate *)
    D3DRENDERSTATE_MIPMAPLODBIAS      = 46;   (* D3DVALUE Mipmap LOD bias *)
    D3DRENDERSTATE_ANISOTROPY         = 49;   (* Max. anisotropy. 1 = no anisotropy *)
    D3DRENDERSTATE_FLUSHBATCH         = 50;   (* Explicit flush for DP batching (DX5 Only) *)
    D3DRENDERSTATE_TRANSLUCENTSORTINDEPENDENT=51; (* BOOL enable sort-independent transparency *)
    D3DRENDERSTATE_STIPPLEPATTERN00   = 64;   (* Stipple pattern 01...  *)
    D3DRENDERSTATE_STIPPLEPATTERN01   = 65;
    D3DRENDERSTATE_STIPPLEPATTERN02   = 66;
    D3DRENDERSTATE_STIPPLEPATTERN03   = 67;
    D3DRENDERSTATE_STIPPLEPATTERN04   = 68;
    D3DRENDERSTATE_STIPPLEPATTERN05   = 69;
    D3DRENDERSTATE_STIPPLEPATTERN06   = 70;
    D3DRENDERSTATE_STIPPLEPATTERN07   = 71;
    D3DRENDERSTATE_STIPPLEPATTERN08   = 72;
    D3DRENDERSTATE_STIPPLEPATTERN09   = 73;
    D3DRENDERSTATE_STIPPLEPATTERN10   = 74;
    D3DRENDERSTATE_STIPPLEPATTERN11   = 75;
    D3DRENDERSTATE_STIPPLEPATTERN12   = 76;
    D3DRENDERSTATE_STIPPLEPATTERN13   = 77;
    D3DRENDERSTATE_STIPPLEPATTERN14   = 78;
    D3DRENDERSTATE_STIPPLEPATTERN15   = 79;
    D3DRENDERSTATE_STIPPLEPATTERN16   = 80;
    D3DRENDERSTATE_STIPPLEPATTERN17   = 81;
    D3DRENDERSTATE_STIPPLEPATTERN18   = 82;
    D3DRENDERSTATE_STIPPLEPATTERN19   = 83;
    D3DRENDERSTATE_STIPPLEPATTERN20   = 84;
    D3DRENDERSTATE_STIPPLEPATTERN21   = 85;
    D3DRENDERSTATE_STIPPLEPATTERN22   = 86;
    D3DRENDERSTATE_STIPPLEPATTERN23   = 87;
    D3DRENDERSTATE_STIPPLEPATTERN24   = 88;
    D3DRENDERSTATE_STIPPLEPATTERN25   = 89;
    D3DRENDERSTATE_STIPPLEPATTERN26   = 90;
    D3DRENDERSTATE_STIPPLEPATTERN27   = 91;
    D3DRENDERSTATE_STIPPLEPATTERN28   = 92;
    D3DRENDERSTATE_STIPPLEPATTERN29   = 93;
    D3DRENDERSTATE_STIPPLEPATTERN30   = 94;
    D3DRENDERSTATE_STIPPLEPATTERN31   = 95;

//
// retired renderstate names - the values are still used under new naming conventions
//
    D3DRENDERSTATE_FOGTABLESTART      = 36;   (* Fog table start    *)
    D3DRENDERSTATE_FOGTABLEEND        = 37;   (* Fog table end      *)
    D3DRENDERSTATE_FOGTABLEDENSITY    = 38;   (* Fog table density  *)

type
// Values for material source
  PD3DMateralColorSource = ^TD3DMateralColorSource;
  TD3DMateralColorSource = (
    D3DMCS_MATERIAL,              // Color from material is used
    D3DMCS_COLOR1,                // Diffuse vertex color is used
    D3DMCS_COLOR2                 // Specular vertex color is used
  );

const
  // For back-compatibility with legacy compilations
  D3DRENDERSTATE_BLENDENABLE = D3DRENDERSTATE_ALPHABLENDENABLE;


// Bias to apply to the texture coordinate set to apply a wrap to.
   D3DRENDERSTATE_WRAPBIAS                = 128;

(* Flags to construct the WRAP render states *)
  D3DWRAP_U   = $00000001;
  D3DWRAP_V   = $00000002;

(* Flags to construct the WRAP render states for 1D thru 4D texture coordinates *)
  D3DWRAPCOORD_0   = $00000001;    // same as D3DWRAP_U
  D3DWRAPCOORD_1   = $00000002;    // same as D3DWRAP_V
  D3DWRAPCOORD_2   = $00000004;
  D3DWRAPCOORD_3   = $00000008;

function D3DRENDERSTATE_STIPPLEPATTERN(y: integer) : TD3DRenderStateType;

type
  PD3DState = ^TD3DState;
  TD3DState = packed record
    case Integer of
    0: (
      dtstTransformStateType: TD3DTransformStateType;
      dwArg: Array [ 0..0 ] of DWORD;
     );
    1: (
      dlstLightStateType: TD3DLightStateType;
      dvArg: Array [ 0..0 ] of TD3DValue;
     );
    2: (
      drstRenderStateType: TD3DRenderStateType;
     );
  end;

(*
 * Operation used to load matrices
 * hDstMat = hSrcMat
 *)
  PD3DMatrixLoad = ^TD3DMatrixLoad;
  TD3DMatrixLoad = packed record
    hDestMatrix: TD3DMatrixHandle;   (* Destination matrix *)
    hSrcMatrix: TD3DMatrixHandle;    (* Source matrix *)
  end;

(*
 * Operation used to multiply matrices
 * hDstMat = hSrcMat1 * hSrcMat2
 *)
  PD3DMatrixMultiply = ^TD3DMatrixMultiply;
  TD3DMatrixMultiply = packed record
    hDestMatrix: TD3DMatrixHandle;   (* Destination matrix *)
    hSrcMatrix1: TD3DMatrixHandle;   (* First source matrix *)
    hSrcMatrix2: TD3DMatrixHandle;   (* Second source matrix *)
  end;

(*
 * Operation used to transform and light vertices.
 *)
  PD3DProcessVertices = ^TD3DProcessVertices;
  TD3DProcessVertices = packed record
    dwFlags: DWORD;           (* Do we transform or light or just copy? *)
    wStart: WORD;             (* Index to first vertex in source *)
    wDest: WORD;              (* Index to first vertex in local buffer *)
    dwCount: DWORD;           (* Number of vertices to be processed *)
    dwReserved: DWORD;        (* Must be zero *)
  end;

const
  D3DPROCESSVERTICES_TRANSFORMLIGHT       = $00000000;
  D3DPROCESSVERTICES_TRANSFORM            = $00000001;
  D3DPROCESSVERTICES_COPY                 = $00000002;
  D3DPROCESSVERTICES_OPMASK               = $00000007;

  D3DPROCESSVERTICES_UPDATEEXTENTS        = $00000008;
  D3DPROCESSVERTICES_NOCOLOR              = $00000010;


(*
 * State enumerants for per-stage texture processing.
 *)
type
  PD3DTextureStageStateType = ^TD3DTextureStageStateType;
  TD3DTextureStageStateType = DWORD;
const
  D3DTSS_COLOROP        =  1; (* D3DTEXTUREOP - per-stage blending controls for color channels *)
  D3DTSS_COLORARG1      =  2; (* D3DTA_* (texture arg) *)
  D3DTSS_COLORARG2      =  3; (* D3DTA_* (texture arg) *)
  D3DTSS_ALPHAOP        =  4; (* D3DTEXTUREOP - per-stage blending controls for alpha channel *)
  D3DTSS_ALPHAARG1      =  5; (* D3DTA_* (texture arg) *)
  D3DTSS_ALPHAARG2      =  6; (* D3DTA_* (texture arg) *)
  D3DTSS_BUMPENVMAT00   =  7; (* D3DVALUE (bump mapping matrix) *)
  D3DTSS_BUMPENVMAT01   =  8; (* D3DVALUE (bump mapping matrix) *)
  D3DTSS_BUMPENVMAT10   =  9; (* D3DVALUE (bump mapping matrix) *)
  D3DTSS_BUMPENVMAT11   = 10; (* D3DVALUE (bump mapping matrix) *)
  D3DTSS_TEXCOORDINDEX  = 11; (* identifies which set of texture coordinates index this texture *)
  D3DTSS_ADDRESS        = 12; (* D3DTEXTUREADDRESS for both coordinates *)
  D3DTSS_ADDRESSU       = 13; (* D3DTEXTUREADDRESS for U coordinate *)
  D3DTSS_ADDRESSV       = 14; (* D3DTEXTUREADDRESS for V coordinate *)
  D3DTSS_BORDERCOLOR    = 15; (* D3DCOLOR *)
  D3DTSS_MAGFILTER      = 16; (* D3DTEXTUREMAGFILTER filter to use for magnification *)
  D3DTSS_MINFILTER      = 17; (* D3DTEXTUREMINFILTER filter to use for minification *)
  D3DTSS_MIPFILTER      = 18; (* D3DTEXTUREMIPFILTER filter to use between mipmaps during minification *)
  D3DTSS_MIPMAPLODBIAS  = 19; (* D3DVALUE Mipmap LOD bias *)
  D3DTSS_MAXMIPLEVEL    = 20; (* DWORD 0..(n-1) LOD index of largest map to use (0 == largest) *)
  D3DTSS_MAXANISOTROPY  = 21; (* DWORD maximum anisotropy *)
  D3DTSS_BUMPENVLSCALE  = 22; (* D3DVALUE scale for bump map luminance *)
  D3DTSS_BUMPENVLOFFSET = 23; (* D3DVALUE offset for bump map luminance *)
  D3DTSS_TEXTURETRANSFORMFLAGS = 24; (* D3DTEXTURETRANSFORMFLAGS controls texture transform *)

// Values, used with D3DTSS_TEXCOORDINDEX, to specify that the vertex data(position
// and normal in the camera space) should be taken as texture coordinates
// Low 16 bits are used to specify texture coordinate index, to take the WRAP mode from
//
  D3DTSS_TCI_PASSTHRU                             = $00000000;
  D3DTSS_TCI_CAMERASPACENORMAL                    = $00010000;
  D3DTSS_TCI_CAMERASPACEPOSITION                  = $00020000;
  D3DTSS_TCI_CAMERASPACEREFLECTIONVECTOR          = $00030000;

type
(*
 * Enumerations for COLOROP and ALPHAOP texture blending operations set in
 * texture processing stage controls in D3DRENDERSTATE.
 *)
  PD3DTextureOp = ^TD3DTextureOp;
  TD3DTextureOp = (
    D3DTOP_INVALID_0,
// Control
    D3DTOP_DISABLE   ,      // disables stage
    D3DTOP_SELECTARG1,      // the default
    D3DTOP_SELECTARG2,

// Modulate
    D3DTOP_MODULATE  ,      // multiply args together
    D3DTOP_MODULATE2X,      // multiply and  1 bit
    D3DTOP_MODULATE4X,      // multiply and  2 bits

// Add
    D3DTOP_ADD        ,   // add arguments together
    D3DTOP_ADDSIGNED  ,   // add with -0.5 bias
    D3DTOP_ADDSIGNED2X,   // as above but left  1 bit
    D3DTOP_SUBTRACT   ,   // Arg1 - Arg2, with no saturation
    D3DTOP_ADDSMOOTH  ,   // add 2 args, subtract product
                          // Arg1 + Arg2 - Arg1*Arg2
                          // = Arg1 + (1-Arg1)*Arg2

// Linear alpha blend: Arg1*(Alpha) + Arg2*(1-Alpha)
    D3DTOP_BLENDDIFFUSEALPHA  , // iterated alpha
    D3DTOP_BLENDTEXTUREALPHA  , // texture alpha
    D3DTOP_BLENDFACTORALPHA   , // alpha from D3DRENDERSTATE_TEXTUREFACTOR
    // Linear alpha blend with pre-multiplied arg1 input: Arg1 + Arg2*(1-Alpha)
    D3DTOP_BLENDTEXTUREALPHAPM, // texture alpha
    D3DTOP_BLENDCURRENTALPHA  , // by alpha of current color

// Specular mapping
    D3DTOP_PREMODULATE           ,     // modulate with next texture before use
    D3DTOP_MODULATEALPHA_ADDCOLOR,     // Arg1.RGB + Arg1.A*Arg2.RGB
                                       // COLOROP only
    D3DTOP_MODULATECOLOR_ADDALPHA,     // Arg1.RGB*Arg2.RGB + Arg1.A
                                            // COLOROP only
    D3DTOP_MODULATEINVALPHA_ADDCOLOR,  // (1-Arg1.A)*Arg2.RGB + Arg1.RGB
                                       // COLOROP only
    D3DTOP_MODULATEINVCOLOR_ADDALPHA,  // (1-Arg1.RGB)*Arg2.RGB + Arg1.A
                                            // COLOROP only

// Bump mapping
    D3DTOP_BUMPENVMAP         , // per pixel env map perturbation
    D3DTOP_BUMPENVMAPLUMINANCE, // with luminance channel
    // This can do either diffuse or specular bump mapping with correct input.
    // Performs the function (Arg1.R*Arg2.R + Arg1.G*Arg2.G + Arg1.B*Arg2.B)
    // where each component has been scaled and offset to make it signed.
    // The result is replicated into all four (including alpha) channels.
    // This is a valid COLOROP only.
    D3DTOP_DOTPRODUCT3
  );

(*
 * Values for COLORARG1,2 and ALPHAARG1,2 texture blending operations
 * set in texture processing stage controls in D3DRENDERSTATE.
 *)
const
  D3DTA_SELECTMASK        = $0000000f;  // mask for arg selector
  D3DTA_DIFFUSE           = $00000000;  // select diffuse color
  D3DTA_CURRENT           = $00000001;  // select result of previous stage
  D3DTA_TEXTURE           = $00000002;  // select texture color
  D3DTA_TFACTOR           = $00000003;  // select RENDERSTATE_TEXTUREFACTOR
  D3DTA_SPECULAR          = $00000004;  // select specular color
  D3DTA_COMPLEMENT        = $00000010;  // take 1.0 - x
  D3DTA_ALPHAREPLICATE    = $00000020;  // replicate alpha to color components

(*
 *  IDirect3DTexture2 State Filter Types
 *)
type
  PD3DTextureMagFilter = ^TD3DTextureMagFilter;
  TD3DTextureMagFilter = (
    D3DTFG_INVALID_0,
    D3DTFG_POINT        ,    // nearest
    D3DTFG_LINEAR       ,    // linear interpolation
    D3DTFG_FLATCUBIC    ,    // cubic
    D3DTFG_GAUSSIANCUBIC,    // different cubic kernel
    D3DTFG_ANISOTROPIC
  );

  PD3DTextureMinFilter = ^TD3DTextureMinFilter;
  TD3DTextureMinFilter = (
    D3DTFN_INVALID_0,
    D3DTFN_POINT      ,    // nearest
    D3DTFN_LINEAR     ,    // linear interpolation
    D3DTFN_ANISOTROPIC
  );

  PD3DTextureMipFilter = ^TD3DTextureMipFilter;
  TD3DTextureMipFilter = (
    D3DTFP_INVALID_0,
    D3DTFP_NONE   ,    // mipmapping disabled (use MAG filter)
    D3DTFP_POINT  ,    // nearest
    D3DTFP_LINEAR      // linear interpolation
  );


(*
 * Triangle flags
 *)

(*
 * Tri strip and fan flags.
 * START loads all three vertices
 * EVEN and ODD load just v3 with even or odd culling
 * START_FLAT contains a count from 0 to 29 that allows the
 * whole strip or fan to be culled in one hit.
 * e.g. for a quad len = 1
 *)
const
  D3DTRIFLAG_START                        = $00000000;
// #define D3DTRIFLAG_STARTFLAT(len) (len)         (* 0 < len < 30 *)
function D3DTRIFLAG_STARTFLAT(len: DWORD) : DWORD;

const
  D3DTRIFLAG_ODD                          = $0000001e;
  D3DTRIFLAG_EVEN                         = $0000001f;

(*
 * Triangle edge flags
 * enable edges for wireframe or antialiasing
 *)
  D3DTRIFLAG_EDGEENABLE1                  = $00000100; (* v0-v1 edge *)
  D3DTRIFLAG_EDGEENABLE2                  = $00000200; (* v1-v2 edge *)
  D3DTRIFLAG_EDGEENABLE3                  = $00000400; (* v2-v0 edge *)
  D3DTRIFLAG_EDGEENABLETRIANGLE = (
      D3DTRIFLAG_EDGEENABLE1 or D3DTRIFLAG_EDGEENABLE2 or D3DTRIFLAG_EDGEENABLE3);

(*
 * Primitive structures and related defines.  Vertex offsets are to types
 * TD3DVertex, TD3DLVertex, or TD3DTLVertex.
 *)

(*
 * Triangle list primitive structure
 *)
type
  PD3DTriangle = ^TD3DTriangle;
  TD3DTriangle = packed record
    case Integer of
    0: (
      v1: WORD;            (* Vertex indices *)
      v2: WORD;
      v3: WORD;
      wFlags: WORD;        (* Edge (and other) flags *)
     );
    1: (
      wV1: WORD;
      wV2: WORD;
      wV3: WORD;
     );
  end;

(*
 * Line strip structure.
 * The instruction count - 1 defines the number of line segments.
 *)
  PD3DLine = ^TD3DLine;
  TD3DLine = packed record
    case Integer of
    0: (
      v1: WORD;            (* Vertex indices *)
      v2: WORD;
     );
    1: (
      wV1: WORD;
      wV2: WORD;
     );
  end;

(*
 * Span structure
 * Spans join a list of points with the same y value.
 * If the y value changes, a new span is started.
 *)
  PD3DSpan = ^TD3DSpan;
  TD3DSpan = packed record
    wCount: WORD;        (* Number of spans *)
    wFirst: WORD;        (* Index to first vertex *)
  end;

(*
 * Point structure
 *)
  PD3DPoint = ^TD3DPoint;
  TD3DPoint = packed record
    wCount: WORD;        (* number of points         *)
    wFirst: WORD;        (* index to first vertex    *)
  end;

(*
 * Forward branch structure.
 * Mask is logically anded with the driver status mask
 * if the result equals 'value', the branch is taken.
 *)
  PD3DBranch = ^TD3DBranch;
  TD3DBranch = packed record
    dwMask: DWORD;         (* Bitmask against D3D status *)
    dwValue: DWORD;
    bNegate: BOOL;         (* TRUE to negate comparison *)
    dwOffset: DWORD;       (* How far to branch forward (0 for exit)*)
  end;

(*
 * Status used for set status instruction.
 * The D3D status is initialised on device creation
 * and is modified by all execute calls.
 *)
  PD3DStatus = ^TD3DStatus;
  TD3DStatus = packed record
    dwFlags: DWORD;        (* Do we set extents or status *)
    dwStatus: DWORD;       (* D3D status *)
    drExtent: TD3DRect;
  end;

const
  D3DSETSTATUS_STATUS    = $00000001;
  D3DSETSTATUS_EXTENTS   = $00000002;
  D3DSETSTATUS_ALL      = (D3DSETSTATUS_STATUS or D3DSETSTATUS_EXTENTS);

type
  PD3DClipStatus = ^TD3DClipStatus;
  TD3DClipStatus = packed record
    dwFlags : DWORD; (* Do we set 2d extents, 3D extents or status *)
    dwStatus : DWORD; (* Clip status *)
    minx, maxx : float; (* X extents *)
    miny, maxy : float; (* Y extents *)
    minz, maxz : float; (* Z extents *)
  end;

const
  D3DCLIPSTATUS_STATUS        = $00000001;
  D3DCLIPSTATUS_EXTENTS2      = $00000002;
  D3DCLIPSTATUS_EXTENTS3      = $00000004;

(*
 * Statistics structure
 *)
type
  PD3DStats = ^TD3DStats;
  TD3DStats = packed record
    dwSize: DWORD;
    dwTrianglesDrawn: DWORD;
    dwLinesDrawn: DWORD;
    dwPointsDrawn: DWORD;
    dwSpansDrawn: DWORD;
    dwVerticesProcessed: DWORD;
  end;

(*
 * Execute options.
 * When calling using D3DEXECUTE_UNCLIPPED all the primitives
 * inside the buffer must be contained within the viewport.
 *)
const
  D3DEXECUTE_CLIPPED       = $00000001;
  D3DEXECUTE_UNCLIPPED     = $00000002;

type
  PD3DExecuteData = ^TD3DExecuteData;
  TD3DExecuteData = packed record
    dwSize: DWORD;
    dwVertexOffset: DWORD;
    dwVertexCount: DWORD;
    dwInstructionOffset: DWORD;
    dwInstructionLength: DWORD;
    dwHVertexOffset: DWORD;
    dsStatus: TD3DStatus;       (* Status after execute *)
  end;

(*
 * Palette flags.
 * This are or'ed with the peFlags in the PALETTEENTRYs passed to DirectDraw.
 *)

const
  D3DPAL_FREE     = $00;    (* Renderer may use this entry freely *)
  D3DPAL_READONLY = $40;    (* Renderer may not set this entry *)
  D3DPAL_RESERVED = $80;    (* Renderer may not use this entry *)


type
  PD3DVertexBufferDesc = ^TD3DVertexBufferDesc;
  TD3DVertexBufferDesc = packed record
    dwSize : DWORD;
    dwCaps : DWORD;
    dwFVF : DWORD;
    dwNumVertices : DWORD;
  end;

const
(* These correspond to DDSCAPS_* flags *)
  D3DVBCAPS_SYSTEMMEMORY      = $00000800;
  D3DVBCAPS_WRITEONLY         = $00010000;
  D3DVBCAPS_OPTIMIZED         = $80000000;
  D3DVBCAPS_DONOTCLIP         = $00000001;

(* Vertex Operations for ProcessVertices *)
  D3DVOP_LIGHT      = (1 shl 10);
  D3DVOP_TRANSFORM  = (1 shl 0);
  D3DVOP_CLIP       = (1 shl 2);
  D3DVOP_EXTENTS    = (1 shl 3);

(* The maximum number of vertices user can pass to any d3d
   drawing function or to create vertex buffer with
*)
  D3DMAXNUMVERTICES  =  ((1 shl 16) - 1);
(* The maximum number of primitives user can pass to any d3d
   drawing function.
*)
  D3DMAXNUMPRIMITIVES = ((1 shl 16) - 1);

(* Bits for dwFlags in ProcessVertices call *)
  D3DPV_DONOTCOPYDATA = (1 shl 0);

//-------------------------------------------------------------------

// Flexible vertex format bits
//
  D3DFVF_RESERVED0        = $001;
  D3DFVF_POSITION_MASK    = $00E;
  D3DFVF_XYZ              = $002;
  D3DFVF_XYZRHW           = $004;
  D3DFVF_XYZB1            = $006;
  D3DFVF_XYZB2            = $008;
  D3DFVF_XYZB3            = $00a;
  D3DFVF_XYZB4            = $00c;
  D3DFVF_XYZB5            = $00e;

  D3DFVF_NORMAL           = $010;
  D3DFVF_RESERVED1        = $020;
  D3DFVF_DIFFUSE          = $040;
  D3DFVF_SPECULAR         = $080;

  D3DFVF_TEXCOUNT_MASK    = $f00;
  D3DFVF_TEXCOUNT_SHIFT   = 8;
  D3DFVF_TEX0             = $000;
  D3DFVF_TEX1             = $100;
  D3DFVF_TEX2             = $200;
  D3DFVF_TEX3             = $300;
  D3DFVF_TEX4             = $400;
  D3DFVF_TEX5             = $500;
  D3DFVF_TEX6             = $600;
  D3DFVF_TEX7             = $700;
  D3DFVF_TEX8             = $800;

  D3DFVF_RESERVED2        = $f000;  // 4 reserved bits

  D3DFVF_VERTEX = ( D3DFVF_XYZ or D3DFVF_NORMAL or D3DFVF_TEX1 );
  D3DFVF_LVERTEX = ( D3DFVF_XYZ or D3DFVF_RESERVED1 or D3DFVF_DIFFUSE or
                         D3DFVF_SPECULAR or D3DFVF_TEX1 );
  D3DFVF_TLVERTEX = ( D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_SPECULAR or
                          D3DFVF_TEX1 );

type
  PD3DDP_PtrStride = ^TD3DDP_PtrStride;
  TD3DDP_PtrStride = packed record
    lpvData : pointer;
    dwStride : DWORD;
  end;

const
  D3DDP_MAXTEXCOORD = 8;

type
  PD3DDrawPrimitiveStridedData = ^TD3DDrawPrimitiveStridedData;
  TD3DDrawPrimitiveStridedData = packed record
    position : TD3DDP_PtrStride;
    normal : TD3DDP_PtrStride;
    diffuse : TD3DDP_PtrStride;
    specular : TD3DDP_PtrStride;
    textureCoords : array [0..D3DDP_MAXTEXCOORD-1] of TD3DDP_PtrStride;
  end;

//---------------------------------------------------------------------
// ComputeSphereVisibility return values
//
const
  D3DVIS_INSIDE_FRUSTUM      = 0;
  D3DVIS_INTERSECT_FRUSTUM   = 1;
  D3DVIS_OUTSIDE_FRUSTUM     = 2;
  D3DVIS_INSIDE_LEFT         = 0;
  D3DVIS_INTERSECT_LEFT      = (1 shl 2);
  D3DVIS_OUTSIDE_LEFT        = (2 shl 2);
  D3DVIS_INSIDE_RIGHT        = 0;
  D3DVIS_INTERSECT_RIGHT     = (1 shl 4);
  D3DVIS_OUTSIDE_RIGHT       = (2 shl 4);
  D3DVIS_INSIDE_TOP          = 0;
  D3DVIS_INTERSECT_TOP       = (1 shl 6);
  D3DVIS_OUTSIDE_TOP         = (2 shl 6);
  D3DVIS_INSIDE_BOTTOM       = 0;
  D3DVIS_INTERSECT_BOTTOM    = (1 shl 8);
  D3DVIS_OUTSIDE_BOTTOM      = (2 shl 8);
  D3DVIS_INSIDE_NEAR         = 0;
  D3DVIS_INTERSECT_NEAR      = (1 shl 10);
  D3DVIS_OUTSIDE_NEAR        = (2 shl 10);
  D3DVIS_INSIDE_FAR          = 0;
  D3DVIS_INTERSECT_FAR       = (1 shl 12);
  D3DVIS_OUTSIDE_FAR         = (2 shl 12);

  D3DVIS_MASK_FRUSTUM        = (3 shl 0);
  D3DVIS_MASK_LEFT           = (3 shl 2);
  D3DVIS_MASK_RIGHT          = (3 shl 4);
  D3DVIS_MASK_TOP            = (3 shl 6);
  D3DVIS_MASK_BOTTOM         = (3 shl 8);
  D3DVIS_MASK_NEAR           = (3 shl 10);
  D3DVIS_MASK_FAR            = (3 shl 12);

// To be used with GetInfo()
  D3DDEVINFOID_TEXTUREMANAGER    = 1;
  D3DDEVINFOID_D3DTEXTUREMANAGER = 2;
  D3DDEVINFOID_TEXTURING         = 3;

type
  PD3DStateBlockType = ^TD3DStateBlockType;
  TD3DStateBlockType = (
    D3DSBT_INVALID_0   ,
    D3DSBT_ALL         , // capture all state
    D3DSBT_PIXELSTATE  , // capture pixel state
    D3DSBT_VERTEXSTATE   // capture vertex state
  );

// The D3DVERTEXBLENDFLAGS type is used with D3DRENDERSTATE_VERTEXBLEND state.
//
  PD3DVertexBlendFlags = ^TD3DVertexBlendFlags;
  TD3DVertexBlendFlags = (
    D3DVBLEND_DISABLE , // Disable vertex blending
    D3DVBLEND_1WEIGHT , // blend between 2 matrices
    D3DVBLEND_2WEIGHTS, // blend between 3 matrices
    D3DVBLEND_3WEIGHTS  // blend between 4 matrices
  );

  PD3DTextureTransformFlags = ^TD3DTextureTransformFlags;
  TD3DTextureTransformFlags = (
    D3DTTFF_DISABLE ,    // texture coordinates are passed directly
    D3DTTFF_COUNT1  ,    // rasterizer should expect 1-D texture coords
    D3DTTFF_COUNT2  ,    // rasterizer should expect 2-D texture coords
    D3DTTFF_COUNT3  ,    // rasterizer should expect 3-D texture coords
    D3DTTFF_COUNT4       // rasterizer should expect 4-D texture coords
  );

const
  D3DTTFF_PROJECTED       = TD3DTextureTransformFlags(256); // texcoords to be divided by COUNTth element

// Macros to set texture coordinate format bits in the FVF id

D3DFVF_TEXTUREFORMAT2 = 0;         // Two floating point values
D3DFVF_TEXTUREFORMAT1 = 3;         // One floating point value
D3DFVF_TEXTUREFORMAT3 = 1;         // Three floating point values
D3DFVF_TEXTUREFORMAT4 = 2;         // Four floating point values

function D3DFVF_TEXCOORDSIZE3(CoordIndex: DWORD) : DWORD;
function D3DFVF_TEXCOORDSIZE2(CoordIndex: DWORD) : DWORD;
function D3DFVF_TEXCOORDSIZE4(CoordIndex: DWORD) : DWORD;
function D3DFVF_TEXCOORDSIZE1(CoordIndex: DWORD) : DWORD;

(*==========================================================================;
 *
 *
 *  File:       d3dcaps.h
 *  Content:    Direct3D capabilities include file
 *
 ***************************************************************************)

(* Description of capabilities of transform *)

type
  PD3DTransformCaps = ^TD3DTransformCaps;
  TD3DTransformCaps = packed record
    dwSize: DWORD;
    dwCaps: DWORD;
  end;

const
  D3DTRANSFORMCAPS_CLIP         = $00000001; (* Will clip whilst transforming *)

(* Description of capabilities of lighting *)

type
  PD3DLightingCaps = ^TD3DLightingCaps;
  TD3DLightingCaps = packed record
    dwSize: DWORD;
    dwCaps: DWORD;                   (* Lighting caps *)
    dwLightingModel: DWORD;          (* Lighting model - RGB or mono *)
    dwNumLights: DWORD;              (* Number of lights that can be handled *)
  end;

const
  D3DLIGHTINGMODEL_RGB            = $00000001;
  D3DLIGHTINGMODEL_MONO           = $00000002;

  D3DLIGHTCAPS_POINT              = $00000001; (* Point lights supported *)
  D3DLIGHTCAPS_SPOT               = $00000002; (* Spot lights supported *)
  D3DLIGHTCAPS_DIRECTIONAL        = $00000004; (* Directional lights supported *)
  D3DLIGHTCAPS_PARALLELPOINT      = $00000008; (* Parallel point lights supported *)
  D3DLIGHTCAPS_GLSPOT             = $00000010; (* GL syle spot lights supported *)

(* Description of capabilities for each primitive type *)

type
  PD3DPrimCaps = ^TD3DPrimCaps;
  TD3DPrimCaps = packed record
    dwSize: DWORD;
    dwMiscCaps: DWORD;                 (* Capability flags *)
    dwRasterCaps: DWORD;
    dwZCmpCaps: DWORD;
    dwSrcBlendCaps: DWORD;
    dwDestBlendCaps: DWORD;
    dwAlphaCmpCaps: DWORD;
    dwShadeCaps: DWORD;
    dwTextureCaps: DWORD;
    dwTextureFilterCaps: DWORD;
    dwTextureBlendCaps: DWORD;
    dwTextureAddressCaps: DWORD;
    dwStippleWidth: DWORD;             (* maximum width and height of *)
    dwStippleHeight: DWORD;            (* of supported stipple (up to 32x32) *)
  end;

const
(* TD3DPrimCaps dwMiscCaps *)

  D3DPMISCCAPS_MASKPLANES         = $00000001;
  D3DPMISCCAPS_MASKZ              = $00000002;
  D3DPMISCCAPS_LINEPATTERNREP     = $00000004;
  D3DPMISCCAPS_CONFORMANT         = $00000008;
  D3DPMISCCAPS_CULLNONE           = $00000010;
  D3DPMISCCAPS_CULLCW             = $00000020;
  D3DPMISCCAPS_CULLCCW            = $00000040;

(* TD3DPrimCaps dwRasterCaps *)

  D3DPRASTERCAPS_DITHER           = $00000001;
  D3DPRASTERCAPS_ROP2             = $00000002;
  D3DPRASTERCAPS_XOR              = $00000004;
  D3DPRASTERCAPS_PAT              = $00000008;
  D3DPRASTERCAPS_ZTEST            = $00000010;
  D3DPRASTERCAPS_SUBPIXEL         = $00000020;
  D3DPRASTERCAPS_SUBPIXELX        = $00000040;
  D3DPRASTERCAPS_FOGVERTEX        = $00000080;
  D3DPRASTERCAPS_FOGTABLE         = $00000100;
  D3DPRASTERCAPS_STIPPLE          = $00000200;
  D3DPRASTERCAPS_ANTIALIASSORTDEPENDENT   = $00000400;
  D3DPRASTERCAPS_ANTIALIASSORTINDEPENDENT = $00000800;
  D3DPRASTERCAPS_ANTIALIASEDGES           = $00001000;
  D3DPRASTERCAPS_MIPMAPLODBIAS            = $00002000;
  D3DPRASTERCAPS_ZBIAS                    = $00004000;
  D3DPRASTERCAPS_ZBUFFERLESSHSR           = $00008000;
  D3DPRASTERCAPS_FOGRANGE                 = $00010000;
  D3DPRASTERCAPS_ANISOTROPY               = $00020000;
  D3DPRASTERCAPS_WBUFFER                      = $00040000;
  D3DPRASTERCAPS_TRANSLUCENTSORTINDEPENDENT   = $00080000;
  D3DPRASTERCAPS_WFOG                         = $00100000;
  D3DPRASTERCAPS_ZFOG                         = $00200000;

(* TD3DPrimCaps dwZCmpCaps, dwAlphaCmpCaps *)

const
  D3DPCMPCAPS_NEVER               = $00000001;
  D3DPCMPCAPS_LESS                = $00000002;
  D3DPCMPCAPS_EQUAL               = $00000004;
  D3DPCMPCAPS_LESSEQUAL           = $00000008;
  D3DPCMPCAPS_GREATER             = $00000010;
  D3DPCMPCAPS_NOTEQUAL            = $00000020;
  D3DPCMPCAPS_GREATEREQUAL        = $00000040;
  D3DPCMPCAPS_ALWAYS              = $00000080;

(* TD3DPrimCaps dwSourceBlendCaps, dwDestBlendCaps *)

  D3DPBLENDCAPS_ZERO              = $00000001;
  D3DPBLENDCAPS_ONE               = $00000002;
  D3DPBLENDCAPS_SRCCOLOR          = $00000004;
  D3DPBLENDCAPS_INVSRCCOLOR       = $00000008;
  D3DPBLENDCAPS_SRCALPHA          = $00000010;
  D3DPBLENDCAPS_INVSRCALPHA       = $00000020;
  D3DPBLENDCAPS_DESTALPHA         = $00000040;
  D3DPBLENDCAPS_INVDESTALPHA      = $00000080;
  D3DPBLENDCAPS_DESTCOLOR         = $00000100;
  D3DPBLENDCAPS_INVDESTCOLOR      = $00000200;
  D3DPBLENDCAPS_SRCALPHASAT       = $00000400;
  D3DPBLENDCAPS_BOTHSRCALPHA      = $00000800;
  D3DPBLENDCAPS_BOTHINVSRCALPHA   = $00001000;

(* TD3DPrimCaps dwShadeCaps *)

  D3DPSHADECAPS_COLORFLATMONO             = $00000001;
  D3DPSHADECAPS_COLORFLATRGB              = $00000002;
  D3DPSHADECAPS_COLORGOURAUDMONO          = $00000004;
  D3DPSHADECAPS_COLORGOURAUDRGB           = $00000008;
  D3DPSHADECAPS_COLORPHONGMONO            = $00000010;
  D3DPSHADECAPS_COLORPHONGRGB             = $00000020;

  D3DPSHADECAPS_SPECULARFLATMONO          = $00000040;
  D3DPSHADECAPS_SPECULARFLATRGB           = $00000080;
  D3DPSHADECAPS_SPECULARGOURAUDMONO       = $00000100;
  D3DPSHADECAPS_SPECULARGOURAUDRGB        = $00000200;
  D3DPSHADECAPS_SPECULARPHONGMONO         = $00000400;
  D3DPSHADECAPS_SPECULARPHONGRGB          = $00000800;

  D3DPSHADECAPS_ALPHAFLATBLEND            = $00001000;
  D3DPSHADECAPS_ALPHAFLATSTIPPLED         = $00002000;
  D3DPSHADECAPS_ALPHAGOURAUDBLEND         = $00004000;
  D3DPSHADECAPS_ALPHAGOURAUDSTIPPLED      = $00008000;
  D3DPSHADECAPS_ALPHAPHONGBLEND           = $00010000;
  D3DPSHADECAPS_ALPHAPHONGSTIPPLED        = $00020000;

  D3DPSHADECAPS_FOGFLAT                   = $00040000;
  D3DPSHADECAPS_FOGGOURAUD                = $00080000;
  D3DPSHADECAPS_FOGPHONG                  = $00100000;

(* TD3DPrimCaps dwTextureCaps *)

(*
 * Perspective-correct texturing is supported
 *)
  D3DPTEXTURECAPS_PERSPECTIVE     = $00000001;

(*
 * Power-of-2 texture dimensions are required
 *)
  D3DPTEXTURECAPS_POW2            = $00000002;

(*
 * Alpha in texture pixels is supported
 *)
  D3DPTEXTURECAPS_ALPHA           = $00000004;

(*
 * Color-keyed textures are supported
 *)
  D3DPTEXTURECAPS_TRANSPARENCY    = $00000008;

(*
 * obsolete, see D3DPTADDRESSCAPS_BORDER
 *)
  D3DPTEXTURECAPS_BORDER          = $00000010;

(*
 * Only square textures are supported
 *)
  D3DPTEXTURECAPS_SQUAREONLY      = $00000020;

(*
 * Texture indices are not scaled by the texture size prior
 * to interpolation.
 *)
  D3DPTEXTURECAPS_TEXREPEATNOTSCALEDBYSIZE = $00000040;

(*
 * Device can draw alpha from texture palettes
 *)
  D3DPTEXTURECAPS_ALPHAPALETTE    = $00000080;

(*
 * Device can use non-POW2 textures if:
 *  1) D3DTEXTURE_ADDRESS is set to CLAMP for this texture's stage
 *  2) D3DRS_WRAP(N) is zero for this texture's coordinates
 *  3) mip mapping is not enabled (use magnification filter only)
 *)
  D3DPTEXTURECAPS_NONPOW2CONDITIONAL  = $00000100;

// 0x00000200L unused

(*
 * Device can divide transformed texture coordinates by the
 * COUNTth texture coordinate (can do D3DTTFF_PROJECTED)
 *)
  D3DPTEXTURECAPS_PROJECTED  = $00000400;

(*
 * Device can do cubemap textures
 *)
  D3DPTEXTURECAPS_CUBEMAP           = $00000800;

  D3DPTEXTURECAPS_COLORKEYBLEND     = $00001000;


(* TD3DPrimCaps dwTextureFilterCaps *)

  D3DPTFILTERCAPS_NEAREST         = $00000001;
  D3DPTFILTERCAPS_LINEAR          = $00000002;
  D3DPTFILTERCAPS_MIPNEAREST      = $00000004;
  D3DPTFILTERCAPS_MIPLINEAR       = $00000008;
  D3DPTFILTERCAPS_LINEARMIPNEAREST = $00000010;
  D3DPTFILTERCAPS_LINEARMIPLINEAR = $00000020;

(* Device3 Min Filter *)
  D3DPTFILTERCAPS_MINFPOINT       = $00000100;
  D3DPTFILTERCAPS_MINFLINEAR      = $00000200;
  D3DPTFILTERCAPS_MINFANISOTROPIC = $00000400;

(* Device3 Mip Filter *)
  D3DPTFILTERCAPS_MIPFPOINT       = $00010000;
  D3DPTFILTERCAPS_MIPFLINEAR      = $00020000;

(* Device3 Mag Filter *)
  D3DPTFILTERCAPS_MAGFPOINT         = $01000000;
  D3DPTFILTERCAPS_MAGFLINEAR        = $02000000;
  D3DPTFILTERCAPS_MAGFANISOTROPIC   = $04000000;
  D3DPTFILTERCAPS_MAGFAFLATCUBIC    = $08000000;
  D3DPTFILTERCAPS_MAGFGAUSSIANCUBIC = $10000000;

(* TD3DPrimCaps dwTextureBlendCaps *)

  D3DPTBLENDCAPS_DECAL            = $00000001;
  D3DPTBLENDCAPS_MODULATE         = $00000002;
  D3DPTBLENDCAPS_DECALALPHA       = $00000004;
  D3DPTBLENDCAPS_MODULATEALPHA    = $00000008;
  D3DPTBLENDCAPS_DECALMASK        = $00000010;
  D3DPTBLENDCAPS_MODULATEMASK     = $00000020;
  D3DPTBLENDCAPS_COPY             = $00000040;
  D3DPTBLENDCAPS_ADD	        	  = $00000080;

(* TD3DPrimCaps dwTextureAddressCaps *)
  D3DPTADDRESSCAPS_WRAP           = $00000001;
  D3DPTADDRESSCAPS_MIRROR         = $00000002;
  D3DPTADDRESSCAPS_CLAMP          = $00000004;
  D3DPTADDRESSCAPS_BORDER         = $00000008;
  D3DPTADDRESSCAPS_INDEPENDENTUV  = $00000010;

(* D3DDEVICEDESC dwStencilCaps *)

  D3DSTENCILCAPS_KEEP     = $00000001;
  D3DSTENCILCAPS_ZERO     = $00000002;
  D3DSTENCILCAPS_REPLACE  = $00000004;
  D3DSTENCILCAPS_INCRSAT  = $00000008;
  D3DSTENCILCAPS_DECRSAT  = $00000010;
  D3DSTENCILCAPS_INVERT   = $00000020;
  D3DSTENCILCAPS_INCR     = $00000040;
  D3DSTENCILCAPS_DECR     = $00000080;

(* D3DDEVICEDESC dwTextureOpCaps *)

  D3DTEXOPCAPS_DISABLE                    = $00000001;
  D3DTEXOPCAPS_SELECTARG1                 = $00000002;
  D3DTEXOPCAPS_SELECTARG2                 = $00000004;
  D3DTEXOPCAPS_MODULATE                   = $00000008;
  D3DTEXOPCAPS_MODULATE2X                 = $00000010;
  D3DTEXOPCAPS_MODULATE4X                 = $00000020;
  D3DTEXOPCAPS_ADD                        = $00000040;
  D3DTEXOPCAPS_ADDSIGNED                  = $00000080;
  D3DTEXOPCAPS_ADDSIGNED2X                = $00000100;
  D3DTEXOPCAPS_SUBTRACT                   = $00000200;
  D3DTEXOPCAPS_ADDSMOOTH                  = $00000400;
  D3DTEXOPCAPS_BLENDDIFFUSEALPHA          = $00000800;
  D3DTEXOPCAPS_BLENDTEXTUREALPHA          = $00001000;
  D3DTEXOPCAPS_BLENDFACTORALPHA           = $00002000;
  D3DTEXOPCAPS_BLENDTEXTUREALPHAPM        = $00004000;
  D3DTEXOPCAPS_BLENDCURRENTALPHA          = $00008000;
  D3DTEXOPCAPS_PREMODULATE                = $00010000;
  D3DTEXOPCAPS_MODULATEALPHA_ADDCOLOR     = $00020000;
  D3DTEXOPCAPS_MODULATECOLOR_ADDALPHA     = $00040000;
  D3DTEXOPCAPS_MODULATEINVALPHA_ADDCOLOR  = $00080000;
  D3DTEXOPCAPS_MODULATEINVCOLOR_ADDALPHA  = $00100000;
  D3DTEXOPCAPS_BUMPENVMAP                 = $00200000;
  D3DTEXOPCAPS_BUMPENVMAPLUMINANCE        = $00400000;
  D3DTEXOPCAPS_DOTPRODUCT3                = $00800000;

(* D3DDEVICEDESC dwFVFCaps flags *)

  D3DFVFCAPS_TEXCOORDCOUNTMASK    = $0000ffff; (* mask for texture coordinate count field *)
  D3DFVFCAPS_DONOTSTRIPELEMENTS   = $00080000; (* Device prefers that vertex elements not be stripped *)

(*
 * Description for a device.
 * This is used to describe a device that is to be created or to query
 * the current device.
 *)

type
  PD3DDeviceDesc = ^TD3DDeviceDesc;
  TD3DDeviceDesc = packed record
    dwSize: DWORD;                       (* Size of TD3DDeviceDesc structure *)
    dwFlags: DWORD;                      (* Indicates which fields have valid data *)
    dcmColorModel: TD3DColorModel;        (* Color model of device *)
    dwDevCaps: DWORD;                    (* Capabilities of device *)
    dtcTransformCaps: TD3DTransformCaps;  (* Capabilities of transform *)
    bClipping: BOOL;                     (* Device can do 3D clipping *)
    dlcLightingCaps: TD3DLightingCaps;    (* Capabilities of lighting *)
    dpcLineCaps: TD3DPrimCaps;
    dpcTriCaps: TD3DPrimCaps;
    dwDeviceRenderBitDepth: DWORD;       (* One of DDBB_8, 16, etc.. *)
    dwDeviceZBufferBitDepth: DWORD;      (* One of DDBD_16, 32, etc.. *)
    dwMaxBufferSize: DWORD;              (* Maximum execute buffer size *)
    dwMaxVertexCount: DWORD;             (* Maximum vertex count *)
    // *** New fields for DX5 *** //

    // Width and height caps are 0 for legacy HALs.
    dwMinTextureWidth, dwMinTextureHeight  : DWORD;
    dwMaxTextureWidth, dwMaxTextureHeight  : DWORD;
    dwMinStippleWidth, dwMaxStippleWidth   : DWORD;
    dwMinStippleHeight, dwMaxStippleHeight : DWORD;

    // New fields for DX6
    dwMaxTextureRepeat : DWORD;
    dwMaxTextureAspectRatio : DWORD;
    dwMaxAnisotropy : DWORD;

    // Guard band that the rasterizer can accommodate
    // Screen-space vertices inside this space but outside the viewport
    // will get clipped properly.
    dvGuardBandLeft : TD3DValue;
    dvGuardBandTop : TD3DValue;
    dvGuardBandRight : TD3DValue;
    dvGuardBandBottom : TD3DValue;

    dvExtentsAdjust : TD3DValue;
    dwStencilCaps : DWORD;

    dwFVFCaps : DWORD;  (* low 4 bits: 0 implies TLVERTEX only, 1..8 imply FVF aware *)
    dwTextureOpCaps : DWORD;
    wMaxTextureBlendStages : WORD;
    wMaxSimultaneousTextures : WORD;
  end;

  PD3DDeviceDesc7 = ^TD3DDeviceDesc7;
  TD3DDeviceDesc7 = packed record
    dwDevCaps:               DWORD;             (* Capabilities of device *)
    dpcLineCaps:             TD3DPrimCaps;
    dpcTriCaps:              TD3DPrimCaps;
    dwDeviceRenderBitDepth:  DWORD;             (* One of DDBB_8, 16, etc.. *)
    dwDeviceZBufferBitDepth: DWORD;             (* One of DDBD_16, 32, etc.. *)

    dwMinTextureWidth, dwMinTextureHeight: DWORD;
    dwMaxTextureWidth, dwMaxTextureHeight: DWORD;

    dwMaxTextureRepeat:                    DWORD;
    dwMaxTextureAspectRatio:               DWORD;
    dwMaxAnisotropy:                       DWORD;

    dvGuardBandLeft:                       TD3DValue;
    dvGuardBandTop:                        TD3DValue;
    dvGuardBandRight:                      TD3DValue;
    dvGuardBandBottom:                     TD3DValue;

    dvExtentsAdjust:                       TD3DValue;
    dwStencilCaps:                         DWORD;

    dwFVFCaps:                             DWORD;
    dwTextureOpCaps:                       DWORD;
    wMaxTextureBlendStages:                WORD;
    wMaxSimultaneousTextures:              WORD;

    dwMaxActiveLights:                     DWORD;
    dvMaxVertexW:                          TD3DValue;
    deviceGUID:                            TGUID;

    wMaxUserClipPlanes:                    WORD;
    wMaxVertexBlendMatrices:               WORD;

    dwVertexProcessingCaps:                DWORD;

    dwReserved1:                           DWORD;
    dwReserved2:                           DWORD;
    dwReserved3:                           DWORD;
    dwReserved4:                           DWORD;
  end;

const
  D3DDEVICEDESCSIZE = sizeof(TD3DDeviceDesc);
  D3DDEVICEDESC7SIZE = sizeof(TD3DDeviceDesc7);

type
  TD3DEnumDevicesCallbackA = function (lpGuid: PGUID; // nil for the default device
      lpDeviceDescription: PAnsiChar; lpDeviceName: PAnsiChar;
      var lpD3DHWDeviceDesc: TD3DDeviceDesc;
      var lpD3DHELDeviceDesc: TD3DDeviceDesc;
      lpContext : pointer) : HResult; stdcall;
  TD3DEnumDevicesCallback = TD3DEnumDevicesCallbackA;

  TD3DEnumDevicesCallback7A = function (
      lpDeviceDescription: PAnsiChar; lpDeviceName: PAnsiChar;
      const lpD3DDeviceDesc: TD3DDeviceDesc7; lpContext: Pointer) : HResult; stdcall;
  TD3DEnumDevicesCallback7 = TD3DEnumDevicesCallback7A;

(* TD3DDeviceDesc dwFlags indicating valid fields *)

const
  D3DDD_COLORMODEL            = $00000001; (* dcmColorModel is valid *)
  D3DDD_DEVCAPS               = $00000002; (* dwDevCaps is valid *)
  D3DDD_TRANSFORMCAPS         = $00000004; (* dtcTransformCaps is valid *)
  D3DDD_LIGHTINGCAPS          = $00000008; (* dlcLightingCaps is valid *)
  D3DDD_BCLIPPING             = $00000010; (* bClipping is valid *)
  D3DDD_LINECAPS              = $00000020; (* dpcLineCaps is valid *)
  D3DDD_TRICAPS               = $00000040; (* dpcTriCaps is valid *)
  D3DDD_DEVICERENDERBITDEPTH  = $00000080; (* dwDeviceRenderBitDepth is valid *)
  D3DDD_DEVICEZBUFFERBITDEPTH = $00000100; (* dwDeviceZBufferBitDepth is valid *)
  D3DDD_MAXBUFFERSIZE         = $00000200; (* dwMaxBufferSize is valid *)
  D3DDD_MAXVERTEXCOUNT        = $00000400; (* dwMaxVertexCount is valid *)

(* TD3DDeviceDesc dwDevCaps flags *)

  D3DDEVCAPS_FLOATTLVERTEX        = $00000001; (* Device accepts floating point *)
                                                    (* for post-transform vertex data *)
  D3DDEVCAPS_SORTINCREASINGZ      = $00000002; (* Device needs data sorted for increasing Z*)
  D3DDEVCAPS_SORTDECREASINGZ      = $00000004; (* Device needs data sorted for decreasing Z*)
  D3DDEVCAPS_SORTEXACT            = $00000008; (* Device needs data sorted exactly *)

  D3DDEVCAPS_EXECUTESYSTEMMEMORY  = $00000010; (* Device can use execute buffers from system memory *)
  D3DDEVCAPS_EXECUTEVIDEOMEMORY   = $00000020; (* Device can use execute buffers from video memory *)
  D3DDEVCAPS_TLVERTEXSYSTEMMEMORY = $00000040; (* Device can use TL buffers from system memory *)
  D3DDEVCAPS_TLVERTEXVIDEOMEMORY  = $00000080; (* Device can use TL buffers from video memory *)
  D3DDEVCAPS_TEXTURESYSTEMMEMORY  = $00000100; (* Device can texture from system memory *)
  D3DDEVCAPS_TEXTUREVIDEOMEMORY   = $00000200; (* Device can texture from device memory *)
  D3DDEVCAPS_DRAWPRIMTLVERTEX     = $00000400; (* Device can draw TLVERTEX primitives *)
  D3DDEVCAPS_CANRENDERAFTERFLIP	  = $00000800; (* Device can render without waiting for flip to complete *)
  D3DDEVCAPS_TEXTURENONLOCALVIDMEM   = $00001000; (* Device can texture from nonlocal video memory *)
  D3DDEVCAPS_DRAWPRIMITIVES2         = $00002000; (* Device can support DrawPrimitives2 *)
  D3DDEVCAPS_SEPARATETEXTUREMEMORIES = $00004000; (* Device is texturing from separate memory pools *)
  D3DDEVCAPS_DRAWPRIMITIVES2EX       = $00008000; (* Device can support Extended DrawPrimitives2 i.e. DX7 compliant driver*)
  D3DDEVCAPS_HWTRANSFORMANDLIGHT     = $00010000; (* Device can support transformation and lighting in hardware and DRAWPRIMITIVES2EX must be also *)
  D3DDEVCAPS_CANBLTSYSTONONLOCAL     = $00020000; (* Device supports a Tex Blt from system memory to non-local vidmem *)
  D3DDEVCAPS_HWRASTERIZATION         = $00080000; (* Device has HW acceleration for rasterization *)

(*
 * These are the flags in the D3DDEVICEDESC7.dwVertexProcessingCaps field
 *)

(* device can do texgen *)
  D3DVTXPCAPS_TEXGEN              = $00000001;
(* device can do IDirect3DDevice7 colormaterialsource ops *)
  D3DVTXPCAPS_MATERIALSOURCE7     = $00000002;
(* device can do vertex fog *)
  D3DVTXPCAPS_VERTEXFOG           = $00000004;
(* device can do directional lights *)
  D3DVTXPCAPS_DIRECTIONALLIGHTS   = $00000008;
(* device can do positional lights (includes point and spot) *)
  D3DVTXPCAPS_POSITIONALLIGHTS    = $00000010;
(* device can do local viewer *)
  D3DVTXPCAPS_LOCALVIEWER         = $00000020;

  D3DFDS_COLORMODEL        = $00000001; (* Match color model *)
  D3DFDS_GUID              = $00000002; (* Match guid *)
  D3DFDS_HARDWARE          = $00000004; (* Match hardware/software *)
  D3DFDS_TRIANGLES         = $00000008; (* Match in triCaps *)
  D3DFDS_LINES             = $00000010; (* Match in lineCaps  *)
  D3DFDS_MISCCAPS          = $00000020; (* Match primCaps.dwMiscCaps *)
  D3DFDS_RASTERCAPS        = $00000040; (* Match primCaps.dwRasterCaps *)
  D3DFDS_ZCMPCAPS          = $00000080; (* Match primCaps.dwZCmpCaps *)
  D3DFDS_ALPHACMPCAPS      = $00000100; (* Match primCaps.dwAlphaCmpCaps *)
  D3DFDS_SRCBLENDCAPS      = $00000200; (* Match primCaps.dwSourceBlendCaps *)
  D3DFDS_DSTBLENDCAPS      = $00000400; (* Match primCaps.dwDestBlendCaps *)
  D3DFDS_SHADECAPS         = $00000800; (* Match primCaps.dwShadeCaps *)
  D3DFDS_TEXTURECAPS       = $00001000; (* Match primCaps.dwTextureCaps *)
  D3DFDS_TEXTUREFILTERCAPS = $00002000; (* Match primCaps.dwTextureFilterCaps *)
  D3DFDS_TEXTUREBLENDCAPS  = $00004000; (* Match primCaps.dwTextureBlendCaps *)
  D3DFDS_TEXTUREADDRESSCAPS  = $00008000; (* Match primCaps.dwTextureBlendCaps *)

(*
 * FindDevice arguments
 *)
type
  PD3DFindDeviceSearch = ^TD3DFindDeviceSearch;
  TD3DFindDeviceSearch = packed record
    dwSize: DWORD;
    dwFlags: DWORD;
    bHardware: BOOL;
    dcmColorModel: TD3DColorModel;
    guid: TGUID;
    dwCaps: DWORD;
    dpcPrimCaps: TD3DPrimCaps;
  end;

  PD3DFindDeviceResult = ^TD3DFindDeviceResult;
  TD3DFindDeviceResult = packed record
    dwSize: DWORD;
    guid: TGUID;               (* guid which matched *)
    ddHwDesc: TD3DDeviceDesc;   (* hardware TD3DDeviceDesc *)
    ddSwDesc: TD3DDeviceDesc;   (* software TD3DDeviceDesc *)
  end;

(*
 * Description of execute buffer.
 *)
  PD3DExecuteBufferDesc = ^TD3DExecuteBufferDesc;
  TD3DExecuteBufferDesc = packed record
    dwSize: DWORD;         (* size of this structure *)
    dwFlags: DWORD;        (* flags indicating which fields are valid *)
    dwCaps: DWORD;         (* capabilities of execute buffer *)
    dwBufferSize: DWORD;   (* size of execute buffer data *)
    lpData: Pointer;       (* pointer to actual data *)
  end;

(* D3DEXECUTEBUFFER dwFlags indicating valid fields *)

const
  D3DDEB_BUFSIZE          = $00000001;     (* buffer size valid *)
  D3DDEB_CAPS             = $00000002;     (* caps valid *)
  D3DDEB_LPDATA           = $00000004;     (* lpData valid *)

(* D3DEXECUTEBUFFER dwCaps *)

  D3DDEBCAPS_SYSTEMMEMORY = $00000001;     (* buffer in system memory *)
  D3DDEBCAPS_VIDEOMEMORY  = $00000002;     (* buffer in device memory *)
  D3DDEBCAPS_MEM          = (D3DDEBCAPS_SYSTEMMEMORY or D3DDEBCAPS_VIDEOMEMORY);

type
  PD3DDevInfo_TextureManager = ^TD3DDevInfo_TextureManager;
  TD3DDevInfo_TextureManager = packed record
    bThrashing:              BOOL;       (* indicates if thrashing *)
    dwApproxBytesDownloaded: DWORD;      (* Approximate number of bytes downloaded by texture manager *)
    dwNumEvicts:             DWORD;      (* number of textures evicted *)
    dwNumVidCreates:         DWORD;      (* number of textures created in video memory *)
    dwNumTexturesUsed:       DWORD;      (* number of textures used *)
    dwNumUsedTexInVid:       DWORD;      (* number of used textures present in video memory *)
    dwWorkingSet:            DWORD;      (* number of textures in video memory *)
    dwWorkingSetBytes:       DWORD;      (* number of bytes in video memory *)
    dwTotalManaged:          DWORD;      (* total number of managed textures *)
    dwTotalBytes:            DWORD;      (* total number of bytes of managed textures *)
    dwLastPri:               DWORD;      (* priority of last texture evicted *)
  end;

  PD3DDevInfo_Texturing = ^TD3DDevInfo_Texturing;
  TD3DDevInfo_Texturing = packed record
    dwNumLoads:          DWORD;          (* counts Load() API calls *)
    dwApproxBytesLoaded: DWORD;          (* Approximate number bytes loaded via Load() *)
    dwNumPreLoads:       DWORD;          (* counts PreLoad() API calls *)
    dwNumSet:            DWORD;          (* counts SetTexture() API calls *)
    dwNumCreates:        DWORD;          (* counts texture creates *)
    dwNumDestroys:       DWORD;          (* counts texture destroys *)
    dwNumSetPriorities:  DWORD;          (* counts SetPriority() API calls *)
    dwNumSetLODs:        DWORD;          (* counts SetLOD() API calls *)
    dwNumLocks:          DWORD;          (* counts number of texture locks *)
    dwNumGetDCs:         DWORD;          (* counts number of GetDCs to textures *)
  end;

(*==========================================================================;
 *
 *
 *  File:   d3d.h
 *  Content:    Direct3D include file
 *
 ****************************************************************************)

function D3DErrorString(Value: HResult) : string;

(*
 * Interface IID's
 *)

const
(*
 * Internal Guid to distinguish requested MMX from MMX being used as an RGB rasterizer
 *)
  IID_IDirect3DRampDevice: TGUID =
      (D1:$F2086B20;D2:$259F;D3:$11CF;D4:($A3,$1A,$00,$AA,$00,$B9,$33,$56));
  IID_IDirect3DRGBDevice: TGUID =
      (D1:$A4665C60;D2:$2673;D3:$11CF;D4:($A3,$1A,$00,$AA,$00,$B9,$33,$56));
  IID_IDirect3DHALDevice: TGUID =
      (D1:$84E63dE0;D2:$46AA;D3:$11CF;D4:($81,$6F,$00,$00,$C0,$20,$15,$6E));
  IID_IDirect3DMMXDevice: TGUID =
      (D1:$881949a1;D2:$d6f3;D3:$11d0;D4:($89,$ab,$00,$a0,$c9,$05,$41,$29));

  IID_IDirect3DRefDevice: TGUID =
      (D1:$50936643;D2:$13e9;D3:$11d1;D4:($89,$aa,$00,$a0,$c9,$05,$41,$29));
  IID_IDirect3DNullDevice: TGUID =
      (D1:$8767df22;D2:$bacc;D3:$11d1;D4:($89,$69,$00,$a0,$c9,$06,$29,$a8));

  IID_IDirect3DTnLHalDevice: TGUID = '{f5049e78-4861-11d2-a407-00a0c90629a8}'; 

type
  IDirect3D = interface;
  IDirect3D2 = interface;
  IDirect3D3 = interface;
  IDirect3D7 = interface;
  IDirect3DDevice = interface;
  IDirect3DDevice2 = interface;
  IDirect3DDevice3 = interface;
  IDirect3DDevice7 = interface;
  IDirect3DExecuteBuffer = interface;
  IDirect3DLight = interface;
  IDirect3DMaterial = interface;
  IDirect3DMaterial2 = interface;
  IDirect3DMaterial3 = interface;
  IDirect3DTexture = interface;
  IDirect3DTexture2 = interface;
  IDirect3DViewport = interface;
  IDirect3DViewport2 = interface;
  IDirect3DViewport3 = interface;
  IDirect3DVertexBuffer = interface;
  IDirect3DVertexBuffer7 = interface;

(*
 * Direct3D interfaces
 *)

  IDirect3D = interface (IUnknown)
    ['{3BBA0080-2421-11CF-A31A-00AA00B93356}']
    (*** IDirect3D methods ***)
    function Initialize (lpREFIID: {REFIID} PGUID) : HResult; stdcall;
    function EnumDevices (lpEnumDevicesCallback: TD3DEnumDevicesCallback;
        lpUserArg: Pointer) : HResult; stdcall;
    function CreateLight (var lplpDirect3Dlight: IDirect3DLight;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function CreateMaterial (var lplpDirect3DMaterial: IDirect3DMaterial;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function CreateViewport (var lplpD3DViewport: IDirect3DViewport;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function FindDevice (var lpD3DFDS: TD3DFindDeviceSearch;
        var lpD3DFDR: TD3DFindDeviceResult) : HResult; stdcall;
  end;

  IDirect3D2 = interface (IUnknown)
    ['{6aae1ec1-662a-11d0-889d-00aa00bbb76a}']
    (*** IDirect3D2 methods ***)
    function EnumDevices(lpEnumDevicesCallback: TD3DEnumDevicesCallback;
        lpUserArg: pointer) : HResult; stdcall;
    function CreateLight (var lplpDirect3Dlight: IDirect3DLight;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function CreateMaterial (var lplpDirect3DMaterial2: IDirect3DMaterial2;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function CreateViewport (var lplpD3DViewport2: IDirect3DViewport2;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function FindDevice (var lpD3DFDS: TD3DFindDeviceSearch;
        var lpD3DFDR: TD3DFindDeviceResult) : HResult; stdcall;
    function CreateDevice (const rclsid: TRefClsID; lpDDS: IDirectDrawSurface;
        out lplpD3DDevice2: IDirect3DDevice2) : HResult; stdcall;
  end;

  IDirect3D3 = interface (IUnknown)
    ['{bb223240-e72b-11d0-a9b4-00aa00c0993e}']
    (*** IDirect3D3 methods ***)
    function EnumDevices(lpEnumDevicesCallback: TD3DEnumDevicesCallback;
        lpUserArg: pointer) : HResult; stdcall;
    function CreateLight (var lplpDirect3Dlight: IDirect3DLight;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function CreateMaterial (var lplpDirect3DMaterial3: IDirect3DMaterial3;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function CreateViewport (var lplpD3DViewport3: IDirect3DViewport3;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function FindDevice (var lpD3DFDS: TD3DFindDeviceSearch;
        var lpD3DFDR: TD3DFindDeviceResult) : HResult; stdcall;
    function CreateDevice (const rclsid: TRefClsID; lpDDS: IDirectDrawSurface4;
        out lplpD3DDevice: IDirect3DDevice3; pUnkOuter: IUnknown) : HResult; stdcall;
    function CreateVertexBuffer (var lpVBDesc: TD3DVertexBufferDesc;
        var lpD3DVertexBuffer: IDirect3DVertexBuffer;
        dwFlags: DWORD; pUnkOuter: IUnknown) : HResult; stdcall;
    function EnumZBufferFormats (const riidDevice: TRefClsID; lpEnumCallback:
        TD3DEnumPixelFormatsCallback; lpContext: pointer) : HResult; stdcall;
    function EvictManagedTextures : HResult; stdcall;
  end;

  IDirect3D7 = interface (IUnknown)
    ['{f5049e77-4861-11d2-a407-00a0c90629a8}']
    (*** IDirect3D7 methods ***)
    function EnumDevices(lpEnumDevicesCallback: TD3DEnumDevicesCallback7;
        lpUserArg: pointer) : HResult; stdcall;
    function CreateDevice (const rclsid: TGUID; lpDDS: IDirectDrawSurface7;
        out lplpD3DDevice: IDirect3DDevice7) : HResult; stdcall;
    function CreateVertexBuffer (const lpVBDesc: TD3DVertexBufferDesc;
        out lplpD3DVertexBuffer: IDirect3DVertexBuffer7;
        dwFlags: DWORD) : HResult; stdcall;
    function EnumZBufferFormats (const riidDevice: TGUID; lpEnumCallback:
        TD3DEnumPixelFormatsCallback; lpContext: pointer) : HResult; stdcall;
    function EvictManagedTextures : HResult; stdcall;
  end;
  
(*
 * Direct3D Device interfaces
 *)

  IDirect3DDevice = interface (IUnknown)
    ['{64108800-957d-11d0-89ab-00a0c9054129}']
    (*** IDirect3DDevice methods ***)
    function Initialize (lpd3d: IDirect3D; lpGUID: PGUID;
        var lpd3ddvdesc: TD3DDeviceDesc) : HResult; stdcall;
    function GetCaps (var lpD3DHWDevDesc: TD3DDeviceDesc;
        var lpD3DHELDevDesc: TD3DDeviceDesc) : HResult; stdcall;
    function SwapTextureHandles (lpD3DTex1: IDirect3DTexture;
        lpD3DTex2: IDirect3DTexture) : HResult; stdcall;
    function CreateExecuteBuffer (var lpDesc: TD3DExecuteBufferDesc ;
        var lplpDirect3DExecuteBuffer: IDirect3DExecuteBuffer;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function GetStats (var lpD3DStats: TD3DStats) : HResult; stdcall;
    function Execute (lpDirect3DExecuteBuffer: IDirect3DExecuteBuffer;
        lpDirect3DViewport: IDirect3DViewport; dwFlags: DWORD) : HResult; stdcall;
    function AddViewport (lpDirect3DViewport: IDirect3DViewport) : HResult; stdcall;
    function DeleteViewport (lpDirect3DViewport: IDirect3DViewport) : HResult; stdcall;
    function NextViewport (lpDirect3DViewport: IDirect3DViewport;
        var lplpDirect3DViewport: IDirect3DViewport; dwFlags: DWORD) : HResult; stdcall;
    function Pick (lpDirect3DExecuteBuffer: IDirect3DExecuteBuffer;
        lpDirect3DViewport: IDirect3DViewport; dwFlags: DWORD;
        var lpRect: TD3DRect) : HResult; stdcall;
    function GetPickRecords (var lpCount: DWORD;
        var lpD3DPickRec: TD3DPickRecord) : HResult; stdcall;
    function EnumTextureFormats (lpd3dEnumTextureProc:
        TD3DEnumTextureFormatsCallback; lpArg: Pointer) :
        HResult; stdcall;
    function CreateMatrix (var lpD3DMatHandle: TD3DMatrixHandle) : HResult; stdcall;
    function SetMatrix (d3dMatHandle: TD3DMatrixHandle;
        var lpD3DMatrix: TD3DMatrix) : HResult; stdcall;
    function GetMatrix (var lpD3DMatHandle: TD3DMatrixHandle;
        var lpD3DMatrix: TD3DMatrix) : HResult; stdcall;
    function DeleteMatrix (d3dMatHandle: TD3DMatrixHandle) : HResult; stdcall;
    function BeginScene: HResult; stdcall;
    function EndScene: HResult; stdcall;
    function GetDirect3D (var lpD3D: IDirect3D) : HResult; stdcall;
  end;

  IDirect3DDevice2 = interface (IUnknown)
    ['{93281501-8cf8-11d0-89ab-00a0c9054129}']
    (*** IDirect3DDevice2 methods ***)
    function GetCaps (var lpD3DHWDevDesc: TD3DDeviceDesc;
        var lpD3DHELDevDesc: TD3DDeviceDesc) : HResult; stdcall;
    function SwapTextureHandles (lpD3DTex1: IDirect3DTexture2;
        lpD3DTex2: IDirect3DTexture2) : HResult; stdcall;
    function GetStats (var lpD3DStats: TD3DStats) : HResult; stdcall;
    function AddViewport (lpDirect3DViewport2: IDirect3DViewport2) : HResult; stdcall;
    function DeleteViewport (lpDirect3DViewport: IDirect3DViewport2) : HResult; stdcall;
    function NextViewport (lpDirect3DViewport: IDirect3DViewport2;
        var lplpDirect3DViewport: IDirect3DViewport2; dwFlags: DWORD) :
        HResult; stdcall;
    function EnumTextureFormats (
        lpd3dEnumTextureProc: TD3DEnumTextureFormatsCallback; lpArg: Pointer) :
        HResult; stdcall;
    function BeginScene: HResult; stdcall;
    function EndScene: HResult; stdcall;
    function GetDirect3D (var lpD3D: IDirect3D2) : HResult; stdcall;

    (*** DrawPrimitive API ***)
    function SetCurrentViewport (lpd3dViewport2: IDirect3DViewport2)
        : HResult; stdcall;
    function GetCurrentViewport (var lplpd3dViewport2: IDirect3DViewport2)
        : HResult; stdcall;

    function SetRenderTarget (lpNewRenderTarget: IDirectDrawSurface)
        : HResult; stdcall;
    function GetRenderTarget (var lplpNewRenderTarget: IDirectDrawSurface)
        : HResult; stdcall;

    function Begin_ (d3dpt: TD3DPrimitiveType; d3dvt: TD3DVertexType;
        dwFlags: DWORD) : HResult; stdcall;
    function BeginIndexed (dptPrimitiveType: TD3DPrimitiveType; dvtVertexType:
        TD3DVertexType; lpvVertices: pointer; dwNumVertices: DWORD;
        dwFlags: DWORD) : HResult; stdcall;
    function Vertex (lpVertexType: pointer) : HResult;  stdcall;
    function Index (wVertexIndex: WORD) : HResult;  stdcall;
    function End_ (dwFlags: DWORD) : HResult; stdcall;

    function GetRenderState (dwRenderStateType: TD3DRenderStateType;
        var lpdwRenderState) : HResult; stdcall;
    function SetRenderState (dwRenderStateType: TD3DRenderStateType;
        dwRenderState: DWORD) : HResult; stdcall;
    function GetLightState (dwLightStateType: TD3DLightStateType;
        var lpdwLightState) : HResult; stdcall;
    function SetLightState (dwLightStateType: TD3DLightStateType;
        dwLightState: DWORD) : HResult; stdcall;
    function SetTransform (dtstTransformStateType: TD3DTransformStateType;
        var lpD3DMatrix: TD3DMatrix) : HResult; stdcall;
    function GetTransform (dtstTransformStateType: TD3DTransformStateType;
        var lpD3DMatrix: TD3DMatrix) : HResult; stdcall;
    function MultiplyTransform (dtstTransformStateType: TD3DTransformStateType;
        var lpD3DMatrix: TD3DMatrix) : HResult; stdcall;

    function DrawPrimitive (dptPrimitiveType: TD3DPrimitiveType;
        dvtVertexType: TD3DVertexType; var lpvVertices; dwVertexCount,
        dwFlags: DWORD) : HResult; stdcall;
    function DrawIndexedPrimitive (dptPrimitiveType: TD3DPrimitiveType;
        dwVertexTypeDesc: DWORD; lpvVertices: pointer; dwVertexCount: DWORD;
        var lpwIndices: WORD; dwIndexCount, dwFlags: DWORD) : HResult; stdcall;
    function SetClipStatus (var lpD3DClipStatus: TD3DClipStatus) : HResult; stdcall;
    function GetClipStatus (var lpD3DClipStatus: TD3DClipStatus) : HResult; stdcall;
  end;

  IDirect3DDevice3 = interface (IUnknown)
    ['{b0ab3b60-33d7-11d1-a981-00c04fd7b174}']
    (*** IDirect3DDevice2 methods ***)
    function GetCaps (var lpD3DHWDevDesc: TD3DDeviceDesc;
        var lpD3DHELDevDesc: TD3DDeviceDesc) : HResult; stdcall;
    function GetStats (var lpD3DStats: TD3DStats) : HResult; stdcall;
    function AddViewport (lpDirect3DViewport: IDirect3DViewport3) : HResult; stdcall;
    function DeleteViewport (lpDirect3DViewport: IDirect3DViewport3) : HResult; stdcall;
    function NextViewport (lpDirect3DViewport: IDirect3DViewport3;
        var lplpAnotherViewport: IDirect3DViewport3; dwFlags: DWORD) : HResult; stdcall;
    function EnumTextureFormats (
        lpd3dEnumPixelProc: TD3DEnumPixelFormatsCallback; lpArg: Pointer) :
        HResult; stdcall;
    function BeginScene: HResult; stdcall;
    function EndScene: HResult; stdcall;
    function GetDirect3D (var lpD3D: IDirect3D3) : HResult; stdcall;
    function SetCurrentViewport (lpd3dViewport: IDirect3DViewport3)
        : HResult; stdcall;
    function GetCurrentViewport (var lplpd3dViewport: IDirect3DViewport3)
        : HResult; stdcall;
    function SetRenderTarget (lpNewRenderTarget: IDirectDrawSurface4)
        : HResult; stdcall;
    function GetRenderTarget (var lplpNewRenderTarget: IDirectDrawSurface4)
        : HResult; stdcall;
    function Begin_ (d3dpt: TD3DPrimitiveType; dwVertexTypeDesc: DWORD;
        dwFlags: DWORD) : HResult; stdcall;
    function BeginIndexed (dptPrimitiveType: TD3DPrimitiveType;
        dwVertexTypeDesc: DWORD; lpvVertices: pointer; dwNumVertices: DWORD;
        dwFlags: DWORD) : HResult; stdcall;
    function Vertex (lpVertex: pointer) : HResult;  stdcall;
    function Index (wVertexIndex: WORD) : HResult;  stdcall;
    function End_ (dwFlags: DWORD) : HResult; stdcall;
    function GetRenderState (dwRenderStateType: TD3DRenderStateType;
        var lpdwRenderState) : HResult; stdcall;
    function SetRenderState (dwRenderStateType: TD3DRenderStateType;
        dwRenderState: DWORD) : HResult; stdcall;
    function GetLightState (dwLightStateType: TD3DLightStateType;
        var lpdwLightState) : HResult; stdcall;
    function SetLightState (dwLightStateType: TD3DLightStateType;
        dwLightState: DWORD) : HResult; stdcall;
    function SetTransform (dtstTransformStateType: TD3DTransformStateType;
        var lpD3DMatrix: TD3DMatrix) : HResult; stdcall;
    function GetTransform (dtstTransformStateType: TD3DTransformStateType;
        var lpD3DMatrix: TD3DMatrix) : HResult; stdcall;
    function MultiplyTransform (dtstTransformStateType: TD3DTransformStateType;
        var lpD3DMatrix: TD3DMatrix) : HResult; stdcall;
    function DrawPrimitive (dptPrimitiveType: TD3DPrimitiveType;
        dwVertexTypeDesc: DWORD; const lpvVertices;
        dwVertexCount, dwFlags: DWORD) : HResult; stdcall;
    function DrawIndexedPrimitive (dptPrimitiveType: TD3DPrimitiveType;
        dwVertexTypeDesc: DWORD; const lpvVertices; dwVertexCount: DWORD;
        var lpwIndices: WORD; dwIndexCount, dwFlags: DWORD) : HResult; stdcall;
    function SetClipStatus (var lpD3DClipStatus: TD3DClipStatus) : HResult; stdcall;
    function GetClipStatus (var lpD3DClipStatus: TD3DClipStatus) : HResult; stdcall;
    function DrawPrimitiveStrided (dptPrimitiveType: TD3DPrimitiveType;
        dwVertexTypeDesc : DWORD;
        var lpVertexArray: TD3DDrawPrimitiveStridedData;
        dwVertexCount, dwFlags: DWORD) : HResult; stdcall;
    function DrawIndexedPrimitiveStrided (dptPrimitiveType: TD3DPrimitiveType;
        dwVertexTypeDesc : DWORD;
        var lpVertexArray: TD3DDrawPrimitiveStridedData; dwVertexCount: DWORD;
        var lpwIndices: WORD; dwIndexCount, dwFlags: DWORD) : HResult; stdcall;
    function DrawPrimitiveVB (dptPrimitiveType: TD3DPrimitiveType;
        lpd3dVertexBuffer: IDirect3DVertexBuffer;
        dwStartVertex, dwNumVertices, dwFlags: DWORD) : HResult; stdcall;
    function DrawIndexedPrimitiveVB (dptPrimitiveType: TD3DPrimitiveType;
        lpd3dVertexBuffer: IDirect3DVertexBuffer; var lpwIndices: WORD;
        dwIndexCount, dwFlags: DWORD) : HResult; stdcall;
    function ComputeSphereVisibility (var lpCenters: TD3DVector;
        var lpRadii: TD3DValue; dwNumSpheres, dwFlags: DWORD;
        var lpdwReturnValues: DWORD) : HResult; stdcall;
    function GetTexture (dwStage: DWORD; var lplpTexture: IDirect3DTexture2)
        : HResult; stdcall;
    function SetTexture (dwStage: DWORD; lplpTexture: IDirect3DTexture2)
        : HResult; stdcall;
    function GetTextureStageState (dwStage: DWORD;
        dwState: TD3DTextureStageStateType; var lpdwValue: DWORD) : HResult; stdcall;
    function SetTextureStageState (dwStage: DWORD;
        dwState: TD3DTextureStageStateType; lpdwValue: DWORD) : HResult; stdcall;
    function ValidateDevice (var lpdwExtraPasses: DWORD) : HResult; stdcall;
  end;

  IDirect3DDevice7 = interface (IUnknown)
    ['{f5049e79-4861-11d2-a407-00a0c90629a8}']
    (*** IDirect3DDevice7 methods ***)
    function GetCaps(out lpD3DDevDesc: TD3DDeviceDesc7) : HResult; stdcall;
    function EnumTextureFormats(lpd3dEnumPixelProc: TD3DEnumPixelFormatsCallback; lpArg: Pointer) : HResult; stdcall;
    function BeginScene: HResult; stdcall;
    function EndScene: HResult; stdcall;
    function GetDirect3D(out lpD3D: IDirect3D7) : HResult; stdcall;
    function SetRenderTarget(lpNewRenderTarget: IDirectDrawSurface7; dwFlags: DWORD) : HResult; stdcall;
    function GetRenderTarget(out lplpRenderTarget: IDirectDrawSurface7) : HResult; stdcall;
    function Clear(dwCount: DWORD; lpRects: PD3DRect; dwFlags, dwColor: DWORD; dvZ: TD3DValue; dwStencil: DWORD) : HResult; stdcall;
    function SetTransform(dtstTransformStateType: TD3DTransformStateType;
        const lpD3DMatrix: TD3DMatrix) : HResult; stdcall;
    function GetTransform(dtstTransformStateType: TD3DTransformStateType;
        out lpD3DMatrix: TD3DMatrix) : HResult; stdcall;
    function SetViewport(const lpViewport: TD3DViewport7) : HResult; stdcall;
    function MultiplyTransform(dtstTransformStateType: TD3DTransformStateType;
        const lpD3DMatrix: TD3DMatrix) : HResult; stdcall;
    function GetViewport(out lpViewport: TD3DViewport7) : HResult; stdcall;
    function SetMaterial(const lpMaterial: TD3DMaterial7) : HResult; stdcall;
    function GetMaterial(out lpMaterial: TD3DMaterial7) : HResult; stdcall;
    function SetLight(dwLightIndex: DWORD; const lpLight: TD3DLight7) : HResult; stdcall;
    function GetLight(dwLightIndex: DWORD; out lpLight: TD3DLight7) : HResult; stdcall;
    function SetRenderState(dwRenderStateType: TD3DRenderStateType; dwRenderState: DWORD) : HResult; stdcall;
    function GetRenderState(dwRenderStateType: TD3DRenderStateType; out dwRenderState: DWORD) : HResult; stdcall;
    function BeginStateBlock : HResult; stdcall;
    function EndStateBlock(out lpdwBlockHandle: DWORD) : HResult; stdcall;
    function PreLoad(lpddsTexture: IDirectDrawSurface7) : HResult; stdcall;
    function DrawPrimitive(dptPrimitiveType: TD3DPrimitiveType;
        dwVertexTypeDesc: DWORD; const lpvVertices;
        dwVertexCount, dwFlags: DWORD) : HResult; stdcall;
    function DrawIndexedPrimitive(dptPrimitiveType: TD3DPrimitiveType;
        dwVertexTypeDesc: DWORD; const lpvVertices; dwVertexCount: DWORD;
        var lpwIndices: WORD; dwIndexCount, dwFlags: DWORD) : HResult; stdcall;
    function SetClipStatus(const lpD3DClipStatus: TD3DClipStatus) : HResult; stdcall;
    function GetClipStatus(out lpD3DClipStatus: TD3DClipStatus) : HResult; stdcall;
    function DrawPrimitiveStrided(dptPrimitiveType: TD3DPrimitiveType;
        dwVertexTypeDesc : DWORD;
        const lpVertexArray: TD3DDrawPrimitiveStridedData;
        dwVertexCount, dwFlags: DWORD) : HResult; stdcall;
    function DrawIndexedPrimitiveStrided(dptPrimitiveType: TD3DPrimitiveType;
        dwVertexTypeDesc : DWORD;
        const lpVertexArray: TD3DDrawPrimitiveStridedData; dwVertexCount: DWORD;
        var lpwIndices: WORD; dwIndexCount, dwFlags: DWORD) : HResult; stdcall;
    function DrawPrimitiveVB(dptPrimitiveType: TD3DPrimitiveType;
        lpd3dVertexBuffer: IDirect3DVertexBuffer7;
        dwStartVertex, dwNumVertices, dwFlags: DWORD) : HResult; stdcall;
    function DrawIndexedPrimitiveVB(dptPrimitiveType: TD3DPrimitiveType;
        lpd3dVertexBuffer: IDirect3DVertexBuffer7; dwStartVertex, dwNumVertices: DWORD;
        var lpwIndices: WORD; dwIndexCount, dwFlags: DWORD) : HResult; stdcall;
    function ComputeSphereVisibility(const lpCenters: TD3DVector;
        var lpRadii: TD3DValue; dwNumSpheres, dwFlags: DWORD;
        var lpdwReturnValues: DWORD) : HResult; stdcall;
    function GetTexture(dwStage: DWORD; out lplpTexture: IDirectDrawSurface7) : HResult; stdcall;
    function SetTexture(dwStage: DWORD; lpTexture: IDirectDrawSurface7) : HResult; stdcall;
    function GetTextureStageState(dwStage: DWORD;
        dwState: TD3DTextureStageStateType; out lpdwValue: DWORD) : HResult; stdcall;
    function SetTextureStageState(dwStage: DWORD;
        dwState: TD3DTextureStageStateType; lpdwValue: DWORD) : HResult; stdcall;
    function ValidateDevice(out lpdwExtraPasses: DWORD) : HResult; stdcall;
    function ApplyStateBlock(dwBlockHandle: DWORD) : HResult; stdcall;
    function CaptureStateBlock(dwBlockHandle: DWORD) : HResult; stdcall;
    function DeleteStateBlock(dwBlockHandle: DWORD) : HResult; stdcall;
    function CreateStateBlock(d3dsbType: TD3DStateBlockType; out lpdwBlockHandle: DWORD) : HResult; stdcall;
    function Load(lpDestTex: IDirectDrawSurface7; lpDestPoint: PPoint;
        lpSrcTex: IDirectDrawSurface7; lprcSrcRect: PRect; dwFlags: DWORD) : HResult; stdcall;
    function LightEnable(dwLightIndex: DWORD; bEnable: BOOL) : HResult; stdcall;
    function GetLightEnable(dwLightIndex: DWORD; out bEnable: BOOL) : HResult; stdcall;
    function SetClipPlane(dwIndex: DWORD; var pPlaneEquation: TD3DValue) : HResult; stdcall;
    function GetClipPlane(dwIndex: DWORD; out pPlaneEquation: TD3DValue) : HResult; stdcall;
    function GetInfo(dwDevInfoID: DWORD; pDevInfoStruct: Pointer; dwSize: DWORD) : HResult; stdcall;
  end;
  
(*
 * Execute Buffer interface
 *)

  IDirect3DExecuteBuffer = interface (IUnknown)
    ['{4417C145-33AD-11CF-816F-0000C020156E}']
    (*** IDirect3DExecuteBuffer methods ***)
    function Initialize (lpDirect3DDevice: IDirect3DDevice;
        var lpDesc: TD3DExecuteBufferDesc) : HResult; stdcall;
    function Lock (var lpDesc: TD3DExecuteBufferDesc) : HResult; stdcall;
    function Unlock: HResult; stdcall;
    function SetExecuteData (var lpData: TD3DExecuteData) : HResult; stdcall;
    function GetExecuteData (var lpData: TD3DExecuteData) : HResult; stdcall;
    function Validate (var lpdwOffset: DWORD; lpFunc: TD3DValidateCallback;
        lpUserArg: Pointer; dwReserved: DWORD) : HResult; stdcall;
    (*** Warning!  Optimize is defined differently in the header files
         and the online documentation ***)
    function Optimize (dwFlags: DWORD) : HResult; stdcall;
  end;

(*
 * Light interfaces
 *)

  IDirect3DLight = interface (IUnknown)
    ['{4417C142-33AD-11CF-816F-0000C020156E}']
    (*** IDirect3DLight methods ***)
    function Initialize (lpDirect3D: IDirect3D) : HResult; stdcall;
    function SetLight (var lpLight: TD3DLight2) : HResult; stdcall;
    function GetLight (var lpLight: TD3DLight2) : HResult; stdcall;
  end;

(*
 * Material interfaces
 *)

  IDirect3DMaterial = interface (IUnknown)
    ['{4417C144-33AD-11CF-816F-0000C020156E}']
    (*** IDirect3DMaterial methods ***)
    function Initialize (lpDirect3D: IDirect3D) : HResult; stdcall;
    function SetMaterial (var lpMat: TD3DMaterial) : HResult; stdcall;
    function GetMaterial (var lpMat: TD3DMaterial) : HResult; stdcall;
    function GetHandle (lpDirect3DDevice: IDirect3DDevice;
        var lpHandle: TD3DMaterialHandle) : HResult; stdcall;
    function Reserve: HResult; stdcall;
    function Unreserve: HResult; stdcall;
  end;

  IDirect3DMaterial2 = interface (IUnknown)
    ['{93281503-8cf8-11d0-89ab-00a0c9054129}']
    (*** IDirect3DMaterial2 methods ***)
    function SetMaterial (var lpMat: TD3DMaterial) : HResult; stdcall;
    function GetMaterial (var lpMat: TD3DMaterial) : HResult; stdcall;
    function GetHandle (lpDirect3DDevice: IDirect3DDevice2;
        var lpHandle: TD3DMaterialHandle) : HResult; stdcall;
  end;

  IDirect3DMaterial3 = interface (IUnknown)
    ['{ca9c46f4-d3c5-11d1-b75a-00600852b312}']
    (*** IDirect3DMaterial2 methods ***)
    function SetMaterial (var lpMat: TD3DMaterial) : HResult; stdcall;
    function GetMaterial (var lpMat: TD3DMaterial) : HResult; stdcall;
    function GetHandle (lpDirect3DDevice: IDirect3DDevice3;
        var lpHandle: TD3DMaterialHandle) : HResult; stdcall;
  end;

(*
 * Texture interfaces
 *)

  IDirect3DTexture = interface (IUnknown)
    ['{2CDCD9E0-25A0-11CF-A31A-00AA00B93356}']
    (*** IDirect3DTexture methods ***)
    function Initialize (lpD3DDevice: IDirect3DDevice;
        lpDDSurface: IDirectDrawSurface) : HResult; stdcall;
    function GetHandle (lpDirect3DDevice: IDirect3DDevice;
        var lpHandle: TD3DTextureHandle) : HResult; stdcall;
    function PaletteChanged (dwStart: DWORD; dwCount: DWORD) : HResult; stdcall;
    function Load (lpD3DTexture: IDirect3DTexture) : HResult; stdcall;
    function Unload: HResult; stdcall;
  end;

  IDirect3DTexture2 = interface (IUnknown)
    ['{93281502-8cf8-11d0-89ab-00a0c9054129}']
    (*** IDirect3DTexture2 methods ***)
    function GetHandle (lpDirect3DDevice: IDirect3DDevice2;
        var lpHandle: TD3DTextureHandle) : HResult; stdcall;
    function PaletteChanged (dwStart: DWORD; dwCount: DWORD) : HResult; stdcall;
    function Load (lpD3DTexture: IDirect3DTexture2) : HResult; stdcall;
  end;

(*
 * Viewport interfaces
 *)

  IDirect3DViewport = interface (IUnknown)
    ['{4417C146-33AD-11CF-816F-0000C020156E}']
    (*** IDirect3DViewport methods ***)
    function Initialize (lpDirect3D: IDirect3D) : HResult; stdcall;
    function GetViewport (out lpData: TD3DViewport) : HResult; stdcall;
    function SetViewport (const lpData: TD3DViewport) : HResult; stdcall;
    function TransformVertices (dwVertexCount: DWORD;
        const lpData: TD3DTransformData; dwFlags: DWORD;
        out lpOffscreen: DWORD) : HResult; stdcall;
    function LightElements (dwElementCount: DWORD;
        var lpData: TD3DLightData) : HResult; stdcall;
    function SetBackground (hMat: TD3DMaterialHandle) : HResult; stdcall;
    function GetBackground (out hMat: TD3DMaterialHandle) : HResult; stdcall;
    function SetBackgroundDepth (lpDDSurface: IDirectDrawSurface) :
        HResult; stdcall;
    function GetBackgroundDepth (out lplpDDSurface: IDirectDrawSurface;
        out lpValid: BOOL) : HResult; stdcall;
    function Clear (dwCount: DWORD; const lpRects: TD3DRect; dwFlags: DWORD) :
        HResult; stdcall;
    function AddLight (lpDirect3DLight: IDirect3DLight) : HResult; stdcall;
    function DeleteLight (lpDirect3DLight: IDirect3DLight) : HResult; stdcall;
     function NextLight (lpDirect3DLight: IDirect3DLight;
        out lplpDirect3DLight: IDirect3DLight; dwFlags: DWORD) : HResult; stdcall;
  end;

  IDirect3DViewport2 = interface (IUnknown)
    ['{93281500-8cf8-11d0-89ab-00a0c9054129}']
    (*** IDirect3DViewport2 methods ***)
    function Initialize (lpDirect3D: IDirect3D) : HResult; stdcall;
    function GetViewport (out lpData: TD3DViewport) : HResult; stdcall;
    function SetViewport (const lpData: TD3DViewport) : HResult; stdcall;
    function TransformVertices (dwVertexCount: DWORD;
        const lpData: TD3DTransformData; dwFlags: DWORD;
        out lpOffscreen: DWORD) : HResult; stdcall;
    function LightElements (dwElementCount: DWORD;
        var lpData: TD3DLightData) : HResult; stdcall;
    function SetBackground (hMat: TD3DMaterialHandle) : HResult; stdcall;
    function GetBackground (out hMat: TD3DMaterialHandle) : HResult; stdcall;
    function SetBackgroundDepth (lpDDSurface: IDirectDrawSurface) :
        HResult; stdcall;
    function GetBackgroundDepth (out lplpDDSurface: IDirectDrawSurface;
        out lpValid: BOOL) : HResult; stdcall;
    function Clear (dwCount: DWORD; const lpRects: TD3DRect; dwFlags: DWORD) :
        HResult; stdcall;
    function AddLight (lpDirect3DLight: IDirect3DLight) : HResult; stdcall;
    function DeleteLight (lpDirect3DLight: IDirect3DLight) : HResult; stdcall;
    function NextLight (lpDirect3DLight: IDirect3DLight;
        out lplpDirect3DLight: IDirect3DLight; dwFlags: DWORD) : HResult; stdcall;
    (*** IDirect3DViewport2 methods ***)
    function GetViewport2 (out lpData: TD3DViewport2) : HResult; stdcall;
    function SetViewport2 (const lpData: TD3DViewport2) : HResult; stdcall;
  end;

  IDirect3DViewport3 = interface (IUnknown)
    ['{b0ab3b61-33d7-11d1-a981-00c04fd7b174}']
    (*** IDirect3DViewport3 methods ***)
    function Initialize (lpDirect3D: IDirect3D) : HResult; stdcall;
    function GetViewport (out lpData: TD3DViewport) : HResult; stdcall;
    function SetViewport (const lpData: TD3DViewport) : HResult; stdcall;
    function TransformVertices (dwVertexCount: DWORD;
        const lpData: TD3DTransformData; dwFlags: DWORD;
        out lpOffscreen: DWORD) : HResult; stdcall;
    function LightElements (dwElementCount: DWORD;
        var lpData: TD3DLightData) : HResult; stdcall;
    function SetBackground (hMat: TD3DMaterialHandle) : HResult; stdcall;
    function GetBackground (var hMat: TD3DMaterialHandle) : HResult; stdcall;
    function SetBackgroundDepth (
        lpDDSurface: IDirectDrawSurface) : HResult; stdcall;
    function GetBackgroundDepth (out lplpDDSurface: IDirectDrawSurface;
        out lpValid: BOOL) : HResult; stdcall;
    function Clear (dwCount: DWORD; const lpRects: TD3DRect; dwFlags: DWORD) :
        HResult; stdcall;
    function AddLight (lpDirect3DLight: IDirect3DLight) : HResult; stdcall;
    function DeleteLight (lpDirect3DLight: IDirect3DLight) : HResult; stdcall;
    function NextLight (lpDirect3DLight: IDirect3DLight;
        out lplpDirect3DLight: IDirect3DLight; dwFlags: DWORD) : HResult; stdcall;
    function GetViewport2 (out lpData: TD3DViewport2) : HResult; stdcall;
    function SetViewport2 (const lpData: TD3DViewport2) : HResult; stdcall;
    function SetBackgroundDepth2 (
        lpDDSurface: IDirectDrawSurface4) : HResult; stdcall;
    function GetBackgroundDepth2 (out lplpDDSurface: IDirectDrawSurface4;
        out lpValid: BOOL) : HResult; stdcall;
    function Clear2 (dwCount: DWORD; const lpRects: TD3DRect; dwFlags: DWORD;
        dwColor: DWORD; dvZ: TD3DValue; dwStencil: DWORD) : HResult; stdcall;
  end;

  IDirect3DVertexBuffer = interface (IUnknown)
    ['{7a503555-4a83-11d1-a5db-00a0c90367f8}']
    (*** IDirect3DVertexBuffer methods ***)
    function Lock (dwFlags: DWORD; var lplpData: pointer; var lpdwSize: DWORD)
        : HResult; stdcall;
    function Unlock : HResult; stdcall;
    function ProcessVertices (dwVertexOp, dwDestIndex, dwCount: DWORD;
        lpSrcBuffer: IDirect3DVertexBuffer; dwSrcIndex: DWORD;
        lpD3DDevice: IDirect3DDevice3; dwFlags: DWORD) : HResult; stdcall;
    function GetVertexBufferDesc (var lpVBDesc: TD3DVertexBufferDesc) : HResult; stdcall;
    function Optimize(lpD3DDevice: IDirect3DDevice3; dwFlags: DWORD) : HResult; stdcall;
  end;

  IDirect3DVertexBuffer7 = interface (IUnknown)
    ['{f5049e7d-4861-11d2-a407-00a0c90629a8}']
    (*** IDirect3DVertexBuffer methods ***)
    function Lock (dwFlags: DWORD; out lplpData: Pointer; out lpdwSize: DWORD) : HResult; stdcall;
    function Unlock : HResult; stdcall;
    function ProcessVertices (dwVertexOp, dwDestIndex, dwCount: DWORD;
        lpSrcBuffer: IDirect3DVertexBuffer7; dwSrcIndex: DWORD;
        lpD3DDevice: IDirect3DDevice7; dwFlags: DWORD) : HResult; stdcall;
    function GetVertexBufferDesc (out lpVBDesc: TD3DVertexBufferDesc) : HResult; stdcall;
    function Optimize(lpD3DDevice: IDirect3DDevice7; dwFlags: DWORD) : HResult; stdcall;
    function ProcessVerticesStrided(dwVertexOp, dwDestIndex, dwCount: DWORD;
      lpVertexArray: TD3DDrawPrimitiveStridedData; dwVertexTypeDesc: DWORD;
      lpD3DDevice: IDirect3DDevice7; dwFlags: DWORD) : HResult; stdcall;
  end;

type
  IID_IDirect3D = IDirect3D;
  IID_IDirect3D2 = IDirect3D2;
  IID_IDirect3D3 = IDirect3D3;
  IID_IDirect3D7 = IDirect3D7;

  IID_IDirect3DDevice = IDirect3DDevice;
  IID_IDirect3DDevice2 = IDirect3DDevice2;
  IID_IDirect3DDevice3 = IDirect3DDevice3;
  IID_IDirect3DDevice7 = IDirect3DDevice7;

  IID_IDirect3DTexture = IDirect3DTexture;
  IID_IDirect3DTexture2 = IDirect3DTexture2;
  IID_IDirect3DLight = IDirect3DLight;
  IID_IDirect3DMaterial = IDirect3DMaterial;
  IID_IDirect3DMaterial2 = IDirect3DMaterial2;
  IID_IDirect3DMaterial3 = IDirect3DMaterial3;
  IID_IDirect3DExecuteBuffer = IDirect3DExecuteBuffer;
  IID_IDirect3DViewport = IDirect3DViewport;
  IID_IDirect3DViewport2 = IDirect3DViewport2;
  IID_IDirect3DViewport3 = IDirect3DViewport3;
  IID_IDirect3DVertexBuffer = IDirect3DVertexBuffer;
  IID_IDirect3DVertexBuffer7 = IDirect3DVertexBuffer7;


const
(****************************************************************************
 *
 * Flags for IDirect3DDevice::NextViewport
 *
 ****************************************************************************)

(*
 * Return the next viewport
 *)
  D3DNEXT_NEXT =	$00000001;

(*
 * Return the first viewport
 *)
  D3DNEXT_HEAD =	$00000002;

(*
 * Return the last viewport
 *)
  D3DNEXT_TAIL =	$00000004;


(****************************************************************************
 *
 * Flags for DrawPrimitive/DrawIndexedPrimitive
 *   Also valid for Begin/BeginIndexed
 *   Also valid for VertexBuffer::CreateVertexBuffer
 ****************************************************************************)

(*
 * Wait until the device is ready to draw the primitive
 * This will cause DP to not return DDERR_WASSTILLDRAWING
 *)
  D3DDP_WAIT =					$00000001;

(*
 * Hint that it is acceptable to render the primitive out of order.
 *)
  D3DDP_OUTOFORDER            = $00000002;

(*
 * Hint that the primitives have been clipped by the application.
 *)
  D3DDP_DONOTCLIP =				$00000004;

(*
 * Hint that the extents need not be updated.
 *)
  D3DDP_DONOTUPDATEEXTENTS =	$00000008;

(*
 * Hint that the lighting should not be applied on vertices.
 *)

  D3DDP_DONOTLIGHT            = $00000010;


(*
 * Direct3D Errors
 * DirectDraw error codes are used when errors not specified here.
 *)

const
  MAKE_DDHRESULT = HResult($88760000);

  D3D_OK                          = DD_OK;
  D3DERR_BADMAJORVERSION          = MAKE_DDHRESULT + 700;
  D3DERR_BADMINORVERSION          = MAKE_DDHRESULT + 701;

(*
 * An invalid device was requested by the application.
 *)
  D3DERR_INVALID_DEVICE   = MAKE_DDHRESULT + 705;
  D3DERR_INITFAILED       = MAKE_DDHRESULT + 706;

(*
 * SetRenderTarget attempted on a device that was
 * QI'd off the render target.
 *)
  D3DERR_DEVICEAGGREGATED = MAKE_DDHRESULT + 707;

  D3DERR_EXECUTE_CREATE_FAILED    = MAKE_DDHRESULT + 710;
  D3DERR_EXECUTE_DESTROY_FAILED   = MAKE_DDHRESULT + 711;
  D3DERR_EXECUTE_LOCK_FAILED      = MAKE_DDHRESULT + 712;
  D3DERR_EXECUTE_UNLOCK_FAILED    = MAKE_DDHRESULT + 713;
  D3DERR_EXECUTE_LOCKED           = MAKE_DDHRESULT + 714;
  D3DERR_EXECUTE_NOT_LOCKED       = MAKE_DDHRESULT + 715;

  D3DERR_EXECUTE_FAILED           = MAKE_DDHRESULT + 716;
  D3DERR_EXECUTE_CLIPPED_FAILED   = MAKE_DDHRESULT + 717;

  D3DERR_TEXTURE_NO_SUPPORT       = MAKE_DDHRESULT + 720;
  D3DERR_TEXTURE_CREATE_FAILED    = MAKE_DDHRESULT + 721;
  D3DERR_TEXTURE_DESTROY_FAILED   = MAKE_DDHRESULT + 722;
  D3DERR_TEXTURE_LOCK_FAILED      = MAKE_DDHRESULT + 723;
  D3DERR_TEXTURE_UNLOCK_FAILED    = MAKE_DDHRESULT + 724;
  D3DERR_TEXTURE_LOAD_FAILED      = MAKE_DDHRESULT + 725;
  D3DERR_TEXTURE_SWAP_FAILED      = MAKE_DDHRESULT + 726;
  D3DERR_TEXTURE_LOCKED           = MAKE_DDHRESULT + 727;
  D3DERR_TEXTURE_NOT_LOCKED       = MAKE_DDHRESULT + 728;
  D3DERR_TEXTURE_GETSURF_FAILED   = MAKE_DDHRESULT + 729;

  D3DERR_MATRIX_CREATE_FAILED     = MAKE_DDHRESULT + 730;
  D3DERR_MATRIX_DESTROY_FAILED    = MAKE_DDHRESULT + 731;
  D3DERR_MATRIX_SETDATA_FAILED    = MAKE_DDHRESULT + 732;
  D3DERR_MATRIX_GETDATA_FAILED    = MAKE_DDHRESULT + 733;
  D3DERR_SETVIEWPORTDATA_FAILED   = MAKE_DDHRESULT + 734;

  D3DERR_INVALIDCURRENTVIEWPORT   = MAKE_DDHRESULT + 735;
  D3DERR_INVALIDPRIMITIVETYPE     = MAKE_DDHRESULT + 736;
  D3DERR_INVALIDVERTEXTYPE        = MAKE_DDHRESULT + 737;
  D3DERR_TEXTURE_BADSIZE          = MAKE_DDHRESULT + 738;
  D3DERR_INVALIDRAMPTEXTURE	  = MAKE_DDHRESULT + 739;

  D3DERR_MATERIAL_CREATE_FAILED   = MAKE_DDHRESULT + 740;
  D3DERR_MATERIAL_DESTROY_FAILED  = MAKE_DDHRESULT + 741;
  D3DERR_MATERIAL_SETDATA_FAILED  = MAKE_DDHRESULT + 742;
  D3DERR_MATERIAL_GETDATA_FAILED  = MAKE_DDHRESULT + 743;

  D3DERR_INVALIDPALETTE	          = MAKE_DDHRESULT + 744;

  D3DERR_ZBUFF_NEEDS_SYSTEMMEMORY = MAKE_DDHRESULT + 745;
  D3DERR_ZBUFF_NEEDS_VIDEOMEMORY  = MAKE_DDHRESULT + 746;
  D3DERR_SURFACENOTINVIDMEM       = MAKE_DDHRESULT + 747;

  D3DERR_LIGHT_SET_FAILED         = MAKE_DDHRESULT + 750;
  D3DERR_LIGHTHASVIEWPORT	  = MAKE_DDHRESULT + 751;
  D3DERR_LIGHTNOTINTHISVIEWPORT   = MAKE_DDHRESULT + 752;

  D3DERR_SCENE_IN_SCENE           = MAKE_DDHRESULT + 760;
  D3DERR_SCENE_NOT_IN_SCENE       = MAKE_DDHRESULT + 761;
  D3DERR_SCENE_BEGIN_FAILED       = MAKE_DDHRESULT + 762;
  D3DERR_SCENE_END_FAILED         = MAKE_DDHRESULT + 763;

  D3DERR_INBEGIN                  = MAKE_DDHRESULT + 770;
  D3DERR_NOTINBEGIN               = MAKE_DDHRESULT + 771;
  D3DERR_NOVIEWPORTS              = MAKE_DDHRESULT + 772;
  D3DERR_VIEWPORTDATANOTSET       = MAKE_DDHRESULT + 773;
  D3DERR_VIEWPORTHASNODEVICE      = MAKE_DDHRESULT + 774;
  D3DERR_NOCURRENTVIEWPORT        = MAKE_DDHRESULT + 775;

  D3DERR_INVALIDVERTEXFORMAT      = MAKE_DDHRESULT + 2048;

(*
 * Attempted to CreateTexture on a surface that had a color key
 *)
  D3DERR_COLORKEYATTACHED                 = MAKE_DDHRESULT + 2050;

  D3DERR_VERTEXBUFFEROPTIMIZED            = MAKE_DDHRESULT + 2060;
  D3DERR_VBUF_CREATE_FAILED               = MAKE_DDHRESULT + 2061;
  D3DERR_VERTEXBUFFERLOCKED               = MAKE_DDHRESULT + 2062;

  D3DERR_ZBUFFER_NOTPRESENT               = MAKE_DDHRESULT + 2070;
  D3DERR_STENCILBUFFER_NOTPRESENT         = MAKE_DDHRESULT + 2071;

  D3DERR_WRONGTEXTUREFORMAT               = MAKE_DDHRESULT + 2072;
  D3DERR_UNSUPPORTEDCOLOROPERATION        = MAKE_DDHRESULT + 2073;
  D3DERR_UNSUPPORTEDCOLORARG              = MAKE_DDHRESULT + 2074;
  D3DERR_UNSUPPORTEDALPHAOPERATION        = MAKE_DDHRESULT + 2075;
  D3DERR_UNSUPPORTEDALPHAARG              = MAKE_DDHRESULT + 2076;
  D3DERR_TOOMANYOPERATIONS                = MAKE_DDHRESULT + 2077;
  D3DERR_CONFLICTINGTEXTUREFILTER         = MAKE_DDHRESULT + 2078;
  D3DERR_UNSUPPORTEDFACTORVALUE           = MAKE_DDHRESULT + 2079;
  D3DERR_CONFLICTINGRENDERSTATE           = MAKE_DDHRESULT + 2081;
  D3DERR_UNSUPPORTEDTEXTUREFILTER         = MAKE_DDHRESULT + 2082;
  D3DERR_TOOMANYPRIMITIVES                = MAKE_DDHRESULT + 2083;
  D3DERR_INVALIDMATRIX                    = MAKE_DDHRESULT + 2084;
  D3DERR_TOOMANYVERTICES                  = MAKE_DDHRESULT + 2085;
  D3DERR_CONFLICTINGTEXTUREPALETTE        = MAKE_DDHRESULT + 2086;

  D3DERR_INVALIDSTATEBLOCK        = MAKE_DDHRESULT + 2100;
  D3DERR_INBEGINSTATEBLOCK        = MAKE_DDHRESULT + 2101;
  D3DERR_NOTINBEGINSTATEBLOCK     = MAKE_DDHRESULT + 2102;

procedure DisableFPUExceptions;
procedure EnableFPUExceptions;

(***************************************************************************
 *
 *  Copyright (C) 1998-1999 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dxfile.h
 *
 *  Content:    DirectX File public header file
 *
 ***************************************************************************)

var
  DXFileDLL : HMODULE;

function DXFileErrorString(Value: HResult) : string;

type
  TDXFileFormat = (
    DXFILEFORMAT_BINARY,
    DXFILEFORMAT_TEXT,
    DXFILEFORMAT_COMPRESSED
  );

  TDXFileLoadOptions = (
    DXFILELOAD_FROMFILE,
    DXFILELOAD_FROMRESOURCE,
    DXFILELOAD_FROMMEMORY,
    DXFILELOAD_INVALID_3,
    DXFILELOAD_FROMSTREAM,
    DXFILELOAD_INVALID_5,
    DXFILELOAD_INVALID_6,
    DXFILELOAD_INVALID_7,
    DXFILELOAD_FROMURL
  );

  PDXFileLoadResource = ^TDXFileLoadResource;
  TDXFileLoadResource = packed record
    hModule: HModule;
    lpName: PAnsiChar;
    lpType: PAnsiChar;
  end;

  PDXFileLoadMemory = ^TDXFileLoadMemory;
  TDXFileLoadMemory = packed record
    lpMemory: Pointer;
    dSize: DWORD;
  end;

(*
 * DirectX File object types.
 *)

  IDirectXFile = interface;
  IDirectXFileEnumObject = interface;
  IDirectXFileSaveObject = interface;
  IDirectXFileObject = interface;
  IDirectXFileData = interface;
  IDirectXFileDataReference = interface;
  IDirectXFileBinary = interface;

(*
 * DirectX File interfaces.
 *)

  IDirectXFile = interface (IUnknown)
    ['{3d82ab40-62da-11cf-ab39-0020af71e433}']
    function CreateEnumObject (pvSource: Pointer;
        dwLoadOptions: TDXFileLoadOptions;
        var ppEnumObj: IDirectXFileEnumObject) : HResult; stdcall;
    function CreateSaveObject (szFileName: PChar; dwFileFormat: TDXFileFormat;
        var ppSaveObj: IDirectXFileSaveObject) : HResult; stdcall;
    function RegisterTemplates (pvData: Pointer; cbSize: DWORD) : HResult; stdcall;
  end;

  IDirectXFileEnumObject = interface (IUnknown)
    ['{3d82ab41-62da-11cf-ab39-0020af71e433}']
    function GetNextDataObject (var ppDataObj: IDirectXFileData) : HResult; stdcall;
    function GetDataObjectById
        (const rguid: TGUID; var ppDataObj: IDirectXFileData) : HResult; stdcall;
    function GetDataObjectByName
        (szName: PChar; var ppDataObj: IDirectXFileData) : HResult; stdcall;
  end;

  IDirectXFileSaveObject = interface (IUnknown)
    ['{3d82ab42-62da-11cf-ab39-0020af71e433}']
    function SaveTemplates
        (cTemplates: DWORD; var ppguidTemplates: PGUID) : HResult; stdcall;
    function CreateDataObject (const rguidTemplate: TGUID; szName: PChar;
        pguid: PGUID; cbSize: DWORD; pvData: Pointer;
        var ppDataObj: IDirectXFileData) : HResult; stdcall;
    function SaveData (pDataObj: IDirectXFileData) : HResult; stdcall;
  end;

  IDirectXFileObject = interface (IUnknown)
    ['{3d82ab43-62da-11cf-ab39-0020af71e433}']
    function GetName (pstrNameBuf: PChar; var dwBufLen: DWORD) : HResult; stdcall;
    function GetId (var pGuidBuf: TGUID) : HResult; stdcall;
  end;

  IDirectXFileData = interface (IDirectXFileObject)
    ['{3d82ab44-62da-11cf-ab39-0020af71e433}']
    function GetData
        (szMember: PChar; var pcbSize: DWORD; var ppvData: Pointer) : HResult; stdcall;
    function GetType (var ppguid: PGUID) : HResult; stdcall;
    function GetNextObject (var ppChildObj: IDirectXFileObject) : HResult; stdcall;
    function AddDataObject (pDataObj: IDirectXFileData) : HResult; stdcall;
    function AddDataReference (szRef: PChar; pguidRef: PGUID) : HResult; stdcall;
    function AddBinaryObject (szName: PChar; pguid: PGUID; szMimeType: PChar;
        pvData: Pointer; cbSize: DWORD) : HResult; stdcall;
  end;

  IDirectXFileDataReference = interface (IDirectXFileObject)
    ['{3d82ab45-62da-11cf-ab39-0020af71e433}']
    function Resolve (var ppDataObj: IDirectXFileData) : HResult; stdcall;
  end;

  IDirectXFileBinary = interface (IDirectXFileObject)
    ['{3d82ab46-62da-11cf-ab39-0020af71e433}']
    function GetSize (var pcbSize: DWORD) : HResult; stdcall;
    function GetMimeType (var pszMimeType: PChar) : HResult; stdcall;
    function Read(pvData: Pointer; cbSize: DWORD; pcbRead: PDWORD{?}) : HResult; stdcall;
  end;

const

(*
 * DirectXFile Object Class Id (for CoCreateInstance())
 *)

   CLSID_CDirectXFile: TGUID =
       (D1:$4516ec43;D2:$8f20;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));

(*
 * DirectX File Interface GUIDs.
 *)

type
  IID_IDirectXFile = IDirectXFile;
  IID_IDirectXFileEnumObject = IDirectXFileEnumObject;
  IID_IDirectXFileSaveObject = IDirectXFileSaveObject;
  IID_IDirectXFileObject = IDirectXFileObject;
  IID_IDirectXFileData = IDirectXFileData;
  IID_IDirectXFileDataReference = IDirectXFileDataReference;
  IID_IDirectXFileBinary = IDirectXFileBinary;

(*
 * DirectX File Header template's GUID.
 *)
const
  TID_DXFILEHeader: TGUID =
      (D1:$3d82ab43;D2:$62da;D3:$11cf;D4:($ab,$39,$00,$20,$af,$71,$e4,$33));

(*
 * DirectX File errors.
 *)

const
  DXFILE_OK = 0;

  DXFILEERR_BADOBJECT                 = MAKE_DDHRESULT or 850;
  DXFILEERR_BADVALUE                  = MAKE_DDHRESULT or 851;
  DXFILEERR_BADTYPE                   = MAKE_DDHRESULT or 852;
  DXFILEERR_BADSTREAMHANDLE           = MAKE_DDHRESULT or 853;
  DXFILEERR_BADALLOC                  = MAKE_DDHRESULT or 854;
  DXFILEERR_NOTFOUND                  = MAKE_DDHRESULT or 855;
  DXFILEERR_NOTDONEYET                = MAKE_DDHRESULT or 856;
  DXFILEERR_FILENOTFOUND              = MAKE_DDHRESULT or 857;
  DXFILEERR_RESOURCENOTFOUND          = MAKE_DDHRESULT or 858;
  DXFILEERR_URLNOTFOUND               = MAKE_DDHRESULT or 859;
  DXFILEERR_BADRESOURCE               = MAKE_DDHRESULT or 860;
  DXFILEERR_BADFILETYPE               = MAKE_DDHRESULT or 861;
  DXFILEERR_BADFILEVERSION            = MAKE_DDHRESULT or 862;
  DXFILEERR_BADFILEFLOATSIZE          = MAKE_DDHRESULT or 863;
  DXFILEERR_BADFILECOMPRESSIONTYPE    = MAKE_DDHRESULT or 864;
  DXFILEERR_BADFILE                   = MAKE_DDHRESULT or 865;
  DXFILEERR_PARSEERROR                = MAKE_DDHRESULT or 866;
  DXFILEERR_NOTEMPLATE                = MAKE_DDHRESULT or 867;
  DXFILEERR_BADARRAYSIZE              = MAKE_DDHRESULT or 868;
  DXFILEERR_BADDATAREFERENCE          = MAKE_DDHRESULT or 869;
  DXFILEERR_INTERNALERROR             = MAKE_DDHRESULT or 870;
  DXFILEERR_NOMOREOBJECTS             = MAKE_DDHRESULT or 871;
  DXFILEERR_BADINTRINSICS             = MAKE_DDHRESULT or 872;
  DXFILEERR_NOMORESTREAMHANDLES       = MAKE_DDHRESULT or 873;
  DXFILEERR_NOMOREDATA                = MAKE_DDHRESULT or 874;
  DXFILEERR_BADCACHEFILE              = MAKE_DDHRESULT or 875;
  DXFILEERR_NOINTERNET                = MAKE_DDHRESULT or 876;


(*
 * API for creating IDirectXFile interface.
 *)

var
  DirectXFileCreate : function
    (out lplpDirectXFile: IDirectXFile) : HResult; stdcall;

(* D3DRM XFile templates in binary form *)
const
  D3DRM_XTEMPLATES: array [0..3214] of byte = (
        $78, $6f, $66, $20, $30, $33, $30, $32, $62,
        $69, $6e, $20, $30, $30, $36, $34, $1f, 0, $1,
        0, $6, 0, 0, 0, $48, $65, $61, $64, $65,
        $72, $a, 0, $5, 0, $43, $ab, $82, $3d, $da,
        $62, $cf, $11, $ab, $39, 0, $20, $af, $71, $e4, 
        $33, $28, 0, $1, 0, $5, 0, 0, 0, $6d, 
        $61, $6a, $6f, $72, $14, 0, $28, 0, $1, 0, 
        $5, 0, 0, 0, $6d, $69, $6e, $6f, $72, $14, 
        0, $29, 0, $1, 0, $5, 0, 0, 0, $66, 
        $6c, $61, $67, $73, $14, 0, $b, 0, $1f, 0, 
        $1, 0, $6, 0, 0, 0, $56, $65, $63, $74, 
        $6f, $72, $a, 0, $5, 0, $5e, $ab, $82, $3d, 
        $da, $62, $cf, $11, $ab, $39, 0, $20, $af, $71, 
        $e4, $33, $2a, 0, $1, 0, $1, 0, 0, 0, 
        $78, $14, 0, $2a, 0, $1, 0, $1, 0, 0, 
        0, $79, $14, 0, $2a, 0, $1, 0, $1, 0,
        0, 0, $7a, $14, 0, $b, 0, $1f, 0, $1, 
        0, $8, 0, 0, 0, $43, $6f, $6f, $72, $64, 
        $73, $32, $64, $a, 0, $5, 0, $44, $3f, $f2, 
        $f6, $86, $76, $cf, $11, $8f, $52, 0, $40, $33, 
        $35, $94, $a3, $2a, 0, $1, 0, $1, 0, 0, 
        0, $75, $14, 0, $2a, 0, $1, 0, $1, 0, 
        0, 0, $76, $14, 0, $b, 0, $1f, 0, $1, 
        0, $9, 0, 0, 0, $4d, $61, $74, $72, $69, 
        $78, $34, $78, $34, $a, 0, $5, 0, $45, $3f, 
        $f2, $f6, $86, $76, $cf, $11, $8f, $52, 0, $40, 
        $33, $35, $94, $a3, $34, 0, $2a, 0, $1, 0, 
        $6, 0, 0, 0, $6d, $61, $74, $72, $69, $78, 
        $e, 0, $3, 0, $10, 0, 0, 0, $f, 0, 
        $14, 0, $b, 0, $1f, 0, $1, 0, $9, 0, 
        0, 0, $43, $6f, $6c, $6f, $72, $52, $47, $42, 
        $41, $a, 0, $5, 0, $e0, $44, $ff, $35, $7c, 
        $6c, $cf, $11, $8f, $52, 0, $40, $33, $35, $94, 
        $a3, $2a, 0, $1, 0, $3, 0, 0, 0, $72, 
        $65, $64, $14, 0, $2a, 0, $1, 0, $5, 0, 
        0, 0, $67, $72, $65, $65, $6e, $14, 0, $2a, 
        0, $1, 0, $4, 0, 0, 0, $62, $6c, $75, 
        $65, $14, 0, $2a, 0, $1, 0, $5, 0, 0, 
        0, $61, $6c, $70, $68, $61, $14, 0, $b, 0, 
        $1f, 0, $1, 0, $8, 0, 0, 0, $43, $6f,
        $6c, $6f, $72, $52, $47, $42, $a, 0, $5, 0, 
        $81, $6e, $e1, $d3, $35, $78, $cf, $11, $8f, $52, 
        0, $40, $33, $35, $94, $a3, $2a, 0, $1, 0, 
        $3, 0, 0, 0, $72, $65, $64, $14, 0, $2a,
        0, $1, 0, $5, 0, 0, 0, $67, $72, $65, 
        $65, $6e, $14, 0, $2a, 0, $1, 0, $4, 0, 
        0, 0, $62, $6c, $75, $65, $14, 0, $b, 0, 
        $1f, 0, $1, 0, $c, 0, 0, 0, $49, $6e, 
        $64, $65, $78, $65, $64, $43, $6f, $6c, $6f, $72, 
        $a, 0, $5, 0, $20, $b8, $30, $16, $42, $78, 
        $cf, $11, $8f, $52, 0, $40, $33, $35, $94, $a3, 
        $29, 0, $1, 0, $5, 0, 0, 0, $69, $6e, 
        $64, $65, $78, $14, 0, $1, 0, $9, 0, 0, 
        0, $43, $6f, $6c, $6f, $72, $52, $47, $42, $41, 
        $1, 0, $a, 0, 0, 0, $69, $6e, $64, $65, 
        $78, $43, $6f, $6c, $6f, $72, $14, 0, $b, 0, 
        $1f, 0, $1, 0, $7, 0, 0, 0, $42, $6f, 
        $6f, $6c, $65, $61, $6e, $a, 0, $5, 0, $a0, 
        $a6, $7d, $53, $37, $ca, $d0, $11, $94, $1c, 0, 
        $80, $c8, $c, $fa, $7b, $29, 0, $1, 0, $9, 
        0, 0, 0, $74, $72, $75, $65, $66, $61, $6c, 
        $73, $65, $14, 0, $b, 0, $1f, 0, $1, 0, 
        $9, 0, 0, 0, $42, $6f, $6f, $6c, $65, $61, 
        $6e, $32, $64, $a, 0, $5, 0, $63, $ae, $85, 
        $48, $e8, $78, $cf, $11, $8f, $52, 0, $40, $33, 
        $35, $94, $a3, $1, 0, $7, 0, 0, 0, $42, 
        $6f, $6f, $6c, $65, $61, $6e, $1, 0, $1, 0, 
        0, 0, $75, $14, 0, $1, 0, $7, 0, 0, 
        0, $42, $6f, $6f, $6c, $65, $61, $6e, $1, 0, 
        $1, 0, 0, 0, $76, $14, 0, $b, 0, $1f, 
        0, $1, 0, $c, 0, 0, 0, $4d, $61, $74, 
        $65, $72, $69, $61, $6c, $57, $72, $61, $70, $a, 
        0, $5, 0, $60, $ae, $85, $48, $e8, $78, $cf, 
        $11, $8f, $52, 0, $40, $33, $35, $94, $a3, $1, 
        0, $7, 0, 0, 0, $42, $6f, $6f, $6c, $65, 
        $61, $6e, $1, 0, $1, 0, 0, 0, $75, $14, 
        0, $1, 0, $7, 0, 0, 0, $42, $6f, $6f,
        $6c, $65, $61, $6e, $1, 0, $1, 0, 0, 0, 
        $76, $14, 0, $b, 0, $1f, 0, $1, 0, $f, 
        0, 0, 0, $54, $65, $78, $74, $75, $72, $65,
        $46, $69, $6c, $65, $6e, $61, $6d, $65, $a, 0, 
        $5, 0, $e1, $90, $27, $a4, $10, $78, $cf, $11, 
        $8f, $52, 0, $40, $33, $35, $94, $a3, $31, 0, 
        $1, 0, $8, 0, 0, 0, $66, $69, $6c, $65,
        $6e, $61, $6d, $65, $14, 0, $b, 0, $1f, 0, 
        $1, 0, $8, 0, 0, 0, $4d, $61, $74, $65, 
        $72, $69, $61, $6c, $a, 0, $5, 0, $4d, $ab, 
        $82, $3d, $da, $62, $cf, $11, $ab, $39, 0, $20, 
        $af, $71, $e4, $33, $1, 0, $9, 0, 0, 0, 
        $43, $6f, $6c, $6f, $72, $52, $47, $42, $41, $1, 
        0, $9, 0, 0, 0, $66, $61, $63, $65, $43, 
        $6f, $6c, $6f, $72, $14, 0, $2a, 0, $1, 0, 
        $5, 0, 0, 0, $70, $6f, $77, $65, $72, $14, 
        0, $1, 0, $8, 0, 0, 0, $43, $6f, $6c, 
        $6f, $72, $52, $47, $42, $1, 0, $d, 0, 0, 
        0, $73, $70, $65, $63, $75, $6c, $61, $72, $43, 
        $6f, $6c, $6f, $72, $14, 0, $1, 0, $8, 0, 
        0, 0, $43, $6f, $6c, $6f, $72, $52, $47, $42, 
        $1, 0, $d, 0, 0, 0, $65, $6d, $69, $73, 
        $73, $69, $76, $65, $43, $6f, $6c, $6f, $72, $14, 
        0, $e, 0, $12, 0, $12, 0, $12, 0, $f, 
        0, $b, 0, $1f, 0, $1, 0, $8, 0, 0, 
        0, $4d, $65, $73, $68, $46, $61, $63, $65, $a, 
        0, $5, 0, $5f, $ab, $82, $3d, $da, $62, $cf, 
        $11, $ab, $39, 0, $20, $af, $71, $e4, $33, $29, 
        0, $1, 0, $12, 0, 0, 0, $6e, $46, $61, 
        $63, $65, $56, $65, $72, $74, $65, $78, $49, $6e, 
        $64, $69, $63, $65, $73, $14, 0, $34, 0, $29, 
        0, $1, 0, $11, 0, 0, 0, $66, $61, $63, 
        $65, $56, $65, $72, $74, $65, $78, $49, $6e, $64, 
        $69, $63, $65, $73, $e, 0, $1, 0, $12, 0, 
        0, 0, $6e, $46, $61, $63, $65, $56, $65, $72, 
        $74, $65, $78, $49, $6e, $64, $69, $63, $65, $73, 
        $f, 0, $14, 0, $b, 0, $1f, 0, $1, 0,
        $d, 0, 0, 0, $4d, $65, $73, $68, $46, $61, 
        $63, $65, $57, $72, $61, $70, $73, $a, 0, $5, 
        0, $c0, $c5, $1e, $ed, $a8, $c0, $d0, $11, $94, 
        $1c, 0, $80, $c8, $c, $fa, $7b, $29, 0, $1, 
        0, $f, 0, 0, 0, $6e, $46, $61, $63, $65, 
        $57, $72, $61, $70, $56, $61, $6c, $75, $65, $73,
        $14, 0, $34, 0, $1, 0, $9, 0, 0, 0, 
        $42, $6f, $6f, $6c, $65, $61, $6e, $32, $64, $1, 
        0, $e, 0, 0, 0, $66, $61, $63, $65, $57, 
        $72, $61, $70, $56, $61, $6c, $75, $65, $73, $e,
        0, $1, 0, $f, 0, 0, 0, $6e, $46, $61, 
        $63, $65, $57, $72, $61, $70, $56, $61, $6c, $75, 
        $65, $73, $f, 0, $14, 0, $b, 0, $1f, 0, 
        $1, 0, $11, 0, 0, 0, $4d, $65, $73, $68, 
        $54, $65, $78, $74, $75, $72, $65, $43, $6f, $6f, 
        $72, $64, $73, $a, 0, $5, 0, $40, $3f, $f2, 
        $f6, $86, $76, $cf, $11, $8f, $52, 0, $40, $33, 
        $35, $94, $a3, $29, 0, $1, 0, $e, 0, 0, 
        0, $6e, $54, $65, $78, $74, $75, $72, $65, $43, 
        $6f, $6f, $72, $64, $73, $14, 0, $34, 0, $1, 
        0, $8, 0, 0, 0, $43, $6f, $6f, $72, $64, 
        $73, $32, $64, $1, 0, $d, 0, 0, 0, $74, 
        $65, $78, $74, $75, $72, $65, $43, $6f, $6f, $72, 
        $64, $73, $e, 0, $1, 0, $e, 0, 0, 0, 
        $6e, $54, $65, $78, $74, $75, $72, $65, $43, $6f, 
        $6f, $72, $64, $73, $f, 0, $14, 0, $b, 0, 
        $1f, 0, $1, 0, $10, 0, 0, 0, $4d, $65, 
        $73, $68, $4d, $61, $74, $65, $72, $69, $61, $6c, 
        $4c, $69, $73, $74, $a, 0, $5, 0, $42, $3f, 
        $f2, $f6, $86, $76, $cf, $11, $8f, $52, 0, $40, 
        $33, $35, $94, $a3, $29, 0, $1, 0, $a, 0, 
        0, 0, $6e, $4d, $61, $74, $65, $72, $69, $61, 
        $6c, $73, $14, 0, $29, 0, $1, 0, $c, 0, 
        0, 0, $6e, $46, $61, $63, $65, $49, $6e, $64, 
        $65, $78, $65, $73, $14, 0, $34, 0, $29, 0, 
        $1, 0, $b, 0, 0, 0, $66, $61, $63, $65, 
        $49, $6e, $64, $65, $78, $65, $73, $e, 0, $1,
        0, $c, 0, 0, 0, $6e, $46, $61, $63, $65, 
        $49, $6e, $64, $65, $78, $65, $73, $f, 0, $14, 
        0, $e, 0, $1, 0, $8, 0, 0, 0, $4d, 
        $61, $74, $65, $72, $69, $61, $6c, $f, 0, $b, 
        0, $1f, 0, $1, 0, $b, 0, 0, 0, $4d, 
        $65, $73, $68, $4e, $6f, $72, $6d, $61, $6c, $73, 
        $a, 0, $5, 0, $43, $3f, $f2, $f6, $86, $76, 
        $cf, $11, $8f, $52, 0, $40, $33, $35, $94, $a3, 
        $29, 0, $1, 0, $8, 0, 0, 0, $6e, $4e,
        $6f, $72, $6d, $61, $6c, $73, $14, 0, $34, 0, 
        $1, 0, $6, 0, 0, 0, $56, $65, $63, $74, 
        $6f, $72, $1, 0, $7, 0, 0, 0, $6e, $6f, 
        $72, $6d, $61, $6c, $73, $e, 0, $1, 0, $8,
        0, 0, 0, $6e, $4e, $6f, $72, $6d, $61, $6c, 
        $73, $f, 0, $14, 0, $29, 0, $1, 0, $c, 
        0, 0, 0, $6e, $46, $61, $63, $65, $4e, $6f, 
        $72, $6d, $61, $6c, $73, $14, 0, $34, 0, $1, 
        0, $8, 0, 0, 0, $4d, $65, $73, $68, $46, 
        $61, $63, $65, $1, 0, $b, 0, 0, 0, $66, 
        $61, $63, $65, $4e, $6f, $72, $6d, $61, $6c, $73, 
        $e, 0, $1, 0, $c, 0, 0, 0, $6e, $46, 
        $61, $63, $65, $4e, $6f, $72, $6d, $61, $6c, $73, 
        $f, 0, $14, 0, $b, 0, $1f, 0, $1, 0, 
        $10, 0, 0, 0, $4d, $65, $73, $68, $56, $65, 
        $72, $74, $65, $78, $43, $6f, $6c, $6f, $72, $73, 
        $a, 0, $5, 0, $21, $b8, $30, $16, $42, $78, 
        $cf, $11, $8f, $52, 0, $40, $33, $35, $94, $a3, 
        $29, 0, $1, 0, $d, 0, 0, 0, $6e, $56, 
        $65, $72, $74, $65, $78, $43, $6f, $6c, $6f, $72, 
        $73, $14, 0, $34, 0, $1, 0, $c, 0, 0, 
        0, $49, $6e, $64, $65, $78, $65, $64, $43, $6f, 
        $6c, $6f, $72, $1, 0, $c, 0, 0, 0, $76, 
        $65, $72, $74, $65, $78, $43, $6f, $6c, $6f, $72, 
        $73, $e, 0, $1, 0, $d, 0, 0, 0, $6e, 
        $56, $65, $72, $74, $65, $78, $43, $6f, $6c, $6f, 
        $72, $73, $f, 0, $14, 0, $b, 0, $1f, 0, 
        $1, 0, $4, 0, 0, 0, $4d, $65, $73, $68,
        $a, 0, $5, 0, $44, $ab, $82, $3d, $da, $62, 
        $cf, $11, $ab, $39, 0, $20, $af, $71, $e4, $33, 
        $29, 0, $1, 0, $9, 0, 0, 0, $6e, $56, 
        $65, $72, $74, $69, $63, $65, $73, $14, 0, $34, 
        0, $1, 0, $6, 0, 0, 0, $56, $65, $63, 
        $74, $6f, $72, $1, 0, $8, 0, 0, 0, $76, 
        $65, $72, $74, $69, $63, $65, $73, $e, 0, $1, 
        0, $9, 0, 0, 0, $6e, $56, $65, $72, $74, 
        $69, $63, $65, $73, $f, 0, $14, 0, $29, 0, 
        $1, 0, $6, 0, 0, 0, $6e, $46, $61, $63, 
        $65, $73, $14, 0, $34, 0, $1, 0, $8, 0, 
        0, 0, $4d, $65, $73, $68, $46, $61, $63, $65,
        $1, 0, $5, 0, 0, 0, $66, $61, $63, $65, 
        $73, $e, 0, $1, 0, $6, 0, 0, 0, $6e, 
        $46, $61, $63, $65, $73, $f, 0, $14, 0, $e, 
        0, $12, 0, $12, 0, $12, 0, $f, 0, $b,
        0, $1f, 0, $1, 0, $14, 0, 0, 0, $46, 
        $72, $61, $6d, $65, $54, $72, $61, $6e, $73, $66, 
        $6f, $72, $6d, $4d, $61, $74, $72, $69, $78, $a, 
        0, $5, 0, $41, $3f, $f2, $f6, $86, $76, $cf, 
        $11, $8f, $52, 0, $40, $33, $35, $94, $a3, $1, 
        0, $9, 0, 0, 0, $4d, $61, $74, $72, $69, 
        $78, $34, $78, $34, $1, 0, $b, 0, 0, 0, 
        $66, $72, $61, $6d, $65, $4d, $61, $74, $72, $69, 
        $78, $14, 0, $b, 0, $1f, 0, $1, 0, $5, 
        0, 0, 0, $46, $72, $61, $6d, $65, $a, 0, 
        $5, 0, $46, $ab, $82, $3d, $da, $62, $cf, $11, 
        $ab, $39, 0, $20, $af, $71, $e4, $33, $e, 0, 
        $12, 0, $12, 0, $12, 0, $f, 0, $b, 0, 
        $1f, 0, $1, 0, $9, 0, 0, 0, $46, $6c, 
        $6f, $61, $74, $4b, $65, $79, $73, $a, 0, $5, 
        0, $a9, $46, $dd, $10, $5b, $77, $cf, $11, $8f, 
        $52, 0, $40, $33, $35, $94, $a3, $29, 0, $1, 
        0, $7, 0, 0, 0, $6e, $56, $61, $6c, $75, 
        $65, $73, $14, 0, $34, 0, $2a, 0, $1, 0, 
        $6, 0, 0, 0, $76, $61, $6c, $75, $65, $73, 
        $e, 0, $1, 0, $7, 0, 0, 0, $6e, $56,
        $61, $6c, $75, $65, $73, $f, 0, $14, 0, $b, 
        0, $1f, 0, $1, 0, $e, 0, 0, 0, $54, 
        $69, $6d, $65, $64, $46, $6c, $6f, $61, $74, $4b, 
        $65, $79, $73, $a, 0, $5, 0, $80, $b1, $6, 
        $f4, $3b, $7b, $cf, $11, $8f, $52, 0, $40, $33, 
        $35, $94, $a3, $29, 0, $1, 0, $4, 0, 0, 
        0, $74, $69, $6d, $65, $14, 0, $1, 0, $9, 
        0, 0, 0, $46, $6c, $6f, $61, $74, $4b, $65, 
        $79, $73, $1, 0, $6, 0, 0, 0, $74, $66, 
        $6b, $65, $79, $73, $14, 0, $b, 0, $1f, 0, 
        $1, 0, $c, 0, 0, 0, $41, $6e, $69, $6d, 
        $61, $74, $69, $6f, $6e, $4b, $65, $79, $a, 0, 
        $5, 0, $a8, $46, $dd, $10, $5b, $77, $cf, $11, 
        $8f, $52, 0, $40, $33, $35, $94, $a3, $29, 0, 
        $1, 0, $7, 0, 0, 0, $6b, $65, $79, $54,
        $79, $70, $65, $14, 0, $29, 0, $1, 0, $5, 
        0, 0, 0, $6e, $4b, $65, $79, $73, $14, 0, 
        $34, 0, $1, 0, $e, 0, 0, 0, $54, $69, 
        $6d, $65, $64, $46, $6c, $6f, $61, $74, $4b, $65,
        $79, $73, $1, 0, $4, 0, 0, 0, $6b, $65, 
        $79, $73, $e, 0, $1, 0, $5, 0, 0, 0, 
        $6e, $4b, $65, $79, $73, $f, 0, $14, 0, $b, 
        0, $1f, 0, $1, 0, $10, 0, 0, 0, $41, 
        $6e, $69, $6d, $61, $74, $69, $6f, $6e, $4f, $70, 
        $74, $69, $6f, $6e, $73, $a, 0, $5, 0, $c0, 
        $56, $bf, $e2, $f, $84, $cf, $11, $8f, $52, 0, 
        $40, $33, $35, $94, $a3, $29, 0, $1, 0, $a, 
        0, 0, 0, $6f, $70, $65, $6e, $63, $6c, $6f, 
        $73, $65, $64, $14, 0, $29, 0, $1, 0, $f, 
        0, 0, 0, $70, $6f, $73, $69, $74, $69, $6f, 
        $6e, $71, $75, $61, $6c, $69, $74, $79, $14, 0, 
        $b, 0, $1f, 0, $1, 0, $9, 0, 0, 0, 
        $41, $6e, $69, $6d, $61, $74, $69, $6f, $6e, $a, 
        0, $5, 0, $4f, $ab, $82, $3d, $da, $62, $cf, 
        $11, $ab, $39, 0, $20, $af, $71, $e4, $33, $e, 
        0, $12, 0, $12, 0, $12, 0, $f, 0, $b, 
        0, $1f, 0, $1, 0, $c, 0, 0, 0, $41,
        $6e, $69, $6d, $61, $74, $69, $6f, $6e, $53, $65, 
        $74, $a, 0, $5, 0, $50, $ab, $82, $3d, $da, 
        $62, $cf, $11, $ab, $39, 0, $20, $af, $71, $e4, 
        $33, $e, 0, $1, 0, $9, 0, 0, 0, $41, 
        $6e, $69, $6d, $61, $74, $69, $6f, $6e, $f, 0, 
        $b, 0, $1f, 0, $1, 0, $a, 0, 0, 0, 
        $49, $6e, $6c, $69, $6e, $65, $44, $61, $74, $61, 
        $a, 0, $5, 0, $a0, $ee, $23, $3a, $b1, $94, 
        $d0, $11, $ab, $39, 0, $20, $af, $71, $e4, $33, 
        $e, 0, $1, 0, $6, 0, 0, 0, $42, $49, 
        $4e, $41, $52, $59, $f, 0, $b, 0, $1f, 0, 
        $1, 0, $3, 0, 0, 0, $55, $72, $6c, $a, 
        0, $5, 0, $a1, $ee, $23, $3a, $b1, $94, $d0, 
        $11, $ab, $39, 0, $20, $af, $71, $e4, $33, $29, 
        0, $1, 0, $5, 0, 0, 0, $6e, $55, $72, 
        $6c, $73, $14, 0, $34, 0, $31, 0, $1, 0, 
        $4, 0, 0, 0, $75, $72, $6c, $73, $e, 0, 
        $1, 0, $5, 0, 0, 0, $6e, $55, $72, $6c,
        $73, $f, 0, $14, 0, $b, 0, $1f, 0, $1, 
        0, $f, 0, 0, 0, $50, $72, $6f, $67, $72, 
        $65, $73, $73, $69, $76, $65, $4d, $65, $73, $68, 
        $a, 0, $5, 0, $60, $c3, $63, $8a, $7d, $99,
        $d0, $11, $94, $1c, 0, $80, $c8, $c, $fa, $7b, 
        $e, 0, $1, 0, $3, 0, 0, 0, $55, $72, 
        $6c, $13, 0, $1, 0, $a, 0, 0, 0, $49, 
        $6e, $6c, $69, $6e, $65, $44, $61, $74, $61, $f, 
        0, $b, 0, $1f, 0, $1, 0, $4, 0, 0, 
        0, $47, $75, $69, $64, $a, 0, $5, 0, $e0, 
        $90, $27, $a4, $10, $78, $cf, $11, $8f, $52, 0, 
        $40, $33, $35, $94, $a3, $29, 0, $1, 0, $5, 
        0, 0, 0, $64, $61, $74, $61, $31, $14, 0, 
        $28, 0, $1, 0, $5, 0, 0, 0, $64, $61, 
        $74, $61, $32, $14, 0, $28, 0, $1, 0, $5, 
        0, 0, 0, $64, $61, $74, $61, $33, $14, 0, 
        $34, 0, $2d, 0, $1, 0, $5, 0, 0, 0, 
        $64, $61, $74, $61, $34, $e, 0, $3, 0, $8, 
        0, 0, 0, $f, 0, $14, 0, $b, 0, $1f,
        0, $1, 0, $e, 0, 0, 0, $53, $74, $72,
        $69, $6e, $67, $50, $72, $6f, $70, $65, $72, $74,
        $79, $a, 0, $5, 0, $e0, $21, $f, $7f, $e1,
        $bf, $d1, $11, $82, $c0, 0, $a0, $c9, $69, $72,
        $71, $31, 0, $1, 0, $3, 0, 0, 0, $6b,
        $65, $79, $14, 0, $31, 0, $1, 0, $5, 0,
        0, 0, $76, $61, $6c, $75, $65, $14, 0, $b,
        0, $1f, 0, $1, 0, $b, 0, 0, 0, $50,
        $72, $6f, $70, $65, $72, $74, $79, $42, $61, $67,
        $a, 0, $5, 0, $e1, $21, $f, $7f, $e1, $bf,
        $d1, $11, $82, $c0, 0, $a0, $c9, $69, $72, $71,
        $e, 0, $1, 0, $e, 0, 0, 0, $53, $74,
        $72, $69, $6e, $67, $50, $72, $6f, $70, $65, $72,
        $74, $79, $f, 0, $b, 0, $1f, 0, $1, 0,
        $e, 0, 0, 0, $45, $78, $74, $65, $72, $6e,
        $61, $6c, $56, $69, $73, $75, $61, $6c, $a, 0,
        $5, 0, $a0, $6a, $11, $98, $ba, $bd, $d1, $11,
        $82, $c0, 0, $a0, $c9, $69, $72, $71, $1, 0,
        $4, 0, 0, 0, $47, $75, $69, $64, $1, 0,
        $12, 0, 0, 0, $67, $75, $69, $64, $45, $78,
        $74, $65, $72, $6e, $61, $6c, $56, $69, $73, $75,
        $61, $6c, $14, 0, $e, 0, $12, 0, $12, 0,
        $12, 0, $f, 0, $b, 0);


implementation

uses
  DXCommon;

function DXFileErrorString(Value: HResult) : string;
begin
  case Value of
    DXFILE_OK: Result := 'Command completed successfully. Equivalent to DD_OK.';
    DXFILEERR_BADVALUE: Result := 'Parameter is invalid.';
    DXFILEERR_BADTYPE: Result := 'Object type is invalid.';
    DXFILEERR_BADALLOC: Result := 'Memory allocation failed.';
    DXFILEERR_NOTFOUND: Result := 'Object could not be found.';
    DXFILEERR_FILENOTFOUND: Result := 'File could not be found.';
    DXFILEERR_RESOURCENOTFOUND: Result := 'Resource could not be found.';
    DXFILEERR_URLNOTFOUND: Result := 'URL could not be found.';
    DXFILEERR_BADRESOURCE: Result := 'Resource is invalid.';
    DXFILEERR_BADFILETYPE: Result := 'File is not a DirectX file.';
    DXFILEERR_BADFILEVERSION: Result := 'File version is not valid.';
    DXFILEERR_BADFILEFLOATSIZE: Result := 'Floating-point size is invalid.';
    DXFILEERR_BADFILE: Result := 'File is invalid.';
    DXFILEERR_PARSEERROR: Result := 'File could not be parsed.';
    DXFILEERR_BADARRAYSIZE: Result := 'Array size is invalid.';
    DXFILEERR_BADDATAREFERENCE: Result := 'Data reference is invalid.';
    DXFILEERR_NOMOREOBJECTS: Result := 'All objects have been enumerated.';
    DXFILEERR_NOMOREDATA: Result := 'No further data is available.';
    else Result := 'Unrecognized Error';
  end;
end;

function D3DFVF_TEXCOORDSIZE3(CoordIndex: DWORD) : DWORD;
begin
  Result := (D3DFVF_TEXTUREFORMAT3 shl (CoordIndex*2 + 16));
end;

function D3DFVF_TEXCOORDSIZE2(CoordIndex: DWORD) : DWORD;
begin
  Result := (D3DFVF_TEXTUREFORMAT2);
end;

function D3DFVF_TEXCOORDSIZE4(CoordIndex: DWORD) : DWORD;
begin
  Result := (D3DFVF_TEXTUREFORMAT4 shl (CoordIndex*2 + 16));
end;

function D3DFVF_TEXCOORDSIZE1(CoordIndex: DWORD) : DWORD;
begin
  Result := (D3DFVF_TEXTUREFORMAT1 shl (CoordIndex*2 + 16));
end;


function D3DVal(val: variant) : float;
begin
  Result := val;
end;

function D3DDivide(a,b: double) : float;
begin
  Result := a / b;
end;

function D3DMultiply(a,b: double) : float;
begin
  Result := a * b;
end;

// #define CI_GETALPHA(ci)    ((ci) >> 24)
function CI_GETALPHA(ci: DWORD) : DWORD;
begin
  Result := ci shr 24;
end;

// #define CI_GETINDEX(ci)    (((ci) >> 8) & 0xffff)
function CI_GETINDEX(ci: DWORD) : DWORD;
begin
  Result := (ci shr 8) and $ffff;
end;

// #define CI_GETFRACTION(ci) ((ci) & 0xff)
function CI_GETFRACTION(ci: DWORD) : DWORD;
begin
  Result := ci and $ff;
end;

// #define CI_ROUNDINDEX(ci)  CI_GETINDEX((ci) + 0x80)
function CI_ROUNDINDEX(ci: DWORD) : DWORD;
begin
  Result := CI_GETINDEX(ci + $80);
end;

// #define CI_MASKALPHA(ci)   ((ci) & 0xffffff)
function CI_MASKALPHA(ci: DWORD) : DWORD;
begin
  Result := ci and $ffffff;
end;

// #define CI_MAKE(a, i, f)    (((a) << 24) | ((i) << 8) | (f))
function CI_MAKE(a,i,f: DWORD) : DWORD;
begin
  Result := (a shl 24) or (i shl 8) or f;
end;

// #define RGBA_GETALPHA(rgb)      ((rgb) >> 24)
function RGBA_GETALPHA(rgb: TD3DColor) : DWORD;
begin
  Result := rgb shr 24;
end;

// #define RGBA_GETRED(rgb)        (((rgb) >> 16) & 0xff)
function RGBA_GETRED(rgb: TD3DColor) : DWORD;
begin
  Result := (rgb shr 16) and $ff;
end;

// #define RGBA_GETGREEN(rgb)      (((rgb) >> 8) & 0xff)
function RGBA_GETGREEN(rgb: TD3DColor) : DWORD;
begin
  Result := (rgb shr 8) and $ff;
end;

// #define RGBA_GETBLUE(rgb)       ((rgb) & 0xff)
function RGBA_GETBLUE(rgb: TD3DColor) : DWORD;
begin
  Result := rgb and $ff;
end;

// #define RGBA_MAKE(r, g, b, a)   ((TD3DColor) (((a) << 24) | ((r) << 16) | ((g) << 8) | (b)))
function RGBA_MAKE(r, g, b, a: DWORD) : TD3DColor;
begin
  Result := (a shl 24) or (r shl 16) or (g shl 8) or b;
end;

// #define D3DRGB(r, g, b) \
//     (0xff000000L | (((long)((r) * 255)) << 16) | (((long)((g) * 255)) << 8) | (long)((b) * 255))
function D3DRGB(r, g, b: float) : TD3DColor;
begin
  Result := $ff000000 or (round(r * 255) shl 16)
                      or (round(g * 255) shl 8)
                      or round(b * 255);
end;

// #define D3DRGBA(r, g, b, a) \
//     (  (((long)((a) * 255)) << 24) | (((long)((r) * 255)) << 16) \
//     |   (((long)((g) * 255)) << 8) | (long)((b) * 255) \
//    )
function D3DRGBA(r, g, b, a: float) : TD3DColor;
begin
  Result := (round(a * 255) shl 24) or (round(r * 255) shl 16)
                                    or (round(g * 255) shl 8)
                                    or round(b * 255);
end;

// #define RGB_GETRED(rgb)         (((rgb) >> 16) & 0xff)
function RGB_GETRED(rgb: TD3DColor) : DWORD;
begin
  Result := (rgb shr 16) and $ff;
end;

// #define RGB_GETGREEN(rgb)       (((rgb) >> 8) & 0xff)
function RGB_GETGREEN(rgb: TD3DColor) : DWORD;
begin
  Result := (rgb shr 8) and $ff;
end;

// #define RGB_GETBLUE(rgb)        ((rgb) & 0xff)
function RGB_GETBLUE(rgb: TD3DColor) : DWORD;
begin
  Result := rgb and $ff;
end;

// #define RGBA_SETALPHA(rgba, x) (((x) << 24) | ((rgba) & 0x00ffffff))
function RGBA_SETALPHA(rgba: TD3DColor; x: DWORD) : TD3DColor;
begin
  Result := (x shl 24) or (rgba and $00ffffff);
end;

// #define RGB_MAKE(r, g, b)       ((TD3DColor) (((r) << 16) | ((g) << 8) | (b)))
function RGB_MAKE(r, g, b: DWORD) : TD3DColor;
begin
  Result := (r shl 16) or (g shl 8) or b;
end;

// #define RGBA_TORGB(rgba)       ((TD3DColor) ((rgba) & 0xffffff))
function RGBA_TORGB(rgba: TD3DColor) : TD3DColor;
begin
  Result := rgba and $00ffffff;
end;

// #define RGB_TORGBA(rgb)        ((TD3DColor) ((rgb) | 0xff000000))
function RGB_TORGBA(rgb: TD3DColor) : TD3DColor;
begin
  Result := rgb or $ff000000;
end;


function D3DSTATE_OVERRIDE(StateType: DWORD) : DWORD;
begin
  Result := StateType + D3DSTATE_OVERRIDE_BIAS;
end;

function D3DTRIFLAG_STARTFLAT(len: DWORD) : DWORD;
begin
  if not (len in [1..29]) then len := 0;
  result := len;
end;

// #define D3DRENDERSTATE_STIPPLEPATTERN(y) (D3DRENDERSTATE_STIPPLEPATTERN00 + (y))
function D3DRENDERSTATE_STIPPLEPATTERN(y: integer) : TD3DRenderStateType;
begin
  Result := TD3DRenderStateType(Ord(D3DRENDERSTATE_STIPPLEPATTERN00) + y);
end;




    // Addition and subtraction
function VectorAdd(const v1, v2: TD3DVector) : TD3DVector;
begin
  result.x := v1.x+v2.x;
  result.y := v1.y+v2.y;
  result.z := v1.z+v2.z;
end;

function VectorSub(const v1, v2: TD3DVector) : TD3DVector;
begin
  result.x := v1.x-v2.x;
  result.y := v1.y-v2.y;
  result.z := v1.z-v2.z;
end;

    // Scalar multiplication and division
function VectorMulS(const v: TD3DVector; s: TD3DValue) : TD3DVector;
begin
  result.x := v.x*s;
  result.y := v.y*s;
  result.z := v.z*s;
end;

function VectorDivS(const v: TD3DVector; s: TD3DValue) : TD3DVector;
begin
  result.x := v.x/s;
  result.y := v.y/s;
  result.z := v.z/s;
end;

    // Memberwise multiplication and division
function VectorMul(const v1, v2: TD3DVector) : TD3DVector;
begin
  result.x := v1.x*v2.x;
  result.y := v1.y*v2.y;
  result.z := v1.z*v2.z;
end;

function VectorDiv(const v1, v2: TD3DVector) : TD3DVector;
begin
  result.x := v1.x/v2.x;
  result.y := v1.y/v2.y;
  result.z := v1.z/v2.z;
end;

    // Vector dominance
function VectorSmaller(v1, v2: TD3DVector) : boolean;
begin
  result := (v1.x < v2.x) and (v1.y < v2.y) and (v1.z < v2.z);
end;

function VectorSmallerEquel(v1, v2: TD3DVector) : boolean;
begin
  result := (v1.x <= v2.x) and (v1.y <= v2.y) and (v1.z <= v2.z);
end;

    // Bitwise equality
function VectorEquel(v1, v2: TD3DVector) : boolean;
begin
  result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z);
end;

    // Length-related functions
function VectorSquareMagnitude(v: TD3DVector) : TD3DValue;
begin
  result := (v.x*v.x) + (v.y*v.y) + (v.z*v.z);
end;

function VectorMagnitude(v: TD3DVector) : TD3DValue;
begin
  result := sqrt((v.x*v.x) + (v.y*v.y) + (v.z*v.z));
end;

    // Returns vector with same direction and unit length
function VectorNormalize(const v: TD3DVector) : TD3DVector;
begin
  result := VectorDivS(v,VectorMagnitude(v));
end;

    // Return min/max component of the input vector
function VectorMin(v: TD3DVector) : TD3DValue;
var
  ret : TD3DValue;
begin
  ret := v.x;
  if (v.y < ret) then ret := v.y;
  if (v.z < ret) then ret := v.z;
  result := ret;
end;

function VectorMax(v: TD3DVector) : TD3DValue;
var
  ret : TD3DValue;
begin
  ret := v.x;
  if (ret < v.y) then ret := v.y;
  if (ret < v.z) then ret := v.z;
  result := ret;
end;

    // Return memberwise min/max of input vectors
function VectorMinimize(const v1, v2: TD3DVector) : TD3DVector;
begin
  if v1.x < v2.x then result.x := v1.x else result.x := v2.x;
  if v1.y < v2.y then result.y := v1.y else result.y := v2.y;
  if v1.z < v2.z then result.z := v1.z else result.z := v2.z;
end;

function VectorMaximize(const v1, v2: TD3DVector) : TD3DVector;
begin
  if v1.x > v2.x then result.x := v1.x else result.x := v2.x;
  if v1.y > v2.y then result.y := v1.y else result.y := v2.y;
  if v1.z > v2.z then result.z := v1.z else result.z := v2.z;
end;

    // Dot and cross product
function VectorDotProduct(v1, v2: TD3DVector) : TD3DValue;
begin
  result := (v1.x*v2.x) + (v1.y * v2.y) + (v1.z*v2.z);
end;

function VectorCrossProduct(const v1, v2: TD3DVector) : TD3DVector;
begin
  result.x := (v1.y*v2.z) - (v1.z*v2.y);
  result.y := (v1.z*v2.x) - (v1.x*v2.z);
  result.z := (v1.x*v2.y) - (v1.y*v2.x);
end;

procedure DisableFPUExceptions;
var
  FPUControlWord: WORD;
asm
  FSTCW   FPUControlWord;
  OR      FPUControlWord, $4 + $1; { Divide by zero + invalid operation }
  FLDCW   FPUControlWord;
end;

procedure EnableFPUExceptions;
var
  FPUControlWord: WORD;
asm
  FSTCW   FPUControlWord;
  AND     FPUControlWord, $FFFF - $4 - $1; { Divide by zero + invalid operation }
  FLDCW   FPUControlWord;
end;

function D3DErrorString(Value: HResult) : string; //Full description not available yet
begin
  case Value of
    D3D_OK: Result := 'No error';

    D3DERR_BADMAJORVERSION: Result := 'D3DERR_BADMAJORVERSION';
    D3DERR_BADMINORVERSION: Result := 'D3DERR_BADMINORVERSION';

    D3DERR_INVALID_DEVICE: Result := 'D3DERR_INITFAILED';
    D3DERR_INITFAILED: Result := 'D3DERR_INITFAILED';

    D3DERR_DEVICEAGGREGATED: Result := 'D3DERR_DEVICEAGGREGATED';

    D3DERR_EXECUTE_CREATE_FAILED: Result := 'D3DERR_EXECUTE_CREATE_FAILED';
    D3DERR_EXECUTE_DESTROY_FAILED: Result := 'D3DERR_EXECUTE_DESTROY_FAILED';
    D3DERR_EXECUTE_LOCK_FAILED: Result := 'D3DERR_EXECUTE_LOCK_FAILED';
    D3DERR_EXECUTE_UNLOCK_FAILED: Result := 'D3DERR_EXECUTE_UNLOCK_FAILED';
    D3DERR_EXECUTE_LOCKED: Result := 'D3DERR_EXECUTE_LOCKED';
    D3DERR_EXECUTE_NOT_LOCKED: Result := 'D3DERR_EXECUTE_NOT_LOCKED';

    D3DERR_EXECUTE_FAILED: Result := 'D3DERR_EXECUTE_FAILED';
    D3DERR_EXECUTE_CLIPPED_FAILED: Result := 'D3DERR_EXECUTE_CLIPPED_FAILED';

    D3DERR_TEXTURE_NO_SUPPORT: Result := 'D3DERR_TEXTURE_NO_SUPPORT';
    D3DERR_TEXTURE_CREATE_FAILED: Result := 'D3DERR_TEXTURE_CREATE_FAILED';
    D3DERR_TEXTURE_DESTROY_FAILED: Result := 'D3DERR_TEXTURE_DESTROY_FAILED';
    D3DERR_TEXTURE_LOCK_FAILED: Result := 'D3DERR_TEXTURE_LOCK_FAILED';
    D3DERR_TEXTURE_UNLOCK_FAILED: Result := 'D3DERR_TEXTURE_UNLOCK_FAILED';
    D3DERR_TEXTURE_LOAD_FAILED: Result := 'D3DERR_TEXTURE_LOAD_FAILED';
    D3DERR_TEXTURE_SWAP_FAILED: Result := 'D3DERR_TEXTURE_SWAP_FAILED';
    D3DERR_TEXTURE_LOCKED: Result := 'D3DERR_TEXTURELOCKED';
    D3DERR_TEXTURE_NOT_LOCKED: Result := 'D3DERR_TEXTURE_NOT_LOCKED';
    D3DERR_TEXTURE_GETSURF_FAILED: Result := 'D3DERR_TEXTURE_GETSURF_FAILED';

    D3DERR_MATRIX_CREATE_FAILED: Result := 'D3DERR_MATRIX_CREATE_FAILED';
    D3DERR_MATRIX_DESTROY_FAILED: Result := 'D3DERR_MATRIX_DESTROY_FAILED';
    D3DERR_MATRIX_SETDATA_FAILED: Result := 'D3DERR_MATRIX_SETDATA_FAILED';
    D3DERR_MATRIX_GETDATA_FAILED: Result := 'D3DERR_MATRIX_GETDATA_FAILED';
    D3DERR_SETVIEWPORTDATA_FAILED: Result := 'D3DERR_SETVIEWPORTDATA_FAILED';

    D3DERR_INVALIDCURRENTVIEWPORT: Result := 'D3DERR_INVALIDCURRENTVIEWPORT';
    D3DERR_INVALIDPRIMITIVETYPE: Result := 'D3DERR_INVALIDPRIMITIVETYPE';
    D3DERR_INVALIDVERTEXTYPE: Result := 'D3DERR_INVALIDVERTEXTYPE';
    D3DERR_TEXTURE_BADSIZE: Result := 'D3DERR_TEXTURE_BADSIZE';
    D3DERR_INVALIDRAMPTEXTURE: Result := 'D3DERR_INVALIDRAMPTEXTURE';

    D3DERR_MATERIAL_CREATE_FAILED: Result := 'D3DERR_MATERIAL_CREATE_FAILED';
    D3DERR_MATERIAL_DESTROY_FAILED: Result := 'D3DERR_MATERIAL_DESTROY_FAILED';
    D3DERR_MATERIAL_SETDATA_FAILED: Result := 'D3DERR_MATERIAL_SETDATA_FAILED';
    D3DERR_MATERIAL_GETDATA_FAILED: Result := 'D3DERR_MATERIAL_GETDATA_FAILED';

    D3DERR_INVALIDPALETTE: Result := 'D3DERR_INVALIDPALETTE';

    D3DERR_ZBUFF_NEEDS_SYSTEMMEMORY: Result := 'D3DERR_ZBUFF_NEEDS_SYSTEMMEMORY';
    D3DERR_ZBUFF_NEEDS_VIDEOMEMORY: Result := 'D3DERR_ZBUFF_NEEDS_VIDEOMEMORY';
    D3DERR_SURFACENOTINVIDMEM: Result := 'D3DERR_SURFACENOTINVIDMEM';

    D3DERR_LIGHT_SET_FAILED: Result := 'D3DERR_LIGHT_SET_FAILED';
    D3DERR_LIGHTHASVIEWPORT: Result := 'D3DERR_LIGHTHASVIEWPORT';
    D3DERR_LIGHTNOTINTHISVIEWPORT: Result := 'D3DERR_LIGHTNOTINTHISVIEWPORT';

    D3DERR_SCENE_IN_SCENE: Result := 'D3DERR_SCENE_IN_SCENE';
    D3DERR_SCENE_NOT_IN_SCENE: Result := 'D3DERR_SCENE_NOT_IN_SCENE';
    D3DERR_SCENE_BEGIN_FAILED: Result := 'D3DERR_SCENE_BEGIN_FAILED';
    D3DERR_SCENE_END_FAILED: Result := 'D3DERR_SCENE_END_FAILED';

    D3DERR_INBEGIN: Result := 'D3DERR_INBEGIN';
    D3DERR_NOTINBEGIN: Result := 'D3DERR_NOTINBEGIN';
    D3DERR_NOVIEWPORTS: Result := 'D3DERR_NOVIEWPORTS';
    D3DERR_VIEWPORTDATANOTSET: Result := 'D3DERR_VIEWPORTDATANOTSET';
    D3DERR_VIEWPORTHASNODEVICE: Result := 'D3DERR_VIEWPORTHASNODEVICE';
    D3DERR_NOCURRENTVIEWPORT: Result := 'D3DERR_NOCURRENTVIEWPORT';

    D3DERR_INVALIDVERTEXFORMAT: Result := 'D3DERR_INVALIDVERTEXFORMAT';

    D3DERR_COLORKEYATTACHED: Result := 'D3DERR_COLORKEYATTACHED';

    D3DERR_VERTEXBUFFEROPTIMIZED: Result := 'D3DERR_VERTEXBUFFEROPTIMIZED';
    D3DERR_VBUF_CREATE_FAILED: Result := 'D3DERR_VBUF_CREATE_FAILED';
    D3DERR_VERTEXBUFFERLOCKED: Result := 'D3DERR_VERTEXBUFFERLOCKED';

    D3DERR_ZBUFFER_NOTPRESENT: Result := 'D3DERR_ZBUFFER_NOTPRESENT';
    D3DERR_STENCILBUFFER_NOTPRESENT: Result := 'D3DERR_STENCILBUFFER_NOTPRESENT';

    D3DERR_WRONGTEXTUREFORMAT: Result := 'D3DERR_WRONGTEXTUREFORMAT';
    D3DERR_UNSUPPORTEDCOLOROPERATION: Result := 'D3DERR_UNSUPPORTEDCOLOROPERATION';
    D3DERR_UNSUPPORTEDCOLORARG: Result := 'D3DERR_UNSUPPORTEDCOLORARG';
    D3DERR_UNSUPPORTEDALPHAOPERATION: Result := 'D3DERR_UNSUPPORTEDALPHAOPERATION';
    D3DERR_UNSUPPORTEDALPHAARG: Result := 'D3DERR_UNSUPPORTEDALPHAARG';
    D3DERR_TOOMANYOPERATIONS: Result := 'D3DERR_TOOMANYOPERATIONS';
    D3DERR_CONFLICTINGTEXTUREFILTER: Result := 'D3DERR_CONFLICTINGTEXTUREFILTER';
    D3DERR_UNSUPPORTEDFACTORVALUE: Result := 'D3DERR_UNSUPPORTEDFACTORVALUE';

    D3DERR_CONFLICTINGRENDERSTATE: Result := 'D3DERR_CONFLICTINGRENDERSTATE';
    D3DERR_UNSUPPORTEDTEXTUREFILTER: Result := 'D3DERR_UNSUPPORTEDTEXTUREFILTER';
    D3DERR_TOOMANYPRIMITIVES: Result := 'D3DERR_TOOMANYPRIMITIVES';
    D3DERR_INVALIDMATRIX: Result := 'D3DERR_INVALIDMATRIX';
    D3DERR_TOOMANYVERTICES: Result := 'D3DERR_TOOMANYVERTICES';
    D3DERR_CONFLICTINGTEXTUREPALETTE: Result := 'D3DERR_CONFLICTINGTEXTUREPALETTE';

    else Result := 'Unrecognized Error';
  end;
end;



initialization
begin
  DisableFPUExceptions;
  if not IsNTandDelphiRunning then
  begin
    DXFileDLL := LoadLibrary('D3DXOF.DLL');
    DirectXFileCreate := GetProcAddress(DXFileDLL,'DirectXFileCreate');
  end;
end;

finalization
begin
  FreeLibrary(DXFileDLL);
end;


end.

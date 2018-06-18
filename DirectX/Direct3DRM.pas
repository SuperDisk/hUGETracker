(*==========================================================================;
 *
 *  Copyright (C) 1994-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  Files:	D3DRMDef.h D3DRMObj.h D3DRM.h D3DRMWin.h RMXFGUID.h RMXFTmpl.h
 *  Content:	Direct3D Retained Mode include files
 *
 *  DirectX 7.0 Delphi adaptation by Erik Unger
 *
 *  Modified: 30-Apr-2000
 *
 *  Download: http://www.delphi-jedi.org/DelphiGraphics/
 *  E-Mail: Erik.Unger@gmx.at
 *
 *
 ***************************************************************************)

unit Direct3DRM;

{$MODE Delphi}

interface

{$MINENUMSIZE 4}
{$ALIGN ON}

uses
  Windows,
  DirectDraw,
  Direct3D;

var
  D3DRMDLL : HMODULE = 0;

(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3drmdef.h
 *  Content:    Direct3DRM include file
 *
 ***************************************************************************)

type
  PD3DRMVector4D = ^TD3DRMVector4D;
  TD3DRMVector4D = packed record
    x, y, z, w: TD3DValue;
  end;

  PD3DRMMatrix4D = ^TD3DRMMatrix4D;
  TD3DRMMatrix4D = array [0..3, 0..3] of TD3DValue;

  PD3DRMQuaternion = ^TD3DRMQuaternion;
  TD3DRMQuaternion = packed record
    s: TD3DValue;
    v: TD3DVector;
  end;

  PD3DRMRay = ^TD3DRMRay;
  TD3DRMRay = packed record
    dvDir: TD3DVector;
    dvPos: TD3DVector;
  end;

  PD3DRMBox = ^TD3DRMBox;
  TD3DRMBox = packed record
    min, max: TD3DVector;
  end;

  TD3DRMWrapCallback = procedure (var lpD3DVector: TD3DVector;
      var lpU, lpV: Integer; var lpD3DRMVA, lpD3DRMVB: TD3DVector; lpArg:
      Pointer); stdcall; // unused ?

  PD3DRMLightType = ^TD3DRMLightType; // is it 16 or 32 bit ?
  TD3DRMLightType = (
    D3DRMLIGHT_AMBIENT,
    D3DRMLIGHT_POINT,
    D3DRMLIGHT_SPOT,
    D3DRMLIGHT_DIRECTIONAL,
    D3DRMLIGHT_PARALLELPOINT
  );

  PD3DRMShadeMode = ^TD3DRMShadeMode;
  TD3DRMShadeMode = WORD;

const
  D3DRMSHADE_FLAT = 0;
  D3DRMSHADE_GOURAUD = 1;
  D3DRMSHADE_PHONG = 2;
  D3DRMSHADE_MASK = 7;
  D3DRMSHADE_MAX = 8;

type
  PD3DRMLightMode = ^TD3DRMLightMode;
  TD3DRMLightMode = WORD;

const
  D3DRMLIGHT_OFF  = 0 * D3DRMSHADE_MAX;
  D3DRMLIGHT_ON   = 1 * D3DRMSHADE_MAX;
  D3DRMLIGHT_MASK = 7 * D3DRMSHADE_MAX;
  D3DRMLIGHT_MAX  = 8 * D3DRMSHADE_MAX;

type
  PD3DRMFillMode = ^TD3DRMFillMode;
  TD3DRMFillMode = WORD;

const
  D3DRMFILL_POINTS    = 0 * D3DRMLIGHT_MAX;
  D3DRMFILL_WIREFRAME = 1 * D3DRMLIGHT_MAX;
  D3DRMFILL_SOLID     = 2 * D3DRMLIGHT_MAX;
  D3DRMFILL_MASK      = 7 * D3DRMLIGHT_MAX;
  D3DRMFILL_MAX       = 8 * D3DRMLIGHT_MAX;

type
  PD3DRMRenderQuality = ^TD3DRMRenderQuality;
  TD3DRMRenderQuality = DWORD;

const
  D3DRMRENDER_WIREFRAME   =
      (D3DRMSHADE_FLAT + D3DRMLIGHT_OFF + D3DRMFILL_WIREFRAME);
  D3DRMRENDER_UNLITFLAT   =
      (D3DRMSHADE_FLAT + D3DRMLIGHT_OFF + D3DRMFILL_SOLID);
  D3DRMRENDER_FLAT        =
      (D3DRMSHADE_FLAT + D3DRMLIGHT_ON + D3DRMFILL_SOLID);
  D3DRMRENDER_GOURAUD     =
      (D3DRMSHADE_GOURAUD + D3DRMLIGHT_ON + D3DRMFILL_SOLID);
  D3DRMRENDER_PHONG       =
      (D3DRMSHADE_PHONG + D3DRMLIGHT_ON + D3DRMFILL_SOLID);

  D3DRMRENDERMODE_BLENDEDTRANSPARENCY	=  1;
  D3DRMRENDERMODE_SORTEDTRANSPARENCY	=  2;
  D3DRMRENDERMODE_LIGHTINMODELSPACE     =  8;
  D3DRMRENDERMODE_VIEWDEPENDENTSPECULAR = 16;

type
  PD3DRMTextureQuality = ^TD3DRMTextureQuality;
  TD3DRMTextureQuality = (
    D3DRMTEXTURE_NEAREST,               (* choose nearest texel *)
    D3DRMTEXTURE_LINEAR,                (* interpolate 4 texels *)
    D3DRMTEXTURE_MIPNEAREST,            (* nearest texel in nearest mipmap  *)
    D3DRMTEXTURE_MIPLINEAR,             (* interpolate 2 texels from 2 mipmaps *)
    D3DRMTEXTURE_LINEARMIPNEAREST,      (* interpolate 4 texels in nearest mipmap *)
    D3DRMTEXTURE_LINEARMIPLINEAR        (* interpolate 8 texels from 2 mipmaps *)
  );

const
(*
 * Texture flags
 *)
  D3DRMTEXTURE_FORCERESIDENT          = $00000001; (* texture should be kept in video memory *)
  D3DRMTEXTURE_STATIC                 = $00000002; (* texture will not change *)
  D3DRMTEXTURE_DOWNSAMPLEPOINT        = $00000004; (* point filtering should be used when downsampling *)
  D3DRMTEXTURE_DOWNSAMPLEBILINEAR     = $00000008; (* bilinear filtering should be used when downsampling *)
  D3DRMTEXTURE_DOWNSAMPLEREDUCEDEPTH  = $00000010; (* reduce bit depth when downsampling *)
  D3DRMTEXTURE_DOWNSAMPLENONE         = $00000020; (* texture should never be downsampled *)
  D3DRMTEXTURE_CHANGEDPIXELS          = $00000040; (* pixels have changed *)
  D3DRMTEXTURE_CHANGEDPALETTE         = $00000080; (* palette has changed *)
  D3DRMTEXTURE_INVALIDATEONLY         = $00000100; (* dirty regions are invalid *)

(*
 * Shadow flags
 *)
   D3DRMSHADOW_TRUEALPHA               = $00000001; (* shadow should render without artifacts when true alpha is on *)

type
  PD3DRMCombineType = ^TD3DRMCombineType;
  TD3DRMCombineType = (
    D3DRMCOMBINE_REPLACE,
    D3DRMCOMBINE_BEFORE,
    D3DRMCOMBINE_AFTER
  );

  PD3DRMColorModel = ^TD3DRMColorModel;
  TD3DRMColorModel = TD3DColorModel;

  PD3DRMPaletteFlags = ^TD3DRMPaletteFlags;
  TD3DRMPaletteFlags = (
    D3DRMPALETTE_FREE,                  (* renderer may use this entry freely *)
    D3DRMPALETTE_READONLY,              (* fixed but may be used by renderer *)
    D3DRMPALETTE_RESERVED               (* may not be used by renderer *)
  );

  PD3DRMPaletteEntry = ^TD3DRMPaletteEntry;
  TD3DRMPaletteEntry = packed record
    red: Byte;          (* 0 .. 255 *)
    green: Byte;        (* 0 .. 255 *)
    blue: Byte;         (* 0 .. 255 *)
    flags: Byte;        (* one of D3DRMPALETTEFLAGS *)
  end;

  PD3DRMImage = ^TD3DRMImage;
  TD3DRMImage = packed record
    width, height: Integer;    (* width and height in pixels *)
    aspectx, aspecty: Integer; (* aspect ratio for non-square pixels *)
    depth: Integer;            (* bits per pixel *)
    rgb: Integer;              (* if false, pixels are indices into a
                                   palette otherwise, pixels encode
                                   RGB values. *)
    bytes_per_line: Integer;   (* number of bytes of memory for a
                                   scanline. This must be a multiple
                                   of 4. *)
    buffer1: Pointer;          (* memory to render into (first buffer). *)
    buffer2: Pointer;          (* second rendering buffer for double
                                   buffering, set to NULL for single
                                   buffering. *)
    red_mask: DWORD;
    green_mask: DWORD;
    blue_mask: DWORD;
    alpha_mask: DWORD;        (* if rgb is true, these are masks for
                                   the red, green and blue parts of a
                                   pixel.  Otherwise, these are masks
                                   for the significant bits of the
                                   red, green and blue elements in the
                                   palette.  For instance, most SVGA
                                   displays use 64 intensities of red,
                                   green and blue, so the masks should
                                   all be set to = $fc. *)
    palette_size: Integer;     (* number of entries in palette *)
    palette: PD3DRMPaletteEntry; (* description of the palette (only if
                                   rgb is false).  Must be (1<<depth)
                                   elements. *)
  end;

  PD3DRMWrapType = ^TD3DRMWrapType;
  TD3DRMWrapType = (
    D3DRMWRAP_FLAT,
    D3DRMWRAP_CYLINDER,
    D3DRMWRAP_SPHERE,
    D3DRMWRAP_CHROME,
    D3DRMWRAP_SHEET,
    D3DRMWRAP_BOX
  );

const
  D3DRMWIREFRAME_CULL             = 1; (* cull backfaces *)
  D3DRMWIREFRAME_HIDDENLINE       = 2; (* lines are obscured by closer objects *)

type
(*
 * Do not use righthanded perspective in Viewport2::SetProjection().
 * Set up righthanded mode by using IDirect3DRM3::SetOptions().
 *)
  PD3DRMProjectionType = ^TD3DRMProjectionType;
  TD3DRMProjectionType = (
    D3DRMPROJECT_PERSPECTIVE,
    D3DRMPROJECT_ORTHOGRAPHIC,
    D3DRMPROJECT_RIGHTHANDPERSPECTIVE, (* Only valid pre-DX6 *)
    D3DRMPROJECT_RIGHTHANDORTHOGRAPHIC (* Only valid pre-DX6 *)
  );

const
  D3DRMOPTIONS_LEFTHANDED  = 00000001; (* Default *)
  D3DRMOPTIONS_RIGHTHANDED = 00000002;

type
  PD3DRMXOFFormat = ^TD3DRMXOFFormat;
  TD3DRMXOFFormat = (
    D3DRMXOF_BINARY,
    D3DRMXOF_COMPRESSED,
    D3DRMXOF_TEXT
  );

  TD3DRMSaveOptions = DWORD;
const
  D3DRMXOFSAVE_NORMALS = 1;
  D3DRMXOFSAVE_TEXTURECOORDINATES = 2;
  D3DRMXOFSAVE_MATERIALS = 4;
  D3DRMXOFSAVE_TEXTURENAMES = 8;
  D3DRMXOFSAVE_ALL = 15;
  D3DRMXOFSAVE_TEMPLATES = 16;
  D3DRMXOFSAVE_TEXTURETOPOLOGY = 32;

type
  PD3DRMColorSource = ^TD3DRMColorSource;
  TD3DRMColorSource = (
    D3DRMCOLOR_FROMFACE,
    D3DRMCOLOR_FROMVERTEX
  );

  PD3DRMFrameConstraint = ^TD3DRMFrameConstraint;
  TD3DRMFrameConstraint = (
    D3DRMCONSTRAIN_Z,           (* use only X and Y rotations *)
    D3DRMCONSTRAIN_Y,           (* use only X and Z rotations *)
    D3DRMCONSTRAIN_X            (* use only Y and Z rotations *)
  );

  PD3DRMMaterialMode = ^TD3DRMMaterialMode;
  TD3DRMMaterialMode = (
    D3DRMMATERIAL_FROMMESH,
    D3DRMMATERIAL_FROMPARENT,
    D3DRMMATERIAL_FROMFRAME
  );

  PD3DRMFogMode = ^TD3DRMFogMode;
  TD3DRMFogMode = (
    D3DRMFOG_LINEAR,            (* linear between start and end *)
    D3DRMFOG_EXPONENTIAL,       (* density * exp(-distance) *)
    D3DRMFOG_EXPONENTIALSQUARED (* density * exp(-distance*distance) *)
  );

  PD3DRMZBufferMode = ^TD3DRMZBufferMode;
  TD3DRMZBufferMode = (
    D3DRMZBUFFER_FROMPARENT,    (* default *)
    D3DRMZBUFFER_ENABLE,        (* enable zbuffering *)
    D3DRMZBUFFER_DISABLE        (* disable zbuffering *)
  );

  PD3DRMSortMode = ^TD3DRMSortMode;
  TD3DRMSortMode = (
    D3DRMSORT_FROMPARENT,       (* default *)
    D3DRMSORT_NONE,             (* don't sort child frames *)
    D3DRMSORT_FRONTTOBACK,      (* sort child frames front-to-back *)
    D3DRMSORT_BACKTOFRONT       (* sort child frames back-to-front *)
  );

  TD3DRMMaterialOverride = packed record
    dwSize : DWORD;       (* Size of this structure *)
    dwFlags : DWORD;      (* Indicate which fields are valid *)
    dcDiffuse : TD3DColorValue;    (* RGBA *)
    dcAmbient : TD3DColorValue;    (* RGB *)
    dcEmissive : TD3DColorValue;   (* RGB *)
    dcSpecular : TD3DColorValue;   (* RGB *)
    dvPower : TD3DValue;
    lpD3DRMTex : IUnknown;
  end;

const
  D3DRMMATERIALOVERRIDE_DIFFUSE_ALPHAONLY     = $00000001;
  D3DRMMATERIALOVERRIDE_DIFFUSE_RGBONLY       = $00000002;
  D3DRMMATERIALOVERRIDE_DIFFUSE               = $00000003;
  D3DRMMATERIALOVERRIDE_AMBIENT               = $00000004;
  D3DRMMATERIALOVERRIDE_EMISSIVE              = $00000008;
  D3DRMMATERIALOVERRIDE_SPECULAR              = $00000010;
  D3DRMMATERIALOVERRIDE_POWER                 = $00000020;
  D3DRMMATERIALOVERRIDE_TEXTURE               = $00000040;
  D3DRMMATERIALOVERRIDE_DIFFUSE_ALPHAMULTIPLY = $00000080;
  D3DRMMATERIALOVERRIDE_ALL                   = $000000FF;

  D3DRMFPTF_ALPHA                           = $00000001;
  D3DRMFPTF_NOALPHA                         = $00000002;
  D3DRMFPTF_PALETTIZED                      = $00000004;
  D3DRMFPTF_NOTPALETTIZED                   = $00000008;

  D3DRMSTATECHANGE_UPDATEONLY               = $000000001;
  D3DRMSTATECHANGE_VOLATILE                 = $000000002;
  D3DRMSTATECHANGE_NONVOLATILE              = $000000004;
  D3DRMSTATECHANGE_RENDER                   = $000000020;
  D3DRMSTATECHANGE_LIGHT                    = $000000040;

(*
 * Values for flags in RM3::CreateDeviceFromSurface
 *)
  D3DRMDEVICE_NOZBUFFER           = $00000001;

(*
 * Values for flags in Object2::SetClientData
 *)
  D3DRMCLIENTDATA_NONE            = $00000001;
  D3DRMCLIENTDATA_LOCALFREE       = $00000002;
  D3DRMCLIENTDATA_IUNKNOWN        = $00000004;

(*
 * Values for flags in Frame2::AddMoveCallback.
 *)
  D3DRMCALLBACK_PREORDER		= 0;
  D3DRMCALLBACK_POSTORDER		= 1;

(*
 * Values for flags in MeshBuilder2::RayPick.
 *)
  D3DRMRAYPICK_ONLYBOUNDINGBOXES	= 1;
  D3DRMRAYPICK_IGNOREFURTHERPRIMITIVES	= 2;
  D3DRMRAYPICK_INTERPOLATEUV		= 4;
  D3DRMRAYPICK_INTERPOLATECOLOR		= 8;
  D3DRMRAYPICK_INTERPOLATENORMAL        = $10;

(*
 * Values for flags in MeshBuilder3::AddFacesIndexed.
 *)
  D3DRMADDFACES_VERTICESONLY             = 1;

(*
 * Values for flags in MeshBuilder2::GenerateNormals.
 *)
  D3DRMGENERATENORMALS_PRECOMPACT	= 1;
  D3DRMGENERATENORMALS_USECREASEANGLE	= 2;

(*
 * Values for MeshBuilder3::GetParentMesh
 *)
  D3DRMMESHBUILDER_DIRECTPARENT          = 1;
  D3DRMMESHBUILDER_ROOTMESH              = 2;

(*
 * Flags for MeshBuilder3::Enable
 *)
  D3DRMMESHBUILDER_RENDERENABLE   = $00000001;
  D3DRMMESHBUILDER_PICKENABLE     = $00000002;

(*
 * Flags for Object2::GetAge when used with MeshBuilders
 *)
  D3DRMMESHBUILDERAGE_GEOMETRY    = $00000001;
  D3DRMMESHBUILDERAGE_MATERIALS   = $00000002;
  D3DRMMESHBUILDERAGE_TEXTURES    = $00000004;

(*
 * Format flags for MeshBuilder3::AddTriangles.
 *)
  D3DRMFVF_TYPE                   = $00000001;
  D3DRMFVF_NORMAL                 = $00000002;
  D3DRMFVF_COLOR                  = $00000004;
  D3DRMFVF_TEXTURECOORDS          = $00000008;

  D3DRMVERTEX_STRIP               = $00000001;
  D3DRMVERTEX_FAN                 = $00000002;
  D3DRMVERTEX_LIST                = $00000004;

(*
 * Values for flags in Viewport2::Clear2
 *)
  D3DRMCLEAR_TARGET               = $00000001;
  D3DRMCLEAR_ZBUFFER              = $00000002;
  D3DRMCLEAR_DIRTYRECTS           = $00000004;
  D3DRMCLEAR_ALL                  = (D3DRMCLEAR_TARGET or
                                         D3DRMCLEAR_ZBUFFER or
                                         D3DRMCLEAR_DIRTYRECTS);

(*
 * Values for flags in Frame3::SetSceneFogMethod
 *)
  D3DRMFOGMETHOD_VERTEX          = $00000001;
  D3DRMFOGMETHOD_TABLE           = $00000002;
  D3DRMFOGMETHOD_ANY             = $00000004;

(*
 * Values for flags in Frame3::SetTraversalOptions
 *)
  D3DRMFRAME_RENDERENABLE        = $00000001;
  D3DRMFRAME_PICKENABLE          = $00000002;

type
  TD3DRMAnimationOptions = DWORD;

const
  D3DRMANIMATION_OPEN = $01;
  D3DRMANIMATION_CLOSED = $02;
  D3DRMANIMATION_LINEARPOSITION = $04;
  D3DRMANIMATION_SPLINEPOSITION = $08;
  D3DRMANIMATION_SCALEANDROTATION = $00000010;
  D3DRMANIMATION_POSITION = $00000020;

type
  TD3DRMInterpolationOptions = DWORD;
const
  D3DRMINTERPOLATION_OPEN = $01;
  D3DRMINTERPOLATION_CLOSED = $02;
  D3DRMINTERPOLATION_NEAREST = $0100;
  D3DRMINTERPOLATION_LINEAR = $04;
  D3DRMINTERPOLATION_SPLINE = $08;
  D3DRMINTERPOLATION_VERTEXCOLOR = $40;
  D3DRMINTERPOLATION_SLERPNORMALS = $80;

type
  TD3DRMLoadOptions = DWORD;

const
  D3DRMLOAD_FROMFILE  = $00;
  D3DRMLOAD_FROMRESOURCE = $01;
  D3DRMLOAD_FROMMEMORY = $02;
  D3DRMLOAD_FROMSTREAM = $04;
  D3DRMLOAD_FROMURL = $08;

  D3DRMLOAD_BYNAME = $10;
  D3DRMLOAD_BYPOSITION = $20;
  D3DRMLOAD_BYGUID = $40;
  D3DRMLOAD_FIRST = $80;

  D3DRMLOAD_INSTANCEBYREFERENCE = $100;
  D3DRMLOAD_INSTANCEBYCOPYING = $200;

  D3DRMLOAD_ASYNCHRONOUS = $400;

type
  PD3DRMLoadResource = ^TD3DRMLoadResource;
  TD3DRMLoadResource = packed record
    hModule: HMODULE;
    lpName: PAnsiChar;
    lpType: PAnsiChar;
  end;

  PD3DRMLoadMemory = ^TD3DRMLoadMemory;
  TD3DRMLoadMemory = packed record
    lpMemory: Pointer;
    dwSize: DWORD;
  end;

const
  D3DRMPMESHSTATUS_VALID = $01;
  D3DRMPMESHSTATUS_INTERRUPTED = $02;
  D3DRMPMESHSTATUS_BASEMESHCOMPLETE = $04;
  D3DRMPMESHSTATUS_COMPLETE = $08;
  D3DRMPMESHSTATUS_RENDERABLE = $10;

  D3DRMPMESHEVENT_BASEMESH = $01;
  D3DRMPMESHEVENT_COMPLETE = $02;

type
  PD3DRMPMeshLoadStatus = ^TD3DRMPMeshLoadStatus;
  TD3DRMPMeshLoadStatus = packed record
    dwSize,            // Size of this structure
    dwPMeshSize,       // Total Size (bytes)
    dwBaseMeshSize,    // Total Size of the Base Mesh
    dwBytesLoaded,     // Total bytes loaded
    dwVerticesLoaded,  // Number of vertices loaded
    dwFacesLoaded : DWORD;     // Number of faces loaded
    dwLoadResult : HResult;    // Result of the load operation
    dwFlags : DWORD;
  end;

  PD3DRMUserVisualReason = ^TD3DRMUserVisualReason;
  TD3DRMUserVisualReason = (
    D3DRMUSERVISUAL_CANSEE,
    D3DRMUSERVISUAL_RENDER
  );

  PD3DRMAnimationKey = ^TD3DRMAnimationKey;
  TD3DRMAnimationKey = packed record
    dwSize : DWORD;
    dwKeyType : DWORD;
    dvTime : TD3DValue;
    dwID : DWORD;
    case integer of
      0 : (dqRotateKey : TD3DRMQuaternion);
      1 : (dvScaleKey : TD3DVector);
      2 : (dvPositionKey : TD3DVector);
      3 : (dvK : array [0..3] of TD3DValue);
    end;

procedure D3DRMAnimationGetRotateKey
    (var rmKey: TD3DRMAnimationKey; var rmQuat: TD3DRMQuaternion);

procedure D3DRMAnimationGetScaleKey
    (var rmKey: TD3DRMAnimationKey; var dvVec: TD3DVector);

procedure D3DRMAnimationGetPositionKey
    (var rmKey: TD3DRMAnimationKey; var dvVec: TD3DVector);

procedure D3DRMAnimatioSetRotateKey
    (var rmKey: TD3DRMAnimationKey; var rmQuat: TD3DRMQuaternion);

procedure D3DRMAnimationSetScaleKey
    (var rmKey: TD3DRMAnimationKey; var dvVec: TD3DVector);

procedure D3DRMAnimationSetPositionKey
    (var rmKey: TD3DRMAnimationKey; var dvVec: TD3DVector);

const
  D3DRMANIMATION_ROTATEKEY = 01;
  D3DRMANIMATION_SCALEKEY = 02;
  D3DRMANIMATION_POSITIONKEY = 03;

type
  TD3DRMMapping = DWORD;
  PD3DRMMappingFlag = ^TD3DRMMappingFlag;
  TD3DRMMappingFlag = DWORD;

const
  D3DRMMAP_WRAPU = 1;
  D3DRMMAP_WRAPV = 2;
  D3DRMMAP_PERSPCORRECT = 4;

type
  PD3DRMVertex = ^TD3DRMVertex;
  TD3DRMVertex = packed record
    position: TD3DVector;
    normal: TD3DVector;
    tu, tv: TD3DValue;
    color: TD3DColor;
  end;

  TD3DRMGroupIndex = LongInt; (* group indexes begin a 0 *)

const
  D3DRMGROUP_ALLGROUPS = -1;

var
(*
 * Create a color from three components in the range 0-1 inclusive.
 *)
  D3DRMCreateColorRGB : function (red, green, blue: TD3DValue) : TD3DColor;
      stdcall;

(*
 * Create a color from four components in the range 0-1 inclusive.
 *)
  D3DRMCreateColorRGBA : function (red, green, blue, alpha: TD3DValue)
      : TD3DColor; stdcall;

(*
 * Get the red component of a color.
 *)
  D3DRMColorGetRed : function (d3drmc: TD3DColor) : TD3DValue; stdcall;

(*
 * Get the green component of a color.
 *)
  D3DRMColorGetGreen : function (d3drmc: TD3DColor) : TD3DValue; stdcall;

(*
 * Get the blue component of a color.
 *)
  D3DRMColorGetBlue : function (d3drmc: TD3DColor) : TD3DValue; stdcall;

(*
 * Get the alpha component of a color.
 *)
  D3DRMColorGetAlpha : function (d3drmc: TD3DColor) : TD3DValue; stdcall;

(*
 * Add two vectors.  Returns its first argument.
 *)
  D3DRMVectorAdd : function (var d, s1, s2: TD3DVector) : PD3DVector; stdcall;

(*
 * Subtract two vectors.  Returns its first argument.
 *)
  D3DRMVectorSubtract : function (var d, s1, s2: TD3DVector) : PD3DVector;
      stdcall;

(*
 * Reflect a ray about a given normal.  Returns its first argument.
 *)
  D3DRMVectorReflect : function (var d, ray, norm: TD3DVector) : PD3DVector;
      stdcall;

(*
 * Calculate the vector cross product.  Returns its first argument.
 *)
  D3DRMVectorCrossProduct : function (var d, s1, s2: TD3DVector) : PD3DVector;
      stdcall;

(*
 * Return the vector dot product.
 *)
  D3DRMVectorDotProduct : function (var s1, s2: TD3DVector) : TD3DValue;
      stdcall;

(*
 * Scale a vector so that its modulus is 1.  Returns its argument or
 * NULL if there was an error (e.g. a zero vector was passed).
 *)
  D3DRMVectorNormalize : function (var lpv: TD3DVector) : PD3DVector; stdcall;

(*
 * Return the length of a vector (e.g. sqrt(x*x + y*y + z*z)).
 *)
  D3DRMVectorModulus : function (var v: TD3DVector) : TD3DValue; stdcall;

(*
 * Set the rotation part of a matrix to be a rotation of theta radians
 * around the given axis.
 *)
  D3DRMVectorRotate : function (var r, v, axis: TD3DVector; theta: TD3DValue) :
      PD3DVector; stdcall;

(*
 * Scale a vector uniformly in all three axes
 *)
  D3DRMVectorScale : function (var d, s: TD3DVector; factor: TD3DValue) :
      PD3DVector; stdcall;

(*
 * Return a random unit vector
 *)
  D3DRMVectorRandom : function (var d: TD3DVector) : PD3DVector; stdcall;

(*
 * Returns a unit quaternion that represents a rotation of theta radians
 * around the given axis.
 *)

  D3DRMQuaternionFromRotation : function (var quat: TD3DRMQuaternion;
      var v: TD3DVector; theta: TD3DValue) : PD3DRMQuaternion; stdcall;

(*
 * Calculate the product of two quaternions
 *)
  D3DRMQuaternionMultiply : function (var q, a, b: TD3DRMQuaternion) :
      PD3DRMQuaternion; stdcall;

(*
 * Interpolate between two quaternions
 *)
  D3DRMQuaternionSlerp : function (var q, a, b: TD3DRMQuaternion;
      alpha: TD3DValue) : PD3DRMQuaternion; stdcall;

(*
 * Calculate the matrix for the rotation that a unit quaternion represents
 *)
  D3DRMMatrixFromQuaternion : procedure (dmMat: TD3DRMMatrix4D; var lpDqQuat:
      TD3DRMQuaternion); stdcall;

(*
 * Calculate the quaternion that corresponds to a rotation matrix
 *)
  D3DRMQuaternionFromMatrix : function (var lpQuat: TD3DRMQuaternion;
      Mat: TD3DRMMatrix4D) : PD3DRMQuaternion; stdcall;

(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3drmobj.h
 *  Content:    Direct3DRM include file
 *
 ***************************************************************************)

(*
 * Direct3DRM Object classes
 *)

const
  CLSID_CDirect3DRMDevice: TGUID =
      (D1:$4fa3568e;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMViewport: TGUID =
      (D1:$4fa3568f;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMFrame: TGUID =
      (D1:$4fa35690;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMMesh: TGUID =
      (D1:$4fa35691;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMMeshBuilder: TGUID =
      (D1:$4fa35692;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMFace: TGUID =
      (D1:$4fa35693;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMLight: TGUID =
      (D1:$4fa35694;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMTexture: TGUID =
      (D1:$4fa35695;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMWrap: TGUID =
      (D1:$4fa35696;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMMaterial: TGUID =
      (D1:$4fa35697;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMAnimation: TGUID =
      (D1:$4fa35698;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMAnimationSet: TGUID =
      (D1:$4fa35699;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMUserVisual: TGUID =
      (D1:$4fa3569a;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMShadow: TGUID =
      (D1:$4fa3569b;D2:$623f;D3:$11cf;D4:($ac,$4a,$00,$00,$c0,$38,$25,$a1));
  CLSID_CDirect3DRMViewportInterpolator: TGUID =
      (D1:$0de9eaa1;D2:$3b84;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  CLSID_CDirect3DRMFrameInterpolator: TGUID =
      (D1:$0de9eaa2;D2:$3b84;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  CLSID_CDirect3DRMMeshInterpolator: TGUID =
      (D1:$0de9eaa3;D2:$3b84;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  CLSID_CDirect3DRMLightInterpolator: TGUID =
      (D1:$0de9eaa6;D2:$3b84;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  CLSID_CDirect3DRMMaterialInterpolator: TGUID =
      (D1:$0de9eaa7;D2:$3b84;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  CLSID_CDirect3DRMTextureInterpolator: TGUID =
      (D1:$0de9eaa8;D2:$3b84;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  CLSID_CDirect3DRMProgressiveMesh: TGUID =
      (D1:$4516ec40;D2:$8f20;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));
  CLSID_CDirect3DRMClippedVisual: TGUID =
      (D1:$5434e72d;D2:$6d66;D3:$11d1;D4:($bb,$0b,$00,$00,$f8,$75,$86,$5a));



type
  IDirect3DRMObject = interface;
  IDirect3DRMDevice = interface;
  IDirect3DRMDevice2 = interface;
  IDirect3DRMDevice3 = interface;
  IDirect3DRMViewport = interface;
  IDirect3DRMViewport2 = interface;
  IDirect3DRMFrame = interface;
  IDirect3DRMFrame2 = interface;
  IDirect3DRMFrame3 = interface;
  IDirect3DRMVisual = interface;
  IDirect3DRMMesh = interface;
  IDirect3DRMMeshBuilder = interface;
  IDirect3DRMMeshBuilder2 = interface;
  IDirect3DRMMeshBuilder3 = interface;
  IDirect3DRMFace = interface;
  IDirect3DRMFace2 = interface;
  IDirect3DRMLight = interface;
  IDirect3DRMTexture = interface;
  IDirect3DRMTexture2 = interface;
  IDirect3DRMTexture3 = interface;
  IDirect3DRMWrap = interface;
  IDirect3DRMMaterial = interface;
  IDirect3DRMMaterial2 = interface;
  IDirect3DRMAnimation = interface;
  IDirect3DRMAnimation2 = interface;
  IDirect3DRMAnimationSet = interface;
  IDirect3DRMAnimationSet2 = interface;
  IDirect3DRMArray = interface;
  IDirect3DRMObjectArray = interface;
  IDirect3DRMDeviceArray = interface;
  IDirect3DRMViewportArray = interface;
  IDirect3DRMFrameArray = interface;
  IDirect3DRMVisualArray = interface;
  IDirect3DRMLightArray = interface;
  IDirect3DRMPickedArray = interface;
  IDirect3DRMFaceArray = interface;
  IDirect3DRMAnimationArray = interface;
  IDirect3DRMUserVisual = interface;
  IDirect3DRMShadow = interface;
  IDirect3DRMShadow2 = interface;
  IDirect3DRMInterpolator = interface;
  IDirect3DRMProgressiveMesh = interface;
  IDirect3DRMPicked2Array = interface;
  IDirect3DRMClippedVisual = interface;

(*
 * Direct3DRM Object interfaces
 *)
  IID_IDirect3DRMObject =          IDirect3DRMObject;
  IID_IDirect3DRMDevice =          IDirect3DRMDevice;
  IID_IDirect3DRMDevice2 =         IDirect3DRMDevice2;
  IID_IDirect3DRMDevice3 =         IDirect3DRMDevice3;
  IID_IDirect3DRMViewport =        IDirect3DRMViewport;
  IID_IDirect3DRMViewport2 =       IDirect3DRMViewport2;
  IID_IDirect3DRMFrame =           IDirect3DRMFrame;
  IID_IDirect3DRMFrame2 =          IDirect3DRMFrame2;
  IID_IDirect3DRMFrame3 =          IDirect3DRMFrame3;
  IID_IDirect3DRMVisual =          IDirect3DRMVisual;
  IID_IDirect3DRMMesh =            IDirect3DRMMesh;
  IID_IDirect3DRMMeshBuilder =     IDirect3DRMMeshBuilder;
  IID_IDirect3DRMMeshBuilder2 =    IDirect3DRMMeshBuilder2;
  IID_IDirect3DRMMeshBuilder3 =    IDirect3DRMMeshBuilder3;
  IID_IDirect3DRMFace =            IDirect3DRMFace;
  IID_IDirect3DRMFace2 =           IDirect3DRMFace2;
  IID_IDirect3DRMLight =           IDirect3DRMLight;
  IID_IDirect3DRMTexture =         IDirect3DRMTexture;
  IID_IDirect3DRMTexture2 =        IDirect3DRMTexture2;
  IID_IDirect3DRMTexture3 =        IDirect3DRMTexture3;
  IID_IDirect3DRMWrap =            IDirect3DRMWrap;
  IID_IDirect3DRMMaterial =        IDirect3DRMMaterial;
  IID_IDirect3DRMMaterial2 =       IDirect3DRMMaterial2;
  IID_IDirect3DRMAnimation =       IDirect3DRMAnimation;
  IID_IDirect3DRMAnimation2 =      IDirect3DRMAnimation2;
  IID_IDirect3DRMAnimationSet =    IDirect3DRMAnimationSet;
  IID_IDirect3DRMAnimationSet2 =   IDirect3DRMAnimationSet2;
  IID_IDirect3DRMObjectArray =     IDirect3DRMObjectArray;
  IID_IDirect3DRMDeviceArray =     IDirect3DRMDeviceArray;
  IID_IDirect3DRMViewportArray =   IDirect3DRMViewportArray;
  IID_IDirect3DRMFrameArray =      IDirect3DRMFrameArray;
  IID_IDirect3DRMVisualArray =     IDirect3DRMVisualArray;
  IID_IDirect3DRMLightArray =      IDirect3DRMLightArray;
  IID_IDirect3DRMPickedArray =     IDirect3DRMPickedArray;
  IID_IDirect3DRMFaceArray =       IDirect3DRMFaceArray;
  IID_IDirect3DRMAnimationArray =  IDirect3DRMAnimationArray;
  IID_IDirect3DRMUserVisual =      IDirect3DRMUserVisual;
  IID_IDirect3DRMShadow =          IDirect3DRMShadow;
  IID_IDirect3DRMShadow2 =         IDirect3DRMShadow2;
  IID_IDirect3DRMInterpolator =    IDirect3DRMInterpolator;
  IID_IDirect3DRMProgressiveMesh = IDirect3DRMProgressiveMesh;
  IID_IDirect3DRMPicked2Array =    IDirect3DRMPicked2Array;
  IID_IDirect3DRMClippedVisual =   IDirect3DRMClippedVisual;





  PIDirect3DRMFaceArray = ^IDirect3DRMFaceArray;

  TD3DRMObjectCallback = procedure (lpD3DRMobj: IDirect3DRMObject;
      lpArg: Pointer); cdecl;
  TD3DRMFrameMoveCallback = procedure (lpD3DRMFrame: IDirect3DRMFrame;
      lpArg: Pointer; delta: TD3DValue); cdecl;
  TD3DRMFrame3MoveCallback = procedure (lpD3DRMFrame: IDirect3DRMFrame3;
      lpArg: Pointer; delta: TD3DValue); cdecl;
  TD3DRMUpdateCallback = procedure (lpobj: IDirect3DRMDevice; lpArg: Pointer;
      iRectCount: Integer; const d3dRectUpdate: TD3DRect); cdecl;
  TD3DRMDevice3UpdateCallback = procedure (lpobj: IDirect3DRMDevice3;
      lpArg: Pointer; iRectCount: Integer; const d3dRectUpdate: TD3DRect);cdecl;
  TD3DRMUserVisualCallback = function (lpD3DRMUV: IDirect3DRMUserVisual;
      lpArg: Pointer; lpD3DRMUVreason: TD3DRMUserVisualReason;
      lpD3DRMDev: IDirect3DRMDevice;
      lpD3DRMview: IDirect3DRMViewport) : Integer; cdecl;
  TD3DRMLoadTextureCallback = function (tex_name: PAnsiChar; lpArg: Pointer;
      out lpD3DRMTex: IDirect3DRMTexture) : HResult; cdecl;
  TD3DRMLoadTexture3Callback = function (tex_name: PAnsiChar; lpArg: Pointer;
      out lpD3DRMTex: IDirect3DRMTexture3) : HResult; cdecl;
  TD3DRMLoadCallback = procedure (lpObject: IDirect3DRMObject;
      const ObjectGuid: TGUID; lpArg: Pointer); cdecl;
  TD3DRMDownSampleCallback = function (lpDirect3DRMTexture: IDirect3DRMTexture3;
      pArg: pointer; pDDSSrc, pDDSDst: IDirectDrawSurface) : HResult; cdecl;
  TD3DRMValidationCallback = function (lpDirect3DRMTexture: IDirect3DRMTexture3;
      pArg: pointer; dwFlags, DWcRects: DWORD; const pRects: TRect) : HResult; cdecl;

  PD3DRMPickDesc = ^TD3DRMPickDesc;
  TD3DRMPickDesc = packed record
    ulFaceIdx: DWORD;
    lGroupIdx: LongInt;
    vPosition: TD3DVector;
  end;

  PD3DRMPickDesc2 = ^TD3DRMPickDesc2;
  TD3DRMPickDesc2 = packed record
    ulFaceIdx: DWORD;
    lGroupIdx: LongInt;
    dvPosition: TD3DVector;
    tu, tv: TD3DValue;
    dvNormal: TD3DVector;
    dcColor: TD3DColor;
  end;

(*
 * Base class
 *)
{$IFDEF D2COM}
  IDirect3DRMObject = class (IUnknown)
{$ELSE}
  IDirect3DRMObject = interface (IUnknown)
    ['{eb16cb00-d271-11ce-ac48-0000c03825a1}']
{$ENDIF}
    (*
     * The methods for IDirect3DRMObject
     *)
    function Clone (pUnkOuter: IUnknown; riid: TGUID;
        var ppvObj: Pointer) : HResult; stdcall;
    function AddDestroyCallback (lpCallback: TD3DRMObjectCallback;
        lpArg: Pointer) : HResult; stdcall;
    function DeleteDestroyCallback (d3drmObjProc: TD3DRMObjectCallback;
        lpArg: Pointer) : HResult; stdcall;
    function SetAppData (ulData: DWORD) : HResult; stdcall;
    function GetAppData: DWORD; stdcall;
    function SetName (lpName: PAnsiChar) : HResult; stdcall;
    function GetName (var lpdwSize: DWORD; lpName: PAnsiChar) : HResult; stdcall;
    function GetClassName (var lpdwSize: DWORD; lpName: PAnsiChar) : HResult; stdcall;
  end;

  IDirect3DRMVisual = interface (IDirect3DRMObject)
  end;

  IDirect3DRMDevice = interface (IDirect3DRMObject)
    ['{e9e19280-6e05-11cf-ac4a-0000c03825a1}']
    (*
     * IDirect3DRMDevice methods
     *)
    function Init (width: LongInt; height: LongInt) : HResult; stdcall;
    function InitFromD3D (lpD3D: IDirect3D; lpD3DIMDev: IDirect3DDevice) : HResult; stdcall;
    function InitFromClipper (lpDDClipper: IDirectDrawClipper; lpGUID: PGUID;
        width: Integer; height: Integer) : HResult; stdcall;
    function Update: HResult; stdcall;
    function AddUpdateCallback (d3drmUpdateProc: TD3DRMUpdateCallback;
        arg: Pointer) : HResult; stdcall;
    function DeleteUpdateCallback (d3drmUpdateProc: TD3DRMUpdateCallback;
        arg: Pointer) : HResult; stdcall;
    function SetBufferCount (dwCount: DWORD) : HResult; stdcall;
    function GetBufferCount: DWORD; stdcall;
    function SetDither (bDither: BOOL) : HResult; stdcall;
    function SetShades (ulShades: DWORD) : HResult; stdcall;
    function SetQuality (rqQuality: TD3DRMRenderQuality) : HResult; stdcall;
    function SetTextureQuality (tqTextureQuality: TD3DRMTextureQuality) : HResult; stdcall;
    function GetViewports (out lplpViewports: IDirect3DRMViewportArray) : HResult; stdcall;
    function GetDither: BOOL; stdcall;
    function GetShades: DWORD; stdcall;
    function GetHeight: DWORD; stdcall;
    function GetWidth: DWORD; stdcall;
    function GetTrianglesDrawn: DWORD; stdcall;
    function GetWireframeOptions: DWORD; stdcall;
    function GetQuality: TD3DRMRenderQuality; stdcall;
    function GetColorModel: TD3DColorModel; stdcall;
    function GetTextureQuality: TD3DRMTextureQuality; stdcall;
    function GetDirect3DDevice (out lplpD3DDevice: IDirect3DDevice) : HResult; stdcall;
  end;

  IDirect3DRMDevice2 = interface (IDirect3DRMDevice)
    ['{4516ec78-8f20-11d0-9b6d-0000c0781bc3}']
    (*
     * IDirect3DRMDevice2 methods
     *)
    function InitFromD3D2(lpD3D: IDirect3D2; lpD3DIMDev: IDirect3DDevice2) : HResult; stdcall;
    function InitFromSurface(const lpGUID: TGUID; lpDD: IDirectDraw; lpDDSBack: IDirectDrawSurface) : HResult; stdcall;
    function SetRenderMode(dwFlags: DWORD ) : HResult; stdcall;
    function GetRenderMode : DWORD; stdcall;
    function GetDirect3DDevice2(out lplpD3DDevice: IDirect3DDevice2) : HResult; stdcall;
  end;

  IDirect3DRMDevice3 = interface (IDirect3DRMObject)
    ['{549f498b-bfeb-11d1-8ed8-00a0c967a482}']
    (*
     * IDirect3DRMDevice methods
     *)
    function Init (width: LongInt; height: LongInt) : HResult; stdcall;
    function InitFromD3D (lpD3D: IDirect3D2; lpD3DIMDev: IDirect3DDevice2) : HResult; stdcall;
    function InitFromClipper (lpDDClipper: IDirectDrawClipper; lpGUID: PGUID;
        width: Integer; height: Integer) : HResult; stdcall;
    function Update: HResult; stdcall;
    function AddUpdateCallback (d3drmUpdateProc: TD3DRMDevice3UpdateCallback;
        arg: Pointer) : HResult; stdcall;
    function DeleteUpdateCallback (d3drmUpdateProc: TD3DRMDevice3UpdateCallback;
        arg: Pointer) : HResult; stdcall;
    function SetBufferCount (dwCount: DWORD) : HResult; stdcall;
    function GetBufferCount: DWORD; stdcall;
    function SetDither (bDither: BOOL) : HResult; stdcall;
    function SetShades (ulShades: DWORD) : HResult; stdcall;
    function SetQuality (rqQuality: TD3DRMRenderQuality) : HResult; stdcall;
    function SetTextureQuality (tqTextureQuality: TD3DRMTextureQuality) : HResult; stdcall;
    function GetViewports (out lplpViewports: IDirect3DRMViewportArray) : HResult; stdcall;
    function GetDither: BOOL; stdcall;
    function GetShades: DWORD; stdcall;
    function GetHeight: DWORD; stdcall;
    function GetWidth: DWORD; stdcall;
    function GetTrianglesDrawn: DWORD; stdcall;
    function GetWireframeOptions: DWORD; stdcall;
    function GetQuality: TD3DRMRenderQuality; stdcall;
    function GetColorModel: TD3DColorModel; stdcall;
    function GetTextureQuality: TD3DRMTextureQuality; stdcall;
    function GetDirect3DDevice (out lplpD3DDevice: IDirect3DDevice) : HResult; stdcall;
    (*
     * IDirect3DRMDevice2 methods
     *)
    function InitFromD3D2(lpD3D: IDirect3D2; lpD3DIMDev: IDirect3DDevice2) : HResult; stdcall;
    function InitFromSurface(const lpGUID: TGUID; lpDD: IDirectDraw;
	    lpDDSBack: IDirectDrawSurface) : HResult; stdcall;
    function SetRenderMode(dwFlags: DWORD ) : HResult; stdcall;
    function GetRenderMode : DWORD; stdcall;
    function GetDirect3DDevice2(out lplpD3DDevice: IDirect3DDevice2) : HResult; stdcall;
    (*
     * IDirect3DRMDevice3 methods
     *)
    function FindPreferredTextureFormat (dwBitDepths, dwFlags: DWORD;
        out lpDDPF: TDDPixelFormat) : HResult; stdcall;
    function RenderStateChange (dwStateNum, dwVal, dwFlags: DWORD) : HResult; stdcall;

    function LightStateChange (drsType: TD3DLightStateType; // defined different in header and help
        dwVal, dwFlags: DWORD) : HResult; stdcall;
    function GetStateChangeOptions (dwStateClass, dwStateNum: DWORD;
        var pdwFlags: DWORD) : HResult; stdcall;
    function SetStateChangeOptions ( dwStateClass, dwStateNum, dwFlags: DWORD) : HResult; stdcall;
  end;

  IDirect3DRMViewport = interface (IDirect3DRMObject)
    ['{eb16cb02-d271-11ce-ac48-0000c03825a1}']
    (*
     * IDirect3DRMViewport methods
     *)
    function Init (lpD3DRMDevice: IDirect3DRMDevice;
        lpD3DRMFrameCamera: IDirect3DRMFrame; xpos, ypos,
        width, height: DWORD) : HResult; stdcall;
    function Clear: HResult; stdcall;
    function Render (lpD3DRMFrame: IDirect3DRMFrame) : HResult; stdcall;
    function SetFront (rvFront: TD3DValue) : HResult; stdcall;
    function SetBack (rvBack: TD3DValue) : HResult; stdcall;
    function SetField (rvField: TD3DValue) : HResult; stdcall;
    function SetUniformScaling (bScale: BOOL) : HResult; stdcall;
    function SetCamera (lpCamera: IDirect3DRMFrame) : HResult; stdcall;
    function SetProjection (rptType: TD3DRMProjectionType) : HResult; stdcall;
    function Transform (out lprvDst: TD3DRMVector4D; const lprvSrc: TD3DVector) : HResult; stdcall;
    function InverseTransform (out lprvDst: TD3DVector;
        const lprvSrc: TD3DRMVector4D) : HResult; stdcall;
    function Configure (lX, lY: LongInt; dwWidth, dwHeight: DWORD) : HResult; stdcall;
    function ForceUpdate (dwX1, dwY1, dwX2, dwY2: DWORD) : HResult; stdcall;
    function SetPlane (rvLeft, rvRight, rvBottom, rvTop: TD3DValue) : HResult; stdcall;
    function GetCamera (out lpCamera: IDirect3DRMFrame) : HResult; stdcall;
    function GetDevice (out lpD3DRMDevice: IDirect3DRMDevice) : HResult; stdcall;
    function GetPlane (out lpd3dvLeft, lpd3dvRight, lpd3dvBottom, lpd3dvTop:
        TD3DValue) : HResult; stdcall;
    function Pick (lX, lY: LongInt; var lplpVisuals: IDirect3DRMPickedArray) : HResult; stdcall;
    function GetUniformScaling: BOOL;  stdcall;
    function GetX: LongInt; stdcall;
    function GetY: LongInt; stdcall;
    function GetWidth: DWORD; stdcall;
    function GetHeight: DWORD; stdcall;
    function GetField: TD3DValue; stdcall;
    function GetBack: TD3DValue; stdcall;
    function GetFront: TD3DValue; stdcall;
    function GetProjection: TD3DRMProjectionType; stdcall;
    function GetDirect3DViewport (out lplpD3DViewport: IDirect3DViewport) : HResult; stdcall;
  end;

  IDirect3DRMViewport2 = interface (IDirect3DRMObject)
    ['{4a1b1be6-bfed-11d1-8ed8-00a0c967a482}']
    (*
     * IDirect3DRMViewport2 methods
     *)
    function Init (lpD3DRMDevice: IDirect3DRMDevice3;
        lpD3DRMFrameCamera: IDirect3DRMFrame3; xpos, ypos,
        width, height: DWORD) : HResult; stdcall;
    function Clear (dwFlags: DWORD): HResult; stdcall;
    function Render (lpD3DRMFrame: IDirect3DRMFrame3) : HResult; stdcall;
    function SetFront (rvFront: TD3DValue) : HResult; stdcall;
    function SetBack (rvBack: TD3DValue) : HResult; stdcall;
    function SetField (rvField: TD3DValue) : HResult; stdcall;
    function SetUniformScaling (bScale: BOOL) : HResult; stdcall;
    function SetCamera (lpCamera: IDirect3DRMFrame3) : HResult; stdcall;
    function SetProjection (rptType: TD3DRMProjectionType) : HResult; stdcall;
    function Transform (out lprvDst: TD3DRMVector4D; const lprvSrc: TD3DVector) : HResult; stdcall;
    function InverseTransform (out lprvDst: TD3DVector;
        const lprvSrc: TD3DRMVector4D) : HResult; stdcall;
    function Configure (lX, lY: LongInt; dwWidth, dwHeight: DWORD) : HResult; stdcall;
    function ForceUpdate (dwX1, dwY1, dwX2, dwY2: DWORD) : HResult; stdcall;
    function SetPlane (rvLeft, rvRight, rvBottom, rvTop: TD3DValue) : HResult; stdcall;
    function GetCamera (out lpCamera: IDirect3DRMFrame3) : HResult; stdcall;
    function GetDevice (out lpD3DRMDevice: IDirect3DRMDevice3) : HResult; stdcall;
    function GetPlane (out lpd3dvLeft, lpd3dvRight, lpd3dvBottom, lpd3dvTop:
        TD3DValue) : HResult; stdcall;
    function Pick (lX, lY: LongInt; var lplpVisuals: IDirect3DRMPickedArray) : HResult; stdcall;
    function GetUniformScaling: BOOL; stdcall;
    function GetX: LongInt; stdcall;
    function GetY: LongInt; stdcall;
    function GetWidth: DWORD; stdcall;
    function GetHeight: DWORD; stdcall;
    function GetField: TD3DValue; stdcall;
    function GetBack: TD3DValue; stdcall;
    function GetFront: TD3DValue; stdcall;
    function GetProjection: TD3DRMProjectionType; stdcall;
    function GetDirect3DViewport (var lplpD3DViewport: IDirect3DViewport) : HResult; stdcall;
    function TransformVectors (dwNumVectors: DWORD; out lpDstVectors:
        TD3DRMVector4D; const lpSrcVectors: TD3DVector) : HResult; stdcall;
    function InverseTransformVectors (dwNumVectors: DWORD; out lpDstVectors:
        TD3DRMVector4D; const lpSrcVectors: TD3DVector) : HResult; stdcall;
  end;

  IDirect3DRMFrame = interface (IDirect3DRMVisual)
    ['{eb16cb03-d271-11ce-ac48-0000c03825a1}']
    (*
     * IDirect3DRMFrame methods
     *)
    function AddChild (lpD3DRMFrameChild: IDirect3DRMFrame) : HResult; stdcall;
    function AddLight (lpD3DRMLight: IDirect3DRMLight) : HResult; stdcall;
    function AddMoveCallback (d3drmFMC: TD3DRMFrameMoveCallback;
        lpArg: Pointer) : HResult; stdcall;
    function AddTransform (rctCombine: TD3DRMCombineType;
        rmMatrix: TD3DRMMatrix4D) : HResult; stdcall;
    function AddTranslation (rctCombine: TD3DRMCombineType; rvX, rvY, rvZ:
        TD3DValue) : HResult; stdcall;
    function AddScale (rctCombine: TD3DRMCombineType; rvX, rvY, rvZ: TD3DValue) : HResult; stdcall;
    function AddRotation (rctCombine: TD3DRMCombineType; rvX, rvY, rvZ,
        rvTheta: TD3DValue) : HResult; stdcall;
    function AddVisual (lpD3DRMVisual: IDirect3DRMVisual) : HResult; stdcall;
    function GetChildren (out lplpChildren: IDirect3DRMFrameArray) : HResult; stdcall;
    function GetColor: TD3DColor; stdcall;
    function GetLights (out lplpLights: IDirect3DRMLightArray) : HResult; stdcall;
    function GetMaterialMode: TD3DRMMaterialMode; stdcall;
    function GetParent (out lplpParent: IDirect3DRMFrame) : HResult; stdcall;
    function GetPosition (lpRef: IDirect3DRMFrame; out lprvPos: TD3DVector) : HResult; stdcall;
    function GetRotation (lpRef: IDirect3DRMFrame; out lprvAxis: TD3DVector;
        out lprvTheta: TD3DValue) : HResult; stdcall;
    function GetScene (out lplpRoot: IDirect3DRMFrame) : HResult; stdcall;
    function GetSortMode: TD3DRMSortMode; stdcall;
    function GetTexture (out lplpTexture: IDirect3DRMTexture) : HResult; stdcall;
    function GetTransform (out rmMatrix: TD3DRMMatrix4D) : HResult; stdcall;
    function GetVelocity (lpRef: IDirect3DRMFrame; var lprvVel: TD3DVector;
        fRotVel: BOOL) : HResult; stdcall;
    function GetOrientation (lpRef: IDirect3DRMFrame; var lprvDir: TD3DVector;
        var lprvUp: TD3DVector) : HResult; stdcall;
    function GetVisuals (out lplpVisuals: IDirect3DRMVisualArray) : HResult; stdcall;
    function GetTextureTopology (out lpU, lpV: BOOL) : HResult; stdcall;
    function InverseTransform (out lprvDst: TD3DVector; const lprvSrc: TD3DVector) : HResult; stdcall;
    function Load (lpvObjSource: Pointer; lpvObjID: Pointer;
        d3drmLOFlags: TD3DRMLoadOptions; d3drmLoadTextureProc:
        TD3DRMLoadTextureCallback; lpArgLTP: Pointer) : HResult; stdcall;
    function LookAt (lpTarget, lpRef: IDirect3DRMFrame;
        rfcConstraint: TD3DRMFrameConstraint ) : HResult; stdcall;
    function Move (delta: TD3DValue) : HResult; stdcall;
    function DeleteChild (lpChild: IDirect3DRMFrame) : HResult; stdcall;
    function DeleteLight (lpD3DRMLight: IDirect3DRMLight) : HResult; stdcall;
    function DeleteMoveCallback (d3drmFMC: TD3DRMFrameMoveCallback;
        lpArg: Pointer) : HResult; stdcall;
    function DeleteVisual (lpD3DRMVisual: IDirect3DRMVisual) : HResult; stdcall;
    function GetSceneBackground: TD3DColor; stdcall;
    function GetSceneBackgroundDepth (out lplpDDSurface: IDirectDrawSurface) : HResult; stdcall;
    function GetSceneFogColor: TD3DColor; stdcall;
    function GetSceneFogEnable: BOOL; stdcall;
    function GetSceneFogMode: TD3DRMFogMode; stdcall;
    function GetSceneFogParams (out lprvStart, lprvEnd, lprvDensity: TD3DValue) : HResult; stdcall;
    function SetSceneBackground (rcColor: TD3DColor) : HResult; stdcall;
    function SetSceneBackgroundRGB (rvRed, rvGreen, rvBlue: TD3DValue) : HResult; stdcall;
    function SetSceneBackgroundDepth (lpImage: IDirectDrawSurface) : HResult; stdcall;
    function SetSceneBackgroundImage (lpTexture: IDirect3DRMTexture) : HResult; stdcall;
    function SetSceneFogEnable (bEnable: BOOL) : HResult; stdcall;
    function SetSceneFogColor (rcColor: TD3DColor) : HResult; stdcall;
    function SetSceneFogMode (rfMode: TD3DRMFogMode) : HResult; stdcall;
    function SetSceneFogParams (rvStart, rvEnd, rvDensity: TD3DValue) : HResult; stdcall;
    function SetColor (rcColor: TD3DColor) : HResult; stdcall;
    function SetColorRGB (rvRed, rvGreen, rvBlue: TD3DValue) : HResult; stdcall;
    function GetZbufferMode: TD3DRMZBufferMode; stdcall;
    function SetMaterialMode (rmmMode: TD3DRMMaterialMode) : HResult; stdcall;
    function SetOrientation (lpRef: IDirect3DRMFrame; rvDx, rvDy, rvDz, rvUx,
        rvUy, rvUz: TD3DValue) : HResult; stdcall;
    function SetPosition (lpRef: IDirect3DRMFrame; rvX, rvY, rvZ: TD3DValue) : HResult; stdcall;
    function SetRotation (lpRef: IDirect3DRMFrame; rvX, rvY, rvZ,
        rvTheta: TD3DValue) : HResult; stdcall;
    function SetSortMode (d3drmSM: TD3DRMSortMode) : HResult; stdcall;
    function SetTexture (lpD3DRMTexture: IDirect3DRMTexture) : HResult; stdcall;
    function SetTextureTopology (cylU, cylV: BOOL) : HResult; stdcall;
    function SetVelocity (lpRef: IDirect3DRMFrame; rvX, rvY, rvZ: TD3DValue;
        fRotVel: BOOL) : HResult; stdcall;
    function SetZbufferMode (d3drmZBM: TD3DRMZBufferMode) : HResult; stdcall;
    function Transform (out lpd3dVDst: TD3DVector; const lpd3dVSrc: TD3DVector) : HResult; stdcall;
  end;

  IDirect3DRMFrame2 = interface (IDirect3DRMFrame)
    ['{c3dfbd60-3988-11d0-9ec2-0000c0291ac3}']
    (*
     * IDirect3DRMFrame2 methods
     *)
    function AddMoveCallback2 (d3drmFMC: TD3DRMFrameMoveCallback; lpArg:
        Pointer; dwFlags: DWORD) : HResult; stdcall;
    function GetBox (out lpTD3DRMBox: TD3DRMBox) : HResult; stdcall;
    function GetBoxEnable : boolean; stdcall;
    function GetAxes (out dir, up: TD3DVector) : HResult; stdcall;
    function GetMaterial (out lplpMaterial: IDirect3DRMMaterial) : HResult; stdcall;
    function GetInheritAxes : boolean; stdcall;
    function GetHierarchyBox (out lpTD3DRMBox: TD3DRMBox) : HResult; stdcall;
    function SetBox (const lpTD3DRMBox: TD3DRMBox) : HResult; stdcall;
    function SetBoxEnable (bEnableFlag: BOOL) : HResult; stdcall;
    function SetAxes (dx, dy, dz, ux, uy, uz: TD3DValue) : HResult; stdcall;
    function SetInheritAxes (inherit_from_parent: BOOL) : HResult; stdcall;
    function SetMaterial (var lplpMaterial: IDirect3DRMMaterial) : HResult; stdcall;
    function SetQuaternion (lpRef: IDirect3DRMFrame;
        const quat: TD3DRMQuaternion) : HResult; stdcall;
    function RayPick (lpRefFrame: IDirect3DRMFrame; var ray: TD3DRMRay;
        dwFlags: DWORD; out lplpPicked2Array: IDirect3DRMPicked2Array) :
        HResult; stdcall;
    function Save (lpFilename: PAnsiChar; d3dFormat: TD3DRMXOFFormat;
        d3dSaveFlags: TD3DRMSaveOptions) : HResult; stdcall;
  end;

  IDirect3DRMFrame3 = interface (IDirect3DRMVisual)
    ['{ff6b7f70-a40e-11d1-91f9-0000f8758e66}']
    (*
     * IDirect3DRMFrame3 methods
     *)
    function AddChild (lpD3DRMFrameChild: IDirect3DRMFrame3) : HResult; stdcall;
    function AddLight (lpD3DRMLight: IDirect3DRMLight) : HResult; stdcall;
    function AddMoveCallback (d3drmFMC: TD3DRMFrame3MoveCallback;
        lpArg: Pointer; dwFlags: DWORD) : HResult; stdcall;
    function AddTransform (rctCombine: TD3DRMCombineType;
        rmMatrix: TD3DRMMatrix4D) : HResult; stdcall;
    function AddTranslation (rctCombine: TD3DRMCombineType; rvX, rvY, rvZ:
        TD3DValue) : HResult; stdcall;
    function AddScale (rctCombine: TD3DRMCombineType; rvX, rvY, rvZ: TD3DValue) : HResult; stdcall;
    function AddRotation (rctCombine: TD3DRMCombineType; rvX, rvY, rvZ,
        rvTheta: TD3DValue) : HResult; stdcall;
    function AddVisual (lpD3DRMVisual: IDirect3DRMVisual) : HResult; stdcall;
    function GetChildren (out lplpChildren: IDirect3DRMFrameArray) : HResult; stdcall;
    function GetColor: TD3DColor; stdcall;
    function GetLights (out lplpLights: IDirect3DRMLightArray) : HResult; stdcall;
    function GetMaterialMode: TD3DRMMaterialMode; stdcall;
    function GetParent (out lplpParent: IDirect3DRMFrame3) : HResult; stdcall;
    function GetPosition (lpRef: IDirect3DRMFrame3; out lprvPos: TD3DVector) : HResult; stdcall;
    function GetRotation (lpRef: IDirect3DRMFrame3; out lprvAxis: TD3DVector;
        out lprvTheta: TD3DValue) : HResult; stdcall;
    function GetScene (out lplpRoot: IDirect3DRMFrame3) : HResult; stdcall;
    function GetSortMode: TD3DRMSortMode; stdcall;
    function GetTexture (out lplpTexture: IDirect3DRMTexture3) : HResult; stdcall;
    function GetTransform (lpRefFrame: IDirect3DRMFrame3;
        var rmMatrix: TD3DRMMatrix4D) : HResult; stdcall;
    function GetVelocity (lpRef: IDirect3DRMFrame3; out lprvVel: TD3DVector;
        fRotVel: BOOL) : HResult; stdcall;
    function GetOrientation (lpRef: IDirect3DRMFrame3; out lprvDir: TD3DVector;
        out lprvUp: TD3DVector) : HResult; stdcall;
    function GetVisuals (out lplpVisuals: IDirect3DRMVisualArray) : HResult; stdcall;
    function InverseTransform (out lprvDst: TD3DVector; const lprvSrc: TD3DVector) : HResult; stdcall;
    function Load (lpvObjSource: Pointer; lpvObjID: Pointer;
        d3drmLOFlags: TD3DRMLoadOptions; d3drmLoadTextureProc:
        TD3DRMLoadTexture3Callback; lpArgLTP: Pointer) : HResult; stdcall;
    function LookAt (lpTarget, lpRef: IDirect3DRMFrame3;
        rfcConstraint: TD3DRMFrameConstraint ) : HResult; stdcall;
    function Move (delta: TD3DValue) : HResult; stdcall;
    function DeleteChild (lpChild: IDirect3DRMFrame3) : HResult; stdcall;
    function DeleteLight (lpD3DRMLight: IDirect3DRMLight) : HResult; stdcall;
    function DeleteMoveCallback (d3drmFMC: TD3DRMFrame3MoveCallback; lpArg: Pointer) : HResult; stdcall;
    function DeleteVisual (lpD3DRMVisual: IDirect3DRMVisual) : HResult; stdcall;
    function GetSceneBackground: TD3DColor; stdcall;
    function GetSceneBackgroundDepth (out lplpDDSurface: IDirectDrawSurface) : HResult; stdcall;
    function GetSceneFogColor: TD3DColor; stdcall;
    function GetSceneFogEnable: BOOL; stdcall;
    function GetSceneFogMode: TD3DRMFogMode; stdcall;
    function GetSceneFogParams (out lprvStart, lprvEnd, lprvDensity: TD3DValue) : HResult; stdcall;
    function SetSceneBackground (rcColor: TD3DColor) : HResult; stdcall;
    function SetSceneBackgroundRGB (rvRed, rvGreen, rvBlue: TD3DValue) : HResult; stdcall;
    function SetSceneBackgroundDepth (lpImage: IDirectDrawSurface) : HResult; stdcall;
    function SetSceneBackgroundImage (lpTexture: IDirect3DRMTexture3) : HResult; stdcall;
    function SetSceneFogEnable (bEnable: BOOL) : HResult; stdcall;
    function SetSceneFogColor (rcColor: TD3DColor) : HResult; stdcall;
    function SetSceneFogMode (rfMode: TD3DRMFogMode) : HResult; stdcall;
    function SetSceneFogParams (rvStart, rvEnd, rvDensity: TD3DValue) : HResult; stdcall;
    function SetColor (rcColor: TD3DColor) : HResult; stdcall;
    function SetColorRGB (rvRed, rvGreen, rvBlue: TD3DValue) : HResult; stdcall;
    function GetZbufferMode: TD3DRMZBufferMode; stdcall;
    function SetMaterialMode (rmmMode: TD3DRMMaterialMode) : HResult; stdcall;
    function SetOrientation (lpRef: IDirect3DRMFrame3; rvDx, rvDy, rvDz, rvUx,
        rvUy, rvUz: TD3DValue) : HResult; stdcall;
    function SetPosition (lpRef: IDirect3DRMFrame3; rvX, rvY, rvZ: TD3DValue) :
        HResult; stdcall;
    function SetRotation (lpRef: IDirect3DRMFrame3; rvX, rvY, rvZ,
        rvTheta: TD3DValue) : HResult; stdcall;
    function SetSortMode (d3drmSM: TD3DRMSortMode) : HResult; stdcall;
    function SetTexture (lpD3DRMTexture: IDirect3DRMTexture3) : HResult; stdcall;
    function SetVelocity (lpRef: IDirect3DRMFrame3; rvX, rvY, rvZ: TD3DValue;
        fRotVel: BOOL) : HResult; stdcall;
    function SetZbufferMode (d3drmZBM: TD3DRMZBufferMode) : HResult; stdcall;
    function Transform (out lpd3dVDst: TD3DVector; const lpd3dVSrc: TD3DVector) : HResult; stdcall;

    function GetBox (out lpTD3DRMBox: TD3DRMBox) : HResult; stdcall;
    function GetBoxEnable : boolean; stdcall;
    function GetAxes (out dir, up: TD3DVector) : HResult; stdcall;
    function GetMaterial (out lplpMaterial: IDirect3DRMMaterial2) : HResult; stdcall;
    function GetInheritAxes : boolean; stdcall;
    function GetHierarchyBox (out lpTD3DRMBox: TD3DRMBox) : HResult; stdcall;
    function SetBox (const lpTD3DRMBox: TD3DRMBox) : HResult; stdcall;
    function SetBoxEnable (bEnableFlag: BOOL) : HResult; stdcall;
    function SetAxes (dx, dy, dz, ux, uy, uz: TD3DValue) : HResult; stdcall;
    function SetInheritAxes (inherit_from_parent: BOOL) : HResult; stdcall;
    function SetMaterial (var lplpMaterial: IDirect3DRMMaterial2) : HResult; stdcall;
    function SetQuaternion (lpRef: IDirect3DRMFrame3;
        const quat: TD3DRMQuaternion) : HResult; stdcall;
    function RayPick (lpRefFrame: IDirect3DRMFrame3; var ray: TD3DRMRay;
        dwFlags: DWORD; out lplpPicked2Array: IDirect3DRMPicked2Array) : HResult; stdcall;
    function Save (lpFilename: PAnsiChar; d3dFormat: TD3DRMXOFFormat;
        d3dSaveFlags: TD3DRMSaveOptions) : HResult; stdcall;
    function TransformVectors (lpRefFrame: IDirect3DRMFrame3;
        dwNumVectors: DWORD; out lpDstVectors: TD3DVector;
        const lpSrcVectors: TD3DVector) : HResult; stdcall;
    function InverseTransformVectors (lpRefFrame: IDirect3DRMFrame3;
        dwNumVectors: DWORD; out lpDstVectors: TD3DVector;
        const lpSrcVectors: TD3DVector) : HResult; stdcall;
    function SetTraversalOptions (dwFlags: DWORD) : HResult; stdcall;
    function GetTraversalOptions (out lpdwFlags: DWORD) : HResult; stdcall;
    function SetSceneFogMethod (dwFlags: DWORD) : HResult; stdcall;
    function GetSceneFogMethod (out lpdwFlags: DWORD) : HResult; stdcall;
    function SetMaterialOverride (
        const lpdmOverride: TD3DRMMaterialOverride) : HResult; stdcall;
    function GetMaterialOverride (
        const lpdmOverride: TD3DRMMaterialOverride) : HResult; stdcall;
  end;


  IDirect3DRMMesh = interface (IDirect3DRMVisual)
    ['{a3a80d01-6e12-11cf-ac4a-0000c03825a1}']
    (*
     * IDirect3DRMMesh methods
     *)
    function Scale (sx, sy, sz: TD3DValue) : HResult; stdcall;
    function Translate (tx, ty, tz: TD3DValue) : HResult; stdcall;
    function GetBox (out lpTD3DRMBox: TD3DRMBox) : HResult; stdcall;
    function AddGroup (vCount, fCount, vPerFace: DWORD; var fData: DWORD;
        var returnId: TD3DRMGroupIndex) : HResult; stdcall;
    function SetVertices (id: TD3DRMGroupIndex; index, count: DWORD;
        var values: TD3DRMVertex) : HResult; stdcall;
    function SetGroupColor (id: TD3DRMGroupIndex; value: TD3DColor) : HResult; stdcall;
    function SetGroupColorRGB (id: TD3DRMGroupIndex; red, green,
        blue: TD3DValue) : HResult; stdcall;
    function SetGroupMapping (id: TD3DRMGroupIndex;
        value: TD3DRMMapping) : HResult; stdcall;
    function SetGroupQuality (id: TD3DRMGroupIndex;
        value: TD3DRMRenderQuality) : HResult; stdcall;
    function SetGroupMaterial (id: TD3DRMGroupIndex; value:
        IDirect3DRMMaterial) : HResult; stdcall;
    function SetGroupTexture (id: TD3DRMGroupIndex; value: IDirect3DRMTexture) : HResult; stdcall;
    function GetGroupCount: DWORD; stdcall;
    function GetGroup (id: TD3DRMGroupIndex; vCount, fCount, vPerFace : PDWORD;
        var fDataSize: DWORD; fData: PDWORD) : HResult; stdcall;
    function GetVertices (id: TD3DRMGroupIndex; index, count : DWORD;
        out returnPtr : TD3DRMVertex) : HResult; stdcall;
    function GetGroupColor (id: TD3DRMGroupIndex) : TD3DColor; stdcall;
    function GetGroupMapping (id: TD3DRMGroupIndex) : TD3DRMMapping; stdcall;
    function GetGroupQuality (id: TD3DRMGroupIndex) : TD3DRMRenderQuality; stdcall;
    function GetGroupMaterial (id: TD3DRMGroupIndex;
        out returnPtr: IDirect3DRMMaterial) : HResult; stdcall;
    function GetGroupTexture (id: TD3DRMGroupIndex;
        out returnPtr: IDirect3DRMTexture) : HResult; stdcall;
  end;

  IDirect3DRMProgressiveMesh = interface (IDirect3DRMVisual)
    ['{4516ec79-8f20-11d0-9b6d-0000c0781bc3}']
    (*
     * IDirect3DRMProgressiveMesh methods
     *)
    function Load (lpSource, lpObjID: pointer; dloLoadflags : TD3DRMLoadOptions;
        lpCallback: TD3DRMLoadTextureCallback; lpArg: pointer) : HResult; stdcall;
    function GetLoadStatus (out lpStatus: TD3DRMPMeshLoadStatus) : HResult; stdcall;
    function SetMinRenderDetail (d3dVal: TD3DValue) : HResult; stdcall;
    function Abort (dwFlags: DWORD) : HResult; stdcall;
    function GetFaceDetail (out lpdwCount: DWORD) : HResult; stdcall;
    function GetVertexDetail (out lpdwCount: DWORD) : HResult; stdcall;
    function SetFaceDetail (dwCount: DWORD) : HResult; stdcall;
    function SetVertexDetail (dwCount: DWORD) : HResult; stdcall;
    function GetFaceDetailRange (out lpdwMin, lpdwMax: DWORD) : HResult; stdcall;
    function GetVertexDetailRange (out lpdwMin, lpdwMax: DWORD) : HResult; stdcall;
    function GetDetail (out lpdvVal: TD3DValue) : HResult; stdcall;
    function SetDetail (lpdvVal: TD3DValue) : HResult; stdcall;
    function RegisterEvents (hEvent: THANDLE; dwFlags, dwReserved: DWORD) : HResult; stdcall;
    function CreateMesh (out lplpD3DRMMesh: IDirect3DRMMesh) : HResult; stdcall;
    function Duplicate (out lplpD3DRMPMesh: IDirect3DRMProgressiveMesh) : HResult; stdcall;
    function GetBox (out lpBBox: TD3DRMBox) : HResult; stdcall;
    function SetQuality (quality: TD3DRMRenderQuality) : HResult; stdcall;
    function GetQuality (out lpdwquality: TD3DRMRenderQuality) : HResult; stdcall;
  end;

  IDirect3DRMShadow = interface (IDirect3DRMVisual)
    ['{af359780-6ba3-11cf-ac4a-0000c03825a1}']
    (*
     * IDirect3DRMShadow methods
     *)
    function Init (lpD3DRMVisual: IDirect3DRMVisual;
        lpD3DRMLight: IDirect3DRMLight;
        px, py, pz, nx, ny, nz: TD3DValue) : HResult; stdcall;
  end;

  IDirect3DRMShadow2 = interface (IDirect3DRMShadow)
    ['{86b44e25-9c82-11d1-bb0b-00a0c981a0a6}']
    (*
     * IDirect3DRMShadow2 methods
     *)
    function GetVisual (out lplpDirect3DRMVisual: IDirect3DRMVisual) : HResult; stdcall;
    function SetVisual (lpDirect3DRMVisual: IDirect3DRMVisual;
        dwFlags: DWORD) : HResult; stdcall;
    function GetLight (out lplpDirect3DRMLight: IDirect3DRMLight) : HResult; stdcall;
    function SetLight (lplpDirect3DRMLight: IDirect3DRMLight;
        dwFlags: DWORD) : HResult; stdcall;
    function GetPlane (
        var pdvPX, pdvPY, pdvPZ, pdvNX, pdvNY, pdvNZ: TD3DValue) : HResult; stdcall;
    function SetPlane (px, py, pz, nx, ny, nz: TD3DValue;
        dwFlags: DWORD) : HResult; stdcall;
    function GetOptions (out pdwOptions: DWORD) : HResult; stdcall;
    function SetOptions (dwOptions: DWORD) : HResult; stdcall;

  end;

  IDirect3DRMFace = interface (IDirect3DRMObject)
    ['{eb16cb07-d271-11ce-ac48-0000c03825a1}']
    (*
     * IDirect3DRMFace methods
     *)
    function AddVertex (x, y, z: TD3DValue) : HResult; stdcall;
    function AddVertexAndNormalIndexed (vertex: DWORD; normal: DWORD) : HResult; stdcall;
    function SetColorRGB (red, green, blue: TD3DValue) : HResult; stdcall;
    function SetColor (color: TD3DColor) : HResult; stdcall;
    function SetTexture (lpD3DRMTexture: IDirect3DRMTexture) : HResult; stdcall;
    function SetTextureCoordinates (vertex: DWORD; u, v: TD3DValue) : HResult; stdcall;
    function SetMaterial (lpMat: IDirect3DRMMaterial) : HResult; stdcall;
    function SetTextureTopology (cylU, cylV: BOOL) : HResult; stdcall;
    function GetVertex (index: DWORD; out lpPosition: TD3DVector;
        out lpNormal: TD3DVector) : HResult; stdcall;
    function GetVertices (var lpdwVertexCount: DWORD;
        out lpPosition, lpNormal: TD3DVector) : HResult; stdcall;
    function GetTextureCoordinates (index: DWORD; out lpU, lpV: TD3DValue) : HResult; stdcall;
    function GetTextureTopology (out lpU, lpV: BOOL) : HResult; stdcall;
    function GetNormal (out lpNormal: TD3DVector) : HResult; stdcall;
    function GetTexture (out lplpTexture: IDirect3DRMTexture) : HResult; stdcall;
    function GetMaterial (out lpMat: IDirect3DRMMaterial) : HResult; stdcall;
    function GetVertexCount: Integer; stdcall;
    function GetVertexIndex (dwIndex: DWORD) : Integer; stdcall;
    function GetTextureCoordinateIndex (dwIndex: DWORD) : Integer; stdcall;
    function GetColor: TD3DColor; stdcall;
  end;

  IDirect3DRMFace2 = interface (IDirect3DRMObject)
    ['{4516ec81-8f20-11d0-9b6d-0000c0781bc3}']
    (*
     * IDirect3DRMFace2 methods
     *)
    function AddVertex (x, y, z: TD3DValue) : HResult; stdcall;
    function AddVertexAndNormalIndexed (vertex: DWORD; normal: DWORD) : HResult; stdcall;
    function SetColorRGB (red, green, blue: TD3DValue) : HResult; stdcall;
    function SetColor (color: TD3DColor) : HResult; stdcall;
    function SetTexture (lpD3DRMTexture: IDirect3DRMTexture3) : HResult; stdcall;
    function SetTextureCoordinates (vertex: DWORD; u, v: TD3DValue) : HResult; stdcall;
    function SetMaterial (lpMat: IDirect3DRMMaterial2) : HResult; stdcall;
    function SetTextureTopology (cylU, cylV: BOOL) : HResult; stdcall;
    function GetVertex (index: DWORD; out lpPosition: TD3DVector;
        out lpNormal: TD3DVector) : HResult; stdcall;
    function GetVertices (var lpdwVertexCount: DWORD;
        out lpPosition, lpNormal: TD3DVector) : HResult; stdcall;
    function GetTextureCoordinates (index: DWORD; out lpU, lpV: TD3DValue) : HResult; stdcall;
    function GetTextureTopology (out lpU, lpV: BOOL) : HResult; stdcall;
    function GetNormal (out lpNormal: TD3DVector) : HResult; stdcall;
    function GetTexture (out lplpTexture: IDirect3DRMTexture3) : HResult; stdcall;
    function GetMaterial (out lpMat: IDirect3DRMMaterial2) : HResult; stdcall;
    function GetVertexCount: Integer; stdcall;
    function GetVertexIndex (dwIndex: DWORD) : Integer; stdcall;
    function GetTextureCoordinateIndex (dwIndex: DWORD) : Integer; stdcall;
    function GetColor: TD3DColor; stdcall;
  end;

  IDirect3DRMMeshBuilder = interface (IDirect3DRMVisual)
    ['{a3a80d02-6e12-11cf-ac4a-0000c03825a1}']
    (*
     * IDirect3DRMMeshBuilder methods
     *)
    function Load (lpvObjSource, lpvObjID: Pointer; d3drmLOFlags:
        TD3DRMLoadOptions; d3drmLoadTextureProc: TD3DRMLoadTextureCallback;
        lpvArg: Pointer) : HResult; stdcall;
    function Save (lpFilename: PChar; TD3DRMXOFFormat: TD3DRMXOFFormat;
        d3drmSOContents: TD3DRMSaveOptions) : HResult; stdcall;
    function Scale (sx, sy, sz: TD3DValue) : HResult; stdcall;
    function Translate (tx, ty, tz: TD3DValue) : HResult; stdcall;
    function SetColorSource (source: TD3DRMColorSource) : HResult; stdcall;
    function GetBox (out lpTD3DRMBox: TD3DRMBox) : HResult; stdcall;
    function GenerateNormals : HResult; stdcall;
    function GetColorSource: TD3DRMColorSource; stdcall;
    function AddMesh (lpD3DRMMesh: IDirect3DRMMesh) : HResult; stdcall;
    function AddMeshBuilder (lpD3DRMMeshBuild: IDirect3DRMMeshBuilder) : HResult; stdcall;
    function AddFrame (lpD3DRMFrame: IDirect3DRMFrame) : HResult; stdcall;
    function AddFace (lpD3DRMFace: IDirect3DRMFace) : HResult; stdcall;
    function AddFaces (dwVertexCount: DWORD; const lpD3DVertices: TD3DVector;
        normalCount: DWORD; lpNormals: PD3DVector; var lpFaceData: DWORD;
        lplpD3DRMFaceArray: PIDirect3DRMFaceArray) : HResult; stdcall;
    function ReserveSpace (vertexCount, normalCount, faceCount: DWORD) : HResult; stdcall;
    function SetColorRGB (red, green, blue: TD3DValue) : HResult; stdcall;
    function SetColor (color: TD3DColor) : HResult; stdcall;
    function SetTexture (lpD3DRMTexture: IDirect3DRMTexture) : HResult; stdcall;
    function SetMaterial (lpIDirect3DRMmaterial: IDirect3DRMMaterial) : HResult; stdcall;
    function SetTextureTopology (cylU, cylV: BOOL) : HResult; stdcall;
    function SetQuality (quality: TD3DRMRenderQuality) : HResult; stdcall;
    function SetPerspective (perspective: BOOL) : HResult; stdcall;
    function SetVertex (index: DWORD; x, y, z: TD3DValue) : HResult; stdcall;
    function SetNormal (index: DWORD; x, y, z: TD3DValue) : HResult; stdcall;
    function SetTextureCoordinates (index: DWORD; u, v: TD3DValue) : HResult; stdcall;
    function SetVertexColor (index: DWORD; color: TD3DColor) : HResult; stdcall;
    function SetVertexColorRGB (index: DWORD; red, green, blue: TD3DValue) : HResult; stdcall;
    function GetFaces (out lplpD3DRMFaceArray: IDirect3DRMFaceArray) : HResult; stdcall;
    function GetVertices (var vcount: DWORD; var vertices : TD3DVector;
        var ncount : DWORD;
        var normals : TD3DVector;
        var face_data_size, face_data : DWORD) : HResult; stdcall;
    function GetTextureCoordinates(index : DWORD; out u, v : TD3DValue) : HResult; stdcall;
    function AddVertex (x, y, z: TD3DValue) : Integer; stdcall;
    function AddNormal (x, y, z: TD3DValue) : Integer; stdcall;
    function CreateFace (out lplpd3drmFace: IDirect3DRMFace) : HResult; stdcall;
    function GetQuality: TD3DRMRenderQuality; stdcall;
    function GetPerspective: BOOL; stdcall;
    function GetFaceCount: Integer; stdcall;
    function GetVertexCount: Integer; stdcall;
    function GetVertexColor (index: DWORD) : TD3DColor; stdcall;
    function CreateMesh (out lplpD3DRMMesh: IDirect3DRMMesh) : HResult; stdcall;
  end;

  IDirect3DRMMeshBuilder2 = interface (IDirect3DRMMeshBuilder)
    ['{4516ec77-8f20-11d0-9b6d-0000c0781bc3}']
    (*
     * IDirect3DRMMeshBuilder2 methods
     *)
    function GenerateNormals2 (
        dvCreaseAngle: TD3DValue; dwFlags: DWORD) : HResult; stdcall;
    function GetFace (dwIndex: DWORD; lplpD3DRMFace: IDirect3DRMFace) : HResult; stdcall;
  end;

  IDirect3DRMMeshBuilder3 = interface (IDirect3DRMVisual)
    ['{ff6b7f71-a40e-11d1-91f9-0000f8758e66}']
    (*
     * IDirect3DRMMeshBuilder3 methods
     *)
    function Load (lpvObjSource, lpvObjID: Pointer;
        d3drmLOFlags: TD3DRMLoadOptions;
        d3drmLoadTextureProc: TD3DRMLoadTexture3Callback;
        lpvArg: Pointer) : HResult; stdcall;
    function Save (lpFilename: PAnsiChar; TD3DRMXOFFormat: TD3DRMXOFFormat;
        d3drmSOContents: TD3DRMSaveOptions) : HResult; stdcall;
    function Scale (sx, sy, sz: TD3DValue) : HResult; stdcall;
    function Translate (tx, ty, tz: TD3DValue) : HResult; stdcall;
    function SetColorSource (source: TD3DRMColorSource) : HResult; stdcall;
    function GetBox (out lpTD3DRMBox: TD3DRMBox) : HResult; stdcall;
    function GenerateNormals (
        dvCreaseAngle: TD3DValue; dwFlags: DWORD): HResult; stdcall;
    function GetColorSource: TD3DRMColorSource; stdcall;
    function AddMesh (lpD3DRMMesh: IDirect3DRMMesh) : HResult; stdcall;
    function AddMeshBuilder (
        lpD3DRMMeshBuild: IDirect3DRMMeshBuilder3) : HResult; stdcall;
    function AddFrame (lpD3DRMFrame: IDirect3DRMFrame3) : HResult; stdcall;
    function AddFace (lpD3DRMFace: IDirect3DRMFace2) : HResult; stdcall;
    function AddFaces (dwVertexCount: DWORD; const lpD3DVertices: TD3DVector;
        normalCount: DWORD; lpNormals: PD3DVector; var lpFaceData: DWORD;
        lplpD3DRMFaceArray: PIDirect3DRMFaceArray) : HResult; stdcall;
    function ReserveSpace (vertexCount, normalCount, faceCount: DWORD) : HResult; stdcall;
    function SetColorRGB (red, green, blue: TD3DValue) : HResult; stdcall;
    function SetColor (color: TD3DColor) : HResult; stdcall;
    function SetTexture (lpD3DRMTexture: IDirect3DRMTexture3) : HResult; stdcall;
    function SetMaterial (lpIDirect3DRMmaterial: IDirect3DRMMaterial2) : HResult; stdcall;
    function SetTextureTopology (cylU, cylV: BOOL) : HResult; stdcall;
    function SetQuality (quality: TD3DRMRenderQuality) : HResult; stdcall;
    function SetPerspective (perspective: BOOL) : HResult; stdcall;
    function SetVertex (index: DWORD; x, y, z: TD3DValue) : HResult; stdcall;
    function SetNormal (index: DWORD; x, y, z: TD3DValue) : HResult; stdcall;
    function SetTextureCoordinates (index: DWORD; u, v: TD3DValue) : HResult; stdcall;
    function SetVertexColor (index: DWORD; color: TD3DColor) : HResult; stdcall;
    function SetVertexColorRGB (index: DWORD; red, green, blue: TD3DValue) : HResult; stdcall;
    function GetFaces (out lplpD3DRMFaceArray: IDirect3DRMFaceArray) : HResult; stdcall;
    function GetGeometry (var vcount: DWORD; var vertices : TD3DVector;
        var ncount : DWORD; var normals : TD3DVector;
        var face_data_size, face_data : DWORD) : HResult; stdcall;
    function GetTextureCoordinates(index : DWORD; out u, v : TD3DValue) : HResult; stdcall;
    function AddVertex (x, y, z: TD3DValue) : Integer; stdcall;
    function AddNormal (x, y, z: TD3DValue) : Integer; stdcall;
    function CreateFace (out lplpd3drmFace: IDirect3DRMFace2) : HResult; stdcall;
    function GetQuality: TD3DRMRenderQuality; stdcall;
    function GetPerspective: BOOL; stdcall;
    function GetFaceCount: Integer; stdcall;
    function GetVertexCount: Integer; stdcall;
    function GetVertexColor (index: DWORD) : TD3DColor; stdcall;
    function CreateMesh (out lplpD3DRMMesh: IDirect3DRMMesh) : HResult; stdcall;
    function GetFace
        (dwIndex: DWORD; lplpD3DRMFace: IDirect3DRMFace) : HResult; stdcall;
    function GetVertex (dwIndex: DWORD; out lpVector: TD3DVector) : HResult; stdcall;
    function GetNormal (dwIndex: DWORD; out lpVector: TD3DVector) : HResult; stdcall;
    function DeleteVertices (dwFirstIndex, dwCount: DWORD) : HResult; stdcall;
    function DeleteNormals (dwFirstIndex, dwCount: DWORD) : HResult; stdcall;
    function DeleteFace (lpFace: IDirect3DRMFace2) : HResult; stdcall;
    function Empty (dwFlags: DWORD) : HResult; stdcall;
    function Optimize (dwFlags: DWORD) : HResult; stdcall;
    function AddFacesIndexed (dwFlags: DWORD; var lpdwvIndices: DWORD;
        lpdwIndexFirst, lpdwCount: PDWORD) : HResult; stdcall;
    function CreateSubMesh (out lplpUnk: IUnknown) : HResult; stdcall;
    function GetParentMesh (dwFlags: DWORD; out lplpUnk: IUnknown) : HResult; stdcall;
    function GetSubMeshes (lpdwCount: PDWORD; lpUnk: IUnknown) : HResult; stdcall;
    function DeleteSubMesh (lplpUnk: IUnknown) : HResult; stdcall;
    function Enable (dwFlags: DWORD) : HResult; stdcall;
    function GetEnable (out lpdwFlags: DWORD) : HResult; stdcall;
    function AddTriangles (dwFlags, dwFormat, dwVertexCount:  DWORD;
        lpData: pointer) : HResult; stdcall;
    function SetVertices
        (dwFirst, dwCount: DWORD; const lpdvVector: TD3DVector) : HResult; stdcall;
    function GetVertices(dwFirst: DWORD; var lpdwCount: DWORD;
        lpdvVector: PD3DVector) : HResult; stdcall;
    function SetNormals(dwFirst, dwCount: DWORD; const lpdvVector: TD3DVector) : HResult; stdcall;
    function GetNormals (dwFirst: DWORD; lpdwCount: PDWORD;
        var lpdvVector: TD3DVector) : HResult; stdcall;
    function GetNormalCount : integer; stdcall;
  end;

  IDirect3DRMLight = interface (IDirect3DRMObject)
    ['{eb16cb08-d271-11ce-ac48-0000c03825a1}']
    (*
     * IDirect3DRMLight methods
     *)
    function SetType (d3drmtType: TD3DRMLightType) : HResult; stdcall;
    function SetColor (rcColor: TD3DColor) : HResult; stdcall;
    function SetColorRGB (rvRed, rvGreen, rvBlue: TD3DValue) : HResult; stdcall;
    function SetRange (rvRange: TD3DValue) : HResult; stdcall;
    function SetUmbra (rvAngle: TD3DValue) : HResult; stdcall;
    function SetPenumbra (rvAngle: TD3DValue) : HResult; stdcall;
    function SetConstantAttenuation (rvAtt: TD3DValue) : HResult; stdcall;
    function SetLinearAttenuation (rvAtt: TD3DValue) : HResult; stdcall;
    function SetQuadraticAttenuation (rvAtt: TD3DValue) : HResult; stdcall;
    function GetRange: TD3DValue; stdcall;
    function GetUmbra: TD3DValue; stdcall;
    function GetPenumbra: TD3DValue; stdcall;
    function GetConstantAttenuation: TD3DValue; stdcall;
    function GetLinearAttenuation: TD3DValue; stdcall;
    function GetQuadraticAttenuation: TD3DValue; stdcall;
    function GetColor: TD3DColor; stdcall;
    function GetType: TD3DRMLightType; stdcall;
    function SetEnableFrame (lpEnableFrame: IDirect3DRMFrame) : HResult; stdcall;
    function GetEnableFrame (out lplpEnableFrame: IDirect3DRMFrame) : HResult; stdcall;
  end;

  IDirect3DRMTexture = interface (IDirect3DRMVisual)
    ['{eb16cb09-d271-11ce-ac48-0000c03825a1}']
    (*
     * IDirect3DRMTexture methods
     *)
    function InitFromFile (filename: PAnsiChar) : HResult; stdcall;
    function InitFromSurface (lpDDS: IDirectDrawSurface) : HResult; stdcall;
    function InitFromResource (rs: HRSRC) : HResult; stdcall;
    function Changed (bPixels, bPalette: BOOL) : HResult; stdcall;
    function SetColors (ulColors: DWORD) : HResult; stdcall;
    function SetShades (ulShades: DWORD) : HResult; stdcall;
    function SetDecalSize (rvWidth, rvHeight: TD3DValue) : HResult; stdcall;
    function SetDecalOrigin (lX, lY: LongInt) : HResult; stdcall;
    function SetDecalScale (dwScale: DWORD) : HResult; stdcall;
    function SetDecalTransparency (bTransp: BOOL) : HResult; stdcall;
    function SetDecalTransparentColor (rcTransp: TD3DColor) : HResult; stdcall;
    function GetDecalSize (out lprvWidth, lprvHeight: TD3DValue) : HResult; stdcall;
    function GetDecalOrigin (out lplX, lplY: LongInt) : HResult; stdcall;
    function GetImage: PD3DRMImage; stdcall;
    function GetShades: DWORD; stdcall;
    function GetColors: DWORD; stdcall;
    function GetDecalScale: DWORD; stdcall;
    function GetDecalTransparency: BOOL; stdcall;
    function GetDecalTransparentColor: TD3DColor; stdcall;
  end;

  IDirect3DRMTexture2 = interface (IDirect3DRMTexture)
    ['{120f30c0-1629-11d0-941c-0080c80cfa7b}']
    (*
     * IDirect3DRMTexture2 methods
     *)
    function InitFromImage (const lpImage: TD3DRMImage) : HResult; stdcall;
    function InitFromResource2 (hModule: HModule;
        strName, strType: PAnsiChar) : HResult; stdcall;
    function GenerateMIPMap (dwFlags: DWORD) : HResult; stdcall;
  end;

  IDirect3DRMTexture3 = interface (IDirect3DRMTexture2)
    ['{ff6b7f73-a40e-11d1-91f9-0000f8758e66}']
    (*
     * IDirect3DRMTexture3 methods
     *)
    function GetSurface
        (dwFlags: DWORD; out lplpDDS: IDirectDrawSurface) : HResult; stdcall;
    function SetCacheOptions (lImportance: integer; dwFlags: DWORD) : HResult; stdcall;
    function GetCacheOptions (var lplImportance: integer; var lpdwFlags: DWORD) : HResult; stdcall;
    function SetDownsampleCallback (
        pCallback: TD3DRMDownSampleCallback; pArg: pointer) : HResult; stdcall;
    function SetValidationCallback (
        pCallback: TD3DRMValidationCallback; pArg: pointer) : HResult; stdcall;
  end;

  IDirect3DRMWrap = interface (IDirect3DRMObject)
    ['{eb16cb0a-d271-11ce-ac48-0000c03825a1}']
    (*
     * IDirect3DRMWrap methods
     *)
    function Init (d3drmwt: TD3DRMWrapType; lpd3drmfRef: IDirect3DRMFrame;
        ox, oy, oz, dx, dy, dz, ux, uy, uz, ou, ov, su, sv: TD3DValue)
        : HResult; stdcall;
    function Apply (lpObject: IDirect3DRMObject) : HResult; stdcall;
    function ApplyRelative(frame: IDirect3DRMFrame; mesh: IDirect3DRMObject) : HResult; stdcall;
  end;

  IDirect3DRMMaterial = interface (IDirect3DRMObject)
    ['{eb16cb0b-d271-11ce-ac48-0000c03825a1}']
    (*
     * IDirect3DRMMaterial methods
     *)
    function SetPower (rvPower: TD3DValue) : HResult; stdcall;
    function SetSpecular (r, g, b: TD3DValue) : HResult; stdcall;
    function SetEmissive (r, g, b: TD3DValue) : HResult; stdcall;
    function GetPower: TD3DValue; stdcall;
    function GetSpecular (out lpr, lpg, lpb: TD3DValue) : HResult; stdcall;
    function GetEmissive (out lpr, lpg, lpb: TD3DValue) : HResult; stdcall;
  end;

  IDirect3DRMMaterial2 = interface (IDirect3DRMMaterial)
    ['{ff6b7f75-a40e-11d1-91f9-0000f8758e66}']
    (*
     * IDirect3DRMMaterial2 methods
     *)
    function GetAmbient(out r,g,b: TD3DValue) : HResult; stdcall;
    function SetAmbient(r,g,b: TD3DValue) : HResult; stdcall;
  end;

  IDirect3DRMAnimation = interface (IDirect3DRMObject)
    ['{eb16cb0d-d271-11ce-ac48-0000c03825a1}']
    (*
     * IDirect3DRMAnimation methods
     *)
    function SetOptions (d3drmanimFlags: TD3DRMAnimationOptions) : HResult; stdcall;
    function AddRotateKey (rvTime: TD3DValue; const rqQuat: TD3DRMQuaternion) : HResult; stdcall;
    function AddPositionKey (rvTime, rvX, rvY, rvZ: TD3DValue) : HResult; stdcall;
    function AddScaleKey (time, x, y, z: TD3DValue) : HResult; stdcall;
    function DeleteKey (time: TD3DValue) : HResult; stdcall;
    function SetFrame (lpD3DRMFrame: IDirect3DRMFrame) : HResult; stdcall;
    function SetTime (rvTime: TD3DValue) : HResult; stdcall;
    function GetOptions: TD3DRMAnimationOptions; stdcall;
  end;

  IDirect3DRMAnimation2 = interface (IDirect3DRMAnimation)
    ['{ff6b7f77-a40e-11d1-91f9-0000f8758e66}']
    (*
     * IDirect3DRMAnimation methods
     *)
    function GetFrame (out lpD3DFrame: IDirect3DRMFrame3) : HResult; stdcall;
    function DeleteKeyByID (dwID: DWORD) : HResult; stdcall;
    function AddKey (const lpKey: TD3DRMAnimationKey) : HResult; stdcall;
    function ModifyKey (const lpKey: TD3DRMAnimationKey) : HResult; stdcall;
    function GetKeys (dvTimeMin, dvTimeMax: TD3DValue; var lpdwNumKeys: DWORD;
        lpKey: PD3DRMAnimationKey) : HResult; stdcall;
  end;

  IDirect3DRMAnimationSet = interface (IDirect3DRMObject)
    ['{eb16cb0e-d271-11ce-ac48-0000c03825a1}']
    (*
     * IDirect3DRMAnimationSet methods
     *)
    function AddAnimation (lpD3DRMAnimation: IDirect3DRMAnimation) : HResult; stdcall;
    function Load (lpvObjSource, lpvObjID: Pointer;
        d3drmLOFlags: TD3DRMLoadOptions;
        d3drmLoadTextureProc: TD3DRMLoadTextureCallback; lpArgLTP: Pointer;
        lpParentFrame: IDirect3DRMFrame) : HResult; stdcall;
    function DeleteAnimation (lpD3DRMAnimation: IDirect3DRMAnimation) : HResult; stdcall;
    function SetTime (rvTime: TD3DValue) : HResult; stdcall;
  end;

  IDirect3DRMAnimationSet2 = interface (IDirect3DRMObject)
    ['{ff6b7f79-a40e-11d1-91f9-0000f8758e66}']
    (*
     * IDirect3DRMAnimationSet methods
     *)
    function AddAnimation (lpD3DRMAnimation: IDirect3DRMAnimation2) : HResult; stdcall;
    function Load (lpvObjSource, lpvObjID: Pointer;
        d3drmLOFlags: TD3DRMLoadOptions;
        d3drmLoadTextureProc: TD3DRMLoadTexture3Callback; lpArgLTP: Pointer;
        lpParentFrame: IDirect3DRMFrame3) : HResult; stdcall;
    function DeleteAnimation (lpD3DRMAnimation: IDirect3DRMAnimation2) : HResult; stdcall;
    function SetTime (rvTime: TD3DValue) : HResult; stdcall;
    function GetAnimations(out lplpArray: IDirect3DRMAnimationArray) : HResult; stdcall;
  end;

  IDirect3DRMUserVisual = interface (IDirect3DRMVisual)
    ['{59163de0-6d43-11cf-ac4a-0000c03825a1}']
    (*
     * IDirect3DRMUserVisual methods
     *)
    function Init (d3drmUVProc: TD3DRMUserVisualCallback;
        lpArg: Pointer) : HResult; stdcall;
  end;

  IDirect3DRMArray = interface (IUnknown)
    function GetSize: DWORD; stdcall;
    (* No GetElement method as it would get overloaded
     * in derived classes, and overloading is
     * a no-no in COM
     *)
  end;

  IDirect3DRMObjectArray = interface (IDirect3DRMArray)
  	['{242f6bc2-3849-11d0-9b6d-0000c0781bc3}']
    function GetElement (index: DWORD; out lplpD3DRMObject:
        IDirect3DRMObject) : HResult; stdcall;
  end;

  IDirect3DRMDeviceArray = interface (IDirect3DRMArray)
    ['{eb16cb0e-d271-11ce-ac48-0000c03825a1}']
    function GetElement (index: DWORD; out lplpD3DRMDevice:
        IDirect3DRMDevice) : HResult; stdcall;
  end;

  IDirect3DRMFrameArray = interface (IDirect3DRMArray)
    ['{eb16cb12-d271-11ce-ac48-0000c03825a1}']
    function GetElement (index: DWORD; out lplpD3DRMFrame: IDirect3DRMFrame) : HResult; stdcall;
  end;

  IDirect3DRMViewportArray = interface (IDirect3DRMArray)
    ['{eb16cb11-d271-11ce-ac48-0000c03825a1}']
    function GetElement (index: DWORD; out lplpD3DRMViewport:
        IDirect3DRMViewport) : HResult; stdcall;
  end;

  IDirect3DRMVisualArray = interface (IDirect3DRMArray)
    ['{eb16cb13-d271-11ce-ac48-0000c03825a1}']
    function GetElement (index: DWORD; out lplpD3DRMVisual:
        IDirect3DRMVisual) : HResult; stdcall;
  end;

  IDirect3DRMAnimationArray = interface (IDirect3DRMArray)
    ['{d5f1cae0-4bd7-11d1-b974-0060083e45f3}']
    function GetElement (index: DWORD; out lplpD3DRMAnimation2:
        IDirect3DRMAnimation2) : HResult; stdcall;
  end;

  IDirect3DRMPickedArray = interface (IDirect3DRMArray)
    ['{eb16cb16-d271-11ce-ac48-0000c03825a1}']
    function GetPick (index: DWORD; out lplpVisual: IDirect3DRMVisual;
        out lplpFrameArray: IDirect3DRMFrameArray;
        const lpD3DRMPickDesc: TD3DRMPickDesc) : HResult; stdcall;

  end;

  IDirect3DRMLightArray = interface (IDirect3DRMArray)
    ['{eb16cb14-d271-11ce-ac48-0000c03825a1}']
    function GetElement (index: DWORD; out lplpD3DRMLight: IDirect3DRMLight) : HResult; stdcall;
  end;


  IDirect3DRMFaceArray = interface (IDirect3DRMArray)
    ['{eb16cb17-d271-11ce-ac48-0000c03825a1}']
    function GetElement (index: DWORD; out lplpD3DRMFace: IDirect3DRMFace) : HResult; stdcall;
  end;

  IDirect3DRMPicked2Array = interface (IDirect3DRMArray)
    ['{4516ec7b-8f20-11d0-9b6d-0000c0781bc3}']
    function GetPick (index: DWORD; out lplpVisual: IDirect3DRMVisual;
        out lplpFrameArray: IDirect3DRMFrameArray; const lpD3DRMPickDesc2:
      	TD3DRMPickDesc2) : HResult; stdcall;
  end;

  IDirect3DRMInterpolator = interface (IDirect3DRMObject)
    ['{242f6bc1-3849-11d0-9b6d-0000c0781bc3}']
    (*
     * IDirect3DRMInterpolator methods
     *)
    function AttachObject (lpD3DRMObject: IDirect3DRMObject) : HResult; stdcall;
    function GetAttachedObjects
        (lpD3DRMObjectArray: IDirect3DRMObjectArray) : HResult; stdcall;
    function DetachObject (lpD3DRMObject: IDirect3DRMObject) : HResult; stdcall;
    function SetIndex (d3dVal: TD3DValue) : HResult; stdcall;
    function GetIndex : TD3DValue; stdcall;
    function Interpolate (d3dVal: TD3DValue; lpD3DRMObject: IDirect3DRMObject;
      	d3drmInterpFlags: TD3DRMInterpolationOptions) : HResult; stdcall;
  end;

  IDirect3DRMClippedVisual = interface (IDirect3DRMObject)
    ['{5434e733-6d66-11d1-bb0b-0000f875865a}']
    (*
     * IDirect3DRMClippedVisual methods
     *)
    function Init (lpD3DRMVisual: IDirect3DRMVisual) : HResult; stdcall;
    function AddPlane (lpRef: IDirect3DRMFrame3;
        const lpdvPoint, lpdvNormal: TD3DVector;
        dwFlags: DWORD; out lpdwReturnID: DWORD) : HResult; stdcall;
    function DeletePlane (dwID, dwFlags: DWORD) : HResult; stdcall;
    function GetPlaneIDs (var lpdwCount: DWORD; out lpdwID: DWORD; dwFlags: DWORD) : HResult; stdcall;
    function GetPlane (dwID: DWORD; lpRef: IDirect3DRMFrame3;
        out lpdvPoint, lpdvNormal: TD3DVector; dwFlags: DWORD) : HResult; stdcall;
    function SetPlane (dwID: DWORD; lpRef: IDirect3DRMFrame3;
        const lpdvPoint, lpdvNormal: TD3DVector; dwFlags: DWORD) : HResult; stdcall;
  end;

(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3drm.h
 *  Content:    Direct3DRM include file
 *
 ***************************************************************************)

function D3DRMErrorString(Value: HResult) : string;

type
  TRefClsID = TGUID;

type
  TD3DRMDevicePaletteCallback = procedure (lpDirect3DRMDev: IDirect3DRMDevice;
      lpArg: Pointer; dwIndex: DWORD; red, green, blue: LongInt); cdecl;

(*
 * Direct3DRM Object Class (for CoCreateInstance())
 *)
const
  CLSID_CDirect3DRM: TGUID =
      (D1:$4516ec41;D2:$8f20;D3:$11d0;D4:($9b,$6d,$00,$00,$c0,$78,$1b,$c3));

type
  IDirect3DRM = interface (IUnknown)
    ['{2bc49361-8327-11cf-ac4a-0000c03825a1}']
    function CreateObject (const rclsid: TRefClsID; pUnkOuter: IUnknown;
        const riid: TGUID; out ppv) : HResult; stdcall;
    function CreateFrame (lpD3DRMFrame: IDirect3DRMFrame;
        var lplpD3DRMFrame: IDirect3DRMFrame) : HResult; stdcall;
    function CreateMesh (var lplpD3DRMMesh: IDirect3DRMMesh) : HResult; stdcall;
    function CreateMeshBuilder (var lplpD3DRMMeshBuilder:
        IDirect3DRMMeshBuilder) : HResult; stdcall;
    function CreateFace (var lplpd3drmFace: IDirect3DRMFace) : HResult; stdcall;
    function CreateAnimation (var lplpD3DRMAnimation: IDirect3DRMAnimation) : HResult; stdcall;
    function CreateAnimationSet (var lplpD3DRMAnimationSet:
        IDirect3DRMAnimationSet) : HResult; stdcall;
    function CreateTexture (var lpImage: TD3DRMImage;
        var lplpD3DRMTexture: IDirect3DRMTexture) : HResult; stdcall;
    function CreateLight (d3drmltLightType: TD3DRMLightType;
        cColor: TD3DColor; var lplpD3DRMLight: IDirect3DRMLight) : HResult; stdcall;
    function CreateLightRGB (ltLightType: TD3DRMLightType; vRed,
        vGreen, vBlue: TD3DValue; var lplpD3DRMLight: IDirect3DRMLight) : HResult; stdcall;
    function CreateMaterial (vPower: TD3DValue; var lplpD3DRMMaterial:
        IDirect3DRMMaterial) : HResult; stdcall;
    function CreateDevice (dwWidth, dwHeight: DWORD; var lplpD3DRMDevice:
        IDirect3DRMDevice) : HResult; stdcall;

    (* Create a Windows Device using DirectDraw surfaces *)
    function CreateDeviceFromSurface (lpGUID: PGUID; lpDD: IDirectDraw;
        lpDDSBack: IDirectDrawSurface; var lplpD3DRMDevice: IDirect3DRMDevice) :
        HResult; stdcall;

      (* Create a Windows Device using D3D objects *)
    function CreateDeviceFromD3D (lpD3D: IDirect3D; lpD3DDev: IDirect3DDevice;
        var lplpD3DRMDevice: IDirect3DRMDevice) : HResult; stdcall;

    function CreateDeviceFromClipper (lpDDClipper: IDirectDrawClipper;
        lpGUID: PGUID; width, height: Integer; var lplpD3DRMDevice:
        IDirect3DRMDevice) : HResult; stdcall;

    function CreateTextureFromSurface ( lpDDS: IDirectDrawSurface;
        var lplpD3DRMTexture: IDirect3DRMTexture) : HResult; stdcall;

    function CreateShadow (lpVisual: IDirect3DRMVisual;
        lpLight: IDirect3DRMLight; px, py, pz, nx, ny, nz: TD3DValue;
        var lplpShadow: IDirect3DRMVisual) : HResult; stdcall;
    function CreateViewport (lpDev: IDirect3DRMDevice;
        lpCamera: IDirect3DRMFrame; dwXPos, dwYPos, dwWidth, dwHeight: DWORD;
        var lplpD3DRMViewport: IDirect3DRMViewport) : HResult; stdcall;
    function CreateWrap (wraptype: TD3DRMWrapType; lpRef: IDirect3DRMFrame;
        ox, oy, oz, dx, dy, dz, ux, uy, uz, ou, ov, su, sv: TD3DValue;
        var lplpD3DRMWrap: IDirect3DRMWrap) : HResult; stdcall;
    function CreateUserVisual (fn: TD3DRMUserVisualCallback; lpArg: Pointer;
        var lplpD3DRMUV: IDirect3DRMUserVisual) : HResult; stdcall;
    function LoadTexture (lpFileName: PAnsiChar; var lplpD3DRMTexture:
        IDirect3DRMTexture) : HResult; stdcall;
    function LoadTextureFromResource (rs: HRSRC; var lplpD3DRMTexture:
        IDirect3DRMTexture) : HResult; stdcall;

    function SetSearchPath (lpPath: PAnsiChar) : HResult; stdcall;
    function AddSearchPath (lpPath: PAnsiChar) : HResult; stdcall;
    function GetSearchPath (var lpdwSize: DWORD; lpszPath: PAnsiChar) : HResult; stdcall;
    function SetDefaultTextureColors (dwColors: DWORD) : HResult; stdcall;
    function SetDefaultTextureShades (dwShades: DWORD) : HResult; stdcall;

    function GetDevices (var lplpDevArray: IDirect3DRMDeviceArray) : HResult; stdcall;
    function GetNamedObject (lpName: PAnsiChar; var lplpD3DRMObject: IDirect3DRMObject) : HResult; stdcall;

    function EnumerateObjects (func: TD3DRMObjectCallback; lpArg: Pointer) : HResult; stdcall;

    function Load (lpvObjSource, lpvObjID: Pointer; var lplpGUIDs: PGUID;
        dwcGUIDs: DWORD; d3drmLOFlags: TD3DRMLoadOptions; d3drmLoadProc:
        TD3DRMLoadCallback; lpArgLP: Pointer; d3drmLoadTextureProc:
        TD3DRMLoadTextureCallback; lpArgLTP: Pointer; lpParentFrame:
        IDirect3DRMFrame) : HResult; stdcall;
    function Tick (d3dvalTick: TD3DValue) : HResult; stdcall;
  end;

// Moved from D3DRMObj, to avoid circular unit reference:

  IDirect3DRMObject2 = interface (IUnknown)
    ['{4516ec7c-8f20-11d0-9b6d-0000c0781bc3}']
    (*
     * IDirect3DRMObject2 methods
     *)
    function AddDestroyCallback (lpCallback: TD3DRMObjectCallback;
        lpArg: Pointer) : HResult; stdcall;
    function Clone (pUnkOuter: IUnknown; const riid: TGUID;
        out ppvObj) : HResult; stdcall;
    function DeleteDestroyCallback (d3drmObjProc: TD3DRMObjectCallback;
        lpArg: Pointer) : HResult; stdcall;
    function GetClientData (dwID: DWORD; out lplpvData: Pointer) : HResult; stdcall;
    function GetDirect3DRM (out lplpDirect3DRM: IDirect3DRM) : HResult; stdcall;
    function GetName (var lpdwSize: DWORD; lpName: PAnsiChar) : HResult; stdcall;
    function SetClientData (dwID: DWORD; lpvData: pointer; dwFlags: DWORD) : HResult; stdcall;
    function SetName (lpName: PAnsiChar) : HResult; stdcall;
    function GetAge (dwFlags: DWORD; out pdwAge: DWORD) : HResult; stdcall;
  end;

  IID_IDirect3DRMObject2 = IDirect3DRMObject2;

  IDirect3DRM2 = interface (IUnknown)
    ['{4516ecc8-8f20-11d0-9b6d-0000c0781bc3}']
    function CreateObject (const rclsid: TRefClsID; pUnkOuter: IUnknown;
        const riid: TGUID; out ppv) : HResult; stdcall;
    function CreateFrame (lpD3DRMFrame: IDirect3DRMFrame2;
        var lplpD3DRMFrame: IDirect3DRMFrame2) : HResult; stdcall;
    function CreateMesh (var lplpD3DRMMesh: IDirect3DRMMesh) : HResult; stdcall;
    function CreateMeshBuilder (var lplpD3DRMMeshBuilder:
        IDirect3DRMMeshBuilder2) : HResult; stdcall;
    function CreateFace (var lplpd3drmFace: IDirect3DRMFace) : HResult; stdcall;
    function CreateAnimation (var lplpD3DRMAnimation: IDirect3DRMAnimation) : HResult; stdcall;
    function CreateAnimationSet (var lplpD3DRMAnimationSet:
        IDirect3DRMAnimationSet) : HResult; stdcall;
    function CreateTexture (var lpImage: TD3DRMImage;
        var lplpD3DRMTexture: IDirect3DRMTexture2) : HResult; stdcall;
    function CreateLight (d3drmltLightType: TD3DRMLightType;
        cColor: TD3DColor; var lplpD3DRMLight: IDirect3DRMLight) : HResult; stdcall;
    function CreateLightRGB (ltLightType: TD3DRMLightType; vRed,
        vGreen, vBlue: TD3DValue; var lplpD3DRMLight: IDirect3DRMLight) : HResult; stdcall;
    function CreateMaterial (vPower: TD3DValue; var lplpD3DRMMaterial:
        IDirect3DRMMaterial) : HResult; stdcall;
    function CreateDevice (dwWidth, dwHeight: DWORD; var lplpD3DRMDevice:
        IDirect3DRMDevice2) : HResult; stdcall;

    (* Create a Windows Device using DirectDraw surfaces *)
    function CreateDeviceFromSurface (lpGUID: PGUID; lpDD: IDirectDraw;
        lpDDSBack: IDirectDrawSurface; var lplpD3DRMDevice: IDirect3DRMDevice2) :
        HResult; stdcall;

      (* Create a Windows Device using D3D objects *)
    function CreateDeviceFromD3D (lpD3D: IDirect3D2; lpD3DDev: IDirect3DDevice2;
        var lplpD3DRMDevice: IDirect3DRMDevice2) : HResult; stdcall;

    function CreateDeviceFromClipper (lpDDClipper: IDirectDrawClipper;
        lpGUID: PGUID; width, height: Integer; var lplpD3DRMDevice:
        IDirect3DRMDevice2) : HResult; stdcall;

    function CreateTextureFromSurface ( lpDDS: IDirectDrawSurface;
        var lplpD3DRMTexture: IDirect3DRMTexture2) : HResult; stdcall;

    function CreateShadow (lpVisual: IDirect3DRMVisual;
        lpLight: IDirect3DRMLight; px, py, pz, nx, ny, nz: TD3DValue;
        var lplpShadow: IDirect3DRMVisual) : HResult; stdcall;
    function CreateViewport (lpDev: IDirect3DRMDevice;
        lpCamera: IDirect3DRMFrame; dwXPos, dwYPos, dwWidth, dwHeight: DWORD;
        var lplpD3DRMViewport: IDirect3DRMViewport) : HResult; stdcall;
    function CreateWrap (wraptype: TD3DRMWrapType; lpRef: IDirect3DRMFrame;
        ox, oy, oz, dx, dy, dz, ux, uy, uz, ou, ov, su, sv: TD3DValue;
        var lplpD3DRMWrap: IDirect3DRMWrap) : HResult; stdcall;
    function CreateUserVisual (fn: TD3DRMUserVisualCallback; lpArg: Pointer;
        var lplpD3DRMUV: IDirect3DRMUserVisual) : HResult; stdcall;
    function LoadTexture (lpFileName: PAnsiChar; var lplpD3DRMTexture:
        IDirect3DRMTexture2) : HResult; stdcall;
    function LoadTextureFromResource (rs: HRSRC; var lplpD3DRMTexture:
        IDirect3DRMTexture2) : HResult; stdcall;

    function SetSearchPath (lpPath: PAnsiChar) : HResult; stdcall;
    function AddSearchPath (lpPath: PAnsiChar) : HResult; stdcall;
    function GetSearchPath (var lpdwSize: DWORD; lpszPath: PAnsiChar) : HResult; stdcall;
    function SetDefaultTextureColors (dwColors: DWORD) : HResult; stdcall;
    function SetDefaultTextureShades (dwShades: DWORD) : HResult; stdcall;

    function GetDevices (var lplpDevArray: IDirect3DRMDeviceArray) : HResult; stdcall;
    function GetNamedObject (lpName: PAnsiChar; var lplpD3DRMObject:
        IDirect3DRMObject) : HResult; stdcall;

    function EnumerateObjects (func: TD3DRMObjectCallback; lpArg: Pointer) : HResult; stdcall;

    function Load (lpvObjSource, lpvObjID: Pointer; var lplpGUIDs: PGUID;
        dwcGUIDs: DWORD; d3drmLOFlags: TD3DRMLoadOptions; d3drmLoadProc:
        TD3DRMLoadCallback; lpArgLP: Pointer; d3drmLoadTextureProc:
        TD3DRMLoadTextureCallback; lpArgLTP: Pointer; lpParentFrame:
        IDirect3DRMFrame) : HResult; stdcall;
    function Tick (d3dvalTick: TD3DValue) : HResult; stdcall;
    function CreateProgressiveMesh (var lplpD3DRMProgressiveMesh:
        IDirect3DRMProgressiveMesh) : HResult; stdcall;
  end;

  IDirect3DRM3 = interface (IUnknown)
    ['{4516ec83-8f20-11d0-9b6d-0000c0781bc3}']
    function CreateObject (const rclsid: TRefClsID; pUnkOuter: IUnknown;
        const riid: TGUID; out ppv) : HResult; stdcall;
    function CreateFrame (lpD3DRMFrame: IDirect3DRMFrame3;
        out lplpD3DRMFrame: IDirect3DRMFrame3) : HResult; stdcall;
    function CreateMesh (out lplpD3DRMMesh: IDirect3DRMMesh) : HResult; stdcall;
    function CreateMeshBuilder (out lplpD3DRMMeshBuilder:
        IDirect3DRMMeshBuilder3) : HResult; stdcall;
    function CreateFace (out lplpd3drmFace: IDirect3DRMFace2) : HResult; stdcall;
    function CreateAnimation (out lplpD3DRMAnimation: IDirect3DRMAnimation2) : HResult; stdcall;
    function CreateAnimationSet (out lplpD3DRMAnimationSet:
        IDirect3DRMAnimationSet2) : HResult; stdcall;
    function CreateTexture (const lpImage: TD3DRMImage;
        out lplpD3DRMTexture: IDirect3DRMTexture3) : HResult; stdcall;
    function CreateLight (d3drmltLightType: TD3DRMLightType;
        cColor: TD3DColor; out lplpD3DRMLight: IDirect3DRMLight) : HResult; stdcall;
    function CreateLightRGB (ltLightType: TD3DRMLightType; vRed,
        vGreen, vBlue: TD3DValue; out lplpD3DRMLight: IDirect3DRMLight) : HResult; stdcall;
    function CreateMaterial (vPower: TD3DValue; out lplpD3DRMMaterial:
        IDirect3DRMMaterial2) : HResult; stdcall;
    function CreateDevice (dwWidth, dwHeight: DWORD; out lplpD3DRMDevice:
        IDirect3DRMDevice3) : HResult; stdcall;

    (* Create a Windows Device using DirectDraw surfaces *)
    function CreateDeviceFromSurface (lpGUID: PGUID; lpDD: IDirectDraw;
        lpDDSBack: IDirectDrawSurface; dwFlags: DWORD;
        out lplpD3DRMDevice: IDirect3DRMDevice3) : HResult; stdcall;

      (* Create a Windows Device using D3D objects *)
    function CreateDeviceFromD3D (lpD3D: IDirect3D2; lpD3DDev: IDirect3DDevice2;
        out lplpD3DRMDevice: IDirect3DRMDevice3) : HResult; stdcall;

    function CreateDeviceFromClipper (lpDDClipper: IDirectDrawClipper;
        lpGUID: PGUID; width, height: Integer;
        out lplpD3DRMDevice: IDirect3DRMDevice3) : HResult; stdcall;

    function CreateTextureFromSurface ( lpDDS: IDirectDrawSurface;
        out lplpD3DRMTexture: IDirect3DRMTexture3) : HResult; stdcall;

    function CreateShadow (pUnk: IUnknown; lpLight: IDirect3DRMLight;
        px, py, pz, nx, ny, nz: TD3DValue;
        out lplpShadow: IDirect3DRMShadow2) : HResult; stdcall;
    function CreateViewport (lpDev: IDirect3DRMDevice3;
        lpCamera: IDirect3DRMFrame3; dwXPos, dwYPos, dwWidth, dwHeight: DWORD;
        out lplpD3DRMViewport: IDirect3DRMViewport2) : HResult; stdcall;
    function CreateWrap (wraptype: TD3DRMWrapType; lpRef: IDirect3DRMFrame3;
        ox, oy, oz, dx, dy, dz, ux, uy, uz, ou, ov, su, sv: TD3DValue;
        out lplpD3DRMWrap: IDirect3DRMWrap) : HResult; stdcall;
    function CreateUserVisual (fn: TD3DRMUserVisualCallback; lpArg: Pointer;
        out lplpD3DRMUV: IDirect3DRMUserVisual) : HResult; stdcall;
    function LoadTexture (lpFileName: PAnsiChar; out lplpD3DRMTexture:
        IDirect3DRMTexture3) : HResult; stdcall;
    function LoadTextureFromResource (hModule: HMODULE;
        strName, strType: PAnsiChar;
        out lplpD3DRMTexture: IDirect3DRMTexture3) : HResult; stdcall;

    function SetSearchPath (lpPath: PAnsiChar) : HResult; stdcall;
    function AddSearchPath (lpPath: PAnsiChar) : HResult; stdcall;
    function GetSearchPath (var lpdwSize: DWORD; lpszPath: PAnsiChar) : HResult; stdcall;
    function SetDefaultTextureColors (dwColors: DWORD) : HResult; stdcall;
    function SetDefaultTextureShades (dwShades: DWORD) : HResult; stdcall;

    function GetDevices (out lplpDevArray: IDirect3DRMDeviceArray) : HResult; stdcall;
    function GetNamedObject (lpName: PAnsiChar; out lplpD3DRMObject: IDirect3DRMObject) : HResult; stdcall;

    function EnumerateObjects (func: TD3DRMObjectCallback; lpArg: Pointer) : HResult; stdcall;

    function Load (lpvObjSource, lpvObjID: Pointer; var lplpGUIDs: PGUID;
        dwcGUIDs: DWORD; d3drmLOFlags: TD3DRMLoadOptions; d3drmLoadProc:
        TD3DRMLoadCallback; lpArgLP: Pointer; d3drmLoadTextureProc:
        TD3DRMLoadTexture3Callback; lpArgLTP: Pointer; lpParentFrame:
        IDirect3DRMFrame3) : HResult; stdcall;
    function Tick (d3dvalTick: TD3DValue) : HResult; stdcall;
    function CreateProgressiveMesh (out lplpD3DRMProgressiveMesh:
        IDirect3DRMProgressiveMesh) : HResult; stdcall;

    (* Used with IDirect3DRMObject2 *)
    function RegisterClient (const rguid: TGUID; out lpdwID: DWORD) : HResult; stdcall;
    function UnregisterClient (const rguid: TGUID) : HResult; stdcall;

    function CreateClippedVisual (lpVisual: IDirect3DRMVisual;
        lpClippedVisual: IDirect3DRMClippedVisual) : HResult; stdcall;
    function SetOptions (dwOptions: DWORD) : HResult; stdcall;
    function GetOptions (out lpdwOptions: DWORD) : HResult; stdcall;
  end;

  IID_IDirect3DRM =  IDirect3DRM;
  IID_IDirect3DRM2 = IDirect3DRM2;
  IID_IDirect3DRM3 = IDirect3DRM3;

const
  MAKE_DDHRESULT = HResult($88760000);

  D3DRM_OK                        = DD_OK;
  D3DRMERR_BADOBJECT              = MAKE_DDHRESULT + 781;
  D3DRMERR_BADTYPE                = MAKE_DDHRESULT + 782;
  D3DRMERR_BADALLOC               = MAKE_DDHRESULT + 783;
  D3DRMERR_FACEUSED               = MAKE_DDHRESULT + 784;
  D3DRMERR_NOTFOUND               = MAKE_DDHRESULT + 785;
  D3DRMERR_NOTDONEYET             = MAKE_DDHRESULT + 786;
  D3DRMERR_FILENOTFOUND           = MAKE_DDHRESULT + 787;
  D3DRMERR_BADFILE                = MAKE_DDHRESULT + 788;
  D3DRMERR_BADDEVICE              = MAKE_DDHRESULT + 789;
  D3DRMERR_BADVALUE               = MAKE_DDHRESULT + 790;
  D3DRMERR_BADMAJORVERSION        = MAKE_DDHRESULT + 791;
  D3DRMERR_BADMINORVERSION        = MAKE_DDHRESULT + 792;
  D3DRMERR_UNABLETOEXECUTE        = MAKE_DDHRESULT + 793;
  D3DRMERR_LIBRARYNOTFOUND        = MAKE_DDHRESULT + 794;
  D3DRMERR_INVALIDLIBRARY         = MAKE_DDHRESULT + 795;
  D3DRMERR_PENDING                = MAKE_DDHRESULT + 796;
  D3DRMERR_NOTENOUGHDATA          = MAKE_DDHRESULT + 797;
  D3DRMERR_REQUESTTOOLARGE        = MAKE_DDHRESULT + 798;
  D3DRMERR_REQUESTTOOSMALL        = MAKE_DDHRESULT + 799;
  D3DRMERR_CONNECTIONLOST         = MAKE_DDHRESULT + 800;
  D3DRMERR_LOADABORTED            = MAKE_DDHRESULT + 801;
  D3DRMERR_NOINTERNET             = MAKE_DDHRESULT + 802;
  D3DRMERR_BADCACHEFILE           = MAKE_DDHRESULT + 803;
  D3DRMERR_BOXNOTSET	          = MAKE_DDHRESULT + 804;
  D3DRMERR_BADPMDATA              = MAKE_DDHRESULT + 805;
  D3DRMERR_CLIENTNOTREGISTERED    = MAKE_DDHRESULT + 806;
  D3DRMERR_NOTCREATEDFROMDDS      = MAKE_DDHRESULT + 807;
  D3DRMERR_NOSUCHKEY              = MAKE_DDHRESULT + 808;
  D3DRMERR_INCOMPATABLEKEY        = MAKE_DDHRESULT + 809;
  D3DRMERR_ELEMENTINUSE           = MAKE_DDHRESULT + 810;
  D3DRMERR_TEXTUREFORMATNOTFOUND  = MAKE_DDHRESULT + 811;

(* Create a Direct3DRM API *)
var
  Direct3DRMCreate : function (out lplpDirect3DRM: IDirect3DRM) : HResult; stdcall;

(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3drmwin.h
 *  Content:    Direct3DRM include file
 *
 ***************************************************************************)

type
  IDirect3DRMWinDevice = interface (IDirect3DRMObject)
    ['{c5016cc0-d273-11ce-ac48-0000c03825a1}']
    (*
     * IDirect3DRMWinDevice methods
     *)

    (* Repaint the window with the last frame which was rendered. *)
    function HandlePaint (hDC: HDC) : HResult; stdcall;

    (* Respond to a WM_ACTIVATE message. *)
    function HandleActivate (wparam: WORD) : HResult; stdcall;
  end;

(*
 * GUIDS used by Direct3DRM Windows interface
 *)
  IID_IDirect3DRMWinDevice = IDirect3DRMWinDevice;

(***************************************************************************
 *
 *  Copyright (C) 1998-1999 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       rmxfguid.h
 *
 *  Content:    Defines GUIDs of D3DRM's templates.
 *
 ***************************************************************************)
 
const
(* {2B957100-9E9A-11cf-AB39-0020AF71E433} *)
  TID_D3DRMInfo: TGUID =
      (D1:$2b957100;D2:$9e9a;D3:$11cf;D4:($ab,$39,$00,$20,$af,$71,$e4,$33));

(* {3D82AB44-62DA-11cf-AB39-0020AF71E433} *)
  TID_D3DRMMesh: TGUID =
      (D1:$3d82ab44;D2:$62da;D3:$11cf;D4:($ab,$39,$00,$20,$af,$71,$e4,$33));

(* {3D82AB5E-62DA-11cf-AB39-0020AF71E433} *)
  TID_D3DRMVector: TGUID =
      (D1:$3d82ab5e;D2:$62da;D3:$11cf;D4:($ab,$39,$00,$20,$af,$71,$e4,$33));

(* {3D82AB5F-62DA-11cf-AB39-0020AF71E433} *)
  TID_D3DRMMeshFace: TGUID =
      (D1:$3d82ab5f;D2:$62da;D3:$11cf;D4:($ab,$39,$00,$20,$af,$71,$e4,$33));

(* {3D82AB4D-62DA-11cf-AB39-0020AF71E433} *)
  TID_D3DRMMaterial: TGUID =
      (D1:$3d82ab4d;D2:$62da;D3:$11cf;D4:($ab,$39,$00,$20,$af,$71,$e4,$33));

(* {35FF44E1-6C7C-11cf-8F52-0040333594A3} *)
  TID_D3DRMMaterialArray: TGUID =
      (D1:$35ff44e1;D2:$6c7c;D3:$11cf;D4:($8F,$52,$00,$40,$33,$35,$94,$a3));

(* {3D82AB46-62DA-11cf-AB39-0020AF71E433} *)
  TID_D3DRMFrame: TGUID =
      (D1:$3d82ab46;D2:$62da;D3:$11cf;D4:($ab,$39,$00,$20,$af,$71,$e4,$33));

(* {F6F23F41-7686-11cf-8F52-0040333594A3} *)
  TID_D3DRMFrameTransformMatrix: TGUID =
      (D1:$f6f23f41;D2:$7686;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {F6F23F42-7686-11cf-8F52-0040333594A3} *)
  TID_D3DRMMeshMaterialList: TGUID =
      (D1:$f6f23f42;D2:$7686;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {F6F23F40-7686-11cf-8F52-0040333594A3} *)
  TID_D3DRMMeshTextureCoords: TGUID =
      (D1:$f6f23f40;D2:$7686;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {F6F23F43-7686-11cf-8F52-0040333594A3} *)
  TID_D3DRMMeshNormals: TGUID =
      (D1:$f6f23f43;D2:$7686;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {F6F23F44-7686-11cf-8F52-0040333594A3} *)
  TID_D3DRMCoords2d: TGUID =
      (D1:$f6f23f44;D2:$7686;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {F6F23F45-7686-11cf-8F52-0040333594A3} *)
  TID_D3DRMMatrix4x4: TGUID =
      (D1:$f6f23f45;D2:$7686;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {3D82AB4F-62DA-11cf-AB39-0020AF71E433} *)
  TID_D3DRMAnimation: TGUID =
      (D1:$3d82ab4f;D2:$62da;D3:$11cf;D4:($ab,$39,$00,$20,$af,$71,$e4,$33));

(* {3D82AB50-62DA-11cf-AB39-0020AF71E433} *)
  TID_D3DRMAnimationSet: TGUID =
      (D1:$3d82ab50;D2:$62da;D3:$11cf;D4:($ab,$39,$00,$20,$af,$71,$e4,$33));

(* {10DD46A8-775B-11cf-8F52-0040333594A3} *)
  TID_D3DRMAnimationKey: TGUID =
      (D1:$10dd46a8;D2:$775b;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$A3));

(* {10DD46A9-775B-11cf-8F52-0040333594A3} *)
  TID_D3DRMFloatKeys: TGUID =
      (D1:$10dd46a9;D2:$775b;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$A3));

(* {01411840-7786-11cf-8F52-0040333594A3} *)
  TID_D3DRMMaterialAmbientColor: TGUID =
      (D1:$01411840;D2:$7786;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$A3));

(* {01411841-7786-11cf-8F52-0040333594A3} *)
  TID_D3DRMMaterialDiffuseColor: TGUID =
      (D1:$01411841;D2:$7786;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$A3));

(* {01411842-7786-11cf-8F52-0040333594A3} *)
  TID_D3DRMMaterialSpecularColor: TGUID =
      (D1:$01411842;D2:$7786;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$A3));

(* {D3E16E80-7835-11cf-8F52-0040333594A3} *)
  TID_D3DRMMaterialEmissiveColor: TGUID =
      (D1:$d3e16e80;D2:$7835;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {01411843-7786-11cf-8F52-0040333594A3} *)
  TID_D3DRMMaterialPower: TGUID =
      (D1:$01411843;D2:$7786;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$A3));

(* {35FF44E0-6C7C-11cf-8F52-0040333594A3} *)
  TID_D3DRMColorRGBA: TGUID =
      (D1:$35ff44e0;D2:$6c7c;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$A3));

(* {D3E16E81-7835-11cf-8F52-0040333594A3} *)
  TID_D3DRMColorRGB: TGUID =
      (D1:$d3e16e81;D2:$7835;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {A42790E0-7810-11cf-8F52-0040333594A3} *)
  TID_D3DRMGuid: TGUID =
      (D1:$a42790e0;D2:$7810;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {A42790E1-7810-11cf-8F52-0040333594A3} *)
  TID_D3DRMTextureFilename: TGUID =
      (D1:$a42790e1;D2:$7810;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {A42790E2-7810-11cf-8F52-0040333594A3} *)
  TID_D3DRMTextureReference: TGUID =
      (D1:$a42790e2;D2:$7810;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {1630B820-7842-11cf-8F52-0040333594A3} *)
  TID_D3DRMIndexedColor: TGUID =
      (D1:$1630b820;D2:$7842;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {1630B821-7842-11cf-8F52-0040333594A3} *)
  TID_D3DRMMeshVertexColors: TGUID =
      (D1:$1630b821;D2:$7842;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {4885AE60-78E8-11cf-8F52-0040333594A3} *)
  TID_D3DRMMaterialWrap: TGUID =
      (D1:$4885ae60;D2:$78e8;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {537DA6A0-CA37-11d0-941C-0080C80CFA7B} *)
  TID_D3DRMBoolean: TGUID =
      (D1:$537da6a0;D2:$ca37;D3:$11d0;D4:($94,$1c,$00,$80,$c8,$0c,$fa,$7b));

(* {ED1EC5C0-C0A8-11d0-941C-0080C80CFA7B} *)
  TID_D3DRMMeshFaceWraps: TGUID =
      (D1:$ed1ec5c0;D2:$c0a8;D3:$11d0;D4:($94,$1c,$00,$80,$c8,$0c,$fa,$7b));

(* {4885AE63-78E8-11cf-8F52-0040333594A3} *)
  TID_D3DRMBoolean2d: TGUID =
      (D1:$4885ae63;D2:$78e8;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {F406B180-7B3B-11cf-8F52-0040333594A3} *)
  TID_D3DRMTimedFloatKeys: TGUID =
      (D1:$f406b180;D2:$7b3b;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {E2BF56C0-840F-11cf-8F52-0040333594A3} *)
  TID_D3DRMAnimationOptions: TGUID =
      (D1:$e2bf56c0;D2:$840f;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {E2BF56C1-840F-11cf-8F52-0040333594A3} *)
  TID_D3DRMFramePosition: TGUID =
      (D1:$e2bf56c1;D2:$840f;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {E2BF56C2-840F-11cf-8F52-0040333594A3} *)
  TID_D3DRMFrameVelocity: TGUID =
      (D1:$e2bf56c2;D2:$840f;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {E2BF56C3-840F-11cf-8F52-0040333594A3} *)
  TID_D3DRMFrameRotation: TGUID =
      (D1:$e2bf56c3;D2:$840f;D3:$11cf;D4:($8f,$52,$00,$40,$33,$35,$94,$a3));

(* {3D82AB4A-62DA-11cf-AB39-0020AF71E433} *)
  TID_D3DRMLight: TGUID =
      (D1:$3d82ab4a;D2:$62da;D3:$11cf;D4:($ab,$39,$00,$20,$af,$71,$e4,$33));

(* {3D82AB51-62DA-11cf-AB39-0020AF71E433} *)
  TID_D3DRMCamera: TGUID =
      (D1:$3d82ab51;D2:$62da;D3:$11cf;D4:($ab,$39,$00,$20,$af,$71,$e4,$33));

(* {E5745280-B24F-11cf-9DD5-00AA00A71A2F} *)
  TID_D3DRMAppData: TGUID =
      (D1:$e5745280;D2:$b24f;D3:$11cf;D4:($9d,$d5,$00,$aa,$00,$a7,$1a,$2f));

(* {AED22740-B31F-11cf-9DD5-00AA00A71A2F} *)
  TID_D3DRMLightUmbra: TGUID =
      (D1:$aed22740;D2:$b31f;D3:$11cf;D4:($9d,$d5,$00,$aa,$00,$a7,$1a,$2f));

(* {AED22742-B31F-11cf-9DD5-00AA00A71A2F} *)
  TID_D3DRMLightRange: TGUID =
      (D1:$aed22742;D2:$b31f;D3:$11cf;D4:($9d,$d5,$00,$aa,$00,$a7,$1a,$2f));

(* {AED22741-B31F-11cf-9DD5-00AA00A71A2F} *)
  TID_D3DRMLightPenumbra: TGUID =
      (D1:$aed22741;D2:$b31f;D3:$11cf;D4:($9d,$d5,$00,$aa,$00,$a7,$1a,$2f));

(* {A8A98BA0-C5E5-11cf-B941-0080C80CFA7B} *)
  TID_D3DRMLightAttenuation: TGUID =
      (D1:$a8a98ba0;D2:$c5e5;D3:$11cf;D4:($b9,$41,$00,$80,$c8,$0c,$fa,$7b));

(* {3A23EEA0-94B1-11d0-AB39-0020AF71E433} *)
  TID_D3DRMInlineData: TGUID =
      (D1:$3a23eea0;D2:$94b1;D3:$11d0;D4:($ab,$39,$00,$20,$af,$71,$e4,$33));

(* {3A23EEA1-94B1-11d0-AB39-0020AF71E433} *)
  TID_D3DRMUrl: TGUID =
      (D1:$3a23eea1;D2:$94b1;D3:$11d0;D4:($ab,$39,$00,$20,$af,$71,$e4,$33));

(* {8A63C360-997D-11d0-941C-0080C80CFA7B} *)
  TID_D3DRMProgressiveMesh: TGUID =
      (D1:$8A63C360;D2:$997D;D3:$11d0;D4:($94,$1C,$00,$80,$C8,$0C,$FA,$7B));

(* {98116AA0-BDBA-11d1-82C0-00A0C9697271} *)
  TID_D3DRMExternalVisual: TGUID =
      (D1:$98116AA0;D2:$BDBA;D3:$11d1;D4:($82,$C0,$00,$A0,$C9,$69,$72,$71));

(* {7F0F21E0-BFE1-11d1-82C0-00A0C9697271} *)
  TID_D3DRMStringProperty: TGUID =
      (D1:$7f0f21e0;D2:$bfe1;D3:$11d1;D4:($82,$c0,$00,$a0,$c9,$69,$72,$71));

(* {7F0F21E1-BFE1-11d1-82C0-00A0C9697271} *)
  TID_D3DRMPropertyBag: TGUID =
      (D1:$7f0f21e1;D2:$bfe1;D3:$11d1;D4:($82,$c0,$00,$a0,$c9,$69,$72,$71));

// {7F5D5EA0-D53A-11d1-82C0-00A0C9697271}
  TID_D3DRMRightHanded: TGUID =
      (D1:$7f5d5ea0;D2:$d53a;D3:$11d1;D4:($82,$c0,$00,$a0,$c9,$69,$72,$71));

(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       rmxftmpl.h
 *  Content:    D3DRM XFile templates in binary form
 *
 ***************************************************************************)

const
  D3DRM_XTEMPLATE_BYTES  = 3278;

  D3DRM_XTEMPLATES: array [0..D3DRM_XTEMPLATE_BYTES-1] of byte = (
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
        $12, 0, $f, 0, $b, 0, $1f, 0, $1, 0, 
        $b, 0, 0, 0, $52, $69, $67, $68, $74, $48, 
        $61, $6e, $64, $65, $64, $a, 0, $5, 0, $a0, 
        $5e, $5d, $7f, $3a, $d5, $d1, $11, $82, $c0, 0, 
        $a0, $c9, $69, $72, $71, $29, 0, $1, 0, $c, 
        0, 0, 0, $62, $52, $69, $67, $68, $74, $48, 
        $61, $6e, $64, $65, $64, $14, 0, $b, 0);

implementation

uses
  DXCommon;

(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3drmdef.h
 *  Content:    Direct3DRM include file
 *
 ***************************************************************************)

procedure D3DRMAnimationGetRotateKey
    (var rmKey: TD3DRMAnimationKey; var rmQuat: TD3DRMQuaternion);
begin
  rmQuat := rmKey.dqRotateKey;
end;

procedure D3DRMAnimationGetScaleKey
    (var rmKey: TD3DRMAnimationKey; var dvVec: TD3DVector);
begin
  dvVec := rmKey.dvScaleKey;
end;

procedure D3DRMAnimationGetPositionKey
    (var rmKey: TD3DRMAnimationKey; var dvVec: TD3DVector);
begin
  dvVec := rmKey.dvPositionKey;
end;

procedure D3DRMAnimatioSetRotateKey
    (var rmKey: TD3DRMAnimationKey; var rmQuat: TD3DRMQuaternion);
begin
  rmKey.dqRotateKey := rmQuat;
end;

procedure D3DRMAnimationSetScaleKey
    (var rmKey: TD3DRMAnimationKey; var dvVec: TD3DVector);
begin
  rmKey.dvScaleKey := dvVec;
end;

procedure D3DRMAnimationSetPositionKey
    (var rmKey: TD3DRMAnimationKey; var dvVec: TD3DVector);
begin
  rmKey.dvPositionKey := dvVec;
end;

(*==========================================================================;
 *
 *  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       d3drm.h
 *  Content:    Direct3DRM include file
 *
 ***************************************************************************)

function D3DRMErrorString(Value: HResult) : string;
begin
  case Value of
    D3DRM_OK: Result := 'No error. Equivalent to DD_OK.';
    D3DRMERR_BADALLOC: Result := 'Out of memory.';
    D3DRMERR_BADDEVICE: Result := 'Device is not compatible with renderer.';
    D3DRMERR_BADFILE: Result := 'Data file is corrupt.';
    D3DRMERR_BADMAJORVERSION: Result := 'Bad DLL major version.';
    D3DRMERR_BADMINORVERSION: Result := 'Bad DLL minor version.';
    D3DRMERR_BADOBJECT: Result := 'Object expected in argument.';
    D3DRMERR_BADPMDATA: Result := 'The data in the .x file is corrupted. The conversion to a progressive mesh succeeded but produced an invalid progressive mesh in the .x file.';
    D3DRMERR_BADTYPE: Result := 'Bad argument type passed.';
    D3DRMERR_BADVALUE: Result := 'Bad argument value passed.';
    D3DRMERR_BOXNOTSET: Result := 'An attempt was made to access a bounding box (for example, with IDirect3DRMFrame3::GetBox) when no bounding box was set on the frame.';
    D3DRMERR_CLIENTNOTREGISTERED: Result := 'Client has not been registered. Call IDirect3DRM3::RegisterClient.';
    D3DRMERR_CONNECTIONLOST: Result := 'Data connection was lost during a load, clone, or duplicate.';
    D3DRMERR_ELEMENTINUSE: Result := 'Element can´t be modified or deleted while in use. To empty a submesh, call Empty() against its parent.';
//    D3DRMERR_ENTRYINUSE: Result := 'Vertex or normal entries are currently in use by a face and cannot be deleted.';
    D3DRMERR_FACEUSED: Result := 'Face already used in a mesh.';
    D3DRMERR_FILENOTFOUND: Result := 'File cannot be opened.';
//    D3DRMERR_INCOMPATIBLEKEY: Result := 'Specified animation key is incompatible. The key cannot be modified.';
    D3DRMERR_INVALIDLIBRARY: Result := 'Specified libary is invalid.';
//    D3DRMERR_INVALIDOBJECT: Result := 'Method received a pointer to an object that is invalid.';
//    D3DRMERR_INVALIDPARAMS: Result := 'One of the parameters passed to the method is invalid.';
    D3DRMERR_LIBRARYNOTFOUND: Result := 'Specified libary not found.';
    D3DRMERR_LOADABORTED: Result := 'Load aborted by user.';
    D3DRMERR_NOSUCHKEY: Result := 'Specified animation key does not exist.';
    D3DRMERR_NOTCREATEDFROMDDS: Result := 'Specified texture was not created from a DirectDraw Surface.';
    D3DRMERR_NOTDONEYET: Result := 'Unimplemented.';
    D3DRMERR_NOTENOUGHDATA: Result := 'Not enough data has been loaded to perform the requested operation.';
    D3DRMERR_NOTFOUND: Result := 'Object not found in specified place.';
//    D3DRMERR_OUTOFRANGE: Result := 'Specified value is out of range.';
    D3DRMERR_PENDING: Result := 'Data required to supply the requested information has not finished loading.';
    D3DRMERR_REQUESTTOOLARGE: Result := 'Attempt was made to set a level of detail in a progressive mesh greater than the maximum available.';
    D3DRMERR_REQUESTTOOSMALL: Result := 'Attempt was made to set the minimum rendering detail of a progressive mesh smaller than the detail in the base mesh (the minimum for rendering).';
    D3DRMERR_TEXTUREFORMATNOTFOUND: Result := 'Texture format could not be found that meets the specified criteria and that the underlying Immediate Mode device supports.';
    D3DRMERR_UNABLETOEXECUTE: Result := 'Unable to carry out procedure.';
    DDERR_INVALIDOBJECT: Result := 'Received pointer that was an invalid object.';
    DDERR_INVALIDPARAMS: Result := 'One or more of the parameters passed to the method are incorrect.';
    DDERR_NOTFOUND: Result := 'The requested item was not found.';
    DDERR_NOTINITIALIZED: Result := 'An attempt was made to call an interface method of an object created by CoCreateInstance before the object was initialized.';
    DDERR_OUTOFMEMORY: Result := 'DirectDraw does not have enough memory to perform the operation.';
    else Result := 'Unrecognized Error';
  end;
end;

initialization
begin
  if not IsNTandDelphiRunning then
  begin
    D3DRMDLL := LoadLibrary('D3DRM.dll');
    //d3drmdef:
    D3DRMCreateColorRGB := GetProcAddress(D3DRMDLL,'D3DRMCreateColorRGB');
    D3DRMCreateColorRGBA := GetProcAddress(D3DRMDLL,'D3DRMCreateColorRGBA');
    D3DRMColorGetRed := GetProcAddress(D3DRMDLL,'D3DRMColorGetRed');
    D3DRMColorGetGreen := GetProcAddress(D3DRMDLL,'D3DRMColorGetGreen');
    D3DRMColorGetBlue := GetProcAddress(D3DRMDLL,'D3DRMColorGetBlue');
    D3DRMColorGetAlpha := GetProcAddress(D3DRMDLL,'D3DRMColorGetAlpha');
    D3DRMVectorAdd := GetProcAddress(D3DRMDLL,'D3DRMVectorAdd');
    D3DRMVectorSubtract := GetProcAddress(D3DRMDLL,'D3DRMVectorSubtract');
    D3DRMVectorReflect := GetProcAddress(D3DRMDLL,'D3DRMVectorReflect');
    D3DRMVectorCrossProduct := GetProcAddress(D3DRMDLL,'D3DRMVectorCrossProduct');
    D3DRMVectorDotProduct := GetProcAddress(D3DRMDLL,'D3DRMVectorDotProduct');
    D3DRMVectorNormalize := GetProcAddress(D3DRMDLL,'D3DRMVectorNormalize');
    D3DRMVectorModulus := GetProcAddress(D3DRMDLL,'D3DRMVectorModulus');
    D3DRMVectorRotate := GetProcAddress(D3DRMDLL,'D3DRMVectorRotate');
    D3DRMVectorScale := GetProcAddress(D3DRMDLL,'D3DRMVectorScale');
    D3DRMVectorRandom := GetProcAddress(D3DRMDLL,'D3DRMVectorRandom');
    D3DRMQuaternionFromRotation := GetProcAddress(D3DRMDLL,'D3DRMQuaternionFromRotation');
    D3DRMQuaternionMultiply := GetProcAddress(D3DRMDLL,'D3DRMQuaternionMultiply');
    D3DRMQuaternionSlerp := GetProcAddress(D3DRMDLL,'D3DRMQuaternionSlerp');
    D3DRMMatrixFromQuaternion := GetProcAddress(D3DRMDLL,'D3DRMMatrixFromQuaternion');
    D3DRMQuaternionFromMatrix := GetProcAddress(D3DRMDLL,'D3DRMQuaternionFromMatrix');
    //d3drm:
    Direct3DRMCreate := GetProcAddress(D3DRMDLL,'Direct3DRMCreate');
  end;
end;

finalization
begin
  if D3DRMDLL <> 0 then FreeLibrary(D3DRMDLL);
end;

end.

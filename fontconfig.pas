unit FontConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  FcBool = Integer;

function FcConfigAppFontAddFile(Config: Pointer; _File: PChar): FcBool; cdecl; external 'libfontconfig.so';

function PangoCairoFontMapGetDefault: Pointer; cdecl; external 'libpangocairo-1.0.so' name 'pango_cairo_font_map_get_default';

procedure PangoFcFontMapConfigChanged(FcFontMap: Pointer); cdecl; external 'libpangoft2-1.0.so' name 'pango_fc_font_map_config_changed';

implementation

end.


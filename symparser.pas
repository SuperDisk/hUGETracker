unit SymParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TSymbolMap = specialize TFPGMap<String, Integer>;

function ParseSymFile(F: String): TSymbolMap;

implementation

function ParseSymFile(F: String): TSymbolMap;
var
  SL: TStringList;
  SA: TStringArray;
  S: String;
begin
  Result := TSymbolMap.Create;
  SL := TStringList.Create;
  try
    SL.LoadFromFile(F);

    // Drop the first two lines
    SL.Delete(0);
    SL.Delete(0);
    // Drop the last line
    SL.Delete(SL.Count-1);

    for S in SL do begin
      SA := S.Split(' ');
      Result.Add(SA[1], StrToInt('x'+SA[0].Substring(3)));
    end;
  finally
    SL.Free;
  end;
end;

end.


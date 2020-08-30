unit SymParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TSymbolMap = specialize TFPGMap<String, Integer>;

var
  SymbolTable: TSymbolMap;

function WordPeekSymbol(Symbol: String): Integer;
procedure WordPokeSymbol(Symbol: String; Value: Word);
function PeekSymbol(Symbol: String): Integer;
procedure PokeSymbol(Symbol: String; Value: Byte);

procedure ParseSymFile(F: String);

implementation

uses machine;

procedure ParseSymFile(F: String);
var
  SL: TStringList;
  SA: TStringArray;
  S: String;
begin
  if Assigned(SymbolTable) then
    SymbolTable.Free;

  SymbolTable := TSymbolMap.Create;
  SL := TStringList.Create;
  try
    SL.LoadFromFile(F);

    // Drop the first two lines
    SL.Delete(0);
    SL.Delete(0);

    for S in SL do begin
      SA := S.Split(' ');
      SymbolTable.Add(SA[1], StrToInt('x'+SA[0].Substring(3)));
    end;
  finally
    SL.Free;
  end;
end;

function PeekSymbol(Symbol: String): Integer;
begin
  if SymbolTable = nil then exit(0);
  Result := speekb(SymbolTable.KeyData[Symbol]);
end;

procedure PokeSymbol(Symbol: String; Value: Byte);
begin
  if SymbolTable = nil then exit;
  spokeb(SymbolTable.KeyData[Symbol], Value);
end;

function WordPeekSymbol(Symbol: String): Integer;
begin
  if SymbolTable = nil then exit(0);
  Result := wordpeek(SymbolTable.KeyData[Symbol]);
end;

procedure WordPokeSymbol(Symbol: String; Value: Word);
begin
  if SymbolTable = nil then exit;
  wordpoke(SymbolTable.KeyData[Symbol], Value);
end;

end.


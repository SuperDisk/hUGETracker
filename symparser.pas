unit SymParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TSymbolMap = specialize TFPGMap<String, Integer>;

function SymbolAddress(Symbol: String): Integer;
function WordPeekSymbol(Symbol: String): Integer;
procedure WordPokeSymbol(Symbol: String; Value: Word);
function PeekSymbol(Symbol: String): Integer;
procedure PokeSymbol(Symbol: String; Value: Byte);

procedure ParseSymFile(F: String);

implementation

uses machine;

var
  SymbolTable: TSymbolMap;

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

function SymbolAddress(Symbol: String): Integer;
begin
  if not SymbolTable.TryGetData(Symbol, Result) then begin
    Writeln(StdErr, '[WARNING] Attempting to read address of unloaded symbol: ', symbol);
    Result := 0;
  end;
end;

function PeekSymbol(Symbol: String): Integer;
begin
  if SymbolTable = nil then exit(0);
  if SymbolTable.IndexOf(Symbol) = -1 then begin
    WriteLn(StdErr, '[WARNING] Attempting to peek unloaded symbol: ', symbol);
    Exit(0);
  end;

  Result := speekb(SymbolTable.KeyData[Symbol]);
end;

procedure PokeSymbol(Symbol: String; Value: Byte);
begin
  if SymbolTable = nil then exit;
  if SymbolTable.IndexOf(Symbol) = -1 then begin
    WriteLn(StdErr, '[WARNING] Attempting to poke unloaded symbol: ', symbol);
    Exit;
  end;
  spokeb(SymbolTable.KeyData[Symbol], Value);
end;

function WordPeekSymbol(Symbol: String): Integer;
begin
  if SymbolTable = nil then exit(0);
  if SymbolTable.IndexOf(Symbol) = -1 then begin
    WriteLn(StdErr, '[WARNING] Attempting to wordpeek unloaded symbol: ', symbol);
    Exit(0);
  end;

  Result := wordpeek(SymbolTable.KeyData[Symbol]);
end;

procedure WordPokeSymbol(Symbol: String; Value: Word);
begin
  if SymbolTable = nil then exit;
  if SymbolTable.IndexOf(Symbol) = -1 then begin
    WriteLn(StdErr, '[WARNING] Attempting to wordpoke unloaded symbol: ', symbol);
    Exit;
  end;
  wordpoke(SymbolTable.KeyData[Symbol], Value);
end;

end.


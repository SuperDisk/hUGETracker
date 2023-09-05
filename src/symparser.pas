unit SymParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazLoggerBase;

type
  TSymbolMap = specialize TFPGMap<String, Integer>;

function SymbolAddress(Symbol: String): Integer;
function WordPeekSymbol(Symbol: String): Integer;
procedure WordPokeSymbol(Symbol: String; Value: Word);
function PeekSymbol(Symbol: String; Offset: Integer = 0): Integer;
procedure PokeSymbol(Symbol: String; Value: Byte);
procedure WriteBufferToSymbol(Symbol: String; const Buffer; Count: Integer);
procedure WriteBufferToAddress(Address: Integer; const Buffer; Count: Integer);

procedure ParseSymFile(F: String);

implementation

uses machine;

var
  SymbolTable: TSymbolMap;


procedure WriteBufferToAddress(Address: Integer; const Buffer; Count: Integer);
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    spokeb(Address+I, PByte(@Buffer)[I]);
end;

procedure WriteBufferToSymbol(Symbol: string; const Buffer; Count: Integer);
begin
  WriteBufferToAddress(SymbolAddress(Symbol), Buffer, Count);
end;

procedure ParseSymFile(F: String);
var
  SL: TStringList;
  SA: TStringArray;
  S: String;
begin
  SymbolTable.Clear;

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
    DebugLn(['[WARNING] Attempting to read address of unloaded symbol: ', symbol]);
    Result := 0;
  end;
end;

function PeekSymbol(Symbol: String; Offset: Integer = 0): Integer;
begin
  if SymbolTable.IndexOf(Symbol) = -1 then begin
    DebugLn(['[WARNING] Attempting to peek unloaded symbol: ', symbol]);
    Exit(0);
  end;

  Result := speekb(SymbolTable.KeyData[Symbol]+Offset);
end;

procedure PokeSymbol(Symbol: String; Value: Byte);
begin
  if SymbolTable.IndexOf(Symbol) = -1 then begin
    DebugLn(['[WARNING] Attempting to poke unloaded symbol: ', symbol]);
    Exit;
  end;

  spokeb(SymbolTable.KeyData[Symbol], Value);
end;

function WordPeekSymbol(Symbol: String): Integer;
begin
  if SymbolTable.IndexOf(Symbol) = -1 then begin
    DebugLn('[WARNING] Attempting to wordpeek unloaded symbol: ', symbol);
    Exit(0);
  end;

  Result := wordpeek(SymbolTable.KeyData[Symbol]);
end;

procedure WordPokeSymbol(Symbol: String; Value: Word);
begin
  if SymbolTable.IndexOf(Symbol) = -1 then begin
    DebugLn('[WARNING] Attempting to wordpoke unloaded symbol: ', symbol);
    Exit;
  end;
  wordpoke(SymbolTable.KeyData[Symbol], Value);
end;

begin
  SymbolTable := TSymbolMap.Create;
end.


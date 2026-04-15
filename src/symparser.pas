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

procedure ParseSymLine(const Line: String);
var
  S, Location, Name, AddrPart: String;
  SemiPos, I: Integer;
  Tokens: TStringList;
  Addr: Integer;
begin
  S := Line;

  // Strip comment starting at ';'
  SemiPos := Pos(';', S);
  if SemiPos > 0 then
    S := Copy(S, 1, SemiPos - 1);

  S := Trim(S);
  if S = '' then Exit;

  // Split on runs of whitespace (spaces/tabs)
  Tokens := TStringList.Create;
  try
    I := 1;
    while I <= Length(S) do begin
      while (I <= Length(S)) and ((S[I] = ' ') or (S[I] = #9)) do Inc(I);
      if I > Length(S) then Break;
      AddrPart := '';
      while (I <= Length(S)) and (S[I] <> ' ') and (S[I] <> #9) do begin
        AddrPart := AddrPart + S[I];
        Inc(I);
      end;
      if AddrPart <> '' then Tokens.Add(AddrPart);
    end;

    // Single-token lines are reserved for future extensions; ignore silently.
    if Tokens.Count < 2 then Exit;

    Location := Tokens[0];
    Name := Tokens[1];

    // Location forms: "bank:address", "BOOT:address", or bare "address".
    SemiPos := Pos(':', Location);
    if SemiPos > 0 then
      AddrPart := Copy(Location, SemiPos + 1, Length(Location))
    else
      AddrPart := Location;

    if AddrPart = '' then begin
      DebugLn(['[WARNING] Skipping malformed sym line: ', Line]);
      Exit;
    end;

    try
      Addr := StrToInt('x' + AddrPart);
    except
      on E: EConvertError do begin
        DebugLn(['[WARNING] Skipping sym line with bad address: ', Line]);
        Exit;
      end;
    end;

    if SymbolTable.IndexOf(Name) = -1 then
      SymbolTable.Add(Name, Addr);
  finally
    Tokens.Free;
  end;
end;

procedure ParseSymFile(F: String);
var
  SL: TStringList;
  S: String;
begin
  SymbolTable.Clear;

  SL := TStringList.Create;
  try
    SL.LoadFromFile(F);
    for S in SL do
      ParseSymLine(S);
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


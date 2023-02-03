unit InstrumentPreview;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, instruments, constants, hugedatatypes, symparser, machine, utils;

procedure StartInstrumentPreview(Instr: TInstrument; Note: Integer; SquareOnCh2: Boolean = False);
procedure StopInstrumentPreview(Channel: Integer);

implementation

function GetLabel(Constant: String; Number: Integer): String;
begin
  Result := Constant+IntToStr(Number);
end;

procedure StartInstrumentPreview(Instr: TInstrument; Note: Integer; SquareOnCh2: Boolean);
var
  Addr: Integer;
  AsmInstrument: TAsmInstrument;
  HighMask: Integer;
  Channel: Integer;
begin
  AsmInstrument := InstrumentToBytes(Instr);

  case Instr.Type_ of
    itSquare: if SquareOnCh2 then Channel := 2 else Channel := 1;
    itWave: Channel := 3;
    itNoise: Channel := 4;
  end;

  if Instr.Type_ = itNoise then begin
    Addr := SymbolAddress(GetLabel(SYM_HALT_INSTR, Channel));
    spokeb(Addr, AsmInstrument[1]);

    HighMask := AsmInstrument[0];
    if Instr.LengthEnabled then
      HighMask := HighMask or %01000000;
    if Instr.CounterStep = swSeven then
      HighMask := HighMask or %10000000;
    spokeb(Addr+3, HighMask);
  end else begin
    WriteBufferToSymbol(GetLabel(SYM_HALT_INSTR, Channel), AsmInstrument, SizeOf(TAsmInstrument));
  end;

  WriteBufferToSymbol(GetLabel(SYM_HALT_SUBPATTERN, Channel),
                      SubpatternToBytes(Instr.Subpattern),
                      SizeOf(TSubpatternBytes));

  WordPokeSymbol(GetLabel(SYM_HALT_PERIOD, Channel), NotesToFreqs.KeyData[Note]);
  PokeSymbol(GetLabel(SYM_HALT_NOTE, Channel), Note);

  PokeSymbol(GetLabel(SYM_HALT_START, Channel), 1);
  if Instr.SubpatternEnabled then
    PokeSymbol(GetLabel(SYM_HALT_RUNNING, Channel), 1);
end;

procedure StopInstrumentPreview(Channel: Integer);
begin
  PokeSymbol(GetLabel(SYM_HALT_RUNNING, Channel), 0);
end;

end.


program rgb2gbdk;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, strutils
  { you can add units after this };

type
  TRSymbol = packed record
    ID: Integer;
    Name: String;
    SymType: Byte;

    FileName: String;
    LineNum, SectionID, Value: LongInt;
  end;

  TRPatch = packed record
    SourceFile: String;
    Offset: LongInt;
    PatchType: Byte;
    RPNSize: LongInt;
    RPN: array of Byte;
  end;

  TRSection = packed record
    ID: Integer;
    Name: String;
    Size: LongInt;
    SectType: Byte;
    Org: LongInt;
    Bank: LongInt;
    Align: LongInt;

    Data: array of Byte;
    NumberOfPatches: LongInt;
    Patches: array of TRPatch;
  end;

  TRObj = packed record
    ID: array[0..3] of Char;
    RevisionNumber: LongInt;
    NumberOfSymbols: LongInt;
    NumberOfSections: LongInt;

    Symbols: array of TRSymbol;
    Sections: array of TRSection;
  end;

  TRPNTag = (
    rpnPlus,
    rpnMinus,
    rpnTimes,
    rpnDiv,
    rpnMod,
    rpnNegate,
    rpnOr,
    rpnAnd,
    rpnXor,
    rpnComplement,
    rpnBoolAnd,
    rpnBoolOr,
    rpnBoolNeg,
    rpnEqual,
    rpnNotEqual,
    rpnGreater,
    rpnLess,
    rpnGreaterEqual,
    rpnLessEqual,
    rpnShl,
    rpnShr,
    rpnBankSymbol,
    rpnBankSection,
    rpnCurrentBank,
    rpnHramCheck,
    rpnRstCheck,
    rpnInteger,
    rpnSymbol);

  TRPNNode = record
    case Tag: TRPNTag of
      rpnBankSymbol: (BankSymbol: Integer);
      rpnBankSection: (BankSection: Integer);
      rpnInteger: (IntValue: Integer);
      rpnSymbol: (SymbolID: Integer);
  end;
  TRPN = array of TRPNNode;

const
  WRAM0 = 0;
  VRAM = 1;
  ROMX = 2;
  ROM0 = 3;
  HRAM = 4;
  WRAMX = 5;
  SRAM = 6;
  OAM = 7;

procedure Die(S: String);
begin
  WriteLn(S);
  Readln;
  Halt
end;

function ReadNullTerm(S: TStream): String; // really
var
  B: Byte;
begin
  Result := '';

  while True do begin
    B := S.ReadByte;
    if B = 0 then Exit;
    Result += Chr(B)
  end
end;

function ReadObjFile(S: TStream): TRObj;
var
  I, J: Integer;
begin
  S.Read(Result, SizeOf(TRObj) - SizeOf(Result.Symbols) - SizeOf(Result.Sections));
  SetLength(Result.Symbols, Result.NumberOfSymbols);
  SetLength(Result.Sections, Result.NumberOfSections);

  for I := 0 to Result.NumberOfSymbols-1 do begin
    Result.Symbols[I] := Default(TRSymbol);
    with Result.Symbols[I] do begin
      ID := I;
      Name := ReadNullTerm(S);
      S.Read(SymType, 1);
      if (SymType and $7F) <> 1 then begin
        FileName := ReadNullTerm(S);
        S.Read(LineNum, SizeOf(LineNum));
        S.Read(SectionID, SizeOf(SectionID));
        S.Read(Value, SizeOf(Value));
      end;
    end;
  end;

  for I := 0 to Result.NumberOfSections-1 do begin
    Result.Sections[I] := Default(TRSection);

    with Result.Sections[I] do begin
      ID := I;
      Name := ReadNullTerm(S);
      S.Read(Size, SizeOf(Size));
      S.Read(SectType, SizeOf(SectType));
      S.Read(Org, SizeOf(Org));
      S.Read(Bank, SizeOf(Bank));
      S.Read(Align, SizeOf(Align));
      if ((SectType = ROMX) or (SectType = ROM0)) then begin
        SetLength(Data, Size);
        S.Read(Data[0], Size);
        S.Read(NumberOfPatches, SizeOf(NumberOfPatches));
        SetLength(Patches, NumberOfPatches);
      end;
    end;

    for J := 0 to Result.Sections[I].NumberOfPatches-1 do begin
      with Result.Sections[I].Patches[J] do begin
        SourceFile := ReadNullTerm(S);
        S.Read(Offset, SizeOf(Offset));
        S.Read(PatchType, SizeOf(PatchType));
        S.Read(RPNSize, SizeOf(RPNSize));
        SetLength(RPN, RPNSize);
        S.Read(RPN[0], RPNSize);
      end;
    end;
  end;
end;

function RPNToString(const RPN: array of Byte; const Syms: array of TRSymbol): String;
var
  I: Integer;

function ReadLong: LongWord;
begin
  Inc(I);
  Result := RPN[I];
  Inc(I);
  Result := (Result shl 8) or RPN[I];
  Inc(I);
  Result := (Result shl 8) or RPN[I];
  Inc(I);
  Result := (Result shl 8) or RPN[I];
  Inc(I);
  Result := SwapEndian(Result);
end;

begin
  Result := '';

  I := Low(RPN);
  while I <= High(RPN) do begin
    case RPN[I] of
      $00: begin Result += '+ '; Inc(I); end;
      $01: begin Result += '- '; Inc(I); end;
      $02: begin Result += '* '; Inc(I); end;
      $03: begin Result += '/ '; Inc(I); end;
      $04: begin Result += '% '; Inc(I);  end;
      $05: begin Result += 'neg '; Inc(I); end;
      $10: begin Result += '| '; Inc(I); end;
      $11: begin Result += '& '; Inc(I); end;
      $12: begin Result += '^ '; Inc(I); end;
      $13: begin Result += '~ '; Inc(I); end;
      $21: begin Result += '&& '; Inc(I);   end;
      $22: begin Result += '|| '; Inc(I);   end;
      $23: begin Result += '! '; Inc(I);        end;
      $30: begin Result += '== '; Inc(I);   end;
      $31: begin Result += '!= '; Inc(I);   end;
      $32: begin Result += '> '; Inc(I);  end;
      $33: begin Result += '< '; Inc(I);  end;
      $34: begin Result += '>= '; Inc(I);   end;
      $35: begin Result += '<= '; Inc(I);   end;
      $40: begin Result += '<< '; Inc(I);   end;
      $41: begin Result += '>> '; Inc(I);   end;
      $50: begin Result += '(bank-sym ' + IntToStr(ReadLong)+') '; end;
      $51: begin Result += '(bank-section ' + IntToStr(ReadLong)+') '; end;
      $52: begin Result += 'current-bank'; Inc(I); end;
      $60: begin Result += 'hram-check'; Inc(I); end;
      $61: begin Result += 'rst-check'; Inc(I); end;
      $80: begin Result += '(int ' + IntToStr(ReadLong)+') '; end;
      $81: begin Result += '(sym ' + Syms[ReadLong].Name+') '; end;
      else Exit('INVALID!');
    end;
  end;
end;

function ParseRPN(const RPN: array of Byte): TRPN;
var
  I: Integer;
  Node: TRPNNode;

function ReadLong: LongWord;
begin
  Inc(I);
  Result := RPN[I];
  Inc(I);
  Result := (Result shl 8) or RPN[I];
  Inc(I);
  Result := (Result shl 8) or RPN[I];
  Inc(I);
  Result := (Result shl 8) or RPN[I];
  Inc(I);
  Result := SwapEndian(Result);
end;

function nd(Tag: TRPNTag): TRPNNode;
begin
  Result.Tag := Tag;
end;

procedure PushRPN(Node: TRPNNode);
begin
  SetLength(Result, Length(Result)+1);
  Result[High(Result)] := Node;
end;

begin
  SetLength(Result, 0);
  I := Low(RPN);
  while I <= High(RPN) do begin
    //Writeln(Format('Parsing %x, len=%d', [RPN[I], Length(Result)]));
    case RPN[I] of
      $00: begin PushRPN(Nd(rpnPlus)); Inc(I); end;
      $01: begin PushRPN(Nd(rpnMinus)); Inc(I); end;
      $02: begin PushRPN(Nd(rpnTimes)); Inc(I); end;
      $03: begin PushRPN(Nd(rpnDiv)); Inc(I); end;
      $04: begin PushRPN(Nd(rpnMod)); Inc(I);  end;
      $05: begin PushRPN(Nd(rpnNegate)); Inc(I); end;
      $10: begin PushRPN(Nd(rpnOr));  Inc(I); end;
      $11: begin PushRPN(Nd(rpnAnd)); Inc(I); end;
      $12: begin PushRPN(Nd(rpnXor)); Inc(I); end;
      $13: begin PushRPN(Nd(rpnComplement)); Inc(I); end;
      $21: begin PushRPN(Nd(rpnBoolAnd)); Inc(I);   end;
      $22: begin PushRPN(Nd(rpnBoolOr)); Inc(I);   end;
      $23: begin PushRPN(Nd(rpnBoolNeg)); Inc(I);        end;
      $30: begin PushRPN(Nd(rpnEqual)); Inc(I);   end;
      $31: begin PushRPN(Nd(rpnNotEqual)); Inc(I);   end;
      $32: begin PushRPN(Nd(rpnGreater)); Inc(I);  end;
      $33: begin PushRPN(Nd(rpnLess)); Inc(I);  end;
      $34: begin PushRPN(Nd(rpnGreaterEqual)); Inc(I);   end;
      $35: begin PushRPN(Nd(rpnLessEqual)); Inc(I);   end;
      $40: begin PushRPN(Nd(rpnShl)); Inc(I);   end;
      $41: begin PushRPN(Nd(rpnShr)); Inc(I);   end;
      $50: begin
        Node.Tag := rpnBankSymbol;
        Node.BankSymbol := ReadLong;
        PushRPN(Node);
      end;
      $51: begin
        Node.Tag := rpnBankSection;
        Node.BankSection := ReadLong;
        PushRPN(Node);
      end;
      $52: begin PushRPN(Nd(rpnCurrentBank)); Inc(I); end;
      $60: begin PushRPN(Nd(rpnHramCheck));  Inc(I); end;
      $61: begin PushRPN(Nd(rpnRstCheck));  Inc(I); end;
      $80: begin
        Node.Tag := rpnInteger;
        Node.IntValue := ReadLong;
        PushRPN(Node);
      end;
      $81: begin
        Node.Tag := rpnSymbol;
        Node.SymbolID := ReadLong;
        PushRPN(Node);
      end;
      else die('got malformed RPN!');
    end;
  end;
end;


procedure PrintObjFile(O: TRObj);
var
  Sym: TRSymbol;
  Sect: TRSection;
  Patch: TRPatch;
  SectSize: Integer;
begin
  WriteLn('-=-=- Symbols -=-=-');
  for Sym in O.Symbols do
    Writeln(Format('%s on %d: type=%d, section=%d, value=%d',
                   [Sym.Name, Sym.LineNum, Sym.SymType, Sym.SectionID, Sym.Value]));

  Writeln;
  Writeln;

  WriteLn('-=-=- Sections -=-=-');
  SectSize := 0;
  for Sect in O.Sections do
    if (Sect.Org <> -1) and ((Sect.SectType = ROM0) or (Sect.SectType = ROMX)) then Inc(SectSize, Sect.Size);
  Writeln('Absolute sections: ', SectSize, ' bytes');
  SectSize := 0;
  for Sect in O.Sections do
    if (Sect.Org = -1) and ((Sect.SectType = ROM0) or (Sect.SectType = ROMX)) then Inc(SectSize, Sect.Size);
  Writeln('Relative sections: ', SectSize, ' bytes');
  Writeln;

  for Sect in O.Sections do begin
    Writeln(Format('%s with size %d: offset=%d, patches=%d, bank=%d',
                   [Sect.Name, Sect.Size, Sect.Org, Sect.NumberOfPatches, Sect.Bank]));

    for Patch in Sect.Patches do begin
      Writeln(Format('%s: %d ; %s', [Patch.SourceFile, Patch.PatchType, RPNToString(Patch.RPN, O.Symbols)]));
    end;
    Writeln;
    writeln;
  end;
end;

function FindPatch(const Section: TRSection; Index: Integer; out Patch: TRPatch): Boolean;
var
  I: Integer;
begin
  for I := Low(Section.Patches) to High(Section.Patches) do
    if Section.Patches[I].Offset = Index then begin
      Patch := Section.Patches[I];
      Exit(True);
    end;

  Result := False;
end;

var
  S: TStream;
  RObj: TRObj;

  Symbol: TRSymbol;
  Section: TRSection;
  Patch: TRPatch;

  F: Text;

  ValueToWrite: Word;
  RPN: TRPN;
  I: Word;
  Sct: Word;
  WritePos: Word;
begin
  S := TFileStream.Create('hUGE.obj', fmOpenRead);
  RObj := ReadObjFile(S);
  PrintObjFile(RObj);

  Assign(F, 'hUGE_trans.o');
  Rewrite(F);

  Writeln(F, 'XL');
  Writeln(F, Format('H %x areas %x global symbols', [RObj.NumberOfSections+1, Length(RObj.Symbols)]));
  Writeln(F, 'A _CODE size 0 flags 0'); // Insanity
  for Section in RObj.Sections do begin
    if Section.Org = -1 then
      //Writeln(F, Format('A %s size %x flags 0', [LeftStr(DelSpace(Section.Name), 25), Section.Size]))
      Writeln(F, Format('A %s size %x flags 0', [IfThen(Section.SectType in [ROM0, ROMX], '_CODE', '_DATA'), Section.Size]))
    else Die('absolute sections currently unsupported: ' + Section.Name);
      //Writeln(F, Format('A %s size %x flags C', [LeftStr(DelSpace(Section.Name), 25), Section.Org+Section.Size]));

    for Symbol in RObj.Symbols do
      if Symbol.SectionID = Section.ID then begin
        Writeln(F, Format('S %s Def%.4x', [StringReplace(Symbol.Name, '.', '____', [rfReplaceAll]), Symbol.Value]));
      end;
  end;

  for Sct := Low(RObj.Sections) to High(RObj.Sections) do begin
    Section := RObj.Sections[Sct];

    if (Section.SectType <> ROMX) and (Section.SectType <> ROM0) then Continue;
    if Length(Section.Data) <= 0 then Continue;

    I := Low(Section.Data);
    while I <= High(Section.Data) do begin
      WritePos := I;
      if Section.Org <> -1 then Inc(WritePos, Section.Org);
      if (Section.Org <> -1) and (WritePos >= $200) then Writeln('Section ', Section.Name, ' is clobbering ', WritePos);

      if FindPatch(Section, I, Patch) then begin
        case Patch.PatchType of
          1: begin
            RPN := ParseRPN(Patch.RPN);
            Symbol := RObj.Symbols[RPN[0].SymbolID];
            ValueToWrite := Symbol.Value;
            if RPN[High(RPN)].Tag = rpnPlus then
              Inc(ValueToWrite, RPN[1].IntValue);
            {if RObj.Sections[Symbol.SectionID].SectType = WRAM0 then
              Inc(ValueToWrite, $C000);}

            Writeln(F, Format('T %.2x %.2x %.2x %.2x', [lo(WritePos), hi(WritePos), lo(ValueToWrite), hi(ValueToWrite)]));
            Writeln(F, Format('R 00 00 %.2x %.2x 00 02 %.2x %.2x', [lo(Sct+1), hi(Sct+1), lo(Symbol.SectionID+1), hi(Symbol.SectionID+1)]));
            Inc(I, 2);
          end;
          3: begin
            RPN := ParseRPN(Patch.RPN);
            if Length(RPN) > 1 then Die('Not handling bigger RPN on JR');
            Symbol := RObj.Symbols[RPN[0].SymbolID];
            Writeln(F, Format('T %.2x %.2x %.2x', [lo(WritePos), hi(WritePos), Byte(Symbol.Value - I - 1)]));
            Writeln(F, Format('R 00 00 %.2x %.2x', [lo(Sct+1), hi(Sct+1)]));
            Inc(I);
          end
          else begin
            Die('Unsupported patch type: ' + IntToStr(Patch.PatchType));
            Inc(I);
          end;
        end;
      end
      else begin
        Writeln(F, Format('T %.2x %.2x %.2x', [lo(WritePos), hi(WritePos), Section.Data[I]]));
        Writeln(F, Format('R 00 00 %.2x %.2x', [lo(Sct+1), hi(Sct+1)]));
        Inc(I);
      end;
    end;
  end;

  Writeln('Success!');
  Close(F);

  readln;
end.


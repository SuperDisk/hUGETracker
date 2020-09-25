unit rgb2sdas;

{ TODO: This unit needs to be refactored, really bad. }

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, strutils, Math
  { you can add units after this };

type
  TRSymbol = packed record
    ID: integer;
    Name: string;
    SymType: byte;

    No: integer;

    FileName: string;
    LineNum, SectionID, Value: longint;
  end;

  TRPatch = packed record
    SourceFile: string;
    Offset: longint;
    PCSectionID: longint;
    PCOffset: longint;
    PatchType: byte;
    RPNSize: longint;
    RPN: array of byte;
  end;

  TRSection = packed record
    ID: integer;
    Name: string;
    Size: longint;
    SectType: byte;
    Org: longint;
    Bank: longint;
    Align: byte;
    Ofs: longint;

    Data: array of byte;
    NumberOfPatches: longint;
    Patches: array of TRPatch;
  end;

  TRObj = packed record
    ID: array[0..3] of AnsiChar;
    RevisionNumber: longint;
    NumberOfSymbols: longint;
    NumberOfSections: longint;

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
      rpnBankSymbol: (BankSymbol: integer);
      rpnBankSection: (BankSection: integer);
      rpnInteger: (IntValue: integer);
      rpnSymbol: (SymbolID: integer);
  end;
  TRPN = array of TRPNNode;

type
  TObjFileStream = class(TFileStream)
    function ReadNullTerm: string; virtual;
  end;

  EObjectConversionException = class(Exception);

procedure ConvertRGB2SDAS(SourceName, DestName: string);

implementation

function TObjFileStream.ReadNullTerm: string;
var
  B: byte;
begin
  Result := '';
  while True do
  begin
    B := ReadByte;
    if B = 0 then
      Exit;
    Result += Chr(B);
  end;
end;

const
  WRAM0 = 0;
  VRAM = 1;
  ROMX = 2;
  ROM0 = 3;
  HRAM = 4;
  WRAMX = 5;
  SRAM = 6;
  OAM = 7;

const
  SYM_LOCAL = 0;
  SYM_IMPORT = 1;
  SYM_EXPORT = 2;

const
  PATCH_BYTE = 0;
  PATCH_LE_WORD = 1;
  PATCH_LE_LONG = 2;
  PATCH_JR = 3;

procedure Die(const S: string); overload;
begin
  raise EObjectConversionException.Create(S);
end;

procedure Die(const fmt: string; const params: array of const); overload;
begin
  Die(format(fmt, params));
end;

function ReadObjFile(const afilename: string): TRObj;
const
  Sign: array[0..3] of AnsiChar = 'RGB9';
var
  I, J: integer;
begin
  with TObjFileStream.Create(afilename, fmOpenRead) do
    try
      Read(Result, SizeOf(TRObj) - SizeOf(Result.Symbols) - SizeOf(Result.Sections));
      if not (CompareMem(@Result.ID, @Sign, sizeof(Result.ID)) and
        (Result.RevisionNumber = 5)) then
        Die('Unsupported object file version!');
      SetLength(Result.Symbols, Result.NumberOfSymbols);
      SetLength(Result.Sections, Result.NumberOfSections);

      for I := 0 to Result.NumberOfSymbols - 1 do
      begin
        Result.Symbols[I] := Default(TRSymbol);
        with Result.Symbols[I] do
        begin
          ID := I;
          Name := ReadNullTerm;
          Read(SymType, 1);
          if ((SymType and $7F) <> SYM_IMPORT) then
          begin
            FileName := ReadNullTerm;
            Read(LineNum, SizeOf(LineNum));
            Read(SectionID, SizeOf(SectionID));
            Read(Value, SizeOf(Value));
          end;
        end;
      end;

      for I := 0 to Result.NumberOfSections - 1 do
      begin
        Result.Sections[I] := Default(TRSection);
        with Result.Sections[I] do
        begin
          ID := I;
          Name := ReadNullTerm;
          Read(Size, SizeOf(Size));
          Read(SectType, SizeOf(SectType));
          Read(Org, SizeOf(Org));
          Read(Bank, SizeOf(Bank));
          Read(Align, SizeOf(Align));
          Read(Ofs, SizeOf(Ofs));
          if ((SectType = ROMX) or (SectType = ROM0)) then
          begin
            SetLength(Data, Size);
            Read(Data[0], Size);
            Read(NumberOfPatches, SizeOf(NumberOfPatches));
            SetLength(Patches, NumberOfPatches);
          end;
        end;

        for J := 0 to Result.Sections[I].NumberOfPatches - 1 do
        begin
          with Result.Sections[I].Patches[J] do
          begin
            SourceFile := ReadNullTerm;
            Read(Offset, SizeOf(Offset));
            Read(PCSectionID, SizeOf(PCSectionID));
            Read(PCOffset, SizeOf(PCOffset));
            Read(PatchType, SizeOf(PatchType));
            Read(RPNSize, SizeOf(RPNSize));
            SetLength(RPN, RPNSize);
            Read(RPN[0], RPNSize);
          end;
        end;
      end;
    finally
      Free;
    end;
end;

function RPNToString(const RPN: array of byte; const Syms: array of TRSymbol): string;
var
  I: integer;

  function ReadLong: longword;
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
  while I <= High(RPN) do
  begin
    case RPN[I] of
      $00:
      begin
        Result += '+ ';
        Inc(I);
      end;
      $01:
      begin
        Result += '- ';
        Inc(I);
      end;
      $02:
      begin
        Result += '* ';
        Inc(I);
      end;
      $03:
      begin
        Result += '/ ';
        Inc(I);
      end;
      $04:
      begin
        Result += '% ';
        Inc(I);
      end;
      $05:
      begin
        Result += 'neg ';
        Inc(I);
      end;
      $10:
      begin
        Result += '| ';
        Inc(I);
      end;
      $11:
      begin
        Result += '& ';
        Inc(I);
      end;
      $12:
      begin
        Result += '^ ';
        Inc(I);
      end;
      $13:
      begin
        Result += '~ ';
        Inc(I);
      end;
      $21:
      begin
        Result += '&& ';
        Inc(I);
      end;
      $22:
      begin
        Result += '|| ';
        Inc(I);
      end;
      $23:
      begin
        Result += '! ';
        Inc(I);
      end;
      $30:
      begin
        Result += '== ';
        Inc(I);
      end;
      $31:
      begin
        Result += '!= ';
        Inc(I);
      end;
      $32:
      begin
        Result += '> ';
        Inc(I);
      end;
      $33:
      begin
        Result += '< ';
        Inc(I);
      end;
      $34:
      begin
        Result += '>= ';
        Inc(I);
      end;
      $35:
      begin
        Result += '<= ';
        Inc(I);
      end;
      $40:
      begin
        Result += '<< ';
        Inc(I);
      end;
      $41:
      begin
        Result += '>> ';
        Inc(I);
      end;
      $50:
      begin
        Result += '(bank-sym ' + IntToStr(ReadLong) + ') ';
      end;
      $51:
      begin
        Result += '(bank-section ' + IntToStr(ReadLong) + ') ';
      end;
      $52:
      begin
        Result += 'current-bank';
        Inc(I);
      end;
      $60:
      begin
        Result += 'hram-check';
        Inc(I);
      end;
      $61:
      begin
        Result += 'rst-check';
        Inc(I);
      end;
      $80:
      begin
        Result += '(int ' + IntToStr(ReadLong) + ') ';
      end;
      $81:
      begin
        Result += '(sym ' + Syms[ReadLong].Name + ') ';
      end;
      else
        Exit('INVALID!');
    end;
  end;
end;

function ParseRPN(const RPN: array of byte): TRPN;
var
  I: integer;
  Node: TRPNNode;

  function ReadLong: longword;
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
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Node;
  end;

begin
  SetLength(Result, 0);
  I := Low(RPN);
  while I <= High(RPN) do
  begin
    //Writeln(Format('Parsing %x, len=%d', [RPN[I], Length(Result)]));
    case RPN[I] of
      $00:
      begin
        PushRPN(Nd(rpnPlus));
        Inc(I);
      end;
      $01:
      begin
        PushRPN(Nd(rpnMinus));
        Inc(I);
      end;
      $02:
      begin
        PushRPN(Nd(rpnTimes));
        Inc(I);
      end;
      $03:
      begin
        PushRPN(Nd(rpnDiv));
        Inc(I);
      end;
      $04:
      begin
        PushRPN(Nd(rpnMod));
        Inc(I);
      end;
      $05:
      begin
        PushRPN(Nd(rpnNegate));
        Inc(I);
      end;
      $10:
      begin
        PushRPN(Nd(rpnOr));
        Inc(I);
      end;
      $11:
      begin
        PushRPN(Nd(rpnAnd));
        Inc(I);
      end;
      $12:
      begin
        PushRPN(Nd(rpnXor));
        Inc(I);
      end;
      $13:
      begin
        PushRPN(Nd(rpnComplement));
        Inc(I);
      end;
      $21:
      begin
        PushRPN(Nd(rpnBoolAnd));
        Inc(I);
      end;
      $22:
      begin
        PushRPN(Nd(rpnBoolOr));
        Inc(I);
      end;
      $23:
      begin
        PushRPN(Nd(rpnBoolNeg));
        Inc(I);
      end;
      $30:
      begin
        PushRPN(Nd(rpnEqual));
        Inc(I);
      end;
      $31:
      begin
        PushRPN(Nd(rpnNotEqual));
        Inc(I);
      end;
      $32:
      begin
        PushRPN(Nd(rpnGreater));
        Inc(I);
      end;
      $33:
      begin
        PushRPN(Nd(rpnLess));
        Inc(I);
      end;
      $34:
      begin
        PushRPN(Nd(rpnGreaterEqual));
        Inc(I);
      end;
      $35:
      begin
        PushRPN(Nd(rpnLessEqual));
        Inc(I);
      end;
      $40:
      begin
        PushRPN(Nd(rpnShl));
        Inc(I);
      end;
      $41:
      begin
        PushRPN(Nd(rpnShr));
        Inc(I);
      end;
      $50:
      begin
        Node.Tag := rpnBankSymbol;
        Node.BankSymbol := ReadLong;
        PushRPN(Node);
      end;
      $51:
      begin
        Node.Tag := rpnBankSection;
        Node.BankSection := ReadLong;
        PushRPN(Node);
      end;
      $52:
      begin
        PushRPN(Nd(rpnCurrentBank));
        Inc(I);
      end;
      $60:
      begin
        PushRPN(Nd(rpnHramCheck));
        Inc(I);
      end;
      $61:
      begin
        PushRPN(Nd(rpnRstCheck));
        Inc(I);
      end;
      $80:
      begin
        Node.Tag := rpnInteger;
        Node.IntValue := ReadLong;
        PushRPN(Node);
      end;
      $81:
      begin
        Node.Tag := rpnSymbol;
        Node.SymbolID := ReadLong;
        PushRPN(Node);
      end;
      else
        Die('Got malformed RPN!');
    end;
  end;
end;


procedure PrintObjFile(O: TRObj);
var
  Sym: TRSymbol;
  Sect: TRSection;
  Patch: TRPatch;
  SectSize: integer;
begin
  WriteLn('Symbols:');
  for Sym in O.Symbols do
    Writeln(Format('  %s on %d: type=%d, section=%d, value=%d',
      [Sym.Name, Sym.LineNum, Sym.SymType, Sym.SectionID, Sym.Value]));

  SectSize := 0;
  for Sect in O.Sections do
    if (Sect.Org <> -1) and ((Sect.SectType = ROM0) or (Sect.SectType = ROMX)) then
      Inc(SectSize, Sect.Size);
  Writeln('Absolute sections: ', SectSize, ' bytes');

  SectSize := 0;
  for Sect in O.Sections do
    if (Sect.Org = -1) and ((Sect.SectType = ROM0) or (Sect.SectType = ROMX)) then
      Inc(SectSize, Sect.Size);
  Writeln('Relative sections: ', SectSize, ' bytes');

  WriteLn('Sections:');
  for Sect in O.Sections do
  begin
    Writeln(Format('  %s with size %d: offset=%d, patches=%d, bank=%d',
      [Sect.Name, Sect.Size, Sect.Org, Sect.NumberOfPatches, Sect.Bank]));

    for Patch in Sect.Patches do
      Writeln(Format('    %s: %d ; %s', [Patch.SourceFile, Patch.PatchType,
        RPNToString(Patch.RPN, O.Symbols)]));
  end;
end;

function FindPatch(const Section: TRSection; Index: integer;
  out Patch: TRPatch): boolean;
var
  I: integer;
begin
  for I := Low(Section.Patches) to High(Section.Patches) do
    if Section.Patches[I].Offset = Index then
    begin
      Patch := Section.Patches[I];
      Exit(True);
    end;

  Result := False;
end;

procedure ConvertRGB2SDAS(SourceName, DestName: String);
var
  RObj: TRObj;

  Symbol: TRSymbol;
  Section: TRSection;
  Patch: TRPatch;

  F: Text;

  ValueToWrite: word;
  RPN: TRPN;
  I: word;
  Sct: word;
  WritePos: word;
  Idx: integer;
  CODESEG: string;
  DefaultBank: integer = 1;
  tmp: string;

  old_sym, new_sym: string;

  verbose: boolean = False;

  export_all: boolean = False;
begin
  if not FileExists(sourcename) then
    Die('File not found: %s', [sourcename]);

  old_sym := '';
  new_sym := '';

  CODESEG := '_CODE';
  {for I:= 1 to ParamCount() - 1 do begin
    tmp:= ParamStr(i);
    if (CompareText(tmp, '-v') = 0) then
      verbose:= true
    else if (CompareText(copy(tmp, 1, 2), '-e') = 0) then
      export_all:= true
    else if (CompareText(copy(tmp, 1, 2), '-c') = 0) then begin
      CODESEG:= copy(tmp, 3, length(tmp));
      if length(CODESEG) = 0 then CODESEG:= '_CODE';
      if verbose then writeln('Using CODESEG: ', CODESEG);
    end else if (CompareText(copy(tmp, 1, 2), '-b') = 0) then begin
      DefaultBank:= StrToIntDef(copy(tmp, 3, length(tmp)), DefaultBank);
      if verbose then writeln('Using DefaultBank: ', DefaultBank);
    end else if (CompareText(copy(tmp, 1, 2), '-r') = 0) then begin
      tmp:= copy(tmp, 3, length(tmp));
      Idx:= pos('=', tmp);
      if (idx > 0) then begin
        old_sym:= copy(tmp, 1, idx - 1);
        new_sym:= copy(tmp, idx + 1, length(tmp));
      end;
    end;
  end;}

  RObj := ReadObjFile(sourcename);
  if verbose then
    PrintObjFile(RObj);

  Assign(F, DestName);
  Rewrite(F);
  try
    Idx := 0;
    // pass 1: all imports first
    for I := Low(RObj.Symbols) to High(RObj.Symbols) do
      with RObj.Symbols[I] do
      begin
        if ((SymType and $7f) = SYM_IMPORT) then
        begin
          No := Idx;
          Inc(Idx);
        end
        else
          No := -1;
      end;
    // pass 2: all other (export local only when forced)
    for I := Low(RObj.Symbols) to High(RObj.Symbols) do
      with RObj.Symbols[I] do
      begin
        case (SymType and $7f) of
          SYM_LOCAL: if export_all then
            begin
              No := Idx;
              Inc(Idx);
            end;
          SYM_IMPORT: ;
          SYM_EXPORT:
          begin
            // rename exported symbol if requested
            if (length(old_sym) > 0) and (Name = old_sym) then
              Name := new_sym;
            No := Idx;
            Inc(Idx);
          end;
          else
            Die('Unsupported symbol type: %d', [SymType and $7f]);
        end;
      end;

    // output object header
    Writeln(F, 'XL2');
    Writeln(F, Format('H %x areas %x global symbols', [RObj.NumberOfSections, idx]));
    Writeln(F, Format('M %s', [StringReplace(ExtractFileName(sourcename),
      '.', '_', [rfReplaceAll])]));
    Writeln(F, 'O -mgbz80');

    // output all imported symbols
    for I := Low(RObj.Symbols) to High(RObj.Symbols) do
      with RObj.Symbols[I] do
        if (SymType = SYM_IMPORT) then
          Writeln(F, Format('S %s Ref%.4x',
            [StringReplace(Name, '.', '____', [rfReplaceAll]), 0]));

    // output all sections and other symbols
    for Section in RObj.Sections do
    begin
      if Section.Org = -1 then
        case Section.SectType of
          ROM0: Writeln(F, Format('A %s size %x flags 0 addr 0',
              [CODESEG, Section.Size]));
          ROMX: if (DefaultBank = 0) then
              Writeln(F, Format('A %s size %x flags 0 addr 0', [CODESEG, Section.Size]))
            else
              Writeln(F, Format('A _CODE_%d size %x flags 0 addr 0',
                [max(DefaultBank, Section.Bank), Section.Size]));
          else
            Writeln(F, Format('A _DATA size %x flags 0 addr 0', [Section.Size]));
        end
      else
        Die('absolute sections currently unsupported: %s', [Section.Name]);

      for Symbol in RObj.Symbols do
        if Symbol.SectionID = Section.ID then
        begin
          if (Symbol.SymType <> SYM_IMPORT) and (Symbol.No >= 0) then
            Writeln(F, Format('S %s Def%.4x',
              [StringReplace(Symbol.Name, '.', '____', [rfReplaceAll]), Symbol.Value]));
        end;
    end;

    // convert object itself
    for Sct := Low(RObj.Sections) to High(RObj.Sections) do
    begin
      Section := RObj.Sections[Sct];

      if (Section.SectType <> ROMX) and (Section.SectType <> ROM0) then
        Continue;
      if Length(Section.Data) <= 0 then
        Continue;

      I := Low(Section.Data);
      while I <= High(Section.Data) do
      begin
        WritePos := I;
        if (Section.Org <> -1) then
          Inc(WritePos, Section.Org);

        if FindPatch(Section, I, Patch) then
        begin
          case Patch.PatchType of
            PATCH_LE_WORD:
            begin
              RPN := ParseRPN(Patch.RPN);
              Symbol := RObj.Symbols[RPN[0].SymbolID];
              ValueToWrite := Symbol.Value;
              if RPN[High(RPN)].Tag = rpnPlus then
                Inc(ValueToWrite, RPN[1].IntValue);

              if (Symbol.SymType = SYM_IMPORT) then
              begin
                if (Symbol.No < 0) then
                  Die('Trying to reference eliminated symbol');
                Writeln(F, Format('T %.2x %.2x %.2x %.2x',
                  [lo(WritePos), hi(WritePos), lo(ValueToWrite), hi(ValueToWrite)]));
                Writeln(F,
                  Format('R 00 00 %.2x %.2x 02 02 %.2x %.2x', [lo(Sct), hi(Sct),
                  lo(Symbol.No), hi(Symbol.No)]));
              end
              else
              begin
                Writeln(F, Format('T %.2x %.2x %.2x %.2x',
                  [lo(WritePos), hi(WritePos), lo(ValueToWrite), hi(ValueToWrite)]));
                Writeln(F,
                  Format('R 00 00 %.2x %.2x 00 02 %.2x %.2x', [lo(Sct), hi(Sct),
                  lo(Symbol.SectionID), hi(Symbol.SectionID)]));
              end;
              Inc(I, 2);
            end;
            PATCH_JR:
            begin
              RPN := ParseRPN(Patch.RPN);
              if Length(RPN) > 1 then
                Die('Not handling bigger RPN on JR');
              Symbol := RObj.Symbols[RPN[0].SymbolID];
              Writeln(F, Format('T %.2x %.2x %.2x',
                [lo(WritePos), hi(WritePos), byte(Symbol.Value - I - 1)]));
              Writeln(F, Format('R 00 00 %.2x %.2x',
                [lo(Sct), hi(Sct)]));
              Inc(I);
            end
            else
              Die('Unsupported patch type: %d', [Patch.PatchType]);
          end;
        end
        else
        begin
          Writeln(F, Format('T %.2x %.2x %.2x',
            [lo(WritePos), hi(WritePos), Section.Data[I]]));
          Writeln(F, Format('R 00 00 %.2x %.2x', [lo(Sct), hi(Sct)]));
          Inc(I);
        end;
      end;
    end;

    //Writeln('rgb2sdas converting ',sourcename,' --> ',sourcename,'.o result: success!');
  finally
    Close(F);
  end;
end;

begin
end.

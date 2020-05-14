{+-----------------------------------------------------------------------------+
 | Description: Gameboy Z80-CPU emulation
 | last changes: 02.11.2000
 |
 | http://www.tu-ilmenau.de/~hackbart
 +----------------------------------------------------------------------------+}

unit Z80CPU;

{$MODE Delphi}
{$ASMMODE Intel}

interface

var
  z80: array[0..511] of function: byte;

implementation

uses vars, machine;

// temporary variables
var
  b: Pair;
  cycle: byte;
  b1: byte;
  w1, w2: word;

function NOP: byte;
begin
  Result := 4;
end;

function LD_BC_WORD: byte;
begin
  {asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     bc.l,AL
           INC     word ptr pc.w
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     bc.h,AL
           INC     word ptr pc.w
  end;}

  bc.l := speekb(pc.w);
  Inc(pc.w);
  bc.h := speekb(pc.w);
  Inc(pc.w);

  Result := 12;
end;

function LD_xBC_A: byte;
begin
  {asm
           MOV     AX,bc.w
           PUSH    dword ptr af.h
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;}

  spokeb(bc.w, af.h);

  Result := 8;
end;

function INC_BC: byte;
begin
  asm
           INC     bc.w
  end;

  Result := 8;
end;

function INC_B: byte;
begin
  asm
           INC     bc.h
           LAHF
           AND     EAX,$5100
           AND     af.l,1
           OR      af.l,AH
  end;

  Result := 4;
end;

function DEC_B: byte;
begin
  asm
           DEC     bc.h
           LAHF
           AND     af.l,1
           AND     EAX,$5500
           OR      EAX,$400
           OR      af.l,AH
  end;

  Result := 4;

end;

function LD_B_BYTE: byte;
begin
  {asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     bc.h,AL
           INC     word ptr pc.w
  end;}

  bc.h := speekb(pc.w);
  Inc(pc.w);

  Result := 8;
end;

function RLCA: byte;
begin
  asm
           ROL     af.h,1
           LAHF
           AND     EAX,$100
           MOV     af.l,AH
  end;


  Result := 4;
end;

function EX_AF_AF: byte;
begin
  {asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           INC     word ptr pc.w
           PUSH    EAX

           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           INC     word ptr pc.w
           XCHG    AH,AL
           POP     EBX
           MOV     AL,BL
           MOV     BX,sp_.w
           PUSH    EBX
           PUSH    EAX

           PUSH    EBX
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]

           POP     EAX
           POP     EBX
           INC     EAX
           XCHG    BH,BL
           PUSH    EBX
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;}
  Result := 20;
end;

function ADD_HL_BC: byte;
begin
  asm
           AND     af.l,64
           MOV     AX,hl.w
           MOV     BX,bc.w


           MOV     ECX,EAX
           XOR     ECX,EBX

           ADD     AX,BX
           JNC     @1
           OR      af.l,1
           @1:
           XOR     ECX,EAX
           TEST    ECX,4096
           JZ      @ende
           OR      af.l,16
           @ende:
           MOV     hl.w,AX

  end;
  Result := 8;
end;

function LD_A_xBC: byte;
begin
  {asm
           MOV     AX,bc.w
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     af.h,AL
  end;}

  af.h := speekb(bc.w);

  Result := 8;
end;

function DEC_BC: byte;
begin
  asm
           DEC     bc.w
  end;

  Result := 8;
end;

function INC_C: byte;
begin
  asm
           INC     bc.l
           LAHF
           AND     EAX,$5100
           AND     af.l,1
           OR      af.l,AH
  end;

  Result := 4;
end;

function DEC_C: byte;
begin
  asm
           DEC     bc.l
           LAHF
           AND     af.l,1
           AND     EAX,$5500
           OR      EAX,$400
           OR      af.l,AH
  end;

  Result := 4;
end;

function LD_C_BYTE: byte;
begin
  {asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     bc.l,AL
           INC     word ptr pc.w
  end;}

  bc.l := speekb(pc.w);
  Inc(pc.w);

  Result := 8;
end;

function RRCA: byte;
begin
  asm
           ROR     af.h,1
           LAHF
           AND     EAX,$100
           MOV     af.l,AH
  end;


  Result := 4;
end;

function DJNZ: byte;
begin
  stop_mode := 1;
  Inc(pc.w);
  Result := 10;
end;

function LD_DE_WORD: byte;
begin
  {asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     de.l,AL
           INC     word ptr pc.w

           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     de.h,AL
           INC     word ptr pc.w
  end;}

  de.l := speekb(pc.w);
  Inc(pc.w);
  de.h := speekb(pc.w);
  Inc(pc.w);

  Result := 12;
end;

function LD_xDE_A: byte;
begin
  {asm
           MOV     AX,de.w
           PUSH    dword ptr af.h
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;}

  spokeb(de.w, af.h);

  Result := 8;
end;

function INC_DE: byte;
begin
  asm
           INC     de.w
  end;
  Result := 8;
end;

function INC_D: byte;
begin
  asm
           INC     de.h
           LAHF
           AND     EAX,$5100
           AND     af.l,1
           OR      af.l,AH
  end;

  Result := 4;
end;

function DEC_D: byte;
begin
  asm
           DEC     de.h
           LAHF
           AND     af.l,1
           AND     EAX,$5500
           OR      EAX,$400
           OR      af.l,AH
  end;

  Result := 4;
end;

function LD_D_BYTE: byte;
begin
  {asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     de.h,AL
           INC     word ptr pc.w
  end;}

  de.h := speekb(pc.w);
  Inc(pc.w);

  Result := 8;
end;

function RLA: byte;
begin
  asm
           MOV     AH,af.l
           AND     EAX,$100
           SAHF
           RCL     af.h,1
           LAHF
           AND     EAX,$100
           MOV     af.l,AH
  end;

  Result := 4;
end;

function JR: byte;
begin
  {asm
           XOR     EAX,EAX
           MOV     AX,pc.w
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           INC     word ptr pc.w
           CBW
           ADD     pc.w,AX
  end;}

  Inc(pc.w, speekb(pc.w)+1);

  Result := 8;
end;

function ADD_HL_DE: byte;
begin
  asm
           AND     af.l,64
           MOV     AX,hl.w
           MOV     BX,de.w


           MOV     ECX,EAX
           XOR     CX,BX

           ADD     AX,BX
           JNC     @1
           OR      af.l,1
           @1:
           XOR     CX,AX
           TEST    CX,4096
           JZ      @ende
           OR      af.l,16
           @ende:
           MOV     hl.w,AX

  end;
  Result := 8;
end;

function LD_A_xDE: byte;
begin
  {asm
           MOV     AX,de.w
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     af.h,AL
  end;}

  af.h := speekb(de.w);

  Result := 8;
end;

function DEC_DE: byte;
begin
  asm
           DEC     de.w
  end;

  Result := 8;
end;

function INC_E: byte;
begin
  asm
           INC     de.l
           LAHF
           AND     EAX,$5100
           AND     af.l,1
           OR      af.l,AH
  end;

  Result := 4;
end;

function DEC_E: byte;
begin
  asm
           DEC     de.l
           LAHF
           AND     af.l,1
           AND     EAX,$5500
           OR      EAX,$400
           OR      af.l,AH
  end;

  Result := 4;
end;

function LD_E_BYTE: byte;
begin
  {asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     de.l,AL
           INC     word ptr pc.w
  end;}

  de.l := speekb(pc.w);
  Inc(pc.w);

  Result := 8;
end;

function RRA: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           RCR     af.h,1
           LAHF
           AND     AH,1
           MOV     af.l,AH
  end;
  Result := 4;
end;

function JR_NZ: byte;
begin
  b1 := speekb(pc.w);
  Inc(pc.w);
  asm
           //PUSH    dword ptr pc.w
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           //INC     word ptr pc.w
           MOV     AH, 0
           MOV     AL, b1
           TEST    af.l,64
           MOV     cycle,8
           JNZ     @ende
           MOV     cycle,12
           CBW
           ADD     pc.w,AX
           @ende:
  end;
  Result := cycle;
end;

function LD_HL_WORD: byte;
begin
  {asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     hl.l,AL
           INC     word ptr pc.w
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     hl.h,AL
           INC     word ptr pc.w
  end;}

  hl.l := speekb(pc.w);
  Inc(pc.w);
  hl.h := speekb(pc.w);
  Inc(pc.w);

  Result := 12;
end;

function LD_xWORD_HL: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    EAX
           PUSH    dword ptr af.h
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
           POP     EAX
           INC     EAX
           MOV     hl.w,AX
  end;}

  spokeb(hl.w, af.h);
  Inc(hl.w);

  Result := 8;
end;

function INC_HL: byte;
begin
  asm
           INC     hl.w
  end;
  Result := 8;
end;

function INC_H: byte;
begin
  asm
           INC     hl.h
           LAHF
           AND     EAX,$5100
           AND     af.l,1
           OR      af.l,AH
  end;

  Result := 4;
end;

function DEC_H: byte;
begin
  asm
           DEC     hl.h
           LAHF
           AND     af.l,1
           AND     EAX,$5500
           OR      EAX,$400
           OR      af.l,AH
  end;

  Result := 4;
end;

function LD_H_BYTE: byte;
begin
  {asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     hl.h,AL
           INC     word ptr pc.w
  end;}

  hl.h := speekb(pc.w);
  Inc(pc.w);

  Result := 8;
end;

function DAA: byte;
begin
  asm
           MOV     AH,af.l
           MOV     BL,AH
           AND     BL,4
           SAHF
           MOV     AL,af.h
           JP      @dec_adjust
           //DAA
           JMP     @ddld

           @dec_adjust:
           //DAS

           @ddld:
           MOV     af.h,AL
           LAHF
           AND     AH,65
           OR      AH,BL
           MOV     af.l,AH
  end;

  Result := 4;
end;

function JR_Z: byte;

begin
  b1 := speekb(pc.w);
  Inc(pc.w);
  asm
           //PUSH    dword ptr pc.w
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           //INC     word ptr pc.w
           MOV     AH, 0
           MOV     AL, b1
           TEST    af.l,64
           MOV     cycle,8
           JZ      @ende
           MOV     cycle,12
           CBW
           ADD     pc.w,AX
           @ende:
  end;

  Result := cycle;
end;

function ADD_HL_HL: byte;
begin
  asm
           AND     af.l,64
           MOV     AX,hl.w
           XOR     ECX,ECX

           ADD     AX,AX
           JNC     @1
           OR      af.l,1
           @1:
           XOR     ECX,EAX
           TEST    CX,4096
           JZ      @ende
           OR      af.l,16
           @ende:
           MOV     hl.w,AX

  end;

  Result := 8;
end;

function LD_HL_xWORD: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    EAX
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     af.h,AL
           POP     EAX
           INC     AX
           MOV     hl.w,AX
  end;}

  af.h := speekb(hl.w);
  Inc(hl.w);

  Result := 8;
end;

function DEC_HL: byte;
begin
  asm
           DEC     hl.w
  end;

  Result := 8;
end;

function INC_L: byte;
begin
  asm
           INC     hl.l
           LAHF
           AND     EAX,$5100
           AND     af.l,1
           OR      af.l,AH
  end;

  Result := 4;
end;

function DEC_L: byte;
begin
  asm
           DEC     hl.l
           LAHF
           AND     af.l,1
           AND     EAX,$5500
           OR      EAX,$400
           OR      af.l,AH
  end;

  Result := 4;
end;

function LD_L_BYTE: byte;
begin
  {asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           INC     word ptr pc.w
           MOV     hl.l,AL
  end;}

  Inc(pc.w);
  hl.l := speekb(pc.w);

  Result := 8;
end;

function CPL: byte;
begin
  asm
           XOR     af.h,255
           OR      af.l,$14
  end;

  Result := 4;
end;

function JR_NC: byte;

begin
  b1 := speekb(pc.w);
  Inc(pc.w);
  asm
           //PUSH    dword ptr pc.w
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           //INC     word ptr pc.w
           MOV     AH, 0
           MOV     AL, b1
           TEST    af.l,1
           MOV     cycle,8
           JNZ     @ende
           MOV     cycle,12
           CBW
           ADD     pc.w,AX
           @ende:
  end;

  Result := cycle;
end;

function LD_SP_WORD: byte;
begin
  {asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           INC     word ptr pc.w
           PUSH    EAX
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           INC     word ptr pc.w
           SHL     EAX,8
           POP     EBX
           MOV     AL,BL

           MOV     sp_.w,AX
  end;}

  sp_.w := wordpeek(pc.w);
  inc(pc.w,2);

  Result := 12;
end;

function LD_xWORD_A: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    EAX
           PUSH    dword ptr af.h
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
           POP     EAX
           DEC     AX
           MOV     hl.w,AX
  end;}

  spokeb(hl.w, af.h);
  Dec(hl.w);

  Result := 8;
end;

function INC_SP: byte;
begin
  asm
           INC     sp_.w
  end;

  Result := 8;
end;

function INC_xHL: byte;
begin
  w1 := speekb(hl.w);
  asm
           MOV     AX,hl.w
           PUSH    RAX
           // PUSH    EAX
           // CALL    speekb
           // LEA     ESP,[ESP+4]
           MOV     AX, w1
           INC     AL
           LAHF
           AND     AH,$51
           AND     af.l,1
           OR      af.l,AH
           POP     RBX
           //PUSH    EAX
           //PUSH    EBX
           //CALL    SpokeB
           //LEA     ESP,[ESP+8]
           MOV w1, BX
           MOV w2, AX
  end;
  spokeb(w1, w2);

  Result := 12;
end;

function DEC_xHL: byte;
begin
  w1 := speekb(hl.w);
  asm
           MOV     AX,hl.w
           PUSH    RAX
           //PUSH    EAX
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1
           DEC     AL
           LAHF
           AND     af.l,1
           AND     AH,$55
           OR      EAX,$400
           OR      af.l,AH
           POP     RBX
           //PUSH    EAX
           //PUSH    EBX
           //CALL    SpokeB
           //LEA     ESP,[ESP+8]
           MOV w1, BX
           MOV w2, AX
  end;
  spokeb(w1, w2);

  Result := 12;
end;

function LD_xHL_BYTE: byte;
begin
  {asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           INC     word ptr pc.w
           PUSH    EAX
           MOV     AX,hl.w
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;}

  w1 := speekb(pc.w);
  Inc(pc.w);
  spokeb(hl.w, w1);

  Result := 12;
end;

function SCF: byte;
begin
  asm
           AND     af.l,64
           OR      af.l,1
  end;

  Result := 4;
end;

function JR_C: byte;

begin
  w1 := speekb(pc.w);
  asm
           //PUSH    dword ptr pc.w
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     EAX, w1
           INC     word ptr pc.w
           TEST    af.l,1
           MOV     cycle,8
           JZ      @ende
           MOV     cycle,12
           CBW
           ADD     pc.w,AX
           @ende:
  end;

  Result := cycle;
end;

function ADD_HL_SP: byte;
begin
  asm
           AND     af.l,64
           MOV     AX,hl.w
           MOV     BX,sp_.w


           MOV     ECX,EAX
           XOR     ECX,EBX

           ADD     AX,BX
           JNC     @1
           OR      af.l,1
           @1:
           XOR     ECX,EAX
           TEST    ECX,4096
           JZ      @ende
           OR      af.l,16
           @ende:
           MOV     hl.w,AX
  end;
  Result := 8;
end;

function LD_A_xWORD: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    EAX
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     af.h,AL
           POP     EAX
           DEC     AX
           MOV     hl.w,AX
  end;}

  af.h := speekb(hl.w);
  Dec(hl.w);

  Result := 8;
end;

function DEC_SP: byte;
begin
  asm
           DEC     sp_.w
  end;
  Result := 8;
end;

function INC_A: byte;
begin
  asm
           INC     af.h
           LAHF
           AND     EAX,$5100
           AND     af.l,1
           OR      af.l,AH
  end;

  Result := 4;
end;

function DEC_A: byte;
begin
  asm
           DEC     af.h
           LAHF
           AND     af.l,1
           AND     EAX,$5500
           OR      EAX,$400
           OR      af.l,AH
  end;

  Result := 4;
end;

function LD_A_BYTE: byte;
begin
  {asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     af.h,AL
           INC     word ptr pc.w
  end;}

  af.h := speekb(pc.w);
  Inc(pc.w);

  Result := 8;
end;

function CCF: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           CMC
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 4;
end;

function LD_B_B: byte;
begin
  Result := 4;
end;

function LD_B_C: byte;
begin
  bc.h := bc.l;
  Result := 4;
end;

function LD_B_D: byte;
begin
  bc.h := de.h;
  Result := 4;
end;

function LD_B_E: byte;
begin
  bc.h := de.l;
  Result := 4;
end;

function LD_B_H: byte;
begin
  bc.h := hl.h;
  Result := 4;
end;

function LD_B_L: byte;
begin
  bc.h := hl.l;
  Result := 4;
end;

function LD_B_xHL: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     bc.h,AL
  end;}

  bc.h := speekb(hl.w);

  Result := 8;
end;

function LD_B_A: byte;
begin
  bc.h := af.h;
  Result := 4;
end;

function LD_C_B: byte;
begin
  bc.l := bc.h;
  Result := 4;
end;

function LD_C_C: byte;
begin
  Result := 4;
end;

function LD_C_D: byte;
begin
  bc.l := de.h;
  Result := 4;
end;

function LD_C_E: byte;
begin
  bc.l := de.l;
  Result := 4;
end;

function LD_C_H: byte;
begin
  bc.l := hl.h;
  Result := 4;
end;

function LD_C_L: byte;
begin
  bc.l := hl.l;
  Result := 4;
end;

function LD_C_xHL: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     bc.l,AL
  end;}

  bc.l := speekb(hl.w);

  Result := 8;
end;

function LD_C_A: byte;
begin
  bc.l := af.h;
  Result := 4;
end;

function LD_D_B: byte;
begin
  de.h := bc.h;

  Result := 4;
end;

function LD_D_C: byte;
begin
  de.h := bc.l;
  Result := 4;
end;

function LD_D_D: byte;
begin
  Result := 4;
end;

function LD_D_E: byte;
begin
  de.h := de.l;
  Result := 4;
end;

function LD_D_H: byte;
begin
  de.h := hl.h;
  Result := 4;
end;

function LD_D_L: byte;
begin
  de.h := hl.l;
  Result := 4;
end;

function LD_D_xHL: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     de.h,AL
  end;}

  de.h := speekb(hl.w);

  Result := 8;
end;

function LD_D_A: byte;
begin
  de.h := af.h;
  Result := 4;
end;

function LD_E_B: byte;
begin
  de.l := bc.h;
  Result := 4;
end;

function LD_E_C: byte;
begin
  de.l := bc.l;
  Result := 4;
end;

function LD_E_D: byte;
begin
  de.l := de.h;
  Result := 4;
end;

function LD_E_E: byte;
begin
  Result := 4;
end;

function LD_E_H: byte;
begin
  de.l := hl.h;
  Result := 4;
end;

function LD_E_L: byte;
begin
  de.l := hl.l;
  Result := 4;
end;

function LD_E_xHL: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     de.l,AL
  end;}

  de.l := speekb(hl.w);

  Result := 8;
end;

function LD_E_A: byte;
begin
  de.l := af.h;
  Result := 4;
end;

function LD_H_B: byte;
begin
  hl.h := bc.h;
  Result := 4;
end;

function LD_H_C: byte;
begin
  hl.h := bc.l;
  Result := 4;
end;

function LD_H_D: byte;
begin
  hl.h := de.h;
  Result := 4;
end;

function LD_H_E: byte;
begin
  hl.h := de.l;
  Result := 4;
end;

function LD_H_H: byte;
begin
  Result := 4;
end;

function LD_H_L: byte;
begin
  hl.h := hl.l;
  Result := 4;
end;

function LD_H_xHL: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     hl.h,AL
  end;}

  hl.h := speekb(hl.w);

  Result := 8;
end;

function LD_H_A: byte;
begin
  hl.h := af.h;
  Result := 4;
end;

function LD_L_B: byte;
begin
  hl.l := bc.h;
  Result := 4;
end;

function LD_L_C: byte;
begin
  hl.l := bc.l;
  Result := 4;
end;

function LD_L_D: byte;
begin
  hl.l := de.h;
  Result := 4;
end;

function LD_L_E: byte;
begin
  hl.l := de.l;
  Result := 4;
end;

function LD_L_H: byte;
begin
  hl.l := hl.h;
  Result := 4;
end;

function LD_L_L: byte;
begin
  Result := 4;
end;

function LD_L_xHL: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     hl.l,AL
  end;}

  hl.l := speekb(hl.w);

  Result := 8;
end;

function LD_L_A: byte;
begin
  hl.l := af.h;
  Result := 4;
end;

function LD_xHL_B: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    dword ptr bc.h
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;}

  spokeb(hl.w, bc.h);

  Result := 8;
end;

function LD_xHL_C: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    dword ptr bc.l
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;}

  spokeb(hl.w, bc.l);

  Result := 8;
end;

function LD_xHL_D: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    dword ptr de.h
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;}

  spokeb(hl.w, de.h);

  Result := 8;
end;

function LD_xHL_E: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    dword ptr de.l
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;}

  spokeb(hl.w, de.l);

  Result := 8;
end;

function LD_xHL_H: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    dword ptr hl.h
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;}

  spokeb(hl.w, hl.h);

  Result := 8;
end;

function LD_xHL_L: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    dword ptr hl.l
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;}

  spokeb(hl.w, hl.l);

  Result := 8;
end;

function HALT: byte;
begin
  if gbr_ime then
    halt_mode := 1
  else
    halt_mode := 2;
  Result := 4;
end;

function LD_xHL_A: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    dword ptr af.h
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;}

  spokeb(hl.w, af.h);

  Result := 8;
end;

function LD_A_B: byte;
begin
  af.h := bc.h;
  Result := 4;
end;

function LD_A_C: byte;
begin
  af.h := bc.l;
  Result := 4;
end;

function LD_A_D: byte;
begin
  af.h := de.h;
  Result := 4;
end;

function LD_A_E: byte;
begin
  af.h := de.l;
  Result := 4;
end;

function LD_A_H: byte;
begin
  af.h := hl.h;
  Result := 4;
end;

function LD_A_L: byte;
begin
  af.h := hl.l;
  Result := 4;
end;

function LD_A_xHL: byte;
begin
  {asm
           MOV     AX,hl.w
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     af.h,AL
  end;}

  af.h := speekb(hl.w);

  Result := 8;
end;

function LD_A_A: byte;
begin
  Result := 4;
end;

function ADD_B: byte;
begin
  asm
           MOV     AL,bc.h
           ADD     af.h,AL
           LAHF
           AND     EAX,$5100
           MOV     af.l,AH

  end;
  Result := 4;
end;

function ADD_C: byte;
begin
  asm
           MOV     AL,bc.l
           ADD     af.h,AL
           LAHF
           AND     AH,81
           MOV     af.l,AH

  end;
  Result := 4;
end;

function ADD_D: byte;
begin
  asm
           MOV     AL,de.h
           ADD     af.h,AL
           LAHF
           AND     AH,81
           MOV     af.l,AH

  end;

  Result := 4;
end;

function ADD_E: byte;
begin
  asm
           MOV     AL,de.l
           ADD     af.h,AL
           LAHF
           AND     AH,81
           MOV     af.l,AH

  end;

  Result := 4;
end;

function ADD_H: byte;
begin
  asm
           MOV     AL,hl.h
           ADD     af.h,AL
           LAHF
           AND     AH,81
           MOV     af.l,AH

  end;

  Result := 4;
end;

function ADD_L: byte;
begin
  asm
           MOV     AL,hl.l
           ADD     af.h,AL
           LAHF
           AND     AH,81
           MOV     af.l,AH

  end;
  Result := 4;
end;

function ADD_xHL: byte;
begin
  w1 := speekb(hl.w);
  asm
           //MOV     AX,hl.w
           //PUSH    EAX
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV       AX, w1

           ADD     af.h,AL
           LAHF
           AND     EAX,$5100
           MOV     af.l,AH

  end;

  Result := 8;
end;

function ADD_A: byte;
begin
  asm
           MOV     AL,af.h
           ADD     af.h,AL
           LAHF
           AND     AH,81
           MOV     af.l,AH

  end;

  Result := 4;
end;

function ADC_B: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,bc.h
           ADC     af.h,AL
           LAHF
           AND     EAX,$5100
           MOV     af.l,AH

  end;

  Result := 4;
end;

function ADC_C: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,bc.l
           ADC     af.h,AL
           LAHF
           AND     EAX,$5100
           MOV     af.l,AH

  end;

  Result := 4;
end;

function ADC_D: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,de.h
           ADC     af.h,AL
           LAHF
           AND     EAX,$5100
           MOV     af.l,AH

  end;
  Result := 4;
end;

function ADC_E: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,de.l
           ADC     af.h,AL
           LAHF
           AND     EAX,$5100
           MOV     af.l,AH

  end;

  Result := 4;
end;

function ADC_H: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,hl.h
           ADC     af.h,AL
           LAHF
           AND     EAX,$5100
           MOV     af.l,AH

  end;

  Result := 4;
end;

function ADC_L: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,hl.l
           ADC     af.h,AL
           LAHF
           AND     EAX,$5100
           MOV     af.l,AH

  end;

  Result := 4;
end;

function ADC_xHL: byte;
begin
  w1 := speekb(hl.w);
  asm
           //MOV     AX,hl.w
           //PUSH    EAX
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1
           MOV     AH,af.l
           AND     AH,1
           SAHF

           ADC     af.h,AL
           LAHF
           AND     EAX,$5100
           MOV     af.l,AH

  end;

  Result := 8;
end;

function ADC_A: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,af.h
           ADC     af.h,AL
           LAHF
           AND     EAX,$5100
           MOV     af.l,AH

  end;

  Result := 4;
end;

function SUB_B: byte;
begin
  asm
           MOV     AL,bc.h
           SUB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;
  Result := 4;
end;

function SUB_C: byte;
begin
  asm
           MOV     AL,bc.l
           SUB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;

  Result := 4;
end;

function SUB_D: byte;
begin
  asm
           MOV     AL,de.h
           SUB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;

  Result := 4;
end;

function SUB_E: byte;
begin
  asm
           MOV     AL,de.l
           SUB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;


  Result := 4;
end;

function SUB_H: byte;
begin
  asm
           MOV     AL,hl.h
           SUB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;

  Result := 4;
end;

function SUB_L: byte;
begin
  asm
           MOV     AL,hl.l
           SUB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;

  Result := 4;
end;

function SUB_xHL: byte;
begin
  w1 := speekb(hl.w);
  asm
           //MOV     AX,hl.w
           //PUSH    EAX
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1

           SUB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;

  Result := 8;
end;

function SUB_A: byte;
begin
  asm
           MOV     AL,af.h
           SUB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;
  Result := 4;

end;

function SBC_B: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,bc.h
           SBB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;

  Result := 4;

end;

function SBC_C: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,bc.l
           SBB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;

  Result := 4;
end;

function SBC_D: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,de.h
           SBB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;

  Result := 4;
end;

function SBC_E: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,de.l
           SBB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;

  Result := 4;
end;

function SBC_H: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,hl.h
           SBB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;

  Result := 4;
end;

function SBC_L: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,hl.l
           SBB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;

  Result := 4;
end;

function SBC_xHL: byte;
begin
  w1 := speekb(hl.w);
  asm
           //MOV     AX,hl.w
           //PUSH    EAX
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1

           MOV     AH,af.l
           SAHF

           SBB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;
  Result := 8;
end;

function SBC_A: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,af.h
           SBB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;
  Result := 4;
end;

function AND_B: byte;
begin
  asm
           MOV     AL,bc.h
           AND     af.h,AL
           LAHF
           AND     EAX,$4000
           OR      EAX,$1000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function AND_C: byte;
begin
  asm
           MOV     AL,bc.l
           AND     af.h,AL
           LAHF
           AND     EAX,$4000
           OR      EAX,$1000
           MOV     af.l,AH
  end;

  Result := 4;
end;

function AND_D: byte;
begin
  asm
           MOV     AL,de.h
           AND     af.h,AL
           LAHF
           AND     EAX,$4000
           OR      EAX,$1000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function AND_E: byte;
begin
  asm
           MOV     AL,de.l
           AND     af.h,AL
           LAHF
           AND     EAX,$4000
           OR      EAX,$1000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function AND_H: byte;
begin
  asm
           MOV     AL,hl.h
           AND     af.h,AL
           LAHF
           AND     EAX,$4000
           OR      EAX,$1000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function AND_L: byte;
begin
  asm
           MOV     AL,hl.l
           AND     af.h,AL
           LAHF
           AND     EAX,$4000
           OR      EAX,$1000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function AND_xHL: byte;
begin
  w1 := speekb(hl.w);
  asm
           //MOV     AX,hl.w
           //PUSH    EAX
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1

           AND     af.h,AL
           LAHF
           AND     EAX,$4000
           OR      EAX,$1000
           MOV     af.l,AH
  end;
  Result := 8;
end;

function AND_A: byte;
begin
  asm
           MOV     AL,af.h
           AND     af.h,AL
           LAHF
           AND     EAX,$4000
           OR      EAX,$1000 // AH
           MOV     af.l,AH
  end;
  Result := 4;
end;

function XOR_B: byte;
begin
  asm
           MOV     AL,bc.h
           XOR     af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function XOR_C: byte;
begin
  asm
           MOV     AL,bc.l
           XOR     af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function XOR_D: byte;
begin
  asm
           MOV     AL,de.h
           XOR     af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function XOR_E: byte;
begin
  asm
           MOV     AL,de.l
           XOR     af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function XOR_H: byte;
begin
  asm
           MOV     AL,hl.h
           XOR     af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function XOR_L: byte;
begin
  asm
           MOV     AL,hl.l
           XOR     af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function XOR_xHL: byte;
begin
  w1 := speekb(hl.w);
  asm
           //MOV     AX,hl.w
           //PUSH    EAX
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1

           XOR     af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;
  Result := 8;
end;

function XOR_A: byte;
begin

  af.h := 0;
  af.l := 64;

  Result := 4;
end;

function OR_B: byte;
begin
  asm
           MOV     AL,bc.h
           OR      af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function OR_C: byte;
begin
  asm
           MOV     AL,bc.l
           OR      af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;

  Result := 4;
end;

function OR_D: byte;
begin
  asm
           MOV     AL,de.h
           OR      af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;

  Result := 4;
end;

function OR_E: byte;
begin
  asm
           MOV     AL,de.l
           OR      af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function OR_H: byte;
begin
  asm
           MOV     AL,hl.h
           OR      af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function OR_L: byte;
begin
  asm
           MOV     AL,hl.l
           OR      af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function OR_xHL: byte;
begin
  w1 := speekb(hl.w);

  asm
           //MOV     AX,hl.w
           //PUSH    EAX
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1


           OR      af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;
  Result := 8;
end;

function OR_A: byte;
begin
  asm
           MOV     AL,af.h
           OR      af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;
  Result := 4;
end;

function CP_B: byte;
begin
  asm
           MOV     AL,bc.h
           CMP     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH
  end;

  Result := 4;
end;

function CP_C: byte;
begin
  asm
           MOV     AL,bc.l
           CMP     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;

  Result := 4;
end;

function CP_D: byte;
begin
  asm
           MOV     AL,de.h
           CMP     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;

  Result := 4;
end;

function CP_E: byte;
begin
  asm
           MOV     AL,de.l
           CMP     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH
  end;
  Result := 4;
end;

function CP_H: byte;
begin
  asm
           MOV     AL,hl.h
           CMP     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH
  end;
  Result := 4;
end;

function CP_L: byte;
begin
  asm
           MOV     AL,hl.l
           CMP     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH
  end;
  Result := 4;
end;

function CP_xHL: byte;
begin
  w1 := speekb(hl.w);
  asm
           //MOV     AX,hl.w
           //PUSH    EAX
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1


           CMP     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH
  end;
  Result := 8;
end;

function CP_A: byte;
begin
  asm
           MOV     AL,af.h
           CMP     af.h,AL
           LAHF

           OR      EAX,$400
           MOV     af.l,AH
  end;
  Result := 4;
end;

function RET_NZ: byte;

begin
  //w1 := speekb(sp_.w);
  // TODO
  asm
           TEST    af.l,64
           MOV     cycle,8
           JNZ     @ende
           PUSH    dword ptr sp_.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     AX, w1
           INC     word ptr sp_.w
           PUSH    EAX
           PUSH    dword ptr sp_.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           INC     word ptr sp_.w
           SHL     EAX,8
           POP     EBX
           MOV     AL,BL
           MOV     pc.w,AX
           MOV     cycle,20
           @ende:
  end;

  Result := cycle;
end;

function POP_BC: byte;
begin
  bc.l := speekb(sp_.w);
  Inc(sp_.w);
  bc.h := speekb(sp_.w);
  Inc(sp_.w);
  Result := 12;
end;

function JP_NZ: byte;
begin
  if (af.l and 64) = 0 then
  begin
    w1 := speekb(pc.w);
    Inc(pc.w);
    w2 := speekb(pc.w);
    Inc(pc.w);
    pc.w := w2 * 256 + w1;
    Result := 16;
  end
  else
  begin
    Inc(pc.w, 2);
    Result := 12;
  end;
end;

function JP: byte;
begin
  w1 := speekb(pc.w);
  Inc(pc.w);
  w2 := speekb(pc.w);
  Inc(pc.w);
  pc.w := w2 * 256 + w1;
  Result := 12;
end;

function CALL_NZ: byte;
begin
  if (af.l and 64) = 0 then
  begin
    w1 := speekb(pc.w);
    Inc(pc.w);
    w2 := speekb(pc.w);
    Inc(pc.w);
    push_pc;
    pc.w := w2 * 256 + w1;
    Result := 24;
  end
  else
  begin
    Inc(pc.w, 2);
    Result := 12;
  end;
end;

function PUSH_BC: byte;
begin
  Dec(sp_.w);
  spokeb(sp_.w, bc.h);
  Dec(sp_.w);
  spokeb(sp_.w, bc.l);
  Result := 16;
end;

function ADD_BYTE: byte;
begin
  w1 := speekb(pc.w);

  asm
           //PUSH    dword ptr pc.w
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1
           INC     word ptr pc.w
           MOV     AH,af.l
           SAHF

           ADD     af.h,AL
           LAHF
           AND     EAX,$5100
           MOV     af.l,AH

  end;

  Result := 8;
end;

function RST00: byte;
begin
  push_pc;
  pc.w := 0;
  Result := 32;
end;

function RET_Z: byte;
begin
  if (af.l and 64) > 0 then
  begin
    w1 := speekb(sp_.w);
    Inc(sp_.w);
    w2 := speekb(sp_.w);
    Inc(sp_.w);
    pc.w := (w2 * 256) + w1;
    Result := 20;
  end
  else
    Result := 8;
end;

function RET: byte;
begin
  w1 := speekb(sp_.w);
  Inc(sp_.w);
  w2 := speekb(sp_.w);
  Inc(sp_.w);
  pc.w := (w2 * 256) + w1;
  Result := 8;
end;

function JP_Z: byte;
begin
  if (af.l and 64) > 0 then
  begin
    w1 := speekb(pc.w);
    Inc(pc.w);
    w2 := speekb(pc.w);
    Inc(pc.w);
    pc.w := w2 * 256 + w1;
    Result := 16;
  end
  else
  begin
    Inc(pc.w, 2);
    Result := 12;
  end;
end;

function PFX_CB: byte;
begin
  Result := 0;
end;

function CALL_Z: byte;
begin
  if (af.l and 64) > 0 then
  begin
    w1 := speekb(pc.w);
    Inc(pc.w);
    w2 := speekb(pc.w);
    Inc(pc.w);
    push_pc;
    pc.w := w2 * 256 + w1;
    Result := 24;
  end
  else
  begin
    Inc(pc.w, 2);
    Result := 12;
  end;
end;

function CALL: byte;
begin
  w1 := speekb(pc.w);
  Inc(pc.w);
  w2 := speekb(pc.w);
  Inc(pc.w);
  push_pc;
  pc.w := w2 * 256 + w1;

  Result := 12;
end;

function ADC_BYTE: byte;
begin
  w1 := speekb(pc.w);
  asm
           //PUSH    dword ptr pc.w
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1

           INC     word ptr pc.w
           MOV     AH,af.l
           SAHF

           ADC     af.h,AL
           LAHF
           AND     EAX,$5100
           MOV     af.l,AH

  end;

  Result := 8;
end;

function RST08: byte;
begin
  push_pc;
  pc.w := 8;
  Result := 32;
end;

function RET_NC: byte;
begin
  if (af.l and 1) = 0 then
  begin
    w1 := speekb(sp_.w);
    Inc(sp_.w);
    w2 := speekb(sp_.w);
    Inc(sp_.w);
    pc.w := (w2 * 256) + w1;
    Result := 20;
  end
  else
    Result := 8;
end;

function POP_DE: byte;
begin
  de.l := speekb(sp_.w);
  Inc(sp_.w);
  de.h := speekb(sp_.w);
  Inc(sp_.w);
  Result := 12;
end;

function JP_NC: byte;
begin
  if (af.l and 1) = 0 then
  begin
    w1 := speekb(pc.w);
    Inc(pc.w);
    w2 := speekb(pc.w);
    Inc(pc.w);
    pc.w := w2 * 256 + w1;
    Result := 16;
  end
  else
  begin
    Inc(pc.w, 2);
    Result := 12;
  end;
end;

function OUTA: byte; // there is no use of this function
begin
  Result := 0;
end;

function CALL_NC: byte;
begin
  if (af.l and 1) = 0 then
  begin
    w1 := speekb(pc.w);
    Inc(pc.w);
    w2 := speekb(pc.w);
    Inc(pc.w);
    push_pc;
    pc.w := w2 * 256 + w1;
    Result := 24;
  end
  else
  begin
    Inc(pc.w, 2);
    Result := 12;
  end;
end;

function PUSH_DE: byte;
begin
  Dec(sp_.w);
  SpokeB(sp_.w, de.h);
  Dec(sp_.w);
  SpokeB(sp_.w, de.l);
  Result := 16;
end;

function SUB_BYTE: byte;
begin
  b.l := speekb(pc.w);
  Inc(pc.w);
  asm
           MOV     AL,af.h
           SUB     AL,b.l
           LAHF
           OR      AH,4
           MOV     af.l,AH
           MOV     af.h,AL
  end;

  Result := 8;
end;

function RST10: byte;
begin
  push_pc;
  pc.w := $10;
  Result := 32;
end;

function RET_C: byte;
begin
  if (af.l and 1) = 1 then
  begin
    w1 := speekb(sp_.w);
    Inc(sp_.w);
    w2 := speekb(sp_.w);
    Inc(sp_.w);
    pc.w := (w2 * 256) + w1;
    Result := 20;
  end
  else
    Result := 8;
end;

function EXX: byte;
begin
  w1 := speekb(sp_.w);
  Inc(sp_.w);
  w2 := speekb(sp_.w);
  Inc(sp_.w);
  pc.w := (w2 * 256) + w1;

  ei_fix := 2;
  Result := 8;
end;

function JP_C: byte;
begin

  if (af.l and 1) = 1 then
  begin
    w1 := speekb(pc.w);
    Inc(pc.w);
    w2 := speekb(pc.w);
    Inc(pc.w);
    pc.w := w2 * 256 + w1;
    Result := 16;
  end
  else
  begin
    Inc(pc.w, 2);
    Result := 12;
  end;
end;

function INA: byte; // equal to outa, not usuable
begin
  Result := 0;
end;

function CALL_C: byte;
begin
  if (af.l and 1) = 1 then
  begin
    b.l := speekb(pc.w);
    Inc(pc.w);
    b.h := speekb(pc.w);
    Inc(pc.w);
    push_pc;
    pc.w := b.w;
    Result := 24;
  end
  else
  begin
    Inc(pc.w, 2);
    Result := 12;
  end;
end;

function PFX_DD: byte;
begin
  Result := 0;
end;

function SBC_BYTE: byte;
begin
  w1 := speekb(pc.w);
  asm
           //PUSH    dword ptr pc.w
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1

           INC     word ptr pc.w
           MOV     AH,af.l
           SAHF

           SBB     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH

  end;
  Result := 8;
end;

function RST18: byte;
begin
  push_pc;
  pc.w := $18;
  Result := 32;
end;

function RET_PO: byte;
begin
  w1 := speekb(pc.w);
  asm
           //PUSH    dword ptr pc.w
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1

           INC     word ptr pc.w
           MOV     AH,$ff


           //push dword ptr af.h
           //PUSH    EAX
           //CALL    SpokeB
           //LEA     ESP,[ESP+8]
           MOV     w1, EAX
  end;

  spokeb(w1, af.h);

  Result := 12;
end;

function POP_HL: byte;
begin
  hl.l := speekb(sp_.w);
  Inc(sp_.w);
  hl.h := speekb(sp_.w);
  Inc(sp_.w);
  Result := 12;
end;

function JP_PO: byte;
begin
  {asm
           MOV     AL,bc.l
           MOV     AH,$ff


           push dword ptr af.h
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;}

  spokeb(bc.l, af.h);

  Result := 8;
end;

function EX_HL_xSP: byte;
begin
  B.l := SpeekB(SP_.W);
  SpokeB(SP_.W, HL.l);
  Inc(SP_.W);
  B.h := SpeekB(SP_.W);
  SpokeB(SP_.W, HL.h);
  Dec(SP_.W);
  HL.W := B.W;
  Result := 4;
end;

function CALL_PO: byte;
begin
  Result := 0;
end;

function PUSH_HL: byte;
begin
  Dec(sp_.w);
  SpokeB(sp_.w, hl.h);
  Dec(sp_.w);
  SpokeB(sp_.w, hl.l);
  Result := 16;
end;

function AND_BYTE: byte;
begin
  b.l := speekb(pc.w);
  Inc(pc.w);
  asm
           MOV     AL,b.l
           AND     af.h,AL
           LAHF
           AND     AH,64
           OR      AH,16
           MOV     af.l,AH
  end;

  Result := 8;
end;

function RST20: byte;
begin
  push_pc;
  pc.w := $20;
  Result := 32;
end;

function RET_PE: byte;
begin
  b.l := speekb(pc.w);
  Inc(pc.w);
  asm
           MOV     AL,b.l
           CBW
           ADD     sp_.w,AX
           AND     af.l,17
  end;
  Result := 16;
end;

function LD_PC_HL: byte;
begin
  pc.w := hl.w;
  Result := 4;
end;

function JP_PE: byte;
begin
  // TODO

  asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           PUSH    EAX
           INC     word ptr pc.w
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           INC     word ptr pc.w
           SHL     EAX,8
           POP     EBX
           MOV     AL,BL
           PUSH    dword ptr af.h
           PUSH    EAX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;

  Result := 16;
end;

function EX_DE_HL: byte;
begin
  b.w := de.W;
  de.W := hl.W;
  hl.W := b.w;
  Result := 4;
end;

function CALL_PE: byte; // ????
begin
  if af.l and 4 > 0 then
  begin
    Inc(pc.w, 2);
    push_pc;
    Dec(pc.w, 2);
    pc.W := wordpeek(pc.W);
    Result := 17;
  end
  else
  begin
    Inc(pc.W, 2);
    Result := 10;
  end;
end;

function PFX_ED: byte;
begin
  Result := 0;
end;

function XOR_BYTE: byte;
begin
  w1 := speekb(pc.w);
  asm
           //PUSH    dword ptr pc.w
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1

           INC     word ptr pc.w

           XOR     af.h,AL
           LAHF
           AND     EAX,$4000
           MOV     af.l,AH
  end;

  Result := 8;
end;

function RST28: byte;
begin
  push_pc;
  pc.w := $28;
  Result := 32;
end;

function RET_P: byte;
begin
  // TODO
  w1 := speekb(pc.w);
  asm
           //PUSH    dword ptr pc.w
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1

           INC     word ptr pc.w
           XOR     AH,AH
           MOV     BX,$ff00
           ADD     AX,BX
           // ?
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     af.h,AL
  end;

  Result := 12;
end;

function POP_AF: byte;
begin
  b.l := speekb(sp_.w);
  Inc(sp_.w);
  asm
           MOV     AL,b.l
           AND     AL,240
           MOV     BL,AL
           SHR     AL,1
           AND     AL,80
           SHR     BL,4
           AND     BL,5
           OR      AL,BL
           MOV     af.l,AL
  end;
  af.h := speekb(sp_.w);
  Inc(sp_.w);
  Result := 12;
end;

function JP_P: byte;
begin
  // TODO
  asm
           MOV     AL,bc.l
           XOR     AH,AH
           MOV     BX,$ff00
           ADD     AX,BX
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     af.h,AL
  end;

  Result := 8;
end;

function DI: byte;
begin
  ei_fix := 0;
  di_fix := 2;
  Result := 4;
end;

function CALL_P: byte;
begin
  Result := 0;
end;

function PUSH_AF: byte;
begin
  asm
           MOV     AL,af.l
           AND     AL,85
           MOV     BL,AL
           SHL     AL,1
           AND     AL,160
           SHL     BL,4
           AND     BL,80
           OR      AL,BL
           MOV     b.l,AL
  end;
  Dec(sp_.w);
  SpokeB(sp_.w, af.h);
  Dec(sp_.w);
  SpokeB(sp_.w, b.l);
  Result := 16;
end;

function OR_BYTE: byte;
begin
  b.l := speekb(pc.w);
  Inc(pc.w);
  asm
           MOV     AL,b.l
           OR      af.h,AL
           LAHF
           AND     AH,64
           MOV     af.l,AH
  end;

  Result := 8;
end;

function RST30: byte;
begin
  push_pc;
  pc.w := $30;
  Result := 32;
end;

function RET_M: byte;
begin
  w1 := speekb(pc.w);
  asm
           //PUSH    dword ptr pc.w
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1

           INC     word ptr pc.w


           CBW




           MOV     BX,sp_.w
           ADD     BX,AX
           MOV     hl.w,BX
           AND     af.l,17
  end;
  Result := 12;
end;

function LD_SP_HL: byte;
begin
  asm
           MOV     AX,hl.w
           MOV     sp_.w,AX
  end;

  Result := 8;
end;

function JP_M: byte;
begin
  // TODO

  asm
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           INC     word ptr pc.w
           PUSH    EAX
           PUSH    dword ptr pc.w
           CALL    speekb
           LEA     ESP,[ESP+4]
           INC     word ptr pc.w
           SHL     EAX,8
           POP     EBX
           MOV     AL,BL
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           MOV     af.h,AL
  end;



  Result := 16;
end;

function EI: byte;
begin
  di_fix := 0;
  ei_fix := 2;
  Result := 4;
end;

function CALL_M: byte;
begin
  Result := 0;
end;

function PFX_FD: byte;
begin
  if Assigned(@FDCallback) then FDCallback;
  Result := 0;
end;

function CP_BYTE: byte;
begin
  w1 := speekb(pc.w);
  asm
           //PUSH    dword ptr pc.w
           //CALL    speekb
           //LEA     ESP,[ESP+4]
           MOV     AX, w1

           INC     word ptr pc.w
           CMP     af.h,AL
           LAHF
           OR      EAX,$400
           MOV     af.l,AH
  end;
  Result := 8;
end;

function RST38: byte;
begin
  push_pc;
  pc.w := $38;
  Result := 32;
end;

function RLC_B: byte;
begin
  if bc.h = 0 then
    af.l := 64
  else
    asm
             ROL     bc.h,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;
  Result := 8;
end;

function RLC_C: byte;
begin
  if bc.l = 0 then
    af.l := 64
  else
    asm
             ROL     bc.l,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;
  Result := 8;
end;

function RLC_D: byte;
begin
  if de.h = 0 then
    af.l := 64
  else
    asm
             ROL     de.h,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;
  Result := 8;
end;

function RLC_E: byte;
begin
  if de.l = 0 then
    af.l := 64
  else
    asm
             ROL     de.l,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;
  Result := 8;
end;

function RLC_H: byte;
begin
  if hl.h = 0 then
    af.l := 64
  else
    asm
             ROL     hl.h,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;
  Result := 8;
end;

function RLC_L: byte;
begin
  if hl.l = 0 then
    af.l := 64
  else
    asm
             ROL     hl.l,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;
  Result := 8;
end;

function RLC_xHL: byte;
begin
  b.l := speekb(hl.w);
  if b.l = 0 then
    af.l := 64
  else
    asm
             ROL     b.l,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;
  SpokeB(hl.w, b.l);
  Result := 16;
end;

function RLC_A: byte;
begin
  if af.h = 0 then
    af.l := 64
  else
    asm
             ROL     af.h,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;
  Result := 8;
end;

function RRC_B: byte;
begin
  if bc.h = 0 then
    af.l := 64
  else
    asm
             ROR     bc.h,1
             LAHF
             AND     AH,235
             MOV     af.l,AH
    end;
  Result := 8;
end;

function RRC_C: byte;
begin
  if bc.l = 0 then
    af.l := 64
  else
    asm
             ROR     bc.l,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;
  Result := 8;
end;

function RRC_D: byte;
begin
  if de.h = 0 then
    af.l := 64
  else
    asm
             ROR     de.h,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;

  Result := 8;
end;

function RRC_E: byte;
begin
  if de.l = 0 then
    af.l := 64
  else
    asm
             ROR     de.l,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;
  Result := 8;
end;

function RRC_H: byte;
begin
  if hl.h = 0 then
    af.l := 64
  else
    asm
             ROR     hl.h,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;
  Result := 8;
end;

function RRC_L: byte;
begin
  if hl.l = 0 then
    af.l := 64
  else
    asm
             ROR     hl.l,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;

  Result := 8;
end;

function RRC_xHL: byte;
begin
  b.l := speekb(hl.w);
  if b.l = 0 then
    af.l := 64
  else
    asm
             ROR     b.l,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;

  SpokeB(w1, b.l);

  Result := 16;
end;

function RRC_A: byte;
begin
  if af.h = 0 then
    af.l := 64
  else
    asm
             ROR     af.h,1
             LAHF
             AND     AH,1
             MOV     af.l,AH
    end;
  Result := 8;
end;

function RL_B: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,bc.h
           RCL     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     bc.h,AL
  end;

  Result := 8;
end;

function RL_C: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,bc.l
           RCL     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     bc.l,AL
  end;


  Result := 8;
end;

function RL_D: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,de.h
           RCL     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     de.h,AL
  end;


  Result := 8;
end;

function RL_E: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,de.l
           RCL     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     de.l,AL
  end;



  Result := 8;
end;

function RL_H: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,hl.h
           RCL     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     hl.h,AL
  end;


  Result := 8;
end;

function RL_L: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,hl.l
           RCL     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     hl.l,AL
  end;

  Result := 8;
end;

function RL_xHL: byte;
begin

  b.l := speekb(hl.w);
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,b.l
           RCL     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     b.l,AL
  end;



  SpokeB(hl.w, b.l);

  Result := 16;
end;

function RL_A: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,af.h
           RCL     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     af.h,AL
  end;

  Result := 8;
end;

function RR_B: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,bc.h
           RCR     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     bc.h,AL
  end;

  Result := 8;
end;

function RR_C: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,bc.l
           RCR     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     bc.l,AL
  end;

  Result := 8;
end;

function RR_D: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,de.h
           RCR     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     de.h,AL
  end;



  Result := 8;
end;

function RR_E: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,de.l
           RCR     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     de.l,AL
  end;


  Result := 8;
end;

function RR_H: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,hl.h
           RCR     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     hl.h,AL
  end;



  Result := 8;
end;

function RR_L: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,hl.l
           RCR     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     hl.l,AL
  end;



  Result := 8;
end;

function RR_xHL: byte;
begin

  b.l := speekb(hl.w);
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,b.l
           RCR     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     b.l,AL
  end;

  SpokeB(hl.w, b.l);
  Result := 16;
end;

function RR_A: byte;
begin
  asm
           MOV     AH,af.l
           SAHF
           MOV     AL,af.h
           RCR     AL,1
           LAHF
           AND     AH,1
           TEST    AL,AL
           JNZ     @1
           OR      AH,64
           @1:
           MOV     af.l,AH
           MOV     af.h,AL
  end;


  Result := 8;
end;

function SLA_B: byte;
begin
  asm
           SHL     bc.h,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 8;
end;

function SLA_C: byte;
begin
  asm
           SHL     bc.l,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 8;
end;

function SLA_D: byte;
begin
  asm
           SHL     de.h,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 8;
end;

function SLA_E: byte;
begin
  asm
           SAL     de.l,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 8;
end;

function SLA_H: byte;
begin
  asm
           SHL     hl.h,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 8;
end;

function SLA_L: byte;
begin
  asm
           SHL     hl.l,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 8;
end;

function SLA_xHL: byte;
begin
  b.l := speekb(hl.w);
  asm
           SHL     b.l,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  SpokeB(hl.w, b.l);
  Result := 16;
end;

function SLA_A: byte;
begin
  asm
           SAL     af.h,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 8;
end;

function SRA_B: byte;
begin
  asm
           SAR     bc.h,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 8;
end;

function SRA_C: byte;
begin
  asm
           SAR     bc.l,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 8;
end;

function SRA_D: byte;
begin
  asm
           SAR     de.h,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 8;
end;

function SRA_E: byte;
begin
  asm
           SAR     de.l,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 8;
end;

function SRA_H: byte;
begin
  asm
           SAR     hl.h,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 8;
end;

function SRA_L: byte;
begin
  asm
           SAR     hl.l,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 8;
end;

function SRA_xHL: byte;
begin
  b.l := speekb(hl.w);
  asm
           SAR     b.l,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  SpokeB(hl.w, b.l);
  Result := 16;
end;

function SRA_A: byte;
begin
  asm
           SAR     af.h,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;
  Result := 8;
end;

function SLL_B: byte;
begin
  asm
           MOV     af.l,0
           ROL     bc.h,4
           CMP     bc.h,0
           JNZ     @ende
           MOV     af.l,64
           @ende:
  end;
  Result := 8;
end;

function SLL_C: byte;
begin
  asm
           MOV     af.l,0
           ROL     bc.l,4
           CMP     bc.l,0
           JNZ     @ende
           MOV     af.l,64
           @ende:
  end;

  Result := 8;
end;

function SLL_D: byte;
begin
  asm
           MOV     af.l,0
           ROL     de.h,4
           CMP     de.h,0
           JNZ     @ende
           MOV     af.l,64
           @ende:
  end;

  Result := 8;
end;

function SLL_E: byte;
begin
  asm
           MOV     af.l,0
           ROL     de.l,4
           CMP     de.l,0
           JNZ     @ende
           MOV     af.l,64
           @ende:
  end;

  Result := 8;
end;

function SLL_H: byte;
begin
  asm
           MOV     af.l,0
           ROL     hl.h,4
           CMP     hl.h,0
           JNZ     @ende
           MOV     af.l,64
           @ende:
  end;

  Result := 8;
end;

function SLL_L: byte;
begin
  asm
           MOV     af.l,0
           ROL     hl.l,4
           CMP     hl.l,0
           JNZ     @ende
           MOV     af.l,64
           @ende:
  end;

  Result := 8;
end;

function SLL_xHL: byte;
begin

  b.l := speekb(hl.w);
  asm
           MOV     af.l,0
           ROL     b.l,4
           CMP     b.l,0
           JNZ     @ende
           MOV     af.l,64
           @ende:
  end;
  SpokeB(hl.w, b.l);

  Result := 16;
end;

function SLL_A: byte;
begin
  asm
           MOV     af.l,0
           ROR     af.h,4
           CMP     af.h,0
           JNZ     @ende
           MOV     af.l,64
           @ende:
  end;
  Result := 8;
end;

function SRL_B: byte;
begin
  asm
           SHR     bc.h,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;


  Result := 8;
end;

function SRL_C: byte;
begin
  asm
           SHR     bc.l,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;


  Result := 8;
end;

function SRL_D: byte;
begin
  asm
           SHR     de.h,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;


  Result := 8;
end;

function SRL_E: byte;
begin
  asm
           SHR     de.l,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;


  Result := 8;
end;

function SRL_H: byte;
begin
  asm
           SHR     hl.h,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;


  Result := 8;
end;

function SRL_L: byte;
begin
  asm
           SHR     hl.l,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;


  Result := 8;
end;

function SRL_xHL: byte;
begin
  b.l := speekb(hl.w);
  asm
           SHR     b.l,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  SpokeB(hl.w, b.l);
  Result := 16;
end;

function SRL_A: byte;
begin
  asm
           SHR     af.h,1
           LAHF
           AND     AH,65
           MOV     af.l,AH
  end;

  Result := 8;
end;

function BIT0_B: byte;
begin

  asm
           TEST    bc.h,1
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH

  end;
  Result := 8;
end;

function BIT0_C: byte;
begin
  asm
           TEST    bc.l,1
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT0_D: byte;
begin
  asm
           TEST    de.h,1
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT0_E: byte;
begin
  asm
           TEST    de.l,1
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT0_H: byte;
begin
  asm
           TEST    hl.h,1
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT0_L: byte;
begin
  asm
           TEST    hl.l,1
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT0_xHL: byte;
begin
  b.l := speekb(hl.w);
  asm
           TEST    b.l,1
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 16;
end;

function BIT0_A: byte;
begin
  asm
           TEST    af.h,1
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT1_B: byte;
begin
  asm
           TEST    bc.h,2
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT1_C: byte;
begin
  asm
           TEST    bc.l,2
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;


  Result := 8;
end;

function BIT1_D: byte;
begin
  asm
           TEST    de.h,2
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;


  Result := 8;
end;

function BIT1_E: byte;
begin
  asm
           TEST    de.l,2
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;


  Result := 8;
end;

function BIT1_H: byte;
begin
  asm
           TEST    hl.h,2
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;


  Result := 8;
end;

function BIT1_L: byte;
begin
  asm
           TEST    hl.l,2
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;


  Result := 8;
end;

function BIT1_xHL: byte;
begin
  b.l := speekb(hl.w);
  asm
           TEST    b.l,2
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;


  Result := 16;
end;

function BIT1_A: byte;
begin
  asm
           TEST    af.h,2
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;


  Result := 8;
end;

function BIT2_B: byte;
begin
  asm
           TEST    bc.h,4
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;


  Result := 8;
end;

function BIT2_C: byte;
begin
  asm
           TEST    bc.l,4
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT2_D: byte;
begin
  asm
           TEST    de.h,4
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT2_E: byte;
begin
  asm
           TEST    de.l,4
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT2_H: byte;
begin
  asm
           TEST    hl.h,4
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT2_L: byte;
begin
  asm
           TEST    hl.l,4
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT2_xHL: byte;
begin

  b.l := speekb(hl.w);
  asm
           TEST    b.l,4
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 16;
end;

function BIT2_A: byte;
begin
  asm
           TEST    af.h,4
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT3_B: byte;
begin
  asm
           TEST    bc.h,8
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT3_C: byte;
begin
  asm
           TEST    bc.l,8
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT3_D: byte;
begin
  asm
           TEST    de.h,8
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT3_E: byte;
begin
  asm
           TEST    de.l,8
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT3_H: byte;
begin
  asm
           TEST    hl.h,8
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT3_L: byte;
begin
  asm
           TEST    hl.l,8
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 1;
end;

function BIT3_xHL: byte;
begin
  b.l := speekb(hl.w);
  asm
           TEST    b.l,8
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 16;
end;

function BIT3_A: byte;
begin
  asm
           TEST    af.h,8
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT4_B: byte;
begin
  asm
           TEST    bc.h,16
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT4_C: byte;
begin
  asm
           TEST    bc.l,16
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT4_D: byte;
begin
  asm
           TEST    de.h,16
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT4_E: byte;
begin
  asm
           TEST    de.l,16
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT4_H: byte;
begin
  asm
           TEST    hl.h,16
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT4_L: byte;
begin
  asm
           TEST    hl.l,16
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT4_xHL: byte;
begin
  b.l := speekb(hl.w);
  asm
           TEST    b.l,16
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 16;
end;

function BIT4_A: byte;
begin
  asm
           TEST    af.h,16
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT5_B: byte;
begin
  asm
           TEST    bc.h,32
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT5_C: byte;
begin
  asm
           TEST    bc.l,32
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT5_D: byte;
begin
  asm
           TEST    de.h,32
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT5_E: byte;
begin
  asm
           TEST    de.l,32
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT5_H: byte;
begin
  asm
           TEST    hl.h,32
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT5_L: byte;
begin
  asm
           TEST    hl.l,32
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT5_xHL: byte;
begin

  b.l := speekb(hl.w);
  asm
           TEST    b.l,32
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 16;
end;

function BIT5_A: byte;
begin
  asm
           TEST    af.h,32
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;

  Result := 8;
end;

function BIT6_B: byte;
begin
  asm
           TEST    bc.h,64
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT6_C: byte;
begin
  asm
           TEST    bc.l,64
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT6_D: byte;
begin
  asm
           TEST    de.h,64
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT6_E: byte;
begin
  asm
           TEST    de.l,64
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT6_H: byte;
begin
  asm
           TEST    hl.h,64
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT6_L: byte;
begin
  asm
           TEST    hl.l,64
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT6_xHL: byte;
begin

  b.l := speekb(hl.w);
  asm
           TEST    b.l,64
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 16;
end;

function BIT6_A: byte;
begin
  asm
           TEST    af.h,64
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT7_B: byte;
begin
  asm
           TEST    bc.h,128
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT7_C: byte;
begin
  asm
           TEST    bc.l,128
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT7_D: byte;
begin
  asm
           TEST    de.h,128
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT7_E: byte;
begin
  asm
           TEST    de.l,128
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT7_H: byte;
begin
  asm
           TEST    hl.h,128
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT7_L: byte;
begin
  asm
           TEST    hl.l,128
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function BIT7_xHL: byte;
begin
  b.l := speekb(hl.w);
  asm
           TEST    b.l,128
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 16;
end;

function BIT7_A: byte;
begin
  asm
           TEST    af.h,128
           LAHF
           AND     AH,64
           AND     af.l,1
           OR      AH,16
           OR      af.l,AH
  end;
  Result := 8;
end;

function RES0_B: byte;
begin
  asm
           AND     bc.h,254
  end;
  Result := 8;
end;

function RES0_C: byte;
begin
  asm
           AND     bc.l,254
  end;

  Result := 8;
end;

function RES0_D: byte;
begin
  asm
           AND     de.h,254
  end;

  Result := 8;
end;

function RES0_E: byte;
begin
  asm
           AND     de.l,254
  end;

  Result := 8;
end;

function RES0_H: byte;
begin
  asm
           AND     hl.h,254
  end;

  Result := 8;
end;

function RES0_L: byte;
begin
  asm
           AND     hl.l,254
  end;

  Result := 8;
end;

function RES0_xHL: byte;
begin
  w1 := hl.w;
  b.l := speekb(w1) and 254;
  SpokeB(w1, b.l);
  Result := 16;
end;

function RES0_A: byte;
begin
  af.h := af.h and 254;
  Result := 8;
end;

function RES1_B: byte;
begin
  bc.h := bc.h and 253;
  Result := 8;
end;

function RES1_C: byte;
begin
  bc.l := bc.l and 253;
  Result := 8;
end;

function RES1_D: byte;
begin
  de.h := de.h and 253;
  Result := 8;
end;

function RES1_E: byte;
begin
  de.l := de.l and 253;
  Result := 8;
end;

function RES1_H: byte;
begin
  hl.h := hl.h and 253;
  Result := 8;
end;

function RES1_L: byte;
begin
  hl.l := hl.l and 253;
  Result := 8;
end;

function RES1_xHL: byte;
begin
  w1 := hl.w;
  b.l := speekb(w1) and 253;
  SpokeB(w1, b.l);

  Result := 16;
end;

function RES1_A: byte;
begin
  af.h := af.h and 253;
  Result := 8;
end;

function RES2_B: byte;
begin
  bc.h := bc.h and 251;
  Result := 8;
end;

function RES2_C: byte;
begin
  bc.l := bc.l and 251;
  Result := 8;
end;

function RES2_D: byte;
begin
  de.h := de.h and 251;
  Result := 8;
end;

function RES2_E: byte;
begin
  de.l := de.l and 251;
  Result := 8;
end;

function RES2_H: byte;
begin
  hl.h := hl.h and 251;
  Result := 8;
end;

function RES2_L: byte;
begin
  hl.l := hl.l and 251;
  Result := 8;
end;

function RES2_xHL: byte;
begin
  w1 := hl.w;
  b.l := speekb(w1);
  b.l := b.l and 251;
  SpokeB(w1, b.l);
  Result := 16;
end;

function RES2_A: byte;
begin
  af.h := af.h and 251;
  Result := 8;
end;

function RES3_B: byte;
begin
  bc.h := bc.h and 247;
  Result := 8;
end;

function RES3_C: byte;
begin
  bc.l := bc.l and 247;
  Result := 8;
end;

function RES3_D: byte;
begin
  de.h := de.h and 247;
  Result := 8;
end;

function RES3_E: byte;
begin
  de.l := de.l and 247;
  Result := 8;
end;

function RES3_H: byte;
begin
  hl.h := hl.h and 247;
  Result := 8;
end;

function RES3_L: byte;
begin
  hl.l := hl.l and 247;
  Result := 8;
end;

function RES3_xHL: byte;
begin
  w1 := hl.w;
  b.l := speekb(w1);
  b.l := b.l and 247;
  SpokeB(w1, b.l);
  Result := 16;
end;

function RES3_A: byte;
begin
  af.h := af.h and 247;
  Result := 8;
end;

function RES4_B: byte;
begin
  bc.h := bc.h and 239;
  Result := 8;
end;

function RES4_C: byte;
begin
  bc.l := bc.l and 239;
  Result := 8;
end;

function RES4_D: byte;
begin
  de.h := de.h and 239;
  Result := 8;
end;

function RES4_E: byte;
begin
  de.l := de.l and 239;
  Result := 8;
end;

function RES4_H: byte;
begin
  hl.h := hl.h and 239;
  Result := 8;
end;

function RES4_L: byte;
begin
  hl.l := hl.l and 239;
  Result := 8;
end;

function RES4_xHL: byte;
begin
  w1 := hl.w;
  b.l := speekb(w1);
  b.l := b.l and 239;
  SpokeB(w1, b.l);
  Result := 16;
end;

function RES4_A: byte;
begin
  af.h := af.h and 239;
  Result := 8;
end;

function RES5_B: byte;
begin
  bc.h := bc.h and 223;
  Result := 8;
end;

function RES5_C: byte;
begin
  bc.l := bc.l and 223;
  Result := 8;
end;

function RES5_D: byte;
begin
  de.h := de.h and 223;
  Result := 8;
end;

function RES5_E: byte;
begin
  de.l := de.l and 223;
  Result := 8;
end;

function RES5_H: byte;
begin
  hl.h := hl.h and 223;
  Result := 8;
end;

function RES5_L: byte;
begin
  hl.l := hl.l and 223;

  Result := 8;
end;

function RES5_xHL: byte;
begin
  w1 := hl.w;
  b.l := speekb(w1);
  b.l := b.l and 223;
  SpokeB(w1, b.l);
  Result := 16;
end;

function RES5_A: byte;
begin
  af.h := af.h and 223;
  Result := 8;
end;

function RES6_B: byte;
begin
  bc.h := bc.h and 191;
  Result := 8;
end;

function RES6_C: byte;
begin
  bc.l := bc.l and 191;
  Result := 8;
end;

function RES6_D: byte;
begin
  de.h := de.h and 191;
  Result := 8;
end;

function RES6_E: byte;
begin
  de.l := de.l and 191;
  Result := 8;
end;

function RES6_H: byte;
begin
  hl.h := hl.h and 191;
  Result := 8;
end;

function RES6_L: byte;
begin
  hl.l := hl.l and 191;
  Result := 8;
end;

function RES6_xHL: byte;
begin
  w1 := hl.w;
  b.l := speekb(w1);
  b.l := b.l and 191;
  SpokeB(w1, b.l);
  Result := 16;
end;

function RES6_A: byte;
begin
  af.h := af.h and 191;
  Result := 8;
end;

function RES7_B: byte;
begin
  bc.h := bc.h and 127;
  Result := 8;
end;

function RES7_C: byte;
begin
  bc.l := bc.l and 127;
  Result := 8;
end;

function RES7_D: byte;
begin
  de.h := de.h and 127;
  Result := 8;
end;

function RES7_E: byte;
begin
  de.l := de.l and 127;
  Result := 8;
end;

function RES7_H: byte;
begin
  hl.h := hl.h and 127;
  Result := 8;
end;

function RES7_L: byte;
begin
  hl.l := hl.l and 127;
  Result := 8;
end;

function RES7_xHL: byte;
begin
  w1 := hl.w;
  b.l := speekb(w1);
  b.l := b.l and 127;
  SpokeB(w1, b.l);
  Result := 16;
end;

function RES7_A: byte;
begin
  af.h := af.h and 127;
  Result := 8;
end;

function SET0_B: byte;
begin
  bc.h := bc.h or 1;
  Result := 8;
end;

function SET0_C: byte;
begin
  bc.l := bc.l or 1;
  Result := 8;
end;

function SET0_D: byte;
begin
  de.h := de.h or 1;
  Result := 8;
end;

function SET0_E: byte;
begin
  de.l := de.l or 1;
  Result := 8;
end;

function SET0_H: byte;
begin
  hl.h := hl.h or 1;
  Result := 8;
end;

function SET0_L: byte;
begin
  hl.l := hl.l or 1;
  Result := 8;
end;

function SET0_xHL: byte;
begin
  w1 := hl.w;
  b.l := speekb(w1);
  b.l := b.l or 1;
  SpokeB(w1, b.l);
  Result := 16;
end;

function SET0_A: byte;
begin
  af.h := af.h or 1;
  Result := 8;
end;

function SET1_B: byte;
begin
  bc.h := bc.h or 2;
  Result := 8;
end;

function SET1_C: byte;
begin
  bc.l := bc.l or 2;
  Result := 8;
end;

function SET1_D: byte;
begin
  de.h := de.h or 2;
  Result := 8;
end;

function SET1_E: byte;
begin
  de.l := de.l or 2;
  Result := 8;
end;

function SET1_H: byte;
begin
  hl.h := hl.h or 2;
  Result := 8;
end;

function SET1_L: byte;
begin
  hl.l := hl.l or 2;
  Result := 8;
end;

function SET1_xHL: byte;
begin
  w1 := hl.w;
  b.l := speekb(w1) or 2;
  SpokeB(w1, b.l);
  Result := 16;
end;

function SET1_A: byte;
begin
  af.h := af.h or 2;
  Result := 8;
end;

function SET2_B: byte;
begin
  bc.h := bc.h or 4;
  Result := 8;
end;

function SET2_C: byte;
begin
  bc.l := bc.l or 4;
  Result := 8;
end;

function SET2_D: byte;
begin
  de.h := de.h or 4;
  Result := 8;
end;

function SET2_E: byte;
begin
  de.l := de.l or 4;
  Result := 8;
end;

function SET2_H: byte;
begin
  hl.h := hl.h or 4;
  Result := 8;
end;

function SET2_L: byte;
begin
  hl.l := hl.l or 4;
  Result := 8;
end;

function SET2_xHL: byte;
begin
  w1 := hl.w;
  b.l := speekb(w1) or 4;
  SpokeB(w1, b.l);
  Result := 16;
end;

function SET2_A: byte;
begin
  af.h := af.h or 4;
  Result := 8;
end;

function SET3_B: byte;
begin
  bc.h := bc.h or 8;
  Result := 8;
end;

function SET3_C: byte;
begin
  bc.l := bc.l or 8;
  Result := 8;
end;

function SET3_D: byte;
begin
  de.h := de.h or 8;
  Result := 8;
end;

function SET3_E: byte;
begin
  de.l := de.l or 8;
  Result := 8;
end;

function SET3_H: byte;
begin
  hl.h := hl.h or 8;
  Result := 8;
end;

function SET3_L: byte;
begin
  hl.l := hl.l or 8;
  Result := 8;
end;

function SET3_xHL: byte;
begin
  // TODO
  asm
           MOV     AX,hl.w
           PUSH    EAX
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           OR      AL,8
           POP     EBX
           PUSH    EAX
           PUSH    EBX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;

  Result := 16;
end;

function SET3_A: byte;
begin
  af.h := af.h or 8;
  Result := 8;
end;

function SET4_B: byte;
begin
  bc.h := bc.h or 16;
  Result := 8;
end;

function SET4_C: byte;
begin
  bc.l := bc.l or 16;
  Result := 8;
end;

function SET4_D: byte;
begin
  de.h := de.h or 16;
  Result := 8;
end;

function SET4_E: byte;
begin
  de.l := de.l or 16;
  Result := 8;
end;

function SET4_H: byte;
begin
  hl.h := hl.h or 16;
  Result := 8;
end;

function SET4_L: byte;
begin
  hl.l := hl.l or 16;
  Result := 8;
end;

function SET4_xHL: byte;
begin
  w1 := hl.w;
  b.l := speekb(w1) or 16;
  SpokeB(w1, b.l);
  Result := 16;
end;

function SET4_A: byte;
begin
  af.h := af.h or 16;
  Result := 8;
end;

function SET5_B: byte;
begin
  bc.h := bc.h or 32;
  Result := 8;
end;

function SET5_C: byte;
begin
  bc.l := bc.l or 32;
  Result := 8;
end;

function SET5_D: byte;
begin
  de.h := de.h or 32;
  Result := 8;
end;

function SET5_E: byte;
begin
  de.l := de.l or 32;
  Result := 8;
end;

function SET5_H: byte;
begin
  hl.h := hl.h or 32;
  Result := 8;
end;

function SET5_L: byte;
begin
  hl.l := hl.l or 32;
  Result := 8;
end;

function SET5_xHL: byte;
begin
  w1 := hl.w;
  b.l := speekb(w1) or 32;
  SpokeB(w1, b.l);
  Result := 16;
end;

function SET5_A: byte;
begin
  af.h := af.h or 32;
  Result := 8;
end;

function SET6_B: byte;
begin
  bc.h := bc.h or 64;
  Result := 8;
end;

function SET6_C: byte;
begin
  bc.l := bc.l or 64;
  Result := 8;
end;

function SET6_D: byte;
begin
  de.h := de.h or 64;
  Result := 8;
end;

function SET6_E: byte;
begin
  de.l := de.l or 64;
  Result := 8;
end;

function SET6_H: byte;
begin
  hl.h := hl.h or 64;
  Result := 8;
end;

function SET6_L: byte;
begin
  hl.l := hl.l or 64;
  Result := 8;
end;

function SET6_xHL: byte;
begin
  w1 := hl.w;
  b.l := speekb(w1) or 64;
  SpokeB(w1, b.l);
  Result := 16;
end;

function SET6_A: byte;
begin
  af.h := af.h or 64;
  Result := 8;
end;

function SET7_B: byte;
begin
  bc.h := bc.h or 128;
  Result := 8;
end;

function SET7_C: byte;
begin
  bc.l := bc.l or 128;
  Result := 8;
end;

function SET7_D: byte;
begin
  de.h := de.h or 128;
  Result := 8;
end;

function SET7_E: byte;
begin
  asm
           OR      de.l,128
  end;
  Result := 8;
end;

function SET7_H: byte;
begin
  asm
           OR      hl.h,128
  end;

  Result := 8;
end;

function SET7_L: byte;
begin
  asm
           OR      hl.l,128
  end;
  Result := 8;
end;

function SET7_xHL: byte;
begin
  // TODO
  asm
           MOV     AX,hl.w
           PUSH    EAX
           PUSH    EAX
           CALL    speekb
           LEA     ESP,[ESP+4]
           POP     EBX
           OR      AL,128
           push EAX
           PUSH    EBX
           CALL    SpokeB
           LEA     ESP,[ESP+8]
  end;
  Result := 16;
end;

function SET7_A: byte;
begin
  asm
           OR      af.h,128
  end;
  Result := 8;
end;

begin
  // Generating Tables
  z80[000] := NOP;
  z80[001] := LD_BC_WORD;
  z80[002] := LD_xBC_A;
  z80[003] := INC_BC;
  z80[004] := INC_B;
  z80[005] := DEC_B;
  z80[006] := LD_B_BYTE;
  z80[007] := RLCA;
  z80[008] := EX_AF_AF;
  z80[009] := ADD_HL_BC;
  z80[010] := LD_A_xBC;
  z80[011] := DEC_BC;
  z80[012] := INC_C;
  z80[013] := DEC_C;
  z80[014] := LD_C_BYTE;
  z80[015] := RRCA;
  z80[016] := DJNZ;
  z80[017] := LD_DE_WORD;
  z80[018] := LD_xDE_A;
  z80[019] := INC_DE;
  z80[020] := INC_D;
  z80[021] := DEC_D;
  z80[022] := LD_D_BYTE;
  z80[023] := RLA;
  z80[024] := JR;
  z80[025] := ADD_HL_DE;
  z80[026] := LD_A_xDE;
  z80[027] := DEC_DE;
  z80[028] := INC_E;
  z80[029] := DEC_E;
  z80[030] := LD_E_BYTE;
  z80[031] := RRA;
  z80[032] := JR_NZ;
  z80[033] := LD_HL_WORD;
  z80[034] := LD_xWORD_HL;
  z80[035] := INC_HL;
  z80[036] := INC_H;
  z80[037] := DEC_H;
  z80[038] := LD_H_BYTE;
  z80[039] := DAA;
  z80[040] := JR_Z;
  z80[041] := ADD_HL_HL;
  z80[042] := LD_HL_xWORD;
  z80[043] := DEC_HL;
  z80[044] := INC_L;
  z80[045] := DEC_L;
  z80[046] := LD_L_BYTE;
  z80[047] := CPL;
  z80[048] := JR_NC;
  z80[049] := LD_SP_WORD;
  z80[050] := LD_xWORD_A;
  z80[051] := INC_SP;
  z80[052] := INC_xHL;
  z80[053] := DEC_xHL;
  z80[054] := LD_xHL_BYTE;
  z80[055] := SCF;
  z80[056] := JR_C;
  z80[057] := ADD_HL_SP;
  z80[058] := LD_A_xWORD;
  z80[059] := DEC_SP;
  z80[060] := INC_A;
  z80[061] := DEC_A;
  z80[062] := LD_A_BYTE;
  z80[063] := CCF;
  z80[064] := LD_B_B;
  z80[065] := LD_B_C;
  z80[066] := LD_B_D;
  z80[067] := LD_B_E;
  z80[068] := LD_B_H;
  z80[069] := LD_B_L;
  z80[070] := LD_B_xHL;
  z80[071] := LD_B_A;
  z80[072] := LD_C_B;
  z80[073] := LD_C_C;
  z80[074] := LD_C_D;
  z80[075] := LD_C_E;
  z80[076] := LD_C_H;
  z80[077] := LD_C_L;
  z80[078] := LD_C_xHL;
  z80[079] := LD_C_A;
  z80[080] := LD_D_B;
  z80[081] := LD_D_C;
  z80[082] := LD_D_D;
  z80[083] := LD_D_E;
  z80[084] := LD_D_H;
  z80[085] := LD_D_L;
  z80[086] := LD_D_xHL;
  z80[087] := LD_D_A;
  z80[088] := LD_E_B;
  z80[089] := LD_E_C;
  z80[090] := LD_E_D;
  z80[091] := LD_E_E;
  z80[092] := LD_E_H;
  z80[093] := LD_E_L;
  z80[094] := LD_E_xHL;
  z80[095] := LD_E_A;
  z80[096] := LD_H_B;
  z80[097] := LD_H_C;
  z80[098] := LD_H_D;
  z80[099] := LD_H_E;
  z80[100] := LD_H_H;
  z80[101] := LD_H_L;
  z80[102] := LD_H_xHL;
  z80[103] := LD_H_A;
  z80[104] := LD_L_B;
  z80[105] := LD_L_C;
  z80[106] := LD_L_D;
  z80[107] := LD_L_E;
  z80[108] := LD_L_H;
  z80[109] := LD_L_L;
  z80[110] := LD_L_xHL;
  z80[111] := LD_L_A;
  z80[112] := LD_xHL_B;
  z80[113] := LD_xHL_C;
  z80[114] := LD_xHL_D;
  z80[115] := LD_xHL_E;
  z80[116] := LD_xHL_H;
  z80[117] := LD_xHL_L;
  z80[118] := HALT;
  z80[119] := LD_xHL_A;
  z80[120] := LD_A_B;
  z80[121] := LD_A_C;
  z80[122] := LD_A_D;
  z80[123] := LD_A_E;
  z80[124] := LD_A_H;
  z80[125] := LD_A_L;
  z80[126] := LD_A_xHL;
  z80[127] := LD_A_A;
  z80[128] := ADD_B;
  z80[129] := ADD_C;
  z80[130] := ADD_D;
  z80[131] := ADD_E;
  z80[132] := ADD_H;
  z80[133] := ADD_L;
  z80[134] := ADD_xHL;
  z80[135] := ADD_A;
  z80[136] := ADC_B;
  z80[137] := ADC_C;
  z80[138] := ADC_D;
  z80[139] := ADC_E;
  z80[140] := ADC_H;
  z80[141] := ADC_L;
  z80[142] := ADC_xHL;
  z80[143] := ADC_A;
  z80[144] := SUB_B;
  z80[145] := SUB_C;
  z80[146] := SUB_D;
  z80[147] := SUB_E;
  z80[148] := SUB_H;
  z80[149] := SUB_L;
  z80[150] := SUB_xHL;
  z80[151] := SUB_A;
  z80[152] := SBC_B;
  z80[153] := SBC_C;
  z80[154] := SBC_D;
  z80[155] := SBC_E;
  z80[156] := SBC_H;
  z80[157] := SBC_L;
  z80[158] := SBC_xHL;
  z80[159] := SBC_A;
  z80[160] := AND_B;
  z80[161] := AND_C;
  z80[162] := AND_D;
  z80[163] := AND_E;
  z80[164] := AND_H;
  z80[165] := AND_L;
  z80[166] := AND_xHL;
  z80[167] := AND_A;
  z80[168] := XOR_B;
  z80[169] := XOR_C;
  z80[170] := XOR_D;
  z80[171] := XOR_E;
  z80[172] := XOR_H;
  z80[173] := XOR_L;
  z80[174] := XOR_xHL;
  z80[175] := XOR_A;
  z80[176] := OR_B;
  z80[177] := OR_C;
  z80[178] := OR_D;
  z80[179] := OR_E;
  z80[180] := OR_H;
  z80[181] := OR_L;
  z80[182] := OR_xHL;
  z80[183] := OR_A;
  z80[184] := CP_B;
  z80[185] := CP_C;
  z80[186] := CP_D;
  z80[187] := CP_E;
  z80[188] := CP_H;
  z80[189] := CP_L;
  z80[190] := CP_xHL;
  z80[191] := CP_A;
  z80[192] := RET_NZ;
  z80[193] := POP_BC;
  z80[194] := JP_NZ;
  z80[195] := JP;
  z80[196] := CALL_NZ;
  z80[197] := PUSH_BC;
  z80[198] := ADD_BYTE;
  z80[199] := RST00;
  z80[200] := RET_Z;
  z80[201] := RET;
  z80[202] := JP_Z;
  z80[203] := PFX_CB;
  z80[204] := CALL_Z;
  z80[205] := CALL;
  z80[206] := ADC_BYTE;
  z80[207] := RST08;
  z80[208] := RET_NC;
  z80[209] := POP_DE;
  z80[210] := JP_NC;
  z80[211] := OUTA;
  z80[212] := CALL_NC;
  z80[213] := PUSH_DE;
  z80[214] := SUB_BYTE;
  z80[215] := RST10;
  z80[216] := RET_C;
  z80[217] := EXX;
  z80[218] := JP_C;
  z80[219] := INA;
  z80[220] := CALL_C;
  z80[221] := PFX_DD;
  z80[222] := SBC_BYTE;
  z80[223] := RST18;
  z80[224] := RET_PO;
  z80[225] := POP_HL;
  z80[226] := JP_PO;
  z80[227] := EX_HL_xSP;
  z80[228] := CALL_PO;
  z80[229] := PUSH_HL;
  z80[230] := AND_BYTE;
  z80[231] := RST20;
  z80[232] := RET_PE;
  z80[233] := LD_PC_HL;
  z80[234] := JP_PE;
  z80[235] := EX_DE_HL;
  z80[236] := CALL_PE;
  z80[237] := PFX_ED;
  z80[238] := XOR_BYTE;
  z80[239] := RST28;
  z80[240] := RET_P;
  z80[241] := POP_AF;
  z80[242] := JP_P;
  z80[243] := DI;
  z80[244] := CALL_P;
  z80[245] := PUSH_AF;
  z80[246] := OR_BYTE;
  z80[247] := RST30;
  z80[248] := RET_M;
  z80[249] := LD_SP_HL;
  z80[250] := JP_M;
  z80[251] := EI;
  z80[252] := CALL_M;
  z80[253] := PFX_FD;
  z80[254] := CP_BYTE;
  z80[255] := RST38;
  z80[256] := RLC_B;
  z80[257] := RLC_C;
  z80[258] := RLC_D;
  z80[259] := RLC_E;
  z80[260] := RLC_H;
  z80[261] := RLC_L;
  z80[262] := RLC_xHL;
  z80[263] := RLC_A;
  z80[264] := RRC_B;
  z80[265] := RRC_C;
  z80[266] := RRC_D;
  z80[267] := RRC_E;
  z80[268] := RRC_H;
  z80[269] := RRC_L;
  z80[270] := RRC_xHL;
  z80[271] := RRC_A;
  z80[272] := RL_B;
  z80[273] := RL_C;
  z80[274] := RL_D;
  z80[275] := RL_E;
  z80[276] := RL_H;
  z80[277] := RL_L;
  z80[278] := RL_xHL;
  z80[279] := RL_A;
  z80[280] := RR_B;
  z80[281] := RR_C;
  z80[282] := RR_D;
  z80[283] := RR_E;
  z80[284] := RR_H;
  z80[285] := RR_L;
  z80[286] := RR_xHL;
  z80[287] := RR_A;
  z80[288] := SLA_B;
  z80[289] := SLA_C;
  z80[290] := SLA_D;
  z80[291] := SLA_E;
  z80[292] := SLA_H;
  z80[293] := SLA_L;
  z80[294] := SLA_xHL;
  z80[295] := SLA_A;
  z80[296] := SRA_B;
  z80[297] := SRA_C;
  z80[298] := SRA_D;
  z80[299] := SRA_E;
  z80[300] := SRA_H;
  z80[301] := SRA_L;
  z80[302] := SRA_xHL;
  z80[303] := SRA_A;
  z80[304] := SLL_B;
  z80[305] := SLL_C;
  z80[306] := SLL_D;
  z80[307] := SLL_E;
  z80[308] := SLL_H;
  z80[309] := SLL_L;
  z80[310] := SLL_xHL;
  z80[311] := SLL_A;
  z80[312] := SRL_B;
  z80[313] := SRL_C;
  z80[314] := SRL_D;
  z80[315] := SRL_E;
  z80[316] := SRL_H;
  z80[317] := SRL_L;
  z80[318] := SRL_xHL;
  z80[319] := SRL_A;
  z80[320] := BIT0_B;
  z80[321] := BIT0_C;
  z80[322] := BIT0_D;
  z80[323] := BIT0_E;
  z80[324] := BIT0_H;
  z80[325] := BIT0_L;
  z80[326] := BIT0_xHL;
  z80[327] := BIT0_A;
  z80[328] := BIT1_B;
  z80[329] := BIT1_C;
  z80[330] := BIT1_D;
  z80[331] := BIT1_E;
  z80[332] := BIT1_H;
  z80[333] := BIT1_L;
  z80[334] := BIT1_xHL;
  z80[335] := BIT1_A;
  z80[336] := BIT2_B;
  z80[337] := BIT2_C;
  z80[338] := BIT2_D;
  z80[339] := BIT2_E;
  z80[340] := BIT2_H;
  z80[341] := BIT2_L;
  z80[342] := BIT2_xHL;
  z80[343] := BIT2_A;
  z80[344] := BIT3_B;
  z80[345] := BIT3_C;
  z80[346] := BIT3_D;
  z80[347] := BIT3_E;
  z80[348] := BIT3_H;
  z80[349] := BIT3_L;
  z80[350] := BIT3_xHL;
  z80[351] := BIT3_A;
  z80[352] := BIT4_B;
  z80[353] := BIT4_C;
  z80[354] := BIT4_D;
  z80[355] := BIT4_E;
  z80[356] := BIT4_H;
  z80[357] := BIT4_L;
  z80[358] := BIT4_xHL;
  z80[359] := BIT4_A;
  z80[360] := BIT5_B;
  z80[361] := BIT5_C;
  z80[362] := BIT5_D;
  z80[363] := BIT5_E;
  z80[364] := BIT5_H;
  z80[365] := BIT5_L;
  z80[366] := BIT5_xHL;
  z80[367] := BIT5_A;
  z80[368] := BIT6_B;
  z80[369] := BIT6_C;
  z80[370] := BIT6_D;
  z80[371] := BIT6_E;
  z80[372] := BIT6_H;
  z80[373] := BIT6_L;
  z80[374] := BIT6_xHL;
  z80[375] := BIT6_A;
  z80[376] := BIT7_B;
  z80[377] := BIT7_C;
  z80[378] := BIT7_D;
  z80[379] := BIT7_E;
  z80[380] := BIT7_H;
  z80[381] := BIT7_L;
  z80[382] := BIT7_xHL;
  z80[383] := BIT7_A;
  z80[384] := RES0_B;
  z80[385] := RES0_C;
  z80[386] := RES0_D;
  z80[387] := RES0_E;
  z80[388] := RES0_H;
  z80[389] := RES0_L;
  z80[390] := RES0_xHL;
  z80[391] := RES0_A;
  z80[392] := RES1_B;
  z80[393] := RES1_C;
  z80[394] := RES1_D;
  z80[395] := RES1_E;
  z80[396] := RES1_H;
  z80[397] := RES1_L;
  z80[398] := RES1_xHL;
  z80[399] := RES1_A;
  z80[400] := RES2_B;
  z80[401] := RES2_C;
  z80[402] := RES2_D;
  z80[403] := RES2_E;
  z80[404] := RES2_H;
  z80[405] := RES2_L;
  z80[406] := RES2_xHL;
  z80[407] := RES2_A;
  z80[408] := RES3_B;
  z80[409] := RES3_C;
  z80[410] := RES3_D;
  z80[411] := RES3_E;
  z80[412] := RES3_H;
  z80[413] := RES3_L;
  z80[414] := RES3_xHL;
  z80[415] := RES3_A;
  z80[416] := RES4_B;
  z80[417] := RES4_C;
  z80[418] := RES4_D;
  z80[419] := RES4_E;
  z80[420] := RES4_H;
  z80[421] := RES4_L;
  z80[422] := RES4_xHL;
  z80[423] := RES4_A;
  z80[424] := RES5_B;
  z80[425] := RES5_C;
  z80[426] := RES5_D;
  z80[427] := RES5_E;
  z80[428] := RES5_H;
  z80[429] := RES5_L;
  z80[430] := RES5_xHL;
  z80[431] := RES5_A;
  z80[432] := RES6_B;
  z80[433] := RES6_C;
  z80[434] := RES6_D;
  z80[435] := RES6_E;
  z80[436] := RES6_H;
  z80[437] := RES6_L;
  z80[438] := RES6_xHL;
  z80[439] := RES6_A;
  z80[440] := RES7_B;
  z80[441] := RES7_C;
  z80[442] := RES7_D;
  z80[443] := RES7_E;
  z80[444] := RES7_H;
  z80[445] := RES7_L;
  z80[446] := RES7_xHL;
  z80[447] := RES7_A;
  z80[448] := SET0_B;
  z80[449] := SET0_C;
  z80[450] := SET0_D;
  z80[451] := SET0_E;
  z80[452] := SET0_H;
  z80[453] := SET0_L;
  z80[454] := SET0_xHL;
  z80[455] := SET0_A;
  z80[456] := SET1_B;
  z80[457] := SET1_C;
  z80[458] := SET1_D;
  z80[459] := SET1_E;
  z80[460] := SET1_H;
  z80[461] := SET1_L;
  z80[462] := SET1_xHL;
  z80[463] := SET1_A;
  z80[464] := SET2_B;
  z80[465] := SET2_C;
  z80[466] := SET2_D;
  z80[467] := SET2_E;
  z80[468] := SET2_H;
  z80[469] := SET2_L;
  z80[470] := SET2_xHL;
  z80[471] := SET2_A;
  z80[472] := SET3_B;
  z80[473] := SET3_C;
  z80[474] := SET3_D;
  z80[475] := SET3_E;
  z80[476] := SET3_H;
  z80[477] := SET3_L;
  z80[478] := SET3_xHL;
  z80[479] := SET3_A;
  z80[480] := SET4_B;
  z80[481] := SET4_C;
  z80[482] := SET4_D;
  z80[483] := SET4_E;
  z80[484] := SET4_H;
  z80[485] := SET4_L;
  z80[486] := SET4_xHL;
  z80[487] := SET4_A;
  z80[488] := SET5_B;
  z80[489] := SET5_C;
  z80[490] := SET5_D;
  z80[491] := SET5_E;
  z80[492] := SET5_H;
  z80[493] := SET5_L;
  z80[494] := SET5_xHL;
  z80[495] := SET5_A;
  z80[496] := SET6_B;
  z80[497] := SET6_C;
  z80[498] := SET6_D;
  z80[499] := SET6_E;
  z80[500] := SET6_H;
  z80[501] := SET6_L;
  z80[502] := SET6_xHL;
  z80[503] := SET6_A;
  z80[504] := SET7_B;
  z80[505] := SET7_C;
  z80[506] := SET7_D;
  z80[507] := SET7_E;
  z80[508] := SET7_H;
  z80[509] := SET7_L;
  z80[510] := SET7_xHL;
  z80[511] := SET7_A;
end.

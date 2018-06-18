{+-----------------------------------------------------------------------------+
 | Description: Gameboy Z80-CPU emulation
 | last changes: 02.11.2000
 |
 | http://www.tu-ilmenau.de/~hackbart
 +----------------------------------------------------------------------------+}

unit Z80CPU;

{$MODE Delphi}

interface

var z80:array[0..511] of function:byte;

implementation uses vars,machine,windows;

// temporäre Variablen
var b:Pair;
    cycle:byte;
    w1,w2:Word;

function NOP:byte;
begin
 result:=4;
end;

function LD_BC_WORD:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 mov bc.l,al
 inc word ptr pc.w
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 mov bc.h,al
 inc word ptr pc.w
 end;
 result:=12;
end;

function LD_xBC_A:byte;
begin
 asm
 mov ax,bc.w
 push dword ptr af.h
 push eax
 call SpokeB
 lea esp,[esp+8]
 end;
 result:=8;
end;

function INC_BC:byte;
begin
 asm
  inc bc.w
 end;
 result:=8;
end;

function INC_B:byte;
begin
 asm
 inc bc.h
 lahf
 and eax,$5100
 and af.l,1
 or af.l,ah
 end;

 result:=4;
end;

function DEC_B:byte;
begin
 asm
 dec bc.h
 lahf
 and af.l,1
 and eax,$5500
 or eax,$400
 or af.l,ah
 end;

 result:=4;

end;

function LD_B_BYTE:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 mov bc.h,al
 inc word ptr pc.w
 end;
 result:=8;
end;

function RLCA:byte;
begin
 asm
 rol af.h,1
 lahf
 and eax,$100
 mov af.l,ah
 end;


 result:=4;
end;

function EX_AF_AF:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 push eax

 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 xchg ah,al
 pop ebx
 mov al,bl
 mov bx,sp_.w
 push ebx
 push eax

 push ebx
 push eax
 call SpokeB
 lea esp,[esp+8]

 pop eax
 pop ebx
 inc eax
 xchg bh,bl
 push ebx
 push eax
 call SpokeB
 lea esp,[esp+8]
 end;
 result:=20;
end;

function ADD_HL_BC:byte;
begin
 asm
 and af.l,64
 mov ax,hl.w
 mov bx,bc.w


 mov ecx,eax
 xor ecx,ebx

 add ax,bx
 jnc @1
 or af.l,1
@1:
 xor ecx,eax
 test ecx,4096
 jz @ende
 or af.l,16
@ende:
 mov hl.w,ax

 end;
 result:=8;
end;

function LD_A_xBC:byte;
begin
 asm
 mov ax,bc.w
 push eax
 call speekb
 lea esp,[esp+4]
 mov af.h,al
 end;
 result:=8;
end;

function DEC_BC:byte;
begin
 asm
 dec bc.w
 end;

 result:=8;
end;

function INC_C:byte;
begin
 asm
 inc bc.l
 lahf
 and eax,$5100
 and af.l,1
 or af.l,ah
 end;

 result:=4;
end;

function DEC_C:byte;
begin
 asm
 dec bc.l
 lahf
 and af.l,1
 and eax,$5500
 or eax,$400
 or af.l,ah
 end;

 result:=4;
end;

function LD_C_BYTE:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 mov bc.l,al
 inc word ptr pc.w
 end;
 result:=8;
end;

function RRCA:byte;
begin
 asm
 ror af.h,1
 lahf
 and eax,$100
 mov af.l,ah
 end;


 result:=4;
end;

function DJNZ:byte;
begin
 stop_mode:=1;
 inc(pc.w);
 result:=10;
end;

function LD_DE_WORD:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 mov de.l,al
 inc word ptr pc.w

 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 mov de.h,al
 inc word ptr pc.w
 end;
 result:=12;
end;

function LD_xDE_A:byte;
begin
 asm
 mov ax,de.w
 push dword ptr af.h
 push eax
 call SpokeB
 lea esp,[esp+8]
 end;

 result:=8;
end;

function INC_DE:byte;
begin
 asm
 inc de.w
 end;
 result:=8;
end;

function INC_D:byte;
begin
 asm
 inc de.h
 lahf
 and eax,$5100
 and af.l,1
 or af.l,ah
 end;

 result:=4;
end;

function DEC_D:byte;
begin
 asm
 dec de.h
 lahf
 and af.l,1
 and eax,$5500
 or eax,$400
 or af.l,ah
 end;

 result:=4;
end;

function LD_D_BYTE:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 mov de.h,al
 inc word ptr pc.w
 end;

 result:=8;
end;

function RLA:byte;
begin
 asm
 mov ah,af.l
 and eax,$100
 sahf
 rcl af.h,1
 lahf
 and eax,$100
 mov af.l,ah
 end;

 result:=4;
end;

function JR:byte;
begin
 asm
 xor eax,eax
 mov ax,pc.w
 push eax
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 cbw
 add pc.w,ax
 end;

 result:=8;
end;

function ADD_HL_DE:byte;
begin
 asm
 and af.l,64
 mov ax,hl.w
 mov bx,de.w


 mov ecx,eax
 xor cx,bx

 add ax,bx
 jnc @1
 or af.l,1
@1:
 xor cx,ax
 test cx,4096
 jz @ende
 or af.l,16
@ende:
 mov hl.w,ax

 end;
 result:=8;
end;

function LD_A_xDE:byte;
begin
 asm
 mov ax,de.w
 push eax
 call speekb
 lea esp,[esp+4]
 mov af.h,al
 end;

 result:=8;
end;

function DEC_DE:byte;
begin
 asm
 dec de.w
 end;

 result:=8;
end;

function INC_E:byte;
begin
 asm
 inc de.l
 lahf
 and eax,$5100
 and af.l,1
 or af.l,ah
 end;

 result:=4;
end;

function DEC_E:byte;
begin
 asm
 dec de.l
 lahf
 and af.l,1
 and eax,$5500
 or eax,$400
 or af.l,ah
 end;

 result:=4;
end;

function LD_E_BYTE:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 mov de.l,al
 inc word ptr pc.w
 end;

 result:=8;
end;

function RRA:byte;
begin
 asm
 mov ah,af.l
 sahf
 rcr af.h,1
 lahf
 and ah,1
 mov af.l,ah
 end;
 result:=4;
end;

function JR_NZ:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 test af.l,64
 mov cycle,8
 jnz @ende
 mov cycle,12
 cbw
 add pc.w,ax
@ende:
 end;
 result:=cycle;
end;

function LD_HL_WORD:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 mov hl.l,al
 inc word ptr pc.w
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 mov hl.h,al
 inc word ptr pc.w
 end;
 result:=12;
end;

function LD_xWORD_HL:byte;
begin
 asm
 mov ax,hl.w
 push eax
 push dword ptr af.h
 push eax
 call SpokeB
 lea esp,[esp+8]
 pop eax
 inc eax
 mov hl.w,ax
 end;
 result:=8;
end;

function INC_HL:byte;
begin
 asm
 inc hl.w
 end;
 result:=8;
end;

function INC_H:byte;
begin
 asm
 inc hl.h
 lahf
 and eax,$5100
 and af.l,1
 or af.l,ah
 end;

 result:=4;
end;

function DEC_H:byte;
begin
 asm
 dec hl.h
 lahf
 and af.l,1
 and eax,$5500
 or eax,$400
 or af.l,ah
 end;

 result:=4;
end;

function LD_H_BYTE:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 mov hl.h,al
 inc word ptr pc.w
 end;

 result:=8;
end;

function DAA:byte;
begin
 asm
 mov ah,af.l
 mov bl,ah
 and bl,4
 sahf
 mov al,af.h
 jp @dec_adjust
 daa
 jmp @ddld

@dec_adjust:
 das

@ddld:
 mov af.h,al
 lahf
 and ah,65
 or ah,bl
 mov af.l,ah
end;



 result:=4;
end;

function JR_Z:byte;

begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 test af.l,64
 mov cycle,8
jz @ende
 mov cycle,12
 cbw
 add pc.w,ax
@ende:
 end;

 result:=cycle;
end;

function ADD_HL_HL:byte;
begin
 asm
 and af.l,64
 mov ax,hl.w
 xor ecx,ecx

 add ax,ax
 jnc @1
 or af.l,1
@1:
 xor ecx,eax
 test cx,4096
 jz @ende
 or af.l,16
@ende:
 mov hl.w,ax

 end;

 result:=8;
end;

function LD_HL_xWORD:byte;
begin

 asm
 mov ax,hl.w
 push eax
 push eax
 call speekb
 lea esp,[esp+4]
 mov af.h,al
 pop eax
 inc ax
 mov hl.w,ax
 end;

 result:=8;
end;

function DEC_HL:byte;
begin
 asm
  dec hl.w
 end;

 result:=8;
end;

function INC_L:byte;
begin
 asm
 inc hl.l
 lahf
 and eax,$5100
 and af.l,1
 or af.l,ah
 end;

 result:=4;
end;

function DEC_L:byte;
begin
 asm
 dec hl.l
 lahf
 and af.l,1
 and eax,$5500
 or eax,$400
 or af.l,ah
 end;

 result:=4;
end;

function LD_L_BYTE:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 mov hl.l,al
 end;

 result:=8;
end;

function CPL:byte;
begin
 asm
 xor af.h,255

 or af.l,$14

 end;

 result:=4;
end;

function JR_NC:byte;

begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 test af.l,1
mov cycle,8
 jnz @ende
 mov cycle,12
 cbw
add pc.w,ax
@ende:
 end;

 result:=cycle;
end;

function LD_SP_WORD:byte;
begin
asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 push eax
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 shl eax,8
 pop ebx
 mov al,bl

 mov sp_.w,ax
 end;
 //sp_.w:=wordpeek(pc.w);inc(pc.w,2);
 result:=12;
end;

function LD_xWORD_A:byte;
begin
 asm
 mov ax,hl.w
 push eax
 push dword ptr af.h
 push eax
 call SpokeB
 lea esp,[esp+8]
 pop eax
 dec ax
 mov hl.w,ax
 end;




 result:=8;
end;

function INC_SP:byte;
begin
 asm
 inc sp_.w
 end;

 result:=8;
end;

function INC_xHL:byte;
begin
 asm
 mov ax,hl.w
 push eax
 push eax
 call speekb
 lea esp,[esp+4]
 inc al
 lahf
 and ah,$51
 and af.l,1
 or af.l,ah
 pop ebx
 push eax
 push ebx
 call SpokeB
 lea esp,[esp+8]
 end;


 result:=12;
end;

function DEC_xHL:byte;
begin
 asm
 mov ax,hl.w
 push eax
 push eax
 call speekb
 lea esp,[esp+4]
 dec al
 lahf
 and af.l,1
 and ah,$55
 or eax,$400
 or af.l,ah
 pop ebx
 push eax
 push ebx
 call SpokeB
 lea esp,[esp+8]
 end;
 result:=12;
end;

function LD_xHL_BYTE:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 push eax
 mov ax,hl.w
 push eax
 call SpokeB
 lea esp,[esp+8]
 end;


 result:=12;
end;

function SCF:byte;
begin
 asm
 and af.l,64
 or af.l,1
 end;

 result:=4;
end;

function JR_C:byte;

begin
 asm
push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 test af.l,1
mov cycle,8
 jz @ende
mov cycle,12
 cbw
 add pc.w,ax
@ende:
 end;

 result:=cycle;
end;

function ADD_HL_SP:byte;
begin
 asm
 and af.l,64
 mov ax,hl.w
 mov bx,sp_.w


 mov ecx,eax
 xor ecx,ebx

 add ax,bx
 jnc @1
 or af.l,1
@1:
 xor ecx,eax
 test ecx,4096
 jz @ende
 or af.l,16
@ende:
 mov hl.w,ax
 end;
 result:=8;
end;

function LD_A_xWORD:byte;
begin

 asm
 mov ax,hl.w
 push eax
 push eax
 call speekb
 lea esp,[esp+4]
 mov af.h,al
 pop eax
 dec ax
 mov hl.w,ax
 end;

 result:=8;
end;

function DEC_SP:byte;
begin
 asm
  dec sp_.w
 end;
 result:=8;
end;

function INC_A:byte;
begin
 asm
 inc af.h
 lahf
 and eax,$5100
 and af.l,1
 or af.l,ah
 end;

 result:=4;
end;

function DEC_A:byte;
begin
 asm
 dec af.h
 lahf
 and af.l,1
 and eax,$5500
 or eax,$400
 or af.l,ah
 end;

 result:=4;
end;

function LD_A_BYTE:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 mov af.h,al
 inc word ptr pc.w
 end;

 result:=8;
end;

function CCF:byte;
begin
 asm
 mov ah,af.l
 sahf
 cmc
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=4;
end;

function LD_B_B:byte;
begin
 result:=4;
end;

function LD_B_C:byte;
begin
 bc.h:=bc.l;
 result:=4;
end;

function LD_B_D:byte;
begin
 bc.h:=de.h;
 result:=4;
end;

function LD_B_E:byte;
begin
 bc.h:=de.l;
 result:=4;
end;

function LD_B_H:byte;
begin
 bc.h:=hl.h;
 result:=4;
end;

function LD_B_L:byte;
begin
 bc.h:=hl.l;
 result:=4;
end;

function LD_B_xHL:byte;
begin
 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]
 mov bc.h,al
 end;


 result:=8;
end;

function LD_B_A:byte;
begin
 bc.h:=af.h;
 result:=4;
end;

function LD_C_B:byte;
begin
 bc.l:=bc.h;
 result:=4;
end;

function LD_C_C:byte;
begin
 result:=4;
end;

function LD_C_D:byte;
begin
 bc.l:=de.h;
 result:=4;
end;

function LD_C_E:byte;
begin
 bc.l:=de.l;
 result:=4;
end;

function LD_C_H:byte;
begin
 bc.l:=hl.h;
 result:=4;
end;

function LD_C_L:byte;
begin
 bc.l:=hl.l;
 result:=4;
end;

function LD_C_xHL:byte;
begin
 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]
 mov bc.l,al
 end;

 result:=8;
end;

function LD_C_A:byte;
begin
 bc.l:=af.h;
 result:=4;
end;

function LD_D_B:byte;
begin
 de.h:=bc.h;

 result:=4;
end;

function LD_D_C:byte;
begin
 de.h:=bc.l;
 result:=4;
end;

function LD_D_D:byte;
begin
 result:=4;
end;

function LD_D_E:byte;
begin
 de.h:=de.l;
 result:=4;
end;

function LD_D_H:byte;
begin
 de.h:=hl.h;
 result:=4;
end;

function LD_D_L:byte;
begin
 de.h:=hl.l;
 result:=4;
end;

function LD_D_xHL:byte;
begin
 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]
 mov de.h,al
 end;

 result:=8;
end;

function LD_D_A:byte;
begin
 de.h:=af.h;
 result:=4;
end;

function LD_E_B:byte;
begin
 de.l:=bc.h;
 result:=4;
end;

function LD_E_C:byte;
begin
 de.l:=bc.l;
 result:=4;
end;

function LD_E_D:byte;
begin
 de.l:=de.h;
 result:=4;
end;

function LD_E_E:byte;
begin
 result:=4;
end;

function LD_E_H:byte;
begin
 de.l:=hl.h;
 result:=4;
end;

function LD_E_L:byte;
begin
 de.l:=hl.l;
 result:=4;
end;

function LD_E_xHL:byte;
begin
 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]
 mov de.l,al
 end;
 ;
 result:=8;
end;

function LD_E_A:byte;
begin
 de.l:=af.h;
 result:=4;
end;

function LD_H_B:byte;
begin
 hl.h:=bc.h;
 result:=4;
end;

function LD_H_C:byte;
begin
 hl.h:=bc.l;
 result:=4;
end;

function LD_H_D:byte;
begin
 hl.h:=de.h;
 result:=4;
end;

function LD_H_E:byte;
begin
 hl.h:=de.l;
 result:=4;
end;

function LD_H_H:byte;
begin
 result:=4;
end;

function LD_H_L:byte;
begin
 hl.h:=hl.l;
 result:=4;
end;

function LD_H_xHL:byte;
begin
 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]
 mov hl.h,al
 end;

 result:=8;
end;

function LD_H_A:byte;
begin
 hl.h:=af.h;
 result:=4;
end;

function LD_L_B:byte;
begin
 hl.l:=bc.h;
 result:=4;
end;

function LD_L_C:byte;
begin
 hl.l:=bc.l;
 result:=4;
end;

function LD_L_D:byte;
begin
 hl.l:=de.h;
 result:=4;
end;

function LD_L_E:byte;
begin
 hl.l:=de.l;
 result:=4;
end;

function LD_L_H:byte;
begin
 hl.l:=hl.h;
 result:=4;
end;

function LD_L_L:byte;
begin
 result:=4;
end;

function LD_L_xHL:byte;
begin
 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]
 mov hl.l,al
 end;

 result:=8;
end;

function LD_L_A:byte;
begin
 hl.l:=af.h;
 result:=4;
end;

function LD_xHL_B:byte;
begin
 asm
 mov ax,hl.w
 push dword ptr bc.h
 push eax
 call SpokeB
 lea esp,[esp+8]
 end;

 result:=8;
end;

function LD_xHL_C:byte;
begin
 asm
 mov ax,hl.w
 push dword ptr bc.l
 push eax
 call SpokeB
 lea esp,[esp+8]
 end;

 result:=8;
end;

function LD_xHL_D:byte;
begin
 asm
 mov ax,hl.w
 push dword ptr de.h
 push eax
 call SpokeB
 lea esp,[esp+8]
 end;

 result:=8;
end;

function LD_xHL_E:byte;
begin
 asm
 mov ax,hl.w
 push dword ptr de.l
 push eax
 call SpokeB
 lea esp,[esp+8]
 end;

 result:=8;
end;

function LD_xHL_H:byte;
begin
 asm
 mov ax,hl.w
 push dword ptr hl.h
 push eax
 call SpokeB
 lea esp,[esp+8]
 end;

 result:=8;
end;

function LD_xHL_L:byte;
begin
 asm
 mov ax,hl.w
 push dword ptr hl.l
 push eax
 call SpokeB
 lea esp,[esp+8]
 end;

 result:=8;
end;

function HALT:byte;
begin
 if gbr_ime then halt_mode:=1 else halt_mode:=2;
 result:=4;
end;

function LD_xHL_A:byte;
begin
 asm
 mov ax,hl.w
 push dword ptr af.h
 push eax
 call SpokeB
 lea esp,[esp+8]
 end;

 result:=8;
end;

function LD_A_B:byte;
begin
 af.h:=bc.h;
 result:=4;
end;

function LD_A_C:byte;
begin
 af.h:=bc.l;
 result:=4;
end;

function LD_A_D:byte;
begin
 af.h:=de.h;
 result:=4;
end;

function LD_A_E:byte;
begin
 af.h:=de.l;
 result:=4;
end;

function LD_A_H:byte;
begin
 af.h:=hl.h;
 result:=4;
end;

function LD_A_L:byte;
begin
 af.h:=hl.l;
 result:=4;
end;

function LD_A_xHL:byte;
begin
 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]
 mov af.h,al
 end;

 result:=8;
end;

function LD_A_A:byte;
begin
 result:=4;
end;

function ADD_B:byte;
begin
 asm
 mov al,bc.h
 add af.h,al
 lahf
 and eax,$5100
 mov af.l,ah

 end;
 result:=4;
end;

function ADD_C:byte;
begin
 asm
 mov al,bc.l
 add af.h,al
 lahf
 and ah,81
 mov af.l,ah

 end;
 result:=4;
end;

function ADD_D:byte;
begin
 asm
 mov al,de.h
 add af.h,al
 lahf
 and ah,81
 mov af.l,ah

 end;

 result:=4;
end;

function ADD_E:byte;
begin
 asm
 mov al,de.l
 add af.h,al
 lahf
 and ah,81
 mov af.l,ah

 end;

 result:=4;
end;

function ADD_H:byte;
begin
 asm
 mov al,hl.h
 add af.h,al
 lahf
 and ah,81
 mov af.l,ah

 end;

 result:=4;
end;

function ADD_L:byte;
begin
 asm
 mov al,hl.l
 add af.h,al
 lahf
 and ah,81
 mov af.l,ah

 end;
 result:=4;
end;

function ADD_xHL:byte;
begin

 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]

 add af.h,al
 lahf
 and eax,$5100
 mov af.l,ah

 end;
 result:=8;
end;

function ADD_A:byte;
begin
 asm
 mov al,af.h
 add af.h,al
 lahf
 and ah,81
 mov af.l,ah

 end;

 result:=4;
end;

function ADC_B:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,bc.h
 adc af.h,al
 lahf
 and eax,$5100
 mov af.l,ah

 end;

 result:=4;
end;

function ADC_C:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,bc.l
 adc af.h,al
 lahf
 and eax,$5100
 mov af.l,ah

 end;

 result:=4;
end;

function ADC_D:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,de.h
 adc af.h,al
 lahf
 and eax,$5100
 mov af.l,ah

 end;
 result:=4;
end;

function ADC_E:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,de.l
 adc af.h,al
 lahf
 and eax,$5100
 mov af.l,ah

 end;

 result:=4;
end;

function ADC_H:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,hl.h
 adc af.h,al
 lahf
 and eax,$5100
 mov af.l,ah

 end;

 result:=4;
end;

function ADC_L:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,hl.l
 adc af.h,al
 lahf
 and eax,$5100
 mov af.l,ah

 end;

 result:=4;
end;

function ADC_xHL:byte;
begin

 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]
 mov ah,af.l
 and ah,1
sahf

 adc af.h,al
 lahf
 and eax,$5100
 mov af.l,ah

 end;

 result:=8;
end;

function ADC_A:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,af.h
 adc af.h,al
 lahf
 and eax,$5100
 mov af.l,ah

 end;

 result:=4;
end;

function SUB_B:byte;
begin
 asm
 mov al,bc.h
 sub af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;
 result:=4;
end;

function SUB_C:byte;
begin
 asm
 mov al,bc.l
 sub af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;

 result:=4;
end;

function SUB_D:byte;
begin
 asm
 mov al,de.h
 sub af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;

 result:=4;
end;

function SUB_E:byte;
begin
 asm
 mov al,de.l
 sub af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;


 result:=4;
end;

function SUB_H:byte;
begin
 asm
 mov al,hl.h
 sub af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;

 result:=4;
end;

function SUB_L:byte;
begin
 asm
 mov al,hl.l
 sub af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;

 result:=4;
end;

function SUB_xHL:byte;
begin

 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]

 sub af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;

 result:=8;
end;

function SUB_A:byte;
begin
 asm
 mov al,af.h
 sub af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;
 result:=4;

end;

function SBC_B:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,bc.h
 sbb af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;

 result:=4;

end;

function SBC_C:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,bc.l
 sbb af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;

 result:=4;
end;

function SBC_D:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,de.h
 sbb af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;

 result:=4;
end;

function SBC_E:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,de.l
 sbb af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;

 result:=4;
end;

function SBC_H:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,hl.h
 sbb af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;

 result:=4;
end;

function SBC_L:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,hl.l
 sbb af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;

 result:=4;
end;

function SBC_xHL:byte;
begin

 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]
 mov ah,af.l
 sahf

 sbb af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;
 result:=8;
end;

function SBC_A:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,af.h
 sbb af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;
 result:=4;
end;

function AND_B:byte;
begin
 asm
 mov al,bc.h
 and af.h,al
 lahf
 and eax,$4000
 or eax,$1000
 mov af.l,ah
 end;
 result:=4;
end;

function AND_C:byte;
begin
 asm
 mov al,bc.l
 and af.h,al
 lahf
 and eax,$4000
 or eax,$1000
 mov af.l,ah
 end;

 result:=4;
end;

function AND_D:byte;
begin
 asm
 mov al,de.h
 and af.h,al
 lahf
 and eax,$4000
 or eax,$1000
 mov af.l,ah
 end;
 result:=4;
end;

function AND_E:byte;
begin
 asm
 mov al,de.l
 and af.h,al
 lahf
 and eax,$4000
 or eax,$1000
 mov af.l,ah
 end;
 result:=4;
end;

function AND_H:byte;
begin
 asm
 mov al,hl.h
 and af.h,al
 lahf
 and eax,$4000
 or eax,$1000
 mov af.l,ah
 end;
 result:=4;
end;

function AND_L:byte;
begin
 asm
 mov al,hl.l
 and af.h,al
 lahf
 and eax,$4000
 or eax,$1000
 mov af.l,ah
 end;
 result:=4;
end;

function AND_xHL:byte;
begin


 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]

 and af.h,al
 lahf
 and eax,$4000
 or eax,$1000
 mov af.l,ah
 end;
 result:=8;
end;

function AND_A:byte;
begin
 asm
 mov al,af.h
 and af.h,al
 lahf
 and eax,$4000
 or eax,$1000 // AH
 mov af.l,ah
 end;
 result:=4;
end;

function XOR_B:byte;
begin
 asm
 mov al,bc.h
 xor af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;
 result:=4;
end;

function XOR_C:byte;
begin
 asm
 mov al,bc.l
 xor af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;
 result:=4;
end;

function XOR_D:byte;
begin
 asm
 mov al,de.h
 xor af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;
 result:=4;
end;

function XOR_E:byte;
begin
 asm
 mov al,de.l
 xor af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;
 result:=4;
end;

function XOR_H:byte;
begin
 asm
 mov al,hl.h
 xor af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;
 result:=4;
end;

function XOR_L:byte;
begin
 asm
 mov al,hl.l
 xor af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;
 result:=4;
end;

function XOR_xHL:byte;
begin


 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]

 xor af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;
 result:=8;
end;

function XOR_A:byte;
begin

 af.h:=0;
 af.l:=64;

 result:=4;
end;

function OR_B:byte;
begin
 asm
 mov al,bc.h
 or af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;
 result:=4;
end;

function OR_C:byte;
begin
 asm
 mov al,bc.l
 or af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;

 result:=4;
end;

function OR_D:byte;
begin
 asm
 mov al,de.h
 or af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;

 result:=4;
end;

function OR_E:byte;
begin
 asm
 mov al,de.l
 or af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;
 result:=4;
end;

function OR_H:byte;
begin
 asm
 mov al,hl.h
 or af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;
 result:=4;
end;

function OR_L:byte;
begin
 asm
 mov al,hl.l
 or af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;
 result:=4;
end;

function OR_xHL:byte;
begin

 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]


 or af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;
 result:=8;
end;

function OR_A:byte;
begin
 asm
 mov al,af.h
 or af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;
 result:=4;
end;

function CP_B:byte;
begin
 asm
 mov al,bc.h
 cmp af.h,al
 lahf
 or eax,$400
 mov af.l,ah
 end;

 result:=4;
end;

function CP_C:byte;
begin
 asm
 mov al,bc.l
 cmp af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;

 result:=4;
end;

function CP_D:byte;
begin
 asm
 mov al,de.h
 cmp af.h,al
 lahf
 or eax,$400
 mov af.l,ah

 end;

 result:=4;
end;

function CP_E:byte;
begin
 asm
 mov al,de.l
 cmp af.h,al
 lahf
 or eax,$400
 mov af.l,ah
 end;
 result:=4;
end;

function CP_H:byte;
begin
 asm
 mov al,hl.h
 cmp af.h,al
 lahf
 or eax,$400
 mov af.l,ah
 end;
 result:=4;
end;

function CP_L:byte;
begin
 asm
 mov al,hl.l
 cmp af.h,al
 lahf
 or eax,$400
 mov af.l,ah
 end;
 result:=4;
end;

function CP_xHL:byte;
begin

 asm
 mov ax,hl.w
 push eax
 call speekb
 lea esp,[esp+4]


 cmp af.h,al
 lahf
 or eax,$400
 mov af.l,ah
 end;
 result:=8;
end;

function CP_A:byte;
begin
 asm
 mov al,af.h
 cmp af.h,al
 lahf

 or eax,$400
 mov af.l,ah
 end;
 result:=4;
end;

function RET_NZ:byte;

begin
 asm
 test af.l,64
 mov cycle,8
 jnz @ende
 push dword ptr sp_.w
 call speekb
 lea esp,[esp+4]
 inc word ptr sp_.w
 push eax
 push dword ptr sp_.w
 call speekb
 lea esp,[esp+4]
 inc word ptr sp_.w
 shl eax,8
 pop ebx
 mov al,bl
 mov pc.w,ax
 mov cycle,20
@ende:
 end;

 result:=cycle;
end;

function POP_BC:byte;
begin
 bc.l:=speekb(sp_.w);inc(sp_.w);
 bc.h:=speekb(sp_.w);inc(sp_.w);
 result:=12;
end;

function JP_NZ:byte;
begin
 if (af.l and 64)=0 then
 begin
  w1:=speekb(pc.w);inc(pc.w);
  w2:=speekb(pc.w);inc(pc.w);
  pc.w:=w2*256+w1;
  result:=16;
 end
 else
 begin
  inc(pc.w,2);
  result:=12;
 end;
end;

function JP:byte;
begin
 w1:=speekb(pc.w);inc(pc.w);
 w2:=speekb(pc.w);inc(pc.w);
 pc.w:=w2*256+w1;
 result:=12;
end;

function CALL_NZ:byte;
begin
 if (af.l and 64)=0 then
 begin
  w1:=speekb(pc.w);inc(pc.w);
  w2:=speekb(pc.w);inc(pc.w);
  push_pc;
  pc.w:=w2*256+w1;
  result:=24;
 end
 else
 begin
  inc(pc.w,2);
  result:=12;
 end;
end;

function PUSH_BC:byte;
begin
 dec(sp_.w);spokeb(sp_.w,bc.h);
 dec(sp_.w);spokeb(sp_.w,bc.l);
 result:=16;
end;

function ADD_BYTE:byte;
begin

 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 mov ah,af.l
 sahf

 add af.h,al
 lahf
 and eax,$5100
 mov af.l,ah

 end;

 result:=8;
end;

function RST00:byte;
begin
 push_pc;
 pc.w:=0;
 result:=32;
end;

function RET_Z:byte;
begin
 if (af.l and 64)>0 then
 begin
  w1:=speekb(sp_.w);inc(sp_.w);
  w2:=speekb(sp_.w);inc(sp_.w);
  pc.w:=(w2*256)+w1;
  result:=20;
 end else result:=8;
end;

function RET:byte;
begin
 w1:=speekb(sp_.w);inc(sp_.w);
 w2:=speekb(sp_.w);inc(sp_.w);
 pc.w:=(w2*256)+w1;
 result:=8;
end;

function JP_Z:byte;
begin
 if (af.l and 64)>0 then
 begin
 w1:=speekb(pc.w);inc(pc.w);
 w2:=speekb(pc.w);inc(pc.w);
 pc.w:=w2*256+w1;
 result:=16;
 end
 else
 begin
  inc(pc.w,2);
  result:=12;
 end;
end;

function PFX_CB:byte;
begin
 result:=0;
end;

function CALL_Z:byte;
begin
 if (af.l and 64)>0 then
 begin
  w1:=speekb(pc.w);inc(pc.w);
  w2:=speekb(pc.w);inc(pc.w);
  push_pc;
  pc.w:=w2*256+w1;
  result:=24;
 end
 else
 begin
  inc(pc.w,2);
  result:=12;
 end;
end;

function CALL:byte;
begin
 w1:=speekb(pc.w);inc(pc.w);
 w2:=speekb(pc.w);inc(pc.w);
 push_pc;
 pc.w:=w2*256+w1;

 result:=12;
end;

function ADC_BYTE:byte;
begin

 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 mov ah,af.l
 sahf

 adc af.h,al
 lahf
 and eax,$5100
 mov af.l,ah

 end;

 result:=8;
end;

function RST08:byte;
begin
 push_pc;
 pc.w:=8;
 result:=32;
end;

function RET_NC:byte;
begin
 if (af.l and 1)=0 then
 begin
 w1:=speekb(sp_.w);inc(sp_.w);
 w2:=speekb(sp_.w);inc(sp_.w);
 pc.w:=(w2*256)+w1;
 result:=20;
 end else result:=8;
end;

function POP_DE:byte;
begin
 de.l:=speekb(sp_.w);inc(sp_.w);
 de.h:=speekb(sp_.w);inc(sp_.w);
 result:=12;
end;

function JP_NC:byte;
begin
 if (af.l and 1)=0 then
 begin
  w1:=speekb(pc.w);inc(pc.w);
  w2:=speekb(pc.w);inc(pc.w);
  pc.w:=w2*256+w1;
  result:=16;
  end else begin inc(pc.w,2);result:=12;end;
end;

function OUTA:byte; // there is no use of this function
begin
 result:=0;
end;

function CALL_NC:byte;
begin
 if (af.l and 1)=0 then
 begin
 w1:=speekb(pc.w);inc(pc.w);
 w2:=speekb(pc.w);inc(pc.w);
 push_pc;
 pc.w:=w2*256+w1;
 result:=24;
 end
 else
 begin
  inc(pc.w,2);
  result:=12;
 end;
end;

function PUSH_DE:byte;
begin
 dec(sp_.w);SpokeB(sp_.w,de.h);
 dec(sp_.w);SpokeB(sp_.w,de.l);
 result:=16;
end;

function SUB_BYTE:byte;
begin
 b.l:=speekb(pc.w);inc(pc.w);
 asm
 mov al,af.h
 sub al,b.l
 lahf
 or ah,4
 mov af.l,ah
 mov af.h,al
 end;

 result:=8;
end;

function RST10:byte;
begin
 push_pc;
 pc.w:=$10;
 result:=32;
end;

function RET_C:byte;
begin
 if (af.l and 1)=1 then
 begin
  w1:=speekb(sp_.w);inc(sp_.w);
  w2:=speekb(sp_.w);inc(sp_.w);
  pc.w:=(w2*256)+w1;
 result:=20;
 end else result:=8;
end;

function EXX:byte;
begin
 w1:=speekb(sp_.w);inc(sp_.w);
 w2:=speekb(sp_.w);inc(sp_.w);
 pc.w:=(w2*256)+w1;

ei_fix:=2;
 result:=8;
end;

function JP_C:byte;
begin

 if (af.l and 1)=1 then
 begin
 w1:=speekb(pc.w);inc(pc.w);
 w2:=speekb(pc.w);inc(pc.w);
 pc.w:=w2*256+w1;
 result:=16;
 end
 else begin inc(pc.w,2);result:=12;end;
end;

function INA:byte; // equal to outa, not usuable
begin
 result:=0;
end;

function CALL_C:byte;
begin
 if (af.l and 1)=1 then
 begin
 b.l:=speekb(pc.w);inc(pc.w);
 b.h:=speekb(pc.w);inc(pc.w);
 push_pc;
 pc.w:=b.w;
 result:=24;
 end
 else begin inc(pc.w,2);result:=12;end;
end;

function PFX_DD:byte;
begin
 result:=0;
end;

function SBC_BYTE:byte;
begin
 asm
  push dword ptr pc.w
  call speekb
  lea esp,[esp+4]
  inc word ptr pc.w
  mov ah,af.l
  sahf

  sbb af.h,al
  lahf
  or eax,$400
  mov af.l,ah

  end;
 result:=8;
end;

function RST18:byte;
begin
 push_pc;
 pc.w:=$18;
 result:=32;
end;

function RET_PO:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 mov ah,$ff


 push dword ptr af.h
 push eax
 call SpokeB
 lea esp,[esp+8]
 end;

 result:=12;
end;

function POP_HL:byte;
begin
 hl.l:=speekb(sp_.w);inc(sp_.w);
 hl.h:=speekb(sp_.w);inc(sp_.w);
 result:=12;
end;

function JP_PO:byte;
begin
 asm
 mov al,bc.l
 mov ah,$ff


 push dword ptr af.h
 push eax
 call SpokeB
 lea esp,[esp+8]
 end;

 result:=8;
end;

function EX_HL_xSP:byte;
begin
 B.l:=SpeekB(SP_.W);SpokeB(SP_.W,HL.l);inc(SP_.W);
 B.h:=SpeekB(SP_.W);SpokeB(SP_.W,HL.h);dec(SP_.W);
 HL.W:=B.W;
 result:=4;
end;

function CALL_PO:byte;
begin
 result:=0;
end;

function PUSH_HL:byte;
begin
 dec(sp_.w);SpokeB(sp_.w,hl.h);
 dec(sp_.w);SpokeB(sp_.w,hl.l);
 result:=16;
end;

function AND_BYTE:byte;
begin
 b.l:=speekb(pc.w);inc(pc.w);
 asm
 mov al,b.l
 and af.h,al
 lahf
 and ah,64
 or ah,16
 mov af.l,ah
 end;

 result:=8;
end;

function RST20:byte;
begin
 push_pc;
 pc.w:=$20;
 result:=32;
end;

function RET_PE:byte;
begin
 b.l:=speekb(pc.w);inc(pc.w);
 asm
 mov al,b.l
 cbw
 add sp_.w,ax
 and af.l,17
 end;
 result:=16;
end;

function LD_PC_HL:byte;
begin
 pc.w:=hl.w;
 result:=4;
end;

function JP_PE:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 push eax
 inc word ptr pc.w
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 shl eax,8
 pop ebx
 mov al,bl
 push dword ptr af.h
 push eax
 call SpokeB
 lea esp,[esp+8]
 end;

 result:=16;
end;

function EX_DE_HL:byte;
begin
 b.w:=de.W;
 de.W:=hl.W;
 hl.W:=b.w;
 result:=4;
end;

function CALL_PE:byte; // ????
begin
if af.l and 4>0 then
 Begin
  inc(pc.w,2);push_pc;dec(pc.w,2);
  pc.W:=wordpeek(pc.W);
  result:=17;
 end
 else
 Begin
  inc(pc.W,2);
  result:=10;
 end
end;

function PFX_ED:byte;
begin
 result:=0;
end;

function XOR_BYTE:byte;
begin

 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w

 xor af.h,al
 lahf
 and eax,$4000
 mov af.l,ah
 end;

 result:=8;
end;

function RST28:byte;
begin
 push_pc;
 pc.w:=$28;
 result:=32;
end;

function RET_P:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 xor ah,ah
 mov bx,$ff00
 add ax,bx
 push eax
 call speekb
 lea esp,[esp+4]
 mov af.h,al
 end;

 result:=12;
end;

function POP_AF:byte;
begin
 b.l:=speekb(sp_.w);inc(sp_.w);
 asm
 mov al,b.l
and al,240
 mov bl,al
 shr al,1
 and al,80
 shr bl,4
 and bl,5
 or al,bl
 mov af.l,al
 end;
 af.h:=speekb(sp_.w);inc(sp_.w);
 result:=12;
end;

function JP_P:byte;
begin
 asm
 mov al,bc.l
 xor ah,ah
 mov bx,$ff00
 add ax,bx
 push eax
 call speekb
 lea esp,[esp+4]
 mov af.h,al
 end;

 result:=8;
end;

function DI:byte;
begin
 ei_fix:=0;
 di_fix:=2;
 result:=4;
end;

function CALL_P:byte;
begin
 result:=0;
end;

function PUSH_AF:byte;
begin
 asm
 mov al,af.l
and al,85
 mov bl,al
 shl al,1
 and al,160
 shl bl,4
 and bl,80
 or al,bl
 mov b.l,al
 end;
 dec(sp_.w);SpokeB(sp_.w,af.h);
 dec(sp_.w);SpokeB(sp_.w,b.l);
 result:=16;
end;

function OR_BYTE:byte;
begin
 b.l:=speekb(pc.w);inc(pc.w);
 asm
 mov al,b.l
 or af.h,al
 lahf
 and ah,64
 mov af.l,ah
 end;

 result:=8;
end;

function RST30:byte;
begin
 push_pc;
 pc.w:=$30;
 result:=32;
end;

function RET_M:byte;
begin




 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w


 cbw





 mov bx,sp_.w
 add bx,ax
 mov hl.w,bx
 and af.l,17
 end;
 result:=12;
end;

function LD_SP_HL:byte;
begin
 asm
 mov ax,hl.w
 mov sp_.w,ax
 end;

 result:=8;
end;

function JP_M:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 push eax
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 shl eax,8
 pop ebx
 mov al,bl
 push eax
 call speekb
 lea esp,[esp+4]
 mov af.h,al
 end;



 result:=16;
end;

function EI:byte;
begin
 di_fix:=0;
 ei_fix:=2;
 result:=4;
end;

function CALL_M:byte;
begin
 result:=0;
end;

function PFX_FD:byte;
begin
 result:=0;
end;

function CP_BYTE:byte;
begin
 asm
 push dword ptr pc.w
 call speekb
 lea esp,[esp+4]
 inc word ptr pc.w
 cmp af.h,al
 lahf
 or eax,$400
 mov af.l,ah
 end;
 result:=8;
end;

function RST38:byte;
begin
 push_pc;
 pc.w:=$38;
 result:=32;
end;

function RLC_B:byte;
begin
 if bc.h=0 then af.l:=64
 else
 asm
  rol bc.h,1
  lahf
  and ah,1
  mov af.l,ah
 end;
 result:=8;
end;

function RLC_C:byte;
begin
 if bc.l=0 then af.l:=64
 else
 asm
  rol bc.l,1
  lahf
  and ah,1
  mov af.l,ah
  end;
 result:=8;
end;

function RLC_D:byte;
begin
 if de.h=0 then af.l:=64
 else
 asm
  rol de.h,1
  lahf
  and ah,1
  mov af.l,ah
  end;
 result:=8;
end;

function RLC_E:byte;
begin
 if de.l=0 then af.l:=64
 else
 asm
  rol de.l,1
  lahf
  and ah,1
  mov af.l,ah
 end;
 result:=8;
end;

function RLC_H:byte;
begin
 if hl.h=0 then af.l:=64
 else
 asm
  rol hl.h,1
  lahf
  and ah,1
  mov af.l,ah
 end;
 result:=8;
end;

function RLC_L:byte;
begin
 if hl.l=0 then af.l:=64
 else
 asm
  rol hl.l,1
  lahf
  and ah,1
  mov af.l,ah
 end;
 result:=8;
end;

function RLC_xHL:byte;
begin
 b.l:=speekb(hl.w);
 if b.l=0 then af.l:=64
 else
 asm
  rol b.l,1
  lahf
  and ah,1
  mov af.l,ah
 end;
 SpokeB(hl.w,b.l);
 result:=16;
end;

function RLC_A:byte;
begin
 if af.h=0 then af.l:=64
 else
 asm
  rol af.h,1
  lahf
  and ah,1
  mov af.l,ah
 end;
 result:=8;
end;

function RRC_B:byte;
begin
 if bc.h=0 then af.l:=64
 else
 asm
  ror bc.h,1
  lahf
  and ah,235
  mov af.l,ah
 end;
 result:=8;
end;

function RRC_C:byte;
begin
 if bc.l=0 then af.l:=64
 else
 asm
  ror bc.l,1
  lahf
  and ah,1
  mov af.l,ah
 end;
 result:=8;
end;

function RRC_D:byte;
begin
 if de.h=0 then af.l:=64
 else
 asm
  ror de.h,1
  lahf
  and ah,1
  mov af.l,ah
 end;

 result:=8;
end;

function RRC_E:byte;
begin
 if de.l=0 then af.l:=64
 else
 asm
  ror de.l,1
  lahf
  and ah,1
  mov af.l,ah
 end;
 result:=8;
end;

function RRC_H:byte;
begin
 if hl.h=0 then af.l:=64
 else
 asm
  ror hl.h,1
  lahf
  and ah,1
  mov af.l,ah
 end;
 result:=8;
end;

function RRC_L:byte;
begin
 if hl.l=0 then af.l:=64
 else
 asm
  ror hl.l,1
  lahf
  and ah,1
  mov af.l,ah
 end;

 result:=8;
end;

function RRC_xHL:byte;
begin
 b.l:=speekb(hl.w);
 if b.l=0 then af.l:=64
 else
 asm
  ror b.l,1
  lahf
  and ah,1
  mov af.l,ah
 end;

 SpokeB(w1,b.l);

 result:=16;
end;

function RRC_A:byte;
begin
 if af.h=0 then af.l:=64
 else
 asm
  ror af.h,1
  lahf
  and ah,1
  mov af.l,ah
 end;
 result:=8;
end;

function RL_B:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,bc.h
 rcl al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov bc.h,al
 end;

 result:=8;
end;

function RL_C:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,bc.l
 rcl al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov bc.l,al
 end;


 result:=8;
end;

function RL_D:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,de.h
 rcl al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov de.h,al
 end;


 result:=8;
end;

function RL_E:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,de.l
 rcl al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
 @1:
 mov af.l,ah
 mov de.l,al
 end;



 result:=8;
end;

function RL_H:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,hl.h
 rcl al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov hl.h,al
 end;


 result:=8;
end;

function RL_L:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,hl.l
 rcl al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov hl.l,al
 end;

 result:=8;
end;

function RL_xHL:byte;
begin

 b.l:=speekb(hl.w);
 asm
 mov ah,af.l
 sahf
 mov al,b.l
 rcl al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov b.l,al
 end;



 SpokeB(hl.w,b.l);

 result:=16;
end;

function RL_A:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,af.h
 rcl al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov af.h,al
 end;

 result:=8;
end;

function RR_B:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,bc.h
 rcr al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov bc.h,al
 end;

 result:=8;
end;

function RR_C:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,bc.l
 rcr al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov bc.l,al
 end;

result:=8;
end;

function RR_D:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,de.h
 rcr al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov de.h,al
 end;



 result:=8;
end;

function RR_E:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,de.l
 rcr al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov de.l,al
 end;


 result:=8;
end;

function RR_H:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,hl.h
 rcr al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov hl.h,al
 end;



 result:=8;
end;

function RR_L:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,hl.l
 rcr al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov hl.l,al
 end;



 result:=8;
end;

function RR_xHL:byte;
begin

 b.l:=speekb(hl.w);
 asm
 mov ah,af.l
 sahf
 mov al,b.l
 rcr al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov b.l,al
 end;

 SpokeB(hl.w,b.l);
 result:=16;
end;

function RR_A:byte;
begin
 asm
 mov ah,af.l
 sahf
 mov al,af.h
 rcr al,1
 lahf
 and ah,1
 test al,al
 jnz @1
 or ah,64
@1:
 mov af.l,ah
 mov af.h,al
 end;


 result:=8;
end;

function SLA_B:byte;
begin
 asm
 shl bc.h,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=8;
end;

function SLA_C:byte;
begin
 asm
 shl bc.l,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=8;
end;

function SLA_D:byte;
begin
 asm
 shl de.h,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=8;
end;

function SLA_E:byte;
begin
 asm
 sal de.l,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=8;
end;

function SLA_H:byte;
begin
 asm
 shl hl.h,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=8;
end;

function SLA_L:byte;
begin
 asm
 shl hl.l,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=8;
end;

function SLA_xHL:byte;
begin
 b.l:=speekb(hl.w);
 asm
 shl b.l,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 SpokeB(hl.w,b.l);
 result:=16;
end;

function SLA_A:byte;
begin
 asm
 sal af.h,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=8;
end;

function SRA_B:byte;
begin
 asm
 sar bc.h,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=8;
end;

function SRA_C:byte;
begin
 asm
 sar bc.l,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=8;
end;

function SRA_D:byte;
begin
 asm
 sar de.h,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=8;
end;

function SRA_E:byte;
begin
 asm
 sar de.l,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=8;
end;

function SRA_H:byte;
begin
 asm
 sar hl.h,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=8;
end;

function SRA_L:byte;
begin
 asm
 sar hl.l,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=8;
end;

function SRA_xHL:byte;
begin
 b.l:=speekb(hl.w);
 asm
 sar b.l,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 SpokeB(hl.w,b.l);
 result:=16;
end;

function SRA_A:byte;
begin
 asm
 sar af.h,1
 lahf
 and ah,65
 mov af.l,ah
 end;
 result:=8;
end;

function SLL_B:byte;
begin
 asm
 mov af.l,0
 rol bc.h,4
 cmp bc.h,0
 jnz @ende
 mov af.l,64
@ende:

 end;
 result:=8;
end;

function SLL_C:byte;
begin
 asm
 mov af.l,0
 rol bc.l,4
 cmp bc.l,0
 jnz @ende
 mov af.l,64
@ende:

 end;

 result:=8;
end;

function SLL_D:byte;
begin
 asm
 mov af.l,0
 rol de.h,4
 cmp de.h,0
 jnz @ende
 mov af.l,64
@ende:

 end;

 result:=8;
end;

function SLL_E:byte;
begin
 asm
 mov af.l,0
 rol de.l,4
 cmp de.l,0
 jnz @ende
 mov af.l,64
@ende:

 end;

 result:=8;
end;

function SLL_H:byte;
begin
 asm
 mov af.l,0
 rol hl.h,4
 cmp hl.h,0
 jnz @ende
 mov af.l,64
@ende:

 end;

 result:=8;
end;

function SLL_L:byte;
begin
 asm
 mov af.l,0
 rol hl.l,4
 cmp hl.l,0
 jnz @ende
 mov af.l,64
@ende:

 end;

 result:=8;
end;

function SLL_xHL:byte;
begin

 b.l:=speekb(hl.w);
 asm
 mov af.l,0
 rol b.l,4
 cmp b.l,0
 jnz @ende
 mov af.l,64
@ende:

 end;
 SpokeB(hl.w,b.l);

 result:=16;
end;

function SLL_A:byte;
begin
 asm
 mov af.l,0
 ror af.h,4
 cmp af.h,0
 jnz @ende
 mov af.l,64
@ende:
 end;
 result:=8;
end;

function SRL_B:byte;
begin
 asm
 shr bc.h,1
 lahf
 and ah,65
 mov af.l,ah
 end;


 result:=8;
end;

function SRL_C:byte;
begin
 asm
 shr bc.l,1
 lahf
 and ah,65
 mov af.l,ah
 end;


 result:=8;
end;

function SRL_D:byte;
begin
 asm
 shr de.h,1
 lahf
 and ah,65
 mov af.l,ah
 end;


 result:=8;
end;

function SRL_E:byte;
begin
 asm
 shr de.l,1
 lahf
 and ah,65
 mov af.l,ah
 end;


 result:=8;
end;

function SRL_H:byte;
begin
 asm
 shr hl.h,1
 lahf
 and ah,65
 mov af.l,ah
 end;


 result:=8;
end;

function SRL_L:byte;
begin
 asm
 shr hl.l,1
 lahf
 and ah,65
 mov af.l,ah
 end;


 result:=8;
end;

function SRL_xHL:byte;
begin
 b.l:=speekb(hl.w);
 asm
 shr b.l,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 SpokeB(hl.w,b.l);
 result:=16;
end;

function SRL_A:byte;
begin
 asm
 shr af.h,1
 lahf
 and ah,65
 mov af.l,ah
 end;

 result:=8;
end;

function BIT0_B:byte;
begin

 asm
 test bc.h,1
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah

 end;
 result:=8;
end;

function BIT0_C:byte;
begin
 asm
 test bc.l,1
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT0_D:byte;
begin
 asm
 test de.h,1
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT0_E:byte;
begin
 asm
 test de.l,1
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT0_H:byte;
begin
 asm
 test hl.h,1
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT0_L:byte;
begin
 asm
 test hl.l,1
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT0_xHL:byte;
begin
 b.l:=speekb(hl.w);
 asm
 test b.l,1
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=16;
end;

function BIT0_A:byte;
begin
 asm
 test af.h,1
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT1_B:byte;
begin
 asm
 test bc.h,2
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT1_C:byte;
begin
 asm
 test bc.l,2
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;


 result:=8;
end;

function BIT1_D:byte;
begin
 asm
 test de.h,2
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;


 result:=8;
end;

function BIT1_E:byte;
begin
 asm
 test de.l,2
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;


 result:=8;
end;

function BIT1_H:byte;
begin
 asm
 test hl.h,2
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;


 result:=8;
end;

function BIT1_L:byte;
begin
 asm
 test hl.l,2
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;


 result:=8;
end;

function BIT1_xHL:byte;
begin
 b.l:=speekb(hl.w);
 asm
 test b.l,2
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;


 result:=16;
end;

function BIT1_A:byte;
begin
 asm
 test af.h,2
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;


 result:=8;
end;

function BIT2_B:byte;
begin
 asm
 test bc.h,4
lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;


 result:=8;
end;

function BIT2_C:byte;
begin
 asm
 test bc.l,4
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT2_D:byte;
begin
 asm
 test de.h,4
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT2_E:byte;
begin
 asm
 test de.l,4
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT2_H:byte;
begin
 asm
 test hl.h,4
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT2_L:byte;
begin
 asm
 test hl.l,4
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT2_xHL:byte;
begin

 b.l:=speekb(hl.w);
 asm
 test b.l,4
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=16;
end;

function BIT2_A:byte;
begin
 asm
 test af.h,4
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT3_B:byte;
begin
 asm
 test bc.h,8
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT3_C:byte;
begin
 asm
 test bc.l,8
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT3_D:byte;
begin
 asm
 test de.h,8
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT3_E:byte;
begin
 asm
 test de.l,8
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT3_H:byte;
begin
 asm
 test hl.h,8
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT3_L:byte;
begin
 asm
 test hl.l,8
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=1;
end;

function BIT3_xHL:byte;
begin
 b.l:=speekb(hl.w);
 asm
 test b.l,8
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=16;
end;

function BIT3_A:byte;
begin
 asm
 test af.h,8
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT4_B:byte;
begin
 asm
 test bc.h,16
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT4_C:byte;
begin
 asm
 test bc.l,16
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT4_D:byte;
begin
 asm
 test de.h,16
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT4_E:byte;
begin
 asm
 test de.l,16
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT4_H:byte;
begin
 asm
 test hl.h,16
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT4_L:byte;
begin
 asm
 test hl.l,16
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT4_xHL:byte;
begin
b.l:=speekb(hl.w);
 asm
 test b.l,16
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=16;
end;

function BIT4_A:byte;
begin
 asm
 test af.h,16
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT5_B:byte;
begin
 asm
 test bc.h,32
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT5_C:byte;
begin
 asm
 test bc.l,32
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT5_D:byte;
begin
 asm
 test de.h,32
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT5_E:byte;
begin
 asm
 test de.l,32
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT5_H:byte;
begin
 asm
 test hl.h,32
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT5_L:byte;
begin
 asm
 test hl.l,32
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT5_xHL:byte;
begin

 b.l:=speekb(hl.w);
 asm
 test b.l,32
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=16;
end;

function BIT5_A:byte;
begin
 asm
 test af.h,32
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;

 result:=8;
end;

function BIT6_B:byte;
begin
 asm
 test bc.h,64
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT6_C:byte;
begin
 asm
 test bc.l,64
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT6_D:byte;
begin
 asm
 test de.h,64
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT6_E:byte;
begin
 asm
 test de.l,64
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT6_H:byte;
begin
 asm
 test hl.h,64
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT6_L:byte;
begin
 asm
 test hl.l,64
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT6_xHL:byte;
begin

 b.l:=speekb(hl.w);
 asm
 test b.l,64
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=16;
end;

function BIT6_A:byte;
begin
 asm
 test af.h,64
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT7_B:byte;
begin
 asm
 test bc.h,128
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT7_C:byte;
begin
 asm
 test bc.l,128
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT7_D:byte;
begin
 asm
 test de.h,128
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT7_E:byte;
begin
 asm
 test de.l,128
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT7_H:byte;
begin
 asm
 test hl.h,128
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT7_L:byte;
begin
 asm
 test hl.l,128
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function BIT7_xHL:byte;
begin
b.l:=speekb(hl.w);
 asm
 test b.l,128
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=16;
end;

function BIT7_A:byte;
begin
 asm
 test af.h,128
 lahf
 and ah,64
 and af.l,1
 or ah,16
 or af.l,ah
 end;
 result:=8;
end;

function RES0_B:byte;
begin
 asm
 and bc.h,254
 end;
 result:=8;
end;

function RES0_C:byte;
begin
 asm
 and bc.l,254
 end;

 result:=8;
end;

function RES0_D:byte;
begin
 asm
 and de.h,254
 end;

 result:=8;
end;

function RES0_E:byte;
begin
 asm
 and de.l,254
 end;

 result:=8;
end;

function RES0_H:byte;
begin
 asm
 and hl.h,254
 end;

 result:=8;
end;

function RES0_L:byte;
begin
 asm
 and hl.l,254
 end;

 result:=8;
end;

function RES0_xHL:byte;
begin
 w1:=hl.w;
 b.l:=speekb(w1) and 254;
 SpokeB(w1,b.l);
 result:=16;
end;

function RES0_A:byte;
begin
 af.h:=af.h and 254;
 result:=8;
end;

function RES1_B:byte;
begin
 bc.h:=bc.h and 253;
 result:=8;
end;

function RES1_C:byte;
begin
 bc.l:=bc.l and 253;
 result:=8;
end;

function RES1_D:byte;
begin
 de.h:=de.h and 253;
 result:=8;
end;

function RES1_E:byte;
begin
 de.l:=de.l and 253;
 result:=8;
end;

function RES1_H:byte;
begin
 hl.h:=hl.h and 253;
 result:=8;
end;

function RES1_L:byte;
begin
 hl.l:=hl.l and 253;
 result:=8;
end;

function RES1_xHL:byte;
begin
 w1:=hl.w;
 b.l:=speekb(w1) and 253;
 SpokeB(w1,b.l);

 result:=16;
end;

function RES1_A:byte;
begin
 af.h:=af.h and 253;
 result:=8;
end;

function RES2_B:byte;
begin
 bc.h:=bc.h and 251;
 result:=8;
end;

function RES2_C:byte;
begin
 bc.l:=bc.l and 251;
 result:=8;
end;

function RES2_D:byte;
begin
 de.h:=de.h and 251;
 result:=8;
end;

function RES2_E:byte;
begin
 de.l:= de.l and 251;
 result:=8;
end;

function RES2_H:byte;
begin
 hl.h:= hl.h and 251;
 result:=8;
end;

function RES2_L:byte;
begin
 hl.l:= hl.l and 251;
 result:=8;
end;

function RES2_xHL:byte;
begin
 w1:=hl.w;
 b.l:=speekb(w1);
 b.l:= b.l and 251;
 SpokeB(w1,b.l);
 result:=16;
end;

function RES2_A:byte;
begin
 af.h:= af.h and 251;
 result:=8;
end;

function RES3_B:byte;
begin
 bc.h:= bc.h and 247;
 result:=8;
end;

function RES3_C:byte;
begin
 bc.l:= bc.l and 247;
 result:=8;
end;

function RES3_D:byte;
begin
 de.h:= de.h and 247;
 result:=8;
end;

function RES3_E:byte;
begin
 de.l:= de.l and 247;
 result:=8;
end;

function RES3_H:byte;
begin
 hl.h:= hl.h and 247;
 result:=8;
end;

function RES3_L:byte;
begin
 hl.l:= hl.l and 247;
 result:=8;
end;

function RES3_xHL:byte;
begin
 w1:=hl.w;
 b.l:=speekb(w1);
 b.l:= b.l and 247;
 SpokeB(w1,b.l);
 result:=16;
end;

function RES3_A:byte;
begin
 af.h:= af.h and 247;
 result:=8;
end;

function RES4_B:byte;
begin
 bc.h:= bc.h and 239;
 result:=8;
end;

function RES4_C:byte;
begin
 bc.l:= bc.l and 239;
 result:=8;
end;

function RES4_D:byte;
begin
 de.h:= de.h and 239;
 result:=8;
end;

function RES4_E:byte;
begin
 de.l:= de.l and 239;
 result:=8;
end;

function RES4_H:byte;
begin
 hl.h:= hl.h and 239;
 result:=8;
end;

function RES4_L:byte;
begin
 hl.l:= hl.l and 239;
 result:=8;
end;

function RES4_xHL:byte;
begin
 w1:=hl.w;
 b.l:=speekb(w1);
 b.l:= b.l and 239;
 SpokeB(w1,b.l);
 result:=16;
end;

function RES4_A:byte;
begin
 af.h:= af.h and 239;
 result:=8;
end;

function RES5_B:byte;
begin
 bc.h:= bc.h and 223;
 result:=8;
end;

function RES5_C:byte;
begin
 bc.l:= bc.l and 223;
 result:=8;
end;

function RES5_D:byte;
begin
 de.h:= de.h and 223;
 result:=8;
end;

function RES5_E:byte;
begin
 de.l:= de.l and 223;
 result:=8;
end;

function RES5_H:byte;
begin
 hl.h:= hl.h and 223;
 result:=8;
end;

function RES5_L:byte;
begin
hl.l:=hl.l and 223;

 result:=8;
end;

function RES5_xHL:byte;
begin
 w1:=hl.w;
 b.l:=speekb(w1);
 b.l:= b.l and 223;
 SpokeB(w1,b.l);
 result:=16;
end;

function RES5_A:byte;
begin
 af.h:= af.h and 223;
 result:=8;
end;

function RES6_B:byte;
begin
 bc.h:= bc.h and 191;
 result:=8;
end;

function RES6_C:byte;
begin
 bc.l:= bc.l and 191;
 result:=8;
end;

function RES6_D:byte;
begin
 de.h:= de.h and 191;
 result:=8;
end;

function RES6_E:byte;
begin
 de.l:= de.l and 191;
 result:=8;
end;

function RES6_H:byte;
begin
 hl.h:= hl.h and 191;
 result:=8;
end;

function RES6_L:byte;
begin
 hl.l:= hl.l and 191;
 result:=8;
end;

function RES6_xHL:byte;
begin
 w1:=hl.w;
 b.l:=speekb(w1);
 b.l:= b.l and 191;
 SpokeB(w1,b.l);
 result:=16;
end;

function RES6_A:byte;
begin
 af.h:= af.h and 191;
 result:=8;
end;

function RES7_B:byte;
begin
 bc.h:= bc.h and 127;
 result:=8;
end;

function RES7_C:byte;
begin
 bc.l:= bc.l and 127;
 result:=8;
end;

function RES7_D:byte;
begin
 de.h:= de.h and 127;
 result:=8;
end;

function RES7_E:byte;
begin
 de.l:= de.l and 127;
 result:=8;
end;

function RES7_H:byte;
begin
 hl.h:= hl.h and 127;
 result:=8;
end;

function RES7_L:byte;
begin
 hl.l:= hl.l and 127;
 result:=8;
end;

function RES7_xHL:byte;
begin
 w1:=hl.w;
 b.l:=speekb(w1);
 b.l:= b.l and 127;
 SpokeB(w1,b.l);
 result:=16;
end;

function RES7_A:byte;
begin
 af.h:= af.h and 127;
 result:=8;
end;

function SET0_B:byte;
begin
 bc.h:=bc.h or 1;
 result:=8;
end;

function SET0_C:byte;
begin
 bc.l:=bc.l or 1;
 result:=8;
end;

function SET0_D:byte;
begin
 de.h:=de.h or 1;
 result:=8;
end;

function SET0_E:byte;
begin
 de.l:=de.l or 1;
 result:=8;
end;

function SET0_H:byte;
begin
 hl.h:=hl.h or 1;
 result:=8;
end;

function SET0_L:byte;
begin
 hl.l:=hl.l or 1;
 result:=8;
end;

function SET0_xHL:byte;
begin
 w1:=hl.w;
 b.l:=speekb(w1);
 b.l:=b.l or 1;
 SpokeB(w1,b.l);
 result:=16;
end;

function SET0_A:byte;
begin
 af.h:=af.h or 1;
 result:=8;
end;

function SET1_B:byte;
begin
 bc.h:=bc.h or 2;
 result:=8;
end;

function SET1_C:byte;
begin
 bc.l:=bc.l or 2;
 result:=8;
end;

function SET1_D:byte;
begin
 de.h:=de.h or 2;
 result:=8;
end;

function SET1_E:byte;
begin
 de.l:=de.l or 2;
 result:=8;
end;

function SET1_H:byte;
begin
 hl.h:=hl.h or 2;
 result:=8;
end;

function SET1_L:byte;
begin
 hl.l:=hl.l or 2;
 result:=8;
end;

function SET1_xHL:byte;
begin
 w1:=hl.w;
 b.l:=speekb(w1) or 2;
 SpokeB(w1,b.l);
 result:=16;
end;

function SET1_A:byte;
begin
 af.h:=af.h or 2;
 result:=8;
end;

function SET2_B:byte;
begin
 bc.h:=bc.h or 4;
 result:=8;
end;

function SET2_C:byte;
begin
 bc.l:=bc.l or 4;
 result:=8;
end;

function SET2_D:byte;
begin
 de.h:=de.h or 4;
 result:=8;
end;

function SET2_E:byte;
begin
 de.l:=de.l or 4;
 result:=8;
end;

function SET2_H:byte;
begin
 hl.h:=hl.h or 4;
 result:=8;
end;

function SET2_L:byte;
begin
 hl.l:=hl.l or 4;
 result:=8;
end;

function SET2_xHL:byte;
begin
 w1:=hl.w;
 b.l:=speekb(w1) or 4;
 SpokeB(w1,b.l);
 result:=16;
end;

function SET2_A:byte;
begin
 af.h:=af.h or 4;
 result:=8;
end;

function SET3_B:byte;
begin
 bc.h:=bc.h or 8;
 result:=8;
end;

function SET3_C:byte;
begin
 bc.l:=bc.l or 8;
 result:=8 ;
end;

function SET3_D:byte;
begin
 de.h:=de.h or 8;
 result:=8;
end;

function SET3_E:byte;
begin
 de.l:=de.l or 8;
 result:=8;
end;

function SET3_H:byte;
begin
 hl.h:=hl.h or 8;
 result:=8;
end;

function SET3_L:byte;
begin
 hl.l:=hl.l or 8;
 result:=8;
end;

function SET3_xHL:byte;
begin

 asm
 mov ax,hl.w
 push eax
 push eax
 call speekb
 lea esp,[esp+4]
 or al,8
 pop ebx
 push eax
 push ebx
 call SpokeB
 lea esp,[esp+8]
 end;

 result:=16;
end;

function SET3_A:byte;
begin
 af.h:=af.h or 8;
 result:=8;
end;

function SET4_B:byte;
begin
 bc.h:=bc.h or 16;
 result:=8;
end;

function SET4_C:byte;
begin
 bc.l:=bc.l or 16;
 result:=8;
end;

function SET4_D:byte;
begin
 de.h:=de.h or 16;
 result:=8;
end;

function SET4_E:byte;
begin
 de.l:=de.l or 16;
 result:=8;
end;

function SET4_H:byte;
begin
 hl.h:=hl.h or 16;
 result:=8;
end;

function SET4_L:byte;
begin
 hl.l:=hl.l or 16;
 result:=8;
end;

function SET4_xHL:byte;
begin
 w1:=hl.w;
 b.l:=speekb(w1) or 16;
 SpokeB(w1,b.l);
 result:=16;
end;

function SET4_A:byte;
begin
 af.h:=af.h or 16;
 result:=8;
end;

function SET5_B:byte;
begin
 bc.h:=bc.h or 32;
 result:=8;
end;

function SET5_C:byte;
begin
 bc.l:=bc.l or 32;
 result:=8;
end;

function SET5_D:byte;
begin
 de.h:=de.h or 32;
 result:=8;
end;

function SET5_E:byte;
begin
 de.l:=de.l or 32;
 result:=8;
end;

function SET5_H:byte;
begin
 hl.h:=hl.h or 32;
 result:=8;
end;

function SET5_L:byte;
begin
 hl.l:=hl.l or 32;
 result:=8;
end;

function SET5_xHL:byte;
begin
 w1:=hl.w;
 b.l:=speekb(w1) or 32;
 SpokeB(w1,b.l);
 result:=16;
end;

function SET5_A:byte;
begin
 af.h:=af.h or 32;
 result:=8;
end;

function SET6_B:byte;
begin
 bc.h:=bc.h or 64;
 result:=8;
end;

function SET6_C:byte;
begin
 bc.l:=bc.l or 64;
 result:=8;
end;

function SET6_D:byte;
begin
 de.h:=de.h or 64;
 result:=8;
end;

function SET6_E:byte;
begin
 de.l:=de.l or 64;
 result:=8;
end;

function SET6_H:byte;
begin
 hl.h:=hl.h or 64;
 result:=8;
end;

function SET6_L:byte;
begin
 hl.l:=hl.l or 64;
 result:=8;
end;

function SET6_xHL:byte;
begin
 w1:=hl.w;
 b.l:=speekb(w1) or 64;
 SpokeB(w1,b.l);
 result:=16;
end;

function SET6_A:byte;
begin
 af.h:=af.h or 64;
 result:=8;
end;

function SET7_B:byte;
begin
 bc.h:=bc.h or 128;
 result:=8;
end;

function SET7_C:byte;
begin
 bc.l:=bc.l or 128;
 result:=8;
end;

function SET7_D:byte;
begin
 de.h:=de.h or 128;
 result:=8;
end;

function SET7_E:byte;
begin
 asm
 or de.l,128
 end;
 result:=8;
end;

function SET7_H:byte;
begin
 asm
 or hl.h,128
 end;

 result:=8;
 end;

function SET7_L:byte;
begin
 asm
  or hl.l,128
 end;
 result:=8;
end;

function SET7_xHL:byte;
begin
 asm
 mov ax,hl.w
 push eax
 push eax
 call speekb
 lea esp,[esp+4]
 pop ebx
 or al,128
 push eax
 push ebx
 call SpokeB
 lea esp,[esp+8]
 end;
 result:=16;
end;

function SET7_A:byte;
begin
 asm
  or af.h,128
 end;
 result:=8;
end;

begin
// Generating Tables
 z80[000]:=NOP;
 z80[001]:=LD_BC_WORD;
 z80[002]:=LD_xBC_A;
 z80[003]:=INC_BC;
 z80[004]:=INC_B;
 z80[005]:=DEC_B;
 z80[006]:=LD_B_BYTE;
 z80[007]:=RLCA;
 z80[008]:=EX_AF_AF;
 z80[009]:=ADD_HL_BC;
 z80[010]:=LD_A_xBC;
 z80[011]:=DEC_BC;
 z80[012]:=INC_C;
 z80[013]:=DEC_C;
 z80[014]:=LD_C_BYTE;
 z80[015]:=RRCA;
 z80[016]:=DJNZ;
 z80[017]:=LD_DE_WORD;
 z80[018]:=LD_xDE_A;
 z80[019]:=INC_DE;
 z80[020]:=INC_D;
 z80[021]:=DEC_D;
 z80[022]:=LD_D_BYTE;
 z80[023]:=RLA;
 z80[024]:=JR;
 z80[025]:=ADD_HL_DE;
 z80[026]:=LD_A_xDE;
 z80[027]:=DEC_DE;
 z80[028]:=INC_E;
 z80[029]:=DEC_E;
 z80[030]:=LD_E_BYTE;
 z80[031]:=RRA;
 z80[032]:=JR_NZ;
 z80[033]:=LD_HL_WORD;
 z80[034]:=LD_xWORD_HL;
 z80[035]:=INC_HL;
 z80[036]:=INC_H;
 z80[037]:=DEC_H;
 z80[038]:=LD_H_BYTE;
 z80[039]:=DAA;
 z80[040]:=JR_Z;
 z80[041]:=ADD_HL_HL;
 z80[042]:=LD_HL_xWORD;
 z80[043]:=DEC_HL;
 z80[044]:=INC_L;
 z80[045]:=DEC_L;
 z80[046]:=LD_L_BYTE;
 z80[047]:=CPL;
 z80[048]:=JR_NC;
 z80[049]:=LD_SP_WORD;
 z80[050]:=LD_xWORD_A;
 z80[051]:=INC_SP;
 z80[052]:=INC_xHL;
 z80[053]:=DEC_xHL;
 z80[054]:=LD_xHL_BYTE;
 z80[055]:=SCF;
 z80[056]:=JR_C;
 z80[057]:=ADD_HL_SP;
 z80[058]:=LD_A_xWORD;
 z80[059]:=DEC_SP;
 z80[060]:=INC_A;
 z80[061]:=DEC_A;
 z80[062]:=LD_A_BYTE;
 z80[063]:=CCF;
 z80[064]:=LD_B_B;
 z80[065]:=LD_B_C;
 z80[066]:=LD_B_D;
 z80[067]:=LD_B_E;
 z80[068]:=LD_B_H;
 z80[069]:=LD_B_L;
 z80[070]:=LD_B_xHL;
 z80[071]:=LD_B_A;
 z80[072]:=LD_C_B;
 z80[073]:=LD_C_C;
 z80[074]:=LD_C_D;
 z80[075]:=LD_C_E;
 z80[076]:=LD_C_H;
 z80[077]:=LD_C_L;
 z80[078]:=LD_C_xHL;
 z80[079]:=LD_C_A;
 z80[080]:=LD_D_B;
 z80[081]:=LD_D_C;
 z80[082]:=LD_D_D;
 z80[083]:=LD_D_E;
 z80[084]:=LD_D_H;
 z80[085]:=LD_D_L;
 z80[086]:=LD_D_xHL;
 z80[087]:=LD_D_A;
 z80[088]:=LD_E_B;
 z80[089]:=LD_E_C;
 z80[090]:=LD_E_D;
 z80[091]:=LD_E_E;
 z80[092]:=LD_E_H;
 z80[093]:=LD_E_L;
 z80[094]:=LD_E_xHL;
 z80[095]:=LD_E_A;
 z80[096]:=LD_H_B;
 z80[097]:=LD_H_C;
 z80[098]:=LD_H_D;
 z80[099]:=LD_H_E;
 z80[100]:=LD_H_H;
 z80[101]:=LD_H_L;
 z80[102]:=LD_H_xHL;
 z80[103]:=LD_H_A;
 z80[104]:=LD_L_B;
 z80[105]:=LD_L_C;
 z80[106]:=LD_L_D;
 z80[107]:=LD_L_E;
 z80[108]:=LD_L_H;
 z80[109]:=LD_L_L;
 z80[110]:=LD_L_xHL;
 z80[111]:=LD_L_A;
 z80[112]:=LD_xHL_B;
 z80[113]:=LD_xHL_C;
 z80[114]:=LD_xHL_D;
 z80[115]:=LD_xHL_E;
 z80[116]:=LD_xHL_H;
 z80[117]:=LD_xHL_L;
 z80[118]:=HALT;
 z80[119]:=LD_xHL_A;
 z80[120]:=LD_A_B;
 z80[121]:=LD_A_C;
 z80[122]:=LD_A_D;
 z80[123]:=LD_A_E;
 z80[124]:=LD_A_H;
 z80[125]:=LD_A_L;
 z80[126]:=LD_A_xHL;
 z80[127]:=LD_A_A;
 z80[128]:=ADD_B;
 z80[129]:=ADD_C;
 z80[130]:=ADD_D;
 z80[131]:=ADD_E;
 z80[132]:=ADD_H;
 z80[133]:=ADD_L;
 z80[134]:=ADD_xHL;
 z80[135]:=ADD_A;
 z80[136]:=ADC_B;
 z80[137]:=ADC_C;
 z80[138]:=ADC_D;
 z80[139]:=ADC_E;
 z80[140]:=ADC_H;
 z80[141]:=ADC_L;
 z80[142]:=ADC_xHL;
 z80[143]:=ADC_A;
 z80[144]:=SUB_B;
 z80[145]:=SUB_C;
 z80[146]:=SUB_D;
 z80[147]:=SUB_E;
 z80[148]:=SUB_H;
 z80[149]:=SUB_L;
 z80[150]:=SUB_xHL;
 z80[151]:=SUB_A;
 z80[152]:=SBC_B;
 z80[153]:=SBC_C;
 z80[154]:=SBC_D;
 z80[155]:=SBC_E;
 z80[156]:=SBC_H;
 z80[157]:=SBC_L;
 z80[158]:=SBC_xHL;
 z80[159]:=SBC_A;
 z80[160]:=AND_B;
 z80[161]:=AND_C;
 z80[162]:=AND_D;
 z80[163]:=AND_E;
 z80[164]:=AND_H;
 z80[165]:=AND_L;
 z80[166]:=AND_xHL;
 z80[167]:=AND_A;
 z80[168]:=XOR_B;
 z80[169]:=XOR_C;
 z80[170]:=XOR_D;
 z80[171]:=XOR_E;
 z80[172]:=XOR_H;
 z80[173]:=XOR_L;
 z80[174]:=XOR_xHL;
 z80[175]:=XOR_A;
 z80[176]:=OR_B;
 z80[177]:=OR_C;
 z80[178]:=OR_D;
 z80[179]:=OR_E;
 z80[180]:=OR_H;
 z80[181]:=OR_L;
 z80[182]:=OR_xHL;
 z80[183]:=OR_A;
 z80[184]:=CP_B;
 z80[185]:=CP_C;
 z80[186]:=CP_D;
 z80[187]:=CP_E;
 z80[188]:=CP_H;
 z80[189]:=CP_L;
 z80[190]:=CP_xHL;
 z80[191]:=CP_A;
 z80[192]:=RET_NZ;
 z80[193]:=POP_BC;
 z80[194]:=JP_NZ;
 z80[195]:=JP;
 z80[196]:=CALL_NZ;
 z80[197]:=PUSH_BC;
 z80[198]:=ADD_BYTE;
 z80[199]:=RST00;
 z80[200]:=RET_Z;
 z80[201]:=RET;
 z80[202]:=JP_Z;
 z80[203]:=PFX_CB;
 z80[204]:=CALL_Z;
 z80[205]:=CALL;
 z80[206]:=ADC_BYTE;
 z80[207]:=RST08;
 z80[208]:=RET_NC;
 z80[209]:=POP_DE;
 z80[210]:=JP_NC;
 z80[211]:=OUTA;
 z80[212]:=CALL_NC;
 z80[213]:=PUSH_DE;
 z80[214]:=SUB_BYTE;
 z80[215]:=RST10;
 z80[216]:=RET_C;
 z80[217]:=EXX;
 z80[218]:=JP_C;
 z80[219]:=INA;
 z80[220]:=CALL_C;
 z80[221]:=PFX_DD;
 z80[222]:=SBC_BYTE;
 z80[223]:=RST18;
 z80[224]:=RET_PO;
 z80[225]:=POP_HL;
 z80[226]:=JP_PO;
 z80[227]:=EX_HL_xSP;
 z80[228]:=CALL_PO;
 z80[229]:=PUSH_HL;
 z80[230]:=AND_BYTE;
 z80[231]:=RST20;
 z80[232]:=RET_PE;
 z80[233]:=LD_PC_HL;
 z80[234]:=JP_PE;
 z80[235]:=EX_DE_HL;
 z80[236]:=CALL_PE;
 z80[237]:=PFX_ED;
 z80[238]:=XOR_BYTE;
 z80[239]:=RST28;
 z80[240]:=RET_P;
 z80[241]:=POP_AF;
 z80[242]:=JP_P;
 z80[243]:=DI;
 z80[244]:=CALL_P;
 z80[245]:=PUSH_AF;
 z80[246]:=OR_BYTE;
 z80[247]:=RST30;
 z80[248]:=RET_M;
 z80[249]:=LD_SP_HL;
 z80[250]:=JP_M;
 z80[251]:=EI;
 z80[252]:=CALL_M;
 z80[253]:=PFX_FD;
 z80[254]:=CP_BYTE;
 z80[255]:=RST38;
 z80[256]:=RLC_B;
 z80[257]:=RLC_C;
 z80[258]:=RLC_D;
 z80[259]:=RLC_E;
 z80[260]:=RLC_H;
 z80[261]:=RLC_L;
 z80[262]:=RLC_xHL;
 z80[263]:=RLC_A;
 z80[264]:=RRC_B;
 z80[265]:=RRC_C;
 z80[266]:=RRC_D;
 z80[267]:=RRC_E;
 z80[268]:=RRC_H;
 z80[269]:=RRC_L;
 z80[270]:=RRC_xHL;
 z80[271]:=RRC_A;
 z80[272]:=RL_B;
 z80[273]:=RL_C;
 z80[274]:=RL_D;
 z80[275]:=RL_E;
 z80[276]:=RL_H;
 z80[277]:=RL_L;
 z80[278]:=RL_xHL;
 z80[279]:=RL_A;
 z80[280]:=RR_B;
 z80[281]:=RR_C;
 z80[282]:=RR_D;
 z80[283]:=RR_E;
 z80[284]:=RR_H;
 z80[285]:=RR_L;
 z80[286]:=RR_xHL;
 z80[287]:=RR_A;
 z80[288]:=SLA_B;
 z80[289]:=SLA_C;
 z80[290]:=SLA_D;
 z80[291]:=SLA_E;
 z80[292]:=SLA_H;
 z80[293]:=SLA_L;
 z80[294]:=SLA_xHL;
 z80[295]:=SLA_A;
 z80[296]:=SRA_B;
 z80[297]:=SRA_C;
 z80[298]:=SRA_D;
 z80[299]:=SRA_E;
 z80[300]:=SRA_H;
 z80[301]:=SRA_L;
 z80[302]:=SRA_xHL;
 z80[303]:=SRA_A;
 z80[304]:=SLL_B;
 z80[305]:=SLL_C;
 z80[306]:=SLL_D;
 z80[307]:=SLL_E;
 z80[308]:=SLL_H;
 z80[309]:=SLL_L;
 z80[310]:=SLL_xHL;
 z80[311]:=SLL_A;
 z80[312]:=SRL_B;
 z80[313]:=SRL_C;
 z80[314]:=SRL_D;
 z80[315]:=SRL_E;
 z80[316]:=SRL_H;
 z80[317]:=SRL_L;
 z80[318]:=SRL_xHL;
 z80[319]:=SRL_A;
 z80[320]:=BIT0_B;
 z80[321]:=BIT0_C;
 z80[322]:=BIT0_D;
 z80[323]:=BIT0_E;
 z80[324]:=BIT0_H;
 z80[325]:=BIT0_L;
 z80[326]:=BIT0_xHL;
 z80[327]:=BIT0_A;
 z80[328]:=BIT1_B;
 z80[329]:=BIT1_C;
 z80[330]:=BIT1_D;
 z80[331]:=BIT1_E;
 z80[332]:=BIT1_H;
 z80[333]:=BIT1_L;
 z80[334]:=BIT1_xHL;
 z80[335]:=BIT1_A;
 z80[336]:=BIT2_B;
 z80[337]:=BIT2_C;
 z80[338]:=BIT2_D;
 z80[339]:=BIT2_E;
 z80[340]:=BIT2_H;
 z80[341]:=BIT2_L;
 z80[342]:=BIT2_xHL;
 z80[343]:=BIT2_A;
 z80[344]:=BIT3_B;
 z80[345]:=BIT3_C;
 z80[346]:=BIT3_D;
 z80[347]:=BIT3_E;
 z80[348]:=BIT3_H;
 z80[349]:=BIT3_L;
 z80[350]:=BIT3_xHL;
 z80[351]:=BIT3_A;
 z80[352]:=BIT4_B;
 z80[353]:=BIT4_C;
 z80[354]:=BIT4_D;
 z80[355]:=BIT4_E;
 z80[356]:=BIT4_H;
 z80[357]:=BIT4_L;
 z80[358]:=BIT4_xHL;
 z80[359]:=BIT4_A;
 z80[360]:=BIT5_B;
 z80[361]:=BIT5_C;
 z80[362]:=BIT5_D;
 z80[363]:=BIT5_E;
 z80[364]:=BIT5_H;
 z80[365]:=BIT5_L;
 z80[366]:=BIT5_xHL;
 z80[367]:=BIT5_A;
 z80[368]:=BIT6_B;
 z80[369]:=BIT6_C;
 z80[370]:=BIT6_D;
 z80[371]:=BIT6_E;
 z80[372]:=BIT6_H;
 z80[373]:=BIT6_L;
 z80[374]:=BIT6_xHL;
 z80[375]:=BIT6_A;
 z80[376]:=BIT7_B;
 z80[377]:=BIT7_C;
 z80[378]:=BIT7_D;
 z80[379]:=BIT7_E;
 z80[380]:=BIT7_H;
 z80[381]:=BIT7_L;
 z80[382]:=BIT7_xHL;
 z80[383]:=BIT7_A;
 z80[384]:=RES0_B;
 z80[385]:=RES0_C;
 z80[386]:=RES0_D;
 z80[387]:=RES0_E;
 z80[388]:=RES0_H;
 z80[389]:=RES0_L;
 z80[390]:=RES0_xHL;
 z80[391]:=RES0_A;
 z80[392]:=RES1_B;
 z80[393]:=RES1_C;
 z80[394]:=RES1_D;
 z80[395]:=RES1_E;
 z80[396]:=RES1_H;
 z80[397]:=RES1_L;
 z80[398]:=RES1_xHL;
 z80[399]:=RES1_A;
 z80[400]:=RES2_B;
 z80[401]:=RES2_C;
 z80[402]:=RES2_D;
 z80[403]:=RES2_E;
 z80[404]:=RES2_H;
 z80[405]:=RES2_L;
 z80[406]:=RES2_xHL;
 z80[407]:=RES2_A;
 z80[408]:=RES3_B;
 z80[409]:=RES3_C;
 z80[410]:=RES3_D;
 z80[411]:=RES3_E;
 z80[412]:=RES3_H;
 z80[413]:=RES3_L;
 z80[414]:=RES3_xHL;
 z80[415]:=RES3_A;
 z80[416]:=RES4_B;
 z80[417]:=RES4_C;
 z80[418]:=RES4_D;
 z80[419]:=RES4_E;
 z80[420]:=RES4_H;
 z80[421]:=RES4_L;
 z80[422]:=RES4_xHL;
 z80[423]:=RES4_A;
 z80[424]:=RES5_B;
 z80[425]:=RES5_C;
 z80[426]:=RES5_D;
 z80[427]:=RES5_E;
 z80[428]:=RES5_H;
 z80[429]:=RES5_L;
 z80[430]:=RES5_xHL;
 z80[431]:=RES5_A;
 z80[432]:=RES6_B;
 z80[433]:=RES6_C;
 z80[434]:=RES6_D;
 z80[435]:=RES6_E;
 z80[436]:=RES6_H;
 z80[437]:=RES6_L;
 z80[438]:=RES6_xHL;
 z80[439]:=RES6_A;
 z80[440]:=RES7_B;
 z80[441]:=RES7_C;
 z80[442]:=RES7_D;
 z80[443]:=RES7_E;
 z80[444]:=RES7_H;
 z80[445]:=RES7_L;
 z80[446]:=RES7_xHL;
 z80[447]:=RES7_A;
 z80[448]:=SET0_B;
 z80[449]:=SET0_C;
 z80[450]:=SET0_D;
 z80[451]:=SET0_E;
 z80[452]:=SET0_H;
 z80[453]:=SET0_L;
 z80[454]:=SET0_xHL;
 z80[455]:=SET0_A;
 z80[456]:=SET1_B;
 z80[457]:=SET1_C;
 z80[458]:=SET1_D;
 z80[459]:=SET1_E;
 z80[460]:=SET1_H;
 z80[461]:=SET1_L;
 z80[462]:=SET1_xHL;
 z80[463]:=SET1_A;
 z80[464]:=SET2_B;
 z80[465]:=SET2_C;
 z80[466]:=SET2_D;
 z80[467]:=SET2_E;
 z80[468]:=SET2_H;
 z80[469]:=SET2_L;
 z80[470]:=SET2_xHL;
 z80[471]:=SET2_A;
 z80[472]:=SET3_B;
 z80[473]:=SET3_C;
 z80[474]:=SET3_D;
 z80[475]:=SET3_E;
 z80[476]:=SET3_H;
 z80[477]:=SET3_L;
 z80[478]:=SET3_xHL;
 z80[479]:=SET3_A;
 z80[480]:=SET4_B;
 z80[481]:=SET4_C;
 z80[482]:=SET4_D;
 z80[483]:=SET4_E;
 z80[484]:=SET4_H;
 z80[485]:=SET4_L;
 z80[486]:=SET4_xHL;
 z80[487]:=SET4_A;
 z80[488]:=SET5_B;
 z80[489]:=SET5_C;
 z80[490]:=SET5_D;
 z80[491]:=SET5_E;
 z80[492]:=SET5_H;
 z80[493]:=SET5_L;
 z80[494]:=SET5_xHL;
 z80[495]:=SET5_A;
 z80[496]:=SET6_B;
 z80[497]:=SET6_C;
 z80[498]:=SET6_D;
 z80[499]:=SET6_E;
 z80[500]:=SET6_H;
 z80[501]:=SET6_L;
 z80[502]:=SET6_xHL;
 z80[503]:=SET6_A;
 z80[504]:=SET7_B;
 z80[505]:=SET7_C;
 z80[506]:=SET7_D;
 z80[507]:=SET7_E;
 z80[508]:=SET7_H;
 z80[509]:=SET7_L;
 z80[510]:=SET7_xHL;
 z80[511]:=SET7_A;
end.

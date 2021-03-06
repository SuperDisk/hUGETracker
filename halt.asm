include "hUGEDriver/include/HARDWARE.INC"

;; This is a short program which just contains the noise macro logic found
;; in hUGEDriver. Values are poked by the tracker into RAM, and they are played back
;; by this routine, which is constantly running when the tracker is not playing a song.

add_a_to_r16: MACRO
    add \2
    ld \2, a
    adc \1
    sub \2
    ld \1, a
ENDM

add_a_to_hl: MACRO
    add_a_to_r16 h, l
ENDM

add_a_to_de: MACRO
    add_a_to_r16 d, e
ENDM

SECTION "Vars", WRAM0

instr: ds 8
note: db

macro_index: db

SECTION "LCD controller status interrupt", ROM0[$0048]
    ;; HACK!!!!!!!!!!!!!
    ;; there's some sort of bug in the emulator which needs to be fixed,
    ;; which screws up the program counter immediately after it exits a halt.
    ;; this nop protects against that for now.
    nop
    nop
    nop
    jp _domacro

SECTION "init", ROM0[$0100]
    jp init

SECTION "romname", ROM0[$0134]
; $0134 - $013E: The title, in upper-case letters, followed by zeroes.
DB "HALT"
DS 7 ; padding
; $013F - $0142: The manufacturer code. Empty for now
DS 4
DS 1
; $0144 - $0145: "New" Licensee Code, a two character name.
DB "NF"

SECTION "code", ROM0[$400]

_domacro:
    ld a, [macro_index]
    cp 1
    jp z, _macro_begin
    jp nc, _macro_update
    reti

_macro_begin:
    ld a, [instr]
    ld [rAUD4ENV], a

    ld a, [note]
    call _convert_ch4_note
    ld d, a
    ld a, [instr+1]
    and %10000000
    swap a
    or d
    ld [rAUD4POLY], a

    ld a, [instr+1]
    and %00111111
    ld [rAUD4LEN], a

    ld a, [instr+1]
    and %01000000 ; get only length enabled
    or  %10000000 ; restart sound
    ld [rAUD4GO], a

    ld hl, macro_index
    inc [hl]
    reti

_macro_update:
    ld a, [macro_index]

    ld de, instr
    add_a_to_de
    ld a, [de]
    ld b, a

    ld a, [note]
    add b

    call _convert_ch4_note
    ld d, a
    ld a, [instr+1]
    and %10000000
    swap a
    or d
    ld [rAUD4POLY], a

    ld a, [instr+1]
    and %01000000 ; get only length enabled
    ld [rAUD4GO], a

    ld a, [macro_index]
    inc a
    cp 8
    jp nz, .done
    xor a
.done:
    ld [macro_index], a
    reti

_convert_ch4_note:
    ;; Call with:
    ;; Note number in A
    ;; Stores polynomial counter in A
    ;; Free: HL

    ;; Invert the order of the numbers
    add 192 ; (255 - 63)
    cpl

    ;; Thanks to RichardULZ for this formula
    ;; https://docs.google.com/spreadsheets/d/1O9OTAHgLk1SUt972w88uVHp44w7HKEbS/edit#gid=75028951
    ; if A > 7 then begin
    ;   B := (A-4) div 4;
    ;   C := (A mod 4)+4;
    ;   A := (C or (B shl 4))
    ; end;

    ; if A < 7 then return
    cp 7
    ret c

    ld h, a

    ; B := (A-4) div 4;
    sub 4
    srl a
    srl a
    ld l, a

    ; C := (A mod 4)+4;
    ld a, h
    and 3
    add 4

    ; A := (C or (B shl 4))
    swap l
    or l
    ret

init:
    ; jp _halt
_addr = _AUD3WAVERAM
    REPT 16
    ld a, $FF
    ld [_addr], a
_addr = _addr + 1
    ENDR

    ld a, $80
    ld [rAUDENA], a
    ; Enable all channels in stereo
    ld a, $FF
    ld [rAUDTERM], a
    ; Set volume
    ld a, $77
    ld [rAUDVOL], a
	  ; silence ch3
	  ld a, 0
	  ld [rAUD3LEVEL], a

    ;; Enable the HBlank interrupt on scanline 0
    ld a, [rSTAT]
    or a, STATF_LYC
    ld [rSTAT], a
    xor a ; ld a, 0
    ld [rLYC], a

    ld a, IEF_LCDC
    ld [rIE], a
    ei

_halt:
    halt
    nop
    jr _halt
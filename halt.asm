include "hardware.inc"
include "hUGE.inc"

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

SECTION "Vars", WRAM0

note1: ds 3
note2: ds 3
note3: ds 3
note4: ds 4

instrument1: ds 6
instrument2: ds 6
instrument3: ds 6
instrument4: ds 6

subpattern1: ds (3*64)
subpattern2: ds (3*64)
subpattern3: ds (3*64)
subpattern4: ds (3*64)

start_zero:

start_ch1: db
start_ch2: db
start_ch3: db
start_ch4: db

running_ch1: db
running_ch2: db
running_ch3: db
running_ch4: db

end_zero:

SECTION "code", ROM0[$400]

init_note1:
  ld bc, note1
  call get_current_row
  call get_note_period
  ld a, l
  ld [channel_period1], a
  ld a, h
  ld [channel_period1+1], a

  ld a, [hl+]
  ldh [rAUD1SWEEP], a
  ld a, [hl+]
  ldh [rAUD1LEN], a
  ld a, [hl+]
  ldh [rAUD1ENV], a

  xor a
  ld [table_row1], a
  ld [start_ch1], a
  inc a
  ld [highmask1], a
  ld [running_ch1], a

  jp play_ch1_note

run_table1:
  ld bc, subpattern1
  ld hl, table_row1
  ld e, 0
  jp do_table

init:
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
    xor a
    ld [rLYC], a

    ld a, IEF_LCDC
    ld [rIE], a
    ei

    ;; Initialize hUGEDriver-related vars
    xor a
    ld [row], a

    ;; Zero some ram
    ld c, end_zero - start_zero
    ld hl, start_zero
    xor a
.fill_loop:
    ld [hl+], a
    dec c
    jr nz, .fill_loop

_halt:
    halt
    nop

    ld a, [running_ch1]
    and $FF
    call nz, run_table1

    ld a, [start_ch1]
    and $FF
    call nz, init_note1

    jr _halt
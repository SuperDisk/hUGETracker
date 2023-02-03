include "hardware.inc"
include "hUGE.inc"

SECTION "LCD controller status interrupt", ROM0[$0048]
    ;; HACK!!!!!!!!!!!!!
    ;; there's some sort of bug in the emulator which needs to be fixed,
    ;; which screws up the program counter immediately after it exits a halt.
    ;; this nop protects against that for now.
    nop
    nop
    nop
    reti

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

instrument1: ds 6
instrument2: ds 6
instrument3: ds 6
instrument4: ds 6

subpattern1: ds (3*64)
subpattern2: ds (3*64)
subpattern3: ds (3*64)
subpattern4: ds (3*64)

waveforms: ds (16*16)

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
  ld hl, instrument1
  ld a, [hl+]
  ldh [rAUD1SWEEP], a
  ld a, [hl+]
  ldh [rAUD1LEN], a
  ld a, [hl+]
  ldh [rAUD1ENV], a
  ld a, [hl]
  ld [highmask1], a

  xor a
  ld [table_row1], a
  ld [start_ch1], a

  jp play_ch1_note

init_note2:
  ld hl, instrument2
  inc hl
  ld a, [hl+]
  ldh [rAUD2LEN], a
  ld a, [hl+]
  ldh [rAUD2ENV], a
  ld a, [hl]
  ld [highmask2], a

  xor a
  ld [table_row2], a
  ld [start_ch2], a

  jp play_ch2_note

init_note3:
  ld hl, instrument3
  ld a, [hl+]
  ldh [rAUD3LEN], a
  ld a, [hl+]
  ldh [rAUD3LEVEL], a
  ld a, [hl+]
  ld [current_wave], a
  ld a, [hl]
  ld [highmask3], a

  xor a
  ld [table_row3], a
  ld [start_ch3], a

  jp play_ch3_note

init_note4:
  ld a, [channel_note4]
  call get_note_poly
  ld [channel_period4], a

  ld hl, instrument4
  ld a, [hl+]
  ldh [rAUD4ENV], a
  inc hl
  inc hl
  ld a, [hl]
  and %00111111
  ldh [rAUD4LEN], a

  ld a, [channel_period4]
  ld d, a
  ld a, [hl]
  and %10000000
  swap a
  ld [step_width4], a
  or d
  ld [channel_period4], a

  ld a, [hl]
  and %01000000
  or  %10000000
  ld [highmask4], a

  xor a
  ld [table_row4], a
  ld [start_ch4], a

  jp play_ch4_note

run_table1:
  ld bc, subpattern1
  ld hl, table_row1
  ld e, 0

  ld a, b
  or c
  jp nz, do_table
  ret

run_table2:
  ld bc, subpattern2
  ld hl, table_row2
  ld e, 1

  ld a, b
  or c
  jp nz, do_table
  ret

run_table3:
  ld bc, subpattern3
  ld hl, table_row3
  ld e, 2

  ld a, b
  or c
  jp nz, do_table
  ret

run_table4:
  ld bc, subpattern4
  ld hl, table_row4
  ld e, 3

  ld a, b
  or c
  jp nz, do_table
  ret

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

    ;; Setup waves pointer
    ld hl, waves
    ld a, LOW(waveforms)
    ld [hl+], a
    ld a, HIGH(waveforms)
    ld [hl], a

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

    ld a, [start_ch1]
    and $FF
    call nz, init_note1

    ld a, [start_ch2]
    and $FF
    call nz, init_note2

    ld a, [start_ch3]
    and $FF
    call nz, init_note3

    ld a, [start_ch4]
    and $FF
    call nz, init_note4

    ;;;;;;;;;;;;;;;;;;;;;

    ld a, [running_ch1]
    and $FF
    call nz, run_table1

    ld a, [running_ch2]
    and $FF
    call nz, run_table2

    ld a, [running_ch3]
    and $FF
    call nz, run_table3

    ld a, [running_ch4]
    and $FF
    call nz, run_table4

    jr _halt
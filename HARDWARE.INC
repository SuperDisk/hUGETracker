;*
;* Gameboy Hardware definitions
;*
;* Based on Jones' hardware.inc
;* And based on Carsten Sorensen's ideas.
;*
;* Rev 1.1 - 15-Jul-97 : Added define check
;* Rev 1.2 - 18-Jul-97 : Added revision check macro
;* Rev 1.3 - 19-Jul-97 : Modified for RGBASM V1.05
;* Rev 1.4 - 27-Jul-97 : Modified for new subroutine prefixes
;* Rev 1.5 - 15-Aug-97 : Added _HRAM, PAD, CART defines
;*                     :  and Nintendo Logo
;* Rev 1.6 - 30-Nov-97 : Added rDIV, rTIMA, rTMA, & rTAC
;* Rev 1.7 - 31-Jan-98 : Added _SCRN0, _SCRN1
;* Rev 1.8 - 15-Feb-98 : Added rSB, rSC
;* Rev 1.9 - 16-Feb-98 : Converted I/O registers to $FFXX format
;* Rev 2.0 -           : Added GBC registers
;* Rev 2.1 -           : Added MBC5 & cart RAM enable/disable defines
;* Rev 2.2             : Fixed NR42,NR43, & NR44 equates
;* Rev 2.3 -           : Fixed incorrect _HRAM equate
;* Rev 2.4 -           : Completed CART defines
;* Rev 2.5 -           : Fixed bug in CART defines

; If all of these are already defined, don't do it again.

        IF      !DEF(HARDWARE_INC)
HARDWARE_INC  SET  1

rev_Check_hardware_inc: MACRO
;NOTE: REVISION NUMBER CHANGES MUST BE ADDED
;TO SECOND PARAMETER IN FOLLOWING LINE.
        IF      \1 > 2.2      ;PUT REVISION NUMBER HERE
        WARN    "Version \1 or later of 'hardware.inc' is required."
        ENDC
        ENDM

_HW	EQU $FF00

_VRAM         EQU  $8000 ; $8000->$A000
_SCRN0        EQU  $9800 ; $9800->$9BFF
_SCRN1        EQU  $9C00 ; $9C00->$9FFF
_RAM          EQU  $C000 ; $C000->$E000
_HRAM         EQU  $FF80 ; $FF80->$FFFE
_OAMRAM       EQU  $FE00 ; $FE00->$FE9F
_AUD3WAVERAM  EQU  $FF30 ; $FF30->$FF3F

; *** MBC5 Equates ***

rRAMG         EQU  $0000 ; $0000->$1fff
rROMB0        EQU  $2000 ; $2000->$2fff
rROMB1        EQU  $3000 ; $3000->$3fff
rRAMB         EQU  $4000 ; $4000->$5fff


; --
; -- OAM flags
; --

OAMF_PRI    EQU  %10000000 ; Priority
OAMF_YFLIP  EQU  %01000000 ; Y flip
OAMF_XFLIP  EQU  %00100000 ; X flip
OAMF_PAL0   EQU  %00000000 ; Palette number; 0,1
OAMF_PAL1   EQU  %00010000 ; Palette number; 0,1


;***************************************************************************
;*
;* Custom registers
;*
;***************************************************************************

; --
; -- P1 ($FF00)
; -- Register for reading joy pad info.    (R/W)
; --
rP1 EQU $FF00

P1F_5   EQU  %00100000 ; P15 out port
P1F_4   EQU  %00010000 ; P14 out port
P1F_3   EQU  %00001000 ; P13 in port
P1F_2   EQU  %00000100 ; P12 in port
P1F_1   EQU  %00000010 ; P11 in port
P1F_0   EQU  %00000001 ; P10 in port

; --
; -- SB ($FF01)
; -- Serial Transfer Data (R/W)
; --
rSB EQU $FF01

; --
; -- SC ($FF02)
; -- Serial I/O Control (R/W)
; --
rSC EQU $FF02

; --
; -- DIV ($FF04)
; -- Divider register (R/W)
; --
rDIV EQU $FF04


; --
; -- TIMA ($FF05)
; -- Timer counter (R/W)
; --
rTIMA EQU $FF05


; --
; -- TMA ($FF06)
; -- Timer modulo (R/W)
; --
rTMA EQU $FF06


; --
; -- TAC ($FF07)
; -- Timer control (R/W)
; --
rTAC EQU $FF07

TACF_START   EQU %00000100
TACF_STOP    EQU %00000000
TACF_4KHZ    EQU %00000000
TACF_16KHZ   EQU %00000011
TACF_65KHZ   EQU %00000010
TACF_262KHZ  EQU %00000001

; --
; -- IF ($FF0F)
; -- Interrupt Flag (R/W)
; --
rIF EQU $FF0F

; --
; -- LCDC ($FF40)
; -- LCD Control (R/W)
; --
rLCDC EQU $FF40

LCDCF_OFF     EQU  %00000000 ; LCD Control Operation
LCDCF_ON      EQU  %10000000 ; LCD Control Operation
LCDCF_WIN9800 EQU  %00000000 ; Window Tile Map Display Select
LCDCF_WIN9C00 EQU  %01000000 ; Window Tile Map Display Select
LCDCF_WINOFF  EQU  %00000000 ; Window Display
LCDCF_WINON   EQU  %00100000 ; Window Display
LCDCF_BG8800  EQU  %00000000 ; BG & Window Tile Data Select
LCDCF_BG8000  EQU  %00010000 ; BG & Window Tile Data Select
LCDCF_BG9800  EQU  %00000000 ; BG Tile Map Display Select
LCDCF_BG9C00  EQU  %00001000 ; BG Tile Map Display Select
LCDCF_OBJ8    EQU  %00000000 ; OBJ Construction
LCDCF_OBJ16   EQU  %00000100 ; OBJ Construction
LCDCF_OBJOFF  EQU  %00000000 ; OBJ Display
LCDCF_OBJON   EQU  %00000010 ; OBJ Display
LCDCF_BGOFF   EQU  %00000000 ; BG Display
LCDCF_BGON    EQU  %00000001 ; BG Display
; "Window Character Data Select" follows BG


; --
; -- STAT ($FF41)
; -- LCDC Status   (R/W)
; --
rSTAT EQU $FF41

STATF_LYC     EQU  %01000000 ; LYCEQULY Coincidence (Selectable)
STATF_MODE10  EQU  %00100000 ; Mode 10
STATF_MODE01  EQU  %00010000 ; Mode 01 (V-Blank)
STATF_MODE00  EQU  %00001000 ; Mode 00 (H-Blank)
STATF_LYCF    EQU  %00000100 ; Coincidence Flag
STATF_HB      EQU  %00000000 ; H-Blank
STATF_VB      EQU  %00000001 ; V-Blank
STATF_OAM     EQU  %00000010 ; OAM-RAM is used by system
STATF_LCD     EQU  %00000011 ; Both OAM and VRAM used by system
STATF_BUSY    EQU  %00000010 ; When set, VRAM access is unsafe


; --
; -- SCY ($FF42)
; -- Scroll Y (R/W)
; --
rSCY  EQU $FF42


; --
; -- SCY ($FF43)
; -- Scroll X (R/W)
; --
rSCX  EQU $FF43


; --
; -- LY ($FF44)
; -- LCDC Y-Coordinate (R)
; --
; -- Values range from 0->153. 144->153 is the VBlank period.
; --
rLY EQU $FF44


; --
; -- LYC ($FF45)
; -- LY Compare (R/W)
; --
; -- When LYEQUEQULYC, STATF_LYCF will be set in STAT
; --
rLYC  EQU $FF45


; --
; -- DMA ($FF46)
; -- DMA Transfer and Start Address (W)
; --
rDMA  EQU $FF46


; --
; -- BGP ($FF47)
; -- BG Palette Data (W)
; --
; -- Bit 7-6 - Intensity for %11
; -- Bit 5-4 - Intensity for %10
; -- Bit 3-2 - Intensity for %01
; -- Bit 1-0 - Intensity for %00
; --
rBGP  EQU $FF47


; --
; -- OBP0 ($FF48)
; -- Object Palette 0 Data (W)
; --
; -- See BGP for info
; --
rOBP0 EQU $FF48


; --
; -- OBP1 ($FF49)
; -- Object Palette 1 Data (W)
; --
; -- See BGP for info
; --
rOBP1 EQU $FF49


; --
; -- WY ($FF4A)
; -- Window Y Position (R/W)
; --
; -- 0 <EQU WY <EQU 143
; --
rWY EQU $FF4A


; --
; -- WX ($FF4B)
; -- Window X Position (R/W)
; --
; -- 7 <EQU WX <EQU 166
; --
rWX EQU $FF4B


; --
; -- KEY 1 ($FF4D)
; -- Select CPU Speed (R/W)
; --
rKEY1 EQU $FF4D


; --
; -- VBK ($FF4F)
; -- Select Video RAM Bank (R/W)
; --
rVBK EQU $FF4F


; --
; -- HDMA1 ($FF51)
; -- Horizontal Blanking, General Purpose DMA (W)
; --
rHDMA1 EQU $FF51


; --
; -- HDMA2 ($FF52)
; -- Horizontal Blanking, General Purpose DMA (W)
; --
rHDMA2 EQU $FF52


; --
; -- HDMA3 ($FF53)
; -- Horizontal Blanking, General Purpose DMA (W)
; --
rHDMA3 EQU $FF53


; --
; -- HDMA4 ($FF54)
; -- Horizontal Blanking, General Purpose DMA (W)
; --
rHDMA4 EQU $FF54


; --
; -- HDMA5 ($FF55)
; -- Horizontal Blanking, General Purpose DMA (R/W)
; --
rHDMA5 EQU $FF55


; --
; -- RP ($FF56)
; -- Infrared Communications Port (R/W)
; --
rRP EQU $FF56


; --
; -- BCPS ($FF68)
; -- Background Color Palette Specification (R/W)
; --
rBCPS EQU $FF68


; --
; -- BCPD ($FF69)
; -- Background Color Palette Data (R/W)
; --
rBCPD EQU $FF69


; --
; -- BCPS ($FF6A)
; -- Object Color Palette Specification (R/W)
; --
rOCPS EQU $FF6A


; --
; -- BCPD ($FF6B)
; -- Object Color Palette Data (R/W)
; --
rOCPD EQU $FF6B


; --
; -- SVBK ($FF4F)
; -- Select Main RAM Bank (R/W)
; --
rSVBK EQU $FF70


; --
; -- IE ($FFFF)
; -- Interrupt Enable (R/W)
; --
rIE EQU $FFFF


IEF_HILO    EQU  %00010000 ; Transition from High to Low of Pin number P10-P13
IEF_SERIAL  EQU  %00001000 ; Serial I/O transfer end
IEF_TIMER   EQU  %00000100 ; Timer Overflow
IEF_LCDC    EQU  %00000010 ; LCDC (see STAT)
IEF_VBLANK  EQU  %00000001 ; V-Blank




;***************************************************************************
;*
;* Sound control registers
;*
;***************************************************************************

; --
; -- AUDVOL/NR50 ($FF24)
; -- Channel control / ON-OFF / Volume (R/W)
; --
; -- Bit 7   - Vin->SO2 ON/OFF (Vin??)
; -- Bit 6-4 - SO2 output level (volume) (# 0-7)
; -- Bit 3   - Vin->SO1 ON/OFF (Vin??)
; -- Bit 2-0 - SO1 output level (volume) (# 0-7)
; --
rNR50 EQU $FF24
rAUDVOL	EQU rNR50


; --
; -- AUDTERM/NR51 ($FF25)
; -- Selection of Sound output terminal (R/W)
; --
; -- Bit 7   - Output sound 4 to SO2 terminal
; -- Bit 6   - Output sound 3 to SO2 terminal
; -- Bit 5   - Output sound 2 to SO2 terminal
; -- Bit 4   - Output sound 1 to SO2 terminal
; -- Bit 3   - Output sound 4 to SO1 terminal
; -- Bit 2   - Output sound 3 to SO1 terminal
; -- Bit 1   - Output sound 2 to SO1 terminal
; -- Bit 0   - Output sound 0 to SO1 terminal
; --
rNR51 EQU $FF25
rAUDTERM	EQU rNR51


; --
; -- AUDENA/NR52 ($FF26)
; -- Sound on/off (R/W)
; --
; -- Bit 7   - All sound on/off (sets all audio regs to 0!)
; -- Bit 3   - Sound 4 ON flag (doesn't work!)
; -- Bit 2   - Sound 3 ON flag (doesn't work!)
; -- Bit 1   - Sound 2 ON flag (doesn't work!)
; -- Bit 0   - Sound 1 ON flag (doesn't work!)
; --
rNR52 EQU $FF26
rAUDENA	EQU rNR52


;***************************************************************************
;*
;* SoundChannel #1 registers
;*
;***************************************************************************

; --
; -- AUD1SWEEP/NR10 ($FF10)
; -- Sweep register (R/W)
; --
; -- Bit 6-4 - Sweep Time
; -- Bit 3   - Sweep Increase/Decrease
; --           0: Addition    (frequency increases???)
; --           1: Subtraction (frequency increases???)
; -- Bit 2-0 - Number of sweep shift (# 0-7)
; -- Sweep Time: (n*7.8ms)
; --
rNR10 EQU $FF10
rAUD1SWEEP	EQU rNR10


; --
; -- AUD1LEN/NR11 ($FF11)
; -- Sound length/Wave pattern duty (R/W)
; --
; -- Bit 7-6 - Wave Pattern Duty (00:12.5% 01:25% 10:50% 11:75%)
; -- Bit 5-0 - Sound length data (# 0-63)
; --
rNR11 EQU $FF11
rAUD1LEN	EQU rNR11


; --
; -- AUD1ENV/NR12 ($FF12)
; -- Envelope (R/W)
; --
; -- Bit 7-4 - Initial value of envelope
; -- Bit 3   - Envelope UP/DOWN
; --           0: Decrease
; --           1: Range of increase
; -- Bit 2-0 - Number of envelope sweep (# 0-7)
; --
rNR12 EQU $FF12
rAUD1ENV	EQU rNR12


; --
; -- AUD1LOW/NR13 ($FF13)
; -- Frequency lo (W)
; --
rNR13 EQU $FF13
rAUD1LOW	EQU rNR13


; --
; -- AUD1HIGH/NR14 ($FF14)
; -- Frequency hi (W)
; --
; -- Bit 7   - Initial (when set, sound restarts)
; -- Bit 6   - Counter/consecutive selection
; -- Bit 2-0 - Frequency's higher 3 bits
; --
rNR14 EQU $FF14
rAUD1HIGH	EQU rNR14


;***************************************************************************
;*
;* SoundChannel #2 registers
;*
;***************************************************************************

; --
; -- AUD2LEN/NR21 ($FF16)
; -- Sound Length; Wave Pattern Duty (R/W)
; --
; -- see AUD1LEN for info
; --
rNR21 EQU $FF16
rAUD2LEN	EQU rNR21


; --
; -- AUD2ENV/NR22 ($FF17)
; -- Envelope (R/W)
; --
; -- see AUD1ENV for info
; --
rNR22 EQU $FF17
rAUD2ENV	EQU rNR22


; --
; -- AUD2LOW/NR23 ($FF18)
; -- Frequency lo (W)
; --
rNR23 EQU $FF18
rAUD2LOW	EQU rNR23


; --
; -- AUD2HIGH/NR24 ($FF19)
; -- Frequency hi (W)
; --
; -- see AUD1HIGH for info
; --
rNR24 EQU $FF19
rAUD2HIGH	EQU rNR24


;***************************************************************************
;*
;* SoundChannel #3 registers
;*
;***************************************************************************

; --
; -- AUD3ENA/NR30 ($FF1A)
; -- Sound on/off (R/W)
; --
; -- Bit 7   - Sound ON/OFF (1EQUON,0EQUOFF)
; --
rNR30 EQU $FF1A
rAUD3ENA	EQU rNR30


; --
; -- AUD3LEN/NR31 ($FF1B)
; -- Sound length (R/W)
; --
; -- Bit 7-0 - Sound length
; --
rNR31 EQU $FF1B
rAUD3LEN	EQU rNR31


; --
; -- AUD3LEVEL/NR32 ($FF1C)
; -- Select output level
; --
; -- Bit 6-5 - Select output level
; --           00: 0/1 (mute)
; --           01: 1/1
; --           10: 1/2
; --           11: 1/4
; --
rNR32 EQU $FF1C
rAUD3LEVEL	EQU rNR32


; --
; -- AUD3LOW/NR33 ($FF1D)
; -- Frequency lo (W)
; --
; -- see AUD1LOW for info
; --
rNR33 EQU $FF1D
rAUD3LOW	EQU rNR33


; --
; -- AUD3HIGH/NR34 ($FF1E)
; -- Frequency hi (W)
; --
; -- see AUD1HIGH for info
; --
rNR34 EQU $FF1E
rAUD3HIGH	EQU rNR34


; --
; -- AUD4LEN/NR41 ($FF20)
; -- Sound length (R/W)
; --
; -- Bit 5-0 - Sound length data (# 0-63)
; --
rNR41 EQU $FF20
rAUD4LEN	EQU rNR41


; --
; -- AUD4ENV/NR42 ($FF21)
; -- Envelope (R/W)
; --
; -- see AUD1ENV for info
; --
rNR42 EQU $FF21
rAUD4ENV	EQU rNR42


; --
; -- AUD4POLY/NR43 ($FF22)
; -- Polynomial counter (R/W)
; --
; -- Bit 7-4 - Selection of the shift clock frequency of the (scf)
; --           polynomial counter (0000-1101)
; --           freqEQUdrf*1/2^scf (not sure)
; -- Bit 3 -   Selection of the polynomial counter's step
; --           0: 15 steps
; --           1: 7 steps
; -- Bit 2-0 - Selection of the dividing ratio of frequencies (drf)
; --           000: f/4   001: f/8   010: f/16  011: f/24
; --           100: f/32  101: f/40  110: f/48  111: f/56  (fEQU4.194304 Mhz)
; --
rNR43 EQU $FF22
rAUD4POLY EQU rNR43


; --
; -- AUD4GO/NR44 ($FF23)
; -- (has wrong name and value (ff30) in Dr.Pan's doc!)
; --
; -- Bit 7 -   Inital
; -- Bit 6 -   Counter/consecutive selection
; --
rNR44 EQU $FF23
rAUD4GO EQU rNR44 ; silly name!

;***************************************************************************
;*
;* Cart related
;*
;***************************************************************************

CART_ROM                  EQU 0   ; ROM
CART_ROM_MBC1             EQU 1   ; ROM+MBC1
CART_ROM_MBC1_RAM         EQU 2   ; ROM+MBC1+RAM
CART_ROM_MBC1_RAM_BAT     EQU 3   ; ROM+MBC1+RAM+BATTERY
CART_ROM_MBC2             EQU 5   ; ROM+MBC2
CART_ROM_MBC2_BAT         EQU 6   ; ROM+MBC2+BATTERY
CART_ROM_RAM              EQU 8   ; ROM+RAM
CART_ROM_RAM_BAT          EQU 9   ; ROM+RAM+BATTERY
CART_ROM_MMM              EQU $b  ; ROM+MMM01
CART_ROM_RAM_MMM          EQU $c  ; ROM+RAM+MMM01
CART_ROM_RAM_MMM_BAT      EQU $d  ; ROM+RAM+MMM01+BATTERY
CART_ROM_MBC3_TIM_BAT     EQU $f  ; ROM+MBC3+TIMER+BATTERY
CART_ROM_MBC3_TIM_RAM_BAT EQU $10 ; ROM+MBC3+TIMER+RAM+BATTERY
CART_ROM_MBC3             EQU $11 ; ROM+MBC3
CART_ROM_MBC3_RAM         EQU $12 ; ROM+MBC3+RAM
CART_ROM_MBC3_RAM_BAT     EQU $13 ; ROM+MBC3+RAM+BATTERY
CART_ROM_MBC4             EQU $15 ; ROM+MBC4
CART_ROM_MBC4_RAM         EQU $16 ; ROM+MBC4+RAM
CART_ROM_MBC4_RAM_BAT     EQU $17 ; ROM+MBC4+RAM+BATTERY
CART_ROM_MBC5             EQU $19 ; ROM+MBC5
CART_ROM_MBC5_RAM         EQU $1a ; ROM+MBC5+RAM
CART_ROM_MBC5_RAM_BAT     EQU $1b ; ROM+MBC5+RAM+BATTERY
CART_ROM_MBC5_RUM         EQU $1c ; ROM+MBC5+RUMBLE
CART_ROM_MBC5_RUM_RAM     EQU $1d ; ROM+MBC5+RUMBLE+RAM
CART_ROM_MBC5_RUM_RAM_BAT EQU $1e ; ROM+MBC5+RUMBLE+RAM+BATTERY
CART_CAMERA               EQU $fc ; Camera ROM
CART_TAMA5                EQU $fd ; Bandai Tama 5
CART_HuC3                 EQU $fe ; HuC3
CART_HuC1_RAM_BAT         EQU $ff ; HuC1+RAM+BATTERY

CART_ROM_256K  EQU 0
CART_ROM_512K  EQU 1
CART_ROM_1M    EQU 2
CART_ROM_2M    EQU 3
CART_ROM_4M    EQU 4
CART_ROM_8M    EQU 5
CART_ROM_16M   EQU 6

CART_RAM_NONE  EQU 0
CART_RAM_16K   EQU 1
CART_RAM_64K   EQU 2
CART_RAM_256K  EQU 3

CART_RAM_ENABLE   EQU $0a
CART_RAM_DISABLE  EQU $00

;***************************************************************************
;*
;* Keypad related
;*
;***************************************************************************

PADF_DOWN       EQU     $80
PADF_UP         EQU     $40
PADF_LEFT       EQU     $20
PADF_RIGHT      EQU     $10
PADF_START      EQU     $08
PADF_SELECT     EQU     $04
PADF_B          EQU     $02
PADF_A          EQU     $01

PADB_DOWN       EQU     $7
PADB_UP         EQU     $6
PADB_LEFT       EQU     $5
PADB_RIGHT      EQU     $4
PADB_START      EQU     $3
PADB_SELECT     EQU     $2
PADB_B          EQU     $1
PADB_A          EQU     $0

;***************************************************************************
;*
;* Screen related
;*
;***************************************************************************

SCRN_X     EQU  160 ; Width of screen in pixels
SCRN_Y     EQU  144 ; Height of screen in pixels
SCRN_X_B   EQU  20  ; Width of screen in bytes
SCRN_Y_B   EQU  18  ; Height of screen in bytes

SCRN_VX    EQU  256 ; Virtual width of screen in pixels
SCRN_VY    EQU  256 ; Virtual height of screen in pixels
SCRN_VX_B  EQU  32  ; Virtual width of screen in bytes
SCRN_VY_B  EQU  32  ; Virtual height of screen in bytes

NINTENDO_LOGO:   MACRO
;*
;* Nintendo scrolling logo
;* (Code won't work on a real GameBoy)
;* (if next six lines are altered.)
 DB $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
 DB $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
 DB $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E
        ENDM

        ENDC    ;HARDWARE_INC
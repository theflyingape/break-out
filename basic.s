;*********************************************************************
; COMMODORE VIC 20 BOOT USING BASIC 2.0
; written by Robert Hurst <robert@hurst-ri.us>
; updated version: 30-Oct-2011
;
		.fileopt author,	"Robert Hurst"
        .fileopt comment,	"Break-Out!"
        .fileopt compiler,	"VIC 20 ASSEMBLER"

		.include "VIC-SSS-MMX.h"

;*********************************************************************
; Commodore BASIC 2.0 program
;
; LOAD "BREAK-OUT!",8
; RUN
;
		.segment "BASIC"

		.word	RUN		; load address
RUN:	.word	@end	; next line link
		.word	2011	; line number
		.byte	$9E		; BASIC token: SYS
		.byte	<(MAIN / 1000 .mod 10) + $30
		.byte	<(MAIN / 100 .mod 10) + $30
		.byte	<(MAIN / 10 .mod 10) + $30
		.byte	<(MAIN / 1 .mod 10) + $30
		.byte	0		; end of line
@end:	.word	0		; end of program

;*********************************************************************
; Starting entry point for this program
;
		.segment "STARTUP"

MAIN:
		LDX $FFFC
		LDY $FFFD
		STX $0318
		STY $0319		; enable RESTORE key as RESET
		LDA MACHINE
		CMP #$05
		BNE PAL
		;
		; NTSC setup
NTSC:	LDX #<@NTSC		; load the timer low-byte latches
		LDY #>@NTSC
		LDA #$75		; raster line 234/235
		BNE IRQSYNC
@NTSC = $4243			; (261 * 65 - 2)
		;
		; PAL setup
PAL:	LDX #<@PAL		; load the timer low-byte latches
		LDY #>@PAL
		LDA #$82		; raster line 260/261
@PAL = $5686			; (312 * 71 - 2)
		;
IRQSYNC:
		CMP VIC+$04
		BNE IRQSYNC
		STX $9126		; load T1 latch low
		STY $9125		; load T1 latch high, and transfer both to T1 counter
		; init VIC
		LDA #$00+$16	; set for videoram @ $1400 with 22-columns
		STA VIC+$02		; video matrix address + columns
		LDA #%10101110	; 8x8 height + 23-rows
		STA VIC+$03		; rows / character height
		LDA #$DF		; set video @ $1400 and char table @ $1C00
		STA VIC+$05
		LDA #$8F		; orange aux, highest volume
		STA VIC+$0E
		LDA #187		; Programmer's Reference Guide: Appendix B
		STA VIC+$0F		; lt cyan screen / cyan border
		; reset sound channels
@cont:	LDA #$00
		TAY
@snd:	STA VIC+$0A,Y
		INY
		CPY #$04
		BNE @snd
		; setup my background processing
		.global MYIRQ
		SEI
		LDX #<MYIRQ
		LDY #>MYIRQ
		STX $0314
		STY $0315
		CLI

;*********************************************************************
; Now that all the VIC startup initialization stuff is completed,
; you can append one-time startup code/data here, i.e., like a splash
; title screen.  Then, you must jump to your CODE segment, linked
; outside of VIC's internal RAM address space ...
;
RUNONCE:
		LDX #<SPLASHCOLOR
		LDY #>SPLASHCOLOR
		STX $FB
		STY $FC
		LDX #$00
		LDY #$94
		STX $FD
		STY $FE
		LDX #$02
		LDY #$00
@fill:	LDA ($FB),Y
		STA ($FD),Y
		INY
		BNE @fill
		INC $FC
		INC $FE
		DEX
		BNE @fill
@loop:
		LDA $028D
		AND #$02		; got C= key?
		BNE @go
		LDY #$00
		STY $9113
		LDA #$7F
		STA $9122
		LDA $9120
		AND #$80		; got paddle #2 FIRE ?
		BEQ @go
		LDA #$FF
		STA $9122
		LDY $9111
		TYA
		AND #$20		; got joystick FIRE ?
		BEQ @go
		TYA
		AND #$10		; got paddle #1 FIRE ?
		BNE @loop
@go:
		LDA #$EB		; cyan border, light blue background
		STA VIC+$0F
		.global RESTART	; useful symbol for MAP and hotkey restarting
		JMP RESTART		; the entry point into your program

;*********************************************************************
; Display startup splash screen
; redirect VIC to look here and paint the screen with ensuing data
; 
		.segment "SPLASH"

SPLASHDATA:
		.byte	$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$94,$88,$89,$93,$A0,$87,$81,$8D,$85,$A0,$92,$85,$91,$95,$89,$92,$85,$93,$A0,$A0
		.byte	$A0,$A0,$A0,$81,$A0,$90,$81,$84,$84,$8C,$85,$A0,$94,$8F,$A0,$90,$8C,$81,$99,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$00,$A0,$A0,$00,$A0,$00,$A0,$A0,$00,$00,$A0,$A0,$A0,$A0,$00,$A0,$A0,$A0,$00,$A0,$A0
		.byte	$A0,$00,$A0,$A0,$00,$A0,$00,$A0,$00,$A0,$A0,$00,$00,$A0,$00,$A0,$00,$A0,$00,$A0,$00,$A0
		.byte	$A0,$00,$A0,$A0,$00,$A0,$00,$A0,$00,$A0,$A0,$00,$A0,$A0,$A0,$A0,$00,$A0,$00,$A0,$00,$A0
		.byte	$A0,$00,$A0,$A0,$00,$A0,$00,$A0,$00,$A0,$A0,$A0,$A0,$A0,$A0,$00,$00,$A0,$00,$A0,$00,$A0
		.byte	$A0,$00,$A0,$A0,$00,$A0,$00,$A0,$00,$A0,$A0,$00,$A0,$A0,$00,$00,$A0,$A0,$00,$A0,$00,$A0
		.byte	$A0,$A0,$00,$00,$A0,$A0,$00,$A0,$00,$A0,$A0,$00,$00,$A0,$00,$A0,$A0,$A0,$00,$A0,$00,$A0
		.byte	$A0,$A0,$00,$00,$A0,$A0,$00,$A0,$A0,$00,$00,$A0,$A0,$A0,$00,$00,$00,$A0,$A0,$00,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$82,$92,$85,$81,$8B,$AD,$8F,$95,$94,$A1,$A0,$A0,$92,$B1,$B0,$AE,$B3,$B0,$A0,$A0
		.byte	$A0,$A0,$7F,$B2,$B0,$B1,$B1,$A0,$92,$8F,$82,$85,$92,$94,$A0,$88,$95,$92,$93,$94,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$8D,$81,$84,$85,$A0,$89,$8E,$A0,$95,$93,$81,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$79,$7C,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$90,$92,$85,$93,$93,$A0,$7A,$7D,$A0,$94,$8F,$A0,$83,$8F,$8E,$94,$89,$8E,$95,$85,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$7B,$7E,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA,$A0,$A0,$DA
		.res	6
SPLASHCOLOR:
		.byte	$00,$00,$00,$01,$00,$00,$02,$00,$00,$03,$00,$00,$04,$00,$00,$05,$00,$00,$06,$00,$00,$07
		.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$00,$00
		.byte	$00,$00,$00,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$06,$00,$00,$06,$00,$04,$00,$00,$08,$07,$00,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00
		.byte	$00,$06,$00,$00,$04,$00,$08,$00,$08,$00,$00,$06,$06,$00,$01,$00,$01,$00,$01,$00,$01,$00
		.byte	$00,$06,$00,$00,$04,$00,$08,$00,$07,$00,$00,$06,$00,$00,$00,$00,$01,$00,$01,$00,$01,$00
		.byte	$00,$06,$00,$00,$04,$00,$08,$00,$07,$00,$00,$00,$00,$00,$00,$01,$01,$00,$01,$00,$01,$00
		.byte	$00,$04,$00,$00,$08,$00,$07,$00,$07,$00,$00,$02,$00,$00,$01,$01,$00,$00,$01,$00,$01,$00
		.byte	$00,$00,$04,$08,$00,$00,$07,$00,$05,$00,$00,$02,$02,$00,$01,$00,$00,$00,$01,$00,$01,$00
		.byte	$00,$00,$08,$08,$00,$00,$07,$00,$00,$05,$05,$00,$00,$00,$01,$01,$01,$00,$00,$01,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$00,$00,$03,$03,$03,$03,$03,$03,$00,$00
		.byte	$00,$00,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$00,$00
		.byte	$00,$00,$00,$00,$00,$03,$03,$03,$03,$03,$03,$03,$00,$02,$01,$06,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$0E,$0E,$0E,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$0E,$0E,$0E,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$0E,$0E,$0E,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$07,$00,$00,$06,$00,$00,$05,$00,$00,$04,$00,$00,$03,$00,$00,$02,$00,$00,$01,$00,$00,$00
		.res	6

;*********************************************************************
; VIC Software Sprite Stack 2010 (VIC-SSS-MMX)
;
; The above BASIC loader will be overwritten by SSS upon its
; initialization (SSSINIT).  The linker will fill this reserved space
; with values used for the dual video frame buffers, play field, and
; the sprite image buffers and registers: 4096 - 6207 ($1000 - $1BFF)
;
; $1000 - $11FF		VICFRAME1 - first video buffer
; $1200 - $13FF		VICFRAME2 - second video buffer
; $1400 - $15FF		PLAYFIELD - write-pending screen buffer
; $1600 - $17FF		PLAYCOLOR - write-pending color / dirty buffer
; $1800 - $1BFF		Sprite image buffers & registers
;
			.segment "SSSBUF"

SSSBUF:		.res 7 * 8		; this can be resized as required --
							; if all 64-chars are used by sprites, that
							; exhausts all 128 custom characters for
							; double-buffering (x2)
;
; SPRITE REGISTERS (17)
;
SPRITEBACK:	.res SPRITEMAX	; 1st char this sprite is in collision with
SPRITEBUFH:	.res SPRITEMAX	; pointer within sprite image buffer
SPRITEBUFL:	.res SPRITEMAX	; pointer within sprite image buffer
SPRITEC1H:	.res SPRITEMAX	; pointer within sprite display character pool
SPRITEC1L:	.res SPRITEMAX	; pointer within sprite display character pool
SPRITEC2H:	.res SPRITEMAX	; pointer within sprite display character pool
SPRITEC2L:	.res SPRITEMAX	; pointer within sprite display character pool
SPRITECOL:	.res SPRITEMAX	; 4-bit VIC color code
SPRITECX:	.res SPRITEMAX	; sprite collision X-coord
SPRITECY:	.res SPRITEMAX	; sprite collision Y-coord
SPRITEDEF:	.res SPRITEMAX	; function/matrix definition (see explanation below)
SPRITEH:	.res SPRITEMAX	; number of raster lines (1-16)
SPRITEIMGH:	.res SPRITEMAX	; pointer to source graphic for rendering at 0,0
SPRITEIMGL:	.res SPRITEMAX	; pointer to source graphic for rendering at 0,0
SPRITEX:	.res SPRITEMAX	; horizontal pixel coordinate, visible >0 - <SSSCLIPX
SPRITEY:	.res SPRITEMAX	; vertical pixel coordinate, visible >0 - <SSSCLIPY
SPRITEZ:	.res SPRITEMAX	; bit 0: last rendered (0 = SPRITEC1; 1 = SPRITEC2)
							; bit 1: fast copy (0 = merge; 1 = copy)
							; bit 3: sprite collision
							; bit 4: sprite image is clipped by a static cell
							; bit 5: background is all SSSNULLs
							; bit 6: copy/merge into alternate sprite character pool
							; bit 7: copy/shift sprite image into its buffer
;
; SPRITEDEF is a bit-structure of these characteristics:
; - height		bit 0: 0 = 8px; 1 = 16px
; - width		bit 1: 0 = 8px; 1 = 16px
; - float Y		bit 2: flag: 0=fixed cell, 1=vertical float
; - float X		bit 3: flag: 0=fixed cell, 1=horizontal float
; - repeat		bit 4: flag: 0=independent, 1=same as previous
; - ghost		bit 5: flag: 0=merge image, 1=invert image
; - collision	bit 6: flag: 0=fast copy, 1=detect
; - enabled		bit 7: flag: 0=invisible, 1=visible
;
						; SSS runtime variables:
sss:		.res 24*2	; screen row index, computed from PLAYCOLS in SSSINIT
;
; other initialized data can be appended here:
;
sssALLOC:	; 8x8, 16x8, 8x16, 16x16
			.byte	8,16,16,32	; fixed:	1,2,2,4
			.byte	16,24,32,48	; float Y:	2,3,4,6
			.byte	16,32,24,48	; float X:	2,4,3,6
			.byte	32,48,48,72	; both:		4,6,6,9
sssROWS:	.byte	1,2,1,2	; fixed
			.byte	2,3,2,3	; float Y
			.byte	1,2,1,2	; float X
			.byte	2,3,2,3	; both
sssCOLS:	.byte	1,1,2,2	; fixed
			.byte	1,1,2,2	; float Y
			.byte	2,2,3,3	; float X
			.byte	2,2,3,3	; both

;*********************************************************************
; VIC Custom Graphic Characters
;
; If < 64 will be used for the software sprite stack, the remaining
; unused characters can be used for other custom graphics, beginning
; at $1C00 where "@", "A", "B", ... characters can be redefined.
;
; Do not use this as an initialized segment if you plan on linking
; this source as a future game cartridge later.  You must COPY any
; read-only data into this address space.
;
; If your data was saved from some tool in binary format, you can
; include that binary file here as:
;		.incbin "graphics.dat"
;
; else, just enter each 8x8 values here, such as:
;		.byte	$FF,$81,$81,$81,$81,$81,$81,$FF
; or:
;		.byte	%11111111	; square
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%11111111
;
		.segment "MYCHAR"
@brick:
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		;
		; digits
@d0l:
		.byte	%00001111
		.byte	%00011111
		.byte	%00110000
		.byte	%00110000
		.byte	%00110000
		.byte	%00110000
		.byte	%00011111
		.byte	%00001111
@d1l:
		.byte	%00000001
		.byte	%00000011
		.byte	%00000111
		.byte	%00000001
		.byte	%00000001
		.byte	%00000001
		.byte	%00001111
		.byte	%00011111
@d2l:
		.byte	%00011111
		.byte	%00111111
		.byte	%00000000
		.byte	%00011111
		.byte	%00111111
		.byte	%00110000
		.byte	%00111111
		.byte	%00111111
@d3l:
		.byte	%00011111
		.byte	%00111111
		.byte	%00000000
		.byte	%00000111
		.byte	%00000011
		.byte	%00000000
		.byte	%00111111
		.byte	%00011111
@d4l:
		.byte	%00001100
		.byte	%00011000
		.byte	%00110000
		.byte	%00111111
		.byte	%00111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
@d5l:
		.byte	%00111111
		.byte	%00111111
		.byte	%00110000
		.byte	%00111111
		.byte	%00111111
		.byte	%00000000
		.byte	%00111111
		.byte	%00011111
@d6l:
		.byte	%00011111
		.byte	%00111111
		.byte	%00110000
		.byte	%00111111
		.byte	%00111111
		.byte	%00110000
		.byte	%00111111
		.byte	%00011111
@d7l:
		.byte	%00011111
		.byte	%00111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000001
		.byte	%00000001
@d8l:
		.byte	%00011111
		.byte	%00111111
		.byte	%00110000
		.byte	%00011111
		.byte	%00111111
		.byte	%00110000
		.byte	%00111111
		.byte	%00011111
@d9l:
		.byte	%00011111
		.byte	%00111111
		.byte	%00110000
		.byte	%00111111
		.byte	%00011111
		.byte	%00000000
		.byte	%00111111
		.byte	%00011111
@d0r:
		.byte	%11110000
		.byte	%11111000
		.byte	%00001100
		.byte	%00001100
		.byte	%00001100
		.byte	%00001100
		.byte	%11111000
		.byte	%11110000
@d1r:
		.byte	%10000000
		.byte	%10000000
		.byte	%10000000
		.byte	%10000000
		.byte	%10000000
		.byte	%10000000
		.byte	%11110000
		.byte	%11111000
@d2r:
		.byte	%11111000
		.byte	%11111100
		.byte	%00001100
		.byte	%11111100
		.byte	%11111000
		.byte	%00000000
		.byte	%11111000
		.byte	%11111100
@d3r:
		.byte	%11111000
		.byte	%11111100
		.byte	%00001100
		.byte	%11111000
		.byte	%11111100
		.byte	%00001100
		.byte	%11111100
		.byte	%11111000
@d4r:
		.byte	%01100000
		.byte	%01100000
		.byte	%01100000
		.byte	%11111100
		.byte	%11111000
		.byte	%01100000
		.byte	%01100000
		.byte	%01100000
@d5r:
		.byte	%11111100
		.byte	%11111000
		.byte	%00000000
		.byte	%11111000
		.byte	%11111100
		.byte	%00001100
		.byte	%11111100
		.byte	%11111000
@d6r:
		.byte	%11111000
		.byte	%11111100
		.byte	%00000000
		.byte	%11111000
		.byte	%11111100
		.byte	%00001100
		.byte	%11111100
		.byte	%11111000
@d7r:
		.byte	%11111100
		.byte	%11111100
		.byte	%00011000
		.byte	%00110000
		.byte	%01100000
		.byte	%11000000
		.byte	%10000000
		.byte	%10000000
@d8r:
		.byte	%11111000
		.byte	%11111100
		.byte	%00001100
		.byte	%11111000
		.byte	%11111100
		.byte	%00001100
		.byte	%11111100
		.byte	%11111000
@d9r:
		.byte	%11110000
		.byte	%11111000
		.byte	%00001100
		.byte	%11111100
		.byte	%11111100
		.byte	%00001100
		.byte	%11111000
		.byte	%11110000
		;
		; function keys
@fkeyl:	.byte	%00001111
		.byte	%00001111
		.byte	%00001111
		.byte	%00001111
		.byte	%00001111
		.byte	%00001111
		.byte	%00001111
		.byte	%00001111
@fkey:	.byte	%11111011
		.byte	%11101111
		.byte	%11101111
		.byte	%10101011
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111
@fkeyb:	.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
@f1:	.byte	%11111011
		.byte	%11101011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
@f3:	.byte	%11101011
		.byte	%10111110
		.byte	%11111110
		.byte	%11101011
		.byte	%11111110
		.byte	%11111110
		.byte	%10111110
		.byte	%11101011
@f5:		.byte	%10101010
		.byte	%10111111
		.byte	%10111111
		.byte	%10101011
		.byte	%11111110
		.byte	%11111110
		.byte	%10111110
		.byte	%11101011
@f7:	.byte	%10101010
		.byte	%11111110
		.byte	%11111110
		.byte	%11111011
		.byte	%11111011
		.byte	%11101111
		.byte	%11101111
		.byte	%11101111

		.res 93*8

@c1:	; C= logo
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00001010
		.byte	%00101010
		.byte	%00101010
		.byte	%10101000
@c2:
		.byte	%10100000
		.byte	%10100000
		.byte	%10100000
		.byte	%10100000
		.byte	%10100000
		.byte	%10100000
		.byte	%10100000
		.byte	%10101000
@c3:
		.byte	%00101010
		.byte	%00101010
		.byte	%00001010
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
@c4:
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%10000000
		.byte	%10000000
		.byte	%10000000
		.byte	%00101010
@c5:
		.byte	%00101010
		.byte	%00101000
		.byte	%00101000
		.byte	%00000000
		.byte	%00111100
		.byte	%00111100
		.byte	%00111111
		.byte	%00111111
@c6:
		.byte	%10000000
		.byte	%10000000
		.byte	%10000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
@c127:	; copyright symbol
		.byte	%00111100
		.byte	%01000010
		.byte	%10011101
		.byte	%10100001
		.byte	%10100001
		.byte	%10011101
		.byte	%01000010
		.byte	%00111100


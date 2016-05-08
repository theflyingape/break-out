		.include "VIC-SSS-MMX.h"

		.segment "SSSBUF"
		.global PLAYERS


;*********************************************************************
; Game runtime variables
;
BALLDX:		.byte 0		; 0 (right) or <>0 (left)
BALLDY:		.byte 0		; +2 (down) or -2 (up)
BALLS:		.res 2		; players' lives
BALLX:		.byte 0		; base index into DX: 0,4,8,12,16,20
BALLX2:		.byte 0		; subscript (0-3) for DX,(BALLX+BALLX2)
BALLZ:		.byte 0		; speed: 0,1,2
BEEP:		.byte 0		; duration
BREAKOUT:	.byte 1		; 0=thru, 1=out
DIFFICULTY:	.byte 1		; 0=easy, 1=normal, 2=hard
EXTRA:		.res 2		; extra ball bonus @ 200-points
OUT:		.byte 0		; 0-66
FLIP:		.byte 0		; governor
FRAME:		.byte 0		; video frame counter for action/animation timing
JOYMODE:	.byte 0		; 0=paddles; 1=joystick
PLAYER:		.byte 0		; player up: 0/1
PLAYERS:	.byte 0		; # of players up (0 = initial startup)
SCORE:		.res 4		; players' score in BCD


;*********************************************************************
; Game data
;
BALL:	.byte	%01000000
		.byte	%11100000
		.byte	%11100000
		.byte	%01000000
		; red, orange, yellow, green, blue, indigo
BRICKS:	.byte	$02,$08,$07,$05,$06,$04
BRICKT:	.byte	0,0,237,0,225,231,228,232,235
BRICKV:	.byte	0,0,5,0,1,2,1,2,3
BRICKZ:	.byte	0,0,2,0,0,1,0,1,1
DX:		.byte	2,0,1,0		; %00000
		.byte	2,0,2,0		; %00100
		.byte	2,1,2,1		; %01000
		.byte	2,2,2,2		; %01100
		.byte	3,2,3,2		; %10000
		.byte	3,3,3,3		; %10100
PADDLE:	.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
SPEED:	.byte	%10111010	; player launch speed
		.byte	%11101110	; hit brick layer 3
		.byte	%11111111	; hit brick layer 6

		.segment "CODE"
		.global RESTART


;*********************************************************************
; Game entry point after boot
;
RESTART:
		JSR SSSINIT		; initialize software sprite stack
		;
		LDA #%10101010	; PADDLE: 3x16 sprite floats X
		LDY #3
		JSR SSSCREATE
		LDA #1			; white
		LDX #<PADDLE
		LDY #>PADDLE
		JSR SSSANIM
		LDA #24*8
		STA SPRITEY
		LDA #12*8
		STA SPRITEX
		;
		LDA #%11101100	; BALL: 4x3 sprite floats X-Y
		LDY #4
		JSR SSSCREATE
		LDA #1			; white
		LDX #<BALL
		LDY #>BALL
		JSR SSSANIM


;*********************************************************************
; Reset playfield
;
NEXTLEVEL:
		LDA PLAYER
		EOR #1			; p1:white, p2:black
		STA COLORCODE
		LDA #SSSNULL
		JSR SSSCLEAR
		LDA #66
		STA OUT
		;
		LDX #$00
		LDY #$04
		JSR SSSPLOT
		LDY #$00
		STY R0
		LDX #$00
		STX R1
@loop:
		LDA BRICKS,Y
		STA COLORCODE
		LDA #$00		; brick character code
		JSR SSSPRINT
		INC R1
		LDX R1
		LDA CRSRCOL
		BNE @loop
		INC R0
		LDY R0
		CPY #$06
		BNE @loop

		LDX PLAYERS
		BNE NEXTBALL
		JSR GAMEOVER
		JMP NEXTLEVEL


;*********************************************************************
; Player up!
;
NEXTBALL:
		JSR SCORESTATUS
		LDA JIFFYL
		AND #$3F
		ASL
		CLC
		ADC #43
		STA SPRITEX+1
		LDA #104
		STA SPRITEY+1
		LDX #1
		JSR SSSTOUCH
		;
		LDA #0
		STA BALLDX		; start by going right
		STA BALLZ		; start with slow speed
		LDA JIFFYL
		AND #$01
		BEQ @down
		LDA #-1			; go left
		STA BALLDX
@down:	LDA #1
		STA BALLDY		; start by going down slowly
		LDA #4
		STA BALLX		; start with med-low angle
		LDY #1
		LDX DIFFICULTY
		CPX #2
		BNE @flips
		DEY
@flips:	STY FLIP		; governor
@loop:
		JSR MOVEPADDLE
		LDY #1			; wait for vertical sync
		JSR SSSFLIP
		LDA $028D
		AND #$02		; got C= key?
		BNE GAMELOOP
		LDX PLAYER
		BEQ @p1
		LDX JOYMODE
		BNE @p1
		LDY #$00
		STY $9113
		LDA #$7F
		STA $9122
		LDA $9120
		AND #$80		; got paddle #2 FIRE ?
		BEQ GAMELOOP
@p1:	LDA #$FF
		STA $9122
		LDY $9111
		TYA
		AND #$20		; got joystick FIRE ?
		BEQ GAMELOOP
		LDX PLAYER
		BNE @loop
		LDX JOYMODE
		BNE @loop
		TYA
		AND #$10		; got paddle #1 FIRE ?
		BEQ GAMELOOP
		JMP @loop


;*********************************************************************
; Main game-playing loop
;
GAMELOOP:
		JSR MOVEBALL
		JSR MOVEPADDLE
@skip:
		LDY FLIP		; governor
		JSR SSSFLIP
		INC FRAME
		JMP GAMELOOP


;*********************************************************************
; Game ball's collision-detection action routine
;
ACTION:
		LDA BREAKOUT
		BEQ @char
		LDA BALLDY
		CMP #$80		; is ball going down?
		BCC @skip
		LDA #%10101100	; no, suppress collision-detection to allow
		STA SPRITEDEF+1	; ball to passthru next layer of bricks
		LDA #-10
		STA FRAME		; for next n-frames
@skip:	LDA #0
		SEC
		SBC BALLDY		; reverse ball Y direction
		STA BALLDY
@char:	LDA SPRITEBACK+1
		BNE @paddle
		LDX SPRITECX+1
		LDY SPRITECY+1
		JSR SSSPEEKXY
		LDA CRSRCOLOR
		AND #$0F
		PHA				; save brick color
		TAX
		LDA BRICKZ,X	; get speed for this brick
		CMP BALLZ		; at high ball speed already?
		BCC @z
		STA BALLZ		; replace speed with higher value
@z:		LDA PLAYER
		EOR #1			; p1:white, p2:black
		STA COLORCODE
		LDA CRSRCOL
		AND #$FE		; do the even brick first
		STA CRSRCOL
		LDA #SSSNULL
		JSR SSSPOKE
		INC CRSRCOL		; now the odd
		LDA #SSSNULL
		JSR SSSPOKE
		PLA
		TAY
		LDA BRICKT,Y
		STA VIC+$0C		; 3rd voice
		LDA #6
		STA BEEP
		LDA BRICKV,Y
		JSR SCOREUPDATE
		DEC OUT
		BNE @fini
		LDA #25
		LDA VIC+$0C
		STA VIC+$0B
		STA VIC+$0A
		; this player finished level
		; allow other player (if any) a turn
		LDA PLAYERS
		CMP #1
		BEQ @nb
		LDA PLAYER
		EOR #1
		TAX
		LDA BALLS,X
		BEQ @nb
		STX PLAYER
@nb:	PLA
		PLA
		JMP NEXTLEVEL
@fini:	RTS
@paddle:
		LDA BREAKOUT
		BNE @thru
		LDA #0
		SEC
		SBC BALLDY		; reverse ball Y direction
		STA BALLDY
@thru:	LDA BALLDY
		CMP #-1
		BNE @beep
		DEC BALLDY		; increase ball speed
@beep:	LDA #219
		STA VIC+$0B
		LDA #5
		STA BEEP
		LDA SPRITEX+1	; ball
		SEC
		SBC SPRITEX		; compute delta on paddle's surface
		BCS @cs
		LDA #-1			; go left
		STA BALLDX
		LDA #4
		LDY DIFFICULTY
		BEQ @anginc
		ASL				; corner-hit goes more extreme
		BNE @anginc
@cs:	CMP #14			; corner-hit?
		BCC @rght
		LDA #0			; go right
		STA BALLDX
		LDA #4
		LDY DIFFICULTY
		BEQ @anginc
		ASL				; corner-hit goes more extreme
		BNE @anginc
@rght:	CMP #11
		BCC @mid
		LDA #0			; go right
		STA BALLDX
		LDA #4
		BNE @anginc
@mid:	CMP #5
		BCS @angdec
@lft:	LDA #-1			; go left
		STA BALLDX
		LDA #4
@anginc:
		CLC
		ADC BALLX
		CMP #20
		BCC @angok
		LDA #20
@angok:	STA BALLX
		RTS
@angdec:
		LDX #4
		STX @xdec+1
		CMP #10
		BCC @good
		LDA #0			; go right
		STA BALLDX
		BEQ @dodec
@good:	CMP #6
		BCS @xgood
		LDA #-1			; go left
		STA BALLDX
		BNE @dodec
@xgood:	ASL @xdec+1
@dodec:	LDA BALLX
		SEC
@xdec:	SBC #8
		BCS @angok
		LDA #0
		BEQ @angok


;*********************************************************************
; end of play sequence
;
DEADBALL:
		JSR SSSREFRESH
		LDY #0			; immediate
		JSR SSSFLIP		; gratuitous
		LDA #219
		STA VIC+$0A
		LDY #50
		STY BEEP
		JSR SSSFLIP		; gratuitous
		LDX PLAYER
		DEC BALLS,X
		LDA BALLS
		BNE @next
		LDA BALLS+1
		BNE @next
		JSR GAMEOVER
		JMP NEXTLEVEL
@next:	PLA
		PLA
		LDA PLAYERS
		CMP #1
		BEQ @nb
		LDA PLAYER
		EOR #1			; swap player up
		TAX
		LDA BALLS,X
		BNE @np
@nb:	JMP NEXTBALL	; same player continues
@np:
		STX PLAYER
		LDA DIFFICULTY
		BNE @restore
		JMP NEXTLEVEL	; easy mode starts with fresh layer of bricks
@restore:
		LDX #0
		LDY #4
		JSR SSSPLOT
		LDX PLAYER
		BEQ @p2			; WAS P2? save to offset 0
		LDX #66			; WAS P1? save to offset 66
@p2:	LDY #0
@save:	LDA (SCRNLINE),Y
		STA DATASETTE,X
		INX
		INY
		INY
		CPY #132
		BNE @save
		;
		LDA PLAYER
		EOR #1			; p1:white, p2:black
		STA COLORCODE
		LDA #SSSNULL
		JSR SSSCLEAR
		;
		LDX #0
		LDY #4
		JSR SSSPLOT
		LDA #0
		STA OUT
		TAY
		STY R0
		TAX
		LDA PLAYER
		BNE @r1			; NOW P2? restore from offset 0
		LDX #66			; NOW P1? restore from offset 66
@r1:	STX R1
@loop:	LDA DATASETTE,X
		TAX
		BNE @mt
		INC OUT			; account for brick
		LDA BRICKS,Y
		BNE @color
@mt:	LDA PLAYER
		EOR #1			; p1:white, p2:black
@color:	STA COLORCODE
		TXA
@skip:	JSR SSSPRINT
		TXA
		JSR SSSPRINT
		INC R1
		LDX R1
		LDA CRSRCOL
		BNE @loop
		INC R0
		LDY R0
		CPY #$06
		BNE @loop
		JMP NEXTBALL


;*********************************************************************
; end of game sequence
;
GAMEOVER:
		LDA PLAYERS
		CMP #2
		BNE @skip
		LDA PLAYER
		EOR #1			; display other player's score
		STA PLAYER
		JSR SCORESTATUS
@skip:
		LDY #0
		STY $C6			; empty keyboard buffer
		DEY
		STY FRAME
		LDY #4
		STY R4
@cont:
		LDX #6
		LDY #13
		JSR SSSPLOT
		LDA JIFFYL
		AND #$40
		BNE @text
		LDA #1			; white
		STA COLORCODE
		JSR SSSPRINTS
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$00
		JMP @anim
@text:	JSR SSSPRINTS	; print GAME
		.byte	$C2,$87,$81,$8D,$85,$00
		INC CRSRCOL
		INC CRSRCOL
		JSR SSSPRINTS	; print OVER
		.byte	$8F,$96,$85,$92,$00
@anim:
		JSR MOVEPADDLE
		LDY #1
		JSR SSSFLIP
		JSR SHOWTOP
		INC FRAME
		BNE @start
		INC R4
		LDA R4
		CMP #4
		BCC @fkey
		LDA #0
		STA R4
@fkey:	CMP #0
		BNE @fkey3
		LDX #1
		LDY #$18
		JSR SHOWSTART
		JMP @fbot
@fkey3:	CMP #1
		BNE @fkey5
		LDX #2
		LDY #$19
		JSR SHOWSTART
		JMP @fbot
@fkey5:	CMP #2
		BNE @fkey7
		JSR SHOWDIFFICULTY
		JMP @fbot
@fkey7:	JSR SHOWMODE
@fbot:	JSR SHOWBOTTOM
@vic:	LDA VIC+$0F
		CLC
		ADC #$10
		BCC @cycle
		LDA #$8B
@cycle:	STA VIC+$0F
@start:
		JSR SHOWTOP
		LDX JOYMODE
		BNE @joy
		LDY #$00
		STY $9113
		LDA #$7F
		STA $9122
		LDA $9120
		AND #$80		; got paddle #2 FIRE ?
		BEQ @p2
@joy:	LDA #$FF
		STA $9122
		LDY $9111
		TYA
		AND #$20		; got joystick FIRE ?
		BEQ @p1
		LDX JOYMODE
		BNE @kb
		TYA
		AND #$10		; got paddle #1 FIRE ?
		BEQ @p1
@kb:	JSR GETIN
		BEQ @next
		LDX #200
		STX FRAME
		STX VIC+$0C		; 3rd voice
		LDX #6
		STX BEEP
@f1:	CMP #$85
		BEQ @p1
@f3:	CMP #$86
		BEQ @p2
@f5:	CMP #$87
		BNE @f7
		INC DIFFICULTY
		LDA DIFFICULTY
		CMP #3
		BCC @xf5
		LDA #0
@xf5:	STA DIFFICULTY
		JSR SHOWDIFFICULTY
		JMP @cont
@f7:	CMP #$88
		BNE @next
		LDA BREAKOUT
		EOR #1
		STA BREAKOUT
		JSR SHOWMODE
@next:	JMP @cont
@p1:
		LDX #1
		STX PLAYERS
		LDY #$18
		JSR SHOWSTART
		LDY #0
		JSR SSSFLIP
		LDY #50
		JSR SSSFLIP
		BNE @fini
@p2:
		LDX #2
		STX PLAYERS
		LDY #$19
		JSR SHOWSTART
		LDY #0
		JSR SSSFLIP
		LDY #50
		JSR SSSFLIP
		LDA #5
		STA BALLS+1
@fini:
		LDA #0
		LDX #2*66
@reset:	STA DATASETTE-1,X
		DEX
		BNE @reset
		STX EXTRA
		STX EXTRA+1
		STX PLAYER
		STX SCORE
		STX SCORE+1
		STX SCORE+2
		STX SCORE+3
		LDA #5
		STA BALLS
		LDA #$EB		; cyan border, light blue background
		STA VIC+$0F
		RTS


;*********************************************************************
; Generate the game ball physics
;
MOVEBALL:
		LDA SPRITEZ+1
		AND #%1000		; collision?
		BNE @hit
@speed:
		LDA FRAME
		BNE @cont
		LDX #%11101100	; re-enable collision-detection
		STX SPRITEDEF+1
@cont:	AND #$07
		TAX
		LDA MASK,X
		LDX BALLZ
		AND SPEED,X
		BNE @y
		RTS
@hit:
		JSR ACTION
@y:
		LDA SPRITEY+1
		CLC
		ADC BALLDY
		STA SPRITEY+1
		CMP SSSCLIPY
		BCC @alive
		JMP DEADBALL
@alive:
		CMP #3*8
		BCS @x
		LDX #%11101100	; enable collision-detection
		STX SPRITEDEF+1
		LDA #223
		STA VIC+$0B
		LDA #5
		STA BEEP
		LDA #3*8
		STA SPRITEY+1
		LDA DIFFICULTY
		BEQ @ez
		LDA #0			; lift governor
		STA FLIP
		LDA #2			; high speed
		STA BALLZ
@ez:	LDA #2			; go down
		STA BALLDY
@x:
		INC BALLX2
		LDA BALLX2
		AND #$03		; 0-3
		ORA BALLX
		TAX
		LDA DX,X
		BEQ @fini
		LDY BALLDX
		BEQ @right
@left:	LDA #0
		SEC
		SBC DX,X
@right:	CLC
		ADC SPRITEX+1
		STA SPRITEX+1
		CMP #(24*8-3)
		BCC @ok
		LDA #223
		STA VIC+$0B
		LDA #5
		STA BEEP
		LDA #(24*8-3)
		STA SPRITEX+1
		LDY #-1			; next move, go left
		STY BALLDX
		BNE @fini
@ok:	CMP #17
		BCS @fini
		LDA #223
		STA VIC+$0B
		LDA #5
		STA BEEP
		LDA #16
		STA SPRITEX+1
		LDY #0			; next move, go right
		STY BALLDX
@fini:	LDX #1
		JSR SSSTOUCH
		RTS


;*********************************************************************
; Process player input
;
MOVEPADDLE:
		LDX PLAYER
		LDA VIC+$08,X	; read POT
		BNE @pot
@joy:
		LDX #1
		STX JOYMODE
		LDY #$00
		STY $9113
		LDA #$7F
		STA $9122
		LDA $9120
		AND #$80
		BNE @joy1
		INC SPRITEX		; right
		INC SPRITEX
		LDA SPRITEX
		BNE @check
@joy1:
		LDA #$FF
		STA $9122
		LDA $9111
		AND #$10
		BNE @fini
		DEC SPRITEX		; left
		DEC SPRITEX
		LDA SPRITEX
		BNE @check
@pot:
		LDX #0
		STX JOYMODE
		EOR #-1			; invert
		CMP SPRITEX
		BEQ @fini
@check:
		CMP #10
		BCS @okl
		LDA #10
@okl:	CMP #(22*8+6)
		BCC @okr
		LDA #(22*8+6)
@okr:	STA SPRITEX		; update
		LDX #0
		JSR SSSTOUCH	; redraw paddle
@fini:	RTS


;*********************************************************************
; Show player's score
;
SCORESTATUS:
		LDX #1
		LDA PLAYER
		BEQ @p1
		LDX #13
@p1:	LDY #0
		JSR SSSPLOT
		LDA #2
		STA R0
		LDA PLAYER
		STA COLORCODE	; 0=black, 1=white
		ASL
		TAX
@loop:	LDA SCORE,X
		LSR
		LSR
		LSR
		LSR
		CLC
		ADC #1
		TAY
		JSR SSSPRINT
		TYA
		CLC
		ADC #10
		JSR SSSPRINT
		LDA SCORE,X
		AND #$0F
		CLC
		ADC #1
		TAY
		JSR SSSPRINT
		TYA
		CLC
		ADC #10
		JSR SSSPRINT
		INX
		DEC R0
		BNE @loop
		;
		LDX #1
		LDA PLAYER
		BNE @p2
		LDX #20
@p2:	LDY #0
		JSR SSSPLOT
		LDX PLAYER
		LDA BALLS,X
		BEQ @fini
		TAX
@balls:	DEX
		BEQ @xicon
		LDA #128+81		; ball icon
		JSR SSSPRINT
		LDA PLAYER
		BNE @balls
		DEC CRSRCOL
		DEC CRSRCOL
		BNE @balls
@xicon:	LDA #SSSNULL
		JSR SSSPRINT
@fini:	RTS


;*********************************************************************
; Update player's score
; send A with decimal number to add
;
SCOREUPDATE:
		PHA
		LDA PLAYER
		ASL
		TAX
		INX				; 1's
		PLA
		SED
		CLC
		ADC SCORE,X
		STA SCORE,X
		BCC @cc
@cs:	DEX				; too bad if 10,000 is breached ... no one
		LDA SCORE,X		; likes a smarty-pants!  :P
		CLC
		ADC #$01
		STA SCORE,X
		BCS @cs
@cc:	CLD
		LDX PLAYER
		LDA EXTRA,X
		BNE @show
		TXA
		ASL
		TAX
		LDA SCORE,X
		CMP #$02
		BCC @show
		LDX PLAYER
		INC EXTRA,X		; woot!
		INC BALLS,X
@show:	JSR SCORESTATUS
		RTS


;*********************************************************************
; Display function key top
;
SHOWTOP:
		LDX #4
		LDY #16
		JSR SSSPLOT
		JSR SSSPRINTS
		.byte	$C9,$EC,$E2,$E2,$FB,$00
		LDX #4
		LDY #17
		JSR SSSPLOT
		RTS


;*********************************************************************
; Display [Fy] START Px
;
SHOWSTART:
		STY @n
		TXA
		CLC
		ADC #$B0
		STA @p
		JSR SSSPRINTS
		.byte	$15,$16
@n:		.byte	$18,$E1,$C6,$A0,$93,$94,$81,$92,$94,$A0
@p:		.byte	$B1,$90,$00
		LDX #4
		LDY #18
		JSR SSSPLOT
		JSR SSSPRINTS
		.byte	$C9,$FC,$17,$17,$FE,$00
		RTS


;*********************************************************************
; Display [F5] NOVICE|CLASSIC|ADVANCED
;
SHOWDIFFICULTY:
		JSR SSSPRINTS
		.byte	$15,$16,$1A,$E1,$00
		LDA DIFFICULTY
		BNE @nez
@ez:	JSR SSSPRINTS
		.byte	$C7,$A0,$8E,$8F,$96,$89,$83,$85,$A0,$A0,$00
		RTS
@nez:	CMP #1
		BNE @adv
		JSR SSSPRINTS
		.byte	$C6,$A0,$83,$8C,$81,$93,$93,$89,$83,$A0,$00
		RTS
@adv:	JSR SSSPRINTS
		.byte	$C4,$A0,$81,$84,$96,$81,$8E,$83,$85,$84,$00
		RTS


;*********************************************************************
; Display [F7]BREAK-OUT|BREAKTHRU
;
SHOWMODE:
		JSR SSSPRINTS
		.byte	$15,$16,$1B,$E1,$00
		LDA BREAKOUT
		BNE @out
@thru:	JSR SSSPRINTS
		.byte	$C1,$82,$92,$85,$81,$8B,$94,$88,$92,$95,$00
		RTS
@out:	JSR SSSPRINTS
		.byte	$C0,$82,$92,$85,$81,$8B,$AD,$8F,$95,$94,$00
		RTS


;*********************************************************************
; Display function key bottom
;
SHOWBOTTOM:
		LDX #4
		LDY #18
		JSR SSSPLOT
		JSR SSSPRINTS
		.byte	$C9,$FC,$17,$17,$FE,$00
		LDY #0
		JSR SSSFLIP
		RTS


;*********************************************************************
; Background software interrupt routine
;
		.global MYIRQ
MYIRQ:
		CLD
		LDA BEEP
		BEQ @mute
		DEC BEEP
		BNE @cont
@mute:	STA VIC+$0A		; 1st voice
		STA VIC+$0B		; 2nd voice
		STA VIC+$0C		; 3rd voice
@cont:	JMP SSSIRQ		; process synchronous flipping


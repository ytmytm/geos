; GEOS KERNAL by Berkeley Softworks
; reverse engineered by Maciej Witkowiak, Michael Steil
;
; Graphics library: BitmapUp syscall

.include "const.inc"
.include "geossym.inc"
.include "geosmac.inc"
.include "config.inc"
.include "kernal.inc"
.include "c64.inc"

.include "kernal/beamracer/beamracer-vlib/vasyl.s"

.import _GetScanLine_BR
.import SetupBeamRacerRAMr5r6

.global BitmapUpHelp
.global BitmapDecode
.global _BitmapUp

.segment "graph3c"

;---------------------------------------------------------------
; BitmapUp                                                $C142
;
; Pass:      r0  ptr of bitmap
;            r1L x pos. in bytes (0-39)
;            r1H y pos. in scanlines (0-199)
;            r2L width in bytes (0-39)
;            r2H height in pixels (0-199)
; Return:    display the bitmap
; Destroyed: a, x, y, r0 - r9l
;---------------------------------------------------------------
_BitmapUp:
	PushB r9H
	LoadB r9H, NULL ; flag, if bit 7 set then data comes from (r13) call with (r14) called after each packet processed (r4L==0)
	sta r3L
	sta r4L
@1:	jsr BitmapUpHelp        ; decode one line
	inc r1H                 ; next line (for _GetScanLine)
	dec r2H
	bne @1                  ; are we done yet?
	PopB r9H
	rts

BitmapUpHelp:
	ldx r1H
	jsr _GetScanLine_BR
	MoveB r2L, r3H          ; copy width to r3H (current column counter)

	lda r1L                 ; xpos (cards)
	clc                     ; add to start of row
	adc r5L
	sta r5L
	sta r6L
	bcc :+
	inc r5H
	inc r6H

:	php
	sei
	START_IO
	jsr SetupBeamRacerRAMr5r6
	lda #1
	sta VREG_STEP0
	sta VREG_STEP1

:	jsr BitmapDecode
	sta VREG_PORT0
	sta VREG_PORT1
	dec r3H
	bne :-

	rmbf CONTROL_PORT_READ_ENABLE_BIT, VREG_CONTROL ; clear to avoid 'weird issues' 
	END_IO
	plp
	rts

BitmapDecode:
BitmapDecodeX:
	lda r3L
	and #%01111111
	beq @2
	bbrf 7, r3L, @1
	jsr BitmapDecode2
	dec r3L
	rts
@1:	lda r7H
	dec r3L
	rts
@2:	lda r4L
	bne @3
	bbrf 7, r9H, @3
	jsr IndirectR14
@3:	jsr BitmapDecode2
	sta r3L
	cmp #$dc
	bcc @4
	sbc #$dc
	sta r7L
	sta r4H
	jsr BitmapDecode2
	subv 1
	sta r4L
	MoveW r0, r8
	bra @2
@4:	cmp #$80
	bcs BitmapDecodeX
	jsr BitmapDecode2
	sta r7H
	bra BitmapDecodeX

BitmapDecode2:
	bit r9H
	bpl @1
	jsr IndirectR13
@1:
	ldy #0
	lda (r0),y
	inc r0L
	bne @2
	inc r0H
@2:	ldx r4L
	beq @3
	dec r4H
	bne @3
	ldx r8H
	stx r0H
	ldx r8L
	stx r0L
	ldx r7L
	stx r4H
	dec r4L
@3:	rts

IndirectR13:
	jmp (r13)

IndirectR14:
	jmp (r14)


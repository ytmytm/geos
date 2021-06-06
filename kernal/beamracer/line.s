; GEOS KERNAL by Berkeley Softworks
; reverse engineered by Maciej Witkowiak, Michael Steil
;
; Graphics library: line functions

.include "const.inc"
.include "geossym.inc"
.include "geosmac.inc"
.include "config.inc"
.include "kernal.inc"
.include "c64.inc"

.include "kernal/beamracer/beamracer-vlib/vasyl.s"

.import BitMaskPow2Rev
.import BitMaskLeadingSet
.import BitMaskLeadingClear
.import _GetScanLine
.import _GetScanLine_BR

.global ImprintLine
.global _HorizontalLine
.global _InvertLine
.global _RecoverLine
.global _VerticalLine


; in: r11L - y position
;     r3   - x left
;     r4   - x right
; out:
;     r5   - line address on foreground
;     r6   - line address on background
;     r8L  - bitmask for first byte on the left (bits set on the left)
;     r8H  - bitmask for last byte on the right (bits set on the right)
;
;.segment "graph2a"
.segment "load1b"
PrepareXCoord_BR:
        ldx r11L
        jsr _GetScanLine_BR
        bra :+
PrepareXCoord:
	ldx r11L
	jsr _GetScanLine
:	lda r4L
	and #%00000111
	tax
	lda BitMaskLeadingClear,x
	sta r8H
	lda r3L
	and #%00000111
	tax
	lda BitMaskLeadingSet,x
	sta r8L
	rts

; in:  r3, r4   X coords (start,end)
; out: r4L      distance in cards (distance divided by 8)
;.segment "graph2a"
.segment "load1b"
GetCardsDistance:
	SubW r3, r4
	lsr r4H
	ror r4L
	lsr r4H
	ror r4L
	lsr r4L
	lda r8L
	rts

; in: r3       X coord (0-319)
;     r5       line address on foreground
;     r6       line address on background
; out:
;     r3       divided by 8
;     r5       adjusted to first card of X
;     r6       adjusted to first card of X
.segment "graph2a"
AdjustR5R6ToX:
	lda r3L
	lsr r3H
	ror
	lsr
	lsr
	sta r3L

	;lda r3L
	add r5L
	sta r5L
	bcc :+
	inc r5H

:	lda r3L
	add r6L
	sta r6L
	bcc :+
	inc r6H
:	rts

;---------------------------------------------------------------
; HorizontalLine                                          $C118
;
; Pass:      a    pattern byte
;            r11L y position in scanlines (0-199)
;            r3   x in pixel of left end (0-319)
;            r4   x in pixel of right end (0-319)
; Return:    r11L unchanged
; Destroyed: a, x, y, r5 - r8, r11
;---------------------------------------------------------------

; speedups when $a000-$bf40 is free:
; - scanline offsets (200 * 2 = 400 bytes)
; - card offsets (320 * 2 = 640 bytes)
; - collapse PrepareXCoord+AdjustR5R6ToX into one function (CALC in cc65 TGI: https://github.com/cc65/cc65/blob/master/libsrc/c128/tgi/c128-vdc.s)
; - can PORT0/PORT1 repeated writes go in parallel?

; in:	r5 - foreground address
;	r6 - background address
; out:
;	PORT0 set to r5
;	PORT1 set to r6
;	STEP0/STEP1 set to 0
;	CONTROL set to bank and read/write operation of ports
; destroys: a
;
.segment "graph2a"
SetupBeamRacerAddresses:
	jsr PrepareXCoord_BR	; --> r5/r6 set to start of line, r8L/r8H bit pattern to protect on left, pattern right
	jsr GetCardsDistance    ; --> r4L set to card distance
	jsr AdjustR5R6ToX	; --> r5/r6 adjusted to card with left X data, r3 set to r3 div 8
	lda VREG_CONTROL
	and #%11111000
	ora #br_screen_bank
	ora #(1 << CONTROL_PORT_READ_ENABLE_BIT)
	sta VREG_CONTROL
	MoveB r5L, VREG_ADR0
	MoveB r5H, VREG_ADR0+1
	MoveB r6L, VREG_ADR1
	MoveB r6H, VREG_ADR1+1
	LoadB VREG_STEP0, 0	; don't advance for the first byte (there will be read/write)
	sta VREG_STEP1
	rts

.segment "graph2a"
_HorizontalLine:
	sta r7L			; temporary for pattern
	PushW r3
	PushW r4
	START_IO
	jsr SetupBeamRacerAddresses

	lda r8L
	beq @wholecards		; skip handling of the first byte (r8L==0?)

				; handle left byte (value already in A)
	eor #$ff		; reverse screen protection bitmask
	and r7L			; take from patternbits that we need
	sta r5L			; keep as temporary
	lda r4L
	beq :+
	dec r4L			; decrease number of full cards

:	ldx #1
	bbrf 7, dispBufferOn, @skipfore1
	lda VREG_PORT0
	and r8L			; clear pattern bits
	ora r5L			; add patern
	stx VREG_STEP0		; increase address on write
	sta VREG_PORT0

@skipfore1:
	bbrf 6, dispBufferOn, @skipback1
	lda VREG_PORT1
	and r8L
	ora r5L
	stx VREG_STEP1
	sta VREG_PORT1
@skipback1:

@wholecards:
	lda r8H			; if whole last byte is occupied (mask==0)
	bne :+
	inc r4L			; then increase number of full cards to write

:	ldx r4L			; how many full cards?
	beq @2			; same byte that we already did, skip over

	LoadB VREG_STEP0, 1	; increase address on write
	sta VREG_STEP1

	bbrf 7, dispBufferOn, @skipfore2
	lda r7L			; pattern
	sta VREG_PORT0		; write once
	dex
	beq @1			; skip if nothing more
	stx VREG_REP0		; repeat this many times
:	ldy VREG_REP0
	bne :-			; wait until done
@1:	inx			; how many full cards? (restore r4L value)

@skipfore2:
	bbrf 6, dispBufferOn, @skipback2
	lda r7L
	sta VREG_PORT1		; write once
	dex
	beq @2
	stx VREG_REP1
:	ldy VREG_REP1
	bne :-

@skipback2:
@2:	ldx r8H			; anything for the last byte?
	beq @end

	LoadB VREG_STEP0, 0	; don't increase address on write
	sta VREG_STEP1

	txa			; handle right byte
	eor #$ff		; reverse bitmask
	and r7L			; take only pattern bits that we need
	sta r5L			; temporary

	bbrf 7, dispBufferOn, @skipfore3
	lda VREG_PORT0
	and r8H			; clear pattern bits
	ora r5L			; add patern
	sta VREG_PORT0

@skipfore3:
	bbrf 6, dispBufferOn, @skipback3
	lda VREG_PORT1
	and r8H
	ora r5L
	sta VREG_PORT1
@skipback3:

@end:
HorizontalLineEnd:
	rmbf CONTROL_PORT_READ_ENABLE_BIT, VREG_CONTROL ; clear to avoid 'weird issues' 
	END_IO
	PopW r4
	PopW r3
	rts

; called from below
HLinEnd1:
    sta (r6),Y
    sta (r5),Y
HLinEnd2:
LineEnd:
    PopW r4
    PopW r3
    rts

;---------------------------------------------------------------
; InvertLine                                              $C11B
;
; Pass:      r3   x pos of left endpoint (0-319)
;            r4   x pos of right endpoint (0-319)
;            r11L y pos (0-199)
; Return:    r3-r4 unchanged
; Destroyed: a, x, y, r5 - r8
;---------------------------------------------------------------
.segment "graph2a"
_InvertLine:
	PushW r3
	PushW r4
	START_IO
	jsr SetupBeamRacerAddresses	; PORT0/PORT1 set to r5/r6 and step is 0

	lda r8L
	beq @wholecards		; skip handling of the first byte
	eor #$ff		; reverse screen protection bitmask
	sta r5L			; keep as temporary
	lda r4L
	beq :+
	dec r4L			; decrease number of full cards

:	ldx #1
	bbrf 7, dispBufferOn, @skipfore1
	lda VREG_PORT0
	eor r5L			; flip bits
	stx VREG_STEP0		; increase address on write
	sta VREG_PORT0

@skipfore1:
	bbrf 6, dispBufferOn, @skipback1
	lda VREG_PORT1
	eor r5L
	stx VREG_STEP1
	sta VREG_PORT1
@skipback1:

@wholecards:
	lda r8H			; if whole last byte is occupied (mask==0)
	bne :+
	inc r4L			; then increase number of full cards to write

:	ldy #0
	ldx r4L			; how many full cards?
	beq @2			; same byte that we already did, skip over

:	sty VREG_STEP0
	sty VREG_STEP1
	iny			; Y=1
	bbrf 7, dispBufferOn, @skipfore2
	lda VREG_PORT0
	eor #$ff
	sty VREG_STEP0		; advance on write
	sta VREG_PORT0
@skipfore2:
	bbrf 6, dispBufferOn, @skipback2
	lda VREG_PORT1
	eor #$ff
	sty VREG_STEP1		; advance on write
	sta VREG_PORT1
@skipback2:
	dey			; Y=0
	dex
	bne :-			; XXX bug: when "bpl" desktop menu #1/#3 have one card too much inverted; when "bne" deskotp menu #2 has one card too much inverted

@2:	ldx r8H			; anything for the last byte?
	beq @end

	txa			; handle right byte
	eor #$ff		; reverse bitmask
	sta r5L			; temporary

	sty VREG_STEP0		; Y=0 here, don't advance on read
	sty VREG_STEP1

	bbrf 7, dispBufferOn, @skipfore3
	lda VREG_PORT0
	eor r5L			; flip bits
	sta VREG_PORT0

@skipfore3:
	bbrf 6, dispBufferOn, @skipback3
	lda VREG_PORT1
	eor r5L
	sta VREG_PORT1
@skipback3:
@end:
	jmp HorizontalLineEnd

;---------------------------------------------------------------
; RecoverLine                                             $C11E
;
; Pass:      r3   x pos of left endpoint (0-319)
;            r4   x pos of right endpoint (0-319)
;            r11L y pos of line (0-199)
; Return:    copies bits of line from background to
;            foreground sceen
; Destroyed: a, x, y, r5 - r8
;---------------------------------------------------------------
.segment "graph2a"
_RecoverLine:
.ifdef wheels_size
	lda #$18 ; clc
	.byte $2c
ImprintLine:
	lda #$38 ; sec
	sta @1
	PushW r3
	PushW r4
	PushB dispBufferOn
	ora #ST_WR_FORE | ST_WR_BACK
	sta dispBufferOn
	jsr PrepareXCoord
	PopB dispBufferOn
@1:	clc
	bcc @2
	lda r5L
	ldy r6L
	sta r6L
	sty r5L
	lda r5H
	ldy r6H
	sta r6H
	sty r5H
@2:	ldy r3L
	lda r3H
	beq @3
	inc r5H
	inc r6H
@3:	CmpW r3, r4
	beq @6
	jsr LineHelp2
	lda r8L
	jsr LineHelp1
@4:	tya
	add #8
	tay
	bcc @5
	inc r5H
	inc r6H
@5:	dec r4L
	beq @7
	lda (r6),y
	sta (r5),y
	bra @4
@6:	lda r8L
	ora r8H
	bra @8
@7:	lda r8H
@8:	jsr LineHelp1
	jmp LineEnd

LineHelp1:
	sta r7L
	and (r5),y
	sta r7H
	lda r7L
	eor #$FF
	and (r6),y
	ora r7H
	sta (r5),y
	rts

LineHelp2:
	SubW r3, r4
	lsr r4H
	ror r4L
	lsr r4L
	lsr r4L
	rts
.else
	PushW r3
	PushW r4
	PushB dispBufferOn
	ora #ST_WR_FORE | ST_WR_BACK
	sta dispBufferOn
	jsr PrepareXCoord
.ifdef bsw128
	bbrf 7, graphMode, @X
	jmp Write80Line
@X:
.endif
RLin0a:
	PopB dispBufferOn
RLin0:
	ldy r3L
	lda r3H
	beq @1
	inc r5H
	inc r6H
@1:
.ifdef bsw128
	jsr CmpWR3R4
	beq @4
	jsr GetCardsDistance
.else
	CmpW r3, r4
	beq @4
	SubW r3, r4
	lsr r4H
	ror r4L
	lsr r4L
	lsr r4L
	lda r8L
.endif
	jsr RecLineHelp
@2:	tya
	addv 8
	tay
	bcc @3
	inc r5H
	inc r6H
@3:	dec r4L
	beq @5
	lda (r6),Y
	sta (r5),Y
	bra @2
@4:	lda r8L
	ora r8H
	bra @6
@5:	lda r8H
@6:	jsr RecLineHelp
	jmp HLinEnd2

.ifdef bsw128
Read80Line:
	jsr CmpWR3R4
	beq @3
	jsr GetCardsDistance
	jsr Read80Help
	iny
@1:	dec r4L
	beq @4
@2:	bit vdcreg
	bpl @2
	lda vdcdata
	sta (r6),y
	iny
	bne @1
@3:	lda r8L
	ora r8H
	bra @6
@4:	tya
	clc
	adc r5L
	sta r5L
	sta r6L
	bcc @5
	inc r5H
	inc r6H
@5:	lda r8H
@6:	jsr Read80Help
	PopB dispBufferOn
	jmp HLinEnd2

Write80Line:
	jsr CmpWR3R4
	beq @3
	jsr GetCardsDistance
	jsr Write80Help
	iny
@1:	dec r4L
	beq @4
	lda (r6),y
@2:	bit vdcreg
	bpl @2
	sta vdcdata
	iny
	bne @1
@3:	lda r8L
	ora r8H
	bra @6
@4:	tya
	clc
	adc r5L
	sta r5L
	sta r6L
	bcc @5
	inc r5H
	inc r6H
@5:	lda r8H
@6:	jsr Write80Help
	PopB dispBufferOn
	jmp HLinEnd2
.endif

RecLineHelp:
	sta r7L
	and (r5),Y
	sta r7H
	lda r7L
	eor #$FF
	and (r6),Y
	ora r7H
	sta (r5),Y
	rts
.endif

.ifdef bsw128
Write80Help:
	sta r7L
	jsr LF558
	sta r7H
	lda r7L
	eor #$FF
	ldy #0
	and (r6),y
	ora r7H
	jmp StaFrontbuffer80

Read80Help:
	sta r7L
	ldy #0
	and (r6),y
	sta r7H
	lda r7L
	eor #$FF
	jsr LF558
	ora r7H
	sta (r6),y
	rts
.endif

;---------------------------------------------------------------
; VerticalLine                                            $C121
;
; Pass:      a pattern
;            r3L top of line (0-199)
;            r3H bottom of line (0-199)
;            r4  x position of line (0-319)
; Return:    draw the line
; Destroyed: a, x, y, r4 - r8, r11
;---------------------------------------------------------------
.segment "graph2a"
_VerticalLine:
	sta r8L			; pattern

	tax
	PushB r8H
	stx r8H

	PushW r3
	ldx r3L			; top Y
	jsr _GetScanLine_BR	; -> r5,r6 line address
	MoveW r4, r3
	jsr AdjustR5R6ToX
	PopW r3

	lda r4L
	and #%00000111
	tax
	lda BitMaskPow2Rev,x	; bit of interest
	sta r7L
	eor #$ff		; bits to protect
	sta r7H

	START_IO

	lda VREG_CONTROL
	and #%11111000
	ora #br_screen_bank
	ora #(1 << CONTROL_PORT_READ_ENABLE_BIT)
	sta VREG_CONTROL
	LoadB VREG_STEP0, 40	; advance one line every r/w
	sta VREG_STEP1

	; pass 1 foreground
	bbrf 7, dispBufferOn, @skipfore
	; read on port 0, write to port 1
	MoveB r5L, VREG_ADR0
	MoveB r5H, VREG_ADR0+1
	MoveB r5L, VREG_ADR1
	MoveB r5H, VREG_ADR1+1
	ldy r3L
@1:	tya
	and #%00000111
	tax
	lda BitMaskPow2Rev,x
	and r8L
	beq :+
	lda #$ff
:	and r7L
	sta r8H
	lda VREG_PORT0
	and r7H
	ora r8H
	sta VREG_PORT1
	iny
	cpy r3H
	bne @1
	bcc @1

@skipfore:
	; pass 2 background
	bbrf 6, dispBufferOn, @skipback
	MoveB r6L, VREG_ADR0
	MoveB r6H, VREG_ADR0+1
	MoveB r6L, VREG_ADR1
	MoveB r6H, VREG_ADR1+1
	ldy r3L
@2:	tya
	and #%00000111
	tax
	lda BitMaskPow2Rev,x
	and r8L
	beq :+
	lda #$ff
:	and r7L
	sta r8H
	lda VREG_PORT0
	and r7H
	ora r8H
	sta VREG_PORT1
	iny
	cpy r3H
	bne @2
	bcc @2

@skipback:
	END_IO
	PopB r8H
	rts


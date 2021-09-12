; GEOS KERNAL by Berkeley Softworks
; reverse engineered by Maciej Witkowiak, Michael Steil
;
; Graphics library: TestPoint, DrawPoint, DrawLine syscalls

.include "const.inc"
.include "geossym.inc"
.include "geosmac.inc"
.include "config.inc"
.include "kernal.inc"
.include "c64.inc"

.include "kernal/beamracer/beamracer-vlib/vasyl.s"

.import BitMaskPow2Rev
.import _HorizontalLine
.import _GetScanLine_BR
.import _Dabs
.import SetupBeamRacerRAMr5r6
.import AdjustR5R6ToX

.global _TestPoint
.global _DrawPoint
.global _DrawLine

.segment "graph4"

;---------------------------------------------------------------
; DrawLine                                                $C130
;
; Pass:      signFlg  set to recover from back screen
;                     reset for drawing
;            carryFlg set for drawing in forground color
;                     reset for background color
;            r3       x pos of 1st point (0-319)
;            r11L     y pos of 1st point (0-199)
;            r4       x pos of 2nd point (0-319)
;            r11H     y pos of 2nd point (0-199)
; Return:    line is drawn or recover
; Destroyed: a, x, y, r4 - r8, r11
;---------------------------------------------------------------
_DrawLine:
	php
	bmi @Y
	lda r11L
	cmp r11H
	bne @Y
	lda #$FF
	plp
	bcs @X
	lda #0
@X:	jmp _HorizontalLine
@Y:
	LoadB r7H, 0
	lda r11H
	sub r11L
	sta r7L
	bcs @1
	lda #0
	sub r7L
	sta r7L
@1:	lda r4L
	sub r3L
	sta r12L
	lda r4H
	sbc r3H
	sta r12H
	ldx #r12
	jsr _Dabs
	CmpW r12, r7
	bcs @2
	jmp @9
@2:
	lda r7L
	asl
	sta r9L
	lda r7H
	rol
	sta r9H
	lda r9L
	sub r12L
	sta r8L
	lda r9H
	sbc r12H
	sta r8H
	lda r7L
	sub r12L
	sta r10L
	lda r7H
	sbc r12H
	sta r10H
	asl r10L
	rol r10H
	LoadB r13L, $ff
	CmpW r3, r4
	bcc @4
	CmpB r11L, r11H
	bcc @3
	LoadB r13L, 1
@3:	ldy r3H
	ldx r3L
	MoveW r4, r3
	sty r4H
	stx r4L
	MoveB r11H, r11L
	bra @5
@4:	ldy r11H
	cpy r11L
	bcc @5
	LoadB r13L, 1
@5:	plp
	php
	jsr _DrawPoint
	CmpW r3, r4
	bcs @8
	inc r3L
	bne @6
	inc r3H
@6:	bbrf 7, r8H, @7
	AddW r9, r8
	bra @5
@7:	AddB_ r13L, r11L
	AddW r10, r8
	bra @5
@8:	plp
	rts
@9:	lda r12L
	asl
	sta r9L
	lda r12H
	rol
	sta r9H
	lda r9L
	sub r7L
	sta r8L
	lda r9H
	sbc r7H
	sta r8H
	lda r12L
	sub r7L
	sta r10L
	lda r12H
	sbc r7H
	sta r10H
	asl r10L
	rol r10H
	LoadW r13, $ffff
	CmpB r11L, r11H
	bcc @B
	CmpW r3, r4
	bcc @A
	LoadW r13, 1
@A:	MoveW r4, r3
	ldx r11L
	lda r11H
	sta r11L
	stx r11H
	bra @C
@B:	CmpW r3, r4
	bcs @C
	LoadW r13, 1
@C:	plp
	php
	jsr _DrawPoint
	CmpB r11L, r11H
	bcs @E
	inc r11L
	bbrf 7, r8H, @D
	AddW r9, r8
	bra @C
@D:	AddW r13, r3
	AddW r10, r8
	bra @C
@E:	plp
	rts

;---------------------------------------------------------------
; DrawPoint                                               $C133
;
; Pass:      same as DrawLine with no 2nd point
; Return:    point is drawn or recovered
; Destroyed: a, x, y, r5 - r6
;---------------------------------------------------------------
_DrawPoint:
	php
	plp
	tax			; preserve flags in X
	PushW r3		; r3 must not be changed
	stx r3L			; preserve flags in r3L
	ldx r11L
	jsr _GetScanLine_BR
	jsr AdjustR5R6ToX
	php
	sei
	START_IO
	jsr SetupBeamRacerRAMr5r6	; r5/r6 copied to PORT0/1 addresses, STEP0/1 set to 0
	PushB r3L		; restore flags from r3L
	lda BitMaskPow2Rev,x
	plp			; restore flags
	bmi @4
	bcc @2
	ora VREG_PORT1
	bra @3
@2:	eor #$ff
	and VREG_PORT1
@3:	sta VREG_PORT1
	sta VREG_PORT0
	bra @5
@4:	pha
	eor #$ff
	and VREG_PORT0
	sta VREG_PORT0
	pla
	and VREG_PORT1
	ora VREG_PORT0
	sta VREG_PORT0
@5:

	rmbf CONTROL_PORT_READ_ENABLE_BIT, VREG_CONTROL ; clear to avoid 'weird issues'
	END_IO
	plp
	PopW r3
	rts

;---------------------------------------------------------------
; TestPoint                                               $C13F
;
; Pass:      a    pattern
;            r3   x position of pixel (0-319)
;            r11L y position of pixel (0-199)
; Return:    carry set if bit is set
; Destroyed: a, x, y, r5, r6
;---------------------------------------------------------------
_TestPoint:
	PushW r3
	ldx r11L
	jsr _GetScanLine_BR
	jsr AdjustR5R6ToX
	php
	sei
	START_IO
	jsr SetupBeamRacerRAMr5r6
	lda VREG_PORT1
	sta @mask1
	rmbf CONTROL_PORT_READ_ENABLE_BIT, VREG_CONTROL ; clear to avoid 'weird issues'
	END_IO
	plp
	PopW r3
	lda BitMaskPow2Rev,x
@mask1 = *+1
	and #0
	beq @2
	sec
	rts
@2:	clc
	rts


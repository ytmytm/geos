;
; Graphics library: GetScanLine syscall
;
; Beamracer version
; Maciej 'YTM/Elysium' Witkowiak

.include "const.inc"
.include "geossym.inc"
.include "geosmac.inc"
.include "config.inc"
.include "kernal.inc"
.include "c64.inc"

.global _GetScanLine_BR

.segment "graph2n"

;---------------------------------------------------------------
; GetScanLine                                             $C13C
;
; Function:  Returns the address of the beginning of a scanline

; Pass:      x   scanline nbr
; Return:    r5  add of 1st byte of foreground scr
;            r6  add of 1st byte of background scr
; Destroyed: a
;---------------------------------------------------------------
_GetScanLine_BR:
	txa
	pha
	sta r5L
	LoadB r5H, 0
	asl r5L
	rol r5H				; *2
	asl r5L
	rol r5H				; *4
	txa
	add r5L
	sta r5L
	bcc :+
	inc r5H				; *4+1 -> *5
:	asl r5L
	rol r5H				; *10
	asl r5L
	rol r5H				; *20
	asl r5L
	rol r5H				; *40

	MoveW r5, r6

	bbrf 7, dispBufferOn, @4	; !ST_WR_FORE
	bvs @3				; ST_WR_FORE | ST_WR_BACK
	AddVW br_screen_base, r5	; ST_WR_FORE
@1:	MoveW r5, r6
@2:	pla
	tax
	rts

@3:	AddVW br_screen_base, r5	; ST_WR_FORE | ST_WR_BACK
	AddVW br_backscreen_base, r6
	bra @2

@4:	bvc @5				; ST_WR_BACK
	AddVW br_backscreen_base, r5
	bra @1

@5:	;AddVW br_screen_base+$0F00, r5	; !ST_WR_FORE && !ST_WR_BACK ?!
	;bra @1
.import Panic
	jmp Panic

.global _GetScanLine

.segment "graph2n"

;---------------------------------------------------------------
; GetScanLine                                             $C13C
;
; Function:  Returns the address of the beginning of a scanline

; Pass:      x   scanline nbr
; Return:    r5  add of 1st byte of foreground scr
;            r6  add of 1st byte of background scr
; Destroyed: a
;---------------------------------------------------------------
_GetScanLine:
	txa
	pha
	pha
	and #%00000111
	sta r6H
	pla
	lsr
	lsr
	lsr
	tax
	lda LineTabL,x
	ora r6H
	sta r5L
	sta r6L
	lda LineTabH,x
	sta r5H
	sta r6H
	pla
	tax
	rts

.segment "graph2o"

.define LineTab SCREEN_BASE+0*320, SCREEN_BASE+1*320, SCREEN_BASE+2*320, SCREEN_BASE+3*320, SCREEN_BASE+4*320, SCREEN_BASE+5*320, SCREEN_BASE+6*320, SCREEN_BASE+7*320, SCREEN_BASE+8*320, SCREEN_BASE+9*320, SCREEN_BASE+10*320, SCREEN_BASE+11*320, SCREEN_BASE+12*320, SCREEN_BASE+13*320, SCREEN_BASE+14*320, SCREEN_BASE+15*320, SCREEN_BASE+16*320, SCREEN_BASE+17*320, SCREEN_BASE+18*320, SCREEN_BASE+19*320, SCREEN_BASE+20*320, SCREEN_BASE+21*320, SCREEN_BASE+22*320, SCREEN_BASE+23*320, SCREEN_BASE+24*320
LineTabL:
	.lobytes LineTab
LineTabH:
	.hibytes LineTab

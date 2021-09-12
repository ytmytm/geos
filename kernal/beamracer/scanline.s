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

.import Panic

.global _GetScanLine_BR
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
	; dummy, point to foreground in case Application would draw something directly
	LoadW r5, SCREEN_BASE
	LoadW r6, SCREEN_BASE
	rts

	; this is used by all Kernal functions
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
	jmp Panic

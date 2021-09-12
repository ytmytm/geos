;
;
; Beamracer support for GEOS
; Maciej 'YTM/Elysium' Witkowiak
;
;

; first visible line
FIRST_LINE = 51
; first visible column
FIRST_COL = 15
; number of rows (1-25) to overlay
LAST_LINE = 12*8

.include "const.inc"
.include "geossym.inc"
.include "geosmac.inc"
.include "config.inc"
.include "c64.inc"

.include "kernal/beamracer/beamracer-vlib/vasyl.s"

.import __VASYL_LOAD__
.import __VASYL_SIZE__
.import __VASYL_RUN__

.global br_dlist_activate
.global dl_start_init
.global dl_start

        .segment "start"
        ; this will be called after BR presence check so we don't need to knock-knock again
br_dlist_activate:
	START_IO


	; clear part of the screen
	LoadW r0, 0
	LoadW r1, 6*8*40
	lda VREG_CONTROL
	and #%11111000
	ora #br_screen_bank
	sta VREG_CONTROL
	LoadB VREG_ADR0, <br_screen_base
	LoadB VREG_ADR0+1, >br_screen_base
	LoadB VREG_STEP0, 1

:	lda #$55
	sta VREG_PORT0
	IncW r0
	lda #$aa
	sta VREG_PORT0
	IncW r0
	CmpW r0, r1
	bne :-

        ; copy dlist to local RAM, this will also set the bank to the same as display list
	LoadW r0, __VASYL_LOAD__
	LoadW r1, __VASYL_LOAD__+__VASYL_SIZE__
	; local RAM bank & address setup
	lda VREG_CONTROL
	and #%11111000
	ora #br_dlist_bank
	sta VREG_CONTROL
	LoadB VREG_ADR0,   <__VASYL_RUN__
	LoadB VREG_ADR0+1, >__VASYL_RUN__
	LoadB VREG_STEP0, 1

	; copy display lists
	ldy #0
:	lda (r0),y
	sta VREG_PORT0
	IncW r0
	CmpW r0, r1
	bne :-

	; activate dlist
	; bank comes from VREG_CONTROL?
	LoadB VREG_DLISTL, <dl_start_init	; not a LoadW because we want to be sure about write order: lo, then hi byte
	LoadB VREG_DLISTH, >dl_start_init
	smbf CONTROL_DLIST_ON_BIT, VREG_CONTROL
	END_IO
	rts

        .segment "VASYL"

        .res br_dlist_base

dl_start_init:
	; we want to have port access independent of display list
	; hence we switch to 2nd display list immediately
	MOV	VREG_DLIST2L, <dl_start
	MOV	VREG_DLIST2H, >dl_start
	; start display list #0 from display list bank (the same as this one, but independent of ports)
	MOV	VREG_DL2STROBE, %00001000+br_dlist_bank
	END

dl_start:
	; wait until display starts
	WAIT	FIRST_LINE, 0
	; start overlay at first column
	MOV	VREG_PBS_CYCLE_START, FIRST_COL
	; finish overlay at the last column (40 cycles further)
	MOV	VREG_PBS_CYCLE_STOP, 40 + FIRST_COL
	; - fetch bytes from successive memory addresses (write in order: lo, then hi byte)
	MOV	VREG_PBS_STEPL, 1
	MOV	VREG_PBS_STEPH, 0
	; - when end-of-line reached, continue to the next byte (no padding) (write in order: lo, then hi byte)
	MOV	VREG_PBS_PADDINGL, 0
	MOV	VREG_PBS_PADDINGH, 0
	; - now turn on sequencer
	MOV	VREG_PBS_CONTROL, (1 << PBS_CONTROL_ACTIVE_BIT) + br_screen_bank
	; - finally set the startaddress (write in order: lo, then hi byte)
	; It's important that the address is set at the very end - otherwise it could
	; be affected by not-yet-ready values of other sequencer registers.
	MOV	VREG_PBS_BASEL, <br_screen_base
	MOV	VREG_PBS_BASEH, >br_screen_base
	MOV	$20, 0  ; debug: border color marker that we're active

	DELAYV	LAST_LINE ; Wait until the entire image has been drawn.

	MOV	$20, 6  ; debug: border color marker that we're no longer active
	MOV	VREG_PBS_CONTROL, 0           ; turn off the sequencer (seems necessary even for whole screen overlay)

	END

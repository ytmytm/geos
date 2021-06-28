; GEOS KERNAL by Berkeley Softworks
;
; C64 RAM expansion support by Maciej Witkowiak

.include "const.inc"
.include "geossym.inc"
.include "geosmac.inc"
.include "config.inc"
.include "kernal.inc"
.include "c64.inc"

.ifdef useRamExp
.import OpenRecordFile
.import ReadRecord
.import PointRecord
.import NewDisk
.import FindFile
.import GetFHdrInfo
.import ToBASIC
.import DoDlgBox
.import _EnterDT_DB
.import DeskTopName
.import RamExpGetStat
.import RamExpPutStat
.import RamExpWrite
.import DeskTopOpen
.import DeskTopStart
.import DeskTopExec
.import DeskTopLgh
.endif

.ifdef useRamExp
.global LoadDeskTop
.global DetectRamExp
.ifdef useBeamRacerRam
.import br_dlist_activate
.global DoClearCacheRamExp
.endif
.endif

.segment "ramexp1"

.ifdef usePlus60K
DetectRamExp:
ASSERT_NOT_BELOW_IO
	LoadB CPU_DATA, IO_IN
	ldx #0
BootP6K_1:
	lda Plus60KTest,x
	sta $0400,x
	inx
	cpx #Plus60KTestEnd-Plus60KTest
	bne BootP6K_1
	jsr $0400
	bne BootP6K_OK
	LoadB CPU_DATA, RAM_64K
ASSERT_NOT_BELOW_IO
	LoadW r0, ExpFaultDB
	jsr DoDlgBox
	jmp ToBASIC
BootP6K_OK:
	LoadB CPU_DATA, RAM_64K
ASSERT_NOT_BELOW_IO
	rts

Plus60KTest:
	ldx #0
	stx PLUS60K_CR
	ldy $1180
	LoadB $1180, 'M'
	LoadB PLUS60K_CR, $80
	lda $1180
	stx PLUS60K_CR
	sty $1180
	cmp #'M'
	rts
Plus60KTestEnd:
.endif

.if .defined(useRamCart64) || .defined(useRamCart128)
DetectRamExp:
	LoadB CPU_DATA, IO_IN
	LoadW RAMC_BASE, 0
	ldx RAMC_WINDOW
	ldy RAMC_WINDOW+$80
	lda #'M'
	sta RAMC_WINDOW
	lda #'W'
	sta RAMC_WINDOW+$80
	cmp RAMC_WINDOW+$80
	bne @1
	lda RAMC_WINDOW
	cmp #'M'
	bne @1
	stx RAMC_WINDOW
	sty RAMC_WINDOW
	jmp @2
@1:	LoadB CPU_DATA, RAM_64K
ASSERT_NOT_BELOW_IO
	LoadW r0, ExpFaultDB
	jsr DoDlgBox
	jmp ToBASIC
@2:	LoadB CPU_DATA, RAM_64K
ASSERT_NOT_BELOW_IO
	rts
.endif

.if .defined(useBeamRacerRam)
.include "kernal/beamracer/beamracer-vlib/vasyl.s"
DetectRamExp:
	START_IO
	; knock_knock and detection routine from vlib.s
	; https://github.com/madhackerslab/beamracer-vlib/blob/09eb0dfd65b9ca3aa30cb98c35499b1dcea1bfd8/vlib.s
	ldx #255
	cpx VREG_CONTROL
	bne @active
	lda #$42
	sta VREG_CONTROL
	lda #$52
	sta VREG_CONTROL
	cpx VREG_CONTROL
	bne @active
	; inactive
	END_IO
	LoadW r0, ExpFaultDB
	jsr DoDlgBox
	jmp ToBASIC
@active:
	END_IO
	; we can start the display list
	jsr br_dlist_activate
	; anything else?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.import HorizontalLine
        LoadW r3, 0
        LoadW r4, 160
        LoadB r11L, 0
        lda #%11011101
        jsr HorizontalLine
        inc r11L
        AddVB 15, r3
        AddVB 20, r4
        lda #%11011101
        jsr HorizontalLine
        inc r11L
        inc r11L
        AddVB 15, r3
        AddVB 20, r4
        lda #%11011101
        jsr HorizontalLine
        inc r11L
        inc r11L
        inc r11L
        AddVB 15, r3
        AddVB 20, r4
        lda #%11011101
        jsr HorizontalLine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	rts

; copied from drv1541
DoClearCacheRamExp:
	; clear 683 pages
	; cache tests if first two bytes are both 0, this works as 1541 initializes sectors with $00, $ff in first two bytes
	ldy curDrive
	lda _ramBase, y	; bank number
	sta r1H
	LoadB r1L, 0	; start page
	MoveW r1, r3
	AddVW 683, r3	; end condition - start+sector count
	; we're clear of IO
	START_IO
	; XXX disable display list, we will switch bank
	rmbf CONTROL_DLIST_ON_BIT, VREG_CONTROL
	LoadB VREG_ADR0, 0
	LoadB VREG_STEP0, 1
	ldy #0
@1:	lda VREG_CONTROL
	and #%11111000
	ora r1H
	sta VREG_CONTROL
	MoveB r1L, VREG_ADR0+1
	sty VREG_PORT0
	sty VREG_PORT0 ; we could do LoadB VREG_REP0, 255 but we don't need all that, we only clear sector link
;	LoadB VREG_REP0, 255
;@11:	lda VREG_REP0
;	bne @11
	IncW r1
	CmpW r1, r3
	bne @1
	; XXX restore display list bank and enable display list
	lda VREG_CONTROL
	and #%11111000
	ora #br_dlist_bank
	sta VREG_CONTROL
	smbf CONTROL_DLIST_ON_BIT, VREG_CONTROL
	END_IO
	rts
.endif

.ifdef useRamExp
ExpFaultDB:
	.byte DEF_DB_POS | 1
	.byte DBTXTSTR, TXT_LN_X, TXT_LN_1_Y
	.word ExpFaultStr
	.byte DBTXTSTR, TXT_LN_X, TXT_LN_2_Y
	.word ExpFaultStr2
	.byte OK, DBI_X_1, DBI_Y_2
	.byte NULL

ExpFaultStr:
	.byte BOLDON
	.byte "This version of GEOS works", 0
ExpFaultStr2:
.if .defined(useRamCart64) || .defined(useRamCart128)
	.byte "only with a RamCart expansion.", 0
.endif
.ifdef usePlus60K
	.byte "only with a +60K expansion.", 0
.endif
.ifdef useBeamRacerRam
	.byte "only with Beamracer.", 0
.endif
.endif

.ifdef useRamExp
LoadDeskTop:
	LoadB a0L, 0
.ifdef useBeamRacerRam
	LoadB BVChainTab, 3	;3 - first free page (0 - stat, 1+2 - dlgbox buf)
.else
	LoadB BVChainTab, 1	;1 - first free page
.endif
LoadDTLp:
	LoadW r6, DeskTopName
	jsr FindFile
	beqx LoadDTCont
	LoadW r0, _EnterDT_DB
	jsr DoDlgBox
	jsr NewDisk
	jmp LoadDTLp
LoadDTCont:
	MoveW r5, r9
	jsr GetFHdrInfo
	MoveW fileHeader+O_GHST_ADDR, DeskTopStart
	MoveW fileHeader+O_GHST_VEC, DeskTopExec

	LoadB DeskTopOpen, $88
	LoadW r0, DeskTopName
	jsr OpenRecordFile

BLoadDTop:
	lda a0L
	jsr PointRecord
	bnex BVLast
	LoadW r2, $ffff
	LoadW r7, BVBuff
	jsr ReadRecord

	lda r7H
	subv >BVBuff
	tay
	ldx a0L
	bne BLoadDTop_1
	sty DeskTopLgh
BLoadDTop_1:
	clc
	adc BVChainTab,x
	adc #1
	sta BVChainTab+1,x
	inc a0L

	LoadB r1H, 0
	lda BVChainTab,x
	sta r1L
	LoadW r0, BVBuff
	sty r2H
	jsr RamExpWrite

ASSERT_NOT_BELOW_IO
	LoadB CPU_DATA, IO_IN
	inc $d020
	LoadB CPU_DATA, RAM_64K
ASSERT_NOT_BELOW_IO
	jmp BLoadDTop

BVLast:	jsr RamExpGetStat
	MoveB a0L, diskBlkBuf+DTOP_CHNUM
	inc a0L
	ldx #0
BVLast_1:
	lda BVChainTab,x
	sta diskBlkBuf+DTOP_CHAIN,x
	inx
	cpx a0L
	bne BVLast_1
	sta diskBlkBuf+RAM_EXP_1STFREE
	LoadB DeskTopOpen, 0
	jmp RamExpPutStat

BVChainTab:
	; this is a buffer, but what's its size?
.endif


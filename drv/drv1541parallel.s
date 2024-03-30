; GEOS by Berkeley Softworks
; reverse engineered by Maciej Witkowiak, Michael Steil
;
; Commodore 1541 disk driver with parallel cable by Maciej Witkowiak
; NOTE: OPTIMAL_INTERLEAVE has to be determined experimentally
; NOTE2: if drive code jams - check if 'jmp (DExecAddy)' is on page boundary - it's dangerously close

.include "const.inc"
.include "geossym.inc"
.include "geosmac.inc"
.include "config.inc"
.include "kernal.inc"
.include "jumptab.inc"
.include "c64.inc"

.import __drv1541_drivecode_RUN__
.import __drv1541_drivecode_SIZE__

OPTIMAL_INTERLEAVE = 6 ; 6 gives a 22% speedup

.segment "drv1541"

_InitForIO:
	.word __InitForIO
_DoneWithIO:
	.word __DoneWithIO
_ExitTurbo:
	.word __ExitTurbo
_PurgeTurbo:
	.word __PurgeTurbo
_EnterTurbo:
	.word __EnterTurbo
_ChangeDiskDevice:
	.word __ChangeDiskDevice
_NewDisk:
	.word __NewDisk
_ReadBlock:
	.word __ReadBlock
_WriteBlock:
	.word __WriteBlock
_VerWriteBlock:
	.word __VerWriteBlock
_OpenDisk:
	.word __OpenDisk
_GetBlock:
	.word __GetBlock
_PutBlock:
	.word __PutBlock
_GetDirHead:
	.word __GetDirHead
_PutDirHead:
	.word __PutDirHead
_GetFreeDirBlk:
	.word __GetFreeDirBlk
_CalcBlksFree:
	.word __CalcBlksFree
_FreeBlock:
	.word __FreeBlock
_SetNextFree:
	.word __SetNextFree
_FindBAMBit:
	.word __FindBAMBit
_NxtBlkAlloc:
	.word __NxtBlkAlloc
_BlkAlloc:
	.word __BlkAlloc
_ChkDkGEOS:
	.word __ChkDkGEOS
_SetGEOSDisk:
	.word __SetGEOSDisk

Get1stDirEntry:
	jmp _Get1stDirEntry
GetNxtDirEntry:
	jmp _GetNxtDirEntry
GetBorder:
	jmp _GetBorder
AddDirBlock:
	jmp _AddDirBlock
ReadBuff:
	jmp _ReadBuff
WriteBuff:
	jmp _WriteBuff
	jmp DUNK4_2
	jmp GetDOSError
AllocateBlock:
	jmp _AllocateBlock
ReadLink:
	jmp _ReadLink

__GetDirHead:
	jsr SetDirHead
	bne __GetBlock
_ReadBuff:
	LoadW r4, diskBlkBuf
__GetBlock:
	jsr EnterTurbo
	bnex GetBlk0
	jsr InitForIO
	jsr ReadBlock
	jsr DoneWithIO
GetBlk0:
	rts

__PutDirHead:
	jsr SetDirHead
	bne __PutBlock
_WriteBuff:
	LoadW r4, diskBlkBuf
__PutBlock:
	jsr EnterTurbo
	bnex PutBlk1
	jsr InitForIO
	jsr WriteBlock
	bnex PutBlk0
	jsr VerWriteBlock
PutBlk0:
	jsr DoneWithIO
PutBlk1:
	rts

SetDirHead:
	LoadB r1L, DIR_TRACK
	LoadB r1H, 0
	sta r4L
	LoadB r4H, (>curDirHead)
	rts

CheckParams:
	bbrf 6, curType, CheckParams_1
	jsr DoCacheVerify
	beq CheckParams_2
CheckParams_1:
	lda #0
	sta errCount
	ldx #INV_TRACK
	lda r1L
	beq CheckParams_2
	cmp #N_TRACKS+1
	bcs CheckParams_2
	sec
	rts
CheckParams_2:
	clc
	rts

__OpenDisk:
	ldy curDrive
	lda _driveType,y
	sta tmpDriveType
	and #%10111111
	sta _driveType,y
	jsr NewDisk
	bnex OpenDsk1
	jsr GetDirHead
	bnex OpenDsk1
	bbrf 6, tmpDriveType, OpenDsk0
	jsr DoCacheVerify
	beq OpenDsk0
	jsr DoClearCache
	jsr SetDirHead
	jsr DoCacheWrite
OpenDsk0:
	LoadW r5, curDirHead
	jsr ChkDkGEOS
	LoadW r4, curDirHead+OFF_DISK_NAME
	ldx #r5
	jsr GetPtrCurDkNm
	ldx #r4
	ldy #r5
	lda #18
	jsr CopyFString
	ldx #0
OpenDsk1:
	lda tmpDriveType
	ldy curDrive
	sta _driveType,y
	rts
tmpDriveType:
	.byte 0

__BlkAlloc:
	ldy #1
	sty r3L
	dey
	sty r3H
__NxtBlkAlloc:
	PushW r9
	PushW r3
	LoadW r3, $00fe
	ldx #r2
	ldy #r3
	jsr Ddiv
	lda r8L
	beq BlkAlc0
	inc r2L
	bne BlkAlc0
	inc r2H
BlkAlc0:
	LoadW r5, curDirHead
	jsr CalcBlksFree
	PopW r3
	ldx #INSUFF_SPACE
	CmpW r2, r4
	beq BlkAlc1
	bcs BlkAlc4
BlkAlc1:
	MoveW r6, r4
	MoveW r2, r5
BlkAlc2:
	jsr SetNextFree
	bnex BlkAlc4
	ldy #0
	lda r3L
	sta (r4),y
	iny
	lda r3H
	sta (r4),y
	AddVW 2, r4
	lda r5L
	bne @X
	dec r5H
@X:	dec r5L
	lda r5L
	ora r5H
	bne BlkAlc2
	ldy #0
	tya
	sta (r4),y
	iny
	lda r8L
	bne BlkAlc3
	lda #$fe
BlkAlc3:
	clc
	adc #1
	sta (r4),y
	ldx #0
BlkAlc4:
	PopW r9
	rts

_Get1stDirEntry:
	LoadB r1L, DIR_TRACK
	LoadB r1H, 1
	jsr ReadBuff
	LoadW r5, diskBlkBuf+FRST_FILE_ENTRY
	lda #0
	sta borderFlag
	rts

_GetNxtDirEntry:
	ldx #0
	ldy #0
	AddVW $20, r5
	CmpWI r5, diskBlkBuf+$ff
	bcc GNDirEntry1
	ldy #$ff
	MoveW diskBlkBuf, r1
	bne GNDirEntry0
	lda borderFlag
	bne GNDirEntry1
	lda #$ff
	sta borderFlag
	jsr GetBorder
	bnex GNDirEntry1
	tya
	bne GNDirEntry1
GNDirEntry0:
	jsr ReadBuff
	ldy #0
	LoadW r5, diskBlkBuf+FRST_FILE_ENTRY
GNDirEntry1:
	rts

_GetBorder:
	jsr GetDirHead
	bnex GetBord2
	LoadW r5, curDirHead
	jsr ChkDkGEOS
	bne GetBord0
	ldy #$ff
	bne GetBord1
GetBord0:
	MoveW curDirHead+OFF_OP_TR_SC, r1
	ldy #0
GetBord1:
	ldx #0
GetBord2:
	rts

__ChkDkGEOS:
	ldy #OFF_GS_ID
	ldx #0
	LoadB isGEOS, 0
ChkDkG0:
	lda (r5),y
	cmp GEOSDiskID,x
	bne ChkDkG1
	iny
	inx
	cpx #11
	bne ChkDkG0
	LoadB isGEOS, $ff
ChkDkG1:
	lda isGEOS
	rts

GEOSDiskID:
	.byte "GEOS format V1.0",NULL

__GetFreeDirBlk:
	php
	sei
	PushB r6L
	PushW r2
	ldx r10L
	inx
	stx r6L
	LoadB r1L, DIR_TRACK
	LoadB r1H, 1
GFDirBlk0:
	jsr ReadBuff
GFDirBlk1:
	bnex GFDirBlk5
	dec r6L
	beq GFDirBlk3
GFDirBlk11:
	lda diskBlkBuf
	bne GFDirBlk2
	jsr AddDirBlock
	bra GFDirBlk1
GFDirBlk2:
	sta r1L
	MoveB diskBlkBuf+1, r1H
	bra GFDirBlk0
GFDirBlk3:
	ldy #FRST_FILE_ENTRY
	ldx #0
GFDirBlk4:
	lda diskBlkBuf,y
	beq GFDirBlk5
	tya
	addv $20
	tay
	bcc GFDirBlk4
	LoadB r6L, 1
	ldx #FULL_DIRECTORY
	ldy r10L
	iny
	sty r10L
	cpy #$12
	bcc GFDirBlk11
GFDirBlk5:
	PopW r2
	PopB r6L
	plp
	rts

_AddDirBlock:
	PushW r6
	ldy #$48
	ldx #FULL_DIRECTORY
	lda curDirHead,y
	beq ADirBlk0
	MoveW r1, r3
	jsr SetNextFree
	MoveW r3, diskBlkBuf
	jsr WriteBuff
	bnex ADirBlk0
	MoveW r3, r1
	jsr ClearAndWrite
ADirBlk0:
	PopW r6
	rts

ClearAndWrite:
	lda #0
	tay
CAndWr0:
	sta diskBlkBuf,y
	iny
	bne CAndWr0
	dey
	sty diskBlkBuf+1
	jmp WriteBuff

__SetNextFree:
	lda r3H
	add interleave
	sta r6H
	MoveB r3L, r6L
	cmp #25
	bcc SNxtFree0
	dec r6H
SNxtFree0:
	cmp #DIR_TRACK
	beq SNxtFree1
SNxtFree00:
	lda r6L
	cmp #DIR_TRACK
	beq SNxtFree3
SNxtFree1:
	asl
	asl
	tax
	lda curDirHead,x
	beq SNxtFree3
	lda r6L
	jsr SNxtFreeHelp
	lda SecScTab,x
	sta r7L
	tay
SNxtFree2:
	jsr SNxtFreeHelp2
	beq SNxtFree4
	inc r6H
	dey
	bne SNxtFree2
SNxtFree3:
	inc r6L
	CmpBI r6L, N_TRACKS+1
	bcs SNxtFree5
	sub r3L
	sta r6H
	asl
	adc #4
	adc interleave
	sta r6H
	bra SNxtFree00
SNxtFree4:
	MoveW_ r6, r3
	ldx #0
	rts
SNxtFree5:
	ldx #INSUFF_SPACE
	rts

SNxtFreeHelp:
	ldx #0
SNFHlp0:
	cmp SecTrTab,x
	bcc SNFHlp1
	inx
	bne SNFHlp0
SNFHlp1:
	rts

SecTrTab:
	.byte 18, 25, 31, 36
SecScTab:
	.byte 21, 19, 18, 17

SNxtFreeHelp2:
	lda r6H
SNFHlp2_1:
	cmp r7L
	bcc SNFHlp2_2
	sub r7L
	bra SNFHlp2_1
SNFHlp2_2:
	sta r6H

_AllocateBlock:
	jsr FindBAMBit
	beq SNFHlp2_3
	lda r8H
	eor #$ff
	and curDirHead,x
	sta curDirHead,x
	ldx r7H
	dec curDirHead,x
	ldx #0
	rts
SNFHlp2_3:
	ldx #BAD_BAM
	rts

__FindBAMBit:
	lda r6L
	asl
	asl
	sta r7H
	lda r6H
	and #%00000111
	tax
	lda FBBBitTab,x
	sta r8H
	lda r6H
	lsr
	lsr
	lsr
	sec
	adc r7H
	tax
	lda curDirHead,x
	and r8H
	rts

FBBBitTab:
	.byte $01, $02, $04, $08
	.byte $10, $20, $40, $80

__FreeBlock:
	jsr FindBAMBit
	bne FreeBlk0
	lda r8H
	eor curDirHead,x
	sta curDirHead,x
	ldx r7H
	inc curDirHead,x
	ldx #0
	rts
FreeBlk0:
	ldx #BAD_BAM
	rts

__CalcBlksFree:
	LoadW_ r4, 0
	ldy #OFF_TO_BAM
CBlksFre0:
	lda (r5),y
	add r4L
	sta r4L
	bcc CBlksFre1
	inc r4H
CBlksFre1:
	tya
	clc
	adc #4
	tay
	cpy #$48
	beq CBlksFre1
	cpy #$90
	bne CBlksFre0
	LoadW r3, $0298
	rts

__SetGEOSDisk:
	jsr GetDirHead
	bnex SetGDisk2
	LoadW r5, curDirHead
	jsr CalcBlksFree
	ldx #INSUFF_SPACE
	lda r4L
	ora r4H
	beq SetGDisk2
	LoadB r3L, DIR_TRACK+1
	LoadB r3H, 0
	jsr SetNextFree
	beqx SetGDisk0
	LoadB r3L, 1
	jsr SetNextFree
	bnex SetGDisk2
SetGDisk0:
	MoveW r3, r1
	jsr ClearAndWrite
	bnex SetGDisk2
	MoveW r1, curDirHead+OFF_OP_TR_SC
	ldy #OFF_GS_ID+15
	ldx #15
SetGDisk1:
	lda GEOSDiskID,x
	sta curDirHead,y
	dey
	dex
	bpl SetGDisk1
	jsr PutDirHead
SetGDisk2:
	rts

__InitForIO:
	php
	PopB tmpPS
	sei
	MoveB CPU_DATA, tmpCPU_DATA
.ifndef bsw128
	LoadB CPU_DATA, KRNL_IO_IN
.endif
	MoveB grirqen, tmpgrirqen
	MoveB clkreg, tmpclkreg
	ldy #0
	sty clkreg
	sty grirqen
	lda #%01111111
	sta grirq
	sta cia1base+13
	sta cia2base+13
	lda #>D_IRQHandler
	sta irqvec+1
.ifdef bsw128
	sta nmivec+1
.endif
	lda #<D_IRQHandler
	sta irqvec
.ifndef bsw128
	lda #>D_NMIHandler
	sta nmivec+1
	lda #<D_NMIHandler
.endif
	sta nmivec
	LoadB cia2base+2, %00111111
	MoveB mobenble, tmpmobenble
	sty mobenble
	sty cia2base+5
	iny
	sty cia2base+4
	LoadB cia2base+13, %10000001
	LoadB cia2base+14, %00001001
	LoadB cia2base+3, $00	; user port input
	lda cia2base+13		; clear /FLAG
	rts

D_IRQHandler:
.ifdef bsw128
	PopB $ff00
.endif
	pla
	tay
	pla
	tax
	pla
D_NMIHandler:
	rti

__DoneWithIO:
	sei
	MoveB tmpclkreg, clkreg
	MoveB tmpmobenble, mobenble
	LoadB cia2base+13, %01111111
	lda cia2base+13
	MoveB tmpgrirqen, grirqen
.ifndef bsw128
	MoveB tmpCPU_DATA, CPU_DATA
.endif
	PushB tmpPS
	plp
	rts

Hst_RecvByte:
	MoveW z8b, RecvAddr
:	nop			; delay adjusted to Drv_SendByte timing
	nop			; so that branch after wait for flag is mostly not taken
	nop			; (4xNOP)=28 cycles per byte loop
	nop
:	lda cia2base+13		; wait for flag
	beq :-
	lda cia2base+1		; read data
	dey
RecvAddr = *+1
	sta $8000,y
	bne :--
	rts

Hst_SendByte:
	tya
	pha
	ldy #$ff
	sty cia2base+3			; port B output
	iny
	jsr Hst_SendByte_01
	pla
	tay
	LoadB cia2base+3, $ff		; port B output
Hst_SendByte_0:
	dey
	lda (z8b),y
Hst_SendByte_01:
	sta cia2base+1			; write
:	lda cia2base+13			; wait for handshake
	beq :-
	cpy #0
	bne Hst_SendByte_0
	LoadB cia2base+3, $00		; port B input
	rts

SendDOSCmd:
	stx z8c
	sta z8b
	LoadB STATUS, 0
	lda curDrive
	jsr $ffb1
	bbsf 7, STATUS, SndDOSCmd1
	lda #$ff
	jsr $ff93
	bbsf 7, STATUS, SndDOSCmd1
	ldy #0
SndDOSCmd0:
	lda (z8b),y
	jsr $ffa8
	iny
	cpy #5
	bcc SndDOSCmd0
	ldx #0
	rts
SndDOSCmd1:
	jsr $ffae
	ldx #DEV_NOT_FOUND
	rts

DUNK4:					; A/X addr of procedure to run, without parameters
	stx z8c
	sta z8b
	ldy #2				; 2 bytes to send
	bne DUNK4_3
DUNK4_1:				; A/X addr of procedure to run, r1 t&s parameters
	stx z8c
	sta z8b
DUNK4_2:				; called by NewDisk, but why send r1?
	ldy #4				; 4 bytes to send
	MoveW r1, DTrkSec
DUNK4_3:
	MoveW z8b, DExeProc
	LoadW z8b, DExeProc
	jmp Hst_SendByte

DUNK5:					; receive 2 bytes, but ignorethe first(?)
	ldy #1
	jsr Hst_RecvByte
	pha
	tay
	jsr Hst_RecvByte
	pla
	tay
	rts

__EnterTurbo:
	LoadB interleave, OPTIMAL_INTERLEAVE
	lda curDrive
	jsr SetDevice
	ldx curDrive
	lda _turboFlags,x
	bmi EntTur0
	jsr SendCODE
	bnex EntTur5
	ldx curDrive
	lda #%10000000
	sta _turboFlags,x
EntTur0:
	and #%01000000
	bne EntTur3
	jsr InitForIO
	ldx #>EnterCommand
	lda #<EnterCommand
	jsr SendDOSCmd
	bnex EntTur4
	jsr $ffae
:	lda cia2base+13			; wait for initial sync
	beq :-
	sei
	jsr DoneWithIO
	ldx curDrive
	lda _turboFlags,x
	ora #%01000000
	sta _turboFlags,x
EntTur3:
	ldx #0
	beq EntTur5
EntTur4:
	jsr DoneWithIO
EntTur5:
	rts

EnterCommand:
	.byte "M-E"
	.word DriveStart

SendExitTurbo:
	jsr InitForIO
	ldx #>Drv_ExitTurbo
	lda #<Drv_ExitTurbo
	jsr DUNK4
	lda curDrive
	jsr $ffb1
	lda #$ef
	jsr $ff93
	jsr $ffae
	jmp DoneWithIO

SendCODE:
	jsr InitForIO
	LoadW z8d, DriveCode
	LoadW WriteAddy, __drv1541_drivecode_RUN__
	LoadB z8f, (<(__drv1541_drivecode_SIZE__ / $0020))
SndCDE0:
	jsr SendCHUNK
	bnex SndCDE1
	clc
	lda #$20
	adc z8d
	sta z8d
	bcc @X
	inc z8e
@X:	clc
	lda #$20
	adc WriteAddy
	sta WriteAddy
	bcc @Y
	inc WriteAddy+1
@Y:	dec z8f
	bpl SndCDE0
SndCDE1:
	jmp DoneWithIO

SendCHUNK:
	lda z8f
	ora NUMDRV
	beq SndCHNK1
	ldx #>WriteCommand
	lda #<WriteCommand
	jsr SendDOSCmd
	bnex SndCHNK2
	lda #$20
	jsr $ffa8
	ldy #0
SndCHNK0:
	lda (z8d),y
	jsr $ffa8
	iny
	cpy #$20
	bcc SndCHNK0
	jsr $ffae
SndCHNK1:
	ldx #0
SndCHNK2:
	rts

WriteCommand:
	.byte "M-W"
WriteAddy:
	.word 0

__ExitTurbo:
	txa
	pha
	ldx curDrive
	lda _turboFlags,x
	and #%01000000
	beq ExiTur0
	jsr SendExitTurbo
	ldx curDrive
	lda _turboFlags,x
	and #%10111111
	sta _turboFlags,x
ExiTur0:
	pla
	tax
	rts

__PurgeTurbo:
	jsr ClearCache
	jsr ExitTurbo
PurTur0:
	ldy curDrive
	lda #0
	sta _turboFlags,y
	rts

__NewDisk:
	jsr EnterTurbo
	bnex NewDsk2
	jsr ClearCache
	jsr InitForIO
	LoadB errCount, 0
NewDsk0:
	LoadW z8b, Drv_NewDisk
	jsr DUNK4_2
	jsr GetDOSError
	beq NewDsk1
	inc errCount
	cpy errCount
	beq NewDsk1
	bcs NewDsk0
NewDsk1:
	jsr DoneWithIO
NewDsk2:
	rts

__ChangeDiskDevice:
	pha
	jsr EnterTurbo
	bnex ChngDskDv0
	pla
	pha
	ora #%00100000
	sta r1L
	jsr InitForIO
	ldx #>Drv_ChngDskDev
	lda #<Drv_ChngDskDev
	jsr DUNK4_1
	jsr DoneWithIO
	jsr PurTur0
	pla
	tax
	lda #%11000000
	sta _turboFlags,x
	stx curDrive
	stx curDevice
	ldx #0
	rts
ChngDskDv0:
	pla
	rts

__ReadBlock:
_ReadLink:
	jsr CheckParams_1
	bcc RdBlock2
	bbrf 6, curType, RdBlock0
	jsr DoCacheRead
	bne RdBlock2
RdBlock0:
	ldx #>Drv_ReadSec
	lda #<Drv_ReadSec
	jsr DUNK4_1
	ldx #>Drv_SendByte
	lda #<Drv_SendByte
	jsr DUNK4
	MoveW r4, z8b
	ldy #0
	jsr Hst_RecvByte
	jsr GetDError
	beqx RdBlock1
	inc errCount
	cpy errCount
	beq RdBlock1
	bcs RdBlock0
RdBlock1:
	bnex RdBlock2
	bbrf 6, curType, RdBlock2
	jsr DoCacheWrite
	bra RdBlock2
RdBlock2:
	ldy #0
	rts

__WriteBlock:
	jsr CheckParams
	bcc WrBlock2
WrBlock1:
	ldx #>Drv_WriteSec
	lda #<Drv_WriteSec
	jsr DUNK4_1
	MoveW r4, z8b
	ldy #0
	jsr Hst_SendByte
	jsr GetDOSError
	beq WrBlock2
	inc errCount
	cpy errCount
	beq WrBlock2
	bcs WrBlock1
WrBlock2:
	rts

__VerWriteBlock:
	jsr CheckParams
	bcc VWrBlock3
VWrBlock0:
	lda #3
	sta tryCount
VWrBlock1:
	ldx #>Drv_ReadSec
	lda #<Drv_ReadSec
	jsr DUNK4_1
	jsr GetDOSError
	beqx VWrBlock2
	dec tryCount
	bne VWrBlock1
	ldx #WR_VER_ERR
	inc errCount
	lda errCount
	cmp #5
	beq VWrBlock2
	pha
	jsr WriteBlock
	pla
	sta errCount
	beqx VWrBlock0
VWrBlock2:
	bnex VWrBlock3
	bbrf 6, curType, VWrBlock3
	jmp DoCacheWrite
VWrBlock3:
	rts

GetDOSError:
	ldx #>Drv_SendByte_0
	lda #<Drv_SendByte_0
	jsr DUNK4
GetDError:
	LoadW z8b, errStore
	jsr DUNK5
	PushB errStore
	tay
	lda DOSErrTab-1,y
	tay
	pla
	cmp #1
	beq GetDErr1
	addv $1e
	bne GetDErr2
GetDErr1:
	lda #0
GetDErr2:
	tax
	rts

DOSErrTab:
	.byte $01, $05, $02, $08
	.byte $08, $01, $05, $01
	.byte $05, $05, $05

DriveCode:
.segment "drv1541_drivecode"

Drv_SendByte:			; send 256 bytes, then length (1) and status byte from $00
	ldy #0
	MoveW $73, Drv_SendAddr
Drv_SendByte_1:
	LoadB $1803, $ff	; port A output
	dey			; first byte
	lda ($73),y
	sta $1801		; send first byte
	lda #$10		; bit mask for handshake test
Drv_SendByte_2:			; 27 cycles per byte
	cpy #0
	beq Drv_SendByteEnd
	dey
:	bit $180d		; wait for handshake
	beq :-
Drv_SendAddr = *+1
	ldx $0700,y		; 1 cycle faster than lda ($73),y and we can keep A unchanged for bit test
	stx $1801		; send data
	bit $1800		; clear flag from previous handshake
	jmp Drv_SendByte_2

Drv_SendByteEnd:
:	bit $180d		; wait for handshake
	beq :-
	bit $1800		; clear flag

Drv_SendByte_0:
	LoadB $1803, $ff	; send length (1) and status byte from $00
	LoadB $1801, $01	; send length
	lda #$10
:	bit $180d		; wait for handshake
	beq :-
	bit $1800		; clear flag
	ldy $00
	sty $1801		; send status
:	bit $180d		; wait for handshake
	beq :-
	bit $1800		; clear flag
	LoadB $1803, $00	; port A input
	rts


Drv_RecvWord:
	ldy #1
	jsr Drv_RecvByte
	sta $71
	tay
	jsr Drv_RecvByte
	ldy $71
	rts

Drv_RecvByte:
Drv_RecvByte_1:
	lda #$10
:	bit $180d
	beq :-
	lda $1801			; read data
	bit $1800			; clear CB1
	dey
	sta ($73),y
	bne Drv_RecvByte_1
	rts

D_DUNK4:
	dec $48
	bne D_DUNK4_1
	jsr D_DUNK8_2
D_DUNK4_1:
	LoadB $1800, 0
	rts

DriveStart:
	php
	sei
	PushB $49
	LoadB $1803, $00		; port A input
	LoadB $180c, $0b
	lda $1801			; notify we're runing (initial sync)
	bit $1800			; clear CB1
	bit $180c
	lda $180f
	and #%11011111
	sta $180f
DriveLoop:
	jsr D_DUNK8
	LoadW $73, DExecAddy		; set rcv buffer to next command 
	jsr Drv_RecvWord		; receive address
	jsr D_DUNK8_1
	LoadW $73, $0700		; buffer to data
	lda #>(DriveLoop-1)		; return address
	pha
	lda #<(DriveLoop-1)
	pha
	jmp (DExecAddy)

Drv_ExitTurbo:
	jsr D_DUNK4_1
	LoadB $33, 0
	sta $1800
	jsr $f98f			; turn drive motor off
	LoadB $1c0c, $ec
	pla
	pla
	PopB $49
	plp
	rts

Drv_ChngDskDev:
	lda DDatas
	sta $77
	eor #$60
	sta $78
	rts

D_DUNK5:
	jsr D_DUNK12
	lda $22
	beq D_DUNK5_1
	ldx $00
	dex
	beq D_DUNK5_2
D_DUNK5_1:
	PushB $12
	PushB $13
	jsr Drv_NewDisk_1
	PopB $13
	tax
	PopB $12
	ldy $00
	cpy #$01
	bne D_DUNK5_41
	cpx $17
	bne D_DUNK5_5
	cmp $16
	bne D_DUNK5_5
	lda #0
D_DUNK5_2:
	pha
	lda $22
	ldx #$ff
	sec
	sbc DDatas
	beq D_DUNK5_4
	bcs D_DUNK5_3
	eor #$ff
	adc #1
	ldx #1
D_DUNK5_3:
	jsr D_DUNK6
	lda DDatas
	sta $22
	jsr Drv_NewDisk_6
D_DUNK5_4:
	pla
D_DUNK5_41:
	rts
D_DUNK5_5:
	LoadB $00, $0b
	rts

D_DUNK6:
	stx $4a
	asl
	tay
	lda $1c00
	and #$fe
	sta $70
	lda #$1e
	sta $71
D_DUNK6_1:
	lda $70
	add $4a
	eor $70
	and #%00000011
	eor $70
	sta $70
	sta $1c00
	lda $71
	jsr D_DUNK6_4
	lda $71
	cpy #5
	bcc D_DUNK6_2
	cmp #$11
	bcc D_DUNK6_3
	sbc #2
	bne D_DUNK6_3
D_DUNK6_2:
	cmp #$1c
	bcs D_DUNK6_3
	adc #4
D_DUNK6_3:
	sta $71
	dey
	bne D_DUNK6_1
	lda #$4b
D_DUNK6_4:
	sta $1805
	lda $1805
	bne *-3
	rts

Drv_NewDisk:
	jsr D_DUNK12
Drv_NewDisk_1:
	ldx $00
	dex
	beq Drv_NewDisk_2
	ldx #$ff
	lda #$01
	jsr D_DUNK6
	ldx #$01
	txa
	jsr D_DUNK6
	lda #$ff
	jsr D_DUNK6_4
Drv_NewDisk_2:
	LoadB $70, $04
Drv_NewDisk_3:
	jsr D_DUNK11
	ldx $18
	stx $22
	ldy $00
	dey
	beq Drv_NewDisk_5
	dec $70
	bmi Drv_NewDisk_4
	ldx $70
	jsr Drv_NewDisk_7
	sec
	bcs Drv_NewDisk_3
Drv_NewDisk_4:
	LoadB $22, 0
	rts
Drv_NewDisk_5:
	txa
Drv_NewDisk_6:
	jsr $f24b			; Establish number of sectors per track (in: A=track, out: A=number of sectors on that track)
	sta $43
Drv_NewDisk_7:
	lda $1c00
	and #$9f
	ora DTrackTab,x
Drv_NewDisk_8:
	sta $1c00
	rts

D_DUNK8:			; LED off?
	lda #$f7
	bne D_DUNK8_3
D_DUNK8_1:			; LED on?
	lda #$08
	ora $1c00
	bne Drv_NewDisk_8

D_DUNK8_2:
	LoadB $20, 0
	LoadB $3e, $ff
	lda #$fb
D_DUNK8_3:
	and $1c00
	jmp Drv_NewDisk_8

DTrackTab:
	.byte $00, $20, $40, $60

D_DUNK9:
	tax
	bbrf 7, $20, D_DUNK9_0
	jsr D_DUNK12_1
	LoadB $20, $20
	ldx #0
D_DUNK9_0:
	cpx $22
	beq D_DUNK9_1
	jsr Drv_NewDisk_2
	cmp #1
	bne D_DUNK9_1
	ldy $19
	iny
	cpy $43
	bcc @X
	ldy #0
@X:	sty $19
	LoadB $45, 0
	LoadW $32, $0018
	jsr D_DUNK11_1
D_DUNK9_1:
	rts

Drv_WriteSec:
	jsr D_DUNK5
	ldx $00
	dex
	bne D_DUNK10_1
	jsr D_DUNK9
D_DUNK10_1:
	jsr Drv_RecvWord
	lda #$10
	bne D_DUNK10_2
Drv_ReadSec:
	jsr D_DUNK5
	lda #0
D_DUNK10_2:
	ldx $00
	dex
	beq D_DUNK11_0
	rts

D_DUNK11:
	lda #$30
D_DUNK11_0:
	sta $45
	lda #>DDatas
	sta $33
	lda #<DDatas
	sta $32
D_DUNK11_1:
	LoadB $31, 7
	tsx
	stx $49
	ldx #1
	stx $00
	dex
	stx $3f
	LoadB $1c0c, $ee
	lda $45
	cmp #$10
	beq D_DUNK11_3
	cmp #$30
	beq D_DUNK11_2
	jmp $f4ca				; Test command code further ($00=read, $10=write, $20=verify, other=read block header)
D_DUNK11_2:
	jmp $f3b1				; Read block header, verify ID
D_DUNK11_3:
	jsr $f5e9				; Calculate parity for data buffer ($30)
	sta $3a
	lda $1c00
	and #$10
	bne D_DUNK11_4
	lda #$08
	bne D_DUNK11_9
D_DUNK11_4:
	jsr $f78f				; Convert 260 bytes (256+4) to 325 bytes group code buffer $01BB-$01FF and ($30)
	jsr $f510				; Read block header (wait until needed block header arrives)
	ldx #9
D_DUNK11_5:
	bvc D_DUNK11_5				; skip over sync
	clv
	dex
	bne D_DUNK11_5
	lda #$ff
	sta $1c03
	lda $1c0c
	and #$1f
	ora #$c0
	sta $1c0c
	lda #$ff
	ldx #5
	sta $1c01
	clv
D_DUNK11_6:
	bvc D_DUNK11_6
	clv
	dex
	bne D_DUNK11_6
	ldy #$bb
D_DUNK11_7:
	lda $0100,y				; write data
	bvc *
	clv
	sta $1c01
	iny
	bne D_DUNK11_7
D_DUNK11_8:
	lda ($30),y				; write data, continued
	bvc *
	clv
	sta $1c01
	iny
	bne D_DUNK11_8
	bvc *
	lda $1c0c
	ora #$e0
	sta $1c0c
	LoadB $1c03, 0
	sta $50
	lda #1
D_DUNK11_9:
	sta $00
	rts

D_DUNK12:					; spin up motor
	lda $20
	and #$20
	bne D_DUNK12_3				; already on
	jsr $f97e				; Turn drive motor on
D_DUNK12_1:
	ldy #$80				; delay until it spins up
:	dex
	bne :-
	dey
	bne :-
	sty $3e
D_DUNK12_3:
	LoadB $48, $ff
	rts

DExecAddy:
	.word 0
DDatas:
	;.word 0
.segment "drv1541_b"

ClrCacheDat:
	.word 0

ClearCache:
	bbsf 6, curType, DoClearCache
	rts
DoClearCache:
	LoadW r0, ClrCacheDat
	ldy #0
	sty r1L
	sty r1H
	sty r2H
	iny
	iny
	sty r2L
	iny
	sty r3H
	ldy curDrive
	lda driveData,y
	sta r3L
DoClrCache1:
	jsr StashRAM
	inc r1H
	bne DoClrCache1
	inc r3L
	dec r3H
	bne DoClrCache1
	rts

DoCacheRead:
	ldy #%10010001
	jsr DoCacheDisk
	ldy #0
	lda (r4),y
	iny
	ora (r4),y
	rts

GiveNoError:
	ldx #0
	rts

DoCacheVerify:
	ldy #%10010011
	jsr DoCacheDisk
	and #$20
	rts

DoCacheWrite:
	ldy #%10010000
DoCacheDisk:
	PushW r0
	PushW r1
	PushW r2
	PushB r3L
	tya
	pha
	ldy r1L
	dey
	lda CacheTabL,y
	add r1H
	sta r1H
	lda CacheTabH,y
	ldy curDrive
	adc driveData,y
	sta r3L
	ldy #0
	sty r1L
	sty r2L
	iny
	sty r2H
	MoveW r4, r0
	pla
	tay
	jsr DoRAMOp
	tax
	PopB r3L
	PopW r2
	PopW r1
	PopW r0
	txa
	ldx #0
	rts

CacheTabL:
	.byte $00, $15, $2a, $3f, $54, $69, $7e, $93
	.byte $a8, $bd, $d2, $e7, $fc, $11, $26, $3b
	.byte $50, $65, $78, $8b, $9e, $b1, $c4, $d7
	.byte $ea, $fc, $0e, $20, $32, $44, $56, $67
	.byte $78, $89, $9a, $ab
CacheTabH:
	.byte $00, $00, $00, $00, $00, $00, $00, $00
	.byte $00, $00, $00, $00, $00, $01, $01, $01
	.byte $01, $01, $01, $01, $01, $01, $01, $01
	.byte $01, $01, $02, $02, $02, $02, $02, $02
	.byte $02, $02, $02, $02

tmpclkreg:
	.byte 0
tmpPS:
	.byte 0
tmpgrirqen:
	.byte 0
tmpCPU_DATA:
	.byte 0
tmpmobenble:
	.byte 0
	.byte 0
DExeProc:
	.word 0
DTrkSec:
	.word 0
errCount:
	.byte 0
errStore:
	.byte 0
tryCount:
	.byte 0
borderFlag:
	.byte 0

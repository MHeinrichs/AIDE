;:ts=8

		include "exec/types.i"
		include	"exec/exec.i"
		include "devices/scsidisk.i"

		include "registers.i"

		xref	_LVOReplyMsg
		xref	_LVOAllocMem
		xref	_LVOAddTask
		xref	_LVOSignal
		xref	_LVOWait
		xref	_LVOAllocSignal
		xref	_LVOFreeSignal
		xref	_LVOFindTask
		xref	_LVOWaitPort
		xref	_LVOGetMsg
		xref	_LVOPutMsg
		xref	_LVOCopyMem

STACK_SIZE	equ	256
PRIORITY	equ	0

SENSE_LENGTH	equ	$22
SENSE_DATA	equ	$24

		moveq	#-1,d0
		rts

initDDescrip:	DC.W    $4AFC	         	; UWORD RT_MATCHWORD
		DC.L    initDDescrip		; APTR  RT_MATCHTAG
		DC.L    EndCode			; APTR  RT_ENDSKIP
		DC.B    128        		; UBYTE RT_FLAGS
		DC.B    1      			; UBYTE RT_VERSION
		DC.B    3        		; UBYTE RT_TYPE
		DC.B    0    			; BYTE  RT_PRI
		DC.L    myName			; APTR  RT_NAME
		DC.L    idString		; APTR  RT_IDSTRING
		DC.L    Init			; APTR  RT_INIT

myName:		DC.B	'scsi-disk.device',0

idString:	dc.b	'scsi-disk 1.0 (22-Sep-89)',13,10,0

		ds.w	0 			; force word alignment

Init:		DC.L	$22+22
		DC.L	funcTable
		DC.L	datatable
		DC.L	initRoutine

datatable:	dc.l	$e0000008
		dc.w    $0300
		dc.l	$c000000a
		dc.l	myName
		dc.l	$e000000e
		dc.w	$0600
		dc.l	$d0000014
		dc.w	1
		dc.l	$d0000016
		dc.w	0
		dc.l	$c0000018
		dc.l	idString
		dc.l	0

funcTable: 	dc.l	Open
		dc.l	Close
		dc.l	Expunge
		dc.l	Null
		dc.l	BeginIO
		dc.l	AbortIO
		dc.l	-1

initRoutine:	movem.l	d0-d1/a0-a3/a6,-(a7)

		; Save the device node pointer.

		move.l	d0,devnode
		move.l	d0,a0
		move.w	#-1,SENSE_LENGTH(a0)

		; Create a subtask.  First, allocate a task descriptor.

		move.l	#TC_SIZE,d0
		move.l	#MEMF_PUBLIC!MEMF_CLEAR,d1
		move.l	4,a6
		jsr	_LVOAllocMem(a6)
		tst.l	d0
		beq	guru
		move.l	d0,a2

		; Next, allocate a stack.

		move.l	#STACK_SIZE,d0
		move.l	#MEMF_PUBLIC!MEMF_CLEAR,d1
		jsr	_LVOAllocMem(a6)
		tst.l	d0
		beq	guru

		; Initialize the task descriptor.

		move.l	d0,TC_SPLOWER(a2)
		add.l	#STACK_SIZE,d0
		move.l	d0,TC_SPUPPER(a2)
		move.l	d0,TC_SPREG(a2)
		move.b	#NT_TASK,LN_TYPE(a2)
		move.b	#PRIORITY,LN_PRI(a2)
		move.l	#myName,LN_NAME(a2)
		lea	TC_MEMENTRY(a2),a0
		NEWLIST a0

		; Allocate a signal, then launch the task.  The task will
		; use the signal to indicate when it's done initializing.

		moveq	#-1,d0
		jsr	_LVOAllocSignal(a6)
		cmp.w	#-1,d0
		beq	guru

		move.w	d0,signalnum
		sub.l	a1,a1
		jsr	_LVOFindTask(a6)

		; Temporarily stash own task ID here.

		move.l	d0,taskport

		move.l	a2,a1
		lea	subtask_start,a2
		sub.l	a3,a3
		jsr	_LVOAddTask(a6)

		; Wait for the signal from the subtask, then free the
		; signal again.

		moveq	#0,d0
		move.w	signalnum,d1
		bset.l	d1,d0
		jsr	_LVOWait(a6)
		move.w	signalnum,d0
		ext.l	d0
		jsr	_LVOFreeSignal(a6)

		movem.l	(a7)+,d0-d1/a0-a3/a6
		rts

Open:		addq.w	#1,32(a6)
		swap	d0
		move.w	d1,d0
		swap	d0
		move.l	d0,$18(a1)		; Save unit number.
		bra.s	Null

Close:		subq.w	#1,32(a6)
Expunge:
Null:		moveq.l	#0,d0
		rts

AbortIO:	moveq.l	#-1,d0
		rts

		; Send an I/O request to the device driver.

BeginIO:	bclr.b	#0,30(a1)
		move.b	#NT_MESSAGE,LN_TYPE(a1)
		move.l	taskport,a0
		move.l	4,a6
		jmp	_LVOPutMsg(a6)

		; Guru... had better not happen, so just use a generic
		; guru number for everything.

guru:		move.l	#$80008032,d7
		move.l	4,a6
		jmp	-$6c(a6)

		; This is the subtask, named "scsi.device".  It must
		; immediately create a message port, store the address
		; of it, and signal its creator.

subtask_start:
		move.l	devnode,a5
		move.l	4,a6

		move.l	#MP_SIZE,d0
		move.l	#MEMF_PUBLIC!MEMF_CLEAR,d1
		jsr	_LVOAllocMem(a6)
		tst.l	d0
		beq	guru

		; Initialize the port structure.

		move.l	d0,a2
		sub.l	a1,a1
		jsr	_LVOFindTask(a6)
		move.l	d0,MP_SIGTASK(a2)

		moveq	#-1,d0
		jsr	_LVOAllocSignal(a6)
		cmp.w	#-1,d0
		beq	guru
		move.b	d0,MP_SIGBIT(a2)
		move.b	#PA_SIGNAL,MP_FLAGS(a2)

		lea	MP_MSGLIST(a2),a0
		NEWLIST	a0

		; All set.  Store the address of the port and tell the
		; parent task that it can continue.

		move.l	taskport,a1
		move.l	a2,taskport
		moveq	#0,d0
		move.w	signalnum,d1
		bset.l	d1,d0
		jsr	_LVOSignal(a6)

		; Wait for something to do.

mainloop:	move.l	taskport,a0
		jsr	_LVOWaitPort(a6)
		move.l	taskport,a0
		jsr	_LVOGetMsg(a6)
		move.l	d0,a1
		clr.b	31(a1)
		move.w	28(a1),d0
		asl.w	#2,d0
		lea	cmdtbl,a0
		move.l	0(a0,d0.w),a0
		jsr	(a0)
		jsr	_LVOReplyMsg(a6)
		bra	mainloop

cmdtbl:		dc.l	badcmd		;  0 - Invalid
		dc.l	noop		;  1 - Reset
		dc.l	readwrt		;  2 - Read
		dc.l	readwrt		;  3 - Write
		dc.l	noop		;  4 - Update
		dc.l	noop		;  5 - Clear
		dc.l	noop		;  6 - Stop
		dc.l	noop		;  7 - Start
		dc.l	noop		;  8 - Flush
		dc.l	noop		;  9 - Motor
		dc.l	noop		; 10 - Seek
		dc.l	readwrt		; 11 - Format
		dc.l	noop		; 12 - Remove
		dc.l	noop		; 13 - ChangeNum
		dc.l	noop		; 14 - ChangeState
		dc.l	noop		; 15 - ProtStatus
		dc.l	badcmd		; 16 - RawRead
		dc.l	badcmd		; 17 - RawWrite
		dc.l	drivetype	; 18 - GetDriveType
		dc.l	numtracks	; 19 - GetNumTracks
		dc.l	noop		; 20 - AddChangeInt
		dc.l	noop		; 21 - RemChangeInt
		dc.l	badcmd		; 22 - Unknown
		dc.l	badcmd		; 23 - Unknown
		dc.l	badcmd		; 24 - Unknown
		dc.l	badcmd		; 25 - Unknown
		dc.l	badcmd		; 26 - Unknown
		dc.l	badcmd		; 27 - Unknown
		dc.l	scsidirect	; 28 - Do SCSI command

		; Custom commands start here... just after highest standard
		; command number known to me.

		dc.l	mygetstatus	; 29 - Get sense information
		dc.l	myresetscsi	; 30 - Reset SCSI bus


badcmd:		move.b	#-3,31(a1)
		rts

drivetype:	move.l	#1,32(a1)
		rts

numtracks:
noop:		clr.l	32(a1)
		rts

;------------------------------------------------------------------------
; Get most recent sense information.  Standard I/O command format.
; Fail with error #20 if no sense information recorded yet.
;

mygetstatus:	move.w	SENSE_LENGTH(a5),d0
		cmp.w	#-1,d0
		beq.s	gotnostatus
		ext.l	d0
		cmp.l	36(a1),d0
		bls.s	length_ok
		move.l	36(a1),d0
length_ok:	move.l	d0,32(a1)
		lea	SENSE_DATA(a5),a0
		move.l	a1,-(a7)
		move.l	40(a1),a1
		jsr	_LVOCopyMem(a6)
		move.l	(a7)+,a1
		rts
gotnostatus:	clr.l	32(a1)
		move.b	#20,31(a1)
		rts

;------------------------------------------------------------------------
; Reset the SCSI bus.  Can be called both as an I/O command and from
; within the device driver.
;

myresetscsi:	move.b	#$80,p1
		moveq	#19,d0
rloop:		nop
		dbra	d0,rloop
		move.b	#0,p1
		rts

;------------------------------------------------------------------------
; Direct access SCSI command.
;

scsidirect:	move.l	40(a1),a4

		; Select the target.

		bsr	select
		beq.s	sel_ok
		move.b	#HFERR_SelTimeout,31(a1)
		rts

		; Send the command bytes.

sel_ok:		move.l	scsi_Command(a4),a0
		move.w	scsi_CmdLength(a4),d6
		bsr	sendcmd
		move.w	d1,scsi_CmdActual(a4)

		moveq	#0,d1			; Transfer counter
		move.l	scsi_Data(a4),a0	; Data pointer
		move.l	scsi_Length(a4),d7	; Buffer size

		; Do read/write as appropriate.

		btst.b	#0,scsi_Flags(a4)
		beq	direct_write

		; Handle data in phase.

		bsr	waitreq

		lea	p5r,a2
	    	lea	dma_read,a3

		move.b	#1,p3
		move.b	#2,p2
		move.b	#0,p7

		move.w	d7,d6		; d6 = low count
		move.l	d7,d5
		swap	d5		; d5 = high count

		move.b	#%01001000,d2
		move.b	d2,d3
		bra.s	loopbot

r_notboth:	btst.b	#3,d3
		beq.s	r_dma_done_1
		move.b	d2,d3
r_waitdma:              
	        and.b   (a2),d3		; 8
		cmp.b	d2,d3		; 4
		bne.s	r_notboth	; 8
	    	move.b  (a3),(a0)+	; 12
loopbot:
		dbra	d6,r_waitdma	; 10 (12)   Total: 44
		dbra	d5,r_waitdma

		move.l	d7,d1
r_skipdma:
	        move.b  (a2),d0
		btst    #3,d0
		beq.s   r_dma_done   
		btst    #6,d0
	        beq.s   r_skipdma   
	    	move.b  (a3),d0
		addq.l  #1,d1      
		bra.s	r_skipdma

r_dma_done_1:	swap.w	d5
		move.w	d6,d5
		move.l	d7,d1
		sub.l	d5,d1
		subq.l	#1,d1

r_dma_done:	move.b	#0,p2
		bra	d_finish

; -------------------------------

direct_write:	move.w	d7,d6		; d6 = low count
		move.l	d7,d5
		swap	d5		; d5 = high count

		lea	p4r,a2
		lea	p5r,a3
		lea	p0,a4
		lea	p1,a5
		move.b	#5,d2
		move.b	#3,d3
		move.b	#1,d4
		move.b	#17,d0

		move.b	#0,p3
		move.b	d4,p1

		bra.s	w_loopbot

w_waitreq:
		btst.b	d2,(a2)
		beq.s	w_waitreq
		btst.b	d3,(a3)
		beq.s	w_done1
		move.b	(a0)+,(a4)
		move.b	d0,(a5)
;w_waitnreq:
;		btst.b	d2,(a2)
;		bne.s	w_waitnreq
		move.b	d4,(a5)
w_loopbot:
		dbra	d6,w_waitreq
		dbra	d5,w_waitreq

		move.l	d7,d1
		move.b	#0,(a4)
w_stuff:
		btst.b	d2,(a2)
		beq.s	w_stuff
		btst.b	d3,(a3)
		beq.s	w_done2
		move.b	d0,(a5)
w_s_waitnreq:
		btst.b	d2,(a2)
		bne.s	w_s_waitnreq
		move.b	d4,(a5)
		addq.l	#1,d1
		bra.s	w_stuff

w_done1:	swap.w	d5
		move.w	d6,d5
		move.l	d7,d1
		sub.l	d5,d1
		subq.l	#1,d1

w_done2:	move.b	#0,(a5)

; -------------------------------

		; Finish the command after phase changes.

d_finish: 	move.l	40(a1),a4
		move.l	d1,scsi_Actual(a4)
		bsr	finishcmd
		tst.w	d4
		beq.s	notfatal
		move.b	#HFERR_Phase,31(a1)
		rts
notfatal:	move.b	d5,scsi_Status(a4)
		btst.b	#1,d5
		beq.s	notsense
		move.b	#HFERR_BadStatus,31(a1)
notsense:	rts


;------------------------------------------------------------------------
; Read/Write command.
;

readwrt:	clr.b	startflag

rw_retry:	move.l	36(a1),d3
		move.l	d3,32(a1)
		beq	trans0

		move.l	44(a1),a0
		move.l	40(a1),a2

		; A2 - point to memory
		; A0 - disk offset
		; D3 - number of bytes to transfer
		; 28(A1).W - command (2 = read)

		; SCSI stuff.

		btst.b	#1,$19(a1)
		beq.s	longcmd

		; 6-byte read/write command.

		move.l	a0,d1
		lsr.l	#5,d1
		lsr.l	#4,d1
		move.l	d1,cmdbuf
		move.b	#8,cmdbuf
		cmp.w	#2,28(a1)
		beq.s	is6read
		move.b	#10,cmdbuf
is6read:	move.l	d3,d4
		lsr.l	#5,d4
		lsr.l	#4,d4
		cmp.l	#$100,d4
		bls.s	length_ok1
		move.b	#20,31(a1)
		clr.l	32(a1)
		rts
length_ok1:	move.b	d4,cmdbuf+4
		clr.b	cmdbuf+5
		moveq	#6,d6
		bra	docmd

longcmd:	move.b	#40,cmdbuf
		cmp.w	#2,28(a1)
		beq.s	isread
		move.b	#42,cmdbuf
isread:		move.b	#0,cmdbuf+1

		move.l	a0,d1
		lsr.l	#5,d1
		lsr.l	#4,d1
		move.l	d1,cmdbuf+2

		clr.b	cmdbuf+6

		move.l	d3,d1
		lsr.l	#5,d1
		lsr.l	#4,d1
		move.w	d1,d4
		move.b	d1,cmdbuf+8
		lsr.w	#8,d1
		move.b	d1,cmdbuf+7

		clr.b	cmdbuf+9

		; Send 10-byte command.

		moveq	#10,d6

		; Command is in cmdbuf, command length is in D3.

docmd:		bsr	select
		tst.w	d0
		bne	iserror

		lea	cmdbuf,a0
		bsr	sendcmd

		cmp.w	#2,28(a1)
		bne	write_phase

		; read phase

		bsr	waitreq
		cmp.b	#1,d0
		bne	nodataphase

		move.b	#1,p3
		move.b	#2,p2
		move.b	#0,p7

		lea	p5r,a0
		move.l	#dma_read,a3
		moveq	#6,d2

		; Check if blind reads are allowed for this unit

		btst.b	#0,$19(a1)
		beq.s	noblind

		; Transfer loop - blind read version

		subq.l	#1,d4
skipit:		btst.b	d2,(a0)
		beq.s	skipit
		move.w	#63,d1
skip3:		move.b	(a3),(a2)+
		move.b	(a3),(a2)+
		move.b	(a3),(a2)+
		move.b	(a3),(a2)+
		move.b	(a3),(a2)+
		move.b	(a3),(a2)+
		move.b	(a3),(a2)+
		move.b	(a3),(a2)+
		dbra	d1,skip3
		dbra	d4,skipit
		bra.s	rddone

		; Transfer loop - regular version

noblind:	subq.l	#1,d3
		move.w	d3,d1
		swap	d3
skipit4:	btst.b	d2,(a0)
		beq.s	skipit4
		move.b	(a3),(a2)+
		dbra	d1,skipit4
		dbra	d3,skipit4

rddone:		move.b	#0,p2

		; Finish up the command.  The SCSI device had better
		; be in status phase, or else...

finishup:	bsr	finishcmd

		; Check that the command terminated properly and the status
		; byte is valid.

		tst.w	d4
		bne	iserror

		; Check the "check condition" bit in the status byte.

		btst.b	#1,d5
		beq	trans0

		bsr	req_sense

		; If the error is "unit attention", then retry the command.
		; Fail for all other errors.

		move.w	SENSE_LENGTH(a5),d0
		cmp.w	#3,d0
		bcs.s	iserror
		move.b	SENSE_DATA(a5),d0
		bclr	#7,d0
		cmp.b	#$70,d0
		bne.s	iserror
		move.b	SENSE_DATA+2(a5),d0
		and.b	#$0f,d0
		cmp.b	#$06,d0
		bne.s	notua

		clr.b	31(a1)
		bra	rw_retry

iserror:	move.b	#20,31(a1)
		clr.l	32(a1)

trans0:		rts

		; If a command failed because of a "not ready" status,
		; then do a "start unit" and retry it.  This will restart
		; a drive which has been stopped by a "park" command if
		; it is subsequently accessed.  Only try this once so
		; we don't get into a loop if the unit is "not ready"
		; for some other reason.

		; Note: This is enabled by bit 2 in the "Flags" word.

notua:		cmp.b	#$02,d0
		bne.s	iserror

		btst.b	#2,$19(a1)
		beq.s	iserror

		tst.b	startflag
		bne.s	iserror

		bsr	select
		tst.w	d0
		bne.s	iserror

		move.l	#$1b000000,cmdbuf
		move.w	#$0100,cmdbuf+4
		moveq	#6,d6
		lea	cmdbuf,a0
		bsr	sendcmd
		bsr	finishcmd
		tst.w	d0
		bne.s	iserror

		move.b	#1,startflag
		clr.b	31(a1)
		bra	rw_retry

		; Enter here if data phase missing for read/write command.
		; Flag an error and finish the command.

nodataphase:	move.b	#20,31(a1)
		clr.l	32(a1)
		bra	finishup

write_phase:	bsr	waitreq
		cmp.b	#0,d0
		bne	nodataphase

		move.b	#0,p3

		subq.l	#1,d3
		move.w	d3,d2
		swap	d3

		move.l	#p4r,a0
		move.l	#p1,a3
		move.l	#p0,a4

		moveq	#1,d0
		moveq	#5,d1
		moveq 	#17,d4

		move.b	d0,(a3)

waitreq2:	btst	d1,(a0)
		beq.s	waitreq2
		move.b	(a2)+,(a4)
		move.b	d4,(a3)
waitack2:	btst.b	d1,(a0)
		bne.s	waitack2
		move.b	d0,(a3)
		dbra	d2,waitreq2
		dbra	d3,waitreq2

		move.b	#0,(a3)

		bra	finishup

;------------------------------------------------------------------------
; Wait for REQ*.  Returns current SCSI bus phase in D0.
;

waitreq:	move.b	p4r,d0
		btst	#5,d0
		beq.s	waitreq
		lsr.b	#2,d0
		and.b	#7,d0
		rts

;------------------------------------------------------------------------
; Send a byte, once REQ* is asserted.

send:		move.b	d0,p0
		move.b	#1,p1
		move.b	#17,p1
		bra.s	waitack

;------------------------------------------------------------------------
; Receive a byte, once REQ* is asserted.
;

rcv:		move.b	p0r,d0
		move.b	#16,p1

;------------------------------------------------------------------------
; Wait for REQ* to go away, then deassert bus if it was asserted.
;

waitack:	btst.b	#5,p4r
		bne.s	waitack
		move.b	#0,p1
		rts

;------------------------------------------------------------------------
; Select a SCSI device.  Does all the initialization of the 5380 from
; idle state.  Gets the device number from the I/O request.
;
; Inputs:   A1 - I/O request
; Outputs:  D0 - -1: Selection failed, 5380 back in idle state.
;                 0: Selection successful.
;

		; Initialize 5380 registers.

select:		moveq	#0,d0
		move.b	d0,p1
		move.b	d0,p2
		move.b	d0,p3

		; Drive own ID bit (bit 0) and target's ID bit onto bus
		; and assert SEL*.

		moveq	#1,d0
		move.b	$1b(a1),d1
		bset	d1,d0
		move.b	d0,p0
		move.b	#5,p1

		; Wait 1/4 second or until BSY* asserted.

		move.w	#52646,d0
waitsel:	btst.b	#6,p4r
		dbne	d0,waitsel
		beq	sel_fail

		; Target responded.  Deassert SEL*, release bus, and
		; return zero to indicate success.

		moveq	#0,d0
		move.b	d0,p1
		rts

		; Target did not respond.  

sel_fail:	move.b	#0,p1
		moveq	#-1,d0
		rts

;------------------------------------------------------------------------
; Do command phase.
;
; Inputs:   A0 - Pointer to command buffer.
;           D6 - Length of command data.
; Outputs:  D1 - Number of bytes accepted by target before phase change
;                or command bytes exhausted.
;

sendcmd:	moveq	#0,d1
		move.b	#2,p3

		; Wait for REQ* and get bus phase.  Bail out if phase
		; is not command phase.

cmdloop:	bsr	waitreq
		cmp.b	#2,d0
		bne.s	cmd_allsent

		; Send a byte from the command buffer and count it.

		move.b	(a0)+,d0
		bsr	send
		addq.w	#1,d1

		; Continue until command buffer exhausted or phase change.

		cmp.w	d6,d1
		bcs.s	cmdloop
cmd_allsent:	rts

;------------------------------------------------------------------------
; Finish a command.  Continue here when a phase other than data in or
; data out occurs after command phase.
;
; This routine will reset the SCSI bus if it becomes confused about what
; state the target is in.  This is better than crashing or leaving the
; target connected.
;
; Inputs:   None
; Outputs:  D4 -  0: All OK.
;		 -1: An error occurred, D5 is not valid.
;           D5 - SCSI status byte.
;

finishcmd:	moveq	#-1,d4

		; Wait for REQ*, check phase.  If Status phase, then clear
		; D4 and get status byte.  Otherwise, leave D4 at -1 and
		; go to next phase.

		bsr	waitreq
		cmp.b	#3,d0
		bne.s	nostatus
		bsr	rcv
		move.b	d0,d5
		moveq	#0,d4

		; Wait for REQ*, check phase.  If message in, get the
		; message, verify that it is "Disconnect" (zero).  If the
		; phase or the message is wrong, give up.

nostatus:	bsr	waitreq
		cmp.b	#7,d0
		bne.s	bigtrouble
		bsr	rcv
		tst.b	d0
		bne.s	bigtrouble

		; And wait for the device to actually disconnect.

waitdisc:	btst.b	#6,p4r
		bne.s	waitdisc
		rts

		; Continue here if totally confused.  Indicate that an
		; error occurred and reset the SCSI bus.

bigtrouble:	moveq	#-1,d4
		bra	myresetscsi


;------------------------------------------------------------------------
; Request 20 bytes or less of sense information, storing them in the
; sense data area of the device structure.
;
; Inputs:   A5 - Pointer to device node.
; Outputs:  Sense data in device structure updated.
;	    D0 -  0: All OK.
;                -1: Command did not complete successfully, sense data
;		     may be invalid.
;

req_sense:	bsr	select
		tst.w	d0
		bne.s	sense_fail

		; Build 6-byte "Request Sense" command and send it.
		; Don't bother to check if target accepted all 6 bytes.

		move.l	#$03000000,cmdbuf
		move.w	#$1400,cmdbuf+4
		moveq	#6,d6
		lea	cmdbuf,a0
		bsr	sendcmd

		; Receive sense info.

		lea	SENSE_DATA(a5),a2
		moveq	#0,d1
senseloop:
		bsr	waitreq
		cmp.b	#1,d0
		bne.s	sense_end
		bsr	rcv
		move.b	d0,(a2)+
		addq.w	#1,d1
		bra.s	senseloop
sense_end:
		move.w	d1,SENSE_LENGTH(a5)

		bsr	finishcmd
		move.w	d4,d0

sense_fail:	rts

EndCode:

		section	one,bss

cmdbuf:		ds.w	5
signalnum:	ds.w	1
taskport:	ds.l	1
devnode:	ds.l	1
startflag:	ds.b	1

		END

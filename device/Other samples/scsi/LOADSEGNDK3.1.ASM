
	INCLUDE	"exec/types.i"
	INCLUDE	"exec/ables.i"
	INCLUDE	"exec/memory.i"
	INCLUDE	"libraries/dos.i"

ABSEXECBASE	EQU	4
	
	XDEF	_PrivateLoadSeg

	XREF	_intena

	XREF	_LVOAllocMem
	XREF	_LVOAllocAbs
	XREF	_LVOClose
	XREF	_LVOCloseLibrary
	XREF	_LVOFreeMem
	XREF	_LVOOpen
	XREF	_LVOOpenLibrary
	XREF	_LVORead

MAXNAMELEN	EQU	128

HUNK_HEADER	EQU	1011


;------	LoadSeg ------------------------------------------------------
;
;   NAME
;	LoadSeg - load a file into memory and relocate it
;
;   SYNOPSIS
;	success = LoadSeg(allocFunc, freeFunc, readFunc,
;	d0                a0         a1        a2
;	                  readHandle, dataEnviron)
;	                  d1          a6
;
;	actual = readFunc(readHandle, buffer, length)
;       d0                d1          a0      d0
;
;	memory = allocFunc(size, flags)
;	d0                 d0    d1
;
;	freeFunc(memory, size)
;	         a1      d0
;
;   EXCEPTIONS
;	This LoadSeg fails if resident or overlay hunks exist.
;
;---------------------------------------------------------------------
lsv_DataEnviron	EQU	0		; function argument passed in a6
lsv_ReadFunc	EQU	lsv_DataEnviron-4
lsv_FreeFunc	EQU	lsv_ReadFunc-4
lsv_AllocFunc	EQU	lsv_FreeFunc-4
lsv_ReadHandle	EQU	lsv_AllocFunc-4
lsv_Segment	EQU	lsv_ReadHandle-4
lsv_Name	EQU	lsv_Segment-MAXNAMELEN
lsv_LINKSIZE	EQU	lsv_Name-4

;
;   d2	various temporaries
;   d3	hunk relocation offset value
;   d4	byte limit of current hunk
;   d5	first hunk slot
;   d6	last hunk slot
;   a2	current hunk address
;   a3	segment tail
;   a4	hunk table
;   a5	lsv structure
;   a6	scratch data area
;
LoadSeg:
		movem.l	d2-d6/a2-a5,-(a7)
		link	a6,#lsv_LINKSIZE
		move.l	a6,a5

		movem.l	d1/a0/a1/a2,lsv_ReadHandle(a5)
		moveq	#0,d6
		move.l	d6,lsv_Segment(a5)
		move.l	d6,a4

	    ;-- ensure this is a valid load file
		bsr	GetLong
		cmp.l	#HUNK_HEADER,d0
		bne	lsFail

	    ;-- handle resident library header by failing if it exists
		bsr	GetLong
		tst.l	d0
		bne	lsFail

	    ;-- handle hunks
		bsr	GetLong		; get table size
		bsr	GetLong		; get first hunk slot
		move.l	d0,d5
		bsr	GetLong		; get last hunk slot
		move.l	d0,d6

		;-- allocate the temporary hunk table via Exec AllocMem
		addq.l	#1,d0		; last slot + 1 long words
		lsl.l	#2,d0		; in bytes
		moveq	#0,d1
		move.l	ABSEXECBASE,a6
		jsr	_LVOAllocMem(a6)
		tst.l	d0
		beq	lsFail
		move.l	d0,a4

		;-- allocate the hunks themselves
lsAllocHunks:
		move.w	d5,d2
		lsl.w	#2,d5
		lea	0(a4,d5.w),a2
		lea	lsv_Segment(a5),a3
lsAllocHunk:
		;-- for each entry in this file
		cmp.w	d6,d2
		bgt.s	lsLoadHunks
		;-- allocate space for the hunk and cache pointer in the table	
		bsr	GetLong		; get size needed
		beq.s	lsEmptyHunk
		bsr	GetVector	; get memory
		move.l	a0,(a2)+	; cache pointer
		move.l	a0,d0		; convert
		lsr.l	#2,d0		;   to BPTR
		move.l	d0,(a3)		;   and link to end of segment
		move.l	a0,a3		; new tail
lsNextHunk:
		addq.w	#1,d2		; count up hunk
		bra.s	lsAllocHunk

lsEmptyHunk:
		clr.l	(a2)+
		bra.s	lsNextHunk

		;-- read in the load image
lsLoadHunks:
		moveq	#-1,d4			; byte limit: between hunks
lsLoadHunk:
		;-- perform GetLong-like function, but EOF is OK here
		subq.l	#4,a7
		move.l	a7,a0
		moveq	#4,d0
		move.l	lsv_ReadHandle(a5),d1
		movem.l	lsv_ReadFunc(a5),a1/a6
		jsr	(a1)
		tst.l	d0			; check for EOF
		beq	lsFreeHunk
		cmp.l	#4,d0
		bne	lsFail
		;-- switch on hunk type
		move.l	(a7)+,d0
		andi.l	#$3fffffff,d0
		sub.l	#1000,d0
		bmi	lsFail
		cmp.l	#HUNKSWITCHLIMIT,d0
		bge	lsFail
		add.w	d0,d0
		move.w	lsSwitch(pc,d0.w),d0
		jsr	lsSwitch(pc,d0.w)
		bra.s	lsLoadHunk

lsSwitch:
		dc.w	lssHunkName-lsSwitch
		dc.w	lssHunkCode-lsSwitch
		dc.w	lssHunkData-lsSwitch
		dc.w	lssHunkBSS-lsSwitch
		dc.w	lssHunkReloc32-lsSwitch
		dc.w	lssHunkReloc16-lsSwitch
		dc.w	lssHunkReloc8-lsSwitch
		dc.w	lssHunkExt-lsSwitch
		dc.w	lssHunkSymbol-lsSwitch
		dc.w	lssHunkDebug-lsSwitch
		dc.w	lssHunkEnd-lsSwitch
		dc.w	lssHunkHeader-lsSwitch
		dc.w	lssHunkCont-lsSwitch
		dc.w	lssHunkOverlay-lsSwitch
		dc.w	lssHunkBreak-lsSwitch

HUNKSWITCHLIMIT	EQU	(*-lsSwitch)/2

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
lssHunkName:
		bsr	ReadName		; read name
		rts				; and ignore

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
lssHunkCode:
lssHunkData:
		;-- get hunk size
		bsr	GetLong
		;-- get new byte limit
		lsl.l	#2,d0
		move.l	d0,d4
		subq.l	#4,d4
		;-- get hunk base
		move.l	0(a4,d5.w),a2		; get table entry
		addq.l	#4,a2			; skip next segment pointer
		addq.l	#4,d5			; bump hunk number
		;-- load in the code
		move.l	d0,d2
		beq.s	lsshdReturn
		move.l	a2,a0
		move.l	lsv_ReadHandle(a5),d1
		movem.l	lsv_ReadFunc(a5),a1/a6
		jsr	(a1)
		cmp.l	d2,d0
		bne	lssFail
lsshdReturn:
		rts

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
lssHunkBSS:
		;-- get hunk size
		bsr	GetLong
		;-- get new byte limit
		move.l	d0,d4
		lsl.l	#2,d4
		subq.l	#4,d4
		;-- get hunk base
		move.l	0(a4,d5.w),a2		; get table entry
		addq.l	#4,a2			; skip next segment pointer
		addq.l	#4,d5			; bump hunk number
		;-- clear the bss area
		move.l	a2,a0
		moveq	#0,d1
		move.w	d0,d2			; low word
		swap	d0			; high word
		bra.s	lsshbssDBF
lsshbssLoop:
		move.l	d1,(a0)+
lsshbssDBF:
		dbf	d2,lsshbssLoop
		dbf	d0,lsshbssLoop
		rts

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
lssHunkReloc32:
		bsr	GetLong
		move.w	d0,d2
		beq.s	lsshrReturn
		bsr	GetLong
		cmp.l	d6,d0
		bgt.s	lssFail

		lsl.w	#2,d0
		move.l	0(a4,d0.w),d3		; get table entry
		addq.l	#4,d3			; skip next segment pointer
		subq.w	#1,d2
lsshrRelocLoop:
		bsr	GetLong
		;-- ensure within hunk
		bmi.s	lssFail
		cmp.l	d4,d0
		bgt.s	lssFail
		add.l	d3,0(a2,d0.l)		; add base to offset
		dbf	d2,lsshrRelocLoop
		bra.s	lssHunkReloc32

lsshrReturn:
		rts

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
lssHunkSymbol:
		bsr	ReadName		; flush symbol name
		tst.l	d0			; check symbol length
		beq.s	lsshsDone		; end of symbol hunk if zero
		bsr	GetLong			; flush symbol value
		bra.s	lssHunkSymbol
lsshsDone:
		rts

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
lssHunkDebug:
		bsr	GetLong
		move.w	d0,d2
		bra.s	lsshdDBF
lsshdLoop:
		bsr	GetLong
lsshdDBF:
		dbf	d2,lsshdLoop
		rts

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
lssHunkEnd:
		moveq	#-1,d4		; flag end in limit
		rts			; (it's OK to see EOF now)

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
lssFail:
lssHunkReloc16:
lssHunkReloc8:
lssHunkExt:
lssHunkHeader:
lssHunkCont:
lssHunkOverlay:
lssHunkBreak:
		moveq	#0,d4		; flag error
		addq.l	#4,a7		; pop switch return address
					; fall thru to lsFreeHunk
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

lsFreeHunk:
		move.l	a4,d0
		beq.s	lsCheckOK
		;-- free the temporary hunk table via Exec FreeMem
		move.l	d6,d0		; recover last slot
		addq.l	#1,d0		; last slot + 1 long words
		lsl.l	#2,d0		; in bytes
		move.l	a4,a1
		move.l	ABSEXECBASE,a6
		jsr	_LVOFreeMem(a6)

		;-- check for error
lsCheckOK:
		cmp.l	#-1,d4
		beq.s	lsOK

		;-- unload the segment
		move.l	lsv_Segment(a5),d1
		move.l	lsv_FreeFunc(a5),a1
		move.l	lsv_DataEnviron(a5),a6
		bsr	UnLoadSeg
		moveq	#0,d0
		bra.s	lsReturn

lsOK:
		;-- return list head BPTR as result
		move.l	lsv_Segment(a5),d0

lsReturn:
		move.l	a5,a6
		unlk	a6
		movem.l	(a7)+,d2-d6/a2-a5
		rts

lsFail:
		moveq	#0,d4
		bra.s	lsFreeHunk

; - - -	GetLong - - - - - - - - - - - - - - - - - - - - - - - - - - -
;
;   INPUT
;   a4	readHandle
;   a4	readFunc
;
;   RESULT
;   d0	next longword in stream
;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GetLong:
		subq.l	#4,a7
		move.l	a7,a0
		moveq	#4,d0
		move.l	lsv_ReadHandle(a5),d1
		movem.l	lsv_ReadFunc(a5),a1/a6
		jsr	(a1)
		cmp.l	#4,d0
		bne.s	gFail
		move.l	(a7)+,d0
		rts


; - - -	ReadName  - - - - - - - - - - - - - - - - - - - - - - - - - -
;
;   INPUT
;   a5	lsv structure
;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ReadName:
		bsr.s	GetLong
		lsl.l	#2,d0
		beq.s	rnReturn
		cmp.l	#MAXNAMELEN,d0
		bge.s	rnFail
		lea	lsv_Name(a5),a0
		move.l	d0,-(a7)
		move.l	lsv_ReadHandle(a5),d1
		movem.l	lsv_ReadFunc(a5),a1/a6
		jsr	(a1)
		cmp.l	(a7)+,d0
		bne.s	rnFail
rnReturn:
		rts
rnFail:
		addq.l	#4,a7
		bra.s	lsFail


; - - -	GetVector - - - - - - - - - - - - - - - - - - - - - - - - - -
;
;   INPUT
;   d0	vector size (without size prefix), in longwords, with memory
;	flags CHIP and FAST in bits 30 and 31
;
;   RESULT
;   a0	vector
;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GetVector:
		move.l	d0,d1
		lsl.l	#2,d0
		addq.l	#4,d0
		move.l	d0,-(a7)
		addq.l	#4,d0
		rol.l	#3,d1
		and.l	#6,d1		; mask to MEMF_FAST+MEMF_CHIP
		move.l	lsv_AllocFunc(a5),a1
		move.l	lsv_DataEnviron(a5),a6
		jsr	(a1)
		tst.l	d0
		bne.s	gvCacheSize
gFail:
		addq.l	#8,a7
		bra	lsFail
gvCacheSize:
		move.l	d0,a0
		move.l	(a7)+,(a0)
		addq.l	#4,a0
		rts


;------ UnLoadSeg ----------------------------------------------------
;
;	UnLoadSeg(segment, freeFunc, dataEnviron)
;	          d1       a1        a6
;
;	freeFunc(memory, size)
;	         a1      d0
;
;   EXCEPTIONS
;
;---------------------------------------------------------------------
UnLoadSeg:
		movem.l	d2/a2,-(a7)
		move.l	d1,d2
		move.l	a1,a2
		bra.s	usNext
usLoop:
		;-- free this hunk
		move.l	(a1),d2		; cache next segment
		move.l	-(a1),d0	; get size
		addq.l	#4,d0
		jsr	(a2)
usNext:
		lsl.l	#2,d2		; get next segment
		beq.s	ulDone
		move.l	d2,a1
		bra.s	usLoop

ulDone:
		movem.l	(a7)+,d2/a2
		rts

;------	AllocRecoverable ---------------------------------------------
;
;	memory = AllocRecoverable(size, requirements)
;	d0                        d0    d1
;
;---------------------------------------------------------------------
AllocRecoverable:
		movem.l	d2/d3/a2/a3/a6,-(a7)
		move.l	ABSEXECBASE,a6
		FORBID	a0

		;-- get first memory header
		move.l	MemList(a6),a0
		moveq	#0,d3			; guess noone big enough

		;-- cycle through memory headers
nextMemHeader:
		move.l	a0,a1
		move.l	(a1),d2			; check for end of list
		beq	noMemory
		;-- handle requirements
		move.l	d2,a0
		move.w	MH_ATTRIBUTES(a1),d2
		and.w	d1,d2
		cmp.w	d1,d2
		bne.s	nextMemHeader

		;-- search this memory for a big enough piece at the end
		move.l	MH_FIRST(a1),a2
checkMemChunk:
		cmp.l	MC_BYTES(a2),d0
		bhi.s	memTooSmall
		move.l	a2,a3			; this is a candidate
		move.l	MC_BYTES(a2),d3		;
memTooSmall:
		;-- get the next chunk
		move.l	MC_NEXT(a2),d2
		beq.s	noMoreChunks
		move.l	d2,a2
		bra.s	checkMemChunk

noMoreChunks:
		tst.l	d3			; check for candidates
		bne.s	gotChunk
		bra.s	nextMemHeader

gotChunk:
		sub.l	d0,d3			; determine excess capacity
		and	#(~MEM_BLOCKMASK),d3	; rounded down to even block
		add.l	d3,a3			; and adjust desired address

		move.l	a3,a1
		jsr	_LVOAllocAbs(a6)
allocEnd:
		PERMIT	a0
		movem.l	(a7)+,d2/d3/a2/a3/a6
		rts

noMemory:
		moveq	#0,d0
		bra.s	allocEnd


;------ Read ---------------------------------------------------------
;
;	actual = Read(readHandle, buffer, length)
;       d0            d1          a0      d0
;
;---------------------------------------------------------------------
Read:
		movem.l	d2/d3,-(a7)
		move.l	a0,d2
		move.l	d0,d3
		jsr	_LVORead(a6)
		movem.l	(a7)+,d2/d3
		rts

;------ FreeHigh -----------------------------------------------------
;
;	FreeHigh(memory, size)
;	         a1      d0
;
;---------------------------------------------------------------------
FreeHigh:
		move.l	a6,-(a7)
		move.l	ABSEXECBASE,a6
		jsr	_LVOFreeMem(a6)
		move.l	(a7)+,a6
		rts

;****** PrivateLoadSeg ***********************************************
;	PrivateLoadSeg(fileName)
;	
;	success = LoadSeg(allocFunc, freeFunc, readFunc,
;	d0                a0         a1        a2
;	                  readHandle, dataEnviron)
;	                  d1          a6
;*********************************************************************
_PrivateLoadSeg:
		movem.l	d2/a2/a6,-(a7)
		moveq	#0,d2
		lea	DLName(pc),a1
		moveq	#0,d0
		move.l	ABSEXECBASE,a6
		jsr	_LVOOpenLibrary(a6)
		tst.l	d0
		beq.s	plsDone
		move.l	d0,a6
		move.l	16(a7),d1
		move.l	#MODE_OLDFILE,d2
		jsr	_LVOOpen(a6)
		move.l	d0,d2
		beq.s	plsCloseLib
		move.l	d0,d1
		lea	AllocRecoverable(pc),a0
		lea	FreeHigh(pc),a1
		lea	Read(pc),a2
		bsr	LoadSeg
		move.l	d2,d1
		move.l	d0,d2
		jsr	_LVOClose(a6)
plsCloseLib:
		move.l	a6,a1
		move.l	ABSEXECBASE,a6
		jsr	_LVOCloseLibrary(a6)
plsDone:
		move.l	d2,d0
		movem.l	(a7)+,d2/a2/a6
		rts

DLName		dc.b	'dos.library',0
		ds.w	0
	
	END

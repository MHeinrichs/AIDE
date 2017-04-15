;Latest modification 31th of march 2016
	SECTION    driver,CODE
	include "exec/types.i"
	include "exec/nodes.i"
	include "exec/lists.i"
	include "exec/libraries.i"
	include "exec/devices.i"
	include "exec/io.i"
	include "exec/alerts.i"
	include "exec/initializers.i"
	include "exec/memory.i"
	include "exec/resident.i"
	include "exec/ables.i"
	include "exec/errors.i"
	include "exec/tasks.i"
	include "devices/timer.i"
	include "devices/scsidisk.i"
	include "libraries/expansion.i"
	include "libraries/configvars.i"
	include "libraries/configregs.i"
	include "libraries/expansionbase.i"
	include "libraries/filehandler.i"
	;Note that the next ASSIGNs ending with : need to be assigned
	;outside of this assembly source file, eg. in compilation scripts.
	;These are AmigaDos "links" to some certain file.
	include "lib/asmsupp.i";Various helper macros made by Commodore
	include "lib/ata.i"    ;ATA commands and other ATA codes
	include "lib/mydev.i"  ;select name etc, of the device
	include "lib/atid.i"   ;This include has the macros which
	                        ;are used to access a particular
	                        ;implementation of an Amiga to ATA 
	                        ;hardware interface, such as an A500 
	                        ;side slot interface or a Parallel port
	                        ;interface.
	;include "lib/bootinfo.i"  ;select name etc, of the device
	
	;include "lib/myscsi.i" ;

	;Routines and other values to be linked from "rdwt.asm"
	XREF  READOPE       ;These two constants are codes for
	XREF  WRITEOPE      ;the types of operation for IDERdWt
	XREF  ATARdWt       ;Read/Write routine
	XREF  ATARdWtLen
	XREF  EndCodeInit
	XREF  InitDrive     ;Drive initialisation routine
	XREF  SCSIDirectCmd ;SCSI direct command routine
	;XREF  blink         ;Routine that blinks the power LED
	XREF  ResetIDE
	
	XLIB  AddIntServer ;XLIB macro: XLIB Something => XREF _LVOSomething
	XLIB  RemIntServer 
	XLIB  Debug
	XLIB  InitStruct
	XLIB  InitCode
	XLIB  OpenLibrary
	XLIB  CloseLibrary
	XLIB  Alert
	XLIB  FreeMem
	XLIB  Remove
	XLIB  AllocMem
	XLIB  AddTask
	XLIB  RemTask
	XLIB  ReplyMsg
	XLIB  Signal
	XLIB  GetMsg
	XLIB  PutMsg
	XLIB  Wait
	XLIB  WaitPort
	XLIB  AllocSignal
	XLIB  SetTaskPri
	XLIB  GetCurrentBinding   ; Get list of boards for this driver
	XLIB  MakeDosNode
	XLIB  AddDosNode
	XLIB  Permit
	XLIB  Forbid
	XLIB  Delay
	XLIB  Alert
	XLIB  FindResident
	XLIB  InitResident
	XLIB  AllocConfigDev
	XLIB  AddConfigDev
	XLIB  AddBootNode
	XLIB  CacheClearU
	XLIB  CopyMem
	XLIB	CreateIORequest
	XLIB	DeleteIORequest
  XREF  _BeginIO
;The _intena address is the register which can be used to disable or 
;enable the interrupts in a way that they do not reach the 68000 CPU. 
_intena  equ   $dff09a ;;;ML


FirstAddress:
	moveq  #-1,d0 
	rts          ;Return in case this code was called as a program

;ROM-Tag 
initDDescrip:
	            ;STRUCTURE RT,0
	  DC.W    RTC_MATCHWORD    ; UWORD RT_MATCHWORD
	  DC.L    initDDescrip     ; APTR  RT_MATCHTAG
	  DC.L    EndCodeInit      ; APTR  RT_ENDSKIP
	  DC.B    RTF_AUTOINIT     ; UBYTE RT_FLAGS
	  DC.B    VERSION          ; UBYTE RT_VERSION
	  DC.B    NT_DEVICE        ; UBYTE RT_TYPE
	  DC.B    MYPRI            ; BYTE  RT_PRI
	  DC.L    myName           ; APTR  RT_NAME
	  DC.L    idString         ; APTR  RT_IDSTRING
	  DC.L    Init             ; APTR  RT_INIT
	            ; LABEL RT_SIZE

myName:     MYDEVNAME
dosName:    DOSNAME
timerName:	TIMERNAME
idString: IDSTRINGMACRO ;This is from MYDEVI: include file
myTaskName: MYTASKNAME
	cnop 0,4
myTaskName2: MYTASKNAME2
	; Force word alignment (even address)
	ds.w   0
Init:
	DC.L  MyDev_Sizeof      ; data space size
	DC.L  funcTable         ; pointer to function initializers
	DC.L  dataTable         ; pointer to data initializers
	DC.L  initRoutine       ; routine to run

funcTable:

	;------ standard system routines
	dc.l  Open
	dc.l  Close
	dc.l  Expunge
	dc.l  Null

	;------ my device definitions
	dc.l  BeginIO
	dc.l  AbortIO

	;------ function table end marker
	dc.l  -1

dataTable:
	INITBYTE LH_TYPE,NT_DEVICE
	INITLONG LN_NAME,myName
	INITBYTE LIB_FLAGS,LIBF_SUMUSED!LIBF_CHANGED
	INITWORD LIB_VERSION,VERSION
	INITWORD LIB_REVISION,REVISION
	INITLONG LIB_IDSTRING,idString
	DC.w     0
initRoutine:
	movem.l  d1/a0-a1/a3-a5,-(sp) ;Preserve ALL modified registers
	move.l   d0,a5
;------------------
; DO INITIALIZE THE INTERFACE NOW, macro from INTERFACEI: include file
	INITATAINTERFACE
;------------------
 
	;------ save a pointer to exec
	move.l   a6,md_SysLib(a5)

	;------ save a pointer to our loaded code
	move.l   a0,md_SegList(a5)

init1
  ;find the location of ATARdWt
  lea    ATARdWt,a0
  move.l a0,md_ATARdWt(a5)
  PRINTF 1,<'Original ATARdWT Position: %lx',13,10>,a0
  lea    Task_Begin,a0
  move.l a0,md_task(a5)
  PRINTF 1,<'Original Task    Position: %lx',13,10>,a0
;  cmp.l  #$600000,a0
;  blt.s  relocate_atardwt
;  cmp.l  #$A00000,a0
;  bgt.s  relocate_atardwt
;  bra.s  end_relocate_atardwt ;already in the right piece of fastram!
relocate_atardwt  
	MOVE.l ATARdWtLen,d0 ; Länge nach D0 Danke Thor!
	MOVE.l #MEMF_PUBLIC!MEMF_CLEAR,d1
  CALLSYS AllocMem
  tst.l  d0
  beq.s  end_relocate_atardwt
  move.l d0,md_ATARdWt(a5)
  PRINTF 1,<'Relocated ATARdWT Position: %lx',13,10>,d0
  lea    ATARdWt,a0
	MOVE.l ATARdWtLen,d0 ; Länge nach D0 Danke Thor!
  move.l md_ATARdWt(a5),a1
  CALLSYS CopyMem
  cmpi.w #37,LIB_VERSION(a6)
  blt.s end_relocate_atardwt
  CALLSYS CacheClearU
end_relocate_atardwt:
relocate_task:
	MOVE.l TaskLen,d0 ; Länge nach D0 Danke Thor!
	MOVE.l #MEMF_PUBLIC!MEMF_CLEAR,d1
  CALLSYS AllocMem
  tst.l  d0
  beq.s  end_relocate_task
  move.l d0,md_task(a5)
  PRINTF 1,<'Relocated ATARdWT Position: %lx',13,10>,d0
  lea    Task_Begin,a0
	MOVE.l TaskLen,d0 ; Länge nach D0 Danke Thor!
  move.l md_task(a5),a1
  CALLSYS CopyMem
  cmpi.w #37,LIB_VERSION(a6)
  blt.s end_relocate_task
  CALLSYS CacheClearU
end_relocate_task:


	move.l   a5,d0
	bra      init_end
init_error:
	moveq    #0,d0
init_end:
	movem.l  (sp)+,d1/a0-a1/a3-a5
	rts

Open:    ; ( device:a6, iob:a1, unitnum:d0, flags:d1 )
	movem.l  d2-d3/a2-a4,-(sp)
	move.l   a1,a2                   ; save the iob
	PRINTF 1,<' Opening device %ld',13,10>,d0
	;------ see if the unit number is in range
	moveq    #0,d3
	cmp.l    #10,d0                  ;convert: scsi unit 10 = dos unit 1
	bne      opn1
	move.l   #4,d3                   ;set offset for unit table
	moveq    #0,d0
	move.b   #1,d0
opn1
	moveq    #0,d2
	move.b   d0,d2                   ; save unit number
	cmp.b    #MD_NUMUNITS,d0         ;Allow unit numbers 0 and 1.
	bge      Open_Error              ;unit number is out of range
  PRINTF 1,<' Opening unit: %lx offset %ld',13,10>,d0,d3
	
	;------ see if the unit is already initialized
	lea.l    md_Units(a6,d3.l),a4
	move.l   (a4),d0
	bne.s    Open_UnitOK
	;------ Try and conjure up a unit
	bsr      InitUnit
	;------ see if it initialized OK
	move.l   (a4),d0
	beq      Open_Error
  PRINTF 1,<'Init unit ok: %lx',13,10>,d0

Open_UnitOK:

	move.l   d0,a3                   ;unit pointer in a3
	move.l   d0,IO_UNIT(a2)

	;------ mark us as having another opener
	addq.w   #1,LIB_OPENCNT(a6)
	addq.w   #1,UNIT_OPENCNT(a3)
	;------ prevent delayed expunges
	bclr     #LIBB_DELEXP,md_Flags(a6)
	;If the IDE drive has not been initialised previously, do it now
	cmp.b    #TRUE,mdu_firstcall(a3)
	bne.s      nav1
	bsr      InitDrive ;Call the IDE drive initialisation routine
	IFGE	DEBUG_DETAIL-1
		move.w   mdu_drv_type(a3),d0  ;known drive type
  	PRINTF 1,<'Init drive ok, drivetype: %ld',13,10>,d0
  ENDC
	move.b   #FALSE,mdu_firstcall(a3)
nav1
	cmp.w    #UNKNOWN_DRV,mdu_drv_type(a3)  ;known drive type
	beq      Open_Error                     ; unknowns cannot be opened!
	moveq    #0,d0
	move.b   d0,IO_ERROR(a2)
	move.b   #NT_REPLYMSG,LN_TYPE(a2) ;IMPORTANT: Mark IORequest as "complete"
Open_End
	PRINTF 1,<'Opend ide.device Result: %lx ',13,10>,d0
	subq.w   #1,LIB_OPENCNT(a6) ;** End of expunge protection <|>
	movem.l  (sp)+,d2-d3/a2-a4
	rts
Open_Error:
	bsr      FreeUnit
	moveq    #IOERR_OPENFAIL,d0
	move.b   d0,IO_ERROR(a2)
	move.l   d0,IO_DEVICE(a2)    ;IMPORTANT: trash IO_DEVICE on open failure
	PRINTF 1,<'Open device error',13,10>	
	bra      Open_End

;----------------------------------------------------------------------------
; There are two different things that might be returned from the Close
; routine.  If the device wishes to be unloaded, then Close must return
; the segment list (as given to Init).  Otherwise close MUST return NULL.

Close:      ;( device:a6, iob:a1 )
	MOVEM.l  d1/a2-a3,-(sp)

	MOVE.l   a1,a2
	MOVE.l   IO_UNIT(a2),a3

	;------ make sure the iob is not used again
	MOVEQ    #-1,d0
	MOVE.l   d0,IO_UNIT(a2)
	MOVE.l   d0,IO_DEVICE(a2)

	;------ see if the unit is still in use
	SUBQ.w   #1,UNIT_OPENCNT(a3)

  ;bne.s    Close_Device
  ;bsr      ExpungeUnit

Close_Device:
	;------ mark us as having one fewer openers
	MOVEQ.l  #0,d0
  subq.w   #1,LIB_OPENCNT(a6)
	;------ see if there is anyone left with us open
  bne.s    Close_End
	;------ see if we have a delayed expunge pending
  btst     #LIBB_DELEXP,md_Flags(a6)
  beq.s    Close_End
	;------ do the expunge
  bsr      Expunge
Close_End:
	MOVEM.l  (sp)+,d1/a2-a3
	RTS

;------- Expunge -----------------------------------------------------------
;
; Expunge is called by the memory allocator when the system is low on
; memory.
;
; There are two different things that might be returned from the Expunge
; routine.  If the device is no longer open then Expunge may return the
; segment list (as given to Init).  Otherwise Expunge may set the
; delayed expunge flag and return NULL.
;
; One other important note: because Expunge is called from the memory
; allocator, it may NEVER Wait() or otherwise take long time to complete.
;
;	A6	    - library base (scratch)
;	D0-D1/A0-A1 - scratch
;

Expunge:    ;( device: a6 )
;
;	movem.l  d1/d2/a5/a6,-(sp)
;
;	move.l   a6,a5
;
;	move.l   md_SysLib(a5),a6
;	;------ see if anyone has us open
;  tst.w    LIB_OPENCNT(a5)
;
;  beq      go_ahead_expunge
;	;------ it is still open.  set the delayed expunge flag
;  bset     #LIBB_DELEXP,md_Flags(a5)
;  CLEAR    d0
;  bra.s    Expunge_End
;go_ahead_expunge:
;	;------ go ahead and get rid of us.  Store our seglist in d2
;  move.l   md_SegList(a5),d2
;	;------ unlink from device list
;  move.l   a5,a1
;  CALLSYS  Remove
;  move.l   md_DosLib(a5),a1
;  CALLSYS  CloseLibrary
;
;
;	;device specific closings here...
;	move.l 	md_ATARdWt(a5),a1
;  move.l	(a1),d0
;  lea     ATARdWt,a1
;  cmp.l   a1,d0
;  beq.s   no_ata_rdwt_relocate
;
;  move.l	 d0,a1
;	move.l   #ATARdWtLen,d0
;	CALLSYS  FreeMem
;no_ata_rdwt_relocate:
;	move.l 	md_task(a5),a1
; move.l	(a1),d0
; lea     Task_Begin,a1
; cmp.l   a1,d0
; beq.s   no_task_relocate
;
; move.l	 d0,a1
;	move.l   #TaskLen,d0
;	CALLSYS  FreeMem
;
;no_task_relocate:
;	;------ free our memory
;  CLEAR   d0
;  CLEAR   d1
;  move.l   a5,a1
;  move.w   LIB_NEGSIZE(a5),d1
;  sub.w    d1,a1
;  add.w    LIB_POSSIZE(a5),d0
;  add.l    d1,d0
;  CALLSYS  FreeMem
;	;------ set up our return value
;	move.l   d2,d0
;Expunge_End:
;  movem.l  (sp)+,d1/d2/a5/a6
;  rts

Null:
	moveq    #0,d0
	rts
	
InitUnit:      ;( d2:unit number, a3:scratch, a6:devptr )

	movem.l  D1-d4/A0-a2,-(sp)
  
	;------ allocate unit memory
	move.l   #0,A3 ;clear a3!
	move.l   #MyDevUnit_Sizeof,d0
	move.l   #MEMF_PUBLIC!MEMF_CLEAR,d1
	LINKSYS  AllocMem,md_SysLib(a6)
	tst.l    d0
	beq      InitUnit_End
	move.l   d0,a3
	
	moveq.l  #0,d0                   ;Dont need to re-zero it
	move.l   a3,a2                   ;InitStruct is initializing the UNIT
	lea.l    mdu_Init(PC),A1
	LINKSYS  InitStruct,md_SysLib(a6)
	PRINTF 1,<'Init Struct passed ',13,10>
	move.l   md_ATARdWt(a6),mdu_ATARdWt(a3) ; copy the relocated ATARdWt-Routine

	moveq.l  #0,d0
	;move.b   d2,d0                   ;unit number
	cmp.b    #0,d2
	beq.s    initunit0
	bset     #MDUB_SLAVE,d0                 ;set slave bit	
initunit0	
	move.b   d0,mdu_UnitNum(a3)      ;initialize unit number
	move.l   a6,mdu_Device(a3)       ;initialize device pointer
	
	;------ start up the unit task.  We do a trick here --
	;------ we set his message port to PA_IGNORE until the
	;------ new task has a change to set it up.
	;------ We cannot go to sleep here: it would be very nasty
	;------ if someone else tried to open the unit
	;------ (exec's OpenDevice has done a Forbid() for us --
	;------ we depend on this to become single threaded).
	
	;------ Initialize the stack information
	lea	    mdu_stack(a3),a0          ; Low end of stack
	move.l   a0,mdu_tcb+TC_SPLOWER(a3)
	lea	    MYPROCSTACKSIZE(a0),a0    ; High end of stack
	move.l   a0,mdu_tcb+TC_SPUPPER(a3)
	move.l   a3,-(A0)                  ; argument -- unit ptr (send on stack)
	move.l   a0,mdu_tcb+TC_SPREG(a3)
	lea	     mdu_tcb(a3),a0
	move.l   a0,MP_SIGTASK(a3)
	btst.b   #MDUB_SLAVE,mdu_UnitNum(a3)
	bne.s    no_name_change
	lea	     myTaskName2(pc),a0
	move.l   a0,mdu_tcb+LN_NAME(a3)
no_name_change:
	
	;------ initialize the unit's message port's list
	lea	    MP_MSGLIST(a3),a0
	NEWLIST  a0			;<- IMPORTANT! Lists MUST! have NEWLIST
									;work magic on them before use.  (AddPort()
									;can do this for you)

;   Startup the task
	lea	     mdu_tcb(a3),a1
	move.l   md_task(A6),a2
	;lea			 Task_Begin(pc),A2
	move.l   a3,-(sp)      ; Preserve UNIT pointer
	lea	    -1,a3	  ; generate address error
		  						; if task ever "returns" (we RemTask() it
		  						; to get rid of it...)
	CLEAR   d0
	LINKSYS AddTask,md_SysLib(a6)
	move.l   (sp)+,a3      ; restore UNIT pointer
	
;	;set up a message port
;	
;	;get memory for the intserver
;	moveq.l	 #IS_SIZE,d0		get interrupt vector memory
;	move.l   #MEMF_PUBLIC!MEMF_CLEAR,d1
;	LINKSYS  AllocMem,md_SysLib(a6)
;	tst.l    d0
;	bne.s    build_interrupt
;	bsr			 InitUnit_Cleanup
;	moveq.l  #-1,D0 ;error
;	bra			 InitUnit_End
;build_interrupt:
;	;store and setup
;	MOVE.l   d0,mdu_timeinterrupt(A3);store interrupt in unit structure (maybe for cleanup)
;	MOVE.l	 D0,A1 ; put the IV in a1
;	MOVE.b	 #NT_INTERRUPT,LN_TYPE(a1)
;	MOVE.b	 #MYTIMEOUTPRI,LN_PRI(a1) ; this is our priority
;	MOVE.l	 a3,IS_DATA(a1) ;server can see our unit structure
;	LEA  	   TimeoutServer(pc),A0 ;where the code is
;	MOVE.l	 a0,IS_CODE(a1)
;	LEA 	   myName(pc),a0
;	MOVE.l	 a0,LN_NAME(a1)
;
;	;get the memory for the port
;	moveq.l  #MP_SIZE,d0
;	move.l   #MEMF_PUBLIC!MEMF_CLEAR,d1
;	LINKSYS  AllocMem,md_SysLib(a6)
;	tst.l    D0
;	bne.s    build_msgport
;	bsr			 InitUnit_Cleanup
;	moveq.l  #-1,D0 ;error
;	bra			 InitUnit_End
;build_msgport:
;	;store and setup
;	move.l   d0,mdu_msgport(A3);store message port in unit structure
;	move.l	 D0,A1; put the port in a1
;	move.b	 #PA_SOFTINT,MP_FLAGS(A1)	;soft interrupt
;	move.l	 mdu_timeinterrupt(A3),MP_SIGTASK(A1); pointer to interrupt vector
;	move.b	 #NT_MSGPORT,LN_TYPE(a1)	;it's a simple message port
;	lea	     MP_MSGLIST(A1),a0;init the list
;	NEWLIST  a0			;<- IMPORTANT! Lists MUST! have NEWLIST
;	;build timer request
;	move.l	 #IOTV_SIZE,D0
;	LINKSYS	 CreateIORequest,md_SysLib(a6)
;	tst.l    D0
;	bne.s    build_timerrequest
;	bsr			 InitUnit_Cleanup
;	moveq.l  #-1,D0 ;error
;	bra			 InitUnit_End
;build_timerrequest:
;	move.l   D0,mdu_timerequest(A3)
;	move.l   D0,A1
;  MOVE.w	 #TR_ADDREQUEST,IO_COMMAND(A1) ;set up command
;	MOVE.l	 #MICRODELAY,IOTV_TIME+TV_MICRO(A1) ;set up delay 
;	;open timer device
;	lea			 timerName(pc),A0 ;timer.device
;	moveq.l	 #0,D0 ;unit 0
;	moveq.l	 #UNIT_MICROHZ,D1 ; CIA timer
;  ;the ioRequest is already in A1
;	LINKSYS  OpenDevice,md_SysLib(a6) ; we should close it somewhere...
	;------ mark us as ready to go
	moveq.l  #0,d0
	btst     #MDUB_SLAVE,mdu_UnitNum(a3)                   ;test unit number
	beq.s    InitUnit_setUnitAdress
	moveq.l  #4,d0                   ;set offset for unit 1 (slave)
InitUnit_setUnitAdress:
	PRINTF 1,<'Setting address %lx at offset %ld',13,10>,a3,d0
	move.l   a3,md_Units(a6,d0.l)    ;set unit table
 
InitUnit_End:
	movem.l  (sp)+,d1-d4/a0-a2
	rts
	
InitUnit_Cleanup: ;( a3:unitptr, a6:deviceptr )
  movem.l   A0-A1,-(sp)
  CMP.l 	  #0,a3
	beq.s 		  InitUnit_CleanupEnd
	cmp.l 		  #0,mdu_msgport(A3)
	beq.s 		  InitUnit_CleanupInterrupt
	moveq.l   #MP_SIZE,D0
	move.l		  mdu_msgport(A3),A1
	CALLSYS  	FreeMem
InitUnit_CleanupInterrupt:
  cmp.l 		  #0,mdu_timeinterrupt(A3)
  beq.s 		  InitUnit_Cleanupiorequest
	moveq.l 	  #IV_SIZE,D0
	move.l		  mdu_timeinterrupt(A3),A1
	CALLSYS  	FreeMem
InitUnit_Cleanupiorequest:
	cmp.l 		  #0,mdu_timerequest(A3)
	BEQ.s			 InitUnit_CleanupUnit
	MOVE.l		  mdu_timerequest(A3),A0
	CALLSYS  	DeleteIORequest
InitUnit_CleanupUnit:
	move.l  	  #MyDevUnit_Sizeof,D0
	move.l		  A3,A1
	CALLSYS  	FreeMem
InitUnit_CleanupEnd:
	moveq.l 	  #0,D0 
 	movem.l  (sp)+,A0-A1
  RTS       
  
FreeUnit:   ;( a3:unitptr, a6:deviceptr )

	tst.l    (a3)   ;valid pointer
	beq FreeUnit_End 
	move.l   a3,a1
	move.l   #MyDevUnit_Sizeof,d0

	LINKSYS  FreeMem,md_SysLib(a6)
FreeUnit_End:
	rts


ExpungeUnit:   ;( a3:unitptr, a6:deviceptr )
   move.l   d2,-(sp)

   ;------ get rid of the unit's task.  We know this is safe
   ;------ because the unit has an open count of zero, so it
   ;------ is 'guaranteed' not in use.
   lea	 mdu_tcb(a3),a1
   LINKSYS RemTask,md_SysLib(a6)

   ;------ save the unit number
   moveq   #0,d2
   move.b  mdu_UnitNum(a3),d2

   ;------ free the unit structure.
   bsr	   FreeUnit

   ;------ clear out the unit vector in the device
   btst    #MDUB_SLAVE,d2
   bne.s   ExpungeUnit_Slave
   moveq.l  #0,d2
   bra.s   ExpungeUnit_ClearUnit
ExpungeUnit_Slave:
   move.l  #4,d2
ExpungeUnit_ClearUnit:   
   clr.l   md_Units(a6,d2.l)
   move.l  (sp)+,d2
	rts

cmdtable:
;32bits ;Address     ;number of command; and its bit mask
	DC.L  Invalid     ;0 $00000001
	DC.L  MyReset     ;1 $00000002
	DC.L  Read        ;2 $00000004    
	DC.L  Write       ;3 $00000008
	DC.L  Update      ;4 $00000010
	DC.L  Clear       ;5 $00000020
	DC.L  MyStop      ;6 $00000040
	DC.L  Start       ;7 $00000080
	DC.L  Flush       ;8 $00000100
	DC.L  Motor       ;9 $00000200
	DC.L  Seek        ;10 $00000400    seek (NO-OP)
	DC.L  Format      ;11 $00000800    format -> WRITE for harddisk
	DC.L  MyRemove    ;12 $00001000    remove (NO-OP)
	DC.L  ChangeNum   ;13 $00002000
	DC.L  ChangeState ;14 $00004000
	DC.L  ProtStatus  ;15 $00008000  protstatus      (Returns 0)
	DC.L  RawRead     ;16  Not supported (INVALID)
	DC.L  RawWrite    ;17 Not supported (INVALID)
	DC.L  GetDriveType;18 Get drive type   (Returns 1)
	DC.L  GetNumTracks;19 Get number of tracks (Returns NUMTRKS)
	DC.L  AddChangeInt;20 Add disk change interrupt (NO-OP)
	DC.L  RemChangeInt;21 Remove disk change interrupt (NO-OP)
	DC.L  Invalid     ;22
	DC.L  Invalid     ;23
	DC.L  td64_read64 ;24
	DC.L  td64_write64;25
	DC.L  td64_seek64 ;26
	DC.L  td64_format64;27
	DC.L  SCSIDirect   ;28
	DC.L  MyCMD        ;29
;  +------------+-------------+
;  |TD_READ64   |     24      |
;  |TD_WRITE64  |     25      |
;  |TD_SEEK64   |     26      |
;  |TD_FORMAT64 |     27      |
;  +------------+-------------+

cmdtable_end:

; this define is used to tell which commands should not be queued
; command zero is bit zero.
; The immediate commands are Invalid, Reset, Stop, Start, Flush
;; (BIT MASKS ARE ORed HERE. (max 32 commands then, by the way.)
IMMEDIATES  EQU   $200001C3

; These commands can NEVER be done "immediately" if using interrupts,
; since they would "wait" for the interrupt forever!
; Read, Write, Format AND td64_read64, td64_write64, td64_format64
NEVERIMMED  EQU   $1000080C+$07000000

;
; BeginIO starts all incoming io.  The IO is either queued up for the
; unit task or processed immediately.
;

BeginIO: ; ( iob: a1, device:a6 )
	movem.l  d0/d1/a0/a3,-(sp)
	;------ bookkeeping
	move.b   #NT_MESSAGE,LN_TYPE(a1)
	move.l   IO_UNIT(a1),a3
	;------ see if the io command is within range
	move.w   IO_COMMAND(a1),d0
	cmp.w    #MYDEV_END,d0
	bcc      BeginIO_NoCmd
	cmp.w    #9,d0
	beq      BeginIO_NoCmd        ;filter MOTOR
	DISABLE  a0
	;------ process all immediate commands no matter what
	move.l   #IMMEDIATES,d1
	btst     d0,d1
	bne.s    BeginIO_Immediate
	;------ queue all NEVERIMMED commands no matter what
	; others commands too

	;------ we need to queue the device.  mark us as needing
	;------ task attention.  Clear the quick flag
BeginIO_QueueMsg:
	BSET     #UNITB_INTASK,UNIT_FLAGS(a3)
	bclr     #IOB_QUICK,IO_FLAGS(a1)
	ENABLE   a0
	move.l   a3,a0
	LINKSYS  PutMsg,md_SysLib(a6)
	bra      BeginIO_End
BeginIO_Immediate:
	ENABLE   a0
	bsr      PerformIO
BeginIO_End:
	movem.l  (sp)+,d0/d1/a0/a3
	rts
BeginIO_NoCmd:
	move.b   #IOERR_NOCMD,IO_ERROR(a1)
	bra.s    BeginIO_End


; PerformIO actually dispatches an io request.  It expects a3 to already
; have the unit pointer in it.  a6 has the device pointer (as always).
; a1 has the io request.  Bounds checking has already been done on
; the io request.
PerformIO:  ; ( iob:a1, unitptr:a3, devptr:a6 )
	;move.l   a2,-(sp)
	;move.l   a1,a2
;	;now start the timer
;	move.l   A1,-(sp)	
;	MOVE.l	 #30000,mdu_timeout(A3) ; io must complete in 30 sec!
;	MOVE.l   mdu_timerequest(A3),A1
;	jsr  		 _BeginIO
;	move.l   (sp)+,A1

	moveq    #0,d0                ;clear D0
	clr.b    IO_ERROR(A1)         ;No error so far
	move.w   IO_COMMAND(A1),d0
	lsl      #2,d0                ;Multiply by 4 to get table offset
	lea      cmdtable(pc),a0
	move.l   0(a0,d0.w),a0
	jsr      (a0) ; JSR TO THE ADDRESS WE READ FROM THE CMD TABLE
	;move.l   (sp)+,a2
;	MOVE.l	 #0,mdu_timeout(A3) ;disable the timeoutint
	rts


; TermIO sends the IO request back to the user.  It knows not to mark
; the device as inactive if this was an immediate request or if the
; request was started from the server task.
TermIO:     ;( iob:a1, unitptr:a3, devptr:a6 )
	move.w   IO_COMMAND(a1),d0
	move.l   #IMMEDIATES,d1
	btst     d0,d1
	bne.s    TermIO_Immediate
	;------ we may need to turn the active bit off.
	btst     #UNITB_INTASK,UNIT_FLAGS(a3)
	bne.s    TermIO_Immediate
	;------ the task does not have more work to do
	bclr     #UNITB_ACTIVE,UNIT_FLAGS(a3)
TermIO_Immediate:
	;------ if the quick bit is still set then we don't need to reply
	;------ msg -- just return to the user.
	btst     #IOB_QUICK,IO_FLAGS(a1)
	bne.s    TermIO_End
	LINKSYS  ReplyMsg,md_SysLib(a6)
TermIO_End:
	rts

ChangeNum:
	cmp.w    #ATA_DRV,mdu_drv_type(a3)
	beq      ClrIOActual
	cmp.w    #SATA_DRV,mdu_drv_type(a3)
	beq      ClrIOActual
	move.l   mdu_change_cnt(a3),IO_ACTUAL(a1)
	bsr      TermIO
	rts

ChangeState:
	cmp.w    #ATA_DRV,mdu_drv_type(a3)
	beq      ClrIOActual
	cmp.w    #SATA_DRV,mdu_drv_type(a3)
	beq      ClrIOActual
	move.l   mdu_no_disk(a3),IO_ACTUAL(a1)
	bsr      TermIO
	rts

MyCMD:
	cmp.l    #1,IO_ACTUAL(a1)           ;if io_actual==1 -> motor control
	bne      MyError
MyMotor:                               ;park drive heads and stop motor
	movem.l  d0-d2,-(sp)  
	move.w   mdu_motor(a3),d0
	move.l   d0,IO_ACTUAL(a1)			;copy a long to IO_ACTUAL(a1)
	tst.l    IO_LENGTH(a1)
	bne      mtr1
	WAITNOTBSY d1,d2
	beq      mtr1
	moveq    #0,d0
	move.b   mdu_UnitNum(a3),d0
	WATABYTE d0,TF_DRIVE_HEAD
	DLY5US
	WATABYTE #ATA_RECALIBRATE,TF_COMMAND
	WAITNOTBSY d1,d2
	beq      mtr1
	WATABYTE d0,TF_DRIVE_HEAD
	DLY5US
	WATABYTE #ATA_STANDBY_IMMEDIATE,TF_COMMAND
	WAITNOTBSY d1,d2
	RATABYTE TF_STATUS,d0
	move.w   #FALSE,mdu_motor(a3)
mtr1
	movem.l  (sp)+,d0-d2  
	bsr      TermIO
	rts
MyError:
	move.b   #IOERR_NOCMD,IO_ERROR(a1)
	bsr      TermIO
	rts

;perform packet command for ATAPI drive and emulate basic packets
;for ATA drives
SCSIDirect     ;( iob:a1, unitptr:a3, devptr:a6 )
  PRINTF 2,<'SCSI Direct',13,10>

	cmp.w    #ATA_DRV,mdu_drv_type(a3)
	beq      EmulateSCSI
	cmp.w    #SATA_DRV,mdu_drv_type(a3)
	beq      EmulateSCSI
	movem.l  a2-a3/a4/a6/d2/d7,-(sp)
	movem.l  a6,-(sp)
	move.l   IO_DATA(a1),a6
	clr.l    IO_ACTUAL(a1)
	clr.l    scsi_Actual(a6)            ;initially, no data moved
	move.l   scsi_Data(a6),a0
	move.l   scsi_Length(a6),d0
	move.l   scsi_Command(a6),a2
	move.l   IO_UNIT(a1),a3             ;get unit pointer
	moveq    #0,d2
	move.b   mdu_UnitNum(a3),d2

	jsr   SCSIDirectCmd                 ;perform packet command

	cmp.b    #$50,scsi_Status(a6)       ;ATAPI status 50h means OK
	bne      rkf1
	clr.b    scsi_Status(a6)            ;convert to scsi_Status OK = 0
rkf1
	movem.l  (sp)+,a6
	bsr      TermIO
	movem.l  (sp)+,a2-a3/a4/a6/d2/d7
	rts

;emualte scsi packets for ATAPI drive
EmulateSCSI
	movem.l  d0/d1/a0/a2/a6,-(sp)
	movem.l  d1/a6,-(sp)
	move.l   IO_DATA(a1),a6
	clr.l    IO_ACTUAL(a1)
	clr.w    scsi_SenseActual(a6)
	clr.l    scsi_Actual(a6)
	move.b   #1,scsi_Status(a6)
	move.l   scsi_Command(a6),a0     ; Which packet to emulate?
	cmp.b    #$12,(a0)               ; Inquiry
	beq      scsi_inq
	cmp.b    #$25,(a0)               ; Read Recorded Capacity
	beq      scsi_cap
	cmp.b    #$1A,(a0)               ; Mode sense(6)
	beq      scsi_ms
	cmp.b    #$00,(a0)               ; Test Unit Ready
	beq      escsi_ok
	cmp.b    #$08,(a0)               ; Read(6)
	beq      scsi_r6
	bra      escsi4
scsi_r6                             ; Read(6) packet
  PRINTF 1,<'Emulate SCSI Read-Package',13,10>
	move.l   (a0),d1
	and.l    #$001FFFFF,d1
	mulu     #512,d1
	moveq    #0,d0
	move.b   4(a0),d0
	mulu     #512,d0
	move.l   d0,-(sp)
	move.l   scsi_Data(a6),a0
	clr.b    scsi_Status(a6)
	move.w   IO_COMMAND(a1),-(sp)
	move.w   #CMD_READ,IO_COMMAND(a1)
	move.l   a1,a2
	move.l   a1,-(sp)
	move.l   mdu_ATARdWt(a3),a1
  jsr      (a1)
	move.l   (sp)+,a1
	move.w   (sp)+,IO_COMMAND(a1)
	move.l   (sp)+,scsi_Actual(a6)
	tst.b    d0
	beq      escsi4
	move.b   #1,scsi_Status(a6)
	bra      escsi4
scsi_ms                             ; Mode sense(6) packet
  PRINTF 1,<'Emulate SCSI Mode Sense',13,10>
	moveq    #0,d0
	move.b   4(a0),d0 ; allocation length
	move.l   d0,d1
	cmp.l    #28,d0
	ble      escsi12
	move.l   #28,d1
escsi12
	move.b   2(a0),d0
	and.b    #$3F,d0
	cmp.b    #$03,d0
	beq      page03
	cmp.b    #$04,d0
	beq      page04
	bra      escsi4
page03
  PRINTF 1,<'Emulate SCSI Mode Sense Page 3 ',13,10>
	move.l   d1,scsi_Actual(a6)
	subq.l   #1,d1
	bmi      escsi4
	;the data is filled in the init-routine
	lea      mdu_EmulMSPage3(a3),a2
	bra      escsi15
page04
  PRINTF 1,<'Emulate SCSI Mode Sense Page 4 ',13,10>
	move.l   d1,scsi_Actual(a6)
	subq.l   #1,d1
	bmi      escsi4
	lea      mdu_EmulMSPage4(a3),a2
escsi15
	move.l   scsi_Data(a6),a0
escsi13
	move.b   (a2)+,(a0)+
	dbra     d1,escsi13
	clr.b    scsi_Status(a6)   
	bra      escsi4
scsi_cap                               ; Read Recorded Capacity packet
  PRINTF 1,<'Emulate SCSI Capacity',13,10>
	cmp.w    #CHS_ACCESS,mdu_lba(a3)
	beq 		chscapa
	move.l   mdu_numlba(a3),d0
;   cmp.w    #LBA28_ACCESS,mdu_lba(a3)
;   beq 		setcapa
;   move.l   mdu_numlba48(a3),d1
;   bra		setcapa ;less than 8 gig->lba28?
;   and.l	#$0F,d1 ;just take the lower 4 bit! -> should read lba32 ;)
;   ror.l	#4,d1
;   or.l     d1,d0
	bra		setcapa   
chscapa
	move.l   mdu_heads(a3),d0
	move.l   mdu_sectors_per_track(a3),d1
	mulu     d1,d0
	move.l   mdu_cylinders(a3),d1
	mulu     d1,d0
setcapa   
	move.l   scsi_Data(a6),a0
	move.l   d0,(a0)+
	move.l   #512,(a0)
	clr.b    scsi_Status(a6)
	move.l   #8,scsi_Actual(a6)
	bra      escsi4
scsi_inq                               ; Inquiry packet
  PRINTF 1,<'Emulate SCSI Inquiery',13,10>
	move.l   scsi_Length(a6),d0
	move.l   d0,d1
	cmp.l    #36,d0
	ble      escsi2
	move.l   #36,d1
escsi2
	move.l   d1,scsi_Actual(a6)
	subq.l   #1,d1
	bmi      escsi4
	move.l   scsi_Data(a6),a0
	;the data is filled in the init-routine
	lea      mdu_EmulInquiry(a3),a2
escsi3
	move.b   (a2)+,(a0)+
	dbra     d1,escsi3
	clr.b    scsi_Status(a6)
	bra      escsi4
escsi_ok
  PRINTF 1,<'Emulate SCSI Test Unit Ready ',13,10>
	clr.b    scsi_Status(a6)
escsi4
	IFGE	DEBUG_DETAIL-1	
	moveq    #0,d1
  move.b   scsi_Status(a6),d1
  PRINTF 1,<'Emulate SCSI End. Status= %ld',13,10>,d1
  ENDC
	movem.l  (sp)+,d1/a6
	bsr   TermIO
	movem.l  (sp)+,d0/d1/a0/a2/a6
	rts


AbortIO:
    moveq   #IOERR_NOCMD,d0 ;return "AbortIO() request failed"
    rts
RawRead:       ; 10 Not supported   (INVALID)
RawWrite:      ; 11 Not supported   (INVALID)
Invalid:
GetNumTracks:
Motor:
	move.b   #IOERR_NOCMD,IO_ERROR(a1)
	bsr      TermIO
	rts

MyReset:
	clr.l    IO_ACTUAL(a1)
	bsr      TermIO
	bsr  ResetIDE
	rts
AddChangeInt:
RemChangeInt:
MyRemove:
Seek:
td64_seek64 ;ML 
Remove:
ProtStatus:                      ; Indicate drive isnt protected
ClrIOActual:
	clr.l    IO_ACTUAL(a1)
	bsr      TermIO
	rts ;; All the above commands (MyReset ... ProtStatus return 0 in IO_ACTUAL)

GetDriveType:
	move.l   #1,IO_ACTUAL(a1)     ; Make it look like 3.5 inch
	bsr      TermIO
	rts

td64_read64       ;a1 has the commandstructure
  PRINTF 2,<'td64_read64',13,10>
	move.l   #READOPE,a0
	bra      td64j
td64_write64
  PRINTF 2,<'td64_write64 ',13,10>
td64_format64
	move.l   #WRITEOPE,a0
td64j
IO_HIGHOFFSET EQU IO_ACTUAL      ;trackdisk64 
	move.l   IO_HIGHOFFSET(a1),d0 ; high offset
	bra      drwf
Read:
  PRINTF 2,<'R  '>
	move.l   #READOPE,a0
	bra		jee32f
Write:
Format:
  PRINTF 2,<'W  '>
	move.l   #WRITEOPE,a0; write or format
jee32f
	move.l   #0,d0  ;high offset 0
drwf
	movem.l  a1-a3/a6/d2/d5,-(sp)
	move.l   a0,a2 ;operation type to A2
	move.l   d0,d5 ;high bits of offset
	move.l   IO_UNIT(a1),a3       ; Get unit pointer
	clr.l    IO_ACTUAL(a1)        ; Initially, no data moved
	move.l   IO_DATA(a1),a0
	move.l   IO_LENGTH(a1),d0
	PRINTF 2,<'length: %lx  '>,d0
	;------ deal with zero length I/O
	beq      RdWt_end

	cmp.w    #ATAPI_DRV,mdu_drv_type(a3)
	beq      Sec_Error            
	cmp.w    #SATAPI_DRV,mdu_drv_type(a3)
	beq      Sec_Error            ; (ATARdWt) only for ATA drives,
	               ;  and not for (S)ATAPI 
	;------ check operation for legality
	move.l   IO_OFFSET(a1),d1
	PRINTF 2,<'offset: %lx',13,10>,d1
	move.l   d1,d2
	and.l    #$1ff,d2
	bne      Sec_Error

	move.l   d0,IO_ACTUAL(a1)   ;high offset is allready saved to d5
	move.l   a1,-(sp)
	move.l   mdu_ATARdWt(a3),a1
  jsr      (a1)
	move.l   (sp)+,a1
	move.w   #TRUE,mdu_motor(a3)  ; Motor will turn on
RdWt_Clean:
	move.b   d0,IO_ERROR(a1)
	bra      RdWt_end
Sec_Error:
	move.b   #IOERR_NOCMD,IO_ERROR(a1)
RdWt_end:
	bsr      TermIO
	movem.l  (sp)+,a1-a3/a6/d2/d5
	rts

Update:
	clr.l    IO_ACTUAL(a1)
	bsr      TermIO
	rts

Clear:
	bsr      TermIO
	rts

MyStop:
	bset     #MDUB_STOPPED,UNIT_FLAGS(a3)
	bsr      TermIO
	rts
	
Start:
	bsr      InternalStart
;  move.l   a2,a1                ; TM simul bug
	bsr      TermIO
	rts

InternalStart:
	;------ turn processing back on
	move.l   a1,-(sp)    ; TM simul bug -- save a1
	bclr     #MDUB_STOPPED,UNIT_FLAGS(a3)

	;------ kick the task to start it moving
	CLEAR    d0
	move.b   MP_SIGBIT(a3),d1     ;TM
	bset     d1,d0
	LINKSYS  Signal,md_SysLib(a6)
	move.l   (sp)+,a1             ; TM simul bug -- restore a1
	rts


Flush:
	movem.l  d2/a1/a6,-(sp)      ; TM simul bug -- save a1
	move.l   md_SysLib(a6),a6
	bset     #MDUB_STOPPED,UNIT_FLAGS(a3)
	sne      d2
Flush_Loop:
	move.l   a3,a0
	CALLSYS  GetMsg
	tst.l    d0
	beq.s    Flush_End
	move.l   d0,a1
	move.b   #IOERR_ABORTED,IO_ERROR(a1)
	CALLSYS  ReplyMsg
	bra.s    Flush_Loop
Flush_End:
	move.l   d2,d0
	movem.l  (sp)+,d2/a1/a6       ; TM simul bug
	tst.b    d0
	beq.s    Flush_term
	bsr      InternalStart
Flush_term:
;  move.l   a2,a1                ; TM simul bug
	bsr      TermIO
	rts

*****************************************************************************
;
; Here begins the task related routines
;
; A Task is provided so that queued requests may be processed at
; a later time.  This is not very justifiable for a ram disk, but
; is very useful for "real" hardware devices.  Take care with
; your arbitration of shared hardware with all the multitasking
; programs that might call you at once.
;
; Register Usage
; ==============
; a3 -- unit pointer
; a6 -- syslib pointer
; a5 -- device pointer
; a4 -- task (NOT process) pointer
; d7 -- wait mask
;----------------------------------------------------------------------

; some dos magic, useful for Processes (not us).  A process is started at
; the first  executable address  after a segment list.	We hand craft a
; segment list here.  See the the DOS technical reference if you really
; need to know more about this.
; The next instruction after the segment list is the first executable address

    cnop    0,4     ; long word align
    DC.L    16	    ; segment length -- any number will do (this is 4
		    ; bytes back from the segment pointer)
myproc_seglist:
    DC.L    0	    ; pointer to next segment

Task_Begin:
    move.l  ABSEXECBASE,a6

    ;------ Grab the argument passed down from our parent
    move.l  4(sp),a3           ; Unit pointer
    move.l  mdu_Device(a3),a5  ; Point to device structure


    ;------ Allocate a signal
    moveq   #-1,d0	    ; -1 is any signal at all
    CALLSYS AllocSignal
    move.b  d0,MP_SIGBIT(a3)
    move.b  #PA_SIGNAL,MP_FLAGS(a3) ;Make message port "live"
    ;------ change the bit number into a mask, and save in d7
    moveq   #0,d7	;Clear D7
    bset    d0,d7

    bra.s   Task_StartHere

; OK, kids, we are done with initialization.  We now can start the main loop
; of the driver.  It goes like this.  Because we had the port marked
; PA_IGNORE for a while (in InitUnit) we jump to the getmsg code on entry.
; (The first message will probably be posted BEFORE our task gets a chance
; to run)
;------     wait for a message
;------     lock the device
;------     get a message.  If no message, unlock device and loop
;------     dispatch the message
;------     loop back to get a message

    ;------ no more messages.  back ourselves out.
Task_Unlock:
    and.b   #$ff&(~(UNITF_ACTIVE!UNITF_INTASK)),UNIT_FLAGS(a3)
    ;------ main loop: wait for a new message

Task_MainLoop:
    move.l  d7,d0
    CALLSYS Wait
Task_StartHere:
    ;------ see if we are stopped
    btst    #MDUB_STOPPED,UNIT_FLAGS(a3)
    bne.s   Task_MainLoop	; device is stopped, ignore messages
    ;------ lock the device
    bset    #UNITB_ACTIVE,UNIT_FLAGS(a3)
    bne     Task_MainLoop	; device in use (immediate command?)


   ;------ get the next request
Task_NextMessage:
    move.l  a3,a0
    CALLSYS GetMsg
    tst.l   d0
    beq     Task_Unlock ; no message?

    ;------ do this request
    move.l  d0,a1
    exg     a5,a6	; put device ptr in right place
    bsr     PerformIO
    exg     a5,a6	; get syslib back in a6

    bra.s   Task_NextMessage
;  Public TaskLen
TaskLen = *-Task_Begin    
 
;  ;; unit pointer in a1
;TimeoutServer:
;	moveq.l		#0,d0	;set no error
;  CMP.l		  #0,mdu_timeout(A1)
;  BEQ.s			TimeOut_End
;  SUBQ.l		#1,mdu_timeout(A1)
;	MOVEM.l		A0-A2/A6,-(sp)
;	move.l		mdu_msgport(A1),A0
;  CALLSYS   GetMsg
;  tst.l     d0
;  beq       TimeOut_NoMessage ; no message?
;  MOVE.l	  d0,a1							; put the port to A1 for ioRequest
;  MOVE.w	  #TR_ADDREQUEST,IO_COMMAND(A1) ;set up command
;	MOVE.l	  #MICRODELAY,IOTV_TIME+TV_MICRO(a1) ;set up delay (10ms)
;	move.l  	mdu_Device(A1),A6  ; Point to device structure
;  jsr		    _BeginIO  
;TimeOut_NoMessage:
;	MOVEM.l		(sp)+,A0-A2/A6
;TimeOut_End:
;	rts
	cnop  0,4    
mdu_Init:
	; ------ Initialize the unit
	INITLONG mdu_tcb+LN_NAME,myTaskName
	INITBYTE mdu_tcb+LN_TYPE,NT_TASK
	INITBYTE mdu_tcb+LN_PRI,MYPROCPRI
	INITBYTE MP_FLAGS,PA_IGNORE
	INITBYTE LN_TYPE,NT_DEVICE
	INITLONG LN_NAME,myName
	INITBYTE mdu_SectorBuffer,1
	INITBYTE mdu_actSectorCount,1
	INITWORD mdu_drv_type,UNKNOWN_DRV
	INITBYTE mdu_firstcall,TRUE
	INITBYTE mdu_auto,TRUE
	INITWORD mdu_motor,TRUE
	INITWORD mdu_lba,CHS_ACCESS
	INITLONG mdu_sectors_per_track,CHS_ACCESS
	INITLONG mdu_heads,0
	INITLONG mdu_cylinders,0
	INITLONG mdu_numlba,0
	INITLONG mdu_numlba48,0
	INITLONG mdu_no_disk,0
	INITLONG mdu_change_cnt,FALSE
	INITSTRUCT 0,mdu_EmulInquiry,0,8
	DC.L		 $00000001
	DC.L		 $1F000000
	DC.L		 $4944452D
	DC.L		 $454D554C
	DC.L		 $20312e30
	DC.L		 $30000000
	DC.L		 $00000000
	DC.L		 $00000000
	DC.L		 $00000000
	INITSTRUCT 0,mdu_EmulMSPage3,0,6
	DC.L		 $1B000000
	DC.L		 $03160000
	DC.L		 $00000000
	DC.L		 $00000000
	DC.L		 $02000000
	DC.L		 $00000000
	DC.L		 $80000000
	INITSTRUCT 0,mdu_EmulMSPage4,0,6
	DC.L		 $1B000000
	DC.L		 $04160000
	DC.L		 $00000000
	DC.L		 $00000000
	DC.L		 $00000000
	DC.L		 $00000000
	DC.L		 $0E100000
	INITSTRUCT 1,mdu_rs_cmd,0,5
	DC.W		 $0300
	DC.W		 $0000
	DC.W		 $2000
	DC.W		 $0000
	DC.W		 $0000
	DC.W		 $0000
	DC.W     0

EndCodeDev:
	END         ;TM

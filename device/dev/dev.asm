;Latest modification 24th of February 2016
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
   include "devices/scsidisk.i"
   include "libraries/expansion.i"
   include "libraries/configvars.i"
   include "libraries/configregs.i"
   include "libraries/expansionbase.i"
   include "libraries/filehandler.i"
   ;Note that the next ASSIGNs ending with : need to be assigned
   ;outside of this assembly source file, eg. in compilation scripts.
   ;These are AmigaDos "links" to some certain file.
   include "/lib/asmsupp.i";Various helper macros made by Commodore
   include "/lib/ata.i"    ;ATA commands and other ATA codes
   include "/lib/mydev.i"  ;select name etc, of the device
   include "/lib/atid.i"   ;This include has the macros which
                           ;are used to access a particular
                           ;implementation of an Amiga to ATA 
                           ;hardware interface, such as an A500 
                           ;side slot interface or a Parallel port
                           ;interface.
   include "/lib/bootinfo.i"  ;select name etc, of the device
   include "/debug/debug-wrapper.i"
   ;include "lib/myscsi.i" ;

   ;Routines and other values to be linked from "rdwt.asm"
   XREF  READOPE       ;These two constants are codes for
   XREF  WRITEOPE      ;the types of operation for IDERdWt
   XREF  ATARdWt       ;Read/Write routine
   XREF  InitDrive     ;Drive initialisation routine
   XREF  SCSIDirectCmd ;SCSI direct command routine
   XREF  SelectDrive   ;Selects which drive to use (0/1)
   XREF  blink         ;Routine that blinks the power LED
   XREF  pause         ;Pause routine
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
   XLIB FindResident
   XLIB InitResident
   XLIB AllocConfigDev
   XLIB AddConfigDev
   XLIB AddBootNode

;The _intena address is the register which can be used to disable or 
;enable the interrupts in a way that they do not reach the 68000 CPU. 
_intena  equ   $dff09a ;;;ML

TRUE  equ   1
FALSE equ   0

		IFND	DEBUG_DETAIL
DEBUG_DETAIL	SET	0	;Detail level of debugging.  Zero for none.
		ENDC



FirstAddress:
   moveq  #-1,d0 
   rts          ;Return in case this code was called as a program
MYPRI   EQU   10

;ROM-Tag 
initDDescrip:
               ;STRUCTURE RT,0
     DC.W    RTC_MATCHWORD    ; UWORD RT_MATCHWORD
     DC.L    initDDescrip     ; APTR  RT_MATCHTAG
     DC.L    EndCode          ; APTR  RT_ENDSKIP
     DC.B    RTF_AUTOINIT     ; UBYTE RT_FLAGS
     DC.B    VERSION          ; UBYTE RT_VERSION
     DC.B    NT_DEVICE        ; UBYTE RT_TYPE
     DC.B    MYPRI            ; BYTE  RT_PRI
     DC.L    myName           ; APTR  RT_NAME
     DC.L    idString         ; APTR  RT_IDSTRING
     DC.L    Init             ; APTR  RT_INIT
               ; LABEL RT_SIZE

subSysName:
myName:     MYDEVNAME
dosName:    DOSNAME
idString: IDSTRINGMACRO ;This is from MYDEVI: include file

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
   INITLONG md_tcb+LN_NAME,myName
   INITBYTE md_tcb+LN_TYPE,NT_TASK
   INITBYTE md_tcb+LN_PRI,MYPROCPRI
   DC.L     0
initRoutine:
   movem.l  d1/a0-a1/a3-a5,-(sp) ;Preserve ALL modified registers
   move.l   d0,a5
;------------------
; DO INITIALIZE THE INTERFACE NOW, macro from INTERFACEI: include file
   INITATAINTERFACE
;------------------
 
   ;------ save a pointer to exec
   move.l   a6,md_SysLib(a5)
   move.l   a5,devadr

   ;------ save a pointer to our loaded code
   move.l   a0,md_SegList(a5)

   ;no doslib required anymore and this resulted 
   ;in errors during coldstart, because there is no doslib at coldstart!

   ;lea.l    dosName,A1              ;Get dos lib. name
   ;moveq    #0,D0
   ;CALLSYS  OpenLibrary             ;Open the dos library
   ;move.l   d0,md_DosLib(a5)
   ;bne.s    init1
   ;ALERT    AG_OpenLib!AO_DOSLib
   ;bra      init_error
init1
   ;------ Allocate the signal for unit 0
   moveq    #-1,d0
   CALLSYS  AllocSignal 
   move.l   d0,unit0sigbit
   moveq    #0,d1
   bset     d0,d1
   move.l   d1,unit0mask
   ;------ Allocate the signal for unit 1
   moveq    #-1,d0
   CALLSYS  AllocSignal
   move.l   d0,unit1sigbit
   moveq    #0,d1
   bset     d0,d1
   move.l   d1,unit1mask
   ;------ Initialize the stack information
   lea      md_stack(a5),a0         ;Low end of stack
   move.l   a0,md_tcb+TC_SPLOWER(a5)
   lea      MYPROCSTACKSIZE(a0),a0  ;High end of stack
   move.l   a0,md_tcb+TC_SPUPPER(a5)
   move.l   a0,md_tcb+TC_SPREG(a5)
   lea      md_tcb(a5),a1
   lea      Proc_Begin(PC),a2
   lea      -1,a3             ;generate address error if task ever "returns".
   moveq    #0,d0
   CALLSYS  AddTask  ;A task for doing things...
   move.l   a5,d0
   bra      init_end
init_error:
   moveq    #0,d0
init_end:
   movem.l  (sp)+,d1/a0-a1/a3-a5
   rts

Open:    ; ( device:a6, iob:a1, unitnum:d0, flags:d1 )
   movem.l  d2/a2-a4,-(sp)
   move.l   a1,a2                   ; save the iob
   ;------ see if the unit number is in range
   cmp.l    #10,d0                  ;convert: scsi unit 10 = dos unit 1
   bne      opn1
   move.l   #1,d0
opn1
   move.l   d0,d2
   and.l    #$FFFFFFFE,d2           ;Allow unit numbers 0 and 1.
   bne      Open_Error              ;unit number is out of range
   ;------ see if the unit is already initialized
   move.l   d0,d2                   ; save unit number
   lsl.l    #2,d0
   lea.l    md_Units(a6,d0.l),a4
   move.l   (a4),d0
   bne.s    Open_UnitOK
   ;------ Try and conjure up a unit
   bsr      InitUnit
   ;------ see if it initialized OK
   move.l   (a4),d0
   beq      Open_Error

Open_UnitOK:

   move.l   d0,a3                   ;unit pointer in a3
   move.l   d0,IO_UNIT(a2)

   ;------ mark us as having another opener
   addq.w   #1,LIB_OPENCNT(a6)
   addq.w   #1,UNIT_OPENCNT(a3)
   ;------ prevent delayed expunges
   bclr     #LIBB_DELEXP,md_Flags(a6)
   ;If the IDE drive has not been initialised previously, do it now
   cmp.w    #TRUE,mdu_firstcall(a3)
   bne      nav1
   bsr      InitDrive ;Call the IDE drive initialisation routine
   move.w   #FALSE,mdu_firstcall(a3)
nav1
   cmp.w    #UNKNOWN_DRV,mdu_drv_type(a3)  ;known drive type
   beq      Open_Error                     ; unknowns cannot be opened!
   moveq    #0,d0

Open_End
   PRINTF 1,<'End ide.device %lx ',13,10>d0
   movem.l  (sp)+,d2/a2-a4
   rts
Open_Error:
   bsr      FreeUnit
   moveq    #IOERR_OPENFAIL,d0
   move.b   d0,IO_ERROR(a2)
   move.l   d0,IO_DEVICE(a2)    ;IMPORTANT: trash IO_DEVICE on open failure
   
   bra.s    Open_End


Close:      ;( device:a6, iob:a1 )
   movem.l  d1/a2-a3,-(sp)

   move.l   a1,a2
   move.l   IO_UNIT(a2),a3

   ;------ make sure the iob is not used again
   moveq    #IOERR_OPENFAIL,d0
   move.l   d0,IO_UNIT(a2)
   move.l   d0,IO_DEVICE(a2)

   ;------ see if the unit is still in use
   subq.w   #1,UNIT_OPENCNT(a3)

;  bne.s    Close_Device
;  bsr      ExpungeUnit

Close_Device:
   ;------ mark us as having one fewer openers
   moveq.l  #0,d0
;  subq.w   #1,LIB_OPENCNT(a6)
   ;------ see if there is anyone left with us open
;  bne.s    Close_End
   ;------ see if we have a delayed expunge pending
;  btst     #LIBB_DELEXP,md_Flags(a6)
;  beq.s    Close_End
   ;------ do the expunge
;  bsr      Expunge
Close_End:
   movem.l  (sp)+,d1/a2-a3
   rts





Expunge:    ;( device: a6 )

;  movem.l  d1/d2/a5/a6,-(sp)

;  move.l   a6,a5

;  move.l   md_SysLib(a5),a6
   ;------ see if anyone has us open
;  tst.w    LIB_OPENCNT(a5)

;  beq      1$
   ;------ it is still open.  set the delayed expunge flag
;  bset     #LIBB_DELEXP,md_Flags(a5)
;  CLEAR    d0
;  bra.s    Expunge_End
1$:
   ;------ go ahead and get rid of us.  Store our seglist in d2
;  move.l   md_SegList(a5),d2
   ;------ unlink from device list
;  move.l   a5,a1
;  CALLSYS  Remove
;  move.l   md_DosLib(a5),a1
;  CALLSYS  CloseLibrary


   ;device specific closings here...
   ;------ free our memory
;  CLEAR   d0
;  CLEAR   d1
;  move.l   a5,a1
;  move.w   LIB_NEGSIZE(a5),d1
;  sub.w    d1,a1
;  add.w    LIB_POSSIZE(a5),d0
;  add.l    d1,d0
;  CALLSYS  FreeMem
   ;------ set up our return value
;  move.l   d2,d0
Expunge_End:
;  movem.l  (sp)+,d1/d2/a5/a6
;  rts

Null:
   moveq    #0,d0
   rts
   
InitUnit:      ;( d2:unit number, a3:scratch, a6:devptr )

   movem.l  d2-d4/a1-a2,-(sp)

   ;------ allocate unit memory
   move.l   #MyDevUnit_Sizeof,d0
   move.l   #MEMF_PUBLIC!MEMF_CLEAR,d1
   LINKSYS  AllocMem,md_SysLib(a6)

   move.l   d0,a3
   tst.l    d0
   beq      InitUnit_End
   move.l   d2,mdu_UnitNum(a3)      ;initialize unit number
   move.l   a6,mdu_Device(a3)       ;initialize device pointer
   ;------ initialize the unit's list
   lea      MP_MSGLIST(a3),a0
   NEWLIST  a0

   lea      md_tcb(a6),a1

   move.l   a1,MP_SIGTASK(a3)
   moveq.l  #0,d0                   ;Dont need to re-zero it
   move.l   a3,a2                   ;InitStruct is initializing the UNIT
   lea.l    mdu_Init,A1

   LINKSYS  InitStruct,md_SysLib(a6)

   ;------ save unit pointer and set unit signal bit
   lea      unit0adr,a1
   move.l   d2,d3
   lsl.l    #2,d3
   move.l   a3,0(a1,d3.w)
   lea      unit0sigbit,a1
   move.l   0(a1,d3.w),d0
   move.b   d0,MP_SIGBIT(a3)

   ;------ default values
   move.b	#1,mdu_SectorBuffer(a3) ;device must at least handle one sector per read/write
   move.b	#1,mdu_actSectorCount(a3) 
   move.w   #UNKNOWN_DRV,mdu_drv_type(a3)
   move.w   #TRUE,mdu_firstcall(a3)
   move.w   #TRUE,mdu_auto(a3)
   move.w   #CHS_ACCESS,mdu_lba(a3)
   move.l   #0,mdu_sectors_per_track(a3)
   move.l   #0,mdu_heads(a3)
   move.l   #0,mdu_cylinders(a3)
   move.l   #0,mdu_numlba(a3)
   move.l   #0,mdu_numlba48(a3)   
   move.w   #TRUE,mdu_motor(a3)     ;units usually start up with motor on
   move.l   #0,mdu_change_cnt(a3)
   move.l   #FALSE,mdu_no_disk(a3)

   ;------ mark us as ready to go
   move.l   d2,d0                   ;unit number
   lsl.l    #2,d0

   move.l   a3,md_Units(a6,d0.l)    ;set unit table

   move.b   #PA_SIGNAL,MP_FLAGS(a3)
InitUnit_End:

   movem.l  (sp)+,d2-d4/a1-a2
   rts
;------ got an error - free the unit structure that we allocated.
InitUnit_FreeUnit:
   bsr      FreeUnit
   move.l	#0,a3	;clear pointer
   bra.s    InitUnit_End



FreeUnit:   ;( a3:unitptr, a6:deviceptr )

   tst.l    (a3)   ;valid pointer
   beq FreeUnit_End 
   move.l   a3,a1
   move.l   #MyDevUnit_Sizeof,d0

   LINKSYS  FreeMem,md_SysLib(a6)
FreeUnit_End:
   rts


ExpungeUnit:   ;( a3:unitptr, a6:deviceptr )
;  movem.l  d2/a1,-(sp)

   ;------ save the unit number
;  move.l   mdu_UnitNum(a3),d2
;  lsl.l    #2,d2
;  lea      unit0mask,a1
;  clr.l    0(a1,d2.w)           ;clear signal mask for unit
;  clr.l    8(a1,d2.w)           ;clear unit pointer
;  lsr.l    #2,d2
   
   ;------ free the unit structure.

;  bsr      FreeUnit
   ;------ clear out the unit vector in the device
;  lsl.l    #2,d2
;  clr.l    md_Units(a6,d2.l)
;  movem.l  (sp)+,d2/a1
   rts

   cnop  0,4

;local registers
unit0mask   dc.l  0                 ;dont change unit#? data order
unit1mask   dc.l  0
unit0adr    dc.l  0
unit1adr    dc.l  0
unit0sigbit dc.l  0
unit1sigbit dc.l  0
devadr      dc.l  0
;emulated inquiry packet data
EmulInquiry    dc.l $00000001,$1F000000,$4944452D,$454D554C,$20202020
               dc.l $20202020,$20202020,$20202020,$312E3030
;emulated mode_sense packet (page 3 and 4) data
EmulMSPage3    dc.l $1B000000,$03160000,$00000000,$00000000,$02000000
               dc.l $00000000,$80000000
EmulMSPage4    dc.l $1B000000,$04160000,$00000000,$00000000,$00000000
               dc.l $00000000,$0E100000
   cnop  0,4

mdu_Init:
   ; ------ Initialize the unit
   INITBYTE MP_FLAGS,PA_IGNORE
   INITBYTE LN_TYPE,NT_DEVICE
   INITLONG LN_NAME,myName
   DC.L     0
   cnop  0,4

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
   move.l   a2,-(sp)
   move.l   a1,a2
  move.l   mdu_UnitNum(a3),d0 ;XXXXXX d0 next 2.nd line!!
   clr.b    IO_ERROR(a2)         ;No error so far
   move.w   IO_COMMAND(a2),d0
   lsl      #2,d0                ;Multiply by 4 to get table offset
   lea      cmdtable(pc),a0
   move.l   0(a0,d0.w),a0

   jsr      (a0) ; JSR TO THE ADDRESS WE READ FROM THE CMD TABLE

   move.l   (sp)+,a2
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
   move.l   mdu_UnitNum(a3),d0
   lsl.b    #4,d0
   or.b     #$a0,d0
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
   move.l   mdu_UnitNum(a3),d2

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
   move.l   (a0),d1
   and.l    #$001FFFFF,d1
   mulu     #512,d1
   moveq    #0,d0
   move.b   4(a0),d0
   mulu     #512,d0
   move.l   d0,-(sp)
   move.l   scsi_Data(a6),a0
   move.l   mdu_UnitNum(a3),d2
   clr.b    scsi_Status(a6)
   move.w   IO_COMMAND(a1),-(sp)
   move.w   #CMD_READ,IO_COMMAND(a1)
   move.l   a1,a2
   jsr      ATARdWt
   move.w   (sp)+,IO_COMMAND(a1)
   move.l   (sp)+,scsi_Actual(a6)
   tst.b    d0
   beq      escsi4
   move.b   #1,scsi_Status(a6)
   bra      escsi4
scsi_ms                             ; Mode sense(6) packet
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
   move.l   d1,scsi_Actual(a6)
   subq.l   #1,d1
   bmi      escsi4
   lea      EmulMSPage3,a2
   move.l   mdu_sectors_per_track(a3),d0
   move.w   d0,14(a2)
   bra      escsi15
page04
   move.l   d1,scsi_Actual(a6)
   subq.l   #1,d1
   bmi      escsi4
   lea      EmulMSPage4,a2
   move.l   mdu_cylinders(a3),d0
   move.l   d1,-(sp)
   move.l   d0,d1
   lsr.l    #8,d0
   move.w   d0,6(a2)
   move.w   d0,10(a2)
   move.w   d0,18(a2)
   move.b   d1,8(a2)
   move.b   d1,12(a2)
   move.b   d1,20(a2)
   move.w   d1,14(a2)
   swap     d1
   move.b   d1,13(a2)
   move.l   mdu_heads(a3),d0
   move.b   d0,9(a2)
   move.l   (sp)+,d1
escsi15
   move.l   scsi_Data(a6),a0
escsi13
   move.b   (a2)+,(a0)+
   dbra     d1,escsi13
   clr.b    scsi_Status(a6)   
   bra      escsi4
scsi_cap                               ; Read Recorded Capacity packet
   cmp.w    #CHS_ACCESS,mdu_lba(a3)
   beq 		chscapa
   move.l   mdu_numlba(a3),d0
;   subq.l		#1,d0												; one less to avoid offset problems!
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
   lea      mdu_model_num(a3),a2

   lea      EmulInquiry+8,a0

   move.l   (a2)+,(a0)+
   move.l   (a2),(a0)+

   lea      mdu_ser_num(a3),a2
   lea      EmulInquiry+16,a0
   move.l   (a2)+,(a0)+
   move.l   (a2)+,(a0)+
   move.l   (a2)+,(a0)+
   move.l   (a2),(a0)
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
   lea      EmulInquiry,a2
escsi3
   move.b   (a2)+,(a0)+
   dbra     d1,escsi3
escsi_ok
   clr.b    scsi_Status(a6)
escsi4
   movem.l  (sp)+,d1/a6
   bsr   TermIO
   movem.l  (sp)+,d0/d1/a0/a2/a6
   rts


AbortIO:
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
   move.l   #READOPE,a0
   bra      td64j
td64_write64
td64_format64
   move.l   #WRITEOPE,a0
td64j
IO_HIGHOFFSET EQU IO_ACTUAL      ;trackdisk64 
   move.l   IO_HIGHOFFSET(a1),d0 ; high offset
   bra      drwf
Read:
   move.l   #READOPE,a0
   bra		jee32f
Write:
Format:
   move.l   #WRITEOPE,a0; write or format
jee32f
   move.l   #0,d0  ;high offset 0
drwf
   movem.l  a2/a6/d2/d5,-(sp)
   move.l   a0,a2 ;operation type to A2
   move.l   d0,d5 ;high bits of offset
   move.l   IO_UNIT(a1),a3       ; Get unit pointer
   clr.l    IO_ACTUAL(a1)        ; Initially, no data moved
   move.l   IO_DATA(a1),a0
   move.l   IO_LENGTH(a1),d0

   ;------ deal with zero length I/O
   beq.s    RdWt_end

   cmp.w    #ATAPI_DRV,mdu_drv_type(a3)
   beq      Sec_Error            
   cmp.w    #SATAPI_DRV,mdu_drv_type(a3)
   beq      Sec_Error            ; (ATARdWt) only for ATA drives,
                  ;  and not for (S)ATAPI 
   ;------ check operation for legality
   move.l   IO_OFFSET(a1),d1
   move.l   d1,d2
   and.l    #$1ff,d2
   bne      Sec_Error

   move.l   d0,IO_ACTUAL(a1)   ;high offset is allready saved to d5
   move.l   mdu_UnitNum(a3),d2
   jsr      ATARdWt
   move.w   #TRUE,mdu_motor(a3)  ; Motor will turn on
RdWt_Clean:
   move.b   d0,IO_ERROR(a1)
   bra      RdWt_end
Sec_Error:
   move.b   #IOERR_NOCMD,IO_ERROR(a1)
RdWt_end:
   bsr      TermIO
   movem.l  (sp)+,a2/a6/d2/d5
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
;  move.l   a3,a1                ; TM simul bug
   CLEAR    d0
;  move.l   MP_SIGBIT(a3),d1
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
   beq.s    1$
   bsr      InternalStart
1$:
;  move.l   a2,a1                ; TM simul bug
   bsr      TermIO
   rts


;  cnop     0,4                  ; long word allign
   DC.L     16                   ; segment length -- any number will do
myproc_seglist:
   DC.L     0                    ; pointer to next segment

; the next instruction after the segment list is the first executable address
Proc_Begin:
   move.l   4,a6
   move.l   devadr,a5
   move.l   unit0mask,d7
   or.l     unit1mask,d7
   move.l   unit0adr,a3
   bsr      Proc_CheckStatus
   move.l   unit1adr,a3
   bsr      Proc_CheckStatus
   ;------ main loop: wait for a new message
Proc_MainLoop:
   move.l   d7,d0
   CALLSYS  Wait
   move.l   d0,-(sp)
   and.l    unit0mask,d0
   beq      pb3
   move.l   unit0adr,a3
   bsr      Proc_CheckStatus
pb3
   move.l   (sp)+,d0
   and.l    unit1mask,d0
   beq      Proc_MainLoop
   move.l   unit1adr,a3
   bsr      Proc_CheckStatus
   bra      Proc_MainLoop


Proc_CheckStatus:
   ;------ is unit initialized?
   cmp.l    #0,a3
   bne      pcs1
   rts
pcs1
   ;------ see if we are stopped
   btst     #MDUB_STOPPED,UNIT_FLAGS(a3)
   beq      pcs2
   rts                                    ;device is stopped
pcs2
   ;------ lock the device
   bset     #UNITB_ACTIVE,UNIT_FLAGS(a3)
   beq      Proc_NextMessage
   rts                                    ;device in use
   ;------ get the next request
Proc_NextMessage:
   move.l   a3,a0
   CALLSYS  GetMsg
   tst.l    d0
   beq.s    Proc_Unlock                   ;no message?
   ;------ do this request
   move.l   d0,a1
   exg      a5,a6                         ;put device ptr in right place
   bsr      PerformIO
   exg      a5,a6                         ;get syslib back in a6
   bra.s    Proc_NextMessage
   ;------ no more messages.  back ourselves out.
Proc_Unlock:
   and.b    #$ff&(~(UNITF_ACTIVE!UNITF_INTASK)),UNIT_FLAGS(a3)
   rts

EndCode:
   END         ;TM

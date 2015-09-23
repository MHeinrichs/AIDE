;Latest modification November 2013
   SECTION   section
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
   ;Note that the next ASSIGNs ending with : need to be assigned
   ;outside of this assembly source file, eg. in compilation scripts.
   ;These are AmigaDos "links" to some certain file.
   include "/lib/asmsupp.i";Various helper macros made by Commodore
   include "/lib/myscsi.i" ;
   include "/lib/ata.i"    ;ATA commands and other ATA codes
   include "/lib/mydev.i"  ;select name etc, of the device
   include "/lib/atid.i"   ;This include has the macros which
                           ;are used to access a particular
                           ;implementation of an Amiga to ATA 
                           ;hardware interface, such as an A500 
                           ;side slot interface or a Parallel port
                           ;interface.
    ;Routines and other values to be linked from "rdwt.asm"
   XREF  READOPE       ;These two constants are codes for
   XREF  WRITEOPE      ;the types of operation for IDERdWt
   XREF  ATARdWt       ;Read/Write routine
   XREF  InitDrive     ;Drive initialisation routine
   XREF  SCSIDirectCmd ;SCSI direct command routine
   XREF  waitnotbusy1  ;ATA stuff.. routine
   XREF  SelectDrive   ;Selects which drive to use (0/1)
   XREF  blink         ;Routine that blinks the power LED
   XREF  pause         ;Pause routine
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
   Public   FindAndMount
FindAndMount: ;( exec-lib:a6 device:a5, iob:a2, unitnum:d0)
   rts
   
            ; Fake ConfigDev and Diagnostic ROM structure.
fakebootrom:
            dc.b    DAC_CONFIGTIME
            dc.b    0
            dc.w    0
            dc.w    0
            dc.w    bootcode-fakebootrom
            dc.w    0
            dc.l    0

            ; If fakebootrom marked the start of a ConfigDev structure,
            ; the following would be 14 bytes into the structure.

            dc.w    0
            dc.b    ERTF_DIAGVALID
            dc.b    0
            dc.b    0
            dc.b    0
            dc.l    0
            dc.l    0
            dc.l    fakebootrom

            ; DOS boot code.

bootcode:
            lea     dosname(pc),a1
            jsr     _LVOFindResident(a6)
            tst.l   d0
            beq.s   boot_return
            move.l  d0,a0
            move.l  RT_INIT(a0),a0
            jsr     (a0)
boot_return:
            rts
dosname:
            dc.b    'dos.library',0  
   
EndCode:
   END         ;TM

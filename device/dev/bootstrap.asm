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
 	 include "devices/hardblocks.i"
   include "libraries/expansion.i"
   include "libraries/configvars.i"
   include "libraries/configregs.i"
   include "libraries/expansionbase.i"
   include "libraries/filehandler.i"
   ;Note that the next ASSIGNs ending with : need to be assigned
   ;outside of this assembly source file, eg. in compilation scripts.
   ;These are AmigaDos "links" to some certain file.
   include "/lib/asmsupp.i";Various helper macros made by Commodore
   INCLUDE "IDE_BASE_ADDRESS.i"
   ;include "/lib/mydev.i"  ;select name etc, of the device
   ;include "/lib/myscsi.i" ;
   include "/lib/ata.i"    ;ATA commands and other ATA codes
   include "/lib/bootstrap.i"  ;select name etc, of the device
   ;include "/lib/atid.i"   ;This include has the macros which
         ;are used to access a particular
         ;implementation of an Amiga to ATA 
         ;hardware interface, such as an A500 
         ;side slot interface or a Parallel port
         ;interface.
   include "/lib/bootinfo.i";Structure for boot infos
   include "/debug/debug-wrapper.i"

   XLIB	AllocMem
   XLIB	FreeMem
   XLIB	MakeDosNode
   XLIB	AddDosNode
   XLIB	OpenLibrary
   XLIB	CloseLibrary
   XLIB	FindResident
   XLIB	InitResident
   XLIB AddBootNode
   XLIB	Enqueue
   XLIB	Permit
   XLIB	Forbid
   XLIB	FindName
   XLIB FindResident
   XLIB InitResident
   XLIB AllocConfigDev
   XLIB FreeConfigDev


		IFND	DEBUG_DETAIL
DEBUG_DETAIL	SET	1	;Detail level of debugging.  Zero for none.
		ENDC




   moveq  #-1,d0 
   rts          ;Return in case this code was called as a program

   ; Fake ConfigDev and Diagnostic ROM structure.
fakebootrom:
   dc.b    DAC_CONFIGTIME
   dc.b    0
   dc.w    endcopy-fakebootrom
   dc.w    DiagEntry-fakebootrom
   dc.w    bootcode-fakebootrom
   dc.w    bootdevicename_diagentry-fakebootrom
   dc.w    0 ;da_Reserved01
   dc.w    0 ;da_Reserved02
DiagEntry:
	moveq	#1,d0			; ROM abbrechen, freigeben
	rts

; DOS boot code.
bootcode:
   lea     dosname(pc),a1
   CALLSYS FindResident
   move.l  d0,a0
   move.l  RT_INIT(a0),a0
   jsr     (a0)
   rts
		CNOP	0,4
bootdevicename_diagentry:
   dc.b    'ide.device',0
dosname:
   dc.b    'dos.library',0
		CNOP	0,2
endcopy:

romtag:
   dc.w    RTC_MATCHWORD
   dc.l    romtag
   dc.l    endcode
   dc.b    RTW_COLDSTART
   dc.b    BD_VERSION
   dc.b    NT_DEVICE
   dc.b    5
   dc.l    bootname
   dc.l    bootid
   dc.l    initRoutine

bootname:
   dc.b    'anyboot',0
bootid:
   dc.b    'anyboot 1.0  (10 Jul 2015)',13,10,0
bootdevicename:
   dc.b    'ide.device',0
bootdosname:
   dc.b    'DH0',0
		CNOP	0,4
   
initRoutine:
   movem.l d0-d4/a0-a6,-(SP)

   PRINTF 1,<'Start',13,10>
		
   ; Get 512 bytes of memory for sector buffer
   move.l  #BLOCKSIZE,d0
   moveq.l #MEMF_ANY,d1
   move.l  ABSEXECBASE,a6
   LINKSYS  AllocMem,a6
   tst.l   d0
   beq     close_and_dealloc
   move.l  d0,buffermem
   
   PRINTF 1,<'alloc %ld bytes for blockbuffer at %lx',13,10>,#BLOCKSIZE,D0
   
   ; get mem for parameter packet
   move.l  #MyParmPkt_Sizeof,d0
   moveq.l #MEMF_ANY,d1
   LINKSYS  AllocMem,a6
   tst.l   d0
   beq     close_and_dealloc
   move.l  d0,parametermem
   PRINTF 1,<'alloc %ld bytes for parameter packet at %lx',13,10>,#MyParmPkt_Sizeof,D0
   move.l  d0,a3
      
   ;find the device
   lea	bootdevicename,a1	; the device name to look for
   lea	DeviceList(a6),a0	; in Device-Liste suchen.
   CALLSYS	FindName   
   PRINTF 1,<'Seach in device list for %s resulted in %lx',13,10>,A1,D0
   tst.l	d0   
   bne device_present

   ;device not loaded: find the resident structure
   lea	bootdevicename(PC),a1	; the device name to look for
   CALLSYS FindResident   
	 PRINTF 1,<'Find Resident resulted in %lx',13,10>,D0
   tst.l	d0   
   beq     close_and_dealloc

	 ;init the resident structure
	 moveq.l #0,d1 ;resident structure in rom 
	 move.l D0,A1
	 
	 CALLSYS InitResident
   
	 PRINTF 1,<'InitResident resulted in %lx',13,10>,D0
	 beq     close_and_dealloc
   ;lea	bootdevicename(PC),a1	; the device name to look for
   ;lea	DeviceList(a6),a0	; in Device-Liste suchen.
   ;CALLSYS	FindName   
   ;PRINTF 1,<'2nd Seach in devicelist for %s resulted in %lx',13,10>,A1,D0
	 
device_present:

   ; Read rdb block here!
   lea		bootdosname(PC),a1
   move.l	a1,pp_dosName(a3)
   lea		bootdevicename(PC),a1
   move.l	a1,pp_execName(a3)
   move.l	#0,pp_unitNumber(a3)
   move.l	#1,pp_flags(a3)
   move.l	#16,pp_paramSize(a3)     ; 16 longwords comming
   move.l	#128,pp_blockSize(a3)    ; 128*4=512 bytes in long words
   move.l	#0,pp_sectorOrigin(a3)    ;leave 0
   move.l	#1,pp_surfaces(a3) ;
   move.l	#1,pp_sectorsPerBlock(a3) 		;leave 1
   move.l	#288,pp_blocksPerTrack(a3)
   move.l	#2,pp_reservedBlocks(a3)
   move.l	#0,pp_preface(a3); leave 0
   move.l	#0,pp_interleave(a3)
   move.l	#2,pp_lowCyl(a3)
   move.l	#433,pp_highCyl(a3)
   move.l	#300,pp_numBuffer(a3)
   move.l	#MEMF_ANY,pp_BufferMemType(a3)
   move.l	#$00FFFFFF,pp_maxTransfer(a3)
   move.l	#$7FFFFFFE,pp_mask(a3)   
   move.l	#20,pp_bootPrio(a3)
   move.l	#$444F5301,pp_dosType(a3) 		;OFS - FFS enddet mit 301

   ; Open expansion.library
   lea     expname(pc),a1
   moveq   #0,d0
   move.l  ABSEXECBASE,a6
   CALLSYS  OpenLibrary
   tst.l   d0
   beq     close_and_dealloc
	 move.l  d0,expansionlib
   
   PRINTF 1,<'Opened expansion lib: %lx',13,10>,D0
   move.l  d0,a6

   ;now build a fake config dev!
   CALLSYS AllocConfigDev
   PRINTF 1,<'Created Config dev: %lx',13,10>,D0
   move.l  d0,configdev
   beq     close_and_dealloc

   ; Create the DOS node.
   move.l  a3,a0
   CALLSYS MakeDosNode
   tst.l   d0
   beq     close_and_dealloc
   PRINTF 1,<'Made DosNode: %lx',13,10>,D0
   ; Initialize the rest of the DOS node.

   move.l  d0,a0
   clr.l   dn_Task(a0)
   move.l  #4000,dn_StackSize(a0)
   move.l  #0,dn_Priority(a0)
   move.l  #-1,dn_GlobalVec(a0)
   move.l  #0,dn_SegList(a0) ; here a pointer to the filesystem should be integrated
   ;Move 
   move.l  a0,d2 ; move the device node to d2
   
   ;moveq   #BootNode_SIZEOF,d0
   ;move.l  #MEMF_ANY!MEMF_CLEAR,d1
   ;move.l  a6,-(SP)
   ;move.l  ABSEXECBASE,a6
   ;LINKSYS  AllocMem,a6
   ;move.l  (SP)+,a6
   ;tst.l   d0
   ;beq     close_and_dealloc
   ;move.l  d0,bootnodemem
   ;PRINTF 1,<'alloc %ld bytes for bootnode at: %lx',13,10>,#BootNode_SIZEOF,D0
   ;; Initialize the boot node.
   ;move.l  d0,a4
   ;move.b  #NT_BOOTNODE,LN_TYPE(a4)          ; Set node type.
   ;move.b  pp_bootPrio(a3),LN_PRI(a4) ; set up boot priority
   ;move.l  fakebootrom(pc),LN_NAME(a4)          ; Set fake configdev pointer
   ;move.l  d2,bn_DeviceNode(a4)   	

   ; Enqueue the boot node in ExpansionBase->eb_Mountlist.


   move.l   a6,-(SP)
   ;lea     eb_MountList(a6),a0
   move.l	  pp_bootPrio(a3),d0			this priority
   moveq.l	#ADNF_STARTPROC,d1	StartProc = true
   move.l   d2,a0 ; move the device node to a0
   move.l   configdev,a1
   ;init the ConfigDev
   move.l	fakebootrom(pc),cd_Rom+er_Reserved0c(a1) ;save the diag entry
   move.l  IDE_BASE_ADDRESS,cd_BoardAddr(a1) ;save the board adress
   move.l  #ERTF_DIAGVALID,cd_Rom+er_Type(a1) ; this makes the thing autoboot
   CALLSYS  AddBootNode	
	 PRINTF 1,<'AddBootNodeResult %xl',13,10>,d0
   move.l   ABSEXECBASE,a6

   ;CALLSYS  Forbid		gotta Forbid() around this
   ;CALLSYS  Enqueue		add our bootnode to the list      
   ;PRINTF 1,<'Enque Result %xl',13,10>,d0
   ;CALLSYS  Permit		gotta Permit() now
   move.l  (SP)+,a1

close_and_dealloc:
   ;tst.l   configdev
	 ;beq     dealloc_exit0
   ;move.l  expansionlib,a6
   ;move.l  configdev,a1
   ;CALLSYS FreeConfigDev 
   ;move.l  #0,configdev
dealloc_exit0:
	 tst.l	 expansionlib
	 beq     dealloc_exit
   move.l  expansionlib,a1
   move.l  ABSEXECBASE,a6
   CALLSYS CloseLibrary
   PRINTF 1,<'Closed expansion lib at %lx',13,10>,a1
   move.l  #0,expansionlib
dealloc_exit:
   move.l  buffermem,d0 ;mem allocated?
   beq     dealloc_exit2
   move.l  buffermem,a1
   move.l  #BLOCKSIZE,d0
   PRINTF 1,<'Free %ld byte buffer mem at %lx',13,10>,d0,a1
   LINKSYS FreeMem,a6
   move.l #0,buffermem
dealloc_exit2:
   tst.l   parametermem ;mem allocated?
   beq     dealloc_exit3
   move.l  parametermem,a1
   move.l  #MyParmPkt_Sizeof,d0
   PRINTF 1,<'Free %ld byte param mem at %lx',13,10>,d0,a1
   LINKSYS FreeMem,a6   
   move.l #0,parametermem
dealloc_exit3:
   tst.l   bootnodemem ;mem allocated?
   beq     bomb
   move.l  bootnodemem,a1
   move.l  #BootNode_SIZEOF,d0
   PRINTF 1,<'Free %ld byte boot node mem at %lx',13,10>,d0,a1
   LINKSYS FreeMem,a6   
   move.l #0,bootnodemem   
bomb:       
   PRINTF 1,<'End',13,10>
   movem.l (SP)+,d0-d4/a0-a6
   rts

buffermem: dc.l 0
parametermem: dc.l 0
bootnodemem: dc.l 0
configdev: dc.l 0
expansionlib: dc.l 0

expname:    dc.b    'expansion.library',0

endcode:
   end

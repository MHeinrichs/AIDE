	SECTION   driver,CODE
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
	include "lib/asmsupp.i";Various helper macros made by Commodore
	include "lib/mydev.i"  ;select name etc, of the device
	;include "lib/myscsi.i" ;
	include "lib/ata.i"    ;ATA commands and other ATA codes
	include "lib/atid.i"   ;This include has the macros which
	      ;are used to access a particular
	      ;implementation of an Amiga to ATA 
	      ;hardware interface, such as an A500 
	      ;side slot interface or a Parallel port
	      ;interface.
	include "lib/bootinfo.i";Structure for boot infos

	XLIB	AllocMem
	XLIB	FreeMem
	XLIB	MakeDosNode
	XLIB	AddMemList
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
	XLIB AddConfigDev
	XREF InitDrive
	XREF ResetIDE
		
		IFND	BIND_MEM
BIND_MEM	SET	0	;Detail level of debugging.  Zero for none.
		ENDC

	moveq  #-1,d0 
	rts          ;Return in case this code was called as a program

romtag:
	dc.w    RTC_MATCHWORD
	dc.l    romtag
	dc.l    endcode
	dc.b    RTW_COLDSTART
	dc.b    BD_VERSION
	dc.b    NT_DEVICE
	dc.b    MYPROCPRI+1
	dc.l    bootname
	dc.l    bootid
	dc.l    initRoutine




	; Fake ConfigDev and Diagnostic ROM structure.
fakebootrom:
	dc.b    DAC_WORDWIDE+DAC_CONFIGTIME
	dc.b    0
	dc.w    0
	dc.w    0
	dc.w    bootcode-fakebootrom
	dc.w    bootdevicename-fakebootrom
	dc.w    0 ;da_Reserved01
	dc.w    0 ;da_Reserved02
	CNOP 0,4

	
; DOS boot code.
bootcode:
	lea     dosname(pc),a1
	CALLSYS FindResident
	tst.l	d0			;did we find it?
	beq.s	BootedMe		;no
	move.l  d0,a0
	move.l  RT_INIT(a0),a0
	jsr     (a0)
BootedMe:
	rts
	CNOP 0,4
dosname:
	dc.b    'dos.library',0
		CNOP	0,2
endcopy:
bootdevicename:
	dc.b    'ide.device',0
expname:    dc.b    'expansion.library',0
mem_name:	dc.b "A1K_FastMEM",0
	cnop	0,4

bootname:
	dc.b    'anyboot',0
bootid:
	dc.b    'anyboot 1.0  (10 Jul 2015)',13,10,0
		CNOP	0,4
	
initRoutine:
	movem.l d1-d7/a0-a6,-(SP)
	PRINTF 1,<'Start',13,10>
	 move.l  d0,d7	
	IFGE	BIND_MEM-1	
	bsr addmem
	ENDC
	move.l  ABSEXECBASE,a6
	;bsr ResetIDE
	;get structure for a mem-pointer-package
	move.l  #MyMemPkt_Sizeof,d0
	move.l  #MEMF_PUBLIC+MEMF_CLEAR,d1
	CALLSYS AllocMem
	tst.l   d0
	beq     end_dealloc
	move.l  d0,a5
	
	
	
	; Get 512 bytes of memory for sector buffer
	move.l  #BLOCKSIZE,d0
	move.l  #MEMF_PUBLIC+MEMF_CLEAR,d1
	CALLSYS AllocMem
	tst.l   d0
	beq     close_and_dealloc
	move.l  d0,buffermem(a5)  

	; Get 512 bytes of memory for rdb buffer
	move.l  #BLOCKSIZE,d0
	move.l  #MEMF_PUBLIC+MEMF_CLEAR,d1
	CALLSYS AllocMem
	tst.l   d0
	beq     close_and_dealloc
	move.l  d0,rdbmem(a5)

	; get mem for parameter packet
	move.l  #MyParmPkt_Sizeof,d0
	move.l  #MEMF_PUBLIC+MEMF_CLEAR,d1
	CALLSYS AllocMem
	tst.l   d0
	beq     close_and_dealloc
	move.l  d0,parametermem(a5)
	;PRINTF 1,<'alloc %ld bytes for parameter packet at %lx',13,10>,#MyParmPkt_Sizeof,D0
	   
	; get mem for io handler
	move.l  #IOSTD_SIZE,d0
	move.l  #MEMF_PUBLIC+MEMF_CLEAR,d1
	CALLSYS AllocMem
	tst.l   d0
	beq     close_and_dealloc
	move.l  d0,iohandler(a5)
	;PRINTF 1,<'alloc %ld bytes for parameter packet at %lx',13,10>,#IOSTD_SIZE,D0


	;find the device
	lea	bootdevicename,a1	; the device name to look for
	lea	DeviceList(a6),a0	; in Device-Liste suchen.
	CALLSYS	FindName   
	;PRINTF 1,<'Seach in device list for %s resulted in %lx',13,10>,A1,D0
	tst.l	d0   
	bne device_present
	
	;device not loaded: find the resident structure
	lea	bootdevicename,a1	; the device name to look for
	CALLSYS FindResident   
	 ;PRINTF 1,<'Find Resident resulted in %lx',13,10>,D0
	tst.l	d0   
	beq     close_and_dealloc
	
	 ;init the resident structure
	 moveq.l #0,d1 ;resident structure in rom 
	 move.l D0,A1
	 
	 CALLSYS InitResident
	
	 ;PRINTF 1,<'InitResident resulted in %lx',13,10>,D0
	 beq     close_and_dealloc
	
device_present:
	move.l  d0,residentstructure(a5)
	move.l  residentstructure(a5),a1	
	move.l  md_ATARdWt(a1),ATARdWtRoutine(a5)
	IFGE	DEBUG_DETAIL-1	
	move.l  ATARdWtRoutine(a5),d0
	PRINTF 1,<'ATARdWt routine located at %lx',13,10>,D0
	ENDC
	; Open expansion.library
	lea     expname,a1
	moveq   #0,d0
	move.l  ABSEXECBASE,a6
	CALLSYS  OpenLibrary
	tst.l   d0
	beq     close_and_dealloc
	 move.l  d0,expansionlib(a5)
	
	;PRINTF 1,<'Opened expansion lib: %lx',13,10>,D0
	move.l  d0,a6

	;now build a fake config dev!
	CALLSYS AllocConfigDev
	;PRINTF 1,<'Created Config dev: %lx',13,10>,D0
	move.l  d0,configdev(a5)
	beq     close_and_dealloc
	;init the ConfigDev
	move.l  d0,a0
	lea.l   fakebootrom,a1
	move.l  a1,cd_Rom+er_Reserved0c(a0) ;save the diag entry
	move.l  #IDE_BASE_ADDRESS,cd_BoardAddr(a0) ;save the board adress
	move.l  #65536,cd_BoardSize(a0) ;store the board size
	move.b  #ERTF_DIAGVALID+ERT_ZORROII,cd_Rom+er_Type(a0) ; this makes the thing autoboot
	move.w  #2588,cd_Rom+er_Manufacturer(a0) ;a1k org :D
	move.b  #123,cd_Rom+er_Product(a0)
	CALLSYS AddConfigDev ;add it to the system

	move.l  #0,unitnum(a5)
check_unit:
	 ;now open the device!
	move.l  unitnum(a5),d0
	 PRINTF 1,<'Opening unit %lx',13,10>,d0
	cmp.l #0,d0 ; is it unit 0?
	beq find_the_drive
	cmp.l #10,d0 ;is it unit 10?
	bne  next_unit
	move.l #1,d0
find_the_drive:   
	 bsr FindDrive ;find if there is at least one drive on the bus and check if ready (spin up for HDD)
	 bne next_unit
	 bsr open_device
	;check result
	cmp.b  #IOERR_OPENFAIL,D0   
	beq next_unit

	; Read rdb block here!
	moveq  #0,d4
	move.l #0,d0  
	move.l #IDNAME_RIGIDDISK,d6

search_rdb:
	cmp.l  #MAX_BLOCK_SEARCH_RDB,d4
	bge    next_unit  ;nothing found!
	move.l rdbmem(a5),a0
	PRINTF 1,<'Searching for rdb at block %ld, offset %lx',13,10>,d4,d0
	bsr    read_block
	bne    next_unit ;on error go to next unit
	;theoretically i have to checksum it...
	move.l rdb_ID(a0),d5
	PRINTF 1,<'block id: %lx, expected: %lx',13,10>,d5,d6
	cmp.l  d5,d6
	beq    rdb_found   
	addi.l #512,d0  
	addi.l #1,d4
	bra    search_rdb

rdb_found:  
	PRINTF 1,<'Found a rdb at block %ld',13,10>,d4
	move.l rdbmem(a5),a0 
	move.l #IDNAME_PARTITION,d6
	move.l rdb_PartitionList(a0),d0 ;there is our first partition
	move.l buffermem(a5),a0 ;work on buffermem now!
	
prepare_partition:
	PRINTF 1,<'Loading partition in block: %lx',13,10>,d0
	lsl    #8,d0 ;multiply by 512
	lsl    #1,d0
	bsr    read_block
	bne    next_unit ;on error go to next unit
	move.l pb_ID(a0),d5
	cmp.l  d5,d6
	beq    found_partition 
	bra    next_unit
	   
found_partition   
	lea.l	pb_DriveName(a0),a1	;get the drive name (BSTR)
	 clr.w	d1
	 move.b	(a1)+,d1		;null terminate it (for safety)
	 clr.b	0(a1,d1.w)
	; now check for dos-name duplicates   
	lea.l	pb_DriveName(a0),a1	;get the drive name (BSTR)
	move.b	(a1)+,d1            ;BSTR->ASTR
	;PRINTF 1,<'Found Dos name: %s, length %lx',13,10>,a1,d1
	bsr    patch_dosname
	bne    close_and_dealloc ; error in name patch
	PRINTF 1,<'Found Dos name: %s, length %lx',13,10>,a1,d1
	move.l parametermem(a5),a3
	move.l	a1,pp_dosName(a3)
	lea		bootdevicename,a1
	move.l	a1,pp_execName(a3)
	move.l unitnum(a5),d1
	move.l	d1,pp_unitNumber(a3)
	move.l	pb_Flags(a0),pp_flags(a3)
	lea.l	pb_Environment(a0),a0    ; start of origin
	lea.l  pp_paramSize(a3),a3      ; start of destination (first word after flag)
	moveq  #16,d0                   ; 17 long word comming one less for dbra
copy_param_packet:
	move.l (a0)+,(a3)+
	dbra   d0,copy_param_packet     
	;restore buffers
	move.l buffermem(a5),a0 
	move.l parametermem(a5),a3
	move.l #MEMF_PUBLIC,pp_BufferMemType(a3) ;fix buffer type we can operate from any memory, which should be fast-mem!
	IFGE	DEBUG_DETAIL-2
	bsr print_param_packet  
	ENDC
	btst.b   #PBFB_NOMOUNT,pp_flags+3(a3) ;+3 for long to byte
	bne      preparenextpartition   

	; Create the DOS node.
	move.l  a3,a0
	CALLSYS MakeDosNode
	tst.l   d0
	beq     close_and_dealloc
	PRINTF 1,<'Made DosNode: %lx',13,10>,D0
	move.l  d0,devicenode(a5) ; move the device node to mem
	move.l parametermem(a5),a3 ;first move parampacket to a3 (again?!?)
	;set d0/d1: priority and startproc
	move.l   pp_bootPrio(a3),d0			this priority
	moveq.l	#ADNF_STARTPROC,d1	StartProc = true
	;put the DeviceNode from mem in A0
	move.l   devicenode(a5),a0 
	move.l   #0,a1
	btst     #PBFB_BOOTABLE,pp_flags+3(a3)
	beq      add_node   ; no boot flag
	;load the ConfigDev in a1
	move.l  configdev(a5),a1
add_node:
	CALLSYS  AddBootNode	
	 PRINTF 1,<'AddBootNodeResult %lx',13,10>,d0
preparenextpartition:
	move.l buffermem(a5),a0
	move.l pb_Next(a0),d0
	PRINTF 1,<'Next partition in block: %lx',13,10>,d0
	cmp.l  #$FFFFFFFF,d0
	bne    prepare_partition
next_unit:   
	bsr    close_device
	cmp.l  #10,unitnum(a5)
	beq    close_and_dealloc
	move.l #10,unitnum(a5)
	bra    check_unit

close_and_dealloc:
	move.l  ABSEXECBASE,a6
	 tst.l	 expansionlib(a5)
	 beq     dealloc_exit1
	move.l  expansionlib(a5),a1
	CALLSYS CloseLibrary
	;PRINTF 1,<'Closed expansion lib at %lx',13,10>,a1
	move.l  #0,expansionlib(a5)
dealloc_exit1:
	tst.l   buffermem(a5) ;mem allocated?
	beq     dealloc_exit2
	move.l  buffermem(a5),a1
	move.l  #BLOCKSIZE,d0
	;PRINTF 1,<'Free %ld byte buffer mem at %lx',13,10>,d0,a1
	CALLSYS FreeMem
	move.l #0,buffermem(a5)
dealloc_exit2:
	tst.l   parametermem(a5) ;mem allocated?
	beq     dealloc_exit3
	move.l  parametermem(a5),a1
	move.l  #MyParmPkt_Sizeof,d0
	;PRINTF 1,<'Free %ld byte param mem at %lx',13,10>,d0,a1
	CALLSYS FreeMem
	move.l #0,parametermem(a5)
dealloc_exit3:
dealloc_exit4:
	tst.l   iohandler(a5) ;mem allocated?
	beq     dealloc_exit5
	move.l  iohandler(a5),a1
	move.l  #IOSTD_SIZE,d0
	;PRINTF 1,<'Free %ld byte iohandler mem at %lx',13,10>,d0,a1
	CALLSYS FreeMem
	move.l #0,iohandler(a5)   
dealloc_exit5:
	tst.l   rdbmem(a5) ;mem allocated?
	beq     dealloc_exit6
	move.l  rdbmem(a5),a1
	move.l  #BLOCKSIZE,d0
	;PRINTF 1,<'Free %ld byte buffer mem at %lx',13,10>,d0,a1
	CALLSYS FreeMem
	move.l #0,rdbmem(a5)
dealloc_exit6:
	tst.l   unitptr(a5)
	beq     dealloc_exit_final
	move.l  unitptr(a5),a1
	move.l  #MyDevUnit_Sizeof,d0
	CALLSYS FreeMem
	move.l  #0,unitptr(a5)
dealloc_exit_final
	move.l  a5,a1
	move.l  #MyMemPkt_Sizeof,d0
	CALLSYS FreeMem
end_dealloc:       
	move.l  d7,d0
	PRINTF 1,<'End: returning %lx',13,10>,D0
	movem.l (SP)+,d1-d7/a0-a6
	rts

open_device:
;   movem.l d1/a1/a6,-(SP)
;   move.l  iohandler(a5),a1
;   moveq #0,d0
;clear_iohandler_loop:   
;   move.b	#0,0(a0,d0)
;   addq.w #1,d0
;   cmp.w  #IOSTD_SIZE,d0
;   blt    clear_iohandler_loop
;   ;device in a6
;   move.l residentstructure(a5),a6
;   ;unitnum in d0
;   move.l unitnum(a5),d0
;   ;flags in d1
;   move.l #0,d1
;   ;iohandler in a1
;   move.l  iohandler(a5),a1
;   CALLLIB LIB_OPEN
;   PRINTF 1,<'Open returned %lx',13,10>,D0
;   move.l  iohandler(a5),a1
;   move.l  IO_UNIT(a1),unitptr(a5)
;   movem.l (SP)+,d1/a1/a6
;   rts

	movem.l d1/a3/a1/a6,-(SP)
	move.l  ABSEXECBASE,a6
	move.l  unitptr(a5),d0
	bne     unit_allready_there
	;PRINTF  1,<'Opening unit',13,10>
	move.l  #MyDevUnit_Sizeof,d0
	move.l  #MEMF_PUBLIC+MEMF_CLEAR,d1
	CALLSYS AllocMem
	tst.l   d0
	beq     open_error;
	
unit_allready_there:
	;PRINTF 1,<'Opend unit at %lx',13,10>,d0
	move.l   d0,a3
	move.l   d0,unitptr(a5)
	
	;------ default values
	move.b	#1,mdu_SectorBuffer(a3) ;device must at least handle one sector per read/write
	move.b	#1,mdu_actSectorCount(a3) 
	move.w   #UNKNOWN_DRV,mdu_drv_type(a3)
	move.b   #TRUE,mdu_firstcall(a3)
	move.b   #TRUE,mdu_auto(a3)
	move.w   #CHS_ACCESS,mdu_lba(a3)
	move.l   #0,mdu_sectors_per_track(a3)
	move.l   #0,mdu_heads(a3)
	move.l   #0,mdu_cylinders(a3)
	move.l   #0,mdu_numlba(a3)
	move.l   #0,mdu_numlba48(a3)   
	move.w   #TRUE,mdu_motor(a3)     ;units usually start up with motor on
	move.l   #0,mdu_change_cnt(a3)
	move.l   #FALSE,mdu_no_disk(a3)
	move.l   unitnum(a5),d0
	beq      unit_default_init_0
	;bset.b   #SLAVE_BIT,d0
	move.b   #$10,d0
unit_default_init_0:
  PRINTF 1,<'Set Unit Num %ld',13,10>,d0
	move.b   d0,mdu_UnitNum(a3)
	bsr      InitDrive 
	move.w   mdu_drv_type(a3),d0
	PRINTF 1,<'Unit is of type %d',13,10>,d0
	cmp.w    #ATA_DRV,d0
	beq      open_ok
	cmp.w    #SATA_DRV,d0
	beq      open_ok
	bra      open_error

	move.l   #0,d0
open_ok;   
	movem.l (SP)+,d1/a3/a1/a6
	rts

	
open_error:   
	tst.l   unitptr(a5)
	beq     open_error_end
	move.l  unitptr(a5),a1
	move.l  #MyDevUnit_Sizeof,d0
	CALLSYS FreeMem
	move.l  #0,unitptr(a5)
open_error_end:
	move.l #IOERR_OPENFAIL,d0
  bra    open_ok

close_device
;   movem.l a1/a6,-(SP)
;   ;device in a6
;   move.l residentstructure(a5),a6
;   ;ihandler in a1
;   move.l  iohandler(a5),a1
;   CALLLIB LIB_CLOSE
;  ;PRINTF 1,<'Close retuned %lx',13,10>,D0
;   movem.l (SP)+,a1/a6
;   rts
	movem.l d0/a1/a6,-(SP)
	move.l  ABSEXECBASE,a6
	tst.l   unitptr(a5)
	beq     close_end
	move.l  unitptr(a5),a1
	move.l  #MyDevUnit_Sizeof,d0
	CALLSYS FreeMem
	move.l  #0,unitptr(a5)
close_end:   
	movem.l (SP)+,d0/a1/a6
	rts

patch_dosname: 
	movem.l a0/a6,-(SP)    
	;PRINTF  1,<'Patching string %s',13,10>,a1
	;first find the end of the string:
	moveq.l #0,d1     ;look from start
	move.l  a1,a0
test_string:
	cmp.b   #0,(a0)+
	beq     end_of_string_found
	addq.l  #1,d1
	cmp.l   #MAXLENGTH,d1
	bls     test_string
	;PRINTF  1,<'Name too long: %d',13,10>,d1
	moveq   #1,d0 ;anything not null is an error!
	bra     end_patch_dosname 
end_of_string_found
	move.l  a1,-(SP)    ;strange bug! I have to put A1 on stack and pop it again?!?
	move.l  (SP)+,a1


	;PRINTF  1,<'String %s has a length of %ld',13,10>,a1,d1
	;name to look for is in a1!
	move.l  expansionlib(a5),a6
patch_dosname_again:
	lea     eb_MountList(a6),a0 ;find the BootNode entries
	cmp.l   #0,a0 ;lea does NOT change the status bits!
	beq     end_patch_dosname ; list empty!
	bsr     FindSameName      
	;PRINTF  1,<'FindSameName resulted in %ld',13,10>,d0
	tst.l   d0
	beq     end_patch_dosname ; no dups = all ok!
	;we have to modify this name d1 holds the end of the string!
	;add a 'x' to the name
	move.b	 #"x",0(a1,d1.w)   
	clr.b	 1(a1,d1.w); null terminate!
	addq.w  #1,d1
	;PRINTF  1,<'New name is %s, length %d',13,10>,a1,d1
	cmp.w   #MAXLENGTH,d1
	ble     patch_dosname_again ;max length reached?
	;PRINTF  1,<'Name too long: %d',13,10>,d1
	moveq   #1,d0 ;anything not null is an error!
end_patch_dosname:
	movem.l (SP)+,a0/a6
	rts

FindSameName: 
	;(a0 = eb_MountList, a1 name of boot node (null terminated), d1 length of dosname)
	movem.l d1-d2/a0-a3,-(SP)    
next_dos_node:
	SUCC	  a0,d0			; Get next node
	 beq    good_end_FindSameName	; last: no dublets found!
	 ;PRINTF 1,<'Checking node at %ld',13,10>,d0
	move.l d0,a0      ;new node
	;moveq  #0,d0
	;move.b LN_TYPE(a0),d0
	;PRINTF  1,<'It is a Node of type %ld',13,10>,d0
	;check type
	;cmp.b	#NT_BOOTNODE,LN_TYPE(a0)
	;bne.s  next_dos_node
	 ;PRINTF  1,<'It is a Boot Node',13,10>
	;check the name
	move.l bn_DeviceNode(a0),a2 ;get the device node
	move.l	dn_Name(a2),d0		; get the name (BPTR!)
	beq    next_dos_node
	lsl.l	#2,d0			; BPTR address divided by 4! mult*4 gets the address of the BSTR 
	move.l	d0,a2     ; now a2 holds the BSTR
	addq.l	#1,a2			; a2 points to the string null terminated!
	;PRINTF  1,<'Node name is %s',13,10>,a2
	move.l d1,d2      ; get max length of test string
	move.l a1,a3      ; get a save adress copy of the string in a1
string_test_loop:
	cmp.b	(a2)+,(a3)+		  ; compare caracters
	bne  	next_dos_node		; not equal: next node!
	subq.l #1,d2           ; end of length reached?
	bne.s	string_test_loop		
	; equal: arg!
	;PRINTF  1,<'Names are equal!',13,10>
bad_end_FindSameName:
	move.l  #-1,d0
	bra     end_FindSameName
good_end_FindSameName:
	;PRINTF  1,<'No equal names found!',13,10>
	move.l  #0,d0   
end_FindSameName:
	movem.l (SP)+,d1-d2/a0-a3
	rts


	IFGE	DEBUG_DETAIL-2	
print_param_packet:
	movem.l d0/a0/a3,-(SP)
	move.l parametermem(a5),a3
	move.l	pp_execName(a3),a0
	PRINTF 1,<'Device: %s',13,10>,a0
	move.l  pp_dosName(a3),a0
	PRINTF 1,<'Dos Name: %s',13,10>,a0
	move.l pp_unitNumber(a3),d0
	PRINTF 1,<'Unit: %ld',13,10>,d0
	move.l pp_flags(a3),d0
	PRINTF 1,<'Flags: %ld',13,10>,d0
	move.l pp_paramSize(a3),d0
	PRINTF 1,<'Environment size: %ld',13,10>,d0
	move.l pp_blockSize(a3),d0
	PRINTF 1,<'Block size: %ld',13,10>,d0
	move.l	pp_sectorOrigin(a3),d0
	PRINTF 1,<'Sector origin: %ld',13,10>,d0
	move.l	pp_surfaces(a3),d0
	PRINTF 1,<'Surfaces: %ld',13,10>,d0
	move.l	pp_sectorsPerBlock(a3),d0
	PRINTF 1,<'Sectors per block: %ld',13,10>,d0
	move.l	pp_blocksPerTrack(a3),d0
	PRINTF 1,<'Blocks per track: %ld',13,10>,d0
	move.l	pp_reservedBlocks(a3),d0
	PRINTF 1,<'Reserved blocks: %ld',13,10>,d0
	move.l	pp_preface(a3),d0
	PRINTF 1,<'Preface: %ld',13,10>,d0
	move.l	pp_interleave(a3),d0
	PRINTF 1,<'Interleave: %ld',13,10>,d0
	move.l	pp_lowCyl(a3),d0
	PRINTF 1,<'Low cylinder: %ld',13,10>,d0
	move.l	pp_highCyl(a3),d0
	PRINTF 1,<'High cylinder: %ld',13,10>,d0
	move.l	pp_numBuffer(a3),d0
	PRINTF 1,<'Num Buffers: %ld',13,10>,d0
	move.l	pp_BufferMemType(a3),d0
	PRINTF 1,<'Buffer type: %ld',13,10>,d0
	move.l	pp_maxTransfer(a3),d0
	PRINTF 1,<'Max transfer: %lx',13,10>,d0
	move.l	pp_mask(a3),d0
	PRINTF 1,<'Mask: %lx',13,10>,d0
	move.l	pp_bootPrio(a3),d0
	PRINTF 1,<'Boot prio: %ld',13,10>,d0
	move.l	pp_dosType(a3),d0
	PRINTF 1,<'Dostype: %lx',13,10>,d0
	movem.l (SP)+,d0/a0/a3
	rts
	ENDC

clear_block
	movem.l d0/a0,-(SP)
	;PRINTF 1,<'Clearing address %lx',13,10>,a0
	moveq #0,d0
clear_block_loop:   
	move.l	#0,(a0)+
	addq.w #4,d0
	cmp.w  #BLOCKSIZE,d0
	blt    clear_block_loop
	;PRINTF 1,<'Cleared: %lx bytes, end adress: %lx',13,10>,d0,a0
	movem.l (SP)+,d0/a0
	rts

read_block ;blocknumber in d0, buffer in a0, returns 0 if read is not successfull
	movem.l d0-d7/a0-a6,-(SP)  
	bsr    clear_block ;clear old result first!

	;ATARdWt:
	;a0 IO data address
	;a2 #READOPE/#WRITEOPE  operation is either read or write
	;a3 unitptr
	;d0 io length
	;d1 io offset low
	;d5 io offset high  !NEW!!
	;d2 unit number 
	move.l unitptr(a5),a3
	moveq  #0,d2
	move.b mdu_UnitNum(a3),d2
	PRINTF 1,<'Reading block %lx on unit %lx',13,10>,d0,d2
	move.l #READOPE,a2
	move.l #0,d5
	move.l d0,d1
	move.l #512,d0
	;PRINTF 1,<'Call ATRdWt',13,10>
	move.l   a1,-(sp)
	move.l   ATARdWtRoutine(a5),a1
  jsr      (a1)
  move.l   (sp)+,a1
	movem.l (SP)+,d0-d7/a0-a6
	rts

	IFGE	BIND_MEM-1	
addmem:
	movem.l	d0-d2/a0-a1/a6,-(sp)
  move.l  ABSEXECBASE,a6
	move.l	#MEM_BASE,a0				; a0 = Basisadresse
	move.l	#MEM_SIZE,d0
	lea	    mem_name,a1			; a1 = Name
	move.l	#MEM_PRIO,d2				; d2 = Priority
	move.l	#MEM_FLAGS,d1	; d1 = Typ
	CALLSYS	AddMemList			; Speicher einbinden
	movem.l	(sp)+,d0-d2/a0-a1/a6
	rts
	ENDC
	
;This routine finds if a drive is attached:
; first it issues a write to a register
; second it reads the status and checks "busy" and "not ready"
; if it is busy or not ready it might be there: wait 5 seconds for the drive to get ready
; if it is busy and nor ready it is a empty bus->no HW
; if it is neither busy nor "not ready" we have to read/write some registers to check if it is there
;d0 holds the unit number
	Public FindDrive
FindDrive
	movem.l  d1/d2,-(sp)	 
	PRINTF 1,<'Searching for drive %ld',13,10>,d0
	lsl.b    #4,d0
	WATABYTE d0,TF_DRIVE_HEAD            ; select the drive
	move.l   #TIMEOUT,d1                 ; wait timeout*sec 
check_status:
	WATABYTE #TESTBYTE1,TF_SECTOR_COUNT ; write first testbyte
	RATABYTE	TF_ALTERNATE_STATUS,d0                ; get status
	and.b    #BSY+DRDY,d0                ; eval Busy and DRDY  
	beq      test_registers              ; none: test registers
	cmp.b    #BSY+DRDY,d0                ; both: impossible
	beq      bad_return_from_find
	and.b    #BSY,d0
	beq      test_registers              ;Not busy->test registers
	PRINTF 1,<'Waiting to respond %ld sec',13,10>,d1
	;bad busy wait
	move.l   #500000,d0	    
wait_loop:
	tst.b    $bfe301 ;slow CIA access cycle takes 12-20 7MHz clocks: 1.7us - 2.8us
	dbra     d0,wait_loop
	dbra     d1,check_status             ; check again
	bra      bad_return_from_find        ; timeout reached: not drive here   
test_registers:
	; Write some Registers and read thew value back
	WATABYTE #TESTBYTE1,TF_SECTOR_COUNT
	 RATABYTE	TF_SECTOR_COUNT,d0
	 cmp.b	#TESTBYTE1,d0
	bne  	bad_return_from_find
	WATABYTE #TESTBYTE2,TF_SECTOR_COUNT
	 RATABYTE	TF_SECTOR_COUNT,d0
	 cmp.b	#TESTBYTE2,d0
	bne  	bad_return_from_find
	WATABYTE #TESTBYTE3,TF_SECTOR_COUNT
	 RATABYTE	TF_SECTOR_COUNT,d0
	 cmp.b	#TESTBYTE3,d0
	bne  	bad_return_from_find
	; we found a drive!
good_return_from_find:
	PRINTF 1,<'Found it!',13,10>
	move.l   #0,d0   
	bra.s    return_from_find
bad_return_from_find:
	PRINTF 1,<'Found it not!',13,10>
	move.l   #-1,d0   
return_from_find:
	movem.l  (sp)+,d1/d2
	rts

endcode:
	end

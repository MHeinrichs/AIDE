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
   include "libraries/filehandler.i"
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
   include "/lib/bootinfo.i";Structure for boot infos

            XLIB	AllocMem
            XLIB	FreeMem
            XLIB	MakeDosNode
            XLIB	AddDosNode
            XLIB	OpenLibrary
            XLIB	CloseLibrary
            XLIB	FindResident
            XLIB	InitResident
            XLIB	Enqueue

romtag:
            dc.w    $4afc
            dc.l    romtag
            dc.l    endcode
            dc.b    RTW_COLDSTART
            dc.b    1
            dc.b    NT_UNKNOWN
            dc.b    -30
            dc.l    bootname
            dc.l    bootid
            dc.l    initcode

bootname:
            dc.b    'anyboot',0
bootid:
            dc.b    'anyboot 1.0  (10 Jul 2015)',13,10,0
bootdevicename:
            dc.b    'ide.device',0
bootdosname:
            dc.b    'DH0',0

            ds.w    0

			; a2 : buffer for block
			; a3 : buffer for mount info
initcode:
            movem.l d0-d4/a0-a3/a6,-(a7)
			move.l  #0,a2 ;clear address
			move.l  #0,a3 
            ; Get 512 bytes of memory for sector buffer

            move.l  #BLOCKSIZE,d0
            moveq.l #MEMF_ANY,d1
            move.l  4,a6
            jsr     _LVOAllocMem(a6)
            tst.l   d0
            beq     bomb
            move.l  d0,a2

			; get mem for parameter packet
            move.l  #MyParmPkt_Sizeof,d0
            moveq.l #MEMF_ANY,d1
            move.l  4,a6
            jsr     _LVOAllocMem(a6)
            tst.l   d0
            beq     dealloc_exit
            move.l  d0,a3
            
            ; Read rdb block


            lea		bootdosname,a1
            move.l	a1,pp_dosName(a3)
            lea		bootdevicename,a1
            move.l	a1,pp_execName(a3)
            move.l	#0,pp_unitNumber(a3)
            move.l	#1,pp_flags(a3)
            move.l	#16,pp_paramSize(a3)  			; 16 longwords comming
            move.l	#128,pp_blockSize(a3) 			; 128*4=512 bytes in long words
            move.l	#0,pp_sectorOrigin(a3) 			;leave 0
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
            move.l	#0,pp_bootPrio(a3)
            move.l	#$444F5301,pp_dosType(a3) 		;FFS

            ; Open expansion.library

            lea     expname(pc),a1
            moveq   #0,d0
            move.l  4,a6
            jsr     _LVOOpenLibrary(a6)
            tst.l   d0
            beq     dealloc_exit

            ; Create the DOS node.

            move.l  d0,a6
            move.l  a3,a0
            jsr     _LVOMakeDosNode(a6)
            tst.l   d0
            beq.s   closelib_bomb

            ; Initialize the rest of the DOS node.

            move.l  d0,a0
            clr.l   dn_Task(a0)
            move.l  #4000,dn_StackSize(a0)
            move.l  #0,dn_Priority(a0)
            move.l  #-1,dn_GlobalVec(a0)
            move.l  d2,dn_SegList(a0)

            move.l  a0,d2

            ; Allocate memory for a boot node.

            moveq   #20,d0
            move.l  #MEMF_ANY!MEMF_CLEAR,d1
            move.l  a6,-(a7)
            move.l  4,a6
            jsr     _LVOAllocMem(a6)
            move.l  (a7)+,a6
            tst.l   d0
            beq.s   closelib_bomb

            ; Initialize the boot node.

            move.l  d0,a1
            move.b  #NT_BOOTNODE,8(a1)          ; Set node type.
            lea     fakebootrom(pc),a0
            move.l  a0,10(a1)                   ; Set fake configdev pointer
            move.l  d2,16(a1)                   ; Set device node pointer
            move.b  103(a2),9(a1)               ; Set priority

            ; Enqueue the boot node in ExpansionBase->eb_Mountlist.

            move.l  a6,-(a7)
            lea     74(a6),a0
            move.l  4,a6
            jsr     _LVOEnqueue(a6)
            move.l  (a7)+,a1
            jsr     _LVOCloseLibrary(a6)
            move.l  #20,d0
            jsr     _LVOFreeMem(a6)

            bra.s   dealloc_exit

closelib_bomb:
            move.l  a6,a1
            move.l  4,a6
            jsr     _LVOCloseLibrary(a6)

dealloc_exit:
			move.l  a2,d0 ;mem allocated?
			beq     dealloc_exit2
            move.l  a2,a1
            move.l  #BLOCKSIZE,d0
            jsr     _LVOFreeMem(a6)            
dealloc_exit2:
			move.l  a3,d0 ;mem allocated?
			beq     bomb
            move.l  a3,a1
            move.l  #MyParmPkt_Sizeof,d0
            jsr     _LVOFreeMem(a6)
            

bomb:       movem.l (a7)+,d0-d4/a0-a3/a6
            rts

expname:    dc.b    'expansion.library',0


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

endcode:
            end

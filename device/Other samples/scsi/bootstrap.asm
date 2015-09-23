
            include "exec/types.i"
            include "exec/memory.i"
            include "exec/resident.i"
            include "libraries/configregs.i"
            include "libraries/filehandler.i"

            xref    _LVOAllocMem
            xref    _LVOFreeMem
            xref    _LVOMakeDosNode
            xref    _LVOAddDosNode
            xref    _LVOOpenLibrary
            xref    _LVOCloseLibrary
            xref    _LVOFindResident
            xref    _LVOInitResident
            xref    _LVOEnqueue

            xref    readsect
            xref    dopen
            xref    dread
            xref    LoadSeg
            xref    UnLoadSeg

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
            dc.b    'scsiboot',0
bootid:
            dc.b    'scsiboot 1.0 (28 Oct 1989)',13,10,0

            ds.w    0

initcode:
            movem.l d0-d4/a0-a2/a6,-(a7)

            ; Get 512+16 bytes of memory for sector buffer and file handle.

            move.l  #512+16,d0
            moveq.l #MEMF_ANY,d1
            move.l  4,a6
            jsr     _LVOAllocMem(a6)
            tst.l   d0
            beq     bomb
            move.l  d0,a2
            lea     512(a2),a3

            ; Read block zero and verify boot signature.

            moveq   #0,d0
            move.l  a2,a0
            bsr     readsect
            tst.l   d0
            bne     dealloc_exit
            cmp.l   #$424c4b30,(a2)
            bne     dealloc_exit
            cmp.l   #$bdb3b4cf,4(a2)
            bne     dealloc_exit

            ; Save the file system load information.

            move.l  16(a2),d3
            move.l  20(a2),d4

            ; Load the device driver code.

            move.l  8(a2),d0
            move.l  12(a2),d1
            move.l  a2,a0
            move.l  a3,a1
            bsr     dloadseg
            tst.l   d0
            beq     dealloc_exit

            ; Activate the device driver.

            move.l  d0,d2
            bsr     startdevice
            tst.l   d0
            beq.s   start_ok
            move.l  d2,d1
            lea     _LVOFreeMem(a6),a1
            bsr     UnLoadSeg
            bra     dealloc_exit

            ; Load the FFS code.  If it fails, too bad, the device
            ; driver is already up and running so it must stay.

start_ok:
            move.l  d3,d0
            move.l  d4,d1
            move.l  a2,a0
            move.l  a3,a1
            bsr     dloadseg
            tst.l   d0
            beq     dealloc_exit

            ; Save FFS seglist.

            move.l  d0,d2

            ; Read block zero.

            moveq   #0,d0
            move.l  a2,a0
            bsr     readsect
            tst.l   d0
            bne     unloadfs_bomb

            ; Fix the pointers in the data structure.

            move.l  a2,d0
            add.l   d0,24(a2)
            add.l   d0,28(a2)

            ; Open expansion.library

            lea     expname(pc),a1
            moveq   #0,d0
            move.l  4,a6
            jsr     _LVOOpenLibrary(a6)
            tst.l   d0
            beq     unloadfs_bomb

            ; Create the DOS node.

            move.l  d0,a6
            lea     24(a2),a0
            jsr     _LVOMakeDosNode(a6)
            tst.l   d0
            beq.s   closelib_bomb

            ; Initialize the rest of the DOS node.

            move.l  d0,a0
            clr.l   dn_Task(a0)
            move.l  108(a2),dn_StackSize(a0)
            move.l  112(a2),dn_Priority(a0)
            move.l  116(a2),dn_GlobalVec(a0)
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
            bra.s   dealloc_exit

closelib_bomb:
            move.l  a6,a1
            move.l  4,a6
            jsr     _LVOCloseLibrary(a6)

unloadfs_bomb:

            move.l  d2,d1
            lea     _LVOFreeMem(a6),a1
            bsr     UnLoadSeg

dealloc_exit:

            move.l  a2,a1
            move.l  #512+16,d0
            jsr     _LVOFreeMem(a6)

bomb:       movem.l (a7)+,d0-d4/a0-a2/a6
            rts

            ;----------------------------------------------------
            ;  error = startdevice(seglist)
            ;  D0                  D0
            ;

startdevice:
            move.l  d0,a1
            asl.l   #2,d0
            addq.l  #4,d0
            move.l  d0,a0
            move.l  -8(a0),d1
            lsr.l   #1,d1

            ; A0 now points to first word of first hunk, D1 contains number
            ; of words in hunk.

            subq.l  #1,d1
scan:
            cmp.w   #$4afc,(a0)
            bne.s   loopend
            cmp.l   2(a0),a0
            beq.s   foundtag
loopend:
            addq.l  #2,a0
            dbra    d1,scan
            moveq   #-1,d0
            rts

            ; A0 now points to the RomTag structure found.

foundtag:
            move.l  a1,d1
            move.l  a0,a1
            jsr     _LVOInitResident(a6)
            moveq   #0,d0
            rts

            ;----------------------------------------------------
            ;  seglist = dloadseg(firstblock,length,buffer,handle)
            ;  D0                 D0         D1     A0     A1
            ;

dloadseg:
            move.l  a2,-(a7)
            move.l  a1,a2
            bsr     dopen
            move.l  a2,d1
            lea     _LVOAllocMem(a6),a0
            lea     _LVOFreeMem(a6),a1
            lea     dread(pc),a2
            bsr     LoadSeg
dopen_ok:
            move.l  (a7)+,a2
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

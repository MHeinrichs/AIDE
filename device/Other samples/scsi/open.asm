        xdef    dopen
        xdef    dread
        xref    readsect

        ; Handle structure:
        ;
        ; Longword 0 - next sector to read
        ; Longword 1 - next byte to read in buffer
        ; Longword 2 - buffer address
        ; Longword 3 - total bytes left to read


        ;---------------------------------------------------
        ; error = dopen(firstblock,length,buffer,handle)
        ; D0            D0         D1     A0     A1
        ;
        ; header_offset is the location in block 0 where
        ; the file dump information (block #, byte size) is.
        ;
        ; handle is the location of a private data area to
        ; be used by dopen and dread.
        ;
        ; buffer is the location of a 512-byte sector buffer.

dopen:
        move.l  d0,(a1)
        move.l  #512,4(a1)
        move.l  a0,8(a1)
        move.l  d1,12(a1)
        rts

        ;---------------------------------------------------
        ; actual = dread(handle,buffer,count)
        ; D0             D1     A1     D0
        ;

dread:
        movem.l d2-d3/a2,-(sp)
        move.l  d0,d2
        move.l  d1,a2
        move.l  a0,a1

        ; Figure out total number of bytes to move (minimum of number
        ; requested and number remaining in file) and subtract from
        ; number remaining in file.  D2 and D3 both get number of bytes
        ; to move.

        cmp.l   12(a2),d2
        bls.s   notless
        move.l  12(a2),d2
notless:
        move.l  d2,d3
        sub.l   d2,12(a2)

        ; Sector loop:  Compute how much remains in current buffer.

sectloop:
        move.l  #512,d1
        sub.l   4(a2),d1

        ; Find minimum of what remains to transfer (D2) and what
        ; remains in buffer (D1).

        cmp.l   d2,d1
        bls.s   notless2
        move.l  d2,d1

notless2:

        ; Compute buffer pointer.

        move.l  8(a2),a0
        add.l   4(a2),a0

        ; Subtract this batch from total to transfer (D2) and add
        ; to buffer pointer.

        sub.l   d1,d2
        add.l   d1,4(a2)

        ; Transfer the right number of bytes.

        move.l  d1,d0
        swap    d0
        bra.s   loopbot
looptop:
        move.b  (a0)+,(a1)+
loopbot:
        dbra    d1,looptop
        dbra    d0,looptop

        ; If the current request is satisfied, D2 will have become zero
        ; earlier and we can go.

        tst.l   d2
        beq.s   alldone

        ; Read a buffer full of data and increment the sector number.

        move.l  a1,-(sp)
        move.l  (a2),d0
        addq.l  #1,(a2)
        move.l  8(a2),a0
        bsr     readsect
        move.l  (sp)+,a1

        ; If successful, move another batch of data.

        clr.l   4(a2)
        tst.l   d0
        beq     sectloop

        ; If not successful, pretend EOF.

        moveq   #0,d3
        move.l  d3,12(a2)

alldone:
        move.l  d3,d0
        movem.l (sp)+,d2-d3/a2
        rts

        end

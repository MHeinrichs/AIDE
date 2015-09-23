;:ts=8

                include "registers.i"

                xdef    readsect

                ;-------------------------------------------------------
                ; error = readsect(secnum,buffer)
                ; D0               D0     A0
                ;
                ; Registers D0-D1, A0-A1 corrupted.
                ;
                ; Reads one sector from SCSI device 1, LUN 0 into memory
                ; pointed to by A0.  Returns zero if OK, non-zero if an
                ; error occurred.

readsect        movem.l a2-a4/d2-d5,-(sp)

                subq.l  #6,a7
                move.l  a7,a4
                move.l  a0,a1
                move.l  d0,d5

                ; 6-byte read/write command.

doread:         move.l  d5,(a4)
                move.b  #8,(a4)
                move.w  #$0100,4(a4)
                moveq   #5,d2

                bsr     select
                bsr     sendcmd

                bsr     waitreq
                cmp.b   #1,d0
                bne     nodataphase

                move.b  #1,p3
                move.b  #2,p2
                move.b  #0,p7

                lea     p5r,a0
                move.l  #dma_read,a3
                moveq   #6,d2

                move.l  a1,a2
                move.w  #511,d1

skipit4:        btst.b  d2,(a0)
                beq.s   skipit4
                move.b  (a3),(a2)+
                dbra    d1,skipit4

                move.b  #0,p2

                ; Finish up the command.  The SCSI device had better
                ; be in status phase, or else...

nodataphase:    bsr     finishcmd

                ; Check the "check condition" bit in the status byte.

                btst    #1,d4
                beq     trans0

                ; Request 20 bytes or less of sense information.
req_sense:
                bsr     select
                move.l  #$03000000,(a4)
                move.w  #$1400,4(a4)
                moveq   #5,d2
                bsr     sendcmd

                ; Read first and third byte of sense information.

                move.b  #1,d1
                bsr     phrcv
                move.b  d0,d2
                bsr     phrcv
                bsr     phrcv
                move.b  d0,d3

                ; Receive rest of sense info.
senseloop:
                bsr     waitreq
                cmp.b   #1,d0
                bne.s   sense_end
                bsr     rcv
                bra.s   senseloop
sense_end:
                bsr     finishcmd

                ; If the error is "unit attention", then retry the command.
                ; Fail for all other errors.

                bclr    #7,d2
                cmp.b   #$70,d2
                bne.s   iserror
                and.b   #$0f,d3
                cmp.b   #$06,d3
                bne.s   iserror
                bra     doread

iserror:        moveq   #-1,d0
                bra.s   r_exit

trans0:         moveq   #0,d0

r_exit:         addq.l  #6,a7
                movem.l (sp)+,a2-a4/d2-d5
                rts

bomb:           move.l  a4,a7
                bra.s   iserror

phsend:         swap    d0
                bsr.s   waitreq
                cmp.b   d0,d1
                bne     bomb
                swap    d0
                bra.s   send

phrcv:          bsr.s   waitreq
                cmp.b   d0,d1
                bne     bomb
                bra.s   rcv

waitreq:        move.b  p4r,d0
                btst    #5,d0
                beq.s   waitreq
                lsr.b   #2,d0
                and.b   #7,d0
                rts

send:           move.b  d0,p0
                move.b  #1,p1
                move.b  #17,p1
                bra.s   waitack

rcv:            move.b  p0r,d0
                move.b  #16,p1

waitack:        btst.b  #5,p4r
                bne.s   waitack
                move.b  #0,p1
                rts

select:         move.b  #0,p1
                move.b  #0,p3
                move.b  #3,p0
                move.b  #5,p1

                move.w  #52646,d0
waitsel:        btst.b  #6,p4r
                dbne    d0,waitsel
                beq     sel_fail

                move.b  #0,p1
                move.b  #0,p2
                rts

sel_fail:       move.b  #0,p1
                bra     bomb

sendcmd:        move.b  #2,p3
                moveq   #2,d1
                move.l  a4,a3
cmdloop:        move.b  (a3)+,d0
                bsr     phsend
                dbra    d2,cmdloop
                rts

finishcmd:      move.b  #3,d1
                bsr     phrcv
                move.b  d0,d4

                ; Get the disconnect message.

                move.b  #7,d1
                bsr     phrcv

                ; Wait for the actual disconnect.

waitdisc:       btst.b  #6,p4r
                bne.s   waitdisc
                rts

                end

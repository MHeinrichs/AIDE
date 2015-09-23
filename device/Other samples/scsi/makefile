#
#  Make everything by default.
#
all: scsi-disk.device geterr resetscsi bootstrap.u edboot dumpfile \
     automount endskip.u wackinit.u test dtest f2hd tape scandisk \
     tape-handler hd2f
#
#  Remove all the intermediate files.
#
clean:
    delete *.o *.o32 *.bak
#
#  Default rules for assembling and compiling.
#
.SUFFIXES: .c .o32 .o .asm

.c.o:
    cc $*.c
.c.o32:
    cc +L -o $*.o32 $*.c
.asm.o:
    a68k -iinclude: -q100 $*.asm
#
#  Make a kickstart disk with the bootstrap on it.
#
kickstart: endskip.u bootstrap.u wackinit.u
    @echo " "
    @echo "Insert 1.3 kickstart disk to be modified, then press RETURN."
    @read
    kickpatch endskip.u
    kickpatch wackinit.u
    kickpatch bootstrap.u
    sumkick df0: -f
    @echo " "
    @echo "Operation complete."

tape-handler: tapedev.o32 req.o32 misc.o32
    ln +Q -o tape-handler tapedev.o32 req.o32 misc.o32 -lc32
    copy tape-handler l:

scandisk: scandisk.o
    ln +Q scandisk -lc

f2hd: f2hd.o32
    ln +Q f2hd.o32 -lc32
    copy f2hd c:

hd2f: hd2f.o32
    ln +Q hd2f.o32 -lc32
    copy hd2f c:

tape: tape.o
    ln +Q tape -lc
    copy tape c:

test: test.o
    ln +Q test -lc

dtest: dtest.o
    ln +Q dtest -lc
    copy dtest c:

edboot: edboot.o
    ln +Q edboot -lc

dumpfile: dumpfile.o
    ln +Q dumpfile -lc

automount: automount.o loadseg.o open.o readsect.o
    blink automount.o loadseg.o open.o readsect.o to automount lib lib:amiga.lib

endskip.u: endskip.asm
    a68k endskip.asm
    blink endskip.o
    unhunk -t 16515260 endskip endskip.u
    delete endskip endskip.o

wackinit.u: wackinit.asm
    a68k wackinit.asm
    blink wackinit.o
    unhunk -t 16524086 wackinit wackinit.u
    delete wackinit wackinit.o

bootstrap.u: bootstrap
    unhunk -t 16524090 bootstrap bootstrap.u

bootstrap: bootstrap.o loadseg.o open.o readsect.o
    blink bootstrap.o loadseg.o open.o readsect.o to bootstrap lib lib:amiga.lib

readsect.o: readsect.asm registers.i

scsi-disk.device: scsi.o
    blink scsi.o to scsi-disk.device lib lib:amiga.lib

scsi.o: scsi.asm registers.i

resetscsi: resetscsi.o
    ln +Q resetscsi -lc
    copy resetscsi c:

geterr: geterr.o
    ln +Q geterr -lc
    copy geterr c:

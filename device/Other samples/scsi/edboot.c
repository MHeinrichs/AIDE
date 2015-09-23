#include <exec/ports.h>
#include <exec/io.h>
#include "block0.h"

struct MsgPort *mp;
struct IOStdReq *iob;

extern struct MsgPort *CreatePort();
extern struct IOStdReq *CreateStdIO();
extern long OpenDevice();

extern int Enable_Abort;

struct block0 blk;
char fsbuf[100];
char devbuf[100];

#define DATA_OFFSET (&blk.data[0] - (char *)&blk)

char inbuf[100];

gethex(what,where)
char *what;
long *where;
{
    printf("%s [$%lx] $",what,*where);
    gets(inbuf);
    if(*inbuf) sscanf(inbuf,"%lx",where);
}

getdec(what,where)
char *what;
long *where;
{
    printf("%s [%ld] ",what,*where);
    gets(inbuf);
    if(*inbuf) sscanf(inbuf,"%ld",where);
}

getstr(what,where)
char *what;
char *where;
{
    char a = *where;
    printf("%s [%s] ",what,where);
    gets(where);
    if(!*where) *where = a;
}

main()
{
    int i;
    long devnum = 1;

    Enable_Abort = 0;

    mp = CreatePort(0L,0L);
    if(!mp) exit(99);
    iob = CreateStdIO(mp);
    if(!iob) {
        DeletePort(mp);
        exit(99);
    }

    getdec("SCSI device number",&devnum);

    if(OpenDevice("scsi-disk.device",devnum,iob,0L)) {
        printf("Unable to open device driver.\n");
        DeleteStdIO(iob);
        DeletePort(mp);
        exit(99);
    }

    iob->io_Command = 2;
    iob->io_Data = (APTR) &blk;
    iob->io_Length = 512;
    iob->io_Offset = 0;
    DoIO(iob);
    if(iob->io_Error) {
        printf("Unable to read block 0.\n");
        goto bomb;
    }
    if(blk.signature[0] != BLK0 || blk.signature[1] != ~BLK0 ||
       blk.tablesize != 16 || blk.mustbe0 != 0 || blk.mustbe1 != 1) {
        printf("Block zero not valid.  Initializing defaults.\n");
        blk.signature[0] = BLK0;
        blk.signature[1] = ~BLK0;
        blk.device_start = 0;
        blk.device_length = 0;
        blk.fs_start = 0;
        blk.fs_length = 12248;
        strcpy(fsbuf,"DH0");
        strcpy(devbuf,"scsi-disk.device");
        blk.unit = 1;
        blk.flags = 1;
        blk.tablesize = 16;
        blk.blocksize = 128;
        blk.mustbe0 = 0;
        blk.heads = 0;
        blk.mustbe1 = 1;
        blk.secpertrack = 0;
        blk.reserved = 2;
        blk.prealloc = 0;
        blk.interleave = 0;
        blk.lowcyl = 0;
        blk.highcyl = 0;
        blk.buffers = 30;
        blk.bufmemtype = 1;
        blk.maxtransfer = 0x7fffffff;
        blk.mask = 0xfffffffe;
        blk.bootpri = -1;
        blk.dostype = 0x444f5301;
        blk.fs_stack = 4000;
        blk.fs_pri = 10;
        blk.fs_globalv = -1;
    } else {
        strcpy(fsbuf,((char *)&blk)+blk.dosname);
        strcpy(devbuf,((char *)&blk)+blk.devname);
    };

    getstr("Name of disk",fsbuf);
    getstr("Name of device driver",devbuf);
    getdec("Sector number of device driver dump",&blk.device_start);
    getdec("Exact length of device driver",&blk.device_length);
    getdec("Sector number of FFS code dump",&blk.fs_start);
    getdec("Exact length of FFS code",&blk.fs_length);
    getdec("Unit number for OpenDevice",&blk.unit);
    getdec("Flags for OpenDevice",&blk.flags);
    getdec("Number of heads",&blk.heads);
    getdec("Number of sectors per track",&blk.secpertrack);
    getdec("Reserved blocks at start",&blk.reserved);
    getdec("Lowest cylinder number",&blk.lowcyl);
    getdec("Highest cylinder number",&blk.highcyl);
    getdec("Number of buffers",&blk.buffers);
    getdec("Memory type for buffers",&blk.bufmemtype);
    gethex("MaxTransfer (in bytes)",&blk.maxtransfer);
    getdec("Boot priority",&blk.bootpri);

    i = strlen(fsbuf);
    strcpy(&blk.data[0],fsbuf);
    blk.dosname = DATA_OFFSET;
    strcpy(&blk.data[i+1],devbuf);
    blk.devname = DATA_OFFSET+i+1;

    for(i=i+strlen(devbuf)+1;i<DATA_SIZE;i++) blk.data[i] = 0;

    iob->io_Command = 3;
    iob->io_Data = (APTR) &blk;
    iob->io_Length = 512;
    iob->io_Offset = 0;
    DoIO(iob);
    if(iob->io_Error) printf("Unable to write block 0 back.\n");

bomb:
    CloseDevice(iob);
    DeleteStdIO(iob);
    DeletePort(mp);
}

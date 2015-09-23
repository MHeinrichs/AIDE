#include <exec/ports.h>
#include <exec/io.h>
#include <exec/memory.h>
#include <devices/trackdisk.h>

#define NUMTRKS 10
#define TD_BUFSIZE (11L * 1024L * NUMTRKS)
#define NUMBLOCKS (NUMTRKS * 22)

typedef unsigned long ulong;
typedef unsigned short ushort;
typedef unsigned char uchar;

struct MsgPort *td_mp;
struct IOStdReq *td_iob;

extern struct MsgPort *CreatePort();
extern struct IOStdReq *CreateStdIO();
extern long OpenDevice();
extern void *AllocMem();

extern int Enable_Abort;

uchar *diskbuf;

main()
{
    ulong i;

    Enable_Abort = 0;

    td_mp = CreatePort(0L,0L);
    if(!td_mp) exit(99);
    td_iob = CreateStdIO(td_mp);
    if(!td_iob) {
        DeletePort(td_mp);
        exit(99);
    }
    if(OpenDevice("trackdisk.device",0L,td_iob,0L)) {
        printf("Unable to open floppy device driver.\n");
        DeleteStdIO(td_iob);
        DeletePort(td_mp);
        exit(99);
    }

    diskbuf = AllocMem(TD_BUFSIZE,MEMF_PUBLIC|MEMF_CHIP);
    if(!diskbuf) {
        printf("Unable to obtain %ld byte buffer in chip memory.\n",TD_BUFSIZE);
        CloseDevice(td_iob);
        DeleteStdIO(td_iob);
        DeletePort(td_mp);
        exit(99);
    }
    for(i=0;i<80;i+=NUMTRKS) {
        printf("Reading tracks %ld to %ld.\n",i,i+NUMTRKS-1);
        td_iob->io_Command = CMD_READ;
        td_iob->io_Data = (APTR) diskbuf;
        td_iob->io_Length = TD_BUFSIZE;
        td_iob->io_Offset = 11L * 1024L * i;
        DoIO(td_iob);
        if(td_iob->io_Error) {
            printf("Floppy read failed, error number %d.\n",td_iob->io_Error);
            break;
        }
    }
    td_iob->io_Command = TD_MOTOR;
    td_iob->io_Length = 0;
    DoIO(td_iob);

    FreeMem(diskbuf,TD_BUFSIZE);
    CloseDevice(td_iob);
    DeleteStdIO(td_iob);
    DeletePort(td_mp);
}

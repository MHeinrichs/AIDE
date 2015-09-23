#include <exec/ports.h>
#include <exec/io.h>
#include <exec/memory.h>
#include <devices/trackdisk.h>
#include <stdio.h>
#include <fcntl.h>

#define FILE_SIG 0x4449534b
#define NUMTRKS 10
#define TD_BUFSIZE (11 * 1024 * NUMTRKS)
#define NUMBLOCKS (NUMTRKS * 22)

typedef unsigned long ulong;
typedef unsigned char uchar;

struct fhb {
    ulong sig1;
    ulong sig2;
    uchar name[504];
};

struct fhb header;

struct MsgPort *td_mp;
struct IOStdReq *td_iob;

char fname[255];

extern struct MsgPort *CreatePort();
extern struct IOStdReq *CreateStdIO();
extern long OpenDevice();
extern void *AllocMem();
extern int open();
extern int write();

extern int Enable_Abort;

uchar *diskbuf;

main()
{
    int fd;
    long i;
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
    header.sig1 = FILE_SIG;
    header.sig2 = ~FILE_SIG;
    printf("Disk name? ");
    gets(&header.name);
    printf("Output file name? ");
    gets(fname);

    fd = open(fname,O_CREAT|O_WRONLY);
    if(fd<0) printf("Unable to create output file.\n");
    else {
        if(sizeof(header)!=write(fd,&header,sizeof(header)))
            printf("Error writing to file.\n");
        else {
            for(i=0;i<80;i+=NUMTRKS) {
                printf("Reading tracks %ld to %ld.\r",i,i+NUMTRKS-1);
                fflush(stdout);
                td_iob->io_Command = CMD_READ;
                td_iob->io_Data = (APTR) diskbuf;
                td_iob->io_Length = TD_BUFSIZE;
                td_iob->io_Offset = 11 * 1024 * i;
                DoIO(td_iob);
                if(td_iob->io_Error) {
                    printf("Floppy read failed, error number %d.\n",td_iob->io_Error);
                    break;
                } else {
                    printf("Writing                 \r");
                    fflush(stdout);
                    if(TD_BUFSIZE != write(fd,diskbuf,TD_BUFSIZE)) {
                        printf("Error writing to file.\n");
                        break;
                    }
                }
            }
        }
        close(fd);
    }
    printf("                             \r");
    fflush(stdout);

    FreeMem(diskbuf,TD_BUFSIZE);

    td_iob->io_Command = TD_MOTOR;
    td_iob->io_Length = 0;
    DoIO(td_iob);

    CloseDevice(td_iob);
    DeleteStdIO(td_iob);
    DeletePort(td_mp);
}

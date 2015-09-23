#include <exec/ports.h>
#include <exec/io.h>
#include <fcntl.h>

struct MsgPort *mp;
struct IOStdReq *iob;

extern struct MsgPort *CreatePort();
extern struct IOStdReq *CreateStdIO();
extern long OpenDevice();

extern int Enable_Abort;

char secbuf[512];
char namebuf[100];

getdec(what,where)
char *what;
long *where;
{
    printf("%s [%ld] ",what,*where);
    gets(namebuf);
    if(*namebuf) sscanf(namebuf,"%ld",where);
}

main()
{
    long start,block;
    long count;
    int length;
    int fd;
    int i;
    long devnum = 1;

    Enable_Abort = 0;

    getdec("SCSI device number",&devnum);

    printf("File name? ");
    scanf("%99s",namebuf);

    fd = open(namebuf,O_RDONLY);
    if(fd<0) {
        printf("Unable to open file.\n");
        exit(20);
    }

    printf("Starting sector number? ");
    scanf("%ld",&start);

    block = start;
    count = 0;

    mp = CreatePort(0L,0L);
    if(!mp) exit(99);
    iob = CreateStdIO(mp);
    if(!iob) {
        DeletePort(mp);
        exit(99);
    }
    if(OpenDevice("scsi-disk.device",devnum,iob,0L)) {
        printf("Unable to open device driver.\n");
        DeleteStdIO(iob);
        DeletePort(mp);
        exit(99);
    }

    while(length=read(fd,secbuf,512)) {
        for(i=length;i<512;i++) secbuf[i] = 0;
        iob->io_Command = 3;
        iob->io_Data = (APTR) &secbuf;
        iob->io_Length = 512;
        iob->io_Offset = block << 9;
        DoIO(iob);
        if(iob->io_Error) {
            printf("Got error writing block %d.\n",block);
            goto bomb;
        }
        count += length;
        block += 1;
    }
    printf("File length %ld, blocks %ld to %ld.\n",count,start,block-1);

bomb:
    close(fd);
    CloseDevice(iob);
    DeleteStdIO(iob);
    DeletePort(mp);
}

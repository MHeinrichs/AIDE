#include <exec/ports.h>
#include <exec/io.h>

#define MAXSIZE 255

struct MsgPort *mp;
struct IOStdReq *iob;

unsigned char errbuf[MAXSIZE];

extern struct MsgPort *CreatePort();
extern struct IOStdReq *CreateStdIO();
extern long OpenDevice();

extern int Enable_Abort;

main()
{
    int i;

    Enable_Abort = 0;

    mp = CreatePort(0L,0L);
    if(!mp) exit(99);
    iob = CreateStdIO(mp);
    if(!iob) {
        DeletePort(mp);
        exit(99);
    }
    if(OpenDevice("scsi-disk.device",0L,iob,0L)) {
        printf("Unable to open device driver.\n");
        DeleteStdIO(iob);
        DeletePort(mp);
        exit(99);
    }

    iob->io_Data = (APTR)errbuf;
    iob->io_Length = MAXSIZE;
    iob->io_Command = 29;

    DoIO(iob);

    if(iob->io_Error)
        printf("No sense information has been retrieved yet.\n");
    else {
        for(i=0;i<iob->io_Actual;i++) printf("%02x ",errbuf[i]);
        putchar('\n');
    }
    CloseDevice(iob);
    DeleteStdIO(iob);
    DeletePort(mp);
}

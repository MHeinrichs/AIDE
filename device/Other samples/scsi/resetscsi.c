#include <exec/ports.h>
#include <exec/io.h>

struct MsgPort *mp;
struct IOStdReq *iob;

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

    iob->io_Command = 30;

    DoIO(iob);

    CloseDevice(iob);
    DeleteStdIO(iob);
    DeletePort(mp);
}

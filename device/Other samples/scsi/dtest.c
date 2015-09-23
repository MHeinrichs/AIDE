#include <exec/ports.h>
#include <exec/io.h>
#include <devices/scsidisk.h>

#define BUFSIZE 32768

unsigned char buffer[BUFSIZE];
char inbuf[100];
char devnamebuf[100];
unsigned char cmdbuf[10];

struct MsgPort *mp;
struct IOStdReq *iob;

struct SCSICmd cmd;

extern struct MsgPort *CreatePort();
extern struct IOStdReq *CreateStdIO();
extern long OpenDevice();

extern int Enable_Abort;

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
    int i,j,c;
    long offset;
    long li,unit,flags;
    int keepgoing = 1;

    Enable_Abort = 0;

    mp = CreatePort(0L,0L);
    if(!mp) exit(99);
    iob = CreateStdIO(mp);
    if(!iob) {
        DeletePort(mp);
        exit(99);
    }

    strcpy(devnamebuf,"scsi-disk.device");
    unit = 1;
    flags = 0;
    getstr("Device driver name",devnamebuf);
    getdec("Unit number",&unit);
    getdec("Open flags",&flags);

    cmd.scsi_Length = BUFSIZE;
    cmd.scsi_CmdLength = 6;
    cmd.scsi_Flags = 1;

    if(OpenDevice(devnamebuf,unit,iob,flags)) {
        printf("Unable to open device driver.\n");
        DeleteStdIO(iob);
        DeletePort(mp);
        exit(99);
    }

    while(keepgoing) {
        printf("\n[V]iew [E]nter [F]ill [C]ommand e[X]ecute [P]recooked [Q]uit ? ");
        gets(inbuf);
        switch(tolower(*inbuf)) {
            case 'v':
                printf("Length: %ld  Actual: %ld  CmdLength: %d  CmdActual: %d\n",
                    cmd.scsi_Length,cmd.scsi_Actual,
                    cmd.scsi_CmdLength,cmd.scsi_CmdActual);
                printf("Flags: %d  Status: %d  io_Error: %d  Cmdbuf:",
                    cmd.scsi_Flags,cmd.scsi_Status,iob->io_Error);
                for(i=0;i<10;i++) printf(" %02x",cmdbuf[i]);
                putchar('\n');
                offset = -1;
                for(;;) {
                    if(offset<0) {
                        printf("Buffer address [Quit] $");
                        gets(inbuf);
                        if(tolower(*inbuf) != 'q' && *inbuf) sscanf(inbuf,"%lx",&offset);
                    } else {
                        printf("Enter address, [Q]uit or CR for next page? ");
                        gets(inbuf);
                        if(tolower(*inbuf) == 'q') offset = -1;
                        else if(*inbuf) sscanf(inbuf,"%lx",&offset);
                    }
                    if(offset<0 || offset>BUFSIZE) break;
                    for(i=0;i<16;i++) {
                        if(offset>=BUFSIZE) break;
                        printf("%04lx: ",offset);
                        for(j=0;j<16;j++) printf("%02x ",buffer[offset+j]);
                        putchar(' ');
                        for(j=0;j<16;j++) {
                            c=buffer[offset+j];
                            if(c>=32 && c<127) putchar(c); else putchar('.');
                        }
                        putchar('\n');
                        offset += 16;
                    }
                }
                break;
            case 'e':
                offset = 0;
                gethex("Starting buffer location",&offset);
                printf("Enter '.' to quit.\n");
                while(offset>=0 && offset<BUFSIZE) {
                    printf("Byte $%04lx [$%02d] $",offset,buffer[offset]);
                    gets(inbuf);
                    if(*inbuf == '.') break;
                    if(*inbuf) {
                        sscanf(inbuf,"%lx",&li);
                        buffer[offset] = li;
                    }
                    ++offset;
                }
                break;
            case 'f':
                li = 0xff;
                gethex("Value to fill with",&li);
                for(offset=0;offset<BUFSIZE;offset++) buffer[offset] = li;
                break;
            case 'c':
                do {
                    getdec("scsi_Length",&cmd.scsi_Length);
                } while(cmd.scsi_Length>BUFSIZE);
                li = cmd.scsi_CmdLength;
                getdec("scsi_CmdLength",&li);
                cmd.scsi_CmdLength = li;
                li = cmd.scsi_Flags;
                getdec("scsi_Flags",&li);
                cmd.scsi_Flags = li;
                for(i=0;i<cmd.scsi_CmdLength;i++) {
                    li = cmdbuf[i];
                    printf("Command byte %d",i);
                    gethex(" ",&li);
                    cmdbuf[i] = li;
                }
                break;
            case 'q':
                keepgoing = 0;
                break;
            case 'x':
                iob->io_Length = sizeof(struct SCSICmd);
                iob->io_Data = (APTR) &cmd;
                iob->io_Command = 28;
                cmd.scsi_Data = (UWORD *) buffer;
                cmd.scsi_Command = (UBYTE *) cmdbuf;
                DoIO(iob);
                printf("Error number: %d  Status byte: %d.\n",
                    iob->io_Error,cmd.scsi_Status);
                break;
            case 'p':
                printf("[R]equest Sense [F]ormat [I]nquiry [C]apacity [S]tart/stop ? ");
                gets(inbuf);
                switch(tolower(*inbuf)) {
                    case 'r':
                        cmdbuf[0] = 3;
                        cmdbuf[1] = cmdbuf[2] = cmdbuf[3] = cmdbuf[5] = 0;
                        cmdbuf[4] = 255;
                        cmd.scsi_CmdLength = 6;
                        cmd.scsi_Length = 255;
                        cmd.scsi_Flags = 1;
                        break;
                    case 'f':
                        cmdbuf[0] = 4;
                        cmdbuf[1] = cmdbuf[2] = cmdbuf[3] = cmdbuf[5] = 0;
                        li = 1;
                        getdec("Interleave factor",&li);
                        cmdbuf[4] = li;
                        cmd.scsi_CmdLength = 6;
                        cmd.scsi_Length = 0;
                        printf("*** WARNING *** This command will wipe out all data on unit %ld.\n",unit);
                        break;
                    case 'i':
                        cmdbuf[0] = 0x12;
                        cmdbuf[1] = cmdbuf[2] = cmdbuf[3] = cmdbuf[5] = 0;
                        cmdbuf[4] = 255;
                        cmd.scsi_CmdLength = 6;
                        cmd.scsi_Length = 255;
                        cmd.scsi_Flags = 1;
                        break;
                    case 's':
                        cmdbuf[0] = 0x1b;
                        cmdbuf[1] = cmdbuf[2] = cmdbuf[3] = cmdbuf[5] = 0;
                        li = 0;
                        getdec("Start (1) or Stop (0)",&li);
                        cmdbuf[4] = li;
                        cmd.scsi_CmdLength = 6;
                        cmd.scsi_Length = 0;
                        break;
                    case 'c':
                        cmdbuf[0] = 0x25;
                        cmdbuf[1] = cmdbuf[2] = cmdbuf[3] = cmdbuf[4] = 0;
                        cmdbuf[5] = cmdbuf[6] = cmdbuf[7] = cmdbuf[8] = 0;
                        cmdbuf[9] = 0;
                        cmd.scsi_CmdLength = 10;
                        cmd.scsi_Length = 8;
                        cmd.scsi_Flags = 1;
                        break;
                    default:
                        printf("Unknown command type.\n");
                }
                break;
            default:
                printf("Invalid command.\n");
        }
    }

    CloseDevice(iob);
    DeleteStdIO(iob);
    DeletePort(mp);
}

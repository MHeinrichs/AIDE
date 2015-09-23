#include <exec/ports.h>
#include <exec/io.h>
#include <devices/scsidisk.h>
#include <exec/memory.h>
#include <devices/trackdisk.h>
#include <stdio.h>

#define FILE_SIG 0x4449534b
#define NUMTRKS 10
#define TD_BUFSIZE (11L * 1024L * NUMTRKS)
#define NUMBLOCKS (NUMTRKS * 22)

typedef unsigned long ulong;
typedef unsigned short ushort;
typedef unsigned char uchar;

struct fhb {
    ulong sig1;
    ulong sig2;
    uchar name[504];
};

struct fhb header;

struct MsgPort *mp;
struct IOStdReq *iob;

struct MsgPort *td_mp;
struct IOStdReq *td_iob;

char inbuf[255];
uchar sensebuf[255];

uchar sense_cmd[] = { 3,0,0,0,255,0 };

extern struct MsgPort *CreatePort();
extern struct IOStdReq *CreateStdIO();
extern long OpenDevice();
extern void *AllocMem();

extern int Enable_Abort;

ulong lcmdbuf[3];
ushort *icmdbuf = (ushort *)&lcmdbuf;
uchar *cmdbuf = (uchar *)&lcmdbuf;

struct SCSICmd cmd;

uchar *diskbuf;

int rifm;

main()
{
    int i,j;
    int keepgoing = 1;
    FILE *fd;

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

    mp = CreatePort(0L,0L);
    if(!mp) exit(99);
    iob = CreateStdIO(mp);
    if(!iob) {
        DeletePort(mp);
        CloseDevice(td_iob);
        DeleteStdIO(td_iob);
        DeletePort(td_mp);
        exit(99);
    }
    if(OpenDevice("scsi-disk.device",4L,iob,0L)) {
        printf("Unable to open SCSI device driver.\n");
        DeleteStdIO(iob);
        DeletePort(mp);
        CloseDevice(td_iob);
        DeleteStdIO(td_iob);
        DeletePort(td_mp);
        exit(99);
    }
    while(keepgoing) {
        do {
            printf("R[e]wind [W]rite [R]ead [I]ndex [S]kip [Q]uit ? ");
            gets(inbuf);
        } while(!*inbuf);
        switch(tolower(*inbuf)) {
            case 'q':
                keepgoing = 0;
                break;
            case 'r':
                lcmdbuf[0] = 0x08010000;
                icmdbuf[2] = 0x0100;
                if(doscsi(&header,512L,6,1)) {
                    printf("Unable to read header block.\n");
                    goto g_abort;
                }
                if(header.sig1 != FILE_SIG || header.sig2 != ~FILE_SIG) {
                    printf("Not at a header block.\n");
                    goto g_abort;
                }
                printf("Header: '%s'\n",&header.name);
                printf("[E]xtract or [S]kip ? ");
                gets(inbuf);
                if(tolower(*inbuf) == 'e') readfile();
                lcmdbuf[0] = 0x11010000;
                icmdbuf[2] = 0x0100;
                if(doscsi(0L,0L,6,1)) 
                    printf("Skip filemark command failed.\n");
g_abort:
                break;
            case 'w':
                printf("File name? ");
                gets(inbuf);
                header.sig1 = FILE_SIG;
                header.sig2 = ~FILE_SIG;
                strcpy(&header.name,inbuf);
                lcmdbuf[0] = 0x0a010000;
                icmdbuf[2] = 0x0100;
                if(doscsi(&header,512L,6,0)) {
                    printf("Error writing file header.\n");
                    goto w_abort;
                }
                writefile();
                lcmdbuf[0] = 0x10000000;
                icmdbuf[2] = 0x0100;
                if(doscsi(0L,0L,6,1)) {
                    printf("Error writing file mark.\n");
                }
w_abort:
                break;
            case 'e':
                lcmdbuf[0] = 0x01000000;
                icmdbuf[2] = 0;
                doscsi(0L,0L,6,1);
                break;
            case 'i':
                printf("How many (0 for all remaining)? ");
                gets(inbuf);
                j = atoi(inbuf);
                printf("Output file (blank for none)? ");
                gets(inbuf);
                fd = 0;
                if(*inbuf) {
                    fd = fopen(inbuf,"w");
                    if(!fd) {
                        printf("Error opening output file.\n");
                        goto i_abort;
                    }
                }
                i = 1;
                do {
                    lcmdbuf[0] = 0x08010000;
                    icmdbuf[2] = 0x0100;
                    if(doscsi(&header,512L,6,1)) {
                        printf("Unable to read header block.\n");
                        goto i_abort;
                    }
                    if(header.sig1 != FILE_SIG || header.sig2 != ~FILE_SIG) {
                        printf("Invalid header block.\n");
                        goto i_abort;
                    }
                    lcmdbuf[0] = 0x13010006;
                    icmdbuf[2] = 0xe000;
                    if(doscsi(0L,0L,6,1)) {
                        printf("*BAD* %s\n",&header.name);
                        if(fd) fprintf(fd,"*BAD* %s\n",&header.name);
                    } else {
                        printf("%4d: %s\n",i,&header.name);
                        if(fd) fprintf(fd,"%4d: %s\n",i,&header.name);
                    }
                    i++;
                    if(!rifm) {
                        lcmdbuf[0] = 0x11010000;
                        icmdbuf[2] = 0x0100;
                        if(doscsi(0L,0L,6,1)) {
                            printf("Skip filemark command failed.\n");
                            goto i_abort;
                        }
                    }
                } while(--j);
i_abort:
                if(fd) fclose(fd);
                break;
            case 's':
                printf("Skip how many? ");
                gets(inbuf);
                i = atoi(inbuf);
                if(i<1 || i>255) printf("Out of range.\n");
                else {
                    lcmdbuf[0] = 0x11010000;
                    cmdbuf[4] = i;
                    cmdbuf[5] = 0;
                    if(doscsi(0L,0L,6,1))
                        printf("Skip filemark(s) command failed.\n");
                }
                break;
            default:
                printf("Unknown command.\n");
        }
    }
    FreeMem(diskbuf,TD_BUFSIZE);
    CloseDevice(iob);
    DeleteStdIO(iob);
    DeletePort(mp);
    CloseDevice(td_iob);
    DeleteStdIO(td_iob);
    DeletePort(td_mp);
}

int doscsi(buf,length,cmdlength,flags)
ushort *buf;
ulong length;
int cmdlength,flags;
{
    int i;
    int j;
    int errflag = 1;
    int keepgoing = 1;
    ulong ln,ac;
    rifm = 0;
    while(keepgoing) {
        cmd.scsi_Data = buf;
        cmd.scsi_Length = length;
        cmd.scsi_CmdLength = cmdlength;
        cmd.scsi_Flags = flags;
        cmd.scsi_Command = cmdbuf;
        iob->io_Command = 28;
        iob->io_Data = (APTR) &cmd;
        iob->io_Length = sizeof(cmd);
        DoIO(iob);
        if(iob->io_Error) {
            printf("SCSI Error.  Cmd = ");
            for(i=0;i<cmdlength;i++) printf("%02x ",cmdbuf[i]);
            printf(" io_Error = %d, scsi_Status = %d.\n",iob->io_Error,cmd.scsi_Status);
            printf("scsi_Length = 0x%lx, scsi_Actual = 0x%lx.\n",ln=cmd.scsi_Length,ac=cmd.scsi_Actual);
            if(iob->io_Error == 45) {
                cmd.scsi_Command = sense_cmd;
                cmd.scsi_Flags = 1;
                cmd.scsi_CmdLength = 6;
                cmd.scsi_Length = 255;
                cmd.scsi_Data = (ushort *) sensebuf;
                DoIO(iob);
                if(iob->io_Error) {
                    printf("Request sense failed, io_Error = %d.\n",iob->io_Error);
                } else {
                    printf("Sense information: ");
                    for(i=0;i<cmd.scsi_Actual;i++) printf("%02x ",sensebuf[i]);
                    putchar('\n');
                    if(cmd.scsi_Actual > 4) {
                        if((sensebuf[2] == 1) && (ac == ln)) {
                            printf("*** Recovered error ignored ***\n");
                            keepgoing = 0;
                            errflag = 0;
                        } else if(sensebuf[2] == 0x80) {
                            printf("*** Ran into file mark ***\n");
                            rifm = 1;
                            keepgoing = 0;
                        }
                    }
                }
            }
            if(keepgoing) {
                printf("[R]etry [I]gnore [F]ail? ");
                gets(inbuf);
                if(tolower(*inbuf) == 'i') {
                    keepgoing = 0;
                    errflag = 0;
                } else if(tolower(*inbuf) != 'r') keepgoing = 0;
            }
        } else {
            errflag = 0;
            keepgoing = 0;
        }
    }
    return errflag;
}

writefile()
{
    long i;
    for(i=0;i<80;i+=NUMTRKS) {
        printf("Reading tracks %ld to %ld.\n",i,i+NUMTRKS-1);
        td_iob->io_Command = CMD_READ;
        td_iob->io_Data = (APTR) diskbuf;
        td_iob->io_Length = TD_BUFSIZE;
        td_iob->io_Offset = 11L * 1024L * i;
        DoIO(td_iob);
        if(td_iob->io_Error) {
            printf("Floppy read failed, error number %d.\n",td_iob->io_Error);
            return;
        }
        printf("Writing to tape...\n");
        icmdbuf[0] = 0x0a01;
        icmdbuf[1] = NUMBLOCKS >> 8;
        cmdbuf[4] = NUMBLOCKS & 0xff;
        cmdbuf[5] = 0;
        if(doscsi(diskbuf,TD_BUFSIZE,6,0)) {
            printf("Tape write failed.\n");
            return;
        }
    }
    td_iob->io_Command = TD_MOTOR;
    td_iob->io_Length = 0;
    DoIO(td_iob);
}

readfile()
{
    long i;
    printf("Disk in DF0: will be overwritten -- are you sure? ");
    gets(inbuf);
    if(tolower(*inbuf) != 'y') return;
    for(i=0;i<80;i+=NUMTRKS) {
        printf("Reading from tape...\n");
        icmdbuf[0] = 0x0801;
        icmdbuf[1] = NUMBLOCKS >> 8;
        cmdbuf[4] = NUMBLOCKS & 0xff;
        cmdbuf[5] = 0;
        if(doscsi(diskbuf,TD_BUFSIZE,6,1)) {
            printf("Tape read failed.\n");
            goto bomb;
        }
        printf("Writing tracks %ld to %ld.\n",i,i+NUMTRKS-1);
        td_iob->io_Command = TD_FORMAT;
        td_iob->io_Data = (APTR) diskbuf;
        td_iob->io_Length = TD_BUFSIZE;
        td_iob->io_Offset = 11L * 1024L * i;
        DoIO(td_iob);
        if(td_iob->io_Error) {
            if(td_iob->io_Error == TDERR_WriteProt)
                printf("Floppy is write protected.\n");
            else
                printf("Floppy write failed, error number %d.\n",td_iob->io_Error);
            goto bomb;
        }
    }
    td_iob->io_Command = CMD_UPDATE;
    DoIO(td_iob);
    if(td_iob->io_Error) {
        if(td_iob->io_Error == TDERR_WriteProt)
            printf("Floppy is write protected.\n");
        else
            printf("Disk update failed.\n");
    }
bomb:
    td_iob->io_Command = TD_MOTOR;
    td_iob->io_Length = 0;
    DoIO(td_iob);
}

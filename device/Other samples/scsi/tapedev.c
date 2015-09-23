#include <exec/ports.h>
#include <exec/io.h>
#include <devices/scsidisk.h>
#include <libraries/dos.h>
#include <libraries/dosextens.h>
#include <libraries/filehandler.h>

#define CLOSED 0
#define JUST_OPENED 1
#define OPEN_FOR_READ 2
#define OPEN_FOR_WRITE 3

#define SCSI_OK 0
#define SCSI_ERROR 1
#define SCSI_UNIT_ATTENTION 2
#define SCSI_FILEMARK 3

#define UNKNOWN 0
#define REWOUND 1
#define IN_FILE 2
#define AT_FILEMARK 3
#define AT_END 4

#define ACT_FINDINPUT 1005
#define ACT_FINDOUTPUT 1006
#define ACT_END 1007

extern void *FindTask();
extern struct DosPacket *taskwait();
extern struct MsgPort *CreatePort();
extern struct IOStdReq *CreateStdIO();
extern long OpenDevice();
extern void *OpenLibrary();
extern int AutoAutoRequest();
extern void *malloc();

unsigned long buffer_size;
unsigned long buffer_cutoff;
long unitnum;
long flags;
char *devname;
int devlength;

struct MsgPort *mp;
struct IOStdReq *iob;
struct Process *myproc;
int tapestate;
int errnum;
ULONG buffer_highmark;
ULONG buffer_current;
int read_eof;
struct SCSICmd cmd;
ULONG actual;
void *IntuitionBase;
int notified;

UBYTE sense_cmd[] = { 3,0,0,0,20,0 };
UBYTE rewind_cmd[] = { 1,0,0,0,0,0 };
UBYTE test_ready_cmd[] = { 0,0,0,0,0,0 };
UBYTE skip_filemark_cmd[] = { 0x11,1,0,0,1,0 };
UBYTE skip_end_cmd[] = {0x11,3,0,0,1,0 };
UBYTE write_filemark_cmd[] = {0x10,0,0,0,1,0};
UBYTE mode_sense_cmd[] = {0x1a,0,0,0,3,0};

UBYTE *buffer;
UBYTE sensebuf[20];

_main()
{
    register struct DosPacket *pkt;
    struct DeviceNode *devnode;
    register int openstate;
    UBYTE *ptr;
    int i;
    long error;

    myproc = (struct Process *)FindTask(0L);
    pkt = taskwait(myproc);
    devnode = (struct DeviceNode *) ((pkt->dp_Arg3)<<2);

    if(parsestartup((pkt->dp_Arg2)<<2) || buffer_size < 10) {
        returnpkt(pkt,myproc,DOSFALSE,ERROR_NO_FREE_STORE);
        return 0;
    }

    buffer_cutoff = buffer_size << 8;
    buffer_size <<= 9;

    buffer = (UBYTE *)malloc(buffer_size);
    if(!buffer) {
        returnpkt(pkt,myproc,DOSFALSE,ERROR_NO_FREE_STORE);
        return 0;
    }

    IntuitionBase = OpenLibrary("intuition.library",0L);

    if(!IntuitionBase) {
        returnpkt(pkt,myproc,DOSFALSE,ERROR_NO_FREE_STORE);
        free(buffer);
        return 0;
    }

    mp = CreatePort(0L,0L);
    if(!mp) {
        returnpkt(pkt,myproc,DOSFALSE,ERROR_NO_FREE_STORE);
        CloseLibrary(IntuitionBase);
        free(buffer);
        return 0;
    }
    iob = CreateStdIO(mp);
    if(!iob) {
        CloseLibrary(IntuitionBase);
        free(buffer);
        DeletePort(mp);
        returnpkt(pkt,myproc,DOSFALSE,ERROR_NO_FREE_STORE);
        return 0;
    }

    devname[devlength] = 0;
    error = OpenDevice(devname,unitnum,iob,flags);
    devname[devlength] = '/';

    if(error) {
        CloseLibrary(IntuitionBase);
        free(buffer);
        DeleteStdIO(iob);
        DeletePort(mp);
        returnpkt(pkt,myproc,DOSFALSE,ERROR_OBJECT_NOT_FOUND);
        return 0;
    }

    devnode->dn_Task = &myproc->pr_MsgPort;
    returnpkt(pkt,myproc,DOSTRUE,0L);

    openstate = CLOSED;
    tapestate = UNKNOWN;

    for(;;) {
        pkt = taskwait(myproc);
        switch(pkt->dp_Type) {
            case ACT_FINDINPUT:
            case ACT_FINDOUTPUT:
                if(openstate != CLOSED) returnpkt(pkt,myproc,DOSFALSE,
                                       ERROR_OBJECT_IN_USE);
                else if(not_ready()) {
                    returnpkt(pkt,myproc,DOSFALSE,ERROR_OBJECT_NOT_FOUND);
                } else {
                    errnum = 0;
                    notified = 0;
                    openstate = JUST_OPENED;
                    ptr = (UBYTE *) ((pkt->dp_Arg3)<<2);
                    for(i=*ptr++;i>1;i--) {
                        if(*ptr++ == ':') {
                            if(*ptr == 'R' || *ptr == 'r')
                                rewind();
                            else if(*ptr == 'A' || *ptr == 'a')
                                append();
                            i=0;
                        }
                    }
                    if(tapestate == UNKNOWN) rewind();
                    returnpkt(pkt,myproc,DOSTRUE,0L);
                }
                break;
            case ACTION_READ:
                switch(openstate) {
                    case JUST_OPENED:
                        init_read();
                        openstate = OPEN_FOR_READ;
                    case OPEN_FOR_READ:
                        tape_read(pkt);
                        break;
                    default:
                        returnpkt(pkt,myproc,-1L,ERROR_READ_PROTECTED);
                }
                break;
            case ACTION_WRITE:
                switch(openstate) {
                    case JUST_OPENED:
                        init_write();
                        openstate = OPEN_FOR_WRITE;
                    case OPEN_FOR_WRITE:
                        tape_write(pkt);
                        break;
                    default:
                        returnpkt(pkt,myproc,-1L,ERROR_OBJECT_NOT_FOUND);
                }
                break;
            case ACT_END:
                if(openstate == OPEN_FOR_WRITE) finish_write();
                openstate = CLOSED;
                returnpkt(pkt,myproc,DOSTRUE,0L);
                break;
            default:
                returnpkt(pkt,myproc,DOSFALSE,(long)ERROR_ACTION_NOT_KNOWN);
        }
    }
}

/*
 *  This function is called for every open and tests if a tape is ready.
 *
 *  tapestate becomes UNKNOWN if no tape is ready.
 *
 */
not_ready()
{
    do {
        switch(doscsi(0L,test_ready_cmd,0L,6,1)) {
            case SCSI_OK:
                return 0;
            case SCSI_UNIT_ATTENTION:
                if(doscsi(0L,test_ready_cmd,0L,6,1)==SCSI_OK) return 0;
            default:
                tapestate = UNKNOWN;
        }
    } while(AutoAutoRequest("Tape unit not ready.",0L,0L,"Retry","Cancel"));
    return -1;
}

rewind()
{
    if(doscsi(0L,rewind_cmd,0L,6,1))
        errnum = ERROR_OBJECT_NOT_FOUND;
    else tapestate = REWOUND;
}

append()
{
    if(doscsi(0L,skip_end_cmd,0L,6,1))
        errnum = ERROR_OBJECT_NOT_FOUND;
    else tapestate = AT_END;
}

init_read()
{
    int i;
    buffer_current = 0;
    buffer_highmark = 0;
    read_eof = 0;
    if(!errnum) switch(tapestate) {
        case IN_FILE:
            if(doscsi(0L,skip_filemark_cmd,0L,6,1)) {
                tapestate = UNKNOWN;
                errnum = ERROR_OBJECT_NOT_FOUND;
            }
            tapestate = AT_FILEMARK;
        case AT_FILEMARK:
        case REWOUND:
            break;
        default:
            errnum = ERROR_OBJECT_NOT_FOUND;
    } 
}

init_write()
{
    UBYTE msbuf[3];
    buffer_current = 0;
    if(!errnum) switch(tapestate) {
        case AT_FILEMARK:
        case IN_FILE:
            if(doscsi(0L,skip_end_cmd,0L,6,1)) {
                errnum = ERROR_OBJECT_NOT_FOUND;
                tapestate = UNKNOWN;
            }
        case AT_END:
        case REWOUND:
            if(doscsi(msbuf,mode_sense_cmd,3L,6,1)) {
                errnum = ERROR_OBJECT_NOT_FOUND;
                return;
            }
            if(msbuf[2] & 0x80) {
                errnum = ERROR_OBJECT_NOT_FOUND;
                AutoAutoRequest("Tape is write protected.",0L,0L,0L,"Abort");
                notified = 1;
                return;
            }
            break;
        default:
            errnum = ERROR_OBJECT_NOT_FOUND;
    }
}

char readerr1[] = "An error has occurred while";
char readerr2[] = "trying to read from the tape.";

tape_read(pkt)
register struct DosPacket *pkt;
{
    register UBYTE *ptr = (UBYTE *)pkt->dp_Arg2;
    register ULONG length = pkt->dp_Arg3;
    register ULONG fragment;
    UBYTE cmdbuf[6];

    if(errnum) {
        if(!notified) {
            AutoAutoRequest(readerr1,readerr2,0L,0L," OK ");
            notified = 1;
        }
        returnpkt(pkt,myproc,-1L,(long)errnum);
        return;
    }
    /*
     *  First, transfer whatever is left in the buffer.
     */
    if(buffer_current < buffer_highmark ) {
        fragment = buffer_highmark - buffer_current;
        if(length<fragment) fragment = length;
        CopyMem(buffer+buffer_current,ptr,fragment);
        length -= fragment;
        buffer_current += fragment;
        ptr += fragment;
    }
    if(!read_eof) {
        if(length>buffer_cutoff) {
            fragment = length & ~511;
            cmdbuf[0] = 8;
            cmdbuf[1] = 1;
            *((ULONG *)&cmdbuf[2]) = fragment>>1;
            switch(doscsi(ptr,cmdbuf,fragment,6,1)) {
                case SCSI_FILEMARK:
                    fragment = actual;
                    read_eof = 1;
                    break;
                case SCSI_OK:
                    tapestate = IN_FILE;
                    break;
                default:
                    read_eof = 1;
                    fragment = 0;
                    AutoAutoRequest(readerr1,readerr2,0L,0L," OK ");
                    notified = 1;
                    break;
            }
            ptr += fragment;
            length -= fragment;
        }
        if(length && !read_eof) {
            cmdbuf[0] = 8;
            cmdbuf[1] = 1;
            *((ULONG *)&cmdbuf[2]) = buffer_size>>1;
            switch(doscsi(buffer,cmdbuf,(long)buffer_size,6,1)) {
                case SCSI_FILEMARK:
                    read_eof = 1;
                    buffer_highmark = actual;
                    break;
                case SCSI_OK:
                    buffer_highmark = actual;
                    tapestate = IN_FILE;
                    break;
                default:
                    read_eof = 1;
                    buffer_highmark = 0;
                    AutoAutoRequest(readerr1,readerr2,0L,0L," OK ");
                    notified = 1;
                    errnum = ERROR_SEEK_ERROR;
            }
            buffer_current = length;
            if(buffer_highmark < buffer_current) buffer_current = buffer_highmark;
            CopyMem(buffer,ptr,buffer_current);
            length -= buffer_current;
        }
    }
    if(errnum) if(pkt->dp_Arg3==length) {
        returnpkt(pkt,myproc,-1L,ERROR_SEEK_ERROR);
        return;
    }
    returnpkt(pkt,myproc,pkt->dp_Arg3-length,0L);
}

char writerr1[] = "An error has occurred while";
char writerr2[] = "trying to write to the tape.";
char writerr3[] = "Tape data is probably invalid.";

tape_write(pkt)
register struct DosPacket *pkt;
{
    register UBYTE *ptr = (UBYTE *)pkt->dp_Arg2;
    register ULONG length = pkt->dp_Arg3;
    register ULONG fragment;
    UBYTE cmdbuf[6];

    if(errnum) {
        if(!notified) {
            AutoAutoRequest(writerr1,writerr2,writerr3,0L," OK ");
            notified = 1;
        }
        returnpkt(pkt,myproc,-1L,(long)errnum);
        return;
    }
    if(buffer_current || (length <= buffer_cutoff)) {
        fragment = buffer_size - buffer_current;
        if(length < fragment) fragment = length;
        CopyMem(ptr,buffer+buffer_current,fragment);
        ptr += fragment;
        buffer_current += fragment;
        length -= fragment;
        /*
         *  If the buffer is full, flush it.
         *
         */
        if(buffer_current == buffer_size) {
            cmdbuf[0] = 10;
            cmdbuf[1] = 1;
            *((ULONG *)&cmdbuf[2]) = buffer_size>>1;
            if(doscsi(buffer,cmdbuf,(long)buffer_size,6,0)) {
                AutoAutoRequest(writerr1,writerr2,writerr3,0L," OK ");
                notified = 1;
                errnum = ERROR_SEEK_ERROR;
            }
            buffer_current = 0;
        }
    }
    if(!errnum) {
        if(length>buffer_cutoff) {
            fragment = length & ~511;
            cmdbuf[0] = 10;
            cmdbuf[1] = 1;
            *((ULONG *)&cmdbuf[2]) = fragment>>1;
            if(doscsi(ptr,cmdbuf,fragment,6,0)) {
                AutoAutoRequest(writerr1,writerr2,writerr3,0L," OK ");
                notified = 1;
                errnum = ERROR_SEEK_ERROR;
            }
            length -= fragment;
            ptr += fragment;
        }
        if(length && !errnum) {
            CopyMem(ptr,buffer,length);
            buffer_current = length;
            length = 0;
        }
    }
    if(errnum)
        returnpkt(pkt,myproc,-1L,ERROR_SEEK_ERROR);
    else
        returnpkt(pkt,myproc,pkt->dp_Arg3,0L);
}

finish_write()
{
    UBYTE cmdbuf[6];
    ULONG fragment;

    if(buffer_current && !errnum) {
        fragment = (buffer_current + 511) & ~511;
        while(buffer_current < fragment) buffer[buffer_current++] = 0;
        cmdbuf[0] = 10;
        cmdbuf[1] = 1;
        *((ULONG *)&cmdbuf[2]) = fragment>>1;
        if(doscsi(buffer,cmdbuf,fragment,6,0))
            AutoAutoRequest(writerr1,writerr2,writerr3,0L," OK ");
            notified = 1;
    }
    if(doscsi(0L,write_filemark_cmd,0L,6,1)) {
        if(!notified) AutoAutoRequest(writerr1,writerr2,writerr3,0L," OK ");
    } else
        tapestate = AT_END;
}


int doscsi(buf,cmdbuf,length,cmdlength,flags)
UWORD *buf;
UBYTE *cmdbuf;
ULONG length;
int cmdlength,flags;
{
    cmd.scsi_Data = buf;
    cmd.scsi_Length = length;
    cmd.scsi_CmdLength = cmdlength;
    cmd.scsi_Flags = flags;
    cmd.scsi_Command = cmdbuf;
    iob->io_Command = 28;
    iob->io_Data = (APTR) &cmd;
    iob->io_Length = sizeof(cmd);
    DoIO(iob);
    actual = cmd.scsi_Actual;
    switch(iob->io_Error) {
        case 0:
            if(length == actual) return SCSI_OK;
            else return SCSI_ERROR;
        case HFERR_BadStatus:
            cmd.scsi_Command = sense_cmd;
            cmd.scsi_Flags = 1;
            cmd.scsi_CmdLength = 6;
            cmd.scsi_Length = 20;
            cmd.scsi_Data = (UWORD *) sensebuf;
            DoIO(iob);
            if(iob->io_Error) return SCSI_ERROR;
            if(cmd.scsi_Actual > 4) {
                if(sensebuf[2] == 1 && actual == length) {
                    return SCSI_OK;
                }
                if(sensebuf[2] == 6) {
                    tapestate = UNKNOWN;
                    return SCSI_UNIT_ATTENTION;
                }
                if(sensebuf[2] == 8) {
                    tapestate = AT_END;
                    return SCSI_ERROR;
                }
                if(sensebuf[2] == 0x80) {
                    tapestate = AT_FILEMARK;
                    return SCSI_FILEMARK;
                }
            }
        default:
            tapestate = UNKNOWN;
            return SCSI_ERROR;
    }
}

int parsestartup(msg)
register char *msg;
{
    register int i,state = 0;

    buffer_size = 0;
    unitnum = 0;
    flags = 0 ;

    if(!msg) return 1;

    for(i=1;i<=msg[0];i++) {
        if(msg[i] == '/') {
            switch(++state) {
                case 1: devname = &msg[i+1];
                        break;
                case 2: devlength = (&msg[i]) - devname;
                        break;
                case 4:
                        return 1;
            }
        } else if(state != 1) {
            if(msg[i] < '0' || msg[i] > '9') return 1;
            switch(state) {
                case 0: buffer_size = 10 * buffer_size + msg[i] - '0';
                        break;
                case 2: unitnum = 10 * unitnum + msg[i] - '0';
                        break;
                case 3: flags = 10 * flags + msg[i] - '0';
            }
        }
    }
    return (state<3);
}

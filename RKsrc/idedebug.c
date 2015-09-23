#include <stdio.h>
#include <string.h>
#include <pragma/exec_lib.h>
#include <clib/alib_protos.h>
#include <devices/trackdisk.h>
#include <devices/scsidisk.h>
#include <dos/dos.h>
#include "mydbg.h"

struct MsgPort *port,*dbport;
struct IOStdReq *req;
unsigned short *adr;
FILE *fd=0;
struct MyDBMsg *msg,*oldmsg;

void main(int argc,char *argv[]) {
	int loop = 1;
	int i,oldcnt=-1,length;
	unsigned long mask1,mask2,resmask; 

	port = (struct MsgPort *)CreatePort(0,0);
	if (!port) exit(20);

	req = (struct IOStdReq *)CreateExtIO (port, sizeof(struct IOExtTD));
	if (!req) exit(21);

	if (OpenDevice ("ide.device",1,(struct IORequest *)req,0)) exit(22);

	dbport = CreatePort(0,0);
	if (!dbport) exit(23);

	req->io_Error = 0;
	req->io_Length = sizeof(struct MsgPort *);
	req->io_Data = (APTR)dbport;
	req->io_Flags = IOF_QUICK;
	req->io_Actual = 2;						// start debug
	req->io_Command = CMD_MYCMD;
	DoIO ((struct IORequest *)req);
	if (!req->io_Error) {
		mask1 = 1 << dbport->mp_SigBit;
		mask2 = SIGBREAKF_CTRL_C;
		do {
			resmask = Wait(mask1 | mask2);
			if (resmask & mask1) {
				while ((msg = (struct MyDBMsg *)GetMsg(dbport)) != 0) {
					if (oldmsg == msg) break;
					printf("%04d. ",msg->mdb_counter);
					switch (msg->mdb_type) {
						case DB_PACKET:		printf("Packet       : "); break;
						case DB_EMULPACKET:	printf("Emul. packet : "); break;
						case DB_COMMAND:		printf("Command      : "); break;
						case DB_OPENDEV:		printf("OpenDev unit : "); break;
						default:					printf("Unknown type : "); break;
					}
					length = msg->mdb_length;
					if (length<0 || length>16) length=16;
					for (i=0;i<length;i++) printf("%02X",msg->mdb_data[i]);
					printf(" (%d)\n",msg->mdb_length);
					oldmsg = msg;
					FreeMem(msg,sizeof(struct MyDBMsg));
				}
			}
			if (resmask & mask2) loop=0;
		} while(loop);
	}

	req->io_Length = sizeof(struct MsgPort *);
	req->io_Data = (APTR)dbport;
	req->io_Flags = IOF_QUICK;
	req->io_Actual	= 3;						// stop debug
	req->io_Command = CMD_MYCMD;
	DoIO ((struct IORequest *)req);

	oldmsg = NULL;
	while ((msg = (struct MyDBMsg *)GetMsg(dbport)) != 0) {
		if (oldmsg != msg) FreeMem(msg,sizeof(struct MyDBMsg));
		else break;
	}

	CloseDevice((struct IORequest *)req);
	DeleteExtIO((struct IORequest *)req);
	DeletePort(port);
	DeletePort(dbport);
	printf("IdeDebug removed\n");
	exit(0);
}


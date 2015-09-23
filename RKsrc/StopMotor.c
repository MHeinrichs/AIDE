#include <stdio.h>
#include <stdlib.h>
//#include <pragma/exec_lib.h>
#include <clib/alib_protos.h>
#include <devices/trackdisk.h>
#include "mydev.h"

struct MsgPort *port;
struct IOStdReq *req;

void main(int argc, char *argv[]) {
	int unit,poc=1,i;

	if (argc == 1) {poc=2; unit=0;}
	else if (sscanf(argv[1],"%d",&unit)!=1) {printf("StopMotor [unit]\n"); exit(20);}
	if (unit!=0 && unit!=1) {printf("Wrong unit number\n"); exit(21);}

	for (i=0;i<poc;i++) {	
		port = (struct MsgPort *)CreatePort(0,0);
		if (!port) exit(22);

		req = (struct IOStdReq *)CreateExtIO (port, sizeof(struct IOExtTD));
		if (!req) exit(23);

		if (OpenDevice ("ide.device",unit,(struct IORequest *)req,0)) exit(24);

		req->io_Actual = 1;				// mymotor subcommand
		req->io_Length = 0;				// turn motor off
		req->io_Command = CMD_MYCMD;	// mycmd

		DoIO ((struct IORequest *)req);

		CloseDevice((struct IORequest *)req);
		DeleteExtIO((struct IORequest *)req);
		DeletePort(port);
		
		unit++;
	}
	exit(0);
}


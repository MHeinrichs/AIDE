#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//#include <pragma/exec_lib.h>
#include <clib/alib_protos.h>
#include <devices/trackdisk.h>
#include <devices/scsidisk.h>

unsigned char buf[4096], sense[64];
unsigned short *adr,cmd[10];
//unsigned short cmd2[6] = {0x4302,0,0,0x0003,0x2400,0};	//Read TOC
//unsigned short cmd[6] = {0xa800,0,0x0010,0,0x0001,0};	//Read (12)
//unsigned short cmd[6] = {0x5a00,0x0e00,0,0,0xff00,0};	//Mode Sense
//unsigned short cmd[6] = {0x4200,0x4001,0,0x00ff,0xff00,0};	//Read sub-channel
//unsigned short cmd[6] = {0x1200,0,0xff00,0,0,0};			//Inquiry
//unsigned short cmd[6] = {0x4302,0,0,0x0004,0x0000,0};	//Read TOC-2
//unsigned short cmd[6] = {0xbd00,0,0,0,0x00ff,0};	//Mechanism status
//unsigned short cmd[6] = {0x0,0,0,0,0,0};	//Test Unit Ready
//unsigned short cmd[6] = {0x2b00,0,1200,0,0,0};	//Seek
//unsigned short cmd[6] = {0xba00,0,0,0,0,0};	//Scan
struct MsgPort *port;
struct IOStdReq	*scsireq;
struct SCSICmd scsicmd;
void packet();
FILE *fd;
long unit;
int cmdlen;
char dev_name[64];

void main(int argc, char *argv[]) {
	int i,chyba;
	char c1,c2;
	unsigned int pom;

	if (argc!=4 && argc!=5) {printf("scsicmd <device> <unit> <scsi packet (hex)> [out file]\n"); exit(20);}
	if (sscanf(argv[1],"%s",dev_name)!=1) {printf("Wrong device\n",dev_name); exit(21);}
	if (sscanf(argv[2],"%d",&unit)!=1) {printf("Wrong unit\n"); exit(22);}

	port = CreatePort(0,0);
	if (!port) exit(23);

	scsireq = (struct IOStdReq *)CreateExtIO (port, sizeof(struct IOExtTD));
	if (!scsireq) exit(24);

	if (OpenDevice (dev_name,unit,(struct IORequest *)scsireq,0))
		{printf("Cannot open device\n"); exit(25);}

	chyba=0;
	cmdlen=strlen(argv[3])/2;
	for(i=0;i<2*cmdlen;i+=4) {
		if (sscanf(&argv[3][i],"%04x",&pom)!=1) {printf("Wrong scsi packet\n"); chyba=1; break;}
		cmd[i/4] = pom;
	}

	if (!chyba) {

		packet(cmd,buf);

		if (scsicmd.scsi_Status) {
			printf("Error\nStatus = 0x%02X\n",scsicmd.scsi_Status);
			if (scsicmd.scsi_SenseActual != 0)
				printf("SK ASC ASCQ = 0x%02X 0x%02X 0x%02X\n",sense[2],sense[12],sense[13]);
		}
		else {
			if (argc == 4) {
				if (scsicmd.scsi_Actual > 20) {
					printf("%d bytes of data\nWriting only 20 bytes\n",scsicmd.scsi_Actual);
					scsicmd.scsi_Actual=20;
				}
				if (scsicmd.scsi_Actual==0) printf("No data returned\n");
				else {
					for (i=0;i<scsicmd.scsi_Actual/2;i++)
						printf("%03d. 0x%04X\n",2*i,scsicmd.scsi_Data[i]);
				}
			}
			else {
				if ((fd = fopen(argv[4],"w"))!=0) {
					adr = (unsigned short *)buf;
					for (i=0;i<scsicmd.scsi_Actual/2;i++) {
						c1 = 0xFF & adr[i];
						if (c1<' ' || c1>127) c1='.';
						c2 = 0xFF & (adr[i]>>8);
						if (c2<' ' || c2>127) c2='.';
						fprintf(fd,"%03d. 0x%04X  [%c%c]\n",2*i,adr[i],c2,c1);
					}
					fclose(fd);
					printf("Result in %s\n",argv[4]);
				} else printf("Cannot open %s\n",argv[4]);
			}
		}
	}
	CloseDevice((struct IORequest *)scsireq);
	DeleteExtIO((struct IORequest *)scsireq);
	DeletePort(port);
}

void packet(unsigned short *bbb, unsigned char *bf) {
	scsireq->io_Length   = sizeof(struct SCSICmd);
	scsireq->io_Data     = (APTR)&scsicmd;
	scsireq->io_Command  = HD_SCSICMD;

	scsicmd.scsi_Data        = (UWORD *)bf;
	scsicmd.scsi_Length      = 4096;
	scsicmd.scsi_Flags       = SCSIF_AUTOSENSE | SCSIF_READ;
	scsicmd.scsi_SenseData   = (UBYTE *)sense;
	scsicmd.scsi_SenseLength = 20;
	scsicmd.scsi_SenseActual = 0;
	scsicmd.scsi_Command     = (UBYTE *)bbb;
	scsicmd.scsi_CmdLength   = cmdlen;

	DoIO ((struct IORequest *)scsireq);
}


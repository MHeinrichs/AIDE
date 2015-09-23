#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mydev.h"
#include <pragma/dos_lib.h>
#include <pragma/exec_lib.h>
#include <clib/alib_protos.h>
#include <devices/trackdisk.h>
#include <devices/scsidisk.h>

#define FFWD 10*75
#define LBA(M,S,F) (75*((60*(M))+(S))+(F))

int packet(unsigned short *,unsigned short *,unsigned long);
void chyba();

unsigned char data[1024],data1[1024];
unsigned char track[99][6];
int unit = 1;
int act_track = -1;
int tracks_cnt  = 0;
int hram = 0;
int disk_in = -1;
int pause = 0;
char txt[64];
unsigned char sendat[256];
unsigned short sen[6] = {0x0300,0,0x1000,0,0,0};
unsigned short readtoc[6] = {0x4302,0,0,0x0004,0x0000,0};
unsigned short playcmd[6] = {0x4700,0,0,0,0,0};
unsigned short stopcmd[6] = {0x4e00,0,0,0,0,0};
unsigned short pausecmd[6] = {0x4b00,0,0,0,0,0};
unsigned short resumecmd[6] = {0x4b00,0,0,0,0x0100,0};
unsigned short sub_ch[6] = {0x4202,0x4001,0,0,0x1000,0};
unsigned short modesense[6] = {0x5a00,0x0e00,0,0,0xff00,0};
unsigned short scancmd[6] = {0xba00,0,0,0,0x0040,0};
char idstring[16+1];
struct MyUnit *mu;

struct MsgPort *port = 0;
struct IOStdReq	*scsireq = 0;
struct SCSICmd scsicmd;

void MSF(unsigned long lba,unsigned char *m,unsigned char *s,unsigned char *f) {
	*m = lba/(75*60);
	*s = (lba - (75*60*(*m)))/75;
	*f = lba - (75*60*(*m)) - (75*(*s));
}

int TestCD() {
	if (packet(modesense,(unsigned short *)data1,1024)>=3) {
		switch (data1[2]) {
			case 0x02:
			case 0x06:
			case 0x12:
			case 0x16:
			case 0x22:
			case 0x26:	disk_in=TRUE; return(TRUE);
			case 0x70:	printf("\tNo disk present");	disk_in=FALSE; return(FALSE);
			case 0x71:	printf("\tDoor open\n"); disk_in=FALSE; return(FALSE);
			default:	printf("\tBad media type\n"); disk_in=FALSE; return(FALSE);
		}
	}
	return(FALSE);
}

int initDev() {
	port = (struct MsgPort *)CreatePort (0,0);
	if (!port) return FALSE;

	scsireq = (struct IOStdReq *)CreateExtIO (port, sizeof(struct IOExtTD));
	if (!scsireq) return FALSE;

	if (OpenDevice ("ide.device",unit,(struct IORequest *)scsireq,0)) return FALSE;

	mu = (struct MyUnit *)scsireq->io_Unit;
	if (mu->mdu_drv_type != ATAPI_DRV) {
		printf("Selected drive is not a CD-ROM\n");
		return FALSE;
	}
	return TRUE;
}

void closeDev() {
	if (scsireq) {
		CloseDevice((struct IORequest *)scsireq);
		DeleteExtIO((struct IORequest *)scsireq);
	}
	if (port) DeletePort(port);
}

int load() {
	int i,poc,akt=0;
	unsigned long llba;


	if (!TestCD()) {printf("\t...not loaded\n"); return(FALSE);}
	if ((poc = packet(readtoc,(unsigned short *)data,1024)) > 0) {
		idstring[0]='I';
		idstring[1]='D';
		for (i=9;i<poc-8;i+=8) {
			if (i==25) {
				llba=LBA(data[i],data[i+1],data[i+2])-150;
				sprintf(&idstring[4],"%06X",llba);
			}
			akt=(i-9)/8;
			track[akt][0]=data[i];
			track[akt][1]=data[i+1];
			track[akt][2]=data[i+2];
		}
	}
	else {printf("\tLoad error\n"); idstring[0]='\0';}
	if (akt!=0) {
		akt++;
		tracks_cnt = akt;
		track[akt][0]=data[i];
		track[akt][1]=data[i+1];
		track[akt][2]=data[i+2];
		llba=LBA(data[i],data[i+1],data[i+2])-150;
		if (poc<=28) sprintf(&idstring[4],"%06X",llba);
		sprintf(&idstring[10],"%06X",llba);
		idstring[2]=tracks_cnt/10+'0';
		idstring[3]=tracks_cnt%10+'0';
		idstring[16]='\0';
		act_track=0;
		printf("Tracks: %d  Length: %02d:%02d\n",akt,track[akt][0],track[akt][1],track[akt][2]);
	}
	else act_track = -1;
	return(TRUE);
}

void stop() {
	if (!hram) return;
	hram=0;
	packet(stopcmd,(unsigned short *)data1,1024);
}

void play(int trc) {
	unsigned char *adr;

	if (disk_in!=1 || act_track==-1) return;
	if (hram) stop();
	act_track = trc-1;
	hram = 1;
	adr = (unsigned char *)playcmd;
	adr[3] = track[act_track][0];
	adr[4] = track[act_track][1];
	adr[5] = track[act_track][2];
	adr[6] = track[tracks_cnt][0];
	adr[7] = track[tracks_cnt][1];
	adr[8] = track[tracks_cnt][2];
	packet(playcmd,(unsigned short *)data1,1024);
	printf("\tPlaying track %d\n",act_track+1);
}

void pausef() {
	if (!pause) {
		if (!hram) return;
		pause = 1;
		hram = 0;
		packet(pausecmd,(unsigned short *)data1,1024);
		printf("\tPause...\n");
	}
	else {
		pause = 0;
		hram = 1;
		packet(resumecmd,(unsigned short *)data1,1024);
	}
}

void Commands() {
	printf("********************\n");
	printf("p  - play all tracks\n");
	printf("pN - play the track N\n");
	printf("l  - load CD\n");
	printf("m  - pause / resume\n");
	printf("s  - stop\n");
	printf("i  - disk info\n");
	printf(">  - next track\n");
	printf("<  - previous track\n");
	printf("q  - quit\n");
	printf("********************\n\n");
}

int Command(char *com) {
	int pom;

	switch(com[0]) {
		case 'q': stop(); return(FALSE);
		case 'p':
			if (com[1]=='\n' || com[1]=='\0') {play(1); break;}
			if (sscanf(&com[1],"%d",&pom) == 1)
				if (pom>=1 && pom<=tracks_cnt) play(pom);
			break;
		case 'h': Commands(); break;
		case 'm': pausef(); break;
		case 's': stop(); break;
		case 'i':
			printf("\tTracks: %d  Length: %02d:%02d\n",tracks_cnt,track[tracks_cnt][0],track[tracks_cnt][1],track[tracks_cnt][2]);
			printf("\tDisk ID: %s\n",idstring);
			break;
		case 'l': if (load()) play(1); break;
		case '>': if (act_track+2<=tracks_cnt) play(act_track+2); break;
		case '<': if (act_track>0) play(act_track); break;
	}
	return TRUE;
}

void main(int argc,char *argv[])
{
	int i;

	for (i=1;i<argc;i++) {
		if (argv[i][0]=='?') {printf("CLIPlayCD [-<unit>]\n"); return;}
		if (argv[i][0]=='-') unit=argv[i][1]-'0';
	}
	if (initDev()) {
		printf("<h = help>\n\n");
		if (TestCD()) {
			if (load()) {
				play(1);
			}
		}
		do {
			printf("* CLIPlayCD: ");
			scanf("%s",txt);
		} while(Command(txt));
	} else printf("Cannot open ide.device\n");
	closeDev();
}

int packet(unsigned short *cmd,unsigned short *dat,unsigned long dlzka) {
	scsireq->io_Length   = sizeof(struct SCSICmd);
	scsireq->io_Data     = (APTR)&scsicmd;
	scsireq->io_Command  = HD_SCSICMD;

	scsicmd.scsi_Data        = (UWORD *)dat;
	scsicmd.scsi_Length      = dlzka;
	scsicmd.scsi_Flags       = SCSIF_AUTOSENSE | SCSIF_READ;
	scsicmd.scsi_SenseData   = (UBYTE *)sendat;
	scsicmd.scsi_SenseLength = 20;
	scsicmd.scsi_SenseActual = 0;
	scsicmd.scsi_Command     = (UBYTE *)cmd;
	scsicmd.scsi_CmdLength   = 12;

	DoIO ((struct IORequest *)scsireq);

//printf("0x%04X\n",scsicmd.scsi_CmdLength);
	if (scsicmd.scsi_Status) {
		hram = 0;
		if (scsicmd.scsi_SenseActual != 0) {
			if (sendat[2]==0x02 && sendat[12]==0x3A) {
				disk_in=-2;
				hram=0;
				pause=0;
				if (cmd != modesense) TestCD();
			}
		}
//		printf("STAT SEN ASC ASCQ = %02X %02X %02X %02X\n",scsicmd.scsi_Status,sendat[2],sendat[12],sendat[13]);
		return -1;
	}
	return scsicmd.scsi_Actual;
}

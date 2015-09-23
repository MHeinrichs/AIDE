/* modified april 2002 ML */


#include <stdio.h>
#include <string.h>
/*/#include <pragma/exec_lib.h>*/
/*#include <clib/alib_protos.h>*/
#include <devices/trackdisk.h>
#include <devices/scsidisk.h>
#include "mydev.h"

struct MsgPort *port;
struct IOStdReq *req;
char *txt,pom,hlp[64];
FILE *fd=0;
struct MyUnit *mu;

void main(int argc,char *argv[]) {
   int unit;

   char *devicename; /* ML */

   if (argc!=2 && argc!=3) exit(0);
   unit = argv[1][0]-'0';
   if (unit!=0 && unit!=1) exit(0);

   port = (struct MsgPort *)CreatePort(0,0);
   if (!port) exit(10);

   req = (struct IOStdReq *)CreateExtIO(port, sizeof(struct IOExtTD));
   if (!req) exit(11);

   devicename="ide.device";  /* ML */
   if (argc==3) devicename=argv[2]; /* ML */
   if (OpenDevice (devicename,unit,(struct IORequest *)req,0)) exit(12);

   mu = (struct MyUnit *)req->io_Unit;
   if (argc==2) {
      printf("Device %d:\n",mu->mdu_UnitNum);
      if (mu->mdu_drv_type==ATAPI_DRV) {
         printf("  ATAPI device\n");
         printf("  Motor is          : ");
         if (mu->mdu_motor==0) printf("OFF\n"); else printf("ON\n");
         printf("  Disk change count : %d\n",mu->mdu_change_cnt);
         printf("  Disk inserted     : ");
         if (mu->mdu_no_disk==0) printf("YES\n"); else printf("NO\n");
      }
      else if (mu->mdu_drv_type==ATA_DRV) {
         printf("  ATA device\n");
         if (mu->mdu_lba) {
            printf("  LBA");
            if (mu->mdu_numlba) printf(" - %d user addressable sectors\n",mu->mdu_numlba);
            else printf("\n");
         } else printf("  no LBA\n");
         printf("  Sectors per track : %d\n",mu->mdu_sectors_per_track);
         printf("  Heads             : %d\n",mu->mdu_heads);
         printf("  Cylinders         : %d\n",mu->mdu_cylinders);
         printf("  Size              : %d MB\n",mu->mdu_sectors_per_track*mu->mdu_heads*mu->mdu_cylinders/2048);
         printf("  Motor is          : ");
         if (mu->mdu_motor==0) printf("OFF\n"); else printf("ON\n");
      }
      else printf("  not present\n",unit);
      if (mu->mdu_drv_type!=UNKNOWN_DRV && mu->mdu_auto) {
         printf("  Serial number     : %s\n",mu->mdu_ser_num);
         printf("  Firmware revision : %s\n",mu->mdu_firm_rev);
         printf("  Model number      : %s\n",mu->mdu_model_num);
      }
   }
   else {
      sprintf(hlp,"Ram:Drive%d",unit);
      if ((fd = fopen(hlp,"w"))!=NULL) {
         if (mu->mdu_drv_type==ATAPI_DRV) {
            fprintf(fd,"ATAPI\n");
         }
         else if (mu->mdu_drv_type==ATA_DRV) {
            fprintf(fd,"ATA\n");
            if (mu->mdu_lba) {
               fprintf(fd,"LBA %d\n",mu->mdu_numlba);
            } else fprintf(fd,"noLBA 0\n");
            fprintf(fd,"%d\n",mu->mdu_sectors_per_track);
            fprintf(fd,"%d\n",mu->mdu_heads);
            fprintf(fd,"%d\n",mu->mdu_cylinders);
            fprintf(fd,"%d\n",mu->mdu_sectors_per_track*mu->mdu_heads*mu->mdu_cylinders/2048);
         }
         else fprintf(fd,"NONE\n");
         if (mu->mdu_drv_type!=UNKNOWN_DRV) {
            if (mu->mdu_auto) {
               fprintf(fd,"[%s]\n",mu->mdu_ser_num);
               fprintf(fd,"[%s]\n",mu->mdu_firm_rev);
               fprintf(fd,"[%s]\n",mu->mdu_model_num);
            }
            else fprintf(fd,"X\nX\nX\n");
         }
      }
      fclose(fd);
   }

   CloseDevice((struct IORequest *)req);
   DeleteExtIO((struct IORequest *)req);
   DeletePort(port);
   exit(0);
}


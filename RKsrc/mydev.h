/* // comments changed to /* */  /* ML */

#include <exec/ports.h>
#include <exec/devices.h>

#define CMD_MYCMD    29

struct MyUnit {
   struct   Unit mdu_Unit;
   ULONG    mdu_UnitNum;
   UBYTE    mdu_SigBit;
   UBYTE    mdu_pad;
   APTR     mdu_Device;
   ULONG    mdu_drv_type;        /*see bellow for possible values*/
   ULONG    mdu_firstcall;       /*was drive called yet?*/
   ULONG    mdu_auto;            /*get drive parameters automatic? = TRUE*/
   ULONG    mdu_lba;             /*use LBA? For ATAPI always TRUE*/
   ULONG    mdu_sectors_per_track;  /*only for ATA*/
   ULONG    mdu_heads;           /*only for ATA*/
   ULONG    mdu_cylinders;       /*only for ATA*/
   ULONG    mdu_numlba;          /*only for ATA with LBA=TRUE*/
   char     mdu_ser_num[22];     /*serial number*/
   char     mdu_firm_rev[48];    /*firware revision*/
   char     mdu_model_num[56];   /*model number*/
   ULONG    mdu_motor;           /*motor status*/
   ULONG    mdu_change_cnt;      /*count of disk changes - only for ATAPI*/
   ULONG    mdu_no_disk;         /*isn't disk inserted? - only for ATAPI*/
};

/*drive types*/
#define ATA_DRV      0
#define ATAPI_DRV    1
#define UNKNOWN_DRV  2


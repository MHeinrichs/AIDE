MYPROCSTACKSIZE   EQU   $1800
MYPROCPRI   EQU   5


   DEVINIT
   DEVCMD   CMD_MOTOR    ; control the disk's motor (NO-OP)
   DEVCMD   CMD_SEEK     ; explicit seek (NO-OP)
   DEVCMD   CMD_FORMAT   ; format disk - equated to WRITE for RAMDISK
   DEVCMD   CMD_REMOVE   ; notify when disk changes (NO-OP)
   DEVCMD   CMD_CHANGENUM     ; number of disk changes (always 0)
   DEVCMD   CMD_CHANGESTATE   ; is there a disk in the drive? (always TRUE)
   DEVCMD   CMD_PROTSTATUS    ; is the disk write protected? (always FALSE)
   DEVCMD   CMD_RAWREAD       ; Not supported
   DEVCMD   CMD_RAWWRITE      ; Not supported
   DEVCMD   CMD_GETDRIVETYPE  ; Get drive type
   DEVCMD   CMD_GETNUMTRACKS  ; Get number of tracks
   DEVCMD   CMD_ADDCHANGEINT  ; Add disk change interrupt (NO-OP)
   DEVCMD   CMD_REMCHANGEINT  ; Remove disk change interrupt ( NO-OP)
;  DEVCMD   MYDEV_END   ; place marker -- first illegal command #
MYDEV_END   EQU 30


;-----------------------------------------------------------------------
;
; device data structures
;
;-----------------------------------------------------------------------

; maximum number of units in this device
MD_NUMUNITS EQU   $2

    STRUCTURE MyDev,LIB_SIZE
   ULONG    md_SysLib
   ULONG    md_DosLib
   ULONG    md_SegList
   ULONG    md_Base      ; Base address of this device's expansion board
	 ULONG    md_Unit0mask
	 ULONG    md_Unit1mask
	 ULONG    md_Unit0adr
   ULONG    md_Unit1adr ;this one must be directly after Unit0adr because it will be set by an offset!
   ULONG    md_Unit0sigbit
   ULONG    md_Unit1sigbit
   UBYTE    md_Flags
   UBYTE    md_pad
   STRUCT   md_Units,MD_NUMUNITS*4
   STRUCT   md_tcb,TC_SIZE             ; TCB for disk task
   STRUCT   md_stack,MYPROCSTACKSIZE
   LABEL    MyDev_Sizeof

    STRUCTURE MyDevUnit,UNIT_SIZE
   ;STRUCT   mdu_PortSpacer,48   ;spacer for messageportstructure (is only 32byte, but who knows!)
   APTR     mdu_Device
   ULONG    mdu_UnitNum
   ULONG    mdu_change_cnt       ;count of disk changes - only for ATAPI
   ULONG    mdu_no_disk          ;isn't disk inserted? - only for ATAPI
   ULONG    mdu_numlba48         ;only for drives with LBA48_ACCESS
   ULONG    mdu_sectors_per_track   ;only for ATA
   ULONG    mdu_heads            ;only for ATA
   ULONG    mdu_cylinders        ;only for ATA
   ULONG    mdu_numlba           ;only for ATA with LBA=LBA24_ACCESS OR LBA48_ACCESS
   ULONG    mdu_act_Actual       ;SCSI-Packet-Stuff
   STRUCT   mdu_sense_data,20        ;data for sense scsi-packet
   STRUCT   mdu_ser_num,24       ;serial number:20Chars + 1null byte +3pad
   STRUCT   mdu_firm_rev,12      ;firware revision: 8 chars + 1 null byte +3pad
   STRUCT   mdu_model_num,44     ;model number: 40 chars + 1 null byte +3 pad
   UWORD    mdu_drv_type         ;see bellow for possible values
   UWORD    mdu_firstcall        ;was drive called yet?
   UWORD    mdu_auto             ;get drive parameters automatic? = TRUE
   UWORD    mdu_lba              ;use LBA? For ATAPI always TRUE
   UWORD    mdu_motor            ;motor status
   UWORD    mdu_act_cmd,8        ;actual SCSI-Command
   UBYTE    mdu_SigBit           ;Signal bit allocated for interrupts
   UBYTE    mdu_SectorBuffer	 ;max number of sectors per transfer block
   UBYTE    mdu_actSectorCount	 ;actual number of sectors per transfer block   
   UBYTE    mdu_act_Flags        ;actual SCSI-Packet Flags
   UBYTE    mdu_act_Status			 ;actual SCSI-Status
   UBYTE    mdu_pad1
   UBYTE    mdu_pad2
   UBYTE    mdu_pad3
   LABEL    MyDevUnit_Sizeof

;drive types
ATA_DRV     equ   0
ATAPI_DRV   equ   1
UNKNOWN_DRV equ   2
SATA_DRV     equ  3
SATAPI_DRV   equ  4

;access types
CHS_ACCESS equ 0
LBA28_ACCESS equ 1
LBA48_ACCESS equ 2

   ;------ state bit for unit stopped
   BITDEF   MDU,STOPPED,2

;NAMES etc.

MYDEVNAME   MACRO
      DC.B   'ide.device',0
      ENDM

IDSTRINGMACRO macro
      dc.b    "IDEDevice 2.24 (24.02.2016)",13,10,0
      ENDM

VERSION equ 2
REVISION equ 24

;DOSNAME      MACRO
;      DC.B   'dos.library',0
;      ENDM


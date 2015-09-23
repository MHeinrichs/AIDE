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
   UBYTE    md_Flags
   UBYTE    md_pad
   STRUCT   md_Units,MD_NUMUNITS*4
   STRUCT   md_tcb,TC_SIZE             ; TCB for disk task
   STRUCT   md_stack,MYPROCSTACKSIZE
   LABEL    MyDev_Sizeof

    STRUCTURE MyDevUnit,UNIT_SIZE
   ULONG    mdu_UnitNum
   UBYTE    mdu_SigBit           ;Signal bit allocated for interrupts
   UBYTE    mdu_pad
   APTR     mdu_Device
   UWORD    mdu_drv_type         ;see bellow for possible values
   UWORD    mdu_firstcall        ;was drive called yet?
   UWORD    mdu_auto             ;get drive parameters automatic? = TRUE
   UWORD    mdu_lba              ;use LBA? For ATAPI always TRUE
   ULONG    mdu_sectors_per_track   ;only for ATA
   ULONG    mdu_heads            ;only for ATA
   ULONG    mdu_cylinders        ;only for ATA
   ULONG    mdu_numlba           ;only for ATA with LBA=TRUE
   STRUCT   mdu_ser_num,22       ;serial number
   STRUCT   mdu_firm_rev,48      ;firware revision
   STRUCT   mdu_model_num,56     ;model number
   UWORD    mdu_motor            ;motor status
   ULONG    mdu_change_cnt       ;count of disk changes - only for ATAPI
   ULONG    mdu_no_disk          ;isn't disk inserted? - only for ATAPI

;now some stuff for automount
   APTR		hu_Addr			points to shared per-ID data
   UBYTE	hu_ATDriveBit		hu_Unit << AT_UNIT_SHIFT
   UBYTE	hu_MountDone		1 = partitions mounted
   UBYTE	hu_LUN			logical unit number for SCSI
   ULONG	hu_BlockSize		byte size of one block
   UWORD	hu_BlockShift		shift for divide by blocksize
   UBYTE	hu_Flags		flags from RDB
   APTR		hu_RDB			RigidDiskBlock data
   UWORD	hu_TotalMapped		number of mapped blocks
   APTR		hu_BadBlockList		list of mapped bad blocks
   LABEL    MyDevUnit_Sizeof

;drive types
ATA_DRV     equ   0
ATAPI_DRV   equ   1
UNKNOWN_DRV equ   2

   ;------ state bit for unit stopped
   BITDEF   MDU,STOPPED,2

;NAMES etc.

MYDEVNAME   MACRO
      DC.B   'ide.device',0
      ENDM

IDSTRINGMACRO macro
      dc.b    "IDEDevice 2.17 (25.10.2014)",13,10,0
      ENDM

VERSION equ 2
REVISION equ 17

DOSNAME      MACRO
      DC.B   'dos.library',0
      ENDM


MYPROCSTACKSIZE   EQU   $1000
MYPROCPRI   EQU   10
MYPRI   EQU   5


	include "debug/debug-wrapper.i"
		IFND	DEBUG_DETAIL
DEBUG_DETAIL	SET	0	;Detail level of debugging.  Zero for none.
		ENDC

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
	ULONG    md_ATARdWt
	STRUCT   md_Units,MD_NUMUNITS*4
	UBYTE    md_Flags
	UBYTE    md_pad,3
	LABEL    MyDev_Sizeof

	 STRUCTURE MyDevUnit,UNIT_SIZE ;34 byte-> 2 byte missing for lon
	UBYTE    mdu_SigBit           ;Signal bit allocated for interrupts
	UBYTE    mdu_SectorBuffer	 ;max number of sectors per transfer block
	;long alligned  
	APTR     mdu_Device
	ULONG    mdu_change_cnt       ;count of disk changes - only for ATAPI
	ULONG    mdu_no_disk          ;isn't disk inserted? - only for ATAPI
	ULONG    mdu_numlba48         ;only for drives with LBA48_ACCESS
	ULONG    mdu_sectors_per_track   ;only for ATA
	ULONG    mdu_heads            ;only for ATA
	ULONG    mdu_cylinders        ;only for ATA
	ULONG    mdu_numlba           ;only for ATA with LBA=LBA24_ACCESS OR LBA48_ACCESS
	ULONG    mdu_act_Actual       ;SCSI-Packet-Stuff
	STRUCT   mdu_EmulInquiry,9*4
	STRUCT   mdu_EmulMSPage3,7*4
	STRUCT   mdu_EmulMSPage4,7*4
	STRUCT   mdu_rs_cmd,6*2       
	STRUCT   mdu_sense_data,20        ;data for sense scsi-packet
	STRUCT   mdu_ser_num,24       ;serial number:20Chars + 1null byte +3pad
	STRUCT   mdu_firm_rev,12      ;firware revision: 8 chars + 1 null byte +3pad
	STRUCT   mdu_model_num,44     ;model number: 40 chars + 1 null byte +3 pad
	STRUCT   mdu_act_cmd,16       ;actual SCSI-Command (8 words = 16 bytes)
	UWORD    mdu_drv_type         ;see bellow for possible values
	UWORD    mdu_lba              ;use LBA? For ATAPI always TRUE
	UWORD    mdu_motor            ;motor status
	;odd word allign
	UBYTE    mdu_actSectorCount	 ;actual number of sectors per transfer block   
	UBYTE    mdu_act_Flags        ;actual SCSI-Packet Flags
	;Long align
	UBYTE    mdu_act_Status			 ;actual SCSI-Status
	UBYTE    mdu_UnitNum
	UBYTE    mdu_firstcall        ;was drive called yet?
	UBYTE    mdu_auto             ;get drive parameters automatic? = TRUE
	;Long align
	ULONG    mdu_ATARdWt          ;Relocation of ATARdWt routine
	STRUCT   mdu_stack,MYPROCSTACKSIZE
	STRUCT   mdu_tcb,TC_SIZE	; Task Control Block (TCB) for disk task
	LABEL    MyDevUnit_Sizeof


;NAMES etc.

MYDEVNAME   MACRO
	   DC.B   'ide.device',0
	   ENDM

MYTASKNAME   MACRO
	   DC.B   'ide0.device',0
	   ENDM

MYTASKNAME2   MACRO
	   DC.B   'ide1.device',0
	   ENDM


IDSTRINGMACRO macro
	   dc.b    "IDE.Device 2.31 (17.05.2016)",13,10,0
	   ENDM

VERSION equ 2
REVISION equ 31

;DOSNAME      MACRO
;      DC.B   'dos.library',0
;      ENDM


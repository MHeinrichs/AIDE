;command block registers
;TF_DATA
;TF_ERROR
;TF_FEATURES
;TF_SECTOR_COUNT
;TF_SECTOR_NUMBER
;TF_CYLINDER_LOW
;TF_CYLINDER_HIGH
;TF_DRIVE_HEAD
;TF_STATUS
;TF_COMMAND
;control block registers
;TF_ALTERNATE_STATUS
;TF_DEVICE_CONTROL
;TF_DRIVE_ADDRESS

;   Bit names for general use 
bit7  equ   $80
bit6  equ   $40
bit5  equ   $20
bit4  equ   $10
bit3  equ   $08
bit2  equ   $04
bit1  equ   $02
bit0  equ   $01

;status register
BSY  equ    bit7 ;Busy
DRDY equ    bit6 ;drive ready
DWF  equ    bit5 ;drive write fault
DF   equ    DWF
DSC  equ    bit4 ;drive seek complete
SKC  equ    DSC
DRQ  equ    bit3 ;data request
CORR equ    bit2 ;corrected data
IDX  equ    bit1 ;index
ERR  equ    bit0 ;error

;device control register
                 ;bit3 always 1
SRST  equ   bit2 ;software reset
nIEN  equ   bit1 ;interrupt enable
                 ;bit0 always 0

;error register
BBK   equ   bit7 ;bad block detected
UNC   equ   bit6 ;uncorrectable data error
;MC    equ   bit5 ;media changed
IDNF  equ   bit4 ;ID Not Found
ABRT  equ   bit3 ;aborted command
MCR   equ   bit2 ;Media Change Requested
TK0NF equ   bit1 ;Track 0 Not Found
AMNF  equ   bit0 ;Address Mark Not Found

;drive / head register
           ;bit7 always 1
L     equ   bit6 ;L=0.CHS , L=1.LBA
           ;bit5 always 1
DRV   equ   bit4 ;drive select 0=drive0
DRV0  equ   0+bit7+bit5
DRV1  equ   DRV+bit7+bit5
HS3   equ   bit3 ; Head select / LBA bits 24-27
HS2   equ   bit2 ;
HS1   equ   bit1
HS0   equ   bit0

;some of the ATA commands
ATA_IDENTIFY_DRIVE equ $EC
IDENTIFY_PACKET_DEVICE equ $a1
ATA_EXECUTE_DRIVE_DIAGNOSTIC equ $90
ATA_SLEEP  equ  $E6
ATA_SEEK equ $70
ATA_READ_SECTORS equ $20
ATA_WRITE_SECTORS equ $30
ATA_READ_MULTIPLE_SECTORS equ $C4
ATA_WRITE_MULTIPLE_SECTORS equ $C5
ATA_READ_SECTORS_EXT equ $24
ATA_WRITE_SECTORS_EXT equ $34
ATA_READ_MULTIPLE_SECTORS_EXT equ $29
ATA_WRITE_MULTIPLE_SECTORS_EXT equ $39
ATA_NOP  equ  0
ATA_IDLE  equ  $e3;$97
ATA_IDLE_IMMEDIATE equ $E1 ;$95
ATA_STANDBY_IMMEDIATE equ $E0
ATA_INITIALIZE_DRIVE_PARAMETERS equ $91
ATA_SET_FEATURES equ $EF
ATA_SET_MULTIPLE equ $C6
ATA_CHECK_POWER_MODE equ $E5
ATA_PACKET equ $A0
ATA_RECALIBRATE equ $10

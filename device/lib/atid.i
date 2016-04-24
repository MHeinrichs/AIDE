   ;INCLUDE "IDE_BASE_ADDRESS.i"
   include "/new-version/IDE_BASE_ADDRESS.i" ;The base adress for the ide-driver
TF  equ  IDE_BASE_ADDRESS+$1000+$2000; Task File base address + 3000hex
CS0      equ  -$1000    ;Amiga A12 line is connected to ATA /CS0
CS1      equ  -$2000    ;Amiga A13 line is connected to ATA /CS1
REG_INC  equ  4         ;Amiga A2,A3,A4 are connected to ATA SA0,SA1,SA2
Y        equ  0         ;Amiga D0..7 are Drive SD8..15, D8..15 are SD0..7
                        ;needs to be 1 if d0..15 are ATA sd0..15

;command block registers
TF_DATA        equ      (TF+$0*REG_INC+CS0)
;TF_DATA_8BIT   equ     (TF+$0*REG_INC+CS0); not implemented usually
TF_ERROR       equ      (Y+TF+$1*REG_INC+CS0)
TF_FEATURES    equ      (Y+TF+$1*REG_INC+CS0)
TF_SECTOR_COUNT   equ   (Y+TF+$2*REG_INC+CS0)
TF_SECTOR_NUMBER  equ   (Y+TF+$3*REG_INC+CS0)
TF_LBA_LOW_BYTE   equ   TF_SECTOR_NUMBER
TF_CYLINDER_LOW   equ   (Y+TF+$4*REG_INC+CS0)
TF_LBA_MID_BYTE   equ   TF_CYLINDER_LOW
TF_CYLINDER_HIGH  equ   (Y+TF+$5*REG_INC+CS0)
TF_LBA_HIGH_BYTE   equ   TF_CYLINDER_HIGH
TF_DRIVE_HEAD     equ   (Y+TF+$6*REG_INC+CS0)
TF_STATUS         equ   (Y+TF+$7*REG_INC+CS0)
TF_COMMAND        equ   (Y+TF+$7*REG_INC+CS0)
;control block registers
TF_ALTERNATE_STATUS equ (Y+TF+$6*REG_INC+CS1)
TF_DEVICE_CONTROL   equ TF_ALTERNATE_STATUS
TF_DRIVE_ADDRESS    equ (Y+TF+$7*REG_INC+CS1)


LOOPPAUSE  equ   512      ; value for pause loop
LOOP  equ   300000      ; timeout value for ATA - motor is on
LOOP2 equ   5120000     ; timeout value for ATA - motor is off
LOOP3 equ   512000       ; timeout value for ATAPI
TESTBYTE1 equ $B0
TESTBYTE2 equ $0B
TESTBYTE3 equ $51
TIMEOUT   equ $45
;some of the 32-bit longword error codes rdwt.asm returns:
BADLENGTH   equ "PELI"
BADUNIT     equ "MELA"
BADOFFSET   equ "TISI"
NOREADWRITE equ "PILU"
TRUE  equ   1
FALSE equ   0

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

MAX_TRANSFER equ 128 ;256 for now!

	;------ state bit for unit stopped
	BITDEF   MDU,STOPPED,2 
	;------ state bit for slave unit
	BITDEF   MDU,SLAVE,4 




;accesses to ATA-registers are done with macros
;this is for future development of parallel port IDE-interface
RATABYTE macro
   move.b \1,\2
  endm
WATABYTE macro
   move.b \1,\2
  endm
RATAWORD macro
   move.w \1,\2
  endm
WATAWORD macro
   move.w \1,\2
 endm

WAITNOTDRQ macro
   move.l   #LOOP,\2
wqd\@ 
   RATABYTE TF_ALTERNATE_STATUS,\1
   and.b    #DRQ,\1
   beq.s    wqd1\@
   dbra			\2,wqd\@   
wqd1\@
   tst.l    \2
 endm

WAITNOTBSY macro
   move.l   #LOOP,\2
wn\@ 
   RATABYTE TF_ALTERNATE_STATUS,\1
   and.b    #BSY,\1
   beq.s    wn1\@
   dbra     \2,wn\@
wn1\@
   tst.l    \2
 endm

WAITDRQ macro
   move.l   #LOOP,\2
wd\@ 
   RATABYTE TF_ALTERNATE_STATUS,\1
   and.b    #BSY+DRQ,\1
   cmp.b    #DRQ,\1
   beq.s    wd1\@
   dbra     \2,wd\@
wd1\@
   tst.l    \2
 endm

WAITREADYFORNEWCOMMAND macro
	move.l	#2*LOOP,\2               ;double time because we wait for bsy to go low and DRDY to go up
wrfc\@
  RATABYTE TF_STATUS,\1
	and.b	 #BSY+DRDY+DWF+ERR,\1
	cmp.b	 #DRDY,\1
	beq.s	 wrfc1\@
  dbra     \2,wrfc\@
wrfc1\@
  tst.l    \2
 endm




;read macros
RATADATAA5_D0_BYTES_LONG macro
   move.l   #TF_DATA,a0
   ;d0 must be < $0002000
   lsr.l    #2,d0;bytes to long and loop unrolling: 8 times
   sub.l    #1,d0;for dbra
gre2\@
   move.l   (a0),(a5)+
   dbra     d0,gre2\@
  endm

;read macros
RATADATAA5_512_BYTES macro
   move.l   #TF_DATA,a0
   ;d0 must be < $0002000
   moveq.l  #7,d0       ;bytes to long and loop unrolling: 8 times -1 for dbra
gre3\@
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   dbra     d0,gre3\@
  endm

;read macros
RATADATAA5_D0_BYTES_64 macro
   move.l   #TF_DATA,a0
   ;d0 must be < $0002000
   lsr.l    #8,d0;bytes to long and loop unrolling: 8 times
   sub.l    #1,d0;for dbra
gre2\@
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   move.l   (a0),(a5)+
   dbra     d0,gre2\@
  endm

RATADATAA5_D0_BYTES macro
   move.l   #TF_DATA,a0
   ;d0 must be < $0002000
   lsr.l    #1,d0;bytes to words
   sub.l    #1,d0;for dbra
gre\@
   move.w   (a0),(a5)+
   dbra     d0,gre\@
  endm


;write macros
WATADATAA5_D0_BYTES_LONG macro
   move.l   #TF_DATA,a0
   lsr.l    #2,d0;bytes to long and loop unrolling: 8 times
   sub.l    #1,d0
cva2\@
   move.l   (a5)+,(a0)
   dbra     d0,cva2\@
 endm

;write macros
WATADATAA5_D0_BYTES_64 macro
   move.l   #TF_DATA,a0
   lsr.l    #8,d0;bytes to long and loop unrolling: 8 times
   sub.l    #1,d0
cva2\@
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   dbra     d0,cva2\@
 endm

;write macros
WATADATAA5_512_BYTES macro
   move.l   #TF_DATA,a0
   moveq.l  #7,d0     ;bytes to long and loop unrolling: 8 times -1 for dbra
cva3\@
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   move.l   (a5)+,(a0)
   dbra     d0,cva3\@
 endm



WATADATAA5_D0_BYTES macro
   move.l   #TF_DATA,a0
   lsr.l    #1,d0
   sub.l    #1,d0
cva\@
   move.w   (a5)+,(a0)
   dbra     d0,cva\@
 endm

;wait macros
DLY400NS macro

   tst.b    $000004  ;access to exec base (Chip RAM) should last at least 4 times 140ns = 560ns
   ;tst.b    $000004
  
  endm

DLY5US macro ;wait at least 5 microseconds

   tst.b    $bfe301 ;slow CIA access cycle takes 12-20 7MHz clocks: 1.7us - 2.8us
   tst.b    $bfe301
   ;tst.b    $bfe301
   ;tst.b    $bfe301

  endm


DLY3US macro ;wait approx 3 microseconds

;   tst.b    $000004 ;one chipram-cycle at least 560ns!
;   tst.b    $000004
;   tst.b    $000004
;   tst.b    $000004

   tst.b    $bfe301 ;slow CIA access cycle takes 12-20 7MHz clocks: 1.7us - 2.8us
;   tst.b    $bfe301
;   tst.b    $bfe301
;   tst.b    $bfe301
  endm

;init macro
INITATAINTERFACE macro
 ENDM


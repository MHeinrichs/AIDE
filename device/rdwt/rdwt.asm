;RdWt has public InitDrive and ATARdWT and SCSIDirectCMD as the

;most important functions.

;there are a other public functions too,

;! ATARdWt now expects high 32 bits of 64 bit offset in d5 !

;! and #READOPE in A2, if it is a read operation,!

;! or  #WRITEOPE value if it is a write!

; Change-list:

; July 2004, December 2003

; --ide.device RDWT,asm APRIL 2002 V1.6

; ide.device read/write-routines

;-- support for ATAPI devices and two drives support added 10.9.2000 (R.K.)



; ! set tabs to 3 !



;READOPE AND WRITEOPE ARE DEFINED HERE AND XPORTED BY XDEF:S

READOPE equ  $b00bdeed

WRITEOPE equ $babecafe

   xdef READOPE

   xdef WRITEOPE



TRUE  equ   1

FALSE equ   0

LOOP  equ   30000      ; timeout value for ATA - motor is on

LOOP2 equ   2500000     ; timeout value for ATA - motor is off

LOOP3 equ   20000       ; timeout value for ATAPI



   cnop  0,4


buffer         dc.l  0        ;buffer pointer for ATA/ATAPI IDENTIFY DRIVE



   xref  _LVOAllocSignal      ;external references

   xref  _LVOSignal           ;these will be linked from amiga.lib

   xref  _LVOWait

   xref  _LVOFindTask



   include "exec/io.i"

   include "exec/types.i"

   include "exec/lists.i"

   include "exec/libraries.i"

   include "exec/devices.i"
   
   include "exec/memory.i"

   include "exec/io.i"

   include "exec/tasks.i"

   include "devices/scsidisk.i"

   include "/lib/mydev.i"  ;select name etc, of the device
   include "/lib/atid.i"   ;This include has the macros which
                           ;are used to access a particular
                           ;implementation of an Amiga to ATA 
                           ;hardware interface, such as an A500 
                           ;side slot interface or a Parallel port
                           ;interface.
    include "/lib/ata.i"    ; defines ATA bits and commands
   
   include "/lib/asmsupp.i"; Various helpers from Commodore

   XLIB AllocMem
   
   XLIB FreeMem



;some of the 32-bit longword error codes rdwt.asm returns:

BADLENGTH   equ "PELI"

BADUNIT     equ "MELA"

BADOFFSET   equ "TISI"

NOREADWRITE equ "PILU"



;macro INITATAINTERFACE *needs* to be executed once before this routine is ever called

;That might happen in dev.asm.

;Both drives on the ide-cable are initialised by separate calls to this

;routine.



   PUBLIC   InitDrive

InitDrive   ;a3 = unitptr

   movem.l  d1/d2/a0/a1/a5,-(sp)

   move.l   #512,d0       ; we want 512 bytes for a buffer
   
   move.l   #MEMF_PUBLIC!MEMF_CLEAR,d1 ;Preferable Fast mem, cleared
   
   LINKSYS  AllocMem,md_SysLib(a6)
   
   tst.l    d0             ; memory ok?
   
   beq      wfc1
   
   move.l   d0,buffer      ;save pointer
   

   RATABYTE TF_STATUS,d0   ;clear drive interrupt line

   move.w   #FALSE,mdu_firstcall(a3)

   cmp.w    #TRUE,mdu_auto(a3)

   bne      notauto



;get drive parameters
   move.w   #CHS_ACCESS,mdu_lba(a3)               ;presumption
   bsr      SelectDrive

   WATABYTE #ATA_IDENTIFY_DRIVE,TF_COMMAND   ;get drive data

   bsr      waitnotbusy1

   beq      wfc1

   RATABYTE TF_STATUS,d0
   ;some atapi drives do NOT aboard this command: just read the CYLH/L Values
   ;and.b    #ERR,d0                          ;atapi drive aborts this command
   ;beq      atadrv                           ;and sets CYLH to EBh and
   RATABYTE TF_CYLINDER_HIGH,d0				 ;read Highcyl value
   RATABYTE TF_CYLINDER_LOW,d1				 ;read lowCyl value
   ;check ata: d0=0 and d1=0
   cmp.b 	#0,d0
   bne		drive_test_ata_sata				 ;is it a sata-drive
   cmp.b	#0,d1 							 ;found an ata-drive
   beq		atadrv
drive_test_ata_sata
   ;check sata: d0=$c3 and d1=$3c
   cmp.b 	#$C3,d0
   bne		drive_test_ata_atapi			 ;is it a atapi-drive
   cmp.b	#$3C,d1 							 ;found an sata-drive
   beq		satadrv
drive_test_ata_atapi
   ;check atapi: d0=$EB and d1=$14
   cmp.b 	#$EB,d0
   bne		drive_test_ata_satapi			 ;is it a satapi-drive
   cmp.b	#$14,d1 							 ;found an atapi-drive
   beq		atapi
drive_test_ata_satapi
   ;check satapi: d0=$96 and d1=$69
   cmp.b 	#$96,d0
   bne		wfc1			   				 	 ;unknown
   cmp.b	#$69,d1 							 ;found an atapi-drive
   beq		satapi

wfc1

   move.w   #UNKNOWN_DRV,mdu_drv_type(a3)

   bra      kr2
satapi
   move.w   #SATAPI_DRV,mdu_drv_type(a3)
   bra		wfc2
atapi
   move.w   #ATAPI_DRV,mdu_drv_type(a3)
wfc2

   WATABYTE #IDENTIFY_PACKET_DEVICE,TF_COMMAND  ;get atapi drive data

   bsr      waitdrq1

   beq      wfc1
   RATABYTE TF_STATUS,d0               ;clear interrupt line
   move.w   #LBA28_ACCESS,mdu_lba(a3)  ; this does not limit DVD-Drives! The read/write routine should chop al access <48bit to lba28
   clr.l    mdu_sectors_per_track(a3)

   clr.l    mdu_heads(a3)

   clr.l    mdu_cylinders(a3)

   bra      kr3
satadrv
   move.w   #SATA_DRV,mdu_drv_type(a3)  ;sata drive
   bra      kr3
atadrv

   move.w   #ATA_DRV,mdu_drv_type(a3)  ;ata drive

kr3

   move.l   buffer,a5                  ;get identify data
   move.l	#512,d0
   RATADATAA5_D0_BYTES_64

   move.l   buffer,a5

   ;IF Y=0 SWAP BYTES IN WORD BECAUSE AMIGA DATA0..7 IS DRIVE DATA8.15

   IFEQ Y,0

      move.l   A5,A0       ;buffer start
      move.w   #256-1,d0   ;move.word because dbra uses 16bits of register

lk    move.w   (a0),d1

      move.w   d1,d2

      lsl.w    #8,d1

      lsr.w    #8,d2

      and.w    #$ff,d2

      or.w     d2,d1

      move.w   d1,(a0)+

      dbra     d0,lk
   ENDC



   move.l   buffer,a5            ;copy serial number to internal info buffer

   add.l    #20,a5

   lea      mdu_ser_num(a3),a0
   move.b   #10,d0				
ckl1

   move.w   (a5)+,(a0)+
   subq.b	#1,d0
   bne		ckl1
   move.b   #0,(a0)				;null termination of string
   move.l   buffer,a5

   add.l    #46,a5
   lea      mdu_firm_rev(a3),a0  ;copy firm. revision to int. buffer 8byte = 2 longwords!
   move.l   (a5)+,(a0)+
   move.l   (a5)+,(a0)+
   move.b   #0,(a0)              ;null termination of string
   move.l   buffer,a5

   add.l    #54,a5
   lea      mdu_model_num(a3),a0 ;copy model number to int. buffer 40 bytes = 10 longwords
   move.b   #10,d0				
ckl3
   move.l   (a5)+,(a0)+
   subq.b	#1,d0
   bne   	ckl3
   move.b   #0,(a0)				;null termination of string

   cmp.w    #ATA_DRV,mdu_drv_type(a3)

   beq      noeritd
   cmp.w    #SATA_DRV,mdu_drv_type(a3)
   beq      noeritd
   WATABYTE #0,TF_FEATURES       ;for atapi cdrom with

   bra      kr2
noeritd     ; ATA disk /SATA disk
   move.l   buffer,a5

   moveq    #0,d0

   move.w   1*2(a5),d0           ;Word 1 Number of logical cylinders

   move.l   d0,mdu_cylinders(a3) ;store to internal buffer

   move.w   6*2(a5),d0           ;Word 6 Default Translation sectors

   and.l    #$ffff,d0

   move.l   d0,mdu_sectors_per_track(a3)  ;store to internal buffer

   move.w   3*2(a5),d0           ;Word 3 Default Translation Mode number of heads

   and.l    #$ffff,d0

   move.l   d0,mdu_heads(a3)     ;store to internal buffer
   move.b	#1,mdu_SectorBuffer(a3)
   bsr		blink
   move.w   47*2(A5),d0          ;WRITE/READ Multiple capabilities
   cmp.l	#1,d0				 ;multi sector supportet?
   ble		multiple_sector_dis  ; not set
   move.b	d0,mdu_SectorBuffer(a3)
multiple_sector_dis
   bsr		blink
   move.w   49*2(A5),d0          ;Word 49 Capabilities

   and.w    #$200,d0             ;Bit 9 1=LBA Supported

   beq      nolba

   move.w   61*2(a5),d0          ;Words 60-61 # of user addressable sectors (LBA)

   swap.l   d0
   or.w     60*2(a5),d0
   move.l   d0,mdu_numlba(a3)    ;store to internal buffer

   beq      nolba                ;propably no lba support if no lba sectors
   move.w   #LBA28_ACCESS,mdu_lba(a3)    ;store to internal buffer
   move.w   83*2(A5),d0          ;Word 83 Capabilities * LBA48 support check
   and.w    #$400,d0             ;Bit 10 1=LBA48 Supported
   bra		endauto				 ; saty at LBA28
   move.l   100*2(a5),d0         ;3rd word LBA48
   swap.l   d0
   or.w     101*2(a5),d0		 ;4th word LBA48
   beq		endauto				 ;LBA48 supported but <8GB: stay at LBA28!
   move.l   d0,mdu_numlba48(a3)  ;store to internal buffer
   ;now the lower 32 bits
   move.l   102*2(a5),d0         ;1st word LBA48
   swap.l   d0
   or.w     103*2(a5),d0		 ;2nd word LBA48
   beq		endauto				 ;LBA48 supported but <8GB: stay at LBA28!
   move.l   d0,mdu_numlba(a3)    ;store to internal buffer
   move.w   #LBA48_ACCESS,mdu_lba(a3)    ;store to internal buffer	   
   bra      endauto

nolba                            ;Then its CHS
   move.w   #CHS_ACCESS,mdu_lba(a3)   ;store to internal buffer
   ;Conner Peripherals CP3044 lies about its default translation mode

   lea      CP3044txt,a0

   move.l   a5,a1

   add.l    #$50,a1

ko move.b   (a0)+,d0

   beq      wascp3044

   move.b   (a1)+,d1

   cmp.b    d0,d1

   beq      ko

;not cp3044

   bra      endauto

wascp3044

   move.l   #4,mdu_heads(a3)

   move.l   #40,mdu_sectors_per_track(a3)

   bra      endauto

CP3044txt   dc.b  $43,$50,$33,$30,$34,$34,$20,0 ;"CP3044 "

   cnop     0,4

endauto

notauto

   RATABYTE TF_STATUS,d0               ;clear interrupt line

   cmp.w    #ATA_DRV,mdu_drv_type(a3)
   beq      setupata
   cmp.w    #SATA_DRV,mdu_drv_type(a3)
   beq      setupata
   bra		kr2
setupata
   move.l   mdu_sectors_per_track(a3),d0  ;send to drive which CHS translation

   WATABYTE d0,TF_SECTOR_COUNT         ;to use - important to drives with

   move.l   mdu_heads(a3),d0           ;LBA support

   subq.b   #1,d0

   or.b     #$a0,d0

   tst.l    mdu_UnitNum(a3)

   beq      pis1

   bset.b   #4,d0

pis1

   WATABYTE d0,TF_DRIVE_HEAD

   DLY400NS

   bsr      waitreadytoacceptnewcommand

   WATABYTE #ATA_INITIALIZE_DRIVE_PARAMETERS,TF_COMMAND  ;get drive data

   bsr      waitnotbusy1

kr2
   move.l   buffer,d1 ;is there a pointer in buffer?
   tst.l    d1
   beq kr21
   
   move.l   buffer,a1
   move.l   #512,d0
   LINKSYS FreeMem,md_SysLib(a6)
   

kr21:
   move.l   #0,buffer
   movem.l  (sp)+,d1/d2/a0/a1/a5

   rts

;----



   Public   ATARdWt

ATARdWt:

   ;a0 IO data address

   ;a2 #READOPE/#WRITEOPE  operation is either read or write

   ;a3 unitptr

   ;d0 io length

   ;d1 io offset low

   ;d5 io offset high  !NEW!!

   ;d2 unit number

   movem.l  d1-d7/a0-a6,-(sp)

   bsr      SelectDrive

   beq      errcode
   move.l   d0,d3			  ;save length to somewhere save
   move.l   #BADUNIT,d0       ;Check that unit is 0 or 1

   move.l   d2,d4

   and.l    #$FFFFFFFE,d4

   bne      Quits

   move.l   #BADLENGTH,d0     ;Check if length is multiple of 512

   move.l   d3,d4

   and.l    #$1ff,d4

   bne      Quits
   move.l   #BADLENGTH,d0     ;No sectors ?
   lsr.l    #8,d3             ;Divide by 512 to get
   lsr.l    #1,d3             ;Number of sectors   
   beq      Quits
   move.l   #BADOFFSET,d0	  ;Check if offset is too large: d1 holds 23 bits (32 minus 9)
   lsr.l    #8,d1
   lsr.l    #1,d1			  ;now d1 holds the bits 0..23 of the start block			
   ;check if d5 contains an offset >32
   ;remember that d1 holds the first 23 bits so this can be only 9 additional ones! 
   cmp.l    #$1FF,d5		     
   bgt      Quits
   move.l   d5,d4			  ;only lower five bits are allowed! this could be extended by 8 bits for REAl 48LBA access!
   and.l	#$1FF,d4
   ror.l	#8,d4			  ;now rotate the high offset (9 bits) to the right position and add it to d1
   ror.l	#1,d4			  ;
   or.l		d4,d1			  ;d1 holds now the start block!
   move.l	d1,d4			  ;now copy d1 to d4
   add.l	d3,d4			  ;add the blocks in d3 to transfer and see if we hit the boundary
   bcs		Quits			  ;overflow!>quit   
   cmp.w    #LBA48_ACCESS,mdu_lba(a3) ;is it a LBA48 drive, this should be able to handle 32bit addresses ;)?      
   beq		transfer		  ;everything is ok!
   and.l	#$F0000000,d4	  ; just the first 28 bits set?
   bne		Quits			  ; nope: Quit!
transfer
   move.l   a0,a5             ;Start address

   move.l   d1,d6             ;Start block

   move.l   d3,d7             ;# blocks

jooei                         ;a2 = #READOPE or #WRITEOPE

   bsr      doblocks          ;a5:startaddress, d6:startblock, d7:numberofblocks

Quits

   cmp.l    #0,d0

   bne      errcode

okcode

   movem.l  (sp)+,d1-d7/a0-a6

   rts                        ;EXIT RDWT.ASM

errcode

   bset.b   #1,$bfe001        ;Amiga power led off
   cmp.w    #ATAPI_DRV,mdu_drv_type(a3)
   beq      Quits
   cmp.w    #SATAPI_DRV,mdu_drv_type(a3)
   beq      Quits
   move.l   mdu_UnitNum(a3),d0;Reset drive - some drives may freeze at bad block

   lsl.b    #4,d0

   or.b     #$a0,d0

   WATABYTE d0,TF_DRIVE_HEAD

   DLY5US

   WATABYTE #8+SRST+nIEN,TF_DEVICE_CONTROL   ;no int., reset ;nIEN=2 SRST=4

   bsr      pause

   WATABYTE #8+nIEN,TF_DEVICE_CONTROL

   bsr      pause

   bclr.b   #1,$bfe001        ;Amiga power led on

   bsr      waitnotbusy1

   move.l   #1,d0

   bra      okcode





doblocks    ;d7=sectors>0 (A5=startaddress, d6=startblock)
;   cmp.l    #READOPE,a2 ;is it read ?
;   beq      gsfg
;   cmp.l    #WRITEOPE,a2
;   beq      gsfg
;   ;not read or write, return error
;   move.l   #NOREADWRITE,d0
;   rts
gsfg

   movem.l  d6-d7/a5,-(sp)

   bsr      dorwv                ;first read or write/format

   movem.l  (sp)+,d6-d7/a5

   cmp.l    #0,d0

   rts

dorwv

; do not make 256 sector read commands,

; because it might not work on some old drives.

domore

   move.l   d7,d5
   and.l    #$ff,d5

   bne      between1and16

   move.l   #$100,d5              ;Make 64 sectors


;   cmp.w    #LBA48_ACCESS,mdu_lba(a3)
;   beq		lba48
;   cmp.l    #$FF,d5
;   ble      between1and16
;   move.l   #$100,d5              ;Make 256 sectors
;   bra		between1and16
;lba48   
;   cmp.l	#$FFFF,d5
;   ble		between1and16
;   move.l   #$10000,d5              ;Make 256*256 sectors
between1and16
   move.l   d5,d4                ;d5 is number of sectors in this round
;   cmp.w    #LBA48_ACCESS,mdu_lba(a3)	
;   beq      maskd4lba48
    and.l    #$FF,d4              ; 256 sectors = 00
;   bra      maskd4done
;maskd4lba48
;   and.l    #$FFFF,d4            ; 256*256 sectors = 0000
maskd4done   
   cmp.l    #READOPE,a2

   beq      wasread

   bsr      writesectors         ;Format or Write

   cmp.l    #0,d0

   beq      sectoracok

   rts

wasread

   bsr      readsectors

   cmp.l    #0,d0

   beq      sectoracok

   rts

sectoracok

   add.l    d5,d6                ;next block number

   sub.l    d5,d7

   bne      domore

   move.l   #0,d0

   rts



;short power led blink

   PUBLIC   blink

blink

   move.l   d0,-(sp)
   move.l   #100000,d0 ; will be one more because of dbne
bl2

   bset.b   #1,$bfe001 ;Amiga power LED off by CIA pin.
   dbne		d0,bl2     ;CIA access cycles should be
                       ;slow on all AMIGAs, because the CIAs

                       ;are clocked at 700 kHz
   move.l   #100000,d0 ; will be one more because of dbne
bl3

   bclr.b   #1,$bfe001 ;Amiga power LED on
   dbne		d0,bl3
   move.l   (sp)+,d0

bl1

   rts



readsectors ;d4 is the number of sectors to read (between 1 and 64)

   bsr      waitreadytoacceptnewcommand

   cmp.l    #0,d0;error?

   bne      rsfl

   move.l   d6,d0                ;logical block number

   bsr      issueread
   sub.l    #1,d4				 ;for dbne
readnextblk

   DLY400NS                      ;wait for BSY go high (400 ns)

   RATABYTE TF_STATUS,d0         ;Also clears the disabled interrupt

   bsr      waitnotbusy1

   beq      rsfl

   bsr      waitdrq1

   beq      rsfl
   move.l	#512,d0
   RATADATAA5_D0_BYTES_64
   DLY5US                        ;wait DRQ go 0
   dbne     d4,readnextblk
   bsr      checkforerrors

   cmp.l    #0,d0

   bne      rsfl

   move.l   #0,d0                ;return value 0 means OK

   rts

rsfl                             ;some error in reading

   move.l   #1,d0

   rts



writesectors ;d4 is the number of sectors to write (between 1 and 64)

   bsr      waitreadytoacceptnewcommand

   cmp.l    #0,d0

   bne      wekfha

   move.l   d6,d0 ;logical block number

   bsr      issuewrite
   sub.l    #1,d4				 ;for dbne   
writenextoneblockki

   bsr      waitdrq1

   beq      wekfha

   bsr      writedata

   DLY5US                        ;BSY will go high within 5 microseconds after filling buffer

   RATABYTE TF_STATUS,d0         ;Also clears the disabled interrupt

   bsr      waitnotbusy1

   beq      wekfha

   bsr      checkforerrors

   cmp.l    #0,d0

   bne      wekfha
   dbne     d4,writenextoneblockki
   rts                           ;d0==0   ok

wekfha

   move.l   #1,d0

   rts                           ;some error in writing



checkforerrors

   bsr      waitnotbusy1

   beq      cfe1

   RATABYTE TF_ALTERNATE_STATUS,d0

   and.l    #DWF+ERR,d0

   rts

cfe1

   move.l   #111,d0

   rts



waitreadytoacceptnewcommand

   move.l   d1,-(sp)

   move.l   #LOOP,d1

   cmp.w    #TRUE,mdu_motor(a3)

   beq      fovc
   move.l   #LOOP2-1,d1
fovc
   ;subq.l   #1,d1
   dbeq     d1,wre1
   bsr      waitnotbusy1

   beq      wre1

   RATABYTE TF_STATUS,d0

   and.b    #BSY+DRDY+DWF+ERR,d0

   cmp.b    #DRDY,d0

   bne      oiuy

   move.l   #0,d0

   move.l   (sp)+,d1

   rts

oiuy

   DLY5US   ; make a processor speed independent minimum delay

   and.b    #DWF+ERR,d0

   beq      fovc

wre1

   move.l   (sp)+,d1

   move.l   #-865,d0

   rts


issueread
   cmp.w    #LBA28_ACCESS,mdu_lba(a3)
   bge      issueLBAread
   bsr      getCHS

issueCHSread

   move.l   d0,-(sp)

   move.l   mdu_UnitNum(a3),d0

   lsl.b    #4,d0

   or.b     d2,d0

   or.b     #$a0,d0

   WATABYTE d0,TF_DRIVE_HEAD

   move.l   (sp)+,d0

   WATABYTE d4,TF_SECTOR_COUNT

   WATABYTE d0,TF_CYLINDER_LOW

   WATABYTE d1,TF_CYLINDER_HIGH

   WATABYTE d3,TF_SECTOR_NUMBER

   WATABYTE #ATA_READ_SECTORS,TF_COMMAND

   rts



issueLBAread
   ;move.l	d0,d2		   ;load offset
   ;add.l	d4,d2		   ;add sector count
   ;cmp.l	#$0FFFFFFF,d2  ;check for lba28 boundary
   ;bgt		issueLBA48read ; higher transfers on lba28 drives should not reach this point!
doLBAread
   move.l   mdu_UnitNum(a3),d2

   lsl.b    #4,d2

   add.b    #L,d2

   WATABYTE d2,TF_DRIVE_HEAD           ;L=lba  lba bits 24..27

   WATABYTE d4,TF_SECTOR_COUNT

   WATABYTE d0,TF_SECTOR_NUMBER        ;lba bits 0..7

   lsr.l    #8,d0

   WATABYTE d0,TF_CYLINDER_LOW         ;lba bits 8..15

   lsr.l    #8,d0

   WATABYTE d0,TF_CYLINDER_HIGH        ;lba bits 16..23

   WATABYTE #ATA_READ_SECTORS,TF_COMMAND
   rts

issueLBA48read
   ;bsr blink
   ;bsr blink
   move.l   mdu_UnitNum(a3),d2
   lsl.b    #4,d2
   add.b    #L,d2
   sub.w	#$A0,d2                    ;read extended
   WATABYTE d2,TF_DRIVE_HEAD           ;L=lba  lba bits 24..27
   move.l	d4,d2
   lsr.l	#8,d2						   ;upper byte sector count
   WATABYTE d2,TF_SECTOR_COUNT
   move.l	d0,d2					   ;extract lbabits 24..31
   rol.l    #8,d2
   and.l    #$FF,d2
   WATABYTE d2,TF_SECTOR_NUMBER        ;lba bits 24..31
   WATABYTE #0,TF_CYLINDER_LOW         ;lba bits 32..39 nothing!
   WATABYTE #0,TF_CYLINDER_HIGH        ;lba bits 40..47 nothing!
   move.l	d4,d2
   and.l    #$FF,d2
   WATABYTE d2,TF_SECTOR_COUNT		   ;write low byte
   WATABYTE d0,TF_SECTOR_NUMBER        ;lba bits 0..7
   lsr.l    #8,d0
   WATABYTE d0,TF_CYLINDER_LOW         ;lba bits 8..15
   lsr.l    #8,d0
   WATABYTE d0,TF_CYLINDER_HIGH        ;lba bits 16..23
   WATABYTE #ATA_READ_SECTORS_EXT,TF_COMMAND
   rts



writedata

   move.l   #512,d0

   WATADATAA5_D0_BYTES_64

   rts



issuewrite
   cmp.w    #LBA28_ACCESS,mdu_lba(a3)
   bge      issueLBAwrite
   bsr      getCHS

issueCHSwrite

   move.l   d0,-(sp)

   move.l   mdu_UnitNum(a3),d0

   lsl.b    #4,d0

   or.b     d2,d0

   or.b     #$a0,d0

   WATABYTE d0,TF_DRIVE_HEAD

   move.l   (sp)+,d0

   WATABYTE d4,TF_SECTOR_COUNT

   WATABYTE d0,TF_CYLINDER_LOW

   WATABYTE d1,TF_CYLINDER_HIGH

   WATABYTE d3,TF_SECTOR_NUMBER

   WATABYTE #ATA_WRITE_SECTORS,TF_COMMAND

   rts



issueLBAwrite
   ;move.l	d0,d2		   ;load offset
   ;add.l	d4,d2		   ;add sector count
   ;cmp.l	#$0FFFFFFF,d2  ;check for lba28 boundary
   ;bgt		issueLBA48write ; higher transfers on lba28 drives should not reach this point!
   move.l   mdu_UnitNum(a3),d2

   lsl.b    #4,d2

   add.b    #L,d2

   WATABYTE d2,TF_DRIVE_HEAD           ;L=lba; lba bits 24..27

   WATABYTE d4,TF_SECTOR_COUNT

   WATABYTE d0,TF_SECTOR_NUMBER        ;LBA  bits  0..7

   lsr.l    #8,d0

   WATABYTE d0,TF_CYLINDER_LOW         ;LBA  bits  8..15

   lsr.l    #8,d0

   WATABYTE d0,TF_CYLINDER_HIGH        ;LBA  bits  16..23

   WATABYTE #ATA_WRITE_SECTORS,TF_COMMAND
   rts

issueLBA48write
   ;bsr blink
   move.l   mdu_UnitNum(a3),d2
   lsl.b    #4,d2
   add.b    #L,d2
   sub.w	#$A0,d2                    ;write extended
   WATABYTE d2,TF_DRIVE_HEAD           ;L=lba  lba bits 24..27
   move.l	d4,d2
   lsr.l	#8,d2						   ;upper byte sector count
   WATABYTE d2,TF_SECTOR_COUNT
   move.l	d0,d2					   ;extract lbabits 24..31
   rol.l    #8,d2
   and.l    #$FF,d2
   WATABYTE d2,TF_SECTOR_NUMBER        ;lba bits 24..31
   WATABYTE #0,TF_CYLINDER_LOW         ;lba bits 32..39 nothing!
   WATABYTE #0,TF_CYLINDER_HIGH        ;lba bits 40..47 nothing!
   move.l	d4,d2
   and.l    #$FF,d2
   WATABYTE d2,TF_SECTOR_COUNT		   ;write low byte
   WATABYTE d0,TF_SECTOR_NUMBER        ;lba bits 0..7
   lsr.l    #8,d0
   WATABYTE d0,TF_CYLINDER_LOW         ;lba bits 8..15
   lsr.l    #8,d0
   WATABYTE d0,TF_CYLINDER_HIGH        ;lba bits 16..23
   WATABYTE #ATA_WRITE_SECTORS_EXT,TF_COMMAND
   rts



getCHS      ;convert block number to Cylinder / Head / Sector numbers

   move.l   d0,d3             ;d0 = number of block (block numbers begin from 0)

   move.l   mdu_sectors_per_track(a3),d2

   divu     d2,d3

   swap.l   d3

   add.w    #1,d3             ;sector numbers begin at 1

   and.l    #$ff,d3           ;sector number byte

   move.l   mdu_sectors_per_track(a3),d2     ;d0 = number of block

   divu     d2,d0

   and.l    #$ffff,d0         ;16bit word

   move.l   mdu_heads(a3),d2

   divu     d2,d0             ;d0 = cyl

   move.l   d0,d2

   swap.l   d2

   and.l    #$f,d2            ;d2 = head

   move.l   d0,d1

   and.l    #$ff,d0           ;cylinder low

   lsr.l    #8,d1

   and.l    #$ff,d1           ;cylinder high

   rts



;perform safe switch to act_drv drive

   PUBLIC   SelectDrive

SelectDrive:

   movem.l  d0/d1,-(sp)
   move.l   act_Drive,d0
   cmp.l    mdu_UnitNum(a3),d0 ; check if this drive si the last selected one

   beq      sdr3              ; just return from subroutine

   

   bsr      waitnotbusy1

   beq      sdr1

   bsr      waitnotdrq1

   beq      sdr1

   moveq    #0,d0

   RATABYTE TF_DRIVE_HEAD,d0

   and.b    #$10,d0

   lsr.b    #4,d0

   cmp.l    mdu_UnitNum(a3),d0

   beq      sdr3

   move.l   mdu_UnitNum(a3),d0

   lsl.b    #4,d0

   or.b     #$a0,d0

   WATABYTE d0,TF_DRIVE_HEAD

   DLY400NS

   move.l   d0,act_Drive       ; store new drive id

;  cmp.l    #ATA_DRV,act_drv_type

;  bne      sdr2

   bra      sdr2

   move.l   #LOOP,d0

   cmp.w    #TRUE,mdu_motor(a3)

   beq      sdr4

   move.l   #LOOP2,d0

sdr4

   subq.l   #1,d0

   beq      sdr1

   move.b   TF_STATUS,d1

   and.b    #BSY+DRDY+SKC,d1

   cmp.b    #DRDY+SKC,d1

   beq      sdr3

   DLY5US

   bra      sdr4

sdr2

   bsr      waitnotbusy1

   beq      sdr1

   bsr      waitnotdrq1

   beq      sdr1

sdr3

   moveq.l  #1,d0                ; clear zero flag

sdr1

   movem.l  (sp)+,d0/d1

   rts



act_Drive   dc.l  $FFFFFFFF      ; actual selected drive

rs_cmd      dc.w  $0300,0,$2000,0,0,0

act_Status  dc.b  0

act_Flags   dc.b  0

act_Actual  dc.l  0

sense_data  ds.b  20             ; sense data of last packet command





; a0 = scsi_Data, d0 = scsi_Length, a1(not a2?;ml) = scsi_Command, a6 = SCSICmd

; d2 = unit number, a3 = io_unit



   Public SCSIDirectCmd

SCSIDirectCmd

   movem.l  a0-a6/d0-d6,-(sp)

   bsr      SelectDrive

   beq      sdc1

   move.l   a0,a5

sdc3

   move.l   d0,d1

   and.l    #$FFFF0000,d0           ;no more than 64KB at once now :-(

   beq      sdc5                    ;maybe will be fixed later

   move.b   #$BB,scsi_Status(a6)    ;scsi_status = BBh -> length was >64KB

   move.b   #1,IO_ERROR(a1)

   clr.w    scsi_SenseActual(a6)

   bra      sdc2

sdc5

   move.w   scsi_CmdLength(a6),d3

   move.b   scsi_Flags(a6),act_Flags

   bsr      Packet

   move.l   act_Actual,d1

   add.l    d1,scsi_Actual(a6)

   move.b   act_Status,scsi_Status(a6)

   cmp.b    #$50,act_Status

   beq      sdc2

   clr.w    scsi_SenseActual(a6)

   move.b   #1,IO_ERROR(a1)

   cmp.b    #$FF,act_Status         ;status = FFh means timeout

   bne      sdc1

   WATABYTE #$08,TF_COMMAND         ;then reset atapi device

   DLY400NS

   bsr      waitnotbusy1

   bra      sdc2

sdc1                                ;read sense data if error

   lea      sense_data,a5

   btst.b   #SCSIB_AUTOSENSE,scsi_Flags(a6)

   beq      sdc6

   move.w   scsi_SenseLength(a6),d1

   bra      sdc7

sdc6

   move.w   #20,d1

sdc7

   move.w   #12,d3

   lea      rs_cmd,a2

   move.b   #SCSIF_READ,act_Flags

   bsr      Packet                  ;do packet request sense

   btst.b   #SCSIB_AUTOSENSE,scsi_Flags(a6)

   beq      sdc9

   move.l   scsi_SenseData(a6),a5

   lea      sense_data,a2

   move.l   act_Actual,d0

   move.w   d0,scsi_SenseActual(a6)

   beq      sdc2

   subq.w   #1,d0

sdc8

   move.b   (a2)+,(a5)+

   dbra     d0,sdc8

sdc9

   lea      sense_data,a2

   move.b   2(a2),d0

   and.b    #$F,d0

   clr.l    mdu_no_disk(a3)

   cmp.b    #2,d0

   bne      sdc91

   move.l   #1,mdu_no_disk(a3)      ;medium is not present

sdc91

   cmp.b    #6,d0

   bne      sdc2

   add.l    #1,mdu_change_cnt(a3)

sdc2

   movem.l  (sp)+,a0-a6/d0-d6

   rts





act_cmd     ds.w  8



; a0 = scsi_Data, a2 = scsi_Command, a3 = io_unit, a6 = SCSICmd

; d1 = scsi_Length, d2 = unit number, d3 = cmd_length



;send packet to atapi drive and read/write data if needed

Packet

   movem.l  a0-a4/d0-d6,-(sp)

   clr.l    act_Actual

   DLY400NS

   bsr      waitnotbusy1               ;wait till drive is not ready

   beq      pretec

   bsr      waitnotdrq1

   beq      pretec

   lsl.b    #4,d2

   or.b     #$a0,d2

   WATABYTE d2,TF_DRIVE_HEAD           ;set task file registers

   DLY400NS

   WATABYTE #0,TF_SECTOR_COUNT

   WATABYTE d1,TF_CYLINDER_LOW

   lsr.w    #8,d1

   WATABYTE d1,TF_CYLINDER_HIGH

   WATABYTE #nIEN+8,TF_DEVICE_CONTROL

   bsr      waitnotbusy1

   beq      pretec

   WATABYTE #ATA_PACKET,TF_COMMAND     ;send packet command

   DLY400NS

   bsr      waitdrq1

   beq      pretec

   RATABYTE TF_STATUS,d0

   and.b    #ERR,d0

   bne      pa_err

   lea      act_cmd,a1                 ;prepare packet

   clr.l    (a1)

   clr.l    4(a1)

   clr.l    8(a1)

   clr.l    12(a1)

   lsr.w    #1,d3

pa2

   move.w   (a2)+,(a1)+

   subq.w   #1,d3

   bne      pa2

   lea      act_cmd,a1

   WATAWORD (a1)+,TF_DATA              ;write packet to drive

   WATAWORD (a1)+,TF_DATA

   WATAWORD (a1)+,TF_DATA

   WATAWORD (a1)+,TF_DATA

   WATAWORD (a1)+,TF_DATA

   WATAWORD (a1)+,TF_DATA

pa3

   move.l   #LOOP3,d2                  ;wait to packet execution result

pa4

   subq.l   #1,d2

   beq      pretec

   bsr      pause

   RATABYTE TF_ALTERNATE_STATUS,d0

   btst     #7,d0

   bne      pa4

   RATABYTE TF_SECTOR_COUNT,d1

   and.b    #3,d1

   btst.b   #3,d0

   bne      pa5

   cmp.b    #3,d1

   beq      pa6

   DLY5US

   bra      pa4

pa5

   btst     #0,d1

   bne      pa4

pa6

   RATABYTE TF_ALTERNATE_STATUS,d0

   btst     #7,d0

   bne      pa4

   RATABYTE TF_STATUS,d0

   and.b    #DRQ,d0

   beq      pa10                       ;skip if no data

   moveq    #0,d3

   RATABYTE TF_CYLINDER_HIGH,d3        ;else read data length

   lsl.w    #8,d3

   RATABYTE TF_CYLINDER_LOW,d3

   move.l   d3,act_Actual

   btst     #0,d3

   beq      pa7

   addq.l   #1,d3 ;add one, if odd length

pa7

   tst.l    d3                         ;test data length and

   beq      pa_zero                    ;data buffer address

   cmp.l    #0,a5

   beq      pa_err

   move.l   a5,d0

   and.l    #1,d0

   bne      pa_err

   btst.b   #SCSIB_READ_WRITE,act_Flags   ;read or write required?

   beq      pa9

pa8                                    ; read data from drive

   move.l   d3,d0

   and.l    #63,d0                     ; is the data 64byte alligned?

   beq      pa8a

   move.l   d3,d0                      ; restore d0

   RATADATAA5_D0_BYTES

   bra      pa3

pa8a 

   move.l   d3,d0                      ; restore d0

   RATADATAA5_D0_BYTES_64

   bra      pa3

pa9                                    ; write data to drive

   move.l   d3,d0

   and.l    #63,d0                     ; is the data 64byte alligned?

   beq         pa9a

   move.l   d3,d0                      ; restore d0

   WATADATAA5_D0_BYTES

   bra      pa3

pa9a

   move.l   d3,d0                      ; restore d0

   WATADATAA5_D0_BYTES_64

   bra      pa3

;

pa10

   bsr      waitnotbusy1

   beq      pretec

   RATABYTE TF_STATUS,d1

   move.b   d1,act_Status

   and.b    #ERR,d1                    ;test, if error occured

   bne      pa_err

pa11

;  bsr      waitnotbusy1

   movem.l  (sp)+,a0-a4/d0-d6

   rts                                 ;return from Packet



pretec                                 ;if timeout, return status=FFh

   move.b   #$FF,act_Status

   bra      pa11

pa_err                                 ;if error, return actual status

   RATABYTE TF_STATUS,act_Status

   bra      pa11



pa_zero                                ;if zero length occured, return AAh

   move.b   #$AA,act_Status

   bra      pa11





waitdrq1

   movem.l  d0/d1,-(sp)

   move.l   #LOOP,d1

   cmp.w    #TRUE,mdu_motor(a3)

   beq      wd

   move.l   #LOOP2,d1

wd DLY3US

   subq.l   #1,d1

   beq      wd1

   RATABYTE TF_ALTERNATE_STATUS,d0

   and.b    #BSY+DRQ,d0

   cmp.b    #DRQ,d0

   bne      wd

wd1

   tst.l    d1

   movem.l  (sp)+,d0/d1

   rts





waitdrdy1

   movem.l  d0/d1,-(sp)

   move.l   #LOOP,d1

   cmp.w    #TRUE,mdu_motor(a3)

   beq      dr

   move.l   #LOOP2,d1

dr DLY3US

   subq.l   #1,d1

   beq      dr1

   RATABYTE TF_ALTERNATE_STATUS,d0

   and.b    #BSY+DRDY,d0

   cmp.b    #DRDY,d0

   bne      dr

dr1

   tst.l    d1

   movem.l  (sp)+,d0/d1

   rts



   PUBLIC waitnotbusy1

waitnotbusy1

   movem.l  d0/d1,-(sp)

   move.l   #LOOP,d1

   cmp.w    #TRUE,mdu_motor(a3)

   beq      wn

   move.l   #LOOP2,d1

wn DLY3US

   subq.l   #1,d1

   beq      wn1

   RATABYTE TF_ALTERNATE_STATUS,d0

   and.b    #BSY,d0

   bne      wn

wn1

   tst.l    d1

   movem.l  (sp)+,d0/d1

   rts



   PUBLIC waitnotdrq1

waitnotdrq1

   movem.l  d0/d1,-(sp)

   move.l   #LOOP,d1

   cmp.w    #TRUE,mdu_motor(a3)

   beq      wq

   move.l   #LOOP2,d1

wq DLY3US

   subq.l   #1,d1

   beq      wq1

   RATABYTE TF_ALTERNATE_STATUS,d0

   and.b    #DRQ,d0

   bne      wq

wq1

   tst.l    d1

   movem.l  (sp)+,d0/d1

   rts



waitbusy1

   movem.l  d0/d1,-(sp)

   move.l   #LOOP,d1

   cmp.w    #TRUE,mdu_motor(a3)

   beq      wb

   move.l   #LOOP2,d1

wb DLY3US

   subq.l   #1,d1

   beq      wb1

   RATABYTE TF_ALTERNATE_STATUS,d0

   and.b    #BSY,d0

   beq      wb

wb1

   tst.l    d1

   movem.l  (sp)+,d0/d1

   rts



   PUBLIC   pause

pause

   move.l   d0,-(sp)

   move.l   #500,d0

pu1

   DLY5US

   dbra     d0,pu1

   move.l   (sp)+,d0

   rts



   END




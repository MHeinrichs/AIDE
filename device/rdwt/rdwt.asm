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
   xdef READOPE
   xdef WRITEOPE

TRUE  equ   1
FALSE equ   0


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
   include "/debug/debug-wrapper.i"
   XLIB AllocMem
   
   XLIB FreeMem

;some of the 32-bit longword error codes rdwt.asm returns:
BADLENGTH   equ "PELI"
BADUNIT     equ "MELA"
BADOFFSET   equ "TISI"
NOREADWRITE equ "PILU"

		IFND	DEBUG_DETAIL
DEBUG_DETAIL	SET	0	;Detail level of debugging.  Zero for none.
		ENDC

;macro INITATAINTERFACE *needs* to be executed once before this routine is ever called
;That might happen in dev.asm.
;Both drives on the ide-cable are initialised by separate calls to this
;routine.
   cnop  0,4

   PUBLIC   InitDrive
InitDrive   ;a3 = unitptr
   movem.l  d1/d2/d3/d4/a0/a1/a5,-(sp)	 
;get memory
   moveq   #0,d4
   bsr      SelectDrive	
	 bne			wfc1														 ;no drive present!
   move.l   ABSEXECBASE,a0
   move.l   #512,d0       ; we want 512 bytes for a buffer   
   move.l   #MEMF_ANY!MEMF_CLEAR,d1 ;Preferable Fast mem, cleared   
   LINKSYS  AllocMem,a0
   tst.l    d0             ; memory ok?
   beq      wfc1
   move.l   d0,d4      ;save pointer
   RATABYTE TF_STATUS,d0   ;clear drive interrupt line
   cmp.w    #TRUE,mdu_auto(a3)
   bne      notauto
;get drive parameters
   move.w   #CHS_ACCESS,mdu_lba(a3)               ;presumption
   WATABYTE #ATA_IDENTIFY_DRIVE,TF_COMMAND   ;get drive data
   WAITNOTBSY D1,D2
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
   WAITDRQ  D1,D2
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
   move.l   d4,a5                  ;get identify data
   move.l	#127,d0
   move.l   #TF_DATA,a0
kr3data
   move.l   (a0),(a5)+
   dbra     d0,kr3data
   move.l   d4,a5                  

   ;IF Y=0 SWAP BYTES IN WORD BECAUSE AMIGA DATA0..7 IS DRIVE DATA8.15
   IFEQ Y,0
      move.l   A5,A0       ;buffer start
      movem.l  d1-d2,-(sp) ;d1-d2 to stack
      move.w   #256-1,d0   ;move.word because dbra uses 16bits of register
lk    move.w   (a0),d1
      move.w   d1,d2
      lsl.w    #8,d1
      lsr.w    #8,d2
      and.w    #$ff,d2
      or.w     d2,d1
      move.w   d1,(a0)+
      dbra     d0,lk
      movem.l  (sp)+,d1-d2 ;restore d1-d2
   ENDC

   move.l   d4,a5   ;copy serial number to internal info buffer
   add.l    #20,a5			
   lea      mdu_ser_num(a3),a0
   move.b   #10,d0				
ckl1
   move.w   (a5)+,(a0)+
   subq.b	#1,d0
   bne		ckl1
   move.b   #0,(a0)				;null termination of string
   move.l   d4,a5
   add.l    #46,a5
   lea      mdu_firm_rev(a3),a0  ;copy firm. revision to int. buffer 8byte = 2 longwords!
   move.l   (a5)+,(a0)+
   move.l   (a5)+,(a0)+
   move.b   #0,(a0)              ;null termination of string
   move.l   d4,a5
   add.l    #54,a5
   lea      mdu_model_num(a3),a0 ;copy model number to int. buffer 40 bytes = 10 longwords
   move.b   #10,d0				
ckl3
   move.l   (a5)+,(a0)+
   subq.b	#1,d0
   bne   	ckl3
   move.b   #0,(a0)				;null termination of string
   lea      mdu_model_num(a3),a0
   moveq    #0,d0
   move.w   mdu_drv_type(a3),d0
   cmp.w    #ATA_DRV,mdu_drv_type(a3)
   beq      noeritd
   cmp.w    #SATA_DRV,mdu_drv_type(a3)
   beq      noeritd
   WATABYTE #0,TF_FEATURES       ;for atapi cdrom with
   bra      kr2
noeritd     ; ATA disk /SATA disk
   move.l   d4,a5
   moveq    #0,d0                ; clear upper 16 bit!
   move.w   1*2(a5),d0           ;Word 1 Number of logical cylinders
   move.l   d0,mdu_cylinders(a3) ;store to internal buffer
   move.w   6*2(a5),d0           ;Word 6 Default Translation sectors
   ;and.l    #$ffff,d0
   move.l   d0,mdu_sectors_per_track(a3)  ;store to internal buffer
   move.w   3*2(a5),d0           ;Word 3 Default Translation Mode number of heads
   ;and.l    #$ffff,d0
   move.l   d0,mdu_heads(a3)     ;store to internal buffer
   move.b	#1,mdu_SectorBuffer(a3)
   move.w   47*2(A5),d0          ;WRITE/READ Multiple capabilities
   cmp.l	#1,d0				 ;multi sector supportet?
   bra		multiple_sector_dis  ; not set
   move.b	d0,mdu_SectorBuffer(a3)
multiple_sector_dis
   move.w   49*2(A5),d0          ;Word 49 Capabilities
   and.w    #$200,d0             ;Bit 9 1=LBA Supported
   beq      nolba
   move.w   61*2(a5),d0          ;Words 60-61 # of user addressable sectors (LBA)
   swap   d0
   or.w     60*2(a5),d0
   move.l   d0,mdu_numlba(a3)    ;store to internal buffer
   beq      nolba                ;propably no lba support if no lba sectors
   move.w   #LBA28_ACCESS,mdu_lba(a3)    ;store to internal buffer
   ;move.w   83*2(A5),d0          ;Word 83 Capabilities * LBA48 support check
   ;and.w    #$400,d0             ;Bit 10 1=LBA48 Supported
   ;bra		endauto				 ; saty at LBA28
   ;move.l   100*2(a5),d0         ;3rd word LBA48
   ;swap   d0
   ;or.w     101*2(a5),d0		 ;4th word LBA48
   ;beq		endauto				 ;LBA48 supported but <8GB: stay at LBA28!
   ;move.l   d0,mdu_numlba48(a3)  ;store to internal buffer
   ;;now the lower 32 bits
   ;move.l   102*2(a5),d0         ;1st word LBA48
   ;swap   d0
   ;or.w     103*2(a5),d0		 ;2nd word LBA48
   ;beq		endauto				 ;LBA48 supported but <8GB: stay at LBA28!
   ;move.l   d0,mdu_numlba(a3)    ;store to internal buffer
   ;move.w   #LBA48_ACCESS,mdu_lba(a3)    ;store to internal buffer	   
   bra      endauto
nolba                            ;Then its CHS
   move.w   #CHS_ACCESS,mdu_lba(a3)   ;store to internal buffer
   ;Conner Peripherals CP3044 lies about its default translation mode
   lea      CP3044txt(pc),a0
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
endauto
notauto
   RATABYTE TF_STATUS,d0               ;clear interrupt line
   cmp.w    #ATA_DRV,mdu_drv_type(a3)
   beq      setupata
   cmp.w    #SATA_DRV,mdu_drv_type(a3)
   beq      setupata
   bra		kr2
setupata
;   cmp.l    #16514064,mdu_numlba(a3)		; devices with less blocks should support the following chs translation
;   bge			kr2
;   move.l   mdu_sectors_per_track(a3),d0  ;send to drive which CHS translation
;   WATABYTE d0,TF_SECTOR_COUNT         ;to use - important to drives with
;   move.l   mdu_heads(a3),d0           ;LBA support
;   subq.b   #1,d0
;   or.b     #$a0,d0
;   tst.l    mdu_UnitNum(a3)
;   beq      pis1
;   bset   #4,d0
;pis1
;   WATABYTE d0,TF_DRIVE_HEAD
;   DLY400NS
;   bsr      waitreadytoacceptnewcommand
;   WATABYTE #ATA_INITIALIZE_DRIVE_PARAMETERS,TF_COMMAND  ;get drive data
;   WAITNOTBSY d1,d2
kr2
   move.l   d4,d1 ;is there a pointer in buffer?
   tst.l    d1
   beq kr21
   
   move.l  ABSEXECBASE,a0
   move.l   d4,a1
   move.l   #512,d0
   LINKSYS FreeMem,a0
   
kr21:
   move.l   #0,d4
   movem.l  (sp)+,d1/d2/d3/d4/a0/a1/a5
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
   move.l   d0,d3			  ;save length to somewhere save
   bsr      SelectDrive
   bne      errcode
   move.l   #BADUNIT,d0       ;Check that unit is 0 or 1
   cmp.l    #1,d2		     
   bgt      Quits
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
   ;cmp.w    #LBA48_ACCESS,mdu_lba(a3) ;is it a LBA48 drive, this should be able to handle 32bit addresses ;)?      
   ;beq		transfer		  ;everything is ok!
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
   beq      errcodeend
   cmp.w    #SATAPI_DRV,mdu_drv_type(a3)
   beq      errcodeend
   ;Reset drive - some drives may freeze at bad block but a reset resets the whole ide-chain!
   bsr      ResetIDE
errcodeend
   bclr.b   #1,$bfe001        ;Amiga power led on
   ;WAITNOTBSY D0,D1
   move.l   #1,d0
   bra      okcode


doblocks    ;d7=sectors>0 (A5=startaddress, d6=startblock)
gsfg
   ;movem.l  d4-d7/a5,-(sp)
domore
   move.l   d7,d5
   and.l    #$ff,d5
   bne      between1and16
   move.l   #$100,d5              ;Make 64 sectors

between1and16
   move.l   d5,d4                ;d5 is number of sectors in this round
    and.l    #$FF,d4              ; 256 sectors = 00
maskd4done   
   bsr      waitreadytoacceptnewcommand
   cmp.l    #READOPE,a2
   beq      wasread
   bsr      writesectors         ;Format or Write
   cmp.l    #0,d0                ;Error?
   beq      sectoracok
   bra      doblocksend 
wasread
   bsr      readsectors
   cmp.l    #0,d0                ;error?
   bne      doblocksend
sectoracok
   add.l    d5,d6                ;next block number
   sub.l    d5,d7
   bne      domore
   move.l   #0,d0

doblocksend:
   ;movem.l  (sp)+,d4-d7/a5
   cmp.l    #0,d0
   rts

readsectors ;d4 is the number of sectors to read (between 1 and 64)
   cmp.l    #0,d0;error?
   bne      rsfl
   move.l   d6,d0                ;logical block number
   ;bsr      issueread

issueread
   cmp.w    #LBA28_ACCESS,mdu_lba(a3)
   bge      issueLBAread

issueCHSread
   bsr      getCHS
   move.l   mdu_UnitNum(a3),d0
   lsl.b    #4,d0
   or.b     d2,d0
   or.b     #$a0,d0
   WATABYTE d0,TF_DRIVE_HEAD
   move.l   d6,d0
   WATABYTE d4,TF_SECTOR_COUNT
   WATABYTE d0,TF_CYLINDER_LOW
   WATABYTE d1,TF_CYLINDER_HIGH
   WATABYTE d3,TF_SECTOR_NUMBER
   bra      sendreadcommand

issueLBAread
   move.l   mdu_UnitNum(a3),d2
   lsl.b    #4,d2
   add.b    #L,d2
   rol.l    #8,d0                      ;put upper 4 bits of d0 into lower 4bit of d2
   and.l    #$0000000f,d0               
   or.l     d0,d2
   move.l   d6,d0                      ;restore d0   
   WATABYTE d2,TF_DRIVE_HEAD           ;L=lba  lba bits 24..27
   WATABYTE d4,TF_SECTOR_COUNT
   WATABYTE d0,TF_SECTOR_NUMBER        ;lba bits 0..7
   lsr.l    #8,d0
   WATABYTE d0,TF_CYLINDER_LOW         ;lba bits 8..15
   lsr.l    #8,d0
   WATABYTE d0,TF_CYLINDER_HIGH        ;lba bits 16..23

sendreadcommand
   WATABYTE #ATA_READ_SECTORS,TF_COMMAND
   
   sub.l    #1,d4				 ;for dbne
readnextblk
   ;DLY400NS                      ;wait for BSY go high (400 ns)
   RATABYTE TF_STATUS,d0         ;Also clears the disabled interrupt
   WAITNOTBSY D2,D3
   beq      rsfl
   WAITDRQ	D2,D3
   beq      rsfl
;   move.l	#127,d0
;   move.l   #TF_DATA,a0
;readnextblkdata
;   move.l   (a0),(a5)+
;   dbra     d0,readnextblkdata
   RATADATAA5_512_BYTES
   DLY5US                        ;wait DRQ go 0
   dbne     d4,readnextblk
   ;check for errors
   WAITNOTBSY D2,D3
   beq      rsfl
   RATABYTE TF_ALTERNATE_STATUS,d0
   and.l    #DWF+ERR,d0
   cmp.l    #0,d0
   bne      rsfl
   move.l   #0,d0                ;return value 0 means OK
   rts
rsfl                             ;some error in reading
   move.l   #1,d0
   rts

writesectors ;d4 is the number of sectors to write (between 1 and 64)
   cmp.l    #0,d0
   bne      wekfha
   move.l   d6,d0 ;logical block number

issuewrite
   cmp.w    #LBA28_ACCESS,mdu_lba(a3)
   bge      issueLBAwrite

issueCHSwrite
   bsr      getCHS
   move.l   mdu_UnitNum(a3),d0
   lsl.b    #4,d0
   or.b     d2,d0
   or.b     #$a0,d0
   WATABYTE d0,TF_DRIVE_HEAD
   move.l   d6,d0
   WATABYTE d4,TF_SECTOR_COUNT
   WATABYTE d0,TF_CYLINDER_LOW
   WATABYTE d1,TF_CYLINDER_HIGH
   WATABYTE d3,TF_SECTOR_NUMBER
   bra      sendwritecommand

issueLBAwrite
   move.l   mdu_UnitNum(a3),d2
   lsl.b    #4,d2
   add.b    #L,d2
   rol.l    #8,d0                      ;put upper 4 bits of d0 into lower 4bit of d2
   and.l    #$0000000f,d0               
   or.l     d0,d2
   move.l   d6,d0                      ;restore d0
   WATABYTE d2,TF_DRIVE_HEAD           ;L=lba; lba bits 24..27
   WATABYTE d4,TF_SECTOR_COUNT
   WATABYTE d0,TF_SECTOR_NUMBER        ;LBA  bits  0..7
   lsr.l    #8,d0
   WATABYTE d0,TF_CYLINDER_LOW         ;LBA  bits  8..15
   lsr.l    #8,d0
   WATABYTE d0,TF_CYLINDER_HIGH        ;LBA  bits  16..23

sendwritecommand:
   WATABYTE #ATA_WRITE_SECTORS,TF_COMMAND

   sub.l    #1,d4				 ;for dbne   
writenextoneblockki
   WAITDRQ	D2,D3
   beq      wekfha
;   move.l   #127,d0
;   move.l   #TF_DATA,a0
;writenextoneblockdata
;   move.l   (a5)+,(a0)
;   dbra     d0,writenextoneblockdata
   WATADATAA5_512_BYTES  
   DLY5US                        ;BSY will go high within 5 microseconds after filling buffer
   RATABYTE TF_STATUS,d0         ;Also clears the disabled interrupt
   ;check for errors
   WAITNOTBSY D2,D3
   beq      wekfha
   RATABYTE TF_ALTERNATE_STATUS,d0
   and.l    #DWF+ERR,d0
   cmp.l    #0,d0
   bne      wekfha
   dbne     d4,writenextoneblockki
   rts                           ;d0==0   ok
wekfha
   move.l   #1,d0
   rts                           ;some error in writing
   
getCHS      ;convert block number to Cylinder / Head / Sector numbers
   move.l   d0,d3             ;d0 = number of block (block numbers begin from 0)
   move.l   mdu_sectors_per_track(a3),d2
   divu     d2,d3
   swap     d3
   add.w    #1,d3             ;sector numbers begin at 1
   and.l    #$ff,d3           ;sector number byte
   move.l   mdu_sectors_per_track(a3),d2     ;d0 = number of block
   divu     d2,d0
   and.l    #$ffff,d0         ;16bit word
   move.l   mdu_heads(a3),d2
   divu     d2,d0             ;d0 = cyl
   move.l   d0,d2
   swap     d2
   and.l    #$f,d2            ;d2 = head
   move.l   d0,d1
   and.l    #$ff,d0           ;cylinder low
   lsr.l    #8,d1
   and.l    #$ff,d1           ;cylinder high
   rts

waitreadytoacceptnewcommand
   movem.l  d1-d2,-(sp)
   move.l   #LOOP,d1
;   cmp.w    #TRUE,mdu_motor(a3)
;   beq      fovc
;   move.l   #LOOP2,d1
fovc
   WAITNOTBSY D0,D2
   beq      wre1
   RATABYTE TF_STATUS,d0
   and.b    #BSY+DRDY+DWF+ERR,d0
   cmp.b    #DRDY,d0
   bne      oiuy
   move.l   #0,d0
   movem.l  (sp)+,d1-d2
   rts
oiuy
   DLY5US   ; make a processor speed independent minimum delay
   and.b    #DWF+ERR,d0
   dbne     d1,fovc
wre1
   movem.l  (sp)+,d1-d2
   move.l   #-865,d0
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


   Public ResetIDE
;SoftwareReset IDE-BUS
ResetIDE
	movem.l  d0,-(sp)
	WATABYTE #8+nIEN+SRST,TF_DEVICE_CONTROL ;assert reset and INTERRUPT
	moveq.l	 #16,d0 ;wait 
rstwait1:
	DLY5US
	dbne		d0,rstwait1
	;release reset
	WATABYTE #8+nIEN,TF_DEVICE_CONTROL ;assert reset and INTERRUPT
	moveq.l	 #120,d0 ;wait 200ms
rstwait2:
	tst.b    $bfe301 ;slow CIA access cycle takes 12-20 7MHz clocks: 1.7us - 2.8us
	dbne		d0,rstwait2
	movem.l  (sp)+,d0
  rts
	


;perform safe switch to act_drv drive
   PUBLIC   SelectDrive
SelectDrive:
   ;movem.l  a0,-(sp)
   ;move.l   mdu_Device(a3),a0
   ;move.l   md_act_Drive(a0),d0
   ;cmp.l    mdu_UnitNum(a3),d0 ; check if this drive si the last selected one
   ;beq      sdr4              ; just successfully return from subroutine
   move.l   mdu_UnitNum(a3),d0
   ;move.l   d0,md_act_Drive(a0)
   lsl.b    #4,d0
   or.b     #$a0,d0
   WATABYTE d0,TF_DRIVE_HEAD
   DLY400NS ;Other sources suggest 5 times TF_STATUS read instead a 400ns wait
   RATABYTE	TF_STATUS,d0				; clear interrupt line
   ;check if it worked: write something to the sector count and read it back
   WATABYTE  #$5A,TF_SECTOR_NUMBER
   RATABYTE	TF_SECTOR_NUMBER,d0				
   cmp.b     #$5A,d0
   beq      sdr4   
   move.l   #1,d0                ; clear zero flag
sdr4
   ;movem.l  (sp)+,a0
   rts

   cnop  0,4
CP3044txt   dc.b  'CP3044 ',0    ; 8 bytes = 2 longwords ->alligned!
   cnop  0,4
   
; a0 = scsi_Data, d0 = scsi_Length, a2 = scsi_Command, a6 = SCSICmd
; d2 = unit number, a3 = io_unit

   Public SCSIDirectCmd
SCSIDirectCmd
   movem.l  a0-a6/d0-d6,-(sp)
   move.l   d0,d1 ;save d0 somewhere save
   bsr      SelectDrive
   bne      sdc1
   move.l   a0,a5
sdc3
   and.l    #$FFFF0000,d0           ;no more than 64KB at once now :-(
   beq      sdc5                    ;maybe will be fixed later
   move.b   #$BB,scsi_Status(a6)    ;scsi_status = BBh -> length was >64KB
   move.b   #1,IO_ERROR(a1)
   clr.w    scsi_SenseActual(a6)
   bra      sdc2
sdc5
   move.w   scsi_CmdLength(a6),d3
   move.b   scsi_Flags(a6),mdu_act_Flags(a3)
   bsr      Packet
   move.l   mdu_act_Actual(a3),d1
   add.l    d1,scsi_Actual(a6)
   move.b   mdu_act_Status(a3),scsi_Status(a6)
   cmp.b    #$50,mdu_act_Status(a3)
   beq      sdc2
   clr.w    scsi_SenseActual(a6)
   move.b   #1,IO_ERROR(a1)
   cmp.b    #$FF,mdu_act_Status(a3)         ;status = FFh means timeout
   bne      sdc1
   WATABYTE #$08,TF_COMMAND         ;then reset atapi device
   DLY400NS
   WAITNOTBSY d1,d2
   bra      sdc2
sdc1                                ;read sense data if error
   lea      mdu_sense_data(a3),a5
   btst.b   #SCSIB_AUTOSENSE,scsi_Flags(a6)
   beq      sdc6
   move.w   scsi_SenseLength(a6),d1
   bra      sdc7
sdc6
   move.w   #20,d1
sdc7
   move.w   #12,d3
   lea      mdu_rs_cmd(a3),a2
   move.b   #SCSIF_READ,mdu_act_Flags(a3)
   bsr      Packet                  ;do packet request sense
   btst.b   #SCSIB_AUTOSENSE,scsi_Flags(a6)
   beq      sdc9
   move.l   scsi_SenseData(a6),a5
   lea      mdu_sense_data(a3),a2
   move.l   mdu_act_Actual(a3),d0
   move.w   d0,scsi_SenseActual(a6)
   beq      sdc2
   subq.w   #1,d0
sdc8
   move.b   (a2)+,(a5)+
   dbra     d0,sdc8
sdc9
   lea      mdu_sense_data(a3),a2
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

; a0 = scsi_Data, a2 = scsi_Command, a3 = io_unit, a6 = SCSICmd
; d1 = scsi_Length, d2 = unit number, d3 = cmd_length

;send packet to atapi drive and read/write data if needed
Packet
   movem.l  a0-a4/d0-d6,-(sp)
   clr.l    mdu_act_Actual(a3)
   DLY400NS
   WAITNOTBSY D0,D6               ;wait till drive is not ready
   beq      pretec
   WAITNOTDRQ D0,D6
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
   WAITNOTBSY D0,D6
   beq      pretec
   WATABYTE #ATA_PACKET,TF_COMMAND     ;send packet command
   DLY400NS
   WAITDRQ  D0,D6
   beq      pretec
   RATABYTE TF_STATUS,d0
   and.b    #ERR,d0
   bne      pa_err
   lea      mdu_act_cmd(a3),a1                 ;prepare packet
   clr.l    (a1)
   clr.l    4(a1)
   clr.l    8(a1)
   clr.l    12(a1)
   lsr.w    #1,d3
pa2
   move.w   (a2)+,(a1)+
   subq.w   #1,d3
   bne      pa2
   lea      mdu_act_cmd(a3),a1
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
   btst     #3,d0
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
   move.l   d3,mdu_act_Actual(a3)
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
   btst.b   #SCSIB_READ_WRITE,mdu_act_Flags(a3)   ;read or write required?
   beq      pa9
pa8              
   ; read data from drive
   move.l   d3,d0
   btst     #1,d0											; is the data long alligned?                      
   ;and.l    #63,d0                     ; is the data 64byte alligned?
   beq      pa8a
   ;move.l   d3,d0                      ; restore d0
   RATADATAA5_D0_BYTES
   bra      pa3
pa8a 
   ;move.l   d3,d0                      ; restore d0
   RATADATAA5_D0_BYTES_LONG
   ;RATADATAA5_D0_BYTES_64
   bra      pa3
pa9                                    ; write data to drive
   move.l   d3,d0
   btst     #1,d0											; is the data long alligned?                      
   ;and.l    #63,d0                     ; is the data 64byte alligned?
   beq         pa9a
   ;move.l   d3,d0                      ; restore d0
   WATADATAA5_D0_BYTES
   bra      pa3
pa9a
   ;move.l   d3,d0                      ; restore d0
   WATADATAA5_D0_BYTES_LONG
   ;WATADATAA5_D0_BYTES_64
   bra      pa3
;
pa10
   WAITNOTBSY D1,D6
   beq      pretec
   RATABYTE TF_STATUS,d1
   move.b   d1,mdu_act_Status(a3)
   and.b    #ERR,d1                    ;test, if error occured
   bne      pa_err
pa11
;   WAITNOTBSY d0,d6
   movem.l  (sp)+,a0-a4/d0-d6
   rts                                 ;return from Packet

pretec                                 ;if timeout, return status=FFh
   move.b   #$FF,mdu_act_Status(a3)
   bra      pa11
pa_err                                 ;if error, return actual status
   RATABYTE TF_STATUS,mdu_act_Status(a3)
   bra      pa11

pa_zero                                ;if zero length occured, return AAh
   move.b   #$AA,mdu_act_Status(a3)
   bra      pa11

   PUBLIC   pause
pause
   move.l   d0,-(sp)
   move.l   #LOOPPAUSE,d0
pu1
   DLY5US
   dbra     d0,pu1
   move.l   (sp)+,d0
   rts

   END


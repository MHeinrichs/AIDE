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

; ! set tabs to 2 !

;READOPE AND WRITEOPE ARE DEFINED HERE AND XPORTED BY XDEF:S
	xdef READOPE
	xdef WRITEOPE

	include "exec/io.i"
	include "exec/types.i"
	include "exec/lists.i"
	include "exec/libraries.i"
	include "exec/devices.i"
	include "exec/ables.i"	
	include "exec/memory.i"
	include "exec/io.i"
	include "exec/tasks.i"
	include "devices/scsidisk.i"
	include "lib/mydev.i"  ;select name etc, of the device
	include "lib/atid.i"	;This include has the macros which
									;are used to access a particular
									;implementation of an Amiga to ATA 
									;hardware interface, such as an A500 
									;side slot interface or a Parallel port
									;interface.
	 include "lib/ata.i"	 ; defines ATA bits and commands
	
	include "lib/asmsupp.i"; Various helpers from Commodore
	XLIB AllocMem  
	XLIB FreeMem
	XREF ResetIDE
	XREF SelectDrive


	Public	ATARdWt
ATARdWt:
	;a0 IO data address
	;a2 #READOPE/#WRITEOPE  operation is either read or write
	;a3 unitptr
	;d0 io length
	;d1 io offset low
	;d5 io offset high  !NEW!!
	movem.l	d1-d7/a0-a6,-(sp)
	move.l	d0,d3			  ;save length to somewhere save
	move.l	#BADLENGTH,d0	  ;No sectors ?
	lsr.l		#8,d3				 ;Divide by 512 to get
	lsr.l		#1,d3				 ;Number of sectors	
	beq 		Quits
	move.l	#BADOFFSET,d0	  ;Check if offset is too large: d1 holds 23 bits (32 minus 9)
	lsr.l		#8,d1
	lsr.l		#1,d1			  ;now d1 holds the bits 0..23 of the start block			
	;check if d5 contains an offset >32
	;remember that d1 holds the first 23 bits so this can be only 9 additional ones! 
	cmp.l		#$1FF,d5			  
	bgt 		Quits
	move.l	d5,d4			  ;only lower five bits are allowed! this could be extended by 8 bits for REAl 48LBA access!
	and.l	#$1FF,d4
	ror.l	#8,d4			  ;now rotate the high offset (9 bits) to the right position and add it to d1
	ror.l	#1,d4			  ;
	or.l	d4,d1			  ;d1 holds now the start block!
	move.l	d1,d4			  ;now copy d1 to d4
	add.l	d3,d4			  ;add the max block to transfer and see if we hit the boundary
	bcs  	Quits			  ;overflow!>quit	
	CMP.w	 #LBA48_ACCESS,mdu_lba(a3) ;is it a LBA48 drive, this should be able to handle 32bit addresses ;)?		
	beq	  transfer48		  ;everything is ok!
	and.l	#$F0000000,d4	  ; just the first 28 bits set?
	bne 	Quits			  ; nope: Quit!
LBA28transferpossible:
	move.l	d3,d7				 ;# blocks
	MOVEQ.l	#0,D3
	bra.s transfer
transfer48	
	AND.l	#$F0000000,d4	  ; just the first 28 bits set?->LBA28 on lba48 drives
	BEQ.s  LBA28transferpossible	
	move.l	d3,d7				 ;# blocks
	MOVE.b #LBA48_ACCESS,D3
transfer
		
	move.l	a0,a5				 ;Start address
	move.l	d1,d6				 ;Start block
	move.l  ABSEXECBASE,A6 ;exec base into A6 for enable/disable
	
	;register sum up:
	;a0 = free (start address)
	;a1 = free
	;a2 = #READOPE or #WRITEOPE
	;a3 = unit pointer
	;a4 = free
	;a5 = startaddress (buffer to write to)
	;a6 = EXEC_BASE
	;d0 = free
	;d1 = free
	;d2 = free
	;d3 = LBA28 indicator
	;d4 = free -> counter for sector
	;d5 = free -> number of sectors per read/write
	;d6 = blockoffset to write to
	;d7 = sectors to write
domore
	move.l	d7,d5
	cmp.l	 #MAX_TRANSFER,d5
	ble.s	 between1andMax
	move.l	#MAX_TRANSFER,d5				  ;Make max_transfer sectors

between1andMax
	move.l d5,d4					 ;d5 is number of sectors in this round
	;PRINTF 1,<'transfering %lx Blocks at LBA %lx',13,10>,D4,D6
	;and.l	 #$FF,d4				  ; 256 sectors = 00
maskd4done	
	WAITREADYFORNEWCOMMAND D0,D1
	bsr    setupdrive ;this routine destroys d0-D2
	;PRINTF 1,<'Drive set up complete!',13,10>
	CMP.b	 #LBA48_ACCESS,D3 ;D3 holds the info if we access >lba28
	BEQ.s  command48
	cmp.l	 #READOPE,a2
	beq 	 wasread
	;Format or Write
  WATABYTE #ATA_WRITE_SECTORS,TF_COMMAND
  bra.s   do_command
wasread
	WATABYTE #ATA_READ_SECTORS,TF_COMMAND	
  bra.s   do_command
command48
	CMP.l	 #READOPE,a2
	BEQ 	 wasread48
	;Format or Write
  WATABYTE #ATA_WRITE_SECTORS_EXT,TF_COMMAND
  bra.s   do_command
wasread48
	WATABYTE #ATA_READ_SECTORS_EXT,TF_COMMAND	

do_command:  
	RATABYTE TF_STATUS,d0			;clears the disabled interrupt
	;PRINTF 1,<'Command issued Status: %d',13,10>,D0
	sub.l	 #1,d4				 ;for dbne	
nextoneblock
	WAITDRQ	D0
	beq     errcode
	cmp.l	  #READOPE,a2
	beq.s   read_block
	WATADATAA5_512_BYTES ;destroys d0
	bra.s  checkerrorforthisblock
read_block:
	RATADATAA5_512_BYTES	;destroys d0
checkerrorforthisblock:
	;DLY400NS
	RATABYTE TF_STATUS,d0			;Also clears the disabled interrupt
	;PRINTF 1,<'Data transfered! Status: %d',13,10>,D0
	AND.b   #ERR+DWF+BSY,d0       ;everything fine (not Bsy and no error)?
	beq.s   looptonextblock
	;check for not busy and then for errors
	bsr     errorcheck
	;PRINTF 1,<'Error check! Status: %d',13,10>,D0
	bne  	  errcode
looptonextblock	
	dbne	  d4,nextoneblock
sectoracok

	;PRINTF 1,<'Next transfer. Remaining: %ld-%ld',13,10>,D7,D5
	add.l	 d5,d6					 ;next block number
	sub.l	 d5,d7
	bne 	 domore
	move.l #0,d0

Quits
	cmp.l	#0,d0
	bne.s	errcode	
okcode
	movem.l	(sp)+,d1-d7/a0-a6
	rts								;EXIT RDWT.ASM
errcode
  PRINTF 1,<'error in R/W TF_STATUS D0:%x D2:%x D3: %x D4: %lx D5: %lx D6:%lx D7: %lx',13,10>,D0,D2,D3,D4,D5,D6,D7
	;cmp.w	#ATAPI_DRV,mdu_drv_type(a3)
	;beq.s	errcodeend
	;cmp.w	#SATAPI_DRV,mdu_drv_type(a3)
	;beq.s	errcodeend
	;Reset drive - some drives may freeze at bad block but a reset resets the whole ide-chain!
	;jsr	  ResetIDE
errcodeend
	move.l	#1,d0
	BRA.s	okcode

;this routine checks for errors and returns 0 in d0 if no error occured
errorcheck
	WAITNOTBSY D0
	beq.s		erroroccured
	moveq.l  #0,D0
	RATABYTE TF_ALTERNATE_STATUS,d0
	and.b	  #ERR+DWF,d0 ;leave everything except the Errorbits ->if set this function returns not null
  rts
erroroccured:
  move.l #1,d0
  rts

;this routine sets all importaint information like master/slave, chs/lba and destroys d0-d2 on the way
setupdrive:
	CMP.b	 #LBA48_ACCESS,D3 ;D3 holds the info if we access >lba28
	BEQ 	 issueLBA48	
	cmp.w	 #CHS_ACCESS,mdu_lba(a3)
	bne 	 issueLBA

	;chs
  ;convert block number to Cylinder / Head / Sector numbers
	; Cyl go to D0 (word)
	; Sec to d1 (byte)
	; Head to D2 (byte)
	
	move.l	d6,d1				 ;d6 = number of block (block numbers begin from 0)
	move.l	mdu_sectors_per_track(a3),d2
	divu	  d2,d1
	move.l	d1,d0
	swap	  d1
	addq.w  #1,d1				 ;sector numbers begin at 1
	and.l	  #$ff,d1			 ;sector number byte
	;now the cylinder we copied the result of the division above to d0! 
	and.l	  #$ffff,d0			;16bit word
	move.l	mdu_heads(a3),d2
	divu	  d2,d0				 ;d0 = cyl
	move.l	d0,d2
	swap	  d2					 ;d2 = head
	AND.l	 #$f,d2      ; mask LBA: bits 24..27 / CHS: head count
	or.b	 mdu_UnitNum(a3),d2 ; set slave and LBA-bit

	;WATABYTE d1,TF_SECTOR_NUMBER
	;WATABYTE d0,TF_CYLINDER_LOW
	;lsr.w	 #8,d0
	;WATABYTE d0,TF_CYLINDER_HIGH
	; write head to the drive!
	;WATABYTE d2,TF_DRIVE_HEAD			  

	bra.s	 setupdriveend

issueLBA:

  ; d0 holds the LBA, but we have to convert it to:
	; d0 = LOW /MidByte :LBA 0..7 8..15 (word)
	; D1 = HIGH BYTE    :LBA x..x 16..23 (byte)
	; d2= Head          :LBA 24..27+SLAVE-BIT+LBA-BIT (byte)
	move.l d6,d0 		   ;logical block number to d0 and d1
  move.l D6,d1       
  swap   d1          ; put the highlba (16..27) to the lower part
  move.l d1,d2       ; make a copy in d2
  lsr.l	 #8,d2       ; rotate LBA 16..23 away: D2 holds lba bitrs 24..27 now!
	and.l	 #$f,d2      ; mask LBA: bits 24..27 / CHS: head count
	or.b	 mdu_UnitNum(a3),d2 ; set slave and LBA-bit to HEAD-register


setupdriveend:
	;LBA_LOW  = SECTOR_NUMBER
	;LBA_MID  = CYLINDER_LOW
	;LBA_HIGH = CYLINDER_HIGH 

	WATABYTE d4,TF_SECTOR_COUNT     ;Sectorcount
	WATABYTE d0,TF_LBA_LOW_BYTE		  ;LBA: bits  0..7  
	lsr.l	 #8,d0
	WATABYTE d0,TF_LBA_MID_BYTE			;LBA: bits  8..15  
	WATABYTE d1,TF_LBA_HIGH_BYTE		;LBA  bits  16..23 
	; write head to the drive!
	WATABYTE d2,TF_DRIVE_HEAD			  

  rts
  
issueLBA48:
	tst.b   D4
	bne.s   lba48sec0
	WATABYTE #1,TF_SECTOR_COUNT     ;write a 1 for 256 sectors
	bra.s lba48secdone
lba48sec0:
	WATABYTE #0,TF_SECTOR_COUNT     ;write a 0 for >256 sectors	
lba48secdone:	
	WATABYTE d4,TF_SECTOR_COUNT     
	move.l	d6,d0 ;logical block number

  ; d0 holds the LBA, but we have to convert it to:
	; d0 = LOW /MidByte :LBA 0..15
	; D1 = HIGH BYTE    :LBA 16..27
	; d2=  :LBA 24..31
	move.l d6,d0 		   ;logical block number to d0 and d1
  move.l D6,d1       
  swap   d1          ; put the highlba (16..27) to the lower part
  move.l d1,d2       ; make a copy in d2
  lsr.l	 #8,d2       ; rotate LBA 16..23 away: D2 holds lba bits 24..31 now!

	WATABYTE D2,TF_LBA_LOW_BYTE		  ;LBA: bits  24..31  
	WATABYTE #0,TF_LBA_MID_BYTE 		;LBA  bits  32..39
	WATABYTE #0,TF_LBA_HIGH_BYTE		;LBA  bits  40..47

	WATABYTE d0,TF_LBA_LOW_BYTE		  ;LBA: bits  0..7  
	lsr.l	 #8,d0
	WATABYTE d0,TF_LBA_MID_BYTE			;LBA: bits  8..15  
	WATABYTE d1,TF_LBA_HIGH_BYTE		;LBA  bits  16..23 

	;move.b	 mdu_UnitNum(a3),d2 ; set slave and LBA-bit
	; write head to the drive!
	WATABYTE mdu_UnitNum(a3),TF_DRIVE_HEAD			  
  rts
  
  
  Public ATARdWtLen
ATARdWtLen = *-ATARdWt
	

; a0 = scsi_Data, d0 = scsi_Length, a2 = scsi_Command, a6 = SCSICmd
; d2 = unit number, a3 = io_unit
	Public SCSIDirectCmd
SCSIDirectCmd
	movem.l  a0-a6/d0-d6,-(sp)
	move.l	d0,d1 ;save d0 somewhere save
	jsr		SelectDrive
	bne		sdc1
	move.l	a0,a5
sdc3
	and.l	 #$FFFF0000,d0			  ;no more than 64KB at once now :-(
	beq.s	 sdc5						  ;maybe will be fixed later
	move.b	#$BB,scsi_Status(a6)	 ;scsi_status = BBh -> length was >64KB
	move.b	#1,IO_ERROR(a1)
	clr.w	 scsi_SenseActual(a6)
	bra		sdc2
sdc5
	move.w	scsi_CmdLength(a6),d3
	move.b	scsi_Flags(a6),mdu_act_Flags(a3)
	bsr		Packet
	move.l	mdu_act_Actual(a3),d1
	add.l	 d1,scsi_Actual(a6)
	move.b	mdu_act_Status(a3),scsi_Status(a6)
	cmp.b	 #$50,mdu_act_Status(a3)
	beq		sdc2
	clr.w	 scsi_SenseActual(a6)
	move.b	#1,IO_ERROR(a1)
	cmp.b	 #$FF,mdu_act_Status(a3)			;status = FFh means timeout
	bne.s	 sdc1
	WATABYTE #$08,TF_COMMAND			;then reset atapi device
	DLY400NS
	WAITNOTBSY D1
	bra.s	 sdc2
sdc1										  ;read sense data if error
	lea		mdu_sense_data(a3),a5
	btst.b	#SCSIB_AUTOSENSE,scsi_Flags(a6)
	beq.s	 sdc6
	move.w	scsi_SenseLength(a6),d1
	bra.s	 sdc7
sdc6
	move.w	#20,d1
sdc7
	move.w	#12,d3
	lea		mdu_rs_cmd(a3),a2
	move.b	#SCSIF_READ,mdu_act_Flags(a3)
	bsr		Packet						;do packet request sense
	btst.b	#SCSIB_AUTOSENSE,scsi_Flags(a6)
	beq.s	 sdc9
	move.l	scsi_SenseData(a6),a5
	lea		mdu_sense_data(a3),a2
	move.l	mdu_act_Actual(a3),d0
	move.w	d0,scsi_SenseActual(a6)
	beq.s	 sdc2
	subq.w	#1,d0
sdc8
	move.b	(a2)+,(a5)+
	dbra	  d0,sdc8
sdc9
	lea		mdu_sense_data(a3),a2
	move.b	2(a2),d0
	and.b	 #$F,d0
	clr.l	 mdu_no_disk(a3)
	cmp.b	 #2,d0
	bne.s	 sdc91
	move.l	#1,mdu_no_disk(a3)		;medium is not present
sdc91
	cmp.b	 #6,d0
	bne.s	 sdc2
	add.l	 #1,mdu_change_cnt(a3)
sdc2
	movem.l  (sp)+,a0-a6/d0-d6
	rts

; a0 = scsi_Data, a2 = scsi_Command, a3 = io_unit, a6 = SCSICmd
; d1 = scsi_Length, d2 = unit number, d3 = cmd_length

;send packet to atapi drive and read/write data if needed
Packet
	movem.l  a0-a4/d0-d6,-(sp)
	clr.l	 mdu_act_Actual(a3)
	
	move.l   #LOOP,D0
waitreadyforpacket: 
  RATABYTE TF_ALTERNATE_STATUS,D6
  AND.b    #DRQ+BSY,D6
  BEQ.s    readyforpacket
  DLY3US   ; make a processor speed independent minimum delay
  SUBQ.L	 #1,D0
  BNE.S		 waitreadyforpacket 
	bra		   pretec ;timeout
readyforpacket: 
	WATABYTE d2,TF_DRIVE_HEAD			  ;set task file registers
	WATABYTE #0,TF_SECTOR_COUNT
	WATABYTE d1,TF_CYLINDER_LOW
	lsr.w	 #8,d1
	WATABYTE d1,TF_CYLINDER_HIGH
	WATABYTE #nIEN+8,TF_DEVICE_CONTROL
	WAITNOTBSY D0
	beq		pretec
	WATABYTE #ATA_PACKET,TF_COMMAND	  ;send packet command
	DLY400NS
	WAITDRQ  D0
	beq		pretec
	RATABYTE TF_STATUS,d0
	and.b	 #ERR,d0
	bne		pa_err
	lea		mdu_act_cmd(a3),a1					  ;prepare packet
	clr.l	 (a1)
	clr.l	 4(a1)
	clr.l	 8(a1)
	clr.l	 12(a1)
	lsr.w	 #1,d3
pa2
	move.w	(a2)+,(a1)+
	subq.w	#1,d3
	bne.s	 pa2
	lea		mdu_act_cmd(a3),a1
	WATAWORD (a1)+,TF_DATA				  ;write packet to drive
	WATAWORD (a1)+,TF_DATA
	WATAWORD (a1)+,TF_DATA
	WATAWORD (a1)+,TF_DATA
	WATAWORD (a1)+,TF_DATA
	WATAWORD (a1)+,TF_DATA
pa3
	move.l	#LOOP3,d2						;wait to packet execution result
pa4
	subq.l	#1,d2
	beq		pretec
	bsr		pause
	RATABYTE TF_ALTERNATE_STATUS,d0
	btst	  #BSY_BIT,d0
	bne.s	 pa4
	RATABYTE TF_SECTOR_COUNT,d1
	and.b	 #3,d1
	btst	  #3,d0
	bne		pa5
	cmp.b	 #3,d1
	beq.s	 pa6
	DLY5US
	bra		pa4
pa5
	btst	  #0,d1
	bne.s	 pa4
pa6
	RATABYTE TF_ALTERNATE_STATUS,d0
	btst	  #BSY_BIT,d0
	bne.s	 pa4
	RATABYTE TF_STATUS,d0
	and.b	 #DRQ,d0
	beq		pa10							  ;skip if no data
	moveq	 #0,d3
	RATABYTE TF_CYLINDER_HIGH,d3		  ;else read data length
	lsl.w	 #8,d3
	RATABYTE TF_CYLINDER_LOW,d3
	move.l	d3,mdu_act_Actual(a3)
	btst	  #0,d3
	beq.s	 pa7
	addq.l	#1,d3 ;add one, if odd length
pa7
	tst.l	 d3								 ;test data length and
	beq		pa_zero						  ;data buffer address
	cmp.l	 #0,a5
	beq		pa_err
	move.l	a5,d0
	and.l	 #1,d0
	bne		pa_err
	btst.b	#SCSIB_READ_WRITE,mdu_act_Flags(a3)	;read or write required?
	beq.s	 pa9
pa8				  
	; read data from drive
	move.l	d3,d0
	btst	  #1,d0											; is the data long alligned?							 
	beq.s	 pa8a
	RATADATAA5_D0_BYTES
	bra		pa3
pa8a 
	RATADATAA5_D0_BYTES_LONG
	bra		pa3
pa9												; write data to drive
	move.l	d3,d0
	btst	  #1,d0											; is the data long alligned?							 
	beq.s		 pa9a
	WATADATAA5_D0_BYTES
	bra		pa3
pa9a
	WATADATAA5_D0_BYTES_LONG
	bra		pa3
pa10
	WAITNOTBSY D1
	beq.s	 pretec
	RATABYTE TF_STATUS,d1
	move.b	d1,mdu_act_Status(a3)
	and.b	 #ERR,d1						  ;test, if error occured
	bne.s	 pa_err
pa11
	movem.l  (sp)+,a0-a4/d0-d6
	rts											;return from Packet

pretec											;if timeout, return status=FFh
	move.b	#$FF,mdu_act_Status(a3)
	bra.s	 pa11
pa_err											;if error, return actual status
	RATABYTE TF_STATUS,mdu_act_Status(a3)
	bra.s	 pa11

pa_zero										  ;if zero length occured, return AAh
	move.b	#$AA,mdu_act_Status(a3)
	bra.s	 pa11

	PUBLIC	pause
pause
	move.l	d0,-(sp)
	move.l	#LOOPPAUSE,d0
pu1
	DLY5US
	dbra	  d0,pu1
	move.l	(sp)+,d0
	rts

	Public EndCodeATARdWt
EndCodeATARdWt
	END


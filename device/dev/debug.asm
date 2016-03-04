SECTION section
	XDEF InitSer
	XDEF PutStringSer
	XDEF PutCharSer


; Set the serial port for receiving 8 bit data at 9600 bps.

InitSer:
	move.w    #$0174,$DFF032         Set up the SERPER register.
	rts       

;Method to print a Null-terminated String in A0 to the Serial port
PutStringSer
	movem.l	D0/D1,-(sp)
PutStringSerNext
	move.b	(A0)+,D0              Get a string character.
	beq.s	PutStringSerEnd
PutStringSerTransmit  
	move.w    $DFF018,D1             Read SERDATR.
	btst      #$0D,D1               Transmitter ready?
	beq.s     PutStringSerTransmit                Wait until true.
	and.w     #$FF,D0               Mask out all but bits 0-7.
	or.w      #$0100,D0             Set the stop bit.
	move.w    D0,$DFF030             Write to SERDAT.
	bra	PutStringSerNext
PutStringSerEnd:
	movem.l  (sp)+,D0/D1
	rts       
	
PutCharSer
	movem.l	D0/D1,-(sp)
PutCharSerTransmit  
	move.w    $DFF018,D1             Read SERDATR.
	btst      #$0D,D1               Transmitter ready?
	beq.s     PutCharSerTransmit                Wait until true.
	and.w     #$FF,D0               Mask out all but bits 0-7.
	or.w      #$0100,D0             Set the stop bit.
	move.w    D0,$DFF030             Write to SERDAT.
PutCharSerEnd:
	movem.l  (sp)+,D0/D1
	rts       


endcode:
	END
	
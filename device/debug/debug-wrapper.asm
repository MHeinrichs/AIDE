   SECTION   driver,CODE

	XDEF	kprint_macro
	XREF	KPrintF

kprint_macro:
	movem.l	d0-d1/a6,-(sp)
	jsr	KPrintF
	movem.l	(sp)+,d0-d1/a6
	rts
	
endcode:
   end
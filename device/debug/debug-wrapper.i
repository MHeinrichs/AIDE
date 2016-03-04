
* FUNCDEF macro definition for 'exec/exec_lib.i'

FUNCDEF     MACRO    *function
_LVO\1      EQU      FUNC_CNT
FUNC_CNT    SET      FUNC_CNT-6
            ENDM
FUNC_CNT    SET      5*-6

		
*************************************************************************
		IFND	PRINTF
PRINTF		MACRO	; level,<string>,...
		IFGE	DEBUG_DETAIL-\1
		XREF	kprint_macro
PUSHCOUNT	SET	0

		IFNC	'\9',''
		move.l	\9,-(sp)
PUSHCOUNT	SET	PUSHCOUNT+4
		ENDC

		IFNC	'\8',''
		move.l	\8,-(sp)
PUSHCOUNT	SET	PUSHCOUNT+4
		ENDC

		IFNC	'\7',''
		move.l	\7,-(sp)
PUSHCOUNT	SET	PUSHCOUNT+4
		ENDC

		IFNC	'\6',''
		move.l	\6,-(sp)
PUSHCOUNT	SET	PUSHCOUNT+4
		ENDC

		IFNC	'\5',''
		move.l	\5,-(sp)
PUSHCOUNT	SET	PUSHCOUNT+4
		ENDC

		IFNC	'\4',''
		move.l	\4,-(sp)
PUSHCOUNT	SET	PUSHCOUNT+4
		ENDC

		IFNC	'\3',''
		move.l	\3,-(sp)
PUSHCOUNT	SET	PUSHCOUNT+4
		ENDC

		movem.l a0/a1,-(sp)
		lea.l	PSS\@(pc),A0
		lea.l	4*2(SP),A1
		BSR	kprint_macro
		movem.l (sp)+,a0/a1
		bra.s	PSE\@

PSS\@		dc.b	\2
		IFEQ	(\1&1)  ;If even, add CR/LF par...
		   dc.b 13,10
		ENDC
		dc.b	0
		ds.w	0
PSE\@
		lea.l	PUSHCOUNT(sp),sp
		ENDC	;IFGE	DEBUG_DETAIL-\1
		ENDM	;PRINTF	MACRO
		ENDC	;IFND	PRINTF
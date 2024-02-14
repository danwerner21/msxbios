;___ROM_MONITOR_PROGRAM_____________________________________________________________________________________________________________
;
;  ORIGINAL CODE BY:	ANDREW LYNCH (LYNCHAJ@YAHOO COM)	13 FEB 2007
;
;  MODIFIED BY : 	DAN WERNER 03 09.2009
;
;__REFERENCES________________________________________________________________________________________________________________________ 
; THOMAS SCHERRER BASIC HAR.DWARE TEST ASSEMBLER SOURCES FROM THE Z80 INFO PAGE
; INCLUDING ORIGINAL SCHEMATIC CONCEPT
; HTTP://Z80 INFO/Z80SOURC.TXT
; CODE SAMPLES FROM BRUCE JONES PUBLIC DOMAIN ROM MONITOR FOR THE SBC-200C 
; HTTP://WWW RETROTECHNOLOGY.COM/HERBS_STUFF/SD_BRUCE_CODE.ZIP
; INSPIRATION FROM JOEL OWENS "Z-80 SPACE-TIME PRODUCTIONS SINGLE BOARD COMPUTER"
; HTTP://WWW JOELOWENS.ORG/Z80/Z80INDEX.HTML
; GREAT HELP AND TECHNICAL ADVICE FROM ALLISON AT ALPACA_DESIGNERS
; HTTP://GROUPS YAHOO.COM/GROUP/ALPACA_DESIGNERS
; INTEL SDK-85 ROM DEBUG MONITOR
;
;
;
;__CONSTANTS_________________________________________________________________________________________________________________________ 
;	
ENDT:		 .EQU	00h		; MARK END OF TEXT
CR:		 .EQU	0DH		; ASCII CARRIAGE RETURN CHARACTER
LF:		 .EQU	0AH		; ASCII LINE FEED CHARACTER
ESC:		 .EQU	1BH		; ASCII ESCAPE CHARACTER
BS:		 .EQU	08H		; ASCII BACKSPACE CHARACTER
;
;
;
;__MAIN_PROGRAM_____________________________________________________________________________________________________________________ 
;

;__DOTINYMON___________________________________________________________________________________________________________________ 
;
;	MONITOR STARTUP
;________________________________________________________________________________________________________________________________
;

DOTINYMON:				; CALL HERE FOR MONITOR WARM START

	XOR	A			;ZERO OUT ACCUMULATOR (ADDED)
	PUSH	HL			;PROTECT HL FROM OVERWRITE    
	LD	HL,TXT_IDENT		;POINT AT TEXT
	CALL	prn_text		;SHOW WE'RE HERE
	LD	HL,TXT_READY		;POINT AT TEXT
	CALL	prn_text		;SHOW WE'RE HERE
	POP	HL			;PROTECT HL FROM OVERWRITE

;
;__MONITOR_COMMANDS_________________________________________________________________________________________________________ 
;
; B XX BOOT CPM FROM DRIVE XX
; D XXXXH YYYYH  DUMP MEMORY FROM XXXX TO YYYY
; F XXXXH YYYYH ZZH FILL MEMORY FROM XXXX TO YYYY WITH ZZ
; I INPUT FROM PORT AND SHOW HEX DATA
; M XXXXH YYYYH ZZZZH MOVE MEMORY BLOCK XXXX TO YYYY TO ZZZZ
; O OUTPUT TO PORT HEX DATA
; P XXXXH YYH PROGRAM RAM FROM XXXXH WITH VALUE IN YYH, WILL PROMPT FOR NEXT LINES FOLLOWING UNTIL CR
; R RUN A PROGRAM FROM CURRENT LOCATION



;__COMMAND_PARSE_________________________________________________________________________________________________________________ 
;
;	PROMPT USER FOR COMMANDS, THEN PARSE THEM
;________________________________________________________________________________________________________________________________
;

MONITORCMDLOOP:
	CALL	CRLFA			; CR,LF,>
	CALL 	MONGETLN		; GET A LINE OF INPUT FROM THE USER
        LD      A,(HL)			; LOAD FIRST CHAR INTO A (THIS SHOULD BE THE COMMAND)
	INC	HL			; INC POINTER

	CP	'R'			; IS IT "R" (Y/N)
	JP	Z,MONRUN		; IF YES GO RUN ROUTINE
	CP	'P'			; IS IT "P" (Y/N)
	JP	Z,PROGRM		; IF YES GO PROGRAM ROUTINE
	CP	'O'			; IS IT AN "O" (Y/N)
	JP	Z,MONPOUT			; PORT OUTPUT
	CP	'I'			; IS IT AN "I" (Y/N)
	JP	Z,PIN			; PORT INPUT
	CP	'D'			; IS IT A "D" (Y/N)
	JP	Z,DUMP			; DUMP MEMORY
	CP	'M'			; IS IT A "M" (Y/N)
	JP	Z,MOVE			; MOVE MEMORY COMMAND
	CP	'F'			; IS IT A "F" (Y/N)
	JP	Z,FILL			; FILL MEMORY COMMAND
	LD	HL,TXT_COMMAND		; POINT AT ERROR TEXT
	CALL	prn_text		; PRINT COMMAND LABEL

	JR	MONITORCMDLOOP





;__MONGETLN_________________________________________________________________________________________________________________________ 
;
;	READ A LINE(80) OF TEXT , HANDLE <BS>, TERM ON <CR> 
;       EXIT IF TOO MANY CHARS    STORE RESULT IN HL.  CHAR COUNT IN C.
;________________________________________________________________________________________________________________________________
;
MONGETLN:
	LD	A,1			; ENABLE CURSOR
	LD      (CSRSW),A		;
	LD	C,00H			; ZERO CHAR COUNTER
	PUSH	DE			; STORE DE
	CALL	$00B1			;
	POP	DE			; RESTORE DE
	RET	
	




;__CRLF__________________________________________________________________________________________________________________________ 
;
;	SEND CR & LF 
;________________________________________________________________________________________________________________________________
;
CRLF:
	PUSH	HL			; PROTECT HL FROM OVERWRITE
	LD	HL,TCRLF		; LOAD MESSAGE POINTER
	CALL	prn_text		; SEBD MESSAGE 
	POP	HL			; PROTECT HL FROM OVERWRITE
	RET				;


;__LDHL__________________________________________________________________________________________________________________________ 
;
;	GET ONE WORD OF HEX DATA FROM BUFFER POINTED TO BY HL , RETURN IN HL
;________________________________________________________________________________________________________________________________
;
LDHL:
	PUSH	DE			; STORE DE
	CALL	HEXIN			; GET K B. AND MAKE HEX
	LD	D,A			; THATS THE HI BYTE
	CALL	HEXIN			; DO HEX AGAIN
	LD	L,A			; THATS THE LOW BYTE
	LD	H,D			; MOVE TO HL
	POP	DE			; RESTORE BC
	RET				; GO BACK WITH ADDRESS  


;__HEXIN__________________________________________________________________________________________________________________________ 
;
;	GET ONE BYTE OF HEX DATA FROM BUFFER IN HL, RETURN IN A
;________________________________________________________________________________________________________________________________
;
HEXIN:
	PUSH	BC			;SAVE BC REGS 
	CALL	NIBL			;DO A NIBBLE
	RLC	A			;MOVE FIRST BYTE UPPER NIBBLE  
	RLC	A			; 
	RLC	A			; 
	RLC	A			; 
	LD	B,A			; SAVE ROTATED BYTE
	CALL	NIBL			; DO NEXT NIBBLE
	ADD	A,B			; COMBINE NIBBLES IN ACC 
	POP	BC			; RESTORE BC
	RET				; DONE  
NIBL:
	LD	A,(HL)			; GET K B. DATA
	INC	HL			; INC KB POINTER
	CP	40H			; TEST FOR ALPHA
	JR	NC,ALPH			;
	AND	0FH			; GET THE BITS
	RET				;
ALPH:
	AND	0FH			; GET THE BITS
	ADD	A,09H			; MAKE IT HEX A-F
	RET				;




;__HXOUT_________________________________________________________________________________________________________________________ 
;
;	PRINT THE ACCUMULATOR CONTENTS AS HEX DATA 
;________________________________________________________________________________________________________________________________
;
HXOUT:
	PUSH	BC			; SAVE BC
	PUSH	AF			;
	LD	B,A			;
	RLC	A			; DO HIGH NIBBLE FIRST  
	RLC	A			;
	RLC	A			;
	RLC	A			;
	AND	0FH			; ONLY THIS NOW
	ADD	A,30H			; TRY A NUMBER
	CP	3AH			; TEST IT
	JR	C,OUT1			; IF CY SET PRINT 'NUMBER'
	ADD	A,07H			; MAKE IT AN ALPHA
OUT1:
	CALL	$00A2			; SCREEN IT
	LD	A,B			; NEXT NIBBLE
	AND	0FH			; JUST THIS
	ADD	A,30H			; TRY A NUMBER
	CP	3AH			; TEST IT
	JR	C,OUT2			; PRINT 'NUMBER'
	ADD	A,07H			; MAKE IT ALPHA
OUT2:
	CALL	$00A2			; SCREEN IT
	POP	AF			;
	POP	BC			; RESTORE BC
	RET				;


;__SPACE_________________________________________________________________________________________________________________________ 
;
;	PRINT A SPACE CHARACTER 
;________________________________________________________________________________________________________________________________
;
SPACE:
	PUSH	AF			; STORE AF
	LD	A,20H			; LOAD A "SPACE"
	CALL	$00A2			; SCREEN IT
	POP	AF			; RESTORE AF
	RET				; DONE

;__PHL_________________________________________________________________________________________________________________________ 
;
;	PRINT THE HL REG 
;________________________________________________________________________________________________________________________________
;
PHL:
	PUSH	AF
	LD	A,H			; GET HI BYTE
	CALL	HXOUT			; DO HEX OUT ROUTINE
	LD	A,L			; GET LOW BYTE
	CALL	HXOUT			; HEX IT
	CALL	SPACE			; 
	POP	AF
	RET				; DONE  

;__MONPOUT__________________________________________________________________________________________________________________________ 
;
;	OUTPUT TO AN I/O PORT, MONITOR COMMAND "O"
;________________________________________________________________________________________________________________________________
;
MONPOUT:
POUT1:
	INC	HL			;
	CALL	HEXIN			; GET PORT
	LD	C,A			; SAVE PORT POINTER
	INC	HL			;
	CALL	HEXIN			; GET DATA
OUTIT:
	OUT	(C),A			;
	JP	MONITORCMDLOOP		;


;__PIN___________________________________________________________________________________________________________________________ 
;
;	INPUT FROM AN I/O PORT, MONITOR COMMAND "I"
;________________________________________________________________________________________________________________________________
;
PIN:
	INC 	HL			;
	CALL	HEXIN			; GET PORT
	LD	C,A			; SAVE PORT POINTER
	CALL	CRLF			;
	IN	A,(C)			; GET DATA
	CALL	HXOUT			; SHOW IT
	JP	MONITORCMDLOOP	        ;





;__CRLFA_________________________________________________________________________________________________________________________ 
;
;	PRINT COMMAND PROMPT 
;________________________________________________________________________________________________________________________________
;
CRLFA:
	PUSH	HL			; PROTECT HL FROM OVERWRITE
	LD	HL,PROMPT		;
	CALL	prn_text		;
	POP	HL			; PROTECT HL FROM OVERWRITE
	RET				; DONE



;__MONRUN___________________________________________________________________________________________________________________________ 
;
;	TRANSFER OUT OF MONITOR, USER OPTION "R"
;________________________________________________________________________________________________________________________________
;
MONRUN:
	INC	HL			; SHOW READY
	CALL	LDHL			; GET START ADDRESS
	JP	(HL)			;	


;__PROGRM________________________________________________________________________________________________________________________ 
;
;	PROGRAM RAM LOCATIONS, USER OPTION "P"
;________________________________________________________________________________________________________________________________
;
PROGRM:
	INC	HL			; SHOW READY
	PUSH	HL			; STORE HL
	CALL	LDHL			; GET START ADDRESS
	LD	D,H			;
	LD	E,L			; DE POINTS TO ADDRESS TO PROGRAM
	POP	HL			;
	INC	HL			;
	INC	HL			;
	INC	HL			;
	INC	HL			;
	INC	HL			;
PROGRMLP:
	CALL	HEXIN			; GET NEXT HEX NUMBER
	LD	(DE),A			; STORE IT
	INC	DE			; NEXT ADDRESS;
	CALL	CRLFA			; CR,LF,>
	LD      A,'P'			;
	CALL	$00A2			;
	CALL  	SPACE			;
	LD	H,D			;
	LD	L,E			;
	CALL	PHL			;
	CALL 	MONGETLN		; GET A LINE OF INPUT FROM THE USER
        LD      A,(HL)			; LOAD FIRST CHAR INTO A 
	CP	00H			; END OF LINE?
	JP	Z,PROGRMEXIT		; YES, EXIT
	JP	PROGRMLP		; NO, LOOP
PROGRMEXIT:
	JP	MONITORCMDLOOP	







;__DUMP__________________________________________________________________________________________________________________________ 
;
;	PRINT A MEMORY DUMP, USER OPTION "D"
;________________________________________________________________________________________________________________________________
;
DUMP:
	INC	HL			; SHOW READY
	PUSH	HL			; STORE HL
	CALL	LDHL			; GET START ADDRESS
	LD	D,H			;
	LD	E,L			;
	POP	HL			;
	PUSH	DE			; SAVE START
	INC	HL			;
	INC	HL			;
	INC	HL			;
	INC	HL			;
	INC	HL			;
	CALL	LDHL			; GET END ADDRESS
	INC	HL			; ADD ONE MORE FOR LATER COMPARE
	EX	DE,HL			; PUT END ADDRESS IN DE
	POP	HL			; GET BACK START
GDATA:	
	CALL	CRLF			;	
BLKRD:
	CALL	PHL			; PRINT START LOCATION
	LD	C,8			; SET FOR 16 LOCS
	PUSH	HL			; SAVE STARTING HL
NXTONE:
	EXX				;
	LD	C,E			;
	IN	A,(C)			;
	EXX				;
	AND	7FH			;
	CP	ESC			;
	JP	Z,MONITORCMDLOOP	;
	CP	19			;
	JR	Z,NXTONE		;
	LD 	A,(HL)			; GET BYTE
	CALL	HXOUT			; PRINT IT
	CALL	SPACE			;
UPDH:	
	INC	HL			; POINT NEXT
	DEC	C			; DEC  LOC COUNT
	JR	NZ,NXTONE		; IF LINE NOT DONE
					; NOW PRINT 'DECODED' DATA TO RIGHT OF DUMP
PCRLF:
	CALL	SPACE			; SPACE IT
	LD	C,8			; SET FOR 16 CHARS
	POP	HL			; GET BACK START
PCRLF0:
	LD	A,(HL)			; GET BYTE
	AND	060H			; SEE IF A 'DOT'
	LD	A,(HL)			; O K. TO GET
	JR	NZ,PDOT			;
	LD	A,2EH			; LOAD A DOT	
PDOT:
	CALL	$00A2			; PRINT IT
	INC	HL			; 
	LD	A,D			;
	CP	H			;
	JR	NZ,UPDH1		;
	LD	A,E			;
	CP	L			;
	JP	Z,MONITORCMDLOOP	;
;
;IF BLOCK NOT DUMPED, DO NEXT CHARACTER OR LINE
UPDH1:
	DEC	C			; DEC  CHAR COUNT
	JR	NZ,PCRLF0		; DO NEXT
CONTD:
	CALL	CRLF			;
	JP	BLKRD			;




;__MOVE__________________________________________________________________________________________________________________________ 
;
;	MOVE MEMORY, USER OPTION "M"
;________________________________________________________________________________________________________________________________
;
MOVE:
	LD	C,03
					; START GETNM REPLACEMENT
					; GET SOURCE STARTING MEMORY LOCATION
	INC	HL			; SHOW EXAMINE READY
	PUSH	HL			;
	CALL	LDHL			; LOAD IN HL REGS 
	LD	D,H			;
	LD	E,L			;
	POP	HL			;
	PUSH	DE			; PUSH MEMORY ADDRESS ON STACK
	INC	HL			;
	INC	HL			;
	INC	HL			;
	INC	HL			;
	INC 	HL			; PRINT SPACE SEPARATOR
	PUSH	HL			;
	CALL	LDHL			; LOAD IN HL REGS 
	LD	D,H			;
	LD	E,L			;
	POP	HL			;
	PUSH	DE			; PUSH MEMORY ADDRESS ON STACK
	INC	HL			;
	INC	HL			;
	INC	HL			;
	INC	HL			;
	INC	HL			; PRINT SPACE SEPARATOR
	CALL	LDHL			; LOAD IN HL REGS 
	PUSH	HL			; PUSH MEMORY ADDRESS ON STACK
					; END GETNM REPLACEMENT
	POP	DE			; DEST
	POP	BC			; SOURCE END
	POP	HL			; SOURCE
	PUSH    HL			;
	LD	A,L			;
	CPL				;
	LD	L,A			;
	LD	A,H			;
	CPL				;
	LD	H,A			;
	INC	HL			;
	ADD	HL,BC			;
	LD	C,L			;
	LD	B,H			;
	POP     HL        		;
	CALL    MOVE_LOOP		;
	JP	MONITORCMDLOOP			; EXIT MOVE COMMAND ROUTINE
MOVE_LOOP:
	LD	A,(HL)			; FETCH
	LD	(DE),A			; DEPOSIT
	INC     HL			; BUMP  SOURCE
	INC     DE			; BUMP DEST
	DEC     BC			; DEC COUNT
	LD	A,C			;
	OR	B       		;
	JP	NZ,MOVE_LOOP		; TIL COUNT=0
	RET				;
               
;__FILL__________________________________________________________________________________________________________________________ 
;
;	FILL MEMORY, USER OPTION "M"
;________________________________________________________________________________________________________________________________
;
FILL:
	LD	C,03			;
					; START GETNM REPLACEMENT
					; GET FILL STARTING MEMORY LOCATION
	INC	HL			; SHOW EXAMINE READY
	PUSH	HL			;
	CALL	LDHL			; LOAD IN HL REGS 
	LD	D,H			;
	LD	E,L			;
	POP	HL			;
	PUSH	DE			; PUSH MEMORY ADDRESS ON STACK
	INC	HL			;
	INC	HL			;
	INC	HL			;
	INC	HL			;
	INC	HL			; PRINT SPACE SEPARATOR
					; GET FILL ENDING MEMORY LOCATION
	PUSH	HL			;
	CALL	LDHL			; LOAD IN HL REGS 
	LD	D,H			;
	LD	E,L			;
	POP	HL			;
	PUSH	DE			; PUSH MEMORY ADDRESS ON STACK
	INC	HL			;
	INC	HL			;
	INC	HL			;
	INC	HL			;
	INC	HL			; PRINT SPACE SEPARATOR
					; GET TARGET STARTING MEMORY LOCATION
	CALL	HEXIN			; GET K B. AND MAKE HEX
	LD	C,A			; PUT FILL VALUE IN F SO IT IS SAVED FOR LATER
	PUSH	BC			; PUSH FILL VALUE BYTE ON STACK
					; END GETNM REPLACEMENT
	POP	BC			; BYTE
	POP	DE			; END
	POP	HL			; START
	LD	(HL),C			;
FILL_LOOP:
	LD	(HL),C			;
	INC     HL			;
	LD	A,E			;
	SUB     L			;
	LD	B,A			;
	LD	A,D			;
	SUB     H			;
	OR	B			;
	JP	NZ,FILL_LOOP		;
	JP	MONITORCMDLOOP		;


;
;__FILL_MEM_______________________________________________________________________________________________________________________ 
;
;	FUNCTION	: FILL MEMORY WITH A VALUE
;	INPUT		: HL = START ADDRESS BLOCK
;			: BC = LENGTH OF BLOCK
;			: A = VALUE TO FILL WITH
;	USES		: DE, BC
;	OUTPUT		:
;	CALLS		: 
;	TESTED		: 13 FEB 2007
;_________________________________________________________________________________________________________________________________
;
FILL_MEM:
	LD	E,L			;
	LD	D,H			;
	INC	DE			;
	LD	(HL),A			; INITIALISE FIRST BYTE OF BLOCK WITH DATA BYTE IN A
	DEC	BC			;
	LDIR				; FILL MEMORY
	RET				; RETURN TO CALLER



;
;__TEXT_STRINGS_________________________________________________________________________________________________________________ 
;
;	SYSTEM TEXT STRINGS
;_____________________________________________________________________________________________________________________________
;
TCRLF:
	.DB  	CR,LF,ENDT

PROMPT:
	.DB  	CR,LF,'>',ENDT

TXT_IDENT:
	.TEXT   "N8VEM HOME COMPUTER  MSX BIOS V0.1"
	.DB   CR,LF,ENDT
TXT_READY:	
	.TEXT   "MONITOR READY "
	.DB   CR,LF,ENDT

TXT_COMMAND:
	.DB   CR,LF
	.TEXT   "UNKNOWN COMMAND."
	.DB   ENDT



 
 

;__PS2DRIVER_______________________________________________________________________________________
;
;	PS/2 DRIVER FOR MSX CBIOS 
;
;	ORIGINAL KEYBOARD DRIVERS BY: DR. JAMES MOXHAM
;	REMAINDER WRITTEN BY: DAN WERNER -- 12/4/2010
;__________________________________________________________________________________________________
;


;__KB_INITIALIZE___________________________________________________________________________________
;
; 	INITIALIZE - CLEAR SOME LOCATIONS AND SEND A RESET TO THE KEYBOARD
;__________________________________________________________________________________________________			   	   	
KB_INITIALIZE:
	LD 	A,10000010B		; A=OUT B=IN, C HIGH=OUT, CLOW=OUT
	OUT 	(KBPPICONT),A		; PPI CONTROL PORT
	CALL 	KB_DATAHIGH		;
	CALL 	KB_CLOCKHIGH		;
	LD 	A,0			;
	LD 	(CAPSLOCK),A		; SET CAPSLOCK OFF TO START
	LD 	(CTRL),A		; CONTROL OFF
	LD 	(NUMLOCK),A		; NUMLOCK OFF
	LD	(BREAKFLAG),A		;
	CALL 	KB_RESET		; RESET TO THE KEYBOARD
	RET


;__KB_RESET________________________________________________________________________________________
;
; 	RESET THE KEYBOARD
;__________________________________________________________________________________________________			   	   	
KB_RESET:
	CALL 	KB_DATAHIGH		;
	CALL 	KB_CLOCKHIGH		;
	LD 	B,255			;
SF1:	DJNZ 	SF1			;
	CALL 	KB_CLOCKLOW		; STEP 1
	LD 	B,255			;
SF2:	DJNZ 	SF2			;
	CALL 	KB_DATALOW		; STEP 2
	CALL 	KB_CLOCKHIGH		; STEP 3
	CALL 	KB_WAITCLOCKLOW		; STEP 4
	LD	B,9			; 8 DATA BITS + 1 PARITY BIT LOW
SF3:	PUSH 	BC			;
	CALL 	KB_DATAHIGH		; STEP 5
	CALL 	KB_WAITCLOCKHIGH	; STEP 6
	CALL 	KB_WAITCLOCKLOW		; STEP 7
	POP 	BC			;
	DJNZ 	SF3			;
	CALL 	KB_DATAHIGH		; STEP9
	CALL 	KB_WAITCLOCKLOW		; STEP 10 COULD READ THE ACK BIT HERE IF WANT TO
	CALL 	KB_WAITCLOCKHIGH	; STEP 11
	LD 	B,255			;
SF4:	DJNZ 	SF4			; FINISH UP DELAY
	RET


;_________________________________________________________________________________________________
;
; 	PORT C BIT ROUTINES
;__________________________________________________________________________________________________			   	   	
KB_DATAHIGH:
	LD 	A,01111001B		; SEE THE 8255 DATA SHEET
	OUT 	(KBPPICONT),A		; BIT 4
	RET
	
KB_DATALOW:				;
	LD 	A,01111000B		; SEE THE 8255 DATA SHEET
	OUT 	(KBPPICONT),A		;
	RET
	
KB_CLOCKHIGH:				;
	LD 	A,01111011B		; BIT 5 HIGH
	OUT 	(KBPPICONT),A		;
	RET
	
KB_CLOCKLOW:				;
	LD 	A,01111010B		;
	OUT 	(KBPPICONT),A		;
	RET				;



;__KB_WAITCLOCKLOW_________________________________________________________________________________
;
; WAITCLOCKLOW SAMPLES DATA BIT 0, AND WAITS TILL
; IT GOES LOW, THEN RETURNS
; ALSO TIMES OUT AFTER 0 001 SECONDS
; USES A, CHANGES B
;__________________________________________________________________________________________________			   	   	
KB_WAITCLOCKLOW:
	LD 	B,255		; FOR TIMEOUT COUNTER
WL1:	IN 	A,(KBPPIB)	; GET A BYTE FROM PORT B
	BIT 	1,A		; TEST THE CLOCK BIT
	RET 	Z		; EXIT IF IT WENT LOW
	DJNZ 	WL1		; LOOP B TIMES
	RET

	
;__KB_WAITCLOCKHIGH_________________________________________________________________________________
;
; WAITCLOCKHIGH SAMPLES DATA BIT 0, AND WAITS TILL
; IT GOES HIGH, THEN RETURNS
; ALSO TIMES OUT AFTER 0 001 SECONDS
; USES A, CHANGES B
;__________________________________________________________________________________________________			   	   	
KB_WAITCLOCKHIGH:	
	LD 	B,255		; FOR TIMEOUT COUNTER
WH1:	IN 	A,(KBPPIB)	; GET A BYTE FROM PORT B
	BIT 	1,A		; TEST THE CLOCK BIT
	RET 	NZ		; EXIT IF IT WENT HIGH
	DJNZ 	WH1		; LOOP B TIMES
	RET



;__KB_PROCESS______________________________________________________________________________________
;
;  
;__________________________________________________________________________________________________			   	   	
KB_PROCESS:	
 	CALL	KB_WAITBYTE	; TEST KEYBOARD  TIMES OUT AFTER A BIT
 	CP	0
 	RET	Z
	CALL 	KB_DECODECHAR	; RETURNS CHAR OR 0 FOR THINGS LIKE KEYUP, SOME RETURN DIRECTLY
	RET			; RETURN 

	
	
;__KB_DECODECHAR____________________________________________________________________________________
;
; DECODE CHARACTER PASS A AND PRINTS OUT THE CHAR
; ON THE LCD SCREEN
;__________________________________________________________________________________________________			   	   	
KB_DECODECHAR:
	CP 	0F0H		; IS A KEY UP (NEED TO DO SPECIAL CODE FOR SHIFT)
	JP 	Z,DECKEYUP	; IGNORE CHAR UP
	CP	0E0H		; TWO BYTE KEYPRESSES
	JP 	Z,TWOBYTE	;
	CP 	058H		; CAPS LOCK SO TOGGLE
	JP 	Z,CAPSTOG	;
	CP	12H		; SHIFT (DOWN, BECAUSE UP WOULD BE TRAPPED BY 0F ABOVE)
	JP 	Z,SHIFTDOWN	;
	CP	59H		; OTHER SHIFT KEY
	JP 	Z,SHIFTDOWN	;
	CP	014H		; CONTROL KEY
	JP 	Z,CONTROLDOWN	;
	CP	05AH		; ENTER KEY
	JP	Z,KBRETURN	;
	CP	066H		; BACKSPACE KEY
	JP	Z,BACKSPACE	;
	CP	0DH		; TAB KEY
	JP 	Z,TABKEY	;
	CP	076H		; ESCAPE KEY
	JP	Z,ESCAPE	;
	CP	07EH		; BREAK (SCROLL LOCK) KEY
	JP	Z,BREAK		;
	LD 	C,A		;
	LD 	B,0		; ADD BC TO HL
	LD 	HL,NORMALKEYS	; OFFSET TO ADD
	ADD 	HL,BC		;
	LD 	A,(CTRL)	;
	CP 	0		; IS CONTROL BEING HELD DOWN?
	JP	Z,DC1		; NO SO GO TO TEST CAPS LOCK ON
	LD	A,(HL)		; GET THE LETTER, SHOULD BE SMALLS 
	SUB	96		; A=97 SO SUBTRACT 96 A=1=^A
	call    key_put_into_buf	; STORE ON KB QUEUE
	RET			; RETURN 
DC1:	LD 	A,(CAPSLOCK)	;
	CP 	0		; IS IT 0, IF SO THEN DON'T ADD THE CAPS OFFSET
	JR 	Z,DC2		;
	LD 	C,080H		; ADD ANOTHER 50H TO SMALLS TO GET CAPS
	ADD 	HL,BC		;
DC2:	LD 	A,(HL)		;
	call    key_put_into_buf	; STORE ON KB QUEUE
	RET			;
TABKEY:				;
	LD 	A,9		;
	call    key_put_into_buf	; STORE ON KB QUEUE
	RET			;TAB
BACKSPACE:			;
	LD	A,8		; BACKSPACE
	call    key_put_into_buf	; STORE ON KB QUEUE
	RET			;
ESCAPE:				;
	LD	A,27		;
	call    key_put_into_buf	; STORE ON KB QUEUE
	RET			;
KBRETURN:				;
	LD	A,13		; CARRIAGE RETURN
	call    key_put_into_buf	; STORE ON KB QUEUE
	RET			;
DECKEYUP:
	CALL 	KB_WAITBYTE	; IGNORE KEY UP THROW AWAY THE CHARACTER UNLESS A SHIFT 
	CP	0
	JR	Z,DECKEYUP
	CP	012H		; IS IT A SHIFT
	JP 	Z,SHIFTUP	;
	CP	59H		; OTHER SHIFT KEY
	JP	Z,SHIFTUP	;
	CP	014H		; CONTROL UP
	JP 	Z,CONTROLUP	; CONTROL UP
	LD	A,0		; NOTHING CAPTURED SO SEND BACK A ZERO 
	RET
TWOBYTE:; ALREADY GOT EO SO GET THE NEXT CHARACTER
	CALL 	KB_WAITBYTE
	CP	0
	JR	Z,TWOBYTE	
	CP	0F0H		; SEE THE NOTES - KEYUP FOR E0 KEYS IS EO F0 NN NOT F0 EO!!
	JP	Z,TWOBYTEUP	;
	CP	071H		; DELETE
	JP	Z,DELETEKEY	;
	CP	05AH		; RETURN ON NUMBER PAD
	JP	Z,RETURNKEY	;
	CP	072H		;
	JP	Z,DOWNARROW	;
	CP	074H		;
	JP	Z,RIGHTARROW	;
	CP	06BH		;
	JP	Z,LEFTARROW	;
	CP	075H		;
	JP	Z,UPARROW	;
	CP	070H		;
	JP	Z,INSERT	;
	CP	07DH		;
	JP	Z,PAGEUP	;
	CP	07AH		;
	JP	Z,PAGEDOWN	;
	CP	06CH		;
	JP	Z,HOME		;
	CP	069H		;
	JP	Z,END		;
	LD 	A,0		; RETURNS NOTHING
	RET
TWOBYTEUP:			;EXPECT A BYTE AND IGNORE IT
	CALL	KB_WAITBYTE	;
	CP	0
	JR	Z,TWOBYTEUP
	LD	A,0		;
	RET			;
HOME:				;
	LD	A,11		; ESC
	call    key_put_into_buf	; STORE ON KB QUEUE
	RET			;
DOWNARROW:			;
	LD	A,31		; ESC
	call    key_put_into_buf	; STORE ON KB QUEUE
	RET			;
RIGHTARROW:			;
	LD	A,28		; ESC
	call    key_put_into_buf	; STORE ON KB QUEUE
	RET			;
LEFTARROW:			;
	LD	A,29		; ESC
	call    key_put_into_buf	; STORE ON KB QUEUE
	RET			;
UPARROW:			;
	LD	A,30		; ESC
	call    key_put_into_buf	; STORE ON KB QUEUE
	RET			;	
INSERT:				;
	LD	A,18		; ESC
	call    key_put_into_buf	; STORE ON KB QUEUE
	RET			;
PAGEUP:				;
PAGEDOWN:			;
END:				;
				;
	LD	A,0		; ESC
	RET			;
CONTROLDOWN:			; SAME CODE AS SHIFTDOWN BUT DIFF LOCATION
	LD 	A,0FFH		;
	LD	(CTRL),A	; CONTROL DOWN
	LD	A,0		;
	RET			;
CONTROLUP:			; CONTROL KEY UP SEE SHIFT FOR EXPLANATION
	LD	A,0		;
	LD 	(CTRL),A	;
	LD 	A,0		;
	RET			;
RETURNKEY:			;
	LD 	A,13		;
	call    key_put_into_buf	; STORE ON KB QUEUE
	RET			;
DELETEKEY:			;
	LD 	A,08H		; 	DELETE KEY VALUE 
	call    key_put_into_buf	; STORE ON KB QUEUE
	RET			;
CAPSTOG:			;
  	LD 	A,(CAPSLOCK)	;
	XOR 	11111111B	; SWAP ALL THE BITS
	LD 	(CAPSLOCK),A	;
	LD	A,0		; RETURNS NOTHING
	RET			;
SHIFTDOWN:			; SHIFT IS SPECIAL - HOLD IT DOWN AND IT AUTOREPEATS
				; SO ONCE IT IS DOWN, TURN CAPS ON AND IGNORE ALL FURTHER SHIFTS
				; ONLY AN F0+SHIFT TURNS CAPS LOCK OFF AGAIN
	LD 	A,0FFH		;
	LD 	(CAPSLOCK),A	;
	LD 	A,0		; RETURNS NOTHING
	RET			;
SHIFTUP:			; SHIFTUP TURNS OFF CAPS LOCK DEFINITELY
	LD 	A,0		;
	LD 	(CAPSLOCK),A	;
	LD 	A,0		; RETURNS NOTHING
	RET			;
BREAK:
	LD	A,$FF		;
	LD	(BREAKFLAG),A	;
	LD	A,0
	RET
	
;__KB_WAITBYTE_____________________________________________________________________________________
;
; WAIT FOR A BYTE - TESTS A NUMBER OF TIMES IF THERE IS A KEYBOARD INPUT,
; OVERWRITES ALL REGISTERS, RETURNS BYTE IN A
;__________________________________________________________________________________________________			   	   		
KB_WAITBYTE:
	CALL	KB_CLOCKHIGH	; TURN ON KEYBOARD
	
WB1:	IN 	A,(KBPPIB)	; GET A BYTE FROM PORT B
	BIT 	1,A		; TEST THE CLOCK BIT
	JR 	Z,WB1		; EXIT IF IT WENT HIGH

	LD 	B,255		; FOR TIMEOUT COUNTER
WB2:	IN 	A,(KBPPIB)	; GET A BYTE FROM PORT B
	BIT 	1,A		; TEST THE CLOCK BIT
	JR 	Z,WB3		; EXIT IF IT WENT LOW
	DJNZ 	WB2		; LOOP B TIMES		
	LD	E,0		;		
	JR	WB4
WB3:	
	CALL 	KB_READBITS	; TEST FOR A LOW ON THE CLOCK LINE
WB4:
	CALL 	KB_CLOCKLOW	; TURN OFF KEYBOARD
	LD	A,E
	RET

;__KB_READBITS_____________________________________________________________________________________
;
; READBITS READS 11 BITS IN FROM THE KEYBOARD
; FIRST BIT IS A START BIT THEN 8 BITS FOR THE BYTE
; THEN A PARITY BIT AND A STOP BIT
; RETURNS AFTER ONE MACHINE CYCLE IF NOT LOW
; USES A, B,D, E 
; RETURNS A=0 IF NO DATA, A= SCANCODE (OR PART THEREOF)
;__________________________________________________________________________________________________			   	   		
KB_READBITS:
	CALL 	KB_WAITCLOCKHIGH; IF GETS TO HERE THEN MUST BE LOW SO WAIT TILL HIGH
	LD 	B,8		; SAMPLE 8 TIMES
	LD 	E,0		; START WITH E=0
R2:	LD 	D,B		; STORE BECAUSE WAITCLOCKHIGH DESTROYS
	CALL 	KB_WAITCLOCKLOW	; WAIT TILL CLOCK GOES LOW
	IN 	A,(KBPPIB)	; SAMPLE THE DATA LINE
	RRA			; MOVE THE DATA BIT INTO THE CARRY REGISTER
	LD 	A,E		; GET THE BYTE WE ARE BUILDING IN E
	RRA			; MOVE THE CARRY BIT INTO BIT 7 AND SHIFT RIGHT
	LD 	E,A		; STORE IT BACK  AFTER 8 CYCLES 1ST BIT READ WILL BE IN B0
	CALL 	KB_WAITCLOCKHIGH; WAIT TILL GOES HIGH
	LD 	B,D		; RESTORE FOR LOOP
	DJNZ 	R2		; DO THIS 8 TIMES
	CALL 	KB_WAITCLOCKLOW	; GET THE PARITY BIT
	CALL 	KB_WAITCLOCKHIGH;
	CALL 	KB_WAITCLOCKLOW	; GET THE STOP BIT
	CALL 	KB_WAITCLOCKHIGH;	
	RET
	
	
NORMALKEYS: ; THE TI CHARACTER CODES, OFFSET FROM LABEL BY KEYBOARD SCAN CODE
		 .DB 000,000,000,000,000,000,000,000,000,000
		 .DB 000,000,000,009,"`",000,000,000,000,000	; 0D = TABKEY=9
		 .DB 000,"q","1",000,000,000,"z","s","a","w"
		 .DB "2",000,000,"c","x","d","e","4","3",000
		 .DB 000," ","v","f","t","r","5",000,000,"n"
		 .DB "b","h","g","y","6",000,000,000,"m","j"
		 .DB "u","7","8",000,000,",","k","i","o","0"
		 .DB "9",000,000,".","/","l",";","p","-",000
		 .DB 000,000,039,000,"[","=",000,000,000,000	; 39 IS '
		 .DB 000,"]",000,092,000,000,000,000,000,000	; 92 IS \
		 .DB 000,000,000,000,000,"1",000,"4","7",000
		 .DB 000,000,"0",".","2","5","6","8",000,000
		 .DB 000,"+","3","-","*","9",000,000		; PAD TO 80H BYTES
		 .DB 000,000,000,000,000,000,000,000,000,000
		 .DB 000,000,000,009,"~",000,000,000,000,000	; 0D = TABKEY=9
		 .DB 000,"Q","!",000,000,000,"Z","S","A","W"
		 .DB "@",000,000,"C","X","D","E","$","#",000
		 .DB 000," ","V","F","T","R","%",000,000,"N"
		 .DB "B","H","G","Y","^",000,000,000,"M","J"
		 .DB "U","&","*",000,000,"<","K","I","O",")"
		 .DB "(",000,000,">","?","L",":","P","_",000
		 .DB 000,000,034,000,"{","+",000,000,000,000	; 34 IS "
		 .DB 000,"}",000,"|",000,000,000,000,000,000	; 92 IS \
		 .DB 000,000,000,000,000,"1",000,"4","7",000
		 .DB 000,000,"0",".","2","5","6","8",000,000
		 .DB 000,"+","3","-","*","9",000,000		; PAD TO 80H BYTES

		

;__________________________________________________________________________________________________
;
; 	RAM STORAGE AREAS
;__________________________________________________________________________________________________			


CAPSLOCK		.EQU	0F419H		; location for caps lock, either 00000000 or 11111111
CTRL			.EQU	0F41AH		; location for ctrl on or off 00000000 or 11111111
NUMLOCK			.EQU	0F41BH		; location for num lock
BREAKFLAG		.EQU	0F41CH		; location for BREAK FLAG


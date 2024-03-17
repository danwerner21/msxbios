;**************************************************************
;*                       MICRO-BASIC                          *
;*------------------------------------------------------------*
;*   A SMALL INTEGER BASIC INTERPRETER FOR THE 8080/8085/Z80  *
;*                                D.F. DUNFIELD  JAN 25/1983  *
;*                                WWW.DUNFIELD.COM            *
;*                                                            *
;*   MODIFIED FOR THE N8VEM HOME COMPUTER 11/21/2010          *
;*   BY Dan Werner                                            *
;*                                                            *
;*------------------------------------------------------------*
;*                      BASIC COMMANDS                        *
;*                                                            *
;*  CLEAR,    DATA,     DIM,      END,      EXIT,     FOR,    *
;*  GOSUB,    GOSUB(N), GOTO,     GOTO(N),  IF/THEN,  INPUT,  *
;*  LET,      LIF/THEN, LIST,     LOAD,     NEW,      NEXT,   *
;*  ORDER,    PLOT,     PRINT,    READ,     REM,      RETURN, *
;*  RUN,      SAVE,     SIZE,     STOP,     USR,      <EDIT>  *
;*------------------------------------------------------------*
;*                     BASIC OPERATORS                        *
;*                                                            *
;* NUMERIC:     + - * % / \ & | ; ( ) < = > == -= <= >=       *
;* CHARACTER:   + = == -=                                     *
;* OTHER:       : # $ @ ? [ ] ( )                             *
;*------------------------------------------------------------*
;*                      BASIC VARIABLES                       *
;*                                                            *
;*     A - Z ......... 16 BIT SIMPLE INTEGER VARIABLES.       *
;*     A$ - Z$ ....... SIMPLE CHARACTER VARIABLES. < 36 CHARS *
;*     A[N] - Z[N] ... 16 BIT INTEGER ARRAYS.                 *
;*     @[N] .......... PSEUDO MEMORY REFERENCE ARRAY.         *
;*     @[N]$ ......... NUMERIC TO CHARACTER CONVERSION.(CHR$) *
;*     ? ............. PSEUDO RANDOM NUMBER GENERATOR.        *
;*------------------------------------------------------------*
;*                        MEMORY MAP                          *
;*                                                            *
;*    		    3K BASIC INTERPRETER 		      *
;*    4000-41FF     0.5K POINTERS, STACKS, BUFFERS.           *
;*    4200-45FF     1K VARIABLES AND POINTERS.                *
;*    4600-FFFF     PROGRAM AND ARRAY STORAGE.                *
;**************************************************************
;*
;**************************************************************
;*                   CONSTANTS AND .EQUATES
;**************************************************************
;*
;* MONITOR ROUTINES
;*
OUT
        .EQU    $00A2                             ;OUTPUT ROUTINE ADDRESS
EXIT
        .EQU    DOTINYMON                         ;TERMINATION RETURN ADDRESS
GETR
        .EQU    $00E4                             ;ROUTINE TO LOAD INTEL HEX FORMAT
TDUMP
        .EQU    $00ED                             ;ROUTINE TO DUMP IN INTEL HEX FORMAT
TON
        .EQU    $00E1                             ;ROUTINE TO START TAPE
TOFF
        .EQU    $00E7                             ;ROUTINE TO STOP TAPE
NL
        .EQU    CRLF                              ;ROUTINE TO PRINT A <LF>, <CR> ON TERMINAL
PMSG
        .EQU    prn_text                          ;DISPLAY'S MSG(HL) UP TO ZERO OR <CR>

;* MEMORY ALLOCATION
BUFF
        .EQU    $4000                             ;START OF RAM, INPUT BUFFER
IOCON
        .EQU    BUFF+$FF                          ;INPUT/OUTPUT CONFIGURATION
USROUT
        .EQU    IOCON-2                           ;USER SUPPLIED OUTPUT DEVICE VECTOR
CURSOR
        .EQU    USROUT-2                          ;CURSOR POSITION
LSTCHR
        .EQU    CURSOR-2                          ;CHARACTER UNDER CURSOR
STACK
        .EQU    LSTCHR-2                          ;MACHINE STACK
TB
        .EQU    BUFF+50                           ;TEMPORARY TEXT BUFFER
XBF
        .EQU    TB+50                             ;EXTRA TEXT BUFFER
EDBUF
        .EQU    BUFF+256                          ;EDIT BUFFER
CS
        .EQU    EDBUF+$FA                         ;CONTROL STACK SPACE
CSP
        .EQU    CS+1                              ;CONTROL STACK POINTER
ARYLOC
        .EQU    CSP+2                             ;LOCATION OF ARRAYS
SEED
        .EQU    ARYLOC+1                          ;RANDOM NUMBER SEED
VARS
        .EQU    SEED+2                            ;VARIABLE SPACE
RFLAG
        .EQU    VARS+52                           ;PROGRAM RUNNING FLAG
IFLAG
        .EQU    RFLAG+1                           ;INPUTTING FLAG
P
        .EQU    IFLAG+1                           ;POINTER TO END OF EXPRESSION
EFLAG
        .EQU    P+1                               ;ASSIGNMENT FLAG
DATA
        .EQU    EFLAG+1                           ;READ/DATA POINTER
LAST
        .EQU    DATA+2                            ;LAST FREE ARRAY SPACE
TEMPBAS
        .EQU    LAST+2                            ;TEMPORARY STORAGE
TEXT
        .EQU    VARS+1024                         ;PROGRAM AND ARRAY STORAGE
DELETE
        .EQU    $7F                               ;DELETE CHARACTER
VARSEND
        .EQU    VARS+$0400
;*
;********************************************************************
;* START OF MAIN PROGRAM, FIRST INITIALIZE, INSURING WE DON'T THINK *
;* WE HAVE A VALID PROGRAM, ALSO CLEAR OUT HIS VARIABLES AND ARRAYS *
;********************************************************************
;*
STARTBASIC:
BASIC:
        LD      A,1                               ; ENABLE CURSOR
        LD      (CSRSW),A                         ;
        LD      HL,TXT_IDENT                      ;POINT AT TEXT
        CALL    prn_text                          ;SHOW WE'RE HERE
        LD      HL,TXT_BASICID                    ;POINT AT TEXT
        CALL    prn_text                          ;SHOW WE'RE HERE
NEW:
        LD      A,$FF                             ;INDICATES END OF PROGRAM
        LD      (TEXT),A                          ;INITIALIZE TO NO PROGRAM
RESV:
        CALL    CLEAR                             ;CLEAR OUT HIS VARIABLES
;* RESET FLAGS, AND PROMPT WITH 'READY', SO HE WILL KNOW WE ARE LISTENING
INIT:
        LD      HL,0                              ;GET DOUBLE BYTE ZERO
        LD      (RFLAG),HL                        ;INDICATE NOT RUNNING, AND NOT INPUT
        LD      (P),HL                            ;INDICATE NO ASSIGNMENT DONE
        LD      HL,RDY                            ;ADDRESS OF 'READY' MESSAGE
        CALL    PMSG                              ;TELL HIM WE ARE READY
;* GET A LINE FROM CONSOLE, AND SEE WHAT HE WANTS
TOP:
        LD      SP,STACK                          ;FIX UP STACK IN CASE WE ABORTED SOMETHING
        CALL    GLINE                             ;LET HIM GIVE US A LINE
        CP      $0D                               ;DID HE ONLY PRESS RETURN
        JP      Z,TOP                             ;NOT GOOD ENOUGH, MAKE HIM TRY AGAIN
        CALL    NUM                               ;DID HIS LINE START WITH A NUMBER
        JP      NC,EDIT                           ;IF SO, HE IS WRITEING A PROGRAM!!!
;* LOOK UP COMMAND AND EXECUTE
        LD      BC,INIT                           ;ADDRESS TO RETURN TO
        PUSH    BC                                ;SAVE SO WE CAN RETURN
        LD      HL,KTAB-1                         ;POINT TO COMMAND TABLE
;*
;* LOCATES COMMAND POINTED TO BY D-E IN THE COMMAND TABLE POINTED TO BY H-L
;* AND CHAINS TO THE COMMAND PROCESSING CODE FOR THAT COMMAND
;*
CMD:
        CALL    PARSE1                            ;ADVANCE TO NEXT CHARACTER
TLP0:
        PUSH    DE                                ;SAVE POINTER TO START OF COMMAND
CMDL:
        INC     HL                                ;ADVANCE IN TABLE
        LD      A,(DE)                            ;GET DATA FROM COMMAND
        CP      (HL)                              ;COMPARE WITH TABLE CONTENTS
        INC     DE                                ;ADVANCE IN COMMAND
        JP      Z,CMDL                            ;IF SAME, KEEP TESTING
;* DIDN'T MATCH, SEE IF IT'S END OF WORD (HIGH BIT SET)
        OR      $80                               ;ARE WE AT END?
        CP      (HL)                              ;AND IS IT THIS ONE?
        JP      Z,GOTCMD                          ;IF SO, WE HAVE IT
;* WASN'T THAT ENTRY, KEEP LOOKING
        SUB     A                                 ;SEE IT THIS IS END OF TABLE
        CP      (HL)                              ;ARE WE AT END?
        JP      Z,GOTDEF                          ;IF SO, WE WILL SAY WE FOUND
        POP     DE                                ;RESTORE POINTER TO COMMAND
CMD1:
        OR      (HL)                              ;TEST FOR AT END OF TABLE ENTRY
        INC     HL                                ;POINT TO NEXT
        JP      P,CMD1                            ;IF NO, KEEP LOOKING
        INC     HL                                ;SKIP FIRST ADDRESS BYTE
        JP      TLP0                              ;TEST THIS ENTRY
;* WE HIT THE END OF THE TABLE, ASSUME THE DEFAULT ADDRESS (LET)
GOTDEF:
        DEC     DE                                ;BACKUP TO START OF WORD
;* WE MATCHED ALL THE WAY TO THE END OF A COMMAND WORD, GET IT'S ADDRESS
GOTCMD:
        INC     HL                                ;POINT TO ADDRESS BYTE
        LD      C,(HL)                            ;SAVE TEMPORARY
        INC     HL                                ;POINT TO NEXT ADDRESS BYTE
        LD      H,(HL)                            ;GET LOW ADDRESS
        LD      L,C                               ;GET HIGH ADDRESS
        EX      (SP),HL                           ;PLACE CODE ADDRESS ON STACK, REMOVE TRASH
        JP      PARSE1                            ;SKIP TO NEXT NON-BLANK, AND CHAIN TO CODE
;*
;****************************************************************
;*              ERROR CHECKING AND HANDLING CODE
;****************************************************************
;*
;* VCHAR....TESTS FOR A VALID VARIABLE, SYNTAX ERROR IF NOT
;*
VCHAR:
        CALL    CHAR                              ;TEST VARIABLE
        RET     NC                                ;IF OK, GO BACK
;*
;* SYNTAX ERROR... HE'S NOT MAKEING ANY SENSE AT ALL
;* ISSUE NASTY MESSAGE TO STRAIGHTEN HIM OUT
;*
SYNT:
        LD      HL,SYN                            ;GET NASTY 'SYNTAX' MESSAGE
        DEC     DE                                ;BACK UP IN SOURCE SO WE DONT SAY WRONG LINE
;*
;* ERROR STUFF... SOMETHING HAS GONE WRONG... TELL HIM THE BAD NEWS AND
;* QUIT ANYTHING THAT WE MAY HAVE STARTED, SO THAT THINGS CAN'T GET WORSE
;* ALSO, IF WE WERE RUNNING, GIVE HIM THE LINE NUMBER AS A CLUE
;*
ERR:
        LD      A,'?'                             ;PRECEDE ERROR MESSAGE BY '?'
        CALL    OUT                               ;DISPLAY ON TERMINAL
        LD      A,(IFLAG)                         ;WERE WE TRYING TO 'INPUT' SOMETHING
        AND     A                                 ;IF WE WERE, THEN ..
        JP      NZ,INERR                          ;SPECIAL MESSAGE + HANDLEING
;* NOW THAT WE HAVE FIGURED OUT WHAT'S GOING ON, LET HIM IN ON IT
        CALL    PMSG                              ;PRINT ERROR MESSAGE
        LD      HL,EM                             ;FOLLOWED BY..
;* PRINT MESSAGE FOLLOWED BY LINE NUMBER (ALSO USED BY 'STOP IN LINE XXXX')
PERR:
        CALL    PMSG                              ;THE ' ERROR ' PART
        LD      A,(RFLAG)                         ;WERE WE RUNNING..
        AND     A                                 ;IF NOT,
        JP      Z,INLF                            ;THEN THATS ALL WE HAVE TO DO
;* DISPLAY LINE NUMBER OF RUNNING PROGRAM
        LD      HL,INL                            ;ADDRESS OF 'IN LINE ' MESSAGE
        CALL    PMSG                              ;DISPLAY FOR HIM
;* FIND START OF OUR LINE, AND DISPLAY LINE NUMBER
FSOL:
        DEC     DE                                ;BACK UP IN SOURCE
        LD      A,D                               ;GET HIGH BYTE OF ADDRESS
        CP      TEXT & $FF00/ $100                ;TEST FOR BEYOND BEGINNING
        JP      C,STLIN                           ;AT START OF LINE
        LD      A,(DE)                            ;GET CHARACTER FROM BUFER
        CP      $0D                               ;TEST FOR CARRIAGE RETURN
        JP      NZ,FSOL                           ;IF NOT, KEEP LOOKING
STLIN:
        INC     DE                                ;ADVANCE IN SOURCE
        EX      DE,HL                             ;SWAP TO H-L FOR PNUM
        CALL    PNUM                              ;PRINT AND BUFFER LINE NUMBER
;* WAIT FOR CONSOLE INPUT, ON A NEW LINE
INLF:
        CALL    NL                                ;ADVANCE A LINE ON HIS TERMINAL
        CALL    RESET                             ;RESET DATA POINTER AND CONTROL-STACK
        JP      INIT                              ;GET NEXT COMMAND
;*
;* SUBROUTINE TEST FOR VALID ASCII DIGIT (0-9), RETURNS WITH C=1 IF NOT
;*
NUM:
        CP      '0'                               ;TEST FOR < '0'
        RET     C                                 ;IF SO, BAD DIGIT
        CP      $3A                               ;TEST FOR >'9'
        CCF                                       ;INVERT LOGIC, C=1 IF BAD
        RET
;*
;****************************************************************
;*                    TEXT EDITING ROUTINES
;****************************************************************
;*
;* SUBROUTINE TO GET AND EDIT COMMAND LINE FROM TERMINAL
;*
BADLN:
        CALL    NL                                ;ADVANCE TO NEW LINE
GLINE:
        CALL    MONGETLN
        CALL    NL
        PUSH    HL
        POP     DE
        PUSH    DE                                ;SAVE BUFFER ADDRESS
        LD      IX,BUFF
MOVL:
        LD      A,(DE)                            ;GET CHARACTER FROM NEW LINE
        LD      (HL),A                            ;SAVE IN OLD LINE BUFFER
        LD      (IX),A
        INC     HL                                ;POINT TO NEXT
        INC     DE                                ;POINT TO NEXT
        INC     IX
;	CP	$0D		;TEST FOR END OF LINE
        CP      $00                               ;TEST FOR END OF LINE
        JP      NZ,MOVL                           ;IF NOT, KEEP MOVEING
        DEC     HL
        DEC     IX
        LD      A,$0D
        LD      (HL),A
        LD      (IX),A
        INC     HL
        POP     DE                                ;RESTORE BUFFER ADDRESS
        LD      A,(DE)                            ;AND FIRST CHARACTER FROM IT
        RET
;*
;* GET A PACKED-DECIMAL LINE NUMBER FROM THE COMMAND BUFFER
;*
GETLN:
        LD      HL,0                              ;START WITH ZERO
ELOOP:
        LD      A,(DE)                            ;GET DIGIT FROM COMMAND BUFFER
        CALL    NUM                               ;TEST FOR ASCII DIGIT
        RET     C                                 ;IF NOT, STOP (WE HAVE IT)
        INC     DE                                ;ADVANCE TO NEXT BUFFER POSITION
        ADD     HL,HL                             ;MAKE ROOM FOR DIGIT IN BOTTOM..
        ADD     HL,HL                             ;OF THE RESULT, BY ..
        ADD     HL,HL                             ;ROTATING IT..
        ADD     HL,HL                             ;LEFT BY FOUR BITS
        AND     $0F                               ;CONVERT DIGIT TO BINARY
        OR      L                                 ;INSERT INTO LOWER DIGITS OF RESULT
        LD      L,A                               ;AND REPLACE BYTE IN RESULT WITH NEW VALUE
        JP      ELOOP                             ;GET NEXT DIGIT
;*
;* LINE EDITOR, EDITS PROGRAM SOURCE BY LINE NUMBER IN COMMAND BUFFER
;*
EDIT:
        CALL    LINEF                             ;LOCATE LINE NUMBER IN SOURCE
        PUSH    HL                                ;SAVE POINTER INTO TEXT
        JP      NZ,INS                            ;IF NEW LINE, DON'T TRY TO DELETE
;* DELETE LINE POINTED TO BY H-L
DEL:
        LD      D,H                               ;COPY POINTER INTO
        LD      E,L                               ;THE D-E PAIR FOR BACKWARDS COPY
        LD      A,$0D                             ;WE ARE LOOKING FOR A CARRIAGE RETURN
;* FIND START OF NEXT LINE
DELNX:
        CP      (HL)                              ;TEST FOR END OF LINE TO DELETE
        INC     HL                                ;POINT TO NEXT CHARACTER IN SOURCE
        JP      NZ,DELNX                          ;IF NOT END OF LINE, KEEP LOOKING
;* COPY REST OF PROGRAM BACK OVER DELETED LINE
DELLP:
        LD      A,(HL)                            ;GET CHARACTER FROM NEXT LINE
        LD      (DE),A                            ;SAVE OVER DELETED LINE
        INC     DE                                ;POINT TO NEXT IN NEW LINE
        INC     HL                                ;POINT TO NEXT IN OLD LINE
        INC     A                                 ;TEST FOR END OF FILE
        JP      NZ,DELLP                          ;IF NOT, KEEP DELETEING
;* INSERT LINE INTO TEXT
INS:
        LD      BC,2                              ;SET LENGTH TO 2 (PACKED DECIMAL NUMBERS ARE 2)
        LD      DE,BUFF                           ;POINT TO BUFFER (CONTAINING NEW LINE)
;* CALCULATE LENGTH OF LINE
        CALL    GETLN                             ;REMOVE NUMBERS AS THEY ARE NOT STORED AS TEXT
ILP:
        INC     C                                 ;INCREMENT LENGTH
        LD      A,(DE)                            ;GET CHARACTER FROM NEW LINE (IN BUFFER)
        INC     DE                                ;POINT TO NEXT CHARACTER FROM NEW LINE
        CP      $0D                               ;TEST FOR END OF LINE
        JP      NZ,ILP                            ;IF NOT, KEEP COUNTING
        LD      A,C                               ;GET LENGTH
        POP     HL                                ;RESTORE POSITION IN TEXT
        CP      3                                 ;TEST FOR NULL LINE
        JP      Z,TOP                             ;IS SO, DON'T INSERT
;* INSERT NEW LINE INTO TEXT
INLN:
        LD      D,H                               ;SET D-E TO POINT TO
        LD      E,L                               ;THE LINE POSITION
        CALL    GETEOF                            ;GET END OF FILE ADDRESS
        INC     HL                                ;ADVANCE TO FREE BYTE
        PUSH    BC                                ;SAVE LENGTH
        PUSH    HL                                ;STACK END OF FILE ADDRESS
        ADD     HL,BC                             ;ADD LENGTH
        POP     BC                                ;GET END OF FILE ADDRESS
        INC     HL                                ;ADVANCE BECAUSE WE DECREMENT
IL01:
        DEC     HL                                ;REDUCE POINTER INTO NEW POSITION
        DEC     BC                                ;REDUCE POINTER TO OLD POSITION
        LD      A,(BC)                            ;GET BYTE OF OLD DATA
        LD      (HL),A                            ;SAVE IN NEW POSITION
        LD      A,C                               ;GET LOW ADDRESS
        CP      E                                 ;TEST AGAINST WHERE WE ARE GOING
        JP      NZ,IL01                           ;IF NOT, KEEP COPYING
        LD      A,B                               ;GET HIGH ADDRESS
        CP      D                                 ;TEST AGAINST DESTINATION
        JP      NZ,IL01                           ;IF NOT SAME, KEEP COPYING
        LD      DE,BUFF                           ;GET ADDRESS OF NEW LINE
        CALL    GETLN                             ;OBTAIN NUMBERS
        LD      A,H                               ;GET HIGH 2 DIGITS
        LD      (BC),A                            ;SAVE IN NEW LINE
        INC     BC                                ;POINT TO NEXT CHARACTER OF NEW LINE
        LD      A,L                               ;GET LOW DIGITS
        LD      (BC),A                            ;SAVE IN NEW LINE
        INC     BC                                ;ADVANCE TO NEXT CHARACTER IN NEW LINE
        POP     HL                                ;RESTORE LENGTH
        LD      A,L                               ;GET LENGTH
        ADD     A,$10                             ;ADD OFFSET TO MAKE UNIQUE
        LD      (BC),A                            ;SAVE IN NEW LINE
        INC     BC                                ;POINT TO NEXT CHARACTER
IL02:
        LD      A,(DE)                            ;GET CHARACTER FROM NEW LINE IN BUFFER
        LD      (BC),A                            ;SAVE IN TEXT
        INC     BC                                ;POINT TO NEXT POSITION IN TEXT
        INC     DE                                ;ADVANCE IN BUFFER
        CP      $0D                               ;TEST FOR END OF LINE
        JP      NZ,IL02                           ;IF NOT, KEEP LOOKING
;* WE ARE INSERTING OR REPLACEING A LINE, SINCE WE DON'T KNOW HOW MUCH
;* MEMORY IT WILL R.EQUIRE, WE MUST CLEAR THE ARRAYS, AS THEY FOLLOW THE
;* PROGRAM. WE DO NOT HAVE TO DO THIS WHEN DELETING LINES
        CALL    CLRARY                            ;CLEAR ARRAYS AND RETURN
        JP      TOP                               ;GO BACK FOR NEXT COMMAND
;*
;* LOCATE LINE IN TEXT, SYNTAX ERROR IF NOT LINE NUMBER
;*
FNDLIN:
        CALL    NUM                               ;IS IT A VALID NUMBER
        JP      C,SYNT                            ;IF NOT, IT'S A INVALID
;*
;* FINDS LINE IN PROGRAM TEXT. RETURNS WITH Z FLAG SET IF LINE EXISTS
;* H-L POINTS TO START OF LINE. B-C CONTAINS LINE NUMBER OF ACTUAL
;* LINE FOUND. (IF LINE NOT FOUND, POINTS TO FIRST GREATER LINE NUMBER)
;*
LINEF:
        CALL    GETLN                             ;GET LINE NUMBER FROM COMMAND BUFFER
        EX      DE,HL                             ;SWAP TO D-E
        LD      HL,TEXT                           ;START AT TOP OF PROGRAM
TRY:
        LD      A,(HL)                            ;GET FIRST CHARACTER FROM PROGRAM LINE
        CP      $FF                               ;TEST FOR END OF FILE
        JP      Z,EOF                             ;IF SO, WE DIDN'T FIND
        INC     HL                                ;ADVANCE POINTER TO LOW DIGITS
        CP      D                                 ;TEST FOR HIGH DIGITS CORRECT
        JP      C,NEXTL                           ;IF LESS, FIND NEXT LINE
        JP      NZ,NOTFND                         ;IF GREATER, LINE WASN'T FOUND
        LD      A,(HL)                            ;GET LOW DIGITS
        CP      E                                 ;TEST LOW DIGITS
        JP      NC,NOTFND                         ;IF LESS, LINE IS HERE OR DOSN'T EXIST
;* ADVANCE TO NEXT LINE IN SOURCE
NEXTL:
        INC     HL                                ;POINT TO LINE LENGTH
        LD      A,(HL)                            ;GET LENGTH
        SUB     $11                               ;SUBTRACT OFFSET USED TO MAKE IT UNIQUE
        ADD     A,L                               ;ADD TO POINTER
        LD      L,A                               ;AND REPLACE IN POINTER
        JP      NC,TRY                            ;IF NO CARRY, THATS IT
        INC     H                                 ;BUMP HIGH ADDRESS
        JP      TRY                               ;AND;TEST THIS LINE
;* LINE IS HERE OR BEFORE
NOTFND:
        DEC     HL                                ;BACK UP TO DIGIT
        LD      C,A                               ;PLACE LOW ORDER DIGIT IN C
        LD      B,(HL)                            ;PLACE HIGH ORDER DIGIT IN B
        CP      E                                 ;TEST FOR LINE FOUND
        RET     NZ                                ;IF NOT SAME, RETURN INDICATING SO
        LD      A,B                               ;GET HIGH DIGIT
        CP      D                                 ;INDICATE IF NUMBERS SAME
        RET
;* LINE WAS GREATER THAN ALL LINES IN PROGRAM, INDICATE EOF REACHED
EOF:
        LD      B,A                               ;RETURN HIGH LINE NUMBER
        AND     A                                 ;INDICATE LINE DOSN'T EXIST
        RET
;*
;* PRINTS PACKED-DECIMAL LINE NUMBER ON TERMINAL, AS WELL AS PLACEING
;* IT AT THE START OF THE EDIT BUFFER
;*
PNUM:
        PUSH    DE                                ;
        LD      D,'0'                             ;
        CALL    HPOUT                             ;PRINT FIRST TWO DIGITS
        CALL    HPOUT                             ;
        POP     DE
        RET

HPOUT:
        LD      A,(HL)                            ;GET CONTENTS OF MEMORY
        INC     HL                                ;AND POINT TO NEXT
        PUSH    AF                                ;SAVE FOR LATER
        RRCA                                      ;ROTATE
        RRCA                                      ;UPPER DIGIT
        RRCA                                      ;INTO
        RRCA                                      ;LOWER DIGIT
        CALL    POUT                              ;DISPLAY UPPER DIGIT
        POP     AF                                ;GET LOWER DIGIT BACK
        CALL    POUT
        RET

;* DISPLAYS ONE DIGIT
POUT:
        AND     $0F                               ;REMOVE UPPER GARBAGE
        OR      $30                               ;CONVERT TO ASCII DIGIT
        CP      D                                 ;
        RET     Z                                 ;
        CALL    OUT                               ;DISPLAY DIGIT ON TERMINAL AND RETURN
        XOR     A                                 ;
        LD      D,A
        RET



;*
;******************************************************************
;*                   BASIC COMMAND HANDLERS
;******************************************************************
;*
;* IT'S A 'LIST' COMMAND, LETS GIVE HIM A PEEK AT THE SOURCE
;* ALSO PLACE LAST LINE LISTED IN BUFFER, INCASE HE WANTS TO EDIT IT
;*
LIST:
        PUSH    DE                                ;SAVE PROGRAM POINTER
        LD      HL,TEXT                           ;START AT THE BEGINNING OF THE PROGRAM
        LD      B,255                             ;SET ENDING LINE BEYOND END OF TEXT
        LD      A,(DE)                            ;GET CHARACTER OF OPERAND
        CALL    NUM                               ;TEST FOR A NUMBER
        JP      C,GO                              ;IF NOT, LIST WHOLE THING
        CALL    GETLN                             ;GET LINE NUMBER
        PUSH    HL                                ;SAVE ON STACK
        INC     DE                                ;POINT TO NEXT CHARACTER
        CP      ','                               ;TEST FOR ENDING NUMBER
        CALL    Z,LINEF                           ;IF SO, GET ENDING NUMBER
        INC     HL                                ;ADVANCE PAST BEGINNING OF LINE
        POP     DE                                ;GET STARTING LINE NUMBER BACK
        PUSH    HL                                ;SAVE ENDING LINE
        CALL    LINEF+4                           ;FIND STARTING LINE ADDRESS
        POP     BC                                ;GET ENDING ADDRESS BACK
;* LIST TEXT FROM STARTING LINE IN H-L TO ENDING LINE IN B-C
GO:
        LD      A,(HL)                            ;GET CHARACTER FROM START OF LINE
        INC     A                                 ;TEST FOR END OF FILE
        JP      Z,LIRET                           ;IF SO, STOP LISTING
        CALL    PNUM                              ;DISPLAY LINE NUMBER AND BUFFER IT
        INC     HL                                ;SKIP LENGTH BYTE, AS IT DOSN'T LOOK PRETTY
PRINS:
        LD      A,(HL)                            ;GET CHARACTER FROM LINE
        LD      (DE),A                            ;PLACE INTO BUFFER
        INC     DE                                ;ADVANCE IN BUFFER
        CALL    OUT                               ;DISPLAY ON TERMINAL
        INC     HL                                ;ADVANCE POINTER IN PROGRAM
        CP      $0D                               ;TEST FOR END OF LINE
        JP      NZ,PRINS                          ;IF NOT, KEEP PRINTING
        CALL    NL                                ;NEW LINE ON TERMINAL
        CALL    COMP                              ;TEST FOR LAST LINE LISTED
        JP      NC,LIRET                          ;IF SO, STOP LISTING
        CALL    breakx                            ;TEST FOR ABORT FROM TERMINAL
        JP      NC,GO                             ;KEEP LISTING IF NOT
LIRET:
        POP     DE                                ;RESTORE PROGRAM POINTER
        RET
;*
;* CLEARS VARIABLES AND ARRAYS. (INITIALIZES THEM) AND INITIALIZES EDIT BUFFER
;*
CLEAR:
        LD      HL,VARS                           ;POINT TO VARIABLE SPACE
        LD      C,52                              ;26 VARIABLE TIMES 2 BYTES/VARIABLE
CVLP:
        LD      (HL),0                            ;CLEAR INTEGER VARAIBLES TO ZERO
        INC     HL                                ;ADVANCE TO NEXT BYTE OF VARIABLE SPACE
        DEC     C                                 ;REDUCE COUNT OF VARAIABLES LEFT
        JP      NZ,CVLP                           ;KEEP GOING TILL ALL INTEGERS ARE ZERO'ED
        LD      A,VARSEND & $FF00/$100            ;ADDRESS OF END OF VARIABLE TABLE
        LD      BC,10                             ;SKIP AHEAD 10 BYTES
        ADD     HL,BC                             ;SO THAT WE DON'T CLOBBER OUR FLAGS
CVL1:
        LD      (HL),255                          ;$FF IS NULL CHARACTER FOR CHAR. VARS
        INC     HL                                ;POINT TO NEXT BYTE IN CHAR. VAR. SPACE
        CP      H                                 ;TEST FOR COMPLETE (ALL SET TO NULL STRINGS)
        JP      NZ,CVL1                           ;KEEP GOING TILL WE DO THEM ALL
;* INITIALIZE ARRAYS, RESET ARRAY SPACE TO FIRST PAGE FOLLOWING PROGRAM
CLRARY:
        CALL    GETEOF                            ;GET ADDRESS OF FIRST FREE PAGE+SET POINTER
        LD      (ARYLOC),A                        ;STASH IN ARRAY TABLE POINTER
        LD      H,A                               ;PLACE IN H, SO WE CAN REFERENCE INDERECT
        LD      L,52                              ;START AT END OF TABLE
        LD      (LAST),HL                         ;INDICATE FREE SPACE FOR NEXT ARRAY
        SUB     A                                 ;GET A ZERO
CALS:
        DEC     L                                 ;BACK UP IN TABLE
        LD      (HL),A                            ;INITIALIZE TO INDICATE NO ARRAY
        JP      NZ,CALS                           ;KEEP GOING TILL TABLE IS CLEARED
;* RESET CONTROL STACK AND DATA POINTER
RESET:
        LD      HL,CS                             ;GET USER STACK POINTER
        LD      (CSP),HL                          ;INITIALZE USER STACK POINTER
        LD      HL,0                              ;GET A ZERO (NO DATA POINTER)
        LD      (DATA),HL                         ;INSURE NO DATA PRESENT
        RET
;*
;* ** WE'VE GOTTEN A 'RUN' COMMAND, LETS START THE PROGRAM ROLLING **
;*
RUN:
        LD      A,(TEXT)                          ;GET FIRST CHARACTER OF PROGRAM
        LD      HL,NP                             ;AND ADDRESS OF 'NO PROGRAM' MESSAGE
        INC     A                                 ;TEST FOR EXISTANCE OF PROGRAM
        JP      Z,ERR                             ;IF NOT, POINT OUT HIS MISTAKE
        CALL    CLEAR                             ;CLEAR VARIABLES AND ARRAYS
        LD      DE,TEXT                           ;START INTERPRETING AT THE BEGINNING
RGON:
        LD      A,255                             ;INDICATE THAT WE ARE RUNNING
        LD      (RFLAG),A                         ;BY SETTING THIS FLAG
RNEWL:
        INC     DE                                ;SKIP PACKED DECIMAL LINE
        INC     DE                                ;NUMBERS, AND THE LENGTH BYTE,
        INC     DE                                ;AS THE COMMAND FINDER WON'T LIKE IT
;* MAIN 'RUN' INTERPRETING LOOP
RLOOP:
        LD      SP,STACK                          ;REPAIR ANY DAMAGE
        CALL    breakx                            ;TEST FOR 'MAGIC' CONTROL-C CHARACTER
        JP      C,STOP                            ;IF SO, FAKE A 'STOP' COMMAND
        LD      HL,PTAB-1                         ;POINT TO PROGRAM COMMAND TABLE
        CALL    CMD                               ;RUN PROGRAM CODE
;* ADVANCE TO NEXT STATEMENT
RNEXT:
        LD      A,(DE)                            ;GET CHARACTER FROM SOURCE
        CP      $22                               ;TEST FOR A QUOTE
        CALL    Z,SKPQUO                          ;IF SO, SEARCH FOR NEXT ONE
        INC     DE                                ;ADVANCE TO NEXT CHARACTER
        CP      ':'                               ;TEST FOR COLON (NEW STATEMENT)
        JP      Z,RLOOP                           ;IF SO, EXECUTE NEXT COMMAND
        CP      $0D                               ;TEST FOR CARRIAGE RETURN
        JP      NZ,RNEXT                          ;IF NOT, KEEP LOOKING
        LD      A,(DE)                            ;GET FIRST CHAR OF NEW LINE
        INC     A                                 ;TEST FOR $FF (END OF FILE)
        JP      Z,INIT                            ;IF SO, GO BACK TO COMMAND MODE
        JP      RNEWL                             ;EXECUTE THIS LINE
;*
;* EITHER WE HAVE GOTTEN A 'STOP' COMMAND, OR THE OPERATOR PRESSED
;* CONTROL-C, EITHER WAY, PRINT THE MESSAGE AND EXIT
;*
STOP:
        LD      HL,STMSG                          ;ADDRESS OF 'STOP' MESSAGE
        JP      PERR                              ;TREAT IT LIKE AN ERROR
;* IT'S A 'THEN', FOLLOWING AN 'IF', LOOK FOR LINE NUMBER OR A STATEMENT
THEN:
        CALL    NUM                               ;IS IT A NUMBER?
        JP      NC,GOTO                           ;IF SO, ITS A NUMBER TO 'GOTO'
        JP      RLOOP                             ;IF NOT, ITS A STATEMENT TO EXECUTE
;*
;* IT'S A 'GOSUB' SAVE RETURN ADDRESS, AND PRETEND IT'S 'GOTO'
;*
GOSUB:
        CALL    PUSHD                             ;SAVE SOURCE POSITION
        SUB     A                                 ;INDICATE GOSUB ENTRY
        CALL    PUSHS                             ;SAVE ON USER STACK
        LD      A,(DE)                            ;RESTORE OPERAND CHARACTER
;*
;* IT'S A 'GOTO' MAKE THE BIG JUMP
;*
GOTO:
        CP      '('                               ;TEST FOR COMPUTED GOTO
        JP      NZ,NOON                           ;IF NO, NOT AN 'ON' STATEMENT
        CALL    EXPR                              ;GET VALUE OF INTERNAL EXPRESSION
GLPO:
        CALL    SKIP                              ;SKIP TO NEXT EXPRESSION
        CP      ','                               ;IF THERE IS NO MORE COMMA'S
        JP      NZ,SYNT                           ;THEN WE RAN OUT OF OPERANDS
GLPD:
        INC     DE                                ;SKIP THE COMMA
        DEC     L                                 ;REDUCE OUR COUNT
        JP      P,GLPO                            ;IF IT'S STILL POSITIVE, KEEP SKIPPING
        LD      A,(DE)                            ;GET CHARACTER FROM SOURCE
        CP      ' '                               ;TEST FOR BLANKS
        JP      Z,GLPD                            ;AND KEEP GOING TILL WE SKIP THEM
NOON:
        PUSH    DE                                ;SAVE POSITION (IN CASE WE FAIL)
        CALL    FNDLIN                            ;FIND THE LINE HE WANTS
        POP     DE                                ;RESTORE OUR POSITION
        EX      DE,HL                             ;SWAP NEW POS INTO D-E
        JP      Z,RGON                            ;IF SUCESS, GOTO NEW LINE
        EX      DE,HL                             ;SWAP BACK
;*
;* OH OH, LOOKS LIKE HE'S TRIED TO GOTO, GOSUB OR ORDER TO A LINE HE FORGOT
;* TO TYPE IN, TELL HIM ABOUT IT AND LET HIM TRY TO FIGURE IT OUT
;*
BADLIN:
        LD      HL,LIN                            ;ADDRESS OF 'LINE NUMBER' MESSAGE
        JP      ERR                               ;HANDLE LIKE ANY ERROR
;*
;* IT'S A 'RETURN', HOPE SOMEBODY DID A 'GOSUB' SOMEWHERE
;*
RETURN:
        CALL    POPS                              ;GET TYPE OF STACK ENTRY
        AND     A                                 ;TEST FOR 'GOSUB' ENTRY
        JP      Z,POPD                            ;IF SO, GET ADDRESS BACK AND RETURN
;*
;* HE SCREWED UP THE FOR/NEXT, GOSUB/RETURN NESTING
;* LET HIM IN ON IT AND DIE WHILE WE CAN
;*
NSTERR:
        LD      HL,CSTK                           ;ADDRESS OF 'NESTING' MESSAGE
        JP      ERR                               ;HANDLE LIKE ANY ERROR
;*
;* IT'S A 'FOR' COMMAND, LETS THROW THIS THING FOR A LOOP
;*
FOR:
        CALL    VCHAR                             ;INSURE IT'S A VARIABLE
        PUSH    AF                                ;SAVE IT (IT'S THE LOOP INDEX VARIABLE)
        DEC     DE                                ;BACK UP POINT JUST BEFORE EXPRESSION
        LD      A,E                               ;GET LOW ADDRESS
        LD      (P),A                             ;AND PLACE IN POSITION FLAG
FINTO:
        LD      A,(DE)                            ;GET CHARACTER FROM SOURCE
        CP      $0D                               ;TEST FOR END OF LINE
        JP      Z,SYNT                            ;IF SO, HE'S GOOFED
        INC     DE                                ;SKIP TO NEXT
        CP      'T'                               ;TEST FOR A 'T'
        JP      NZ,FINTO                          ;IF NOT, WE ARN'T THERE YET
        LD      A,(DE)                            ;GET NEXT CHARACTER
        CP      'O'                               ;IS IT 'TO'
        JP      NZ,FINTO                          ;NO, MUST BE VARIABLE 'T'
        PUSH    DE                                ;SAVE OUR POSITION
        DEC     DE                                ;BACKUP TO THE 'T'
        CALL    DOEXP                             ;EVALUATE EXPRESSION
        POP     DE                                ;RESTORE OUT POSITION
        INC     DE                                ;SKIP 'O'
        CALL    EXPR                              ;GET LIMIT EXPRESSION
        CALL    PUSHD                             ;SAVE OUR POSITION ON STACK
        EX      DE,HL                             ;GET LIMIT VALUE
        CALL    PUSHD                             ;SAVE ON STACK
        EX      DE,HL                             ;GET POSITION BACK
        POP     AF                                ;GET INDEX VARIABLE NAME
;*
;* SAVES A SINGLE BYTE ENTRY ON THE USER (CONTROL) STACK
;*
PUSHS:
        PUSH    HL                                ;SAVE H-L
        LD      HL,(CSP)                          ;GET STACK POINTER
        LD      (HL),A                            ;SAVE BYTE ON STACK
PSH1:
        DEC     HL                                ;REDUCE POINTER
PSH2:
        LD      (CSP),HL                          ;RESAVE STACK POINTER
        POP     HL                                ;RESTORE H-L
        RET
;*
;* POP A SINGLE BYTE ENTRY FROM THE USER (CONTROL) STACK
;*
POPS:
        PUSH    HL                                ;SAVE H-L
        LD      HL,(CSP)                          ;GET STACK POINTER
        INC     HL                                ;ADVANCE TO NEXT ENTRY
        LD      A,(HL)                            ;GET BYTE BACK
        JP      PSH2                              ;SAVE POINTER AND CONTINUE
;*
;* PUSHES A DOUBLE BYTE ENTRY ON THE USER (CONTROL) STACK
;*
PUSHD:
        PUSH    HL                                ;SAVE H-L
        LD      HL,(CSP)                          ;GET STACK POINTER
        LD      (HL),D                            ;SAVE HIGH BYTE
        DEC     HL                                ;BACK UP
        LD      (HL),E                            ;SAVE LOW BYTE
        JP      PSH1                              ;SAVE POINTER AND CONTINUE
;*
;* POPS A DOUBLE BYTE ENTRY FROM THE USER STACK
;*
POPD:
        PUSH    HL                                ;SAVE H-L
        LD      HL,(CSP)                          ;GET STACK POINTER
        INC     HL                                ;ADVANCE TO LAST ENTRY
        LD      E,(HL)                            ;GET LOW BYTE
        INC     HL                                ;ADVANCE TO HIGH BYTE
        LD      D,(HL)                            ;GET HIGH BYTE
        JP      PSH2                              ;SAVE AND CONTINUE
;*
;* LET COMMAND, EVALUATE EXPRESSION
;*
LET:
        CALL    EXPR                              ;EVALUATE EXPRESSION
        LD      A,(EFLAG)                         ;DID HE MAKE AN ASSIGNMENT?
        AND     A                                 ;IF NOT..
        JP      Z,SYNT                            ;HE'S MADE ANOTHER MISTAKE
        SUB     A                                 ;RESET THE FLAG
        LD      (EFLAG),A                         ;SO WE KNOW WHEN HE SCREW'S UP AGAIN
        RET
;*
;* IT'S A NEXT COMMAND,;TEST INDEX AGAINST LIMIT, AND LOOP IF NEEDED
;*
NEXT:
        CALL    VCHAR                             ;TEST FOR VALID VARIABLE
        LD      B,A                               ;STASH IN B FOR SAFEKEEPING
        LD      HL,(CSP)                          ;SAVE CONTROL STACK POINTER..
        LD      (TEMPBAS),HL                      ;IN CASE WE NEED TO LOOP AGAIN
        CALL    POPS                              ;GET VARIABLE NAME FROM STACK
        CP      B                                 ;TEST FOR WHAT HE GAVE US
        JP      NZ,NSTERR                         ;IF NOT, HE'S SCREWED UP THE NESTING
        CALL    LOOK                              ;GET VARIABLE VALUE
        PUSH    DE                                ;SAVE POSITION
        CALL    POPD                              ;GET LIMIT FROM STACK
        LD      B,D                               ;GET LIMIT
        LD      C,E                               ;INTO B-C SO WE CAN 'COMP'
        CALL    COMP                              ;TEST IF INDEX >= LIMIT
        JP      NC,NOMORE                         ;IF SO, DON'T LOOP ANYMORE
        POP     DE                                ;GET POSITION BACK
        INC     HL                                ;INCREMENT LOOP INDEX
        LD      A,(DE)                            ;GET VARIABLE NAME BACK
        CALL    STOR                              ;SAVE IT AWAY
        CALL    POPD                              ;GET NEW POSITION
        LD      HL,(TEMPBAS)                      ;GET CONTROL-STACK POINTER
        LD      (CSP),HL                          ;AND REPLACE IT (LEAVING STACK UNCHANGED)
        RET
;* WE HAVE HIT THE END OF A FOR NEXT LOOP
NOMORE:
        CALL    POPD                              ;CLEAN UP CONTROL STACK
        POP     DE                                ;GET PROGRAM COUNTER BACK
;*
;* REMARK, DO NOTHING, BUT RETURN, ALLOWING 'RNEXT' TO SKIP THE COMMAND
;*
REM:
        RET
;*
;* IT'S AN 'IF' STATEMENT. FIND OUT 'IF' WE DO IT OR NOT
;*
IF:
        DEC     DE                                ;BACK UP IN SOURCE
        LD      A,E                               ;GET LOW ADDRESS
        LD      (P),A                             ;SAVE IN POSITION POINTER
FTHEN:
        LD      A,(DE)                            ;GET CHARACTER FROM SOURCE
        CP      $0D                               ;IF IT'S A CARRIAGE RETURN..
        JP      Z,SYNT                            ;THEN HE DIDN'T TYPE IN A 'THEN'
        INC     DE                                ;ADVANCE TO NEXT CHARACTER
        CP      'T'                               ;IS IT A 'T'?
        JP      NZ,FTHEN                          ;IF NOT, IT AINT THE START OF 'THEN'
        LD      A,(DE)                            ;GET NEXT CHARACTER
        CP      'H'                               ;TEST FOR NEXT CHARACTER OF 'THEN'
        JP      NZ,FTHEN                          ;NO, MUST BE VARIABLE 'T' (OR HE CAN'T SPELL)
        DEC     DE                                ;BACK UP TO 'T'
        PUSH    DE                                ;SAVE POSITION IN SOURCE
        CALL    DOEXP                             ;EVALUATE CONDITION EXPRESSION
        POP     DE                                ;GET POSITION BACK
        LD      A,H                               ;GET RESULT AND;TEST.
        OR      L                                 ;IT FOR ZERO (FALSE)
        RET     Z                                 ;IF SO, SKIP THIS STATEMENT
        JP      RLOOP                             ;EXECUTE THE 'THEN'
;*
;* LONG IF, CONTROLS REMAINDER OF ENTIRE LINE
;*
LIF:
        CALL    IF                                ;CALCULATE AND PROCESS IF TRUE
LNXT:
        INC     DE                                ;ADVANCE IN SOURCE
        LD      A,(DE)                            ;GET CHARACTER FROM SOURCE
        CP      $0D                               ;TEST FOR END OF LINE
        JP      NZ,LNXT                           ;KEEP LOOKING
        RET
;*
;* IT A 'PLOT' COMMAND, (HE'S PLOTTING AGAINST US)
;*
PLOT:
        CALL    chput_remove_cursor               ;
        CALL    EXPR                              ;GET X COORDINATE
        JP      C,SYNT                            ;IF CHARACTER, IT'S NO GOOD
        PUSH    HL                                ;SAVE X COORDINATE
        INC     DE                                ;ADVANCE PAST ','
        CALL    EXPR                              ;GET Y COORDINATE
        POP     BC                                ;GET X POSITION IN B-C
        LD      A,C
        LD      H,A
        CALL    posit                             ;POSITION THE CURSOR
;* LOOK FOR END OF LINE, OR OTHER OPERANDS
        CALL    SKIP                              ;GET NEXT SEPERATOR
        CP      ','                               ;TEST FOR COMMA
        RET     NZ                                ;WE ARE DONE
        INC     DE                                ;SKIP ','
;*
;* PRINT STATEMENT, LET'S OUTPUT SOMETHING SO HE WON'T GET UPSET
;* WHILE STAREING AT THE TUBE WONDERING IF WE DIED
;*
PRINT:
        CALL    EXPR                              ;GET EXPRESSION TO PRINT
        PUSH    DE                                ;SAVE BASIC'S PROGRAM COUNTER
        CALL    NC,DECPRT                         ;IF NUMERIC, OUTPUT DECIMAL NUMBER
        POP     DE                                ;RESTORE BASIC'S PROGRAM COUNTER
        CALL    C,PV1                             ;IF CHARACTER, DISPLAY CHARACTER VALUE
        LD      A,(DE)                            ;GET CHARACTER FROM SOURCE
        CP      ','                               ;TEST FOR COMMA
        JP      NZ,NL                             ;IF NOT, IT'S THE END
        CALL    PARSE                             ;ADVANCE TO NEXT NON-BLANK
        JP      NZ,PRINT                          ;PRINT NEXT EXPRESSION
        RET
;* PRINT CHARACTER EXPRESSIONS
PV1:
        LD      HL,XBF                            ;EXPRESSION IS IN EXTRA BUFFER
PZ:
        LD      A,(HL)                            ;GET CHARACTER FROM EXPRESSION
        INC     HL                                ;POINT TO NEXT
        AND     A                                 ;TEST FOR END OF EXPRESSION
        RET     M                                 ;IF SO, END IT NOW
        CALL    OUT                               ;PRINT CHARACTER
        JP      PZ                                ;KEEP GOING TILL END
;* RECURSIVE ROUTINE OUTPUTS NUMBER IN DECIMAL
DECPRT:
        CP      '('                               ;TEST FOR SPECIAL CASE
        CALL    NZ,SPACE                          ;IF NOT, PRECEDE WITH SPACE
DECP1:
        LD      BC,10                             ;DIVIDE BY 10
        CALL    DODIV                             ;PERFORM DIVISION
        LD      A,$30                             ;TO CONVERT TO ASCII
        ADD     A,L                               ;GET DIGIT
        PUSH    AF                                ;SAVE FOR OUTPUT
        EX      DE,HL                             ;SWAP, REMAINDER IS NOW IN HL
        LD      A,H                               ;GET HIGH BYTE
        OR      L                                 ;TEST FOR ZERO, (FINISHED)
        CALL    NZ,DECP1                          ;IF NOT, GET NEXT VALUE
        POP     AF                                ;GET DIGIT OFF STACK
        JP      OUT                               ;DISPLAY AND RETURN
;*
;* IT'S AN 'INPUT', LETS GIVE HIM A CHANCE TO DO SOME TYPING.. BUT
;* KEEP AN EYE ON HIM, IN CASE HE TRY'S TO PUT SOMETHING OVER ON US
;*
INPUT:
        CALL    CLBF                              ;CLEAR EXTRA TEXT BUFFER
        LD      A,'?'                             ;GET A QUESTION MARK.
        LD      (XBF),A                           ;TO USE AS THE DEFAULT PROMPT
        LD      A,(DE)                            ;GET FIRST CHAR OF OPERAND
        CP      $22                               ;TEST FOR USER SUPPLIED PROMPT
        JP      NZ,INP1                           ;IF NOT, DON'T CHANGE EXISTING ONE
        CALL    EXPR                              ;EVALUATE USER SUPPLIED PROMPT
        CALL    PARSE                             ;SKIP TO NEXT NON-BLANK
INP1:
        CALL    VCHAR                             ;TEST FOR VALID VARIABLE NAME
        PUSH    DE                                ;SAVE SOURCE POSITION
        INC     DE                                ;ADVANCE TO NEXT CHARACTER
        PUSH    AF                                ;SAVE VARIABLE NAME
        LD      A,(DE)                            ;GET NEXT CHARACTER
        CP      '$'                               ;TEST FOR CHARACTER INPUT
        JP      Z,GCHR                            ;IF SO, GET CHARACTER DATA
        LD      HL,0                              ;START WITH A ZERO
        ADD     HL,SP                             ;AND GET STACK POINTER. IN CASE WE BLOW UP
        LD      (TEMPBAS),HL                      ;SAVE SO WE CAN GET IT BACK LATER
RETRY:
        CALL    PV1                               ;DISPLAY USER PROMPT
        SUB     A                                 ;GET A ZERO
        LD      (IFLAG),A                         ;AND CLEAR THE INPUT FLAG (IN CASE HE CTRL-C'S)
        CALL    GLINE                             ;GET A LINE FROM THE TERMINAL
        LD      (IFLAG),A                         ;SET IFLAG. (SO GET ERROR, WE CAN COME BACK)
        CALL    EXPR                              ;EVALUATE EXPRESSION
        POP     AF                                ;RESTORE VARAIABLE NAME
        CALL    STOR                              ;STASH VALUE IN VARIABLE
        POP     DE                                ;RESTORE SOURCE POSITION
        SUB     A                                 ;GET A ZERO
        LD      (IFLAG),A                         ;AND CLEAR IFLAG
        RET
;* CHARACTER INPUT
GCHR:
        CALL    PV1                               ;DISPLAY USER PROMPT
        CALL    GLINE                             ;GET A LINE OF INPUT
        POP     AF                                ;GET VARIABLE BACK
        CALL    LTA                               ;GET THE TEXT VARIABLE'S ADDRESS
        LD      B,35                              ;LENGTH IS 35
Z1:
        LD      A,(DE)                            ;GET CHARACTER FROM INPUT BUFFER
        CP      $0D                               ;TEST FOR END OF BUFFER
        JP      Z,Z2                              ;IF SO, END THE LINE
        DEC     B                                 ;TEST FOR END OF VARIABLE SPACE
        JP      Z,Z2                              ;IF SO, END THE LINE
        LD      (HL),A                            ;SAVE IN VARIABLE SPACE
        INC     HL                                ;NEXT CHARACTER IN VARIABLE
        INC     DE                                ;NEXT CHARACTER IN INPUT BUFFER
        JP      Z1                                ;COPY NEXT CHARACTER
Z2:
        POP     DE                                ;RESTORE SOURCE POSITION
Z3:
        LD      (HL),$FF                          ;PAD BUFFER WITH NULL CHARACTERS
        INC     HL                                ;NEXT POSITION IN VARIABLE
        DEC     B                                 ;REDUCE COUNT TILL END
        JP      P,Z3                              ;KEEP GOING TILL VARIABLE IS FILLED
        RET
;*
;* LOOK'S LIKE HE CAN'T EVEN ENTER A SIMPLE NUMBER, CLEAN UP ANY STACK
;* HE MAY HAVE USED, AND LET HIM TAKE ANOTHER BLIND STAB AT THE KEYBOARD
;*
INERR:
        LD      HL,IERMS                          ;GET NASTY MESSAGE
        CALL    PMSG                              ;GIVE HIM THE BAD NEWS
        LD      HL,(TEMPBAS)                      ;GET HIS OLD STACK BACK
        LD      SP,HL                             ;RESET HIS STACK
        JP      RETRY                             ;LET HIM TRY AGAIN
;*
;* DIMENSION, HE WANTS SOME ARRAY SPACE.. I SUPPOSE WE SHOULD GIVE IT TO HIM
;*
DIM:
        LD      A,E                               ;GET ADDRESS OF OUR POSITION
        LD      (P),A                             ;SAVE IN POSITION POINTER
DIM0:
        LD      A,(DE)                            ;GET CHARACTER FROM SOURCE
        INC     DE                                ;ADVANCE TO NEXT
        CP      $0D                               ;TEST FOR END OF LINE
        JP      Z,SYNT                            ;IF SO, TELL HIM TO STRAIGHTEN UP
        CP      ')'                               ;TEST FOR START OF ARRAY DIMENSION
        JP      NZ,DIM0                           ;IF NOT, KEEP LOOKING
        DEC     DE                                ;BACK UP FOR EXPRESSION
        PUSH    DE                                ;SAVE BASIC PROGRAM COUNTER
        CALL    DOEXP                             ;EVALUATE ARRAY SIZE
        INC     HL                                ;ADD ONE ENTRY (ZERO ENTRY DOES EXIST)
        ADD     HL,HL                             ;DOUBLE BECAUSE THEY ARE 16 BIT'S
        DEC     DE                                ;BACK UP TO NAME
        LD      B,H                               ;COPY SIZE NEEDED
        LD      C,L                               ;INTO B AND C
        LD      HL,(LAST)                         ;GET FREE ADDRESS
        PUSH    HL                                ;SAVE FOR TABLE
DLOOP:
        LD      (HL),0                            ;ZERO ARRAY BYTE
        INC     HL                                ;ADVANCE TO NEXT
        DEC     BC                                ;REDUCE COUNT
        LD      A,B                               ;GET HIGH BYTE OF REMAINING TO DO
        OR      C                                 ;TEST FOR NONE LEFT
        JP      NZ,DLOOP                          ;IF NOT, KEEP ZEROING
        LD      (LAST),HL                         ;SAVE NEXT FREE SPACE INDICATOR
        POP     HL                                ;GET ADDRESS OF ARRAY BACK
        LD      A,(DE)                            ;GET ARRAY NAME
        CALL    TABENT                            ;GET TABLE ENTRY
        LD      A,H                               ;GET HIGH ADDRESS
        LD      (BC),A                            ;PLACE IN TABLE
        INC     BC                                ;ADVANCE IN TABLE
        LD      A,L                               ;GET LOW ADDRESS
        LD      (BC),A                            ;PLACE IN TABLE
        POP     DE                                ;GET SOURCE POSITION BACK
        CALL    PARSE                             ;ADVANCE TO NEXT NON-BLANK
        RET     Z
        CP      ','                               ;TEST FOR ANOTHER OPERAND
        JP      Z,DIM                             ;IF SO, KEEP GOING
        SUB     A                                 ;CAUSE A SYNTAX ERROR BECAUSE WE ARE BAD
;*
;* LOCATES TABLE POSITION OF AN ARRAY
;*
TABENT:
        CALL    VCHAR                             ;INSURE IT'S OK
        SUB     'A'                               ;CONVERT TO BINARY
        ADD     A,A                               ;X 2 FOR TWO BYTE ENTRIES
        LD      C,A                               ;SAVE IN C
        LD      A,(ARYLOC)                        ;GET ARRAY PAGE
        LD      B,A                               ;SAVE IN HIGH ZBYTE
        RET
;* LOOKS UP AN ARRAY VALUE
ALOOK:
        CALL    DOEXP                             ;CALCULATE INDEX VALUE
        DEC     DE                                ;BACK UP PAST '['
        LD      A,(DE)                            ;GET VARIABLE NAME
        CALL    LOOKT                             ;FIND ADDRESS OF ENTRY
        LD      B,(HL)                            ;GET HIGH BYTE OF ENTRY
        INC     HL                                ;ADVANCE TO LOW BYTE
        LD      C,(HL)                            ;GET LOW BYTE OF ARRAY ENTRY
        LD      H,B                               ;TRANSFER RESULT TO .
        LD      L,C                               ;H AND L WHERE THEY ARE EXPECTED
        LD      A,(DE)                            ;GET VARIABLE NAME BACK
        CP      '@'                               ;TEST FOR MAGIC 'PEEK' ARRAY
        RET     NZ                                ;IF NOT, WE ARE OK
        LD      L,H                               ;SET VALUE TO THAT OF FIRST BYTE
        LD      H,0                               ;AND ELIMINATE HIGH BYTE
        RET
;*
;* LOCATES ADDRESS OF AN ARRAY ENTRY IN THE ARRAY TABLE. INDEX IN HL
;*
LOOKT:
        CP      '@'                               ;TEST FOR SPECIAL CASE
        RET     Z                                 ;IF SO, PEEK AT ADDRESS
        CALL    TABENT                            ;LOCATE TABLE ENTRY
        PUSH    DE                                ;SAVE BASIC PROGRAM COUNTER
        LD      A,(BC)                            ;GET FIRST BYTE
        LD      D,A                               ;COPY TO HIGH BYTE
        INC     BC                                ;ADVANCE TO NEXT
        LD      A,(BC)                            ;GET LOW BYTE
        LD      E,A                               ;COPY TO D
        ADD     HL,HL                             ;X TWO FOR TWO BYTE ENTRIES
        ADD     HL,DE                             ;ADD IN OFFSET FOR START OF ARRAY
        OR      D                                 ;TEST FOR ADDRESS OF ZERO, = NOT DIMENSIONED
        POP     DE                                ;RESTORE PROGRAM COUNTER
        RET     NZ                                ;NOT A DIMENSION ERROR, GO BACK
;*
;* EITHER HE'S TRIED TO INDEX A NON-ARRAY VARIABLE, TRIED TO INDEX A CHARACTER
;* VARIABLE WITH A VALUE GREATER THEN 34, OR HE'S PLOTTED OUTSIDE OF THE SCREEN
;* NO MATTER WHAT HE'S DONE, GIVE HIM A NASTY MESSAGE SO HE WON'T DO IT AGAIN
;*
DIMERR:
        LD      HL,OVM                            ;ADDRESS OF NASTY MESSAGE
        JP      ERR                               ;GIVE IT TO HIM
;* LOCATES THE ADDRESS OF A CHARACTER (TEXT) VARIABLE
LTA:
        SUB     $41                               ;REDUCE TO SIMPLE BINARY
        CP      26                                ;TEST FOR VALID VARIABLE
        JP      NC,SYNT                           ;IF NOT, GET MAD
        LD      HL,VARS+25                        ;START OF CHARACTER VARIABLES (-37)
        LD      BC,37                             ;LENGTH OF CHARACTER VARIABLES
V1:
        ADD     HL,BC                             ;OFFSET INTO TABLE
        DEC     A                                 ;REDUCE VARIABLES WE HAVE TO GO
        JP      P,V1                              ;IF NOT FINISHED, KEEP OFFSETING
        RET
;*
;* IT'S A 'USR' COMMAND, FIND OUT WHAT HE WANT'S, PASS CONTROL  TO
;* HIS MACHINE LANGUAGE ROUTINE, AND GOD HELP HIM IF HE SCREW'S UP
;* BECAUSE WE CAN'T DO ANYTHING FOR HIM UNTIL HE RETURNS
;*
USR:
        LD      HL,URET                           ;GET ON STACK (SO HE CAN 'RET' TO IT)
        PUSH    HL                                ;SAVE IN MACHINE STACK
        CALL    EXPR                              ;EVALUATE ADDRESS
        PUSH    HL                                ;SAVE ON STACK (SO WE CAN 'RET' TO IT)
        LD      A,(DE)                            ;GET NEXT CHARACTER
        CP      ','                               ;TEST FOR MORE PARAMETERS
        JP      NZ,CSAV                           ;IF NOT, DONT EVALUATE
        INC     DE                                ;SKIP THE ','
        CALL    EXPR                              ;EVALUATE PARAMETER TO PASS
;* WHEN 'PUSHD' RETURNS, IT WILL EFFECT A JUMP TO HIS CODE
CSAV:
        JP      PUSHD                             ;SAVE PROGRAM POSITION
;* IF WE GET HERE, HE MADE IT BACK IN ONE PIECE
URET:
        CALL    POPD                              ;GET PROGRAM COUNTER BACK
        LD      A,(DE)                            ;GET CHARACTER FROM SOURCE
        CP      ','                               ;TEST FOR VARIABLE TO RECEIVE H-L
        RET     NZ                                ;IF NOT, WE ARE DONE
        CALL    PARSE                             ;KEEP LOOKING
;*
;* STORES H-L INTO A INTEGER VARIABLE PASSED IN A
;*
STOR:
        LD      BC,VARS                           ;GET ADDRESS OF VARIABLE TABLE
        SUB     $41                               ;CONVERT TO BINARY
        CP      26                                ;TEST FOR VALID VARIABLE NAME
        JP      NC,SYNT                           ;IF NOT, IT'S INVALID
        ADD     A,A                               ;DOUBLE BECAUSE THEY ARE 16 BIT ENTRIES
        LD      C,A                               ;PLACE IN C, MAKING COMPLETE ADDRESS
        LD      A,L                               ;GET HIGH VALUE TO SAVE
        LD      (BC),A                            ;SAVE IN VARIABLE
        INC     BC                                ;NEXT BYTE OF VARIABLE
        LD      A,H                               ;GET LOW BYTE
        LD      (BC),A                            ;SAVE IN VARIABLE
        RET
;*
;* RETERIVES CONTENTS OF A VARIABLE
;*
LOOK:
        LD      BC,VARS                           ;ADDRESS OF VARIABLES
        SUB     $41                               ;CONVERT NAME TO BINARY
        ADD     A,A                               ;DOUBLE FOR 16 BIT ENTRIES
        LD      C,A                               ;MAKE COMPLETE ADDRESS
        LD      A,(BC)                            ;GET HIGH BYTE
        LD      L,A                               ;PLACE IN H
        INC     BC                                ;NEXT BYTE OF VARIABLE
        LD      A,(BC)                            ;GET LOW BYTE
        LD      H,A                               ;PLACE IN L
        RET
;*
;* IT'S AN 'ORDER', (HE THINKS HE KNOWS WHERE THERE IS SOME DATA)
;*
ORDER:
        PUSH    DE                                ;SAVE OUR SOURCE POSITION
        CALL    FNDLIN                            ;GET ADDRESS OF THE LINE HE WANTS
        POP     DE                                ;RESTORE OUR POSITION
        PUSH    DE                                ;AND RESAVE OUR POSITION
        JP      NZ,BADLIN                         ;IF IT DOSN'T EXIST, THEN FORGET IT
        INC     HL                                ;SKIP FIRST TWO DIGITS OF LINE NUMBER
        INC     HL                                ;SKIP LAST TWO DIGITS OF LINE NUMBER
        INC     HL                                ;SKIP LENGTH BYTE
        EX      DE,HL                             ;MOVE TO D-E
        CALL    VERDAT                            ;GET STATEMENT FROM LINE
        LD      (DATA),HL                         ;SAVE DATA POINTER
        POP     DE                                ;RESTORE OUR LINE, (SO WE CAN TELL HIM)
        RET     Z
;*
;* DATA ERROR... ATTEMPT TO READ FROM A LINE WITHOUT 'DATA' OR
;* ATTEMPT TO READ THE WRONG DATA TYPE. LET HIM IN ON IT
;*
DERR:
        LD      HL,DTXT                           ;ADDRESS OF 'DATA' MESSAGE
        JP      SYNT+3                            ;DISPLAY IT
;*
;* IT'S A READ. (HE WANTS TO KNOW WHATS IN THAT DATA WE FOUND)
;*
READ:
        CALL    VCHAR                             ;IS IT A VALID VARIABLE
        PUSH    AF                                ;SAVE VARIABLE NAME
        INC     DE                                ;ADVANCE TO NEXT CHARACTER
        LD      A,(DE)                            ;GET NEXT CHARACTER
        CP      '$'                               ;IS IT A CHARACTER VARIABLE?
        JP      Z,CDAT                            ;IF SO, LOOK FOR CHARACTER DATA
;* NUMERIC DATA, FOR NUMERIC VARIABLE
        CALL    GETDAT                            ;GET NUMERIC DATA
        JP      C,DERR                            ;IF CHARACTER, IT A DATA TYPE ERROR
        POP     AF                                ;GET VARIABLE NAME BACK
        CALL    STOR                              ;STASH VALUE IN IT
        JP      MORDAT                            ;SEE IF HE WANT'S MORE DATA
;* CHARACTER DATA, FOR CHARACTER VARIABLE
CDAT:
        INC     DE                                ;SKIP DOLLAR SIGN
        CALL    GETDAT                            ;GET DATA
        JP      NC,DERR                           ;IF NUMERIC, IT'S BAD
        POP     AF                                ;GET VARIABLE NAME BACK
        CALL    LTA                               ;FIND IT'S ADDRESS
        LD      BC,XBF                            ;DATA IS IN EXTRA BUFFER
        PUSH    DE                                ;SAVE SOURCE POSITION
        LD      E,35                              ;MOVE 35 CHARACTERS
SL1:
        LD      A,(BC)                            ;GET CHARACTER FROM BUFFER. (DATA)
        LD      (HL),A                            ;STASH IT IN THE VARIABLE
        INC     BC                                ;SKIP TO THE NEXT CHARACTER IN THE BUFFER
        INC     HL                                ;SKIP TO THE NEXT POSITION IN VARIABLE
        DEC     E                                 ;REDUCE COUNT OF HOW MANY TO MOVE
        JP      NZ,SL1                            ;IF NOT FINISHED, KEEP COPYING
        POP     DE                                ;RESTORE SOURCE POSITION
;* LOOK FOR MORE VARIABLES (OPERANDS) IN THE 'READ' STATEMENT
MORDAT:
        CALL    PARSE1                            ;FIND NEXT NON-BLANK
        CP      ','                               ;IF COMMA..
        RET     NZ                                ;IF NOT, WE HAVE ALL THERE IS
        CALL    PARSE                             ;SKIP COMMA AND FIND VARIABLE NAME
        JP      READ                              ;GET MORE DATA FOR FOLLOWING VARIABLE
;* GETS DATA FROM THE DATA STATEMENTS, POINTED TO BY THE CURRENT READ POINTER
GETDAT:
        LD      HL,(DATA)                         ;GET DATA POINTER
        LD      A,H                               ;SEE IF IT IS ZERO.
        OR      L                                 ;WHICH INDICATES THAT IT WASN'T INITIALIZED
        JP      Z,DERR                            ;IF SO, IT'S A DATA ERROR
        PUSH    DE                                ;SAVE SOURCE POSIITION
        EX      DE,HL                             ;SWAP DATA POINTER TO D-E
        CALL    EXPR                              ;EVALUATE THE DATA EXPRESSION
        PUSH    AF                                ;SAVE THE CONDITION FLAGS
ENDAT:
        LD      A,(DE)                            ;GET NEXT CHARACTER FROM THE SOURCE
        CP      ','                               ;TEST FOR MORE DATA
        JP      Z,COMA                            ;IF SO, WE ARE OK
        CP      ':'                               ;TEST FOR END OF STATEMENT
        JP      Z,DAT1                            ;GO TO NEXT DATA STATEMENT
        INC     DE                                ;ADVANCE ONE CHARACTER
        CP      $0D                               ;CARRIAGE RETURN?
        JP      NZ,ENDAT                          ;KEEP LOOKING IF NOT
;* HIT THE END OF A LINE, SKIP TO NEXT DATA STATEMENT
        INC     DE                                ;SKIP FIRST TWO DIGITS
        INC     DE                                ;SKIP SECOND TWO DIGITS
DAT1:
        INC     DE                                ;SKIP LENGTH (OR ':' IF STMT)
        PUSH    HL                                ;SAVE H-L REG
        CALL    VERDAT                            ;CHECK FOR DATA STATEMENT
        EX      DE,HL                             ;SWAP POINTER BACK TO D-E
        POP     HL                                ;RESTORE REGISTERS
        JP      Z,GDEND                           ;RETURN, WITH NEW DATA POINTER
        LD      DE,$FFFF                          ;INDICATE NO MORE DATA STATEMENTS
COMA:
        INC     DE                                ;SKIP THE COMMA
GDEND:
        EX      DE,HL                             ;SWAP DATA POINTER BACK TO H-L
        LD      (DATA),HL                         ;SAVE IN POINTER
        EX      DE,HL                             ;SWAP VALUE BACK TO H-L
        POP     AF                                ;GET FLAGS BACK
        POP     DE                                ;GET SOURCE POSITION BACK
        RET
;*
;* VERIFY THAT COMMAND WAS 'DATA'
;*
VERDAT:
        CALL    PARSE1                            ;SKIP TO COMMAND
        EX      DE,HL                             ;SWAP TO H-L
        LD      DE,DATCMD                         ;POINT TO DATA COMMAND
VER1:
        LD      A,E                               ;GET LOW ADDRESS
        CP      ((DATCMD+4) &$FF)                 ;ARE WE AT END
        RET     Z
        LD      A,(DE)                            ;GET CHR FROM TABLE
        INC     DE                                ;ADVANCE TO NEXT
        AND     $7F                               ;INSURE IT'S CORRECT
        CP      (HL)                              ;DUZ IT MATCH?
        INC     HL                                ;NEXT IN DATA COMMAND
        JP      Z,VER1                            ;OK,;TEST NEXT
        RET
;*
;* HE WANT'S TO KNOW HOW BIG IT IS... LETS FIGURE IT OUT AND LET HIM IN ON IT
;*
SIZE:
        PUSH    DE                                ;SAVE PROGRAM POINTER
        CALL    GETEOF                            ;FIND THE END OF THE FILE
        LD      BC,0-TEXT                         ;GET THE (NEGATIVE) FILE START ADDRESS
        ADD     HL,BC                             ;SUBTRACT FILE START FROM FILE END
        CALL    DECPRT                            ;DISPLAY VALUE IN DECIMAL
        LD      HL,SIMSG                          ;GET ' BYTES' MESSAGE
        POP     DE                                ;RESTORE PROGRAM POINTER
        JP      PMSG                              ;TELL HIM WHAT IT IS
;* FINDS THE END OF THE FILE, HL=LAST BYTE OF PGM., A=FIRST FREE PAGE
GETEOF:
        LD      HL,TEXT                           ;START AT THE BEGINING
        LD      A,255                             ;LOOKING FOR AN FF
GLPX:
        CP      (HL)                              ;IS THIS IT?
        INC     HL                                ;ADVANCE TO NEXT
        JP      NZ,GLPX                           ;IF NOT IT, KEEP LOOKING
        DEC     HL                                ;POINT BACK TO $FF
        LD      A,H                               ;GET HIGH VALUE
        INC     A                                 ;ADVANCE TO NEXT PAGE
        RET
;*
;* HE'S TRYING TO 'LOAD' SOMETHING, I WONDER IF HE HAS SOMETHING SAVED..
;*
LOAD:
        CALL    TON                               ;TURN ON TAPE AND WAIT
LOD1:
        CALL    GETR                              ;GET A RECORD
        JP      C,LOD1                            ;KEEP GOING TILL WE HAVE IT ALL
        CALL    TOFF                              ;SHUT TAPE OFF
        JP      RESV                              ;CLEAR VARIABLES AND GET A NEW COMMAND
;*
;* HE'S TRYING TO 'SAVE' SOMETHING..
;*
SAVE:
        PUSH    DE                                ;SAVE PROGRAM POINTER
        LD      DE,TEXT                           ;GET ADDRESS OF TEXT
        LD      A,(DE)                            ;GET FIRST BYTE
        INC     A                                 ;TEST FOR NO PROGRAM
        JP      Z,RUN                             ;IF SO, RUN WILL ABORT WITH ERROR
        CALL    GETEOF                            ;GET ENDING ADDRESS
        CALL    TDUMP                             ;DUMP PROGRAM AND RETURN
        POP     DE                                ;RESTORE PROGRAM POINTER
        RET
;*
;*****************************************************************
;*                 EXPRESSION EVALUATION CODE
;*****************************************************************
;*
;* EVALUATES 16 BIT DECIMAL NUMBERS
;*
EVAL:
        LD      BC,1                              ;MULTIPLIER IS ONE
        LD      H,B                               ;INITIALIZE
        LD      L,B                               ;STARTING RESULT TO ZERO
ETOP:
        LD      A,(DE)                            ;GET DIGIT FROM SOURCE
        CALL    NUM                               ;TEST FOR INVALID DIGIT
        RET     C                                 ;IF SO, WE ARE FINISHED
        AND     $0F                               ;CONVERT TO BINARY
;* ADD DIGIT TIMES MULTIPLIER IN B-C TO H-L
ZLOOP:
        DEC     A                                 ;REDUCE BY ONE
        JP      M,ESP1                            ;EXIT WHEN EXAUSTED
        ADD     HL,BC                             ;ADD MULTIPLIER
        JP      ZLOOP                             ;CONTINUE TILL DONE
;* MULTIPLY MULTIPLIER (BC) BY 10
ESP1:
        PUSH    HL                                ;SAVE H-L
        LD      H,B                               ;GET B-C INTO
        LD      L,C                               ;H-L SO WE CAN USE 'DAD'
        ADD     HL,BC                             ;BC=BC*2
        ADD     HL,HL                             ;BC=BC*4
        ADD     HL,BC                             ;BC=BC*5
        ADD     HL,HL                             ;BC=BC*10
        LD      B,H                               ;SAVE BACK INTO
        LD      C,L                               ;B-C REGISTER PAIR
        POP     HL                                ;RESTORE H-L
        DEC     DE                                ;REDUCE POINTER IN SOURCE
        JP      ETOP                              ;EVALUATE NEXT CHARACTER
;*
;* SUBROUTINE;TESTS FOR VALID ASCII CHARACTERS
;*
CHAR:
        CP      'A'                               ;TEST FOR < 'A'
        RET     C                                 ;RETURN SAYING IT'S BAD
        CP      '['                               ;TEST FOR >'Z'
        CCF                                       ;INVERT LOGIC
        RET
;*
;* PARSES FORWARD, SEARCHING FOR FIRST NON-BLANK CHARACTER
;*
PARSE:
        INC     DE                                ;ADVANCE IN SOURCE
PARSE1:
        LD      A,(DE)                            ;GET CHARACTER FROM SOURCE
        CP      ' '                               ;TEST FOR SPACE
        JP      Z,PARSE                           ;KEEP LOOKING
        CP      ':'                               ;TEST FOR END OF STATEMENT
        RET     Z                                 ;IF SO, RETURN WITH Z SET
        CP      $0D                               ;TEST FOR END OF LINE
        RET
;*
;* SKIPS TO NEXT EXPRESSION OR COMMAND
;*
SKIP:
        CALL    PARSE1                            ;ADVANCE TO NEXT NON-BLANK
        DEC     DE                                ;BACK UP TO POSITION
        LD      A,E                               ;GET LOW ORDER ADDRESS
        LD      (P),A                             ;SAVE IN POSITION BYTE
;* LOOK FOR DELIMITER
SKIP1:
        INC     DE                                ;ADVANCE TO NEXT
        LD      A,(DE)                            ;GET CHARACTER
        CP      ':'                               ;TEST FOR DELIMITER
        RET     Z                                 ;IF SO, RETURN
        CP      ','                               ;TEST FOR DELIMITER
        RET     Z                                 ;IF SO, RETURN
        CP      $0D                               ;TEST FOR DELIMITER
        RET     Z                                 ;IF SO, RETURN
        CP      $22                               ;TEST FOR QUOTE
        CALL    Z,SKPQUO                          ;IF SO, ADVANCE TO NEXT QUOTE
        JP      SKIP1                             ;KEEP LOOKING
;* FIND NEXT QUOTE IN SOURCE
SKPQUO:
        INC     DE                                ;ADVANCE TO NEXT CHARACTER IN SOURCE
        LD      A,(DE)                            ;GET THE CHARACTER
        CP      $22                               ;IS IT A QUOTE?
        RET     Z                                 ;IF SO, WE FOUND IT
        CP      $0D                               ;IF IT A CARRIAGE RETUEN
        JP      NZ,SKPQUO                         ;IF NOT, OK
        JP      SYNT                              ;UNMATCHED QUOTES WHILE PARSING
;*
;* EVALUATES AN EXPRESSION POINTED TO BY D-E. RETURN WITH CARRY SET
;* INDICATES THAT EXPRESSION WAS A CHARACTER EXPRESSION
;*
EXPR:
        CALL    SKIP                              ;ADVANCE TO END OF EXPRESSION
        PUSH    DE                                ;SAVE POINTER TO END
        CALL    DOEXP                             ;EVALUATE
        POP     DE                                ;RESTORE POINTER TO END OF EXPRESSION
        RET
;* CALCULATES EXPRESSION BACKWARDS (LIKE APL)
DOEXP:
        DEC     DE                                ;BACK UP IN SOURCE
        CALL    FE                                ;GET CHARACTER FROM SOURCE
        CP      '$'                               ;TEST FOR CHARACTER VARIABLE
        JP      Z,CEXP                            ;IF SO, ITS A CHARACTER EXPRESSION
        CP      $22                               ;TEST FOR QUOTE
        JP      Z,CEXP                            ;IF SO, IT'S A CHARACTER EXRESSION
        INC     DE                                ;ADVANCE
        LD      A,$59                             ;NULL OPERATOR TO START
EGO1:
        PUSH    HL                                ;SAVE OLD VALUE
        PUSH    AF                                ;SAVE OPERATOR
        DEC     DE                                ;BACK UP TO VALUE
        CALL    FE                                ;GET CHARACTER FROM SOURCE
        CP      ')'                               ;TEST FOR BRACKET
        JP      Z,BRKTS                           ;IF SO, RECURSE
        CP      ']'                               ;TEST FOR ARRAY LOOKUP
        JP      Z,ARYL                            ;IF SO, LOOK UP ARRAY VALUE
        CALL    CHAR                              ;TEST FOR VARAIABLE
        JP      NC,LOOKU                          ;IF SO, LOOK IT UP
        CP      '?'                               ;TEST FOR RANDOM NUMBER RETERIVAL
        JP      Z,RANDR                           ;GET RANDOM VALUE
        CP      '#'                               ;TEST FOR HEX CONSTANT
        JP      Z,HEXVL                           ;IF SO, GET HEV VALUE
        CALL    NUM                               ;TEST FOR A NUMBER
        JP      C,SYNT                            ;IF NOT, I DON'T KNOW WHAT HE'S DOING
;* DECIMAL NUMBER
CALN:
        CALL    EVAL                              ;EVALUATE DECIMAL NUMBER
        JP      OLOOK                             ;LOOK FOR OPERATOR
;* HEX. NUMBER
HEXVL:
        DEC     DE                                ;BACK UP IN SOURCE
        LD      A,(P)                             ;GET ENDING POSITION
        CP      E                                 ;TEST FOR PASSED THE LIMIT
        JP      Z,HEXGO                           ;IF SO, THATS IT
        LD      A,(DE)                            ;GET CHARACTER FROM SOURCE
        CALL    NUM                               ;TEST FOR VALID DIGIT
        JP      NC,HEXVL                          ;KEEP GOING TILL WE GET TO START OF STRING
        SUB     'A'                               ;TEST FOR VALID LETTER
        CP      6                                 ;OF 'A' TO 'F'
        JP      C,HEXVL                           ;IF SO, KEEP LOOKING
HEXGO:
        LD      HL,0                              ;START WITH A ZERO
        LD      B,H                               ;FLAG TO SEE IF ANY DIGITS
        PUSH    DE                                ;SAVE POSITION IN SOURCE
GETHX:
        INC     DE                                ;ADVANCE TO NEXT DIGIT OF HEX NUMBER
        LD      A,(DE)                            ;GET DIGIT
        CP      '#'                               ;TEST FOR END OF STRING
        JP      Z,HGON                            ;IF SO, WE ARE DONE
        LD      B,A                               ;SET FLAG SO WE KNOW WE GOT AT LEAST ONE DIGIT
        ADD     HL,HL                             ;SHIFT H-L
        ADD     HL,HL                             ;RIGHT IN ORDER
        ADD     HL,HL                             ;TO MAKE ROOM FOR
        ADD     HL,HL                             ;THE NEW DIGIT
        SUB     '0'                               ;REDUCE TO BINARY
        CP      10                                ;TEST FOR FURTHER REDUCTION NEEDED
        JP      C,HISG                            ;IF NOT, PROCESS
        SUB     7                                 ;CONVERT LETTER TO BINARY
HISG:
        OR      L                                 ;ADD IN BOTTOM DIGIT OF RESULT
        LD      L,A                               ;REPLACE IN RESULT
        JP      GETHX                             ;GET NEXT DIGIT
HGON:
        POP     DE                                ;GET POSITION IN SOURCE BACK
        LD      A,B                               ;GET FLAG
        AND     A                                 ;TEST FOR DIGIT'S PROCESSED
        JP      NZ,OLOOK                          ;NO PROBLEM
        JP      SYNT                              ;'#' WITH NO DIGITS... ERROR
;* A ')' HAS BEEN DETECTED
BRKTS:
        CALL    DOEXP                             ;RECURSE ON OURSELVES
        JP      DCLB                              ;CONTINUE WITH VALUE
;* LOOK UP AN ARRAY VALUE
ARYL:
        POP     AF                                ;GET OPERATOR BACK
        PUSH    AF                                ;STASH OPERATOR
        CP      '='                               ;TEST FOR ASSIGNMENT
        CALL    NZ,ALOOK                          ;IF NOT, GET VALUE
        JP      DCLB                              ;CONTINUE WITH VALUE
;* GET VARIABLE CONTENTS
LOOKU:
        CALL    LOOK                              ;LOOK UP VALUE OF VARIABLE
DCLB:
        DEC     DE                                ;BACK UP IN SOURCE
OLOOK:
        POP     AF                                ;GET OPERATOR BACK
        POP     BC                                ;GET OLD VALUE BACK
;* 16 BIT ADDITION
        CP      '+'                               ;TEST FOR ADDITION
        JP      Z,ADD                             ;IF SO, PERFORM ADD
;* SIXTEEN BIT SUBTRACTION
        CP      '-'                               ;TEST FOR SUBTRACTION
        JP      NZ,MULT                           ;NO, TRY MULTIPLICATION
        LD      A,B                               ;GET B
        CPL                                       ;COMPLEMENT
        LD      B,A                               ;RESAVE
        LD      A,C                               ;GET C
        CPL                                       ;COMPLEMENT
        LD      C,A                               ;RESAVE
        INC     BC                                ;ADD 1 GIVING TWO'S COMPLEMENT
ADD:
        ADD     HL,BC                             ;ADD TO NEW VALUE
        JP      EGO                               ;CONTINUE
;* 16 BIT MULTIPLICATION
MULT:
        CP      '*'                               ;TEST FOR MULTIPLY
        JP      NZ,DIV                            ;NO, TRY DIVIDE
        LD      A,B                               ;TEST OLD VALUE FOR ZERO
        OR      C                                 ;AS IT IS A
        JP      Z,EGZ                             ;SPECIAL CASE
        CALL    DMULT                             ;PERFORM THE MULTIPLY
        JP      EGO                               ;AND CONTINUE
;* MULTIPLY SUBROUTINE (ALSO USED BY RANDOM NUMBER GENERATOR)
DMULT:
        PUSH    DE                                ;SAVE POSITION IN SOURCE
        LD      DE,0                              ;START OUT WITH A ZERO
MUL1:
        AND     A                                 ;INSURE CARRY CLEAR
        LD      A,B                               ;GET B
        RRA                                       ;ROTATE
        LD      B,A                               ;RESAVE
        LD      A,C                               ;GET C
        RRA                                       ;ROTATE WITH CARRY
        LD      C,A                               ;REPLACE
        PUSH    AF                                ;SAVE FLAGS
        OR      B                                 ;TEST FOR B-C = ZER0
        JP      Z,MEXIT                           ;IF SO, WE ARE DONE
        POP     AF                                ;GET FLAGS BACK
        JP      NC,NOMAD                          ;NO ONE BIT, DON'T ADD
        EX      DE,HL                             ;SWAP SO WE CAN
        ADD     HL,DE                             ;ADD TO D-E
        EX      DE,HL                             ;AND SWAP BACK
NOMAD:
        ADD     HL,HL                             ;SHIFT H-L RIGHT BY ONE BIT
        JP      MUL1                              ;KEEP GOING
MEXIT:
        ADD     HL,DE                             ;ADD RESULT
        POP     AF                                ;CLEAN UP STACK
        POP     DE                                ;RESTORE SOURCE POSITION
        RET
;* 16 BIT DIVISION
DIV:
        CP      '%'                               ;TEST FOR DIVIDE
        JP      NZ,FLOR                           ;NO, TRY FLOR
        LD      A,B                               ;TEST FOR AN OLD
        OR      C                                 ;VALUE OF ZERO,
        JP      Z,DIVZE                           ;BECAUSE THAT IS  A BAD THING
        PUSH    DE                                ;SAVE SOURCE POSITION
        CALL    DODIV                             ;PERFORM DIVIDE OPERATION
        LD      (VARS+34),HL                      ;SET 'R' REMAINDER VARIABLE
        EX      DE,HL                             ;PLACE RESULT IN H-L
        POP     DE                                ;RESTORE SOURCE POSITION
        JP      EGO                               ;AND CARRY ON
;*
;* PERFORMS 16 BIT(HL) BY 16 BIT(BC) DIVIDE, RESULT IN DE, REM IN HL
;*
DODIV:
        LD      A,B                               ;GET CONTENTS OV B
        CPL                                       ;INVERT
        LD      B,A                               ;REPLECE
        LD      A,C                               ;GET CONTENTS IN C
        CPL                                       ;INVERT
        LD      C,A                               ;REPLACE
        INC     BC                                ;COMPLETE TWO COMPLEMENT OPERATION
        EX      DE,HL                             ;COPY HL TO DE, LOWER HALF OF 32 BIT VALUE
        LD      HL,0                              ;ZERO HIGHER HALF
        CALL    DIVBYT                            ;PERFORM FIRST HALF
DIVBYT:
        LD      A,D                               ;GET UPPER HALF
        LD      D,E                               ;SAVE LOWER HALF
        LD      E,8                               ;GET LOOP COUNT
DIVTOP:
        ADD     HL,HL                             ;SHIFT LEFT
        JP      C,OVER1                           ;OVERFLOWED
        ADD     A,A                               ;SHIFT  RESULT
        JP      NC,SUBB                           ;IF NO CARRY, DON'T INC
        INC     HL                                ;ADVANCE UPPER VALUE
SUBB:
        PUSH    HL                                ;SAVE VALUE
        ADD     HL,BC                             ;SUBTRACT LOWER HALF OF FRACTION
        JP      C,OKKK                            ;IF WRAP PAST ZERO
        POP     HL                                ;RESTORE VALUE
        JP      NXLP                              ;FINISH LOOP
OKKK:
        INC     SP                                ;FIX UP
        INC     SP                                ;STACK
        INC     A                                 ;ADVANCE RESULT
        JP      NXLP                              ;FINISH LOOP
OVER1:
        ADC     A,A                               ;SHIFT RESULT, +1 FOR CARRY
        JP      NC,OVRSUB                         ;IF NO WRAP
        INC     HL                                ;INC. VALUE
OVRSUB:
        ADD     HL,BC                             ;SUBTRACT LOWER
NXLP:
        DEC     E                                 ;REDUCE LOOP COUNTER
        JP      NZ,DIVTOP                         ;LOOP IF NOT FINISHED
        LD      E,A                               ;LOWER BYTE OF RESULT
        RET
;*
;* HE SHOULD KNOW THAT HE CAN'T DIVIDE BY ZERO, BUT JUST IN CASE...
;* WE WILL TELL HIM ANYWAY
;*
DIVZE:
        LD      HL,DER                            ;ADDRESS OF 'DIVIDE BYE ZERO MESSAGE'
        JP      ERR                               ;HANDLE LIKE ANY OTHER ERROR
;* COMPARES H-L TO B-C, Z=1 IF HL=BC, C=1 IF HL<BC
COMP:
        LD      A,H                               ;GET HIGH BYTE OF HL
        CP      B                                 ;COMPARE WITH HIGH BYTE OF BC
        RET     NZ                                ;IF NOT SAME, LOWER BYTE CAN BE IGNORED
        LD      A,L                               ;GET LOW BYTE OF HL
        CP      C                                 ;COMPARE WITH LOW BYTE OF BC
        RET
;* FLOOR, RETURNS THE LESSER OF THE TWO NUMBERS
FLOR:
        CP      $92                               ;IS IT FLOOR?
        JP      NZ,CEIL                           ;NO, TRY CEILING
        CALL    COMP                              ;COMPARE NEW TO OLD
        JP      C,EGO                             ;IF LESS, WE ARE OK (RESULT IS ALREADY IN HL)
SWAP:
        LD      H,B                               ;MAKE OLD NUMBER..
        LD      L,C                               ;INTO THE NEW NUMBER..
        JP      EGO                               ;CONTINUE
;* CEILING, RETURNS THE GREATER OF THE TWO NUMBERS
CEIL:
        CP      '/'                               ;IS IT CEILING?
        JP      NZ,LAND                           ;NO, TRY LOGICAL AND
        CALL    COMP                              ;COMPARE NEW AND OLD
        JP      NC,EGO                            ;IF GREATER, WE ARE OK (RESULT ALREADY IN HL)
        JP      SWAP                              ;MAKE OLD NEW AND CONTINUE
;* LOGICAL AND
LAND:
        CP      '&'                               ;IF IT LOGICAL AND ?
        JP      NZ,LOR                            ;NO, TRY LOGICAL OR
        LD      A,B                               ;GET HIGH BYTE OF OLD
        AND     H                                 ;AND WITH HIGH BYTE OF NEW
        LD      H,A                               ;AND REPLACE HIGH BYTE OF NEW
        LD      A,L                               ;GET LOW BYTE OF OLD
        AND     C                                 ;AND WITH LOW BYTE OF NEW
        JP      CPYL                              ;CONTINUE
;* LOGICAL OR
LOR:
        CP      '|'                               ;TEST FOR LOGICAL OR
        JP      NZ,GRTR                           ;NO, TRY GREATER THAN
        LD      A,H                               ;GET HIGH BYTE OF NEW
        OR      B                                 ;OR WITH HIGH BYTE OF OLD
        LD      H,A                               ;AND REPLACE HIGH BYTE OF NEW
        LD      A,L                               ;GET LOW BYTE OF NEW
        OR      C                                 ;OR WITH LOW BYTE OF OLD
CPYL:
        LD      L,A                               ;AND REPLACE LOW BYTE OF NEW
        JP      EGO                               ;CONTINUE
;* GREATER THAN, RETURNS ONE OR ZERO
GRTR:
        CP      '>'                               ;TEST FOR GREATER THAN
        JP      NZ,LETH                           ;IF NOT, TRY LESS THAN
        CALL    COMP                              ;COMPARE OLD AND NEW
        JP      Z,EGZ                             ;FALSE IF .EQUAL
        JP      C,EGZ                             ;FALSE IF LESS THAN
        JP      EG1                               ;TRUE IF NOT LESS OR .EQUAL
;* LESS THAN, RETURNS ONE OR ZERO
LETH:
        CP      '<'                               ;IS IT LESS THAN?
        JP      NZ,ENOP                           ;NO, TRY NO-OP OPERATOR
        CALL    COMP                              ;COMPARE OLD AND NEW
        JP      C,EG1                             ;TRUE IF LESS
        JP      EGZ                               ;FALSE IF NOT LESS
;* NO-OP OPERATOR, RETURNS NEW VALUE ONLY
ENOP:
        CP      $59                               ;IS IT NO-OP? ';'
        JP      Z,EGO                             ;IF SO, DON'T DO ANYTHING
;* ASSIGNMENT, SET A VARIABLE'S VALUE
ASST:
        CP      '='                               ;TEST FOR ASSIGNMENT
        JP      NZ,EQUAL                          ;IF NOT, TRY .EQUALITY
        LD      (EFLAG),A                         ;SET ASSIGNMENT FLAG
        INC     DE                                ;BACK UP TO VARIABLE NAME
        LD      A,(DE)                            ;GET VARIABLE CHARACTER
        CP      ']'                               ;TEST FOR ARRAY STORAGE
        JP      Z,ASTOR                           ;IF SO, STORE INTO ARRAY
        LD      H,B                               ;GET OLD VALUE
        LD      L,C                               ;INTO H-L (WHERE STORE WANTS THEM)
        CP      '?'                               ;TEST FOR SETTING RANDOM SEED
        JP      Z,SRSEED                          ;IF SO, SET THE SEED
        CALL    STOR                              ;STORE VALUE INTO VARIABLE
STRT:
        DEC     DE                                ;STEP BACK FROM VARIABLE
        JP      EGO                               ;AND CONTINUE
;* SET THE RANDOM SEED
SRSEED:
        LD      (SEED),HL                         ;SO WE CAN STORE IN SEED
        JP      STRT                              ;AND CONTINUE
;* SET THE VALUE OF AN ARRAY ELEMENT
ASTOR:
        PUSH    HL                                ;SAVE H-L
        CALL    DOEXP                             ;CALCULATE INDEX VALUE
        DEC     DE                                ;BACK UP PAST '['
        LD      A,(DE)                            ;GET ARRAY NAME
        CALL    LOOKT                             ;LOOK UP IT'S ADDRESS IN THE TABLE
        LD      B,H                               ;GET ARRAY ADDRESS
        LD      C,L                               ;INTO B-C
        POP     HL                                ;RERSTORE H-L
        LD      A,(DE)                            ;GET ARRAY NAME BACK
        CP      '@'                               ;TEST FOR 'MAGIC', MEMORY REFERENCE
        JP      Z,STMEM                           ;IF SO, SET MEMORY LOCATION
        LD      A,H                               ;GET HIGH BYTE OF VALUE
        LD      (BC),A                            ;STASH IN ARRAY
        INC     BC                                ;POINT TO NEXT
STMEM:
        LD      A,L                               ;GET LOW BYTE OF VALUE
        LD      (BC),A                            ;STASH IN ARRAY
        JP      STRT                              ;CONTINUE
;*;TEST FOR .EQUALITY.  ('==')
EQUAL:
        SUB     $81                               ;IS A '=='?
        JP      NZ,GEQL                           ;IF NOT, TRY GREATER OR .EQUAL
        CALL    COMP                              ;COMPARE OLD AND NEW
        JP      Z,EG1                             ;TRUE IF .EQUAL
        JP      EGZ                               ;FALSE IF NOT .EQUAL
;* GREATER OR .EQUAL.  ('>=')
GEQL:
        DEC     A                                 ;TEST FOR '>='?
        JP      NZ,LEQL                           ;NO, TRY LESS OR .EQUAL
        CALL    COMP                              ;COMPARE OLD AND NEW
        JP      C,EGZ                             ;FALSE IF LESS THAN
        JP      EG1                               ;TRUE IF GREATER OR .EQUAL
;* LESS OR .EQUAL.  ('<=')
LEQL:
        DEC     A                                 ;TEST FOR '<='?
        JP      NZ,NEQL                           ;IF NOT, TRY NOT .EQUAL
        CALL    COMP                              ;COMPARE OLD AND NEW
        JP      Z,EG1                             ;TRUE IF SAME
        JP      C,EG1                             ;TRUE IF LESS THAN
        JP      EGZ                               ;FALSE OTHERWISE
;*;TEST FOR NOT .EQUAL.  ('-=')
NEQL:
        DEC     A                                 ;IS IT '-='?
        JP      NZ,SYNT                           ;BEATS ME WHAT IT IS!
        CALL    COMP                              ;COMPARE OLD AND NEW
        JP      Z,EGZ                             ;IF SAME, FALSE
;* RETURN RESULT OF ONE
EG1:
        LD      HL,1                              ;SET RESULT TO ONE
        JP      EGO                               ;PASS ON RESULT
;* RETURN RESULT OF ZERO
EGZ:
        LD      HL,0                              ;SET RESULT TO ZERO
;* END OF OPERATION, GET NEXT OPERATOR
EGO:
        CALL    FE                                ;GET NEXT CHARACTER
        RET     Z                                 ;IF WE PASS BEGINNING OF EXPRESSION, QUIT
        CP      '('                               ;ARE WE RETURNING FROM A NEST?
        RET     Z                                 ;IF SO, BACK UP ONE LEVEL
        CP      '['                               ;FINISHED AN ARRAY INDEX EVALUATION?
        RET     Z                                 ;RETURN TO MAIN EXPRESSION
        CP      '='                               ;IF IT A MULTI-CHARACTER OPERATOR
        JP      NZ,EGO1                           ;IF NOT, DON'T PRE-EVALUATE
        DEC     DE                                ;BACK UP TO PRECEDING CHARACTER
        LD      A,(DE)                            ;GET PRECEDING CHARACTER
        CP      '='                               ;IS IT '=='?
        LD      B,$81                             ;SET UNIQUE CODE
        JP      Z,EGO2                            ;IF '==' THEN WE HAVE IT
        INC     B                                 ;NEXT UNIQUE CODE
        CP      '>'                               ;IS IT '>='?
        JP      Z,EGO2                            ;IF SO, WE HAVE IT
        INC     B                                 ;NEXT UNIQUE CODE
        CP      '<'                               ;IS IT '<='?
        JP      Z,EGO2                            ;IF SO, WE HAVE IT
        INC     B                                 ;NEXT UNIQUE CODE
        CP      '-'                               ;IS IT '-='?
        JP      Z,EGO2                            ;IF SO, WE HAVE IT
        INC     DE                                ;WASN'T A TWO CHARACTER OPERATOR. BACK UP
        LD      A,'='                             ;MUST HAVE BEEN A SIMPLE '='
        JP      EGO1                              ;CONTINUE EVALUATING EXPRESSION
EGO2:
        LD      A,B                               ;SET OPERATOR TO OUR UNIQUE CODE
        JP      EGO1                              ;AND CONTINUE EVALUATING EXPRESSION
;* FINDS NEXT CHARACTER IN EXPRESSION, SETS Z FLAG IF WE PASS THE BEGINNING
FE:
        LD      A,(P)                             ;GET ADDRESS OF BEGINNING OF LINE
        CP      E                                 ;ARE WE THERE??
        RET     Z                                 ;IF SO, WE ARE FINISHED
        LD      A,(DE)                            ;GET CHARACTER FROM SOURCE
        CP      ' '                               ;IS A (USELESS) BLANK?
        RET     NZ                                ;IF NOT, WE ARE FINISHED
        DEC     DE                                ;BACK UP ANOTHER CHARACTER
        JP      FE                                ;AND TRY AGAIN
;* CALCULATE A PSEUDO-RANDOM VALUE
RANDR:
        LD      HL,(SEED)                         ;GET RANDOM SEED
        LD      A,H                               ;GET HIGH BYTE OF SEED
        AND     $F7                               ;AND WITH HIGH MASK
        PUSH    AF                                ;SAVE PARITY FLAG
        LD      A,L                               ;GET LOW BYTE OF SEED
        AND     $42                               ;AND WITH LOW BYTE OF MASK
        PUSH    AF                                ;SAVE PARITY FLAG
        POP     BC                                ;GET FLAGS IN C
        LD      A,C                               ;COPY TO A
        POP     BC                                ;GET FIRST SET OF FLAGS IN C
        XOR     C                                 ;COMPUTE PARITY FOR ENTIRE WORD
        RRCA                                      ;MOVE COMPUTED
        RRCA                                      ;PARITY INTO
        RRCA                                      ;THE CARRY FLAG
        CCF                                       ;COMP, SO SHIFT IN 1 IF EVEN
        LD      A,L                               ;GET LOW BYTE OF SEED
        RLA                                       ;SHIFT IN CARRY, OUT HIGH BIT
        LD      L,A                               ;RESAVE
        LD      A,H                               ;GET HIGH BYTE OF SEED
        RLA                                       ;SHIFT IN CARRY (HIGH BIT OF OLD LOWER)
        LD      H,A                               ;RESAVE
        LD      (SEED),HL                         ;RESULT IS NEW SEED
        JP      DCLB                              ;KEEP GOING
;*
;* EVALUATES A CHARACTER EXPRESSION
;*
CEXP:
        CALL    CLBF                              ;CLEAR EXTRA BUFFER
        INC     DE                                ;SKIP TO END OF EXPRESSION
        LD      A,'+'                             ;TO BEGIN, CONCATINATE A NULL STRING
CG1:
        PUSH    AF                                ;STACK THE OPERATOR FOR LATER
        CALL    PUSHB                             ;COPY NEW BUFFER INTO OLD BUFFER
        CALL    CLBF                              ;CLEAR THE NEW BUFFER
        DEC     DE                                ;BACK UP IN SOURCE
        CALL    FE                                ;GET CHARACTER AND;TEST FOR END
        CP      $22                               ;TEST FOR QUOTE
        JP      Z,CQ                              ;IF SO, HANDLE QUOTED STRING
        CP      '$'                               ;TEST FOR CHARACTER VARIABLE
        JP      NZ,SYNT                           ;IF NOT, IT'S NOT ANYTHING I RECOGNISE
CV:
        DEC     DE                                ;BACK UP PAST DOLLAR SIGN
        LD      A,(DE)                            ;GET VARIABLE NAME
        CP      ']'                               ;TEST FOR INDEX INTO CHARACTER VARIABLE
        JP      Z,CINDX                           ;GET INDEX VALUE
        CALL    LTA                               ;GET ADDRESS OF VARIABLE
        PUSH    DE                                ;SAVE SOURCE POSITION
        EX      DE,HL                             ;SWAP ADDRESS TO D-E
        DEC     DE                                ;BACK UP IN SOURCE
        JP      Q0                                ;SAVE VARIABLE IN NEW BUFFER
;* BACKUP TO PRECEDING QUOTE
CQ:
        DEC     DE                                ;BACKUP IN SOURCE
        LD      A,(DE)                            ;GET CHARACTER FROM SOURCE
        CP      $0D                               ;TEST FOR END OF LINE
        JP      Z,SYNT                            ;IF SO, THERE IS NO CLOSEING QUOTE
        CP      $22                               ;TEST FOR CLOSEING QUOTE
        JP      NZ,CQ                             ;IF NO, KEEP LOOKING
        PUSH    DE                                ;SAVE ENDING POSITION
Q0:
        LD      HL,XBF                            ;GET ADDRESS OF NEW (EXTRA) BUFFER
Q1:
        INC     DE                                ;ADVANCE TO SOURCE OR VARIABLE CHARACTER
        LD      A,(DE)                            ;GET CHARACTER
        CP      $22                               ;TEST FOR CLOSEING QUOTE
        JP      Z,Q2                              ;IF SO, STOP COPYING
        CP      $FF                               ;TEST FOR END OF VARIABLE
        JP      Z,Q2                              ;IF SO, STOP COPYING
        LD      (HL),A                            ;SAVE IN BUFFER
        INC     HL                                ;ADVANCE IN BUFFER
        JP      Q1                                ;KEEP COPYING
Q2:
        POP     DE                                ;GET POSITION BACK
Q3:
        DEC     DE                                ;BACK UP TO OPERATOR
        POP     AF                                ;GET OPERATOR
        CP      '+'                               ;TEST FOR CONCATIONATION
        JP      NZ,Q5                             ;NO, TRY ASSIGNMENT
;* CONCATONATION. XBF=XBF+TB
QPP:
        LD      BC,TB                             ;ADDRESS OF TEMPORARY BUFFER
Q4:
        LD      A,(BC)                            ;GET CHARACTER FROM BUFFER
        LD      (HL),A                            ;MOVE TO BUFFER
        INC     BC                                ;ADVANCE IN OLD
        INC     HL                                ;ADVANCE IN NEW
        LD      A,C                               ;GET ADDRESS IN OLD
        CP      ((TB+35) & $FF)                   ;TEST FOR OVER
        JP      C,Q4                              ;IF SO, STOP
        LD      HL,(XBF)                          ;GET CHARACTER FROM BUFFER
        LD      H,0                               ;SET HIGH BYTE TO ZERO
        JP      Q9                                ;CONTINUE
;* ASSIGNMENT
Q5:
        CP      '='                               ;TEST FOR ASSIGNMENT
        JP      NZ,Q6                             ;IF NOT, TRY .EQUALITY
        INC     DE                                ;SKIP TO VARIABLE NAME
        INC     DE                                ;SKIP TO DOLLARSIGN
        LD      A,(DE)                            ;GET DOLLARSIGN
        DEC     DE                                ;BACK UP TO VARIABLE NAME
        CP      '$'                               ;TEST FOR DOLLAR SIGN
        JP      NZ,SYNT                           ;IF NOT, THIS AIN'T NO CHARACTER VARIABLE
        LD      (EFLAG),A                         ;SET ASSIGNMENT FLAG
        LD      A,(DE)                            ;GET VARIABLE NAME
        DEC     DE                                ;BACK UP BAST NAME
        CALL    LTA                               ;GET IT'S ADDRESS
        JP      QPP                               ;COPY IT OVER
;*;TEST FOR .EQUALITY
Q6:
        CP      $81                               ;TEST FOR '=='
        JP      NZ,X0                             ;NO, TRY '=-'
        CALL    COMSTR                            ;COMPARE STRINGS
        JP      Q9                                ;CONTINUE
;* COMPARES STRINGS. SETS H-L TO 1 OR 0 IF .EQUAL OR NOT .EQUAL
COMSTR:
        LD      BC,TB                             ;GET ADDRESS OF OLD STRING
        LD      HL,XBF                            ;ADDRESS OF NEW STRING
Q7:
        LD      A,(BC)                            ;GET CHARACTER FROM OLD
        CP      (HL)                              ;TEST AGAINST NEW
        JP      NZ,Q8                             ;IF NOT, THEY ARE UN.EQUAL
        INC     BC                                ;NEXT PLACE IN OLD
        INC     HL                                ;NEXT PLACE IN NEW
        INC     A                                 ;TEST FOR END OF STRING
        JP      NZ,Q7                             ;IF NOT, CONTINUE;TESTING
        LD      HL,1                              ;INDICATE THEY ARE .EQUAL
        RET
Q8:
        LD      HL,0                              ;INDICATE NOT .EQUAL
        RET
;* NOT .EQUAL .. '-='
X0:
        CP      $82                               ;TEST FOR '-='
        JP      NZ,SYNT                           ;BEATS ME, BUT IT ISN'T RIGHT
        CALL    COMSTR                            ;TEST STRINGS
        LD      A,L                               ;GET RESULT
        XOR     1                                 ;AND COMPLEMENT IT
        LD      L,A                               ;REPLACE IN RESULT
;* GET NEXT STRING
Q9:
        CALL    FE                                ;GET NEXT CHARACTER
        SCF                                       ;INDICATE CHARACTER EXPRESSION
        RET     Z                                 ;IF END OF LINE, QUIT
        CP      '('                               ;TEST FOR END OF NUMERIC SUBSTRING
        RET     Z                                 ;QUIT, INDICATING NUMBERIC RESULT
        CP      '='                               ;TEST FOR .EQUAL,ASSIGNMENT, OR NOT .EQUALS
        JP      NZ,CG1                            ;NO, NO NEED TO;TEST FURTHER
QTST:
        DEC     DE                                ;BACK UP TO PREVIOUS CHARACTER
        LD      A,(DE)                            ;GET CHARACTER
        LD      B,A                               ;SAVE FOR COMPARISON
        CP      '='                               ;TEST FOR '=='
        LD      A,$81                             ;INDICATE '=='
        JP      Z,CG1                             ;CONTINUE
        LD      A,B                               ;GET CHARACTER BACK
        CP      '-'                               ;TEST FOR '-='
        LD      A,$82                             ;INDICATE '-='
        JP      Z,CG1                             ;CONTINUE
        INC     DE                                ;ADVANCE BACK TO PREVIOUS CHARACTER
        LD      A,'='                             ;INDICATE '='
        JP      CG1                               ;CONTINUE
;* CLEARS THE TEXT BUFFER
CLBF:
        LD      HL,XBF                            ;GET ADDRESS OF BUFFER
        LD      A,40                              ;CLEAR FOR LENGTH OF 40
CL2:
        LD      (HL),$FF                          ;CLEAR TO NULL CHARACTER
        INC     HL                                ;NEXT POSITION IN BUFFER
        DEC     A                                 ;REDUCE COUNT OF REMAINING
        JP      NZ,CL2                            ;KEEP GOING TILL WE ARE FINISHED
        RET
;* COPY'S NEW BUFFER INTO OLD BUFFER
PUSHB:
        PUSH    DE                                ;SAVE POSITION IS SOURCE
        LD      DE,XBF                            ;GET ADDRESS OF NEW BUFFER
        LD      HL,TB                             ;GET ADDRESS OF OLD BUFFER
        LD      B,40                              ;COPY 40 CHARACTERS
PU1:
        LD      A,(DE)                            ;GET CHARACTER FROM NEW
        LD      (HL),A                            ;SAVE IN OLD
        INC     HL                                ;NEXT POSITION IN OLD
        INC     DE                                ;NEXT POSITION IN NEW
        DEC     B                                 ;REDUCE COUNT
        JP      NZ,PU1                            ;KEEP GOING TILL 40 ARE MOVED
        POP     DE                                ;RESTORE POSITION IN SOURCE
        RET
;* INDEXED CHARACTER VARIABLE, EXTRACT A SINGLE CHARACTER
CINDX:
        CALL    DOEXP                             ;EVALUATE INDEX EXPRESSION
        LD      A,L                               ;GET INDEX VALUE
        PUSH    AF                                ;SAVE INDEX VALUE
        DEC     DE                                ;BACK UP TO VARIABLE NAME
        LD      A,(DE)                            ;GET VARIABLE NAME
        CP      '@'                               ;TEST FOR 'MAGIC' CHR$ VARIABLE
        JP      Z,CHR                             ;IF SO, HANDLE SPECIAL CASE
        CALL    LTA                               ;GET TEXT VARIABLE ADDRESS
        POP     AF                                ;GET INDEX BACK
        CP      35                                ;TEST FOR TOO BIG
        JP      NC,DIMERR                         ;IF SO, TELL HIM HE SCREWED UP
        LD      C,A                               ;GET INTO A DOUBLE PAIR
        ADD     HL,BC                             ;SO WE CAN DAD IT TO THE ADDRESS
        LD      A,(HL)                            ;AND GET THE CHARACTER
FILBUF:
        LD      HL,XBF                            ;ADDRESS OF TEXT BUFFER
        LD      (HL),A                            ;PLACE CHARACTER THERE
        INC     HL                                ;BUMP TO NEXT POSITION
        JP      Q3                                ;AND FILL WITH NULLS
;*
;* 'MAGIC' CHARACTER ARRAY, RETURNS CHARACTER WITH VALUE OF IT'S INDEX
;*
CHR:
        POP     AF                                ;GET INDEX VALUE
        JP      FILBUF                            ;SAVE IN BUFFER AND PAD WITH NULLS



;* END OF BASIC INTERPRETER CODE SECTION
;*
;**********************************************************************
;*                          COMMAND TABLE
;*
;*   FORMAT IS:
;*               COMMAND WORDS, HIGH BIT SET ON LAST CHARACTER
;*               ADDRESS OF COMMAND PROCESSOR FOLLOWES
;*               ENTRY OF HEX 00 INDICATES LAST ENTRY IN TABLE (DEFAULT)
;*
;**********************************************************************
;*
;* COMMANDS ALLOWED ONLY FROM WITHING A PROGRAM..
PTAB:
        .TEXT   "NEX"
        .DB     'T'+$80
        .DW     NEXT
        .TEXT   "THE"
        .DB     'N'+$80
        .DW     THEN
        .TEXT   "GOSU"
        .DB     'B'+$80
        .DW     GOSUB
        .TEXT   "RETUR"
        .DB     'N'+$80
        .DW     RETURN
        .TEXT   "FO"
        .DB     'R'+$80
        .DW     FOR
        .TEXT   "I"
        .DB     'F'+$80
        .DW     IF
DATCMD:
        .TEXT   "DAT"
        .DB     'A'+$80
        .DW     RNEXT
        .TEXT   "LI"
        .DB     'F'+$80
        .DW     LIF
;* COMMANDS ALLOWED FROM BOTH A PROGRAM, AND INTERACTIVE KEYBOARD ENTRY
KTAB:
        .TEXT   "GOT"
        .DB     'O'+$80
        .DW     GOTO
        .TEXT   "LE"
        .DB     'T'+$80
        .DW     LET
        .TEXT   "PRIN"
        .DB     'T'+$80
        .DW     PRINT
        .TEXT   "US"
        .DB     'R'+$80
        .DW     USR
        .TEXT   "REA"
        .DB     'D'+$80
        .DW     READ
        .TEXT   "PLO"
        .DB     'T'+$80
        .DW     PLOT
        .TEXT   "RE"
        .DB     'M'+$80
        .DW     REM
        .TEXT   "DI"
        .DB     'M'+$80
        .DW     DIM
        .TEXT   "RU"
        .DB     'N'+$80
        .DW     RUN
        .TEXT   "ORDE"
        .DB     'R'+$80
        .DW     ORDER
        .TEXT   "INPU"
        .DB     'T'+$80
        .DW     INPUT
        .TEXT   "CLEA"
        .DB     'R'+$80
        .DW     CLEAR
        .TEXT   "STO"
        .DB     'P'+$80
        .DW     STOP
        .TEXT   "EN"
        .DB     'D'+$80
        .DW     INIT
        .TEXT   "LIS"
        .DB     'T'+$80
        .DW     LIST
        .TEXT   "NE"
        .DB     'W'+$80
        .DW     NEW
        .TEXT   "SIZ"
        .DB     'E'+$80
        .DW     SIZE
        .TEXT   "LOA"
        .DB     'D'+$80
        .DW     LOAD
        .TEXT   "SAV"
        .DB     'E'+$80
        .DW     SAVE
        .TEXT   "MONITO"
        .DB     'R'+$80
        .DW     EXIT
        .DB     0                                 ;UNRECOGNIZED COMMAND, ASSUME 'LET'
        .DW     LET
;*
;***************************************************************
;*                    STRINGS AND MESSAGES
;***************************************************************
;*
;* ERROR MESSAGES..
;*
DER:
        .TEXT   "DIVIDE BY ZERO"                  ;DIVIDE BY ZERO
        .DB     0
IERMS:
        .TEXT   "BAD DATA - RETRY"                ;BAD RESPONSE TO INPUT STATEMENT
        .DB     $0D,$0A,0
CSTK:
        .TEXT   "NESTING"                         ;INVALID FOR/NEXT, GOSUB/RETURN NESTING
        .DB     0
LIN:
        .TEXT   "LINE NUMBER"                     ;GOTO, GOSUB, OR ORDER TO UNKNOWN LINE
        .DB     0
NP:
        .TEXT   "NO PROGRAM"                      ;RUN OR SAVE EMPTY PROGRAM
        .DB     0
INL:
        .TEXT   " IN LINE "                       ;INDICATES LINE ERROR WAS IN
        .DB     0
SYN:
        .TEXT   "SYNTAX"                          ;DOES NOT FOLLOW SYNTAX RULES
        .DB     0
DTXT:
        .TEXT   "DATA"                            ;BAD LINE OR DATA TYPE
        .DB     0
OVM:
        .TEXT   "DIMENSION"                       ;TO MANY ARRAYS, ARGUMENT OUT OF RANGE
        .DB     0
;*
;* INFORMATIONAL MESSAGES..
;*
RDY:
        .TEXT   "READY."                          ;READY PROMPT
        .DB     $0D,$0A,0
STMSG:
        .TEXT   "STOP"                            ;INDICATES PROGRAM STOPPED
        .DB     0
EM:
        .TEXT   " ERROR"                          ;INDICATES ERROR OCCURED
        .DB     0
SIMSG:
        .TEXT   " BYTES"                          ;DISPLAYED IN RESPONSE TO 'SIZE'
        .DB     $0D,$0A,0
TXT_BASICID:
        .TEXT   "MICRO-BASIC V0.1"
        .DB     $0D,$0A,0
ENDIT
        .EQU    *


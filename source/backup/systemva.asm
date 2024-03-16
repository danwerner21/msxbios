; $Id: systemvars.asm 566 2010-03-02 00:59:42Z mthuurne $
;
; C-BIOS system variable declarations
;
; Copyright (c) 2002-2003 BouKiCHi.  All rights reserved.
; Copyright (c) 2003 Reikan.  All rights reserved.
; Copyright (c) 2004-2005 Maarten ter Huurne.  All rights reserved.
; Copyright (c) 2004 Manuel Bilderbeek.  All rights reserved.
; Copyright (c) 2004-2006 Joost Yervante Damad.  All rights reserved.
; Copyright (c) 2004-2005 Albert Beevendorp.  All rights reserved.
; Copyright (c) 2005 Jussi Pitkänen.  All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSequENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOO.ds OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;


;-------------------
; help function area
;-------------------
; Note: Functions defined in "main.asm" are disabled here.


; F39A-F3AD: workarea for the DEF USR statement
; this area is initialized with the 10 times the value $475A, which gives
; the error 'Syntax Error'
USRTAB:         EQU $F39A

;----------------------
; screen parameter area
;----------------------

; F3AE: # of positions on a line in SCREEN 0 (ini:39)
LINL40:         EQU $F3AE

; F3AF: # of positions on a line in SCREEN 1 (ini:29)
LINL32:         EQU $F3AF

; F3B0: # of actually used positions in the current screenmodus (ini:39)
LINLEN:         EQU $F3B0

; F3B1: # of used lines on screen (ini:24)
CRTCNT:         EQU $F3B1

; F3B2: # of positions within a tabulator-column (ini:14)
CLMLST:         EQU $F3B2

; F3B3-F3B4: BASE(0): name table address for SCREEN 0 (ini:$0000)
; used to initialize NAMBAS when SCREEN 0 is activated
TXTNAM:         EQU $F3B3

; F3B5-F3B6: BASE(1): color table address for SCREEN 0, unused? (ini:$0000)
TXTCOL:         EQU $F3B5

; F3B7-F3B8: BASE(2): pattern table address for SCREEN 0 (ini:$0800)
; used to initialize CGPBAS when SCREEN 0 is activated
TXTCGP:         EQU $F3B7

; F3B9-F3BA: BASE(3): sprite attribute table address for SCREEN 0, unused (ini:$0000)
; used to initialize ATRBAS when SCREEN 0 is activated
TXTATR:         EQU $F3B9

; F3BB-F3BC: BASE(4): sprite pattern table address for SCREEN 0, unused (ini:$0000)
; used to initialize PATBAS when SCREEN 0 is activated
TXTPAT:         EQU $F3BB

; F3BD-F3BE: BASE(5): nametable address for SCREEN 1 (ini:$1800)
; used to initialize NAMBAS when SCREEN 1 is activated
T32NAM:         EQU $F3BD

; F3BF-F3C0: BASE(6): color table address for SCREEN 1 (ini:$2000)
T32COL:         EQU $F3BF

; F3C1-F3C2: BASE(7): pattern table address for SCREEN 1 (ini:$0000)
; used to initialize CGPBAS when SCREEN 1 is activated
T32CGP:         EQU $F3C1

; F3C3-F3C4: BASE(8): sprite attribute table address for SCREEN 1 (ini:$1B00)
; used to initialize ATRBAS when SCREEN 1 is activated
T32ATR:         EQU $F3C3

; F3C5-F3C6: BASE(9): sprite pattern table address for SCREEN 1 (ini:$0800)
; used to initialize PATBAS when SCREEN 1 is activated
T32PAT:         EQU $F3C5

; F3C7-F3C8: BASE(10): name table address for SCREEN 2 (ini:$1800)
; used to initialize NAMBAS when SCREEN 2 is activated
GRPNAM:         EQU $F3C7

; F3C9-F3CA: BASE(11): color table address for SCREEN 2 (ini:$2000)
GRPCOL:         EQU $F3C9                         ; Screen2 Color

; F3CB-F3CC: BASE(12): pattern table address for SCREEN 2 (ini:$0000)
; used to initialize CGPBAS when SCREEN 2 is activated
GRPCGP:         EQU $F3CB

; F3CD-F3CE: BASE(13): sprite attribute table address for SCREEN 2 (ini:$1B00)
; used to initialize ATRBAS when SCREEN 2 is activated
GRPATR:         EQU $F3CD

; F3CF-F3D0: BASE(14): sprite pattern table address for SCREEN 2 (ini:$3800)
; used to initialize PATBAS when SCREEN 2 is activated
GRPPAT:         EQU $F3CF

; F3D1-F3D2: BASE(15): name table address for SCREEN 3 (ini:$0800)
; used to initialize NAMBAS when SCREEN 3 is activated
MLTNAM:         EQU $F3D1

; F3D3-F3D4: BASE(16): color table address for SCREEN 3 (ini:$0000)
; the color table is unused in SCREEN 3
MLTCOL:         EQU $F3D3

; F3D5-F3D6: BASE(17): pattern table address for SCREEN 3 (ini:$0000)
; used to initialize CGPBAS when SCREEN 3 is activated
MLTCGP:         EQU $F3D5

; F3D7-F3D8: BASE(18): sprite attribute table address for SCREEN 3 (ini:$1B00)
; used to initialize ATRBAS when SCREEN 3 is activated
MLTATR:         EQU $F3D7

; F3D9-F3DA: BASE(19): sprite pattern table address for SCREEN 3 (ini:$3800)
; used to initialize PATBAS when SCREEN 3 is activated
MLTPAT:         EQU $F3D9

; F3db: keyclick when a key is pressed: 0: no, 1: yes (ini: 1)
; SCREEN ,,n will write to this address
CLIKSW:         EQU $F3db

; F3DC: line where the cursor is located
; starts to count at 1 for the topmost line
CSRY:           EQU $F3DC

; F3DD: column where the cursor is located
; starts to count at 1 for the leftmost column
CSRX:           EQU $F3DD

; F3DE: function key definition shown: 0: no, -1: yes
; Note: MSX BIOS will mess up end-of-screen if this variable contains
;       something other than $00 or $FF.
CNSDFG:         EQU $F3DE

; F3DF-D3E6: storage for the last written value towar.ds VDP registers 0 till 7
; this is needed because these registers are write only
RG0SAV:         EQU $F3DF
RG1SAV:         EQU $F3E0
RG2SAV:         EQU $F3E1
RG3SAV:         EQU $F3E2
RG4SAV:         EQU $F3E3
RG5SAV:         EQU $F3E4
RG6SAV:         EQU $F3E5
RG7SAV:         EQU $F3E6
; F3E7: last read value of VDP register 8
STATFL:         EQU $F3E7

; F3E8: information about the joystick and space bar
; 7 6 5 4 3 2 1 0
; | | | |       +-- Space bar, trig(0) (0 = pressed)
; | | | +---------- Stick 1, Trigger 1 (0 = pressed)
; | | +------------ Stick 1, Trigger 2 (0 = pressed)
; | +-------------- Stick 2, Trigger 1 (0 = pressed)
; +---------------- Stick 2, Trigger 2 (0 = pressed)
TRGFLG:         EQU $F3E8

; F3E9: code for the standard foreground color (ini:15)
FORCLR:         EQU $F3E9

; F3EA: code for the standard background color (ini:4)
BAKCLR:         EQU $F3EA

; F3EB: code for the standard border color (ini:7)
BDRCLR:         EQU $F3EB

; F3EC-F3EE: Jump instruction used by Basic LINE command.
; The routines used are: RIGHTC, LEFTC, UPC and DOWNC
MAXUPD:         EQU $F3EC

; F3EF-F3F1: Jump instruction used by Basic LINE command.
; The routines used are: RIGHTC, LEFTC, UPC and DOWNC
MINUPD:         EQU $F3EF

; F3F2: working color, as used for graphical operations
; normally equals to the foreground color (ini:15)
ATRBYT:         EQU $F3F2

; F3F3-F3F4: starting value of the address of the queue-table
; the queue-table contains 4 queue's: 3 for sound and one for RS232
; (ini: QUETAB ($F959))
QUEUES:         EQU $F3F3

; F3F5: CLOAD flag =0 when CLOAD =255 when CLOAD?
FRCNEW:         EQU $F3F5

; F3F6: VDP-interupt counter that counts from 3 to 0, when it reaches zero, the
; keyboard matrix is scanned, and the counters is reset at 3
SCNCNT:         EQU $F3F6

; F3F7: key repeat counter. Runs from 13 to 0, and is changed when SCNCNT is changed
; if the key remained the same. If it reaches 0, keyrepetition starts. If another key
; is pressed the value is reset at 13.
REPCNT:         EQU $F3F7

; F3F8-F3F9: first free space in the inputbuffer of the keyboard
; everytime a key is added to the inputbuffer, this address is incremented,
; when it equals to GETPNT, the buffer is full
; the buffer is located at KEYBUF
PUTPNT:         EQU $F3F8                         ; L[obt@ÖÌ|C^

; F3FA-F3FB: address in inputbuffer of first character that is not yet read
; everytime a key is read from the buffer it is incremented
; the buffer is located at KEYBUF
GETPNT:         EQU $F3FA                         ; L[obt@ÖÌ|C^

; F3FC-F400: memory area for tape system parameters for 1200 baud
; F3FC: length of  low signal for 0     (ini:83)
; F3FD: length of high signal for 0     (ini:92)
; F3FE: length of  low signal for 1     (ini:38)
; F3FF: length of high signal for 1     (ini:45)
; F400: length of synchronization block (ini:15)
CS120:          EQU $F3FC

; F401-F405: memory area for tape system parameters for 1200 baud
; F401: length of  low signal for 0     (ini:37)
; F402: length of high signal for 0     (ini:45)
; F403: length of  low signal for 1     (ini:14)
; F404: length of high signal for 1     (ini:22)
; F405: length of synchronization block (ini:31)
CS240:          EQU $F401

; F406-F407: lenghts of signal for 0 for the current speed of the tape system
; either equal to the content of F3FC-F3FD or the content of F401-F402
; (ini: 83, 92)
LOW_:           EQU $F406                         ; real name: LOW, but doesn't compile?

; F408-F409: lenghts of signal for 1 for the current speed of the tape system
; either equal to the content of F3FE-F3FF or the content of F403-F404
; (ini: 38, 45)
HIGH_:          EQU $F408                         ; real name: HIGH, but doesn't compile?

; F40A: lenghts of synchronization block for the current speed of the tape system
; either equal to the content of F400 or the content of F405 (ini: 15)
HEADER:         EQU $F40A

; F40B-F40C: standard setting for the height/width aspect of the
; BASIC statement CIRCLE; only the byte in F40B is actually used
; If ASPECT2 is larger then 255, the value of F40B is the number of horizontal
; dots per 256 verical dots of the radius (ini:$0100)
; ! not verified :)
ASPCT1:         EQU $F40B

; F40D-F40E: standard setting for the height/width aspect of the
; BASIC statement CIRCLE; If ASPCT2 is smaller then 512, then ASPCT2 is the
; number of vertical dots per 256 horizontal dots of the radius (ini:$0100)
; ! not verified :)
ASPCT2:         EQU $F40D

; F40F-F413: work area for the BASIC statement RESUME NEXT
; contains a fake end of basic program
; (ini: 58, 0, 0, 0, 0)
ENDPRG:         EQU $F40F

; F414: errornumber of last error that happened while executing a BASIC program
; (ini:0)
ERRFLG:         EQU $F414

; F415: number of characters in the writebuffer of the printer that still
; need printing
LPTPOS:         EQU $F415

; F416: switch indicating if output should be screen or printer
; (think LIST vs LLIST) (ini:0) values: 0: screen, 1: printer
PRTFLG:         EQU $F416

; F417: switch indicating if hooked up printer is an MSX printer or not
; values: 0: MSX-Printer, 1: no MSX-Printer
; if the printer is no MSX-Printer, non-ASCII (>=128) characters are replaced
; by spaces before sending them to the printer (ini: 0)
NTMSXP:         EQU $F417

; F418: switch indicating of printing routines should use raw-mode or
; should convert:
; =0 to convert tabs and unknown characters to spaces and remove graphical headers
; =1 to send data just like it gets it (ini: 0)
; if RAWPRT is 1, the value if NTMSXP is ignored
RAWPRT:         EQU $F418

; ---------------------------
; basic interpreter work area
; ---------------------------

; F419-F41A: work area for the BASIC command VAL: contains address of character that
; has temporarely been replaced by O by VAL
VLZADR:         EQU $F419

; F41B: work area for the BASIC command VAL: contains the character originally at
; the location of VLZADR
VLZDAT:         EQU $F41B

; F41C-F41D: line number of current BASIC line being executed, in direct modus this
; contains $FFFF (ini:$FFFF)
CURLIN:         EQU $F41C

; F41E: error detection prefix for KBUF, always contains ":"
; originally undocumented :)
KBFMIN:         EQU $F41E

; F41F-F55C: workarea for coding basic rules that have been typed in direct modus
; this are contains the code for the line interpreted in direct modus
KBUF:           EQU $F41F

; F55D: byte used as first byte of BUF for input statements, giving them always
; an extra ',' (ini:44 == ',')
BUFMIN:         EQU $F55D

; F55E-F65F: used in direct modus to store the ASCII codes of the line, or simulary
; for INPUT or LINE INPUT BASIC statements
BUF:            EQU $F55E

; F562-F570: used by bitblit routines to store the register data
SX:             EQU $F562
SY:             EQU $F564
DX:             EQU $F566
DY:             EQU $F568
NX:             EQU $F56A
NY:             EQU $F56C
CDUMMY:         EQU $F56E
ARG_:           EQU $F56F
L_OP:           EQU $F570

; F660: last usable byte of BUF
ENDBUF:         EQU $F660

; F661: number of column of last written character on the screen
TTYPOS:         EQU $F661

; F662: switch indicating during variable lookup for arrays, if this has not already
; been done for a DIM BASIC statement
DIMFLG:         EQU $F662

; F663: workarea for evaluation of expressions; contains type of last evaluated
; expression; the value of the expression is in DAC, possible values of VALTYP:
; 2: integer
; 3: string
; 4: normal real
; 8: double real
VALTYP:         EQU $F663

; F664: workarea for coding of BASIC statements. switch indicating if keywor.ds have
; to be encoded or not. E.g. in DATA fiel.ds they should not be encoded
; 0: encoding on, 1: encoding off
DORES:          EQU $F664

; F665: workarea for coding of BASIC statements. swithc indication of numbers have to be
; encoded; values: $0: encode as const, $1: encode as line number, $FF: do not encode
DONUM:          EQU $F665

; F666-F667: work area for evaluation of expressions: contains address of first character
; after the code of the last evaluated expression
CONTXT:         EQU $F666

; F668: work area for evaluation of expressions: contains information byte about the
; encoding of the last evaluated constant number; value of this constant is in CONLO
; values:
; $0B: octal (2 bytes)
; $0C: hexadecimal (2 bytes)
; $0F: decimal 0<=value<256 (1 byte)
; $11-$1B: short encoding for 0->10
; $1C: decimal (2bytes, 2s-compliment)
; $26: $42 binary as ASCII
; $0E: line number
; $0D: line pointer
; $1D: normal real (1 byte exp, 3 bytes BCD)
; $1F: double real (1 byte exp, 7 bytes BCD)
CONSAV:         EQU $F668

; F669: work area for evaluation of expressions: contains type of last evaluated number
; constant; the value is in CONLO, for values of CONTYP, see VALTYP
; Strings are never contant in BASIC!
CONTYP:         EQU $F669

; F66A-F671: work area for evaluation of expressions: contains the value of the last
; evaluated number contant; value starts at F66A, and takes bytes as needed for the type
CONLO:          EQU $F66A

; F672-F673: upper limit of memory area reserved for strings, contains the upper address
; that is allowed to be used
MEMSIZ:         EQU $F672

; F674-F675: top of stack; also first byte below string area
STKTOP:         EQU $F674

; F676-F677: start address of current basic program, set at initialization, and
; not changed by OS (ini:$8001)
TXTTAB:         EQU $F676

; F678-F679: address of first unused string-descriptor in TEMPST
; (ini:value of TEMPST)
TEMPPT:         EQU $F678

; F67A-F697: work area for evaluation of string expressions; this area has space
; for 10 string descriptors of 3 bytes; these can be used for temporarely results
; of string arythmetics
TEMPST:         EQU $F67A

; F698-F69A: work area for evaluation of string expressions; this contains the
; string descriptor of the intermediate result
DSCTMP:         EQU $F698

; F69B-F69C: first address within the string memory area that is still free
; the string area is filled backwar.ds, soo the lower the value, the less space
; remains (ini: value of MEMSIZ)
FRETOP:         EQU $F69B

; F69D-F69E: temporarely storage for adminstration of the basic interpreter
TEMP3:          EQU $F69D

; F69F-F6A0: temporarely storage for garbage collection
TEMP8:          EQU $F69F

; F6A1-F6A2: address of first byte in BASIC-code after last FOR statement
ENDFOR:         EQU $F6A1

; F6A3-F6A4: line number of last used line of DATA statements
DATLIN:         EQU $F6A3

; F6A5: switch indicating if a variable is allowed to be an array variable.
; This is e.g. not allowed for the loop variable of a FOR statement
; 0 = allowed, 1 = not allowed
SUBFLG:         EQU $F6A5

; F6A6: switch indicating if currently a READ or INPUT statement is being executed
FLKINP:         EQU $F6A6

; F6A7-F6A8: temporarely storage for adminstration of the basic interpreter
TEMP:           EQU $F6A7

; F6A9: switch indicating if there are still linenumber constants in the BASIC code
; that are encoded as pointers?
PTRFLG:         EQU $F6A9

; F6AA: switch indication if currently an AUTO statement is active
; 0 = no auto, 1 = auto
AUTFLG:         EQU $F6AA

; F6AB-F6AC: last generated AUTO line number
AUTLIN:         EQU $F6AB

; F6AD-F6AE: last used AUTO increment
AUTINC:         EQU $F6AD

; F6AF-F6B0: work area of the error system; contains address of first byte
; of statement currently being executed
SAVTXT:         EQU $F6AF

; F6B1-F6B2: work area of the error system; contains address of the stack
; before executing of the current statement started
SAVSTK:         EQU $F6B1

; F6B3-F6B4: line number of last failed line
ERRLIN:         EQU $F6B3

; F6B5-F6B6: line number of last used (changed, listed, added) line
DOT:            EQU $F6B5

; F6B7-F5B8: work area of the error system; contains the address of the first
; byte of the statement that last failed; on failure it is stored with the
; content of SAVTXT
ERRTXT:         EQU $F6B7

; F6B9-F6BA: work area of the error system; contains the line number where
; execution should go to on error (as in basic: ON ERROR GOTO x)
ONELIN:         EQU $F6B9

; F6BB-F6BC: work area of the error system; indication if the interpreter is
; currently executing an error catch routine
; 0 = no, FF = yes
ONEFLG:         EQU $F6BB

; F6BC-F6BD: temporarely storage for the interpreter
TEMP2:          EQU $F6BC

; F6BE-F6BF: line number of last program break, reset at 0 at any program change
OLDLIN:         EQU $F6BE

; F6C0-F6C1: address of first statement that is not executed due to a break
OLDTXT:         EQU $F6C0

; F6C2-F6C3: begin address of storage of basic variables and function descriptors;
; possibly adjusted when program changes in size
VARTAB:         EQU $F6C2

; F6C4-F6C5: begin address of array variables; possibly adjusted when program
; changes size or more variables are allocated
ARYTAB:         EQU $F6C4

; F6C6-F6C7: address of first free byte not used for storage of code or variables
; (ini: $8003)
STREND:         EQU $F6C6

; F6C8-F6C9: address where data nee.ds to be searched at next READ statement
DATPTR:         EQU $F6C8

; F6CA-F6E3: table with variable types, one for each letter in the alphabet
; possible values:
;       2 = integer     3 = string      4 = single      8 = double
DEFTBL:         EQU $F6CA

; F6E4-F7B4: work area for execution of self defined functions

; F6E4-F6E5: contains address ; of previous parameter block on the stack;
; needed for garbage collection
PRMSTK:         EQU $F6E4

; F6E6-F6E7:  amount of valid bytes in PARM1
PRMLEN:         EQU $F6E6

; F6E8-F74B: contains definitions of the variables in the parameter lists
; of self defined functions
PARM1:          EQU $F6E8

; F74C-F74D: previous value of PRMSTK
PRMDRV:         EQU $F74C

; F74E-F74F: number of valid bytes in PARM2
PRMLN2:         EQU $F74E

; F750-F7B3: area used for calculation of values that end up in PARM1
PARM2:          EQU $F750

; F7B4: switch indicating of while searching a variable name PARM1 has
; been looked at; 0 = no, 1 = yes
PRMFLG:         EQU $F7B4

; F7B5-F7B6: address of first byte where it is no longer needed to search
; for a variable name; it is equal to ARYTAB when the normal variable area
; is searched, and equal to PARM1+PRMLEN when PARM1 is searched
ARYTA2:         EQU $F7B5

; F7B7-F7B8: switch indicating iif PARM1 contains a valid parameter block
; 0 = no, 1 = yes
NOFUNS:         EQU $F7B7

; F7B8-F7B9: temporarely memory used while searching parameter blocks on
; the stack
TEMP9:          EQU $F7B8

; F7BA-F7BB: counter of the nesting-dept of the function being evaluated
FUNACT:         EQU $F7BA

; F7BC-F7C3: work area when executing the SWAP statement; the first variable
; is stored here
SWPTMP:         EQU $F7BC

; F7C4: switch indicating if TRON is on; 0 = off, >0 = on
TRCFLG:         EQU $F7C4

; F7C5-F7F4: workarea when executing numeric operators
FBUFFR:         EQU $F7C5
DECTMP:         EQU $F7F0
DECTM2:         EQU $F7F2
DECCNT:         EQU $F7F4

; F7F6-F805: workarea when executing numeric operators; intermediate
; results are stored here; also used for parameter transfer when using
; the USR functions; VALTYPE then contains the type, and the value is
; stored like this:
; typename  type  where
; integer   2     F7F8-F7F9
; string    3     F7F8-F7F9 (address descriptor)
; single    4     F7F6-F7F9
; double    8     F7F6-F7FD
DAC:            EQU $F7F6

; F806-F856: workarea when executing numeric operators
HOLD8:          EQU $F806
HOLD2:          EQU $F836
HOLD:           EQU $F83E
ARG:            EQU $F847

; F857-F85E: last calculated random double
RNDX:           EQU $F857

; --------------------
; filesystem work area
; --------------------

; F85F: # of filedescriptors reserved minus 1
; this is also the maximum number of open files possible
MAXFIL:         EQU $F85F

; F860-F861: start address of the file information table
FILTAB:         EQU $F860

; F862-F863: start address of the first file-buffer
NULBUF:         EQU $F862

; F864-F865: during file I/O the start address of the active file-buffer
PTRFIL:         EQU $F864

; F866: flag indicating if the file that is being loaded have to be started
; immediately; 0 = no, FF = yes
RUNFLG:         EQU $F866

; note that RUNFLG and FILNAM overlap!

; F866-F870: filename of last file that has been active;
; first 8 chars are name, last 3 are extension
FILNAM:         EQU $F866

; F871-F87B: second filename if needed, e.g. the NAME command
FILNM2:         EQU $F871

; F87C: switch indicating if currently a BASIC program is being loaded
; 0 = no, 1 = yes
NLONLY:         EQU $F87C

; F87D-F87E: workarea for BLOAD and BSAVE; when a part of normal memory
; is written, it contains the end address of the written memory region
; if video memory is written it contains $4BE5 + start address of the
; written memory region ??
SAVEND:         EQU $F87D

; F87F-F91E: storage area for the text of the function keys 10x16 bytes,
; but strings need to be zero-terminated, soo maximum length of command is
; 15 characters
FNKSTR:         EQU $F87F

; ------------------------
; screen routine work area
; ------------------------

; F91F-F921: start address of the standard ASCII pattern table
; at every change towar.ds a text mode it is copied in the pattern table
; of the VDP
;   F91F: slot indication (ini: 0)
;   F920-F921: address (ini: 1BBF)
; TODO: make CBIOS use this value instead of hardcoded value
CGPNT:          EQU $F91F

; F922-F923: start address of the nametable in the VRAM
NAMBAS:         EQU $F922

; F924-F925: start address of the pattern table in the VRAM
CGPBAS:         EQU $F924

; F926-F927: start address of the sprite pattern table in the VRAM
PATBAS:         EQU $F926

; F928-F929: start address of the sprite location table in the VRAM
ATRBAS:         EQU $F928

; F92A-F92B: address in VRAM of the pattern of the current position
; on screen
CLOC:           EQU $F92A

; F92C: mask for CLOC selecting the right bits that correspond with
; the current position
CMASK:          EQU $F92C

; F92D-F930: work area for graphical calculations
MINDEL:         EQU $F92D
MAXDEL:         EQU $F92F

; ----------------------------------------------
; F931-F941: work area for calculation of CIRCLE
; ----------------------------------------------

; F931-F932: ratio of # of dots in the horizontal and vertical direction
; if = $0100 then ASPCT1 and ASPCT2 are used
; if < $0100 then it is the # of dots in one direction for each
; $0100 # of dots in the other direction; the direction is indicated
; by CSCLXY
ASPECT:         EQU $F931

; F933:F934: ; distance, in # of dots from the center of the most
; distant point of the circle
CENCNT:         EQU $F933

; F935: switch indication if the start and/or end point need to be
; connected to the center
;  bit 7: connect end point; 1 = yes
;  bit 0: connect start point; 1 = yes
CLINEF:         EQU $F935

; F936-F937: used during calculation of CIRCLE
CNPNTS:         EQU $F936

; F938: direction of drawing of circle:
;  00 = from CSTCNT towar.ds CENCNT
;  FF = from CENCNT towar.ds CSTCNT
CPLOTF:         EQU $F938

; F939-F93A: used during calculation of CIRCLE
CPCNT:          EQU $F939

; F93B-F93C: ; contains the total # of dots of the full circle,
; even when only a part is drawn
CPCNT8:         EQU $F93B

; F93D-F93E: used during calculation of CIRCLE
CRCSUM:         EQU $F93D

; F93F-F940: ; distance in dots from the center towar.ds the closest
; circle point
CSTCNT:         EQU $F93F

; F941: switch indicating if the X or Y direction nee.ds to be streched:
; 0 = X, 1 = Y
CSCLXY:         EQU $F941

; F942-F943: store of CLOC, also used for PAINT
CSAVEA:         EQU $F942

; F944: storage of CMASK; also used for PAINT
CSAVEM:         EQU $F944

; F945-F946: horizontal distance towar.ds the center
CXOFF:          EQU $F945

; F947-F948: vertical distance towar.ds the center
CYOFF:          EQU $F947

; -------------------------------------------
; work area for executing the PAINT statement
; -------------------------------------------

; F949: leftmost position of protrusion towar.ds the left
LOHMSK:         EQU $F949

; F94A: new workdirection for protrusion towar.ds the left
LOHDIR:         EQU $F94A

;F94B-F94C: leftmost position of protrusion towar.ds the left
LOHADR:         EQU $F94B

; F94D: size of protrusion towar.ds the left
LOHCNT:         EQU $F94D

; F94F-F950: # of pixels that may be skipped
SKPCNT:         EQU $F94F

; F951-F952: # of movements
MOVCNT:         EQU $F951

; F953: current direction; $40 = \/, $C0 = /\, $00 = stop
PDIREC:         EQU $F953

; F954: indicate if paint towar.ds the left worked
LFPROG:         EQU $F954

; F955: indicate of a paint towar.ds the right worked
RTPROG:         EQU $F955

; F956-F957: start address of a jumptable for subcomman.ds
; contained in a string variable, used for both PLAY and DRAW
; where this systemvar points to either the PLAY or the DRAW
; table
MCLTAB:         EQU $F956

; F958: switch indication if MCLTAB is for PLAY or DRAW
; $0 = DRAW, $FF = PLAY
MCLFLG:         EQU $F958

; ------------------------------------------
; work area for sound and queueing and RS232
; ------------------------------------------

; F959-F971: Variables for three music queues and one RS232 queue
; F959: VOICAQ put position
; F95A: VOICAQ get position
; F95B: VOICAQ putback flag
; F95C: VOICAQ size
; F95D: VOICAQ address
; F95F-F964: VOICBQ
; F965-F96A: VOICCQ
; F96B-F970: RS2IQ
QUETAB:         EQU $F959

; Putback characters for queues. TODO: what purpose do these have exactly?
QUEBAK:         EQU $F971

; Buffers for queues.
VOICAQ:         EQU $F975                         ; Voice A queue
VOICBQ:         EQU $F9F5                         ; Voice B queue
VOICCQ:         EQU $FA75                         ; Voice C queue
RS2IQ:          EQU $FAF5                         ; RS232   queue

; in MSX2 the content of RS2IQ is used differently:
DPPAGE:         EQU $FAF5                         ; Display page (SCR5+)
ACPAGE:         EQU $FAF6                         ; Active page (SCR5+)

; FAF7: AV control port value storage
AVCSAV:         EQU $FAF7

; FAF8: extended BASIC ROM slot address
EXBRSA:         EQU $FAF8                         ; TuÊu

; FAF9: character count for ROMA-KANA
CHRCNT:         EQU $FAF9

; FAFA-FAFB: character save for ROMA-KANA
ROMA:           EQU $FAFA

; ROMA-KANA extension mode switch or VRAM size??
MODE:           EQU $FAFC
;Reserved       equ     $FAFD

; FAFE-FAFF: x position for mouse or lightpen
XSAVE:          EQU $FAFE

; FB00-FB01: y position for mouse or lightpen
YSAVE:          EQU $FB00
LOGOPR:         EQU $FB02
; FB21-FB28: Table which contains info for up to 4 disk ROMs, 2 bytes each:
; - first byte: number of drives connected to this interface
; - second byte: slot ID of the disk ROM
DRVINF:         EQU $FB21
; end of MSX2 only usage of RS2IQ

; --------------------------------
; work area for the PLAY statement
; --------------------------------

; FB35: status about the parsing of a PLAY string
;  bit 7: only one time parsed; 1 = yes
;  bit 1-0: number of parsed strings (0-3)
PRSCNT:         EQU $FB35

; FB36-FB37: storage of stack
SAVSP:          EQU $FB36

; FB38: # of voice currently being parsed (0-2)
VOICEN:         EQU $FB38

; FB39-FB3A: storage of volume of a muted voice
SAVVOL:         EQU $FB39

; FB3B: size of string being parsed (also used by DRAW)
MCLLEN:         EQU $FB3B

; FB3C-FB3D: address of string being parsed (also used by DRAW)
MCLPTR:         EQU $FB3C

; FB3E: temporarely storage of active queue # (0-2)
QUEUEN:         EQU $FB3E

; FB3F: flag indicating which queues are active
; bit 2 = queue 2; 1 = active
; bit 1 = queue 1; 1 = active
; bit 0 = queue 0; 1 = active
MUSICF:         EQU $FB3F

; FB40: count of the # of PLAY statements parsed, but not executed yet
PLYCNT:         EQU $FB40

; FB41-FB65: Voice Control Block for voice A (queue 0)
VCBA:           EQU $FB41
; FB66-FB8A: Voice Control Block for voice B (queue 1)
VCBB:           EQU $FB66
; FB8B-FBAF: Voice Control Block for voice C (queue 2)
VCBC:           EQU $FB8B

; each VCB has the following structure:

; name                  offset  length  purpose
METREX:         EQU 0                             ;     2       interrupt counter
VCXLEN:         EQU 2                             ;     1       MCLLEN for voice
VCXPTR:         EQU 3                             ;     2       MCLPTR for voice
VCXSTP:         EQU 5                             ;     2       stack pointer
QLENGX:         EQU 7                             ;     1       # bytes in queue
NTICSX:         EQU 8                             ;     2       new counter ?
TONPRX:         EQU 10                            ;     2       pitch
AMPLTX:         EQU 12                            ;     1       amplitude
ENVPRX:         EQU 13                            ;     2       envelope speed
OCTAVX:         EQU 15                            ;     1       octave
NOTELX:         EQU 16                            ;     1       tone length
TEMPOX:         EQU 17                            ;     1       tempo
VOLUMX:         EQU 18                            ;     1       volume
ENVLPX:         EQU 19                            ;     1       envelope shape
MCLSTX:         EQU 33                            ;             space for stack storage
MCLSEX:         EQU 36                            ;             start of stack
; the stack mentioned above is used to store bytevalues
; that are readied to be put on the voice queue

; -----------------------------------------------
; settings for screen editor and interrupt system
; -----------------------------------------------

; FBB0: switch indicating if software reset is enabled
; 0 = n, 1 = yes; can be used to reset BASIC by pressing
; SHIFT-CODE-GRAPH; does not erase the existing program
; (ini: 0)
ENSTOP:         EQU $FBB0

; FBB1: switch indicating if the current BASIC program is in a ROM
; 0 = no; 1 = yes
BASROM:         EQU $FBB1

; FBB2-FBC9: table containing for each line if it continues on the
; next line; 0 = yes, >0 = no
LINTTB:         EQU $FBB2

; FBCA-FBCB storage of location of cursor for INLIN and QINLIN
;  FBCA: CSRY , FBCB: CSRX
FSTPOS:         EQU $FBCA

; ASCII code of the character currently covered by the cursor
; TODO: is the name CURSAV or CO.dsAV ?
CURSAV:         EQU $FBCC

; FBCD: switch indicating which function keys are to be displayed
; on the screen; 0 = F6-F10, 1 = F1-F5
FNKSWI:         EQU $FBCD

; FBCE-FBD7: for each function key, a flag indicating if it has
; interrupt facility enabled; 0 = disabled, 1 = enabled
FNKFLG:         EQU $FBCE

; FBD8: counter of # of interrupts that still have a pending ON .. GOSUB
ONGSBF:         EQU $FBD8

; FBD9: flag indicating if a keyclick has already been generated, to avoid
; keyclicks for a key that generates two ASCII codes
; $00 = no click, $0F = click
CLIKFL:         EQU $FBD9

; FBDA-FBE4: storage of keyboard matrix, used for detection key repetition
OLDKEY:         EQU $FBDA

; FBE5-FBEF: current state of the keyboard matrix
NEWKEY:         EQU $FBE5

; keyboard buffer; each char entered via the keyboard en.ds up here
KEYBUF:         EQU $FBF0
; LIMPNT: something about "key buffer pointer"
LIMPNT:         EQU $FC17                         ; L[obt@ÖÌ|C^

; FC18-FC3F: work area for processing the last typed line
LINWRK:         EQU $FC18                         ; 40ªÌobt@

; FC40-FC47: storage for the patter of an ASCII character
; used when writing an ASCII character in a graphical mode
PATWRK:         EQU $FC40

; FC48-FC49: lowest address of the RAM memory; initialized at startup
; and not changed normally
BOTTOM:         EQU $FC48

; FC4A-FC4B: highest address of the RAM memory that is not reserved by
; the OS; string area, filebuffers and stack are below this address
; initialized at startup and not changed normally
HIMEM:          EQU $FC4A

; FC4C-FC99: table for interrupt facilities of MSX BASIC
; each 3 bytes are used like this:
; byte 1 is a flag:
;  bit 2: interrupt happened; 1 = yes
;  bit 1: interrupt stop; 1 = yes
;  bit 0: interrupt off; 1 = no
; byte 2-3 is the adress of the line in BASIC where should be
; jumped too
; the offsets in the table are:
;  offset  address interrupt
;       0  FC4C    F1
;       3  FC4F    F2
;       6  FC52    F3
;       9  FC55    F4
;      12  FC58    F5
;      15  FC5B    F6
;      18  FC5E    F7
;      21  FC61    F8
;      24  FC64    F9
;      27  FC67    F10
;      30  FC6A    STOP
;      33  FC6D    sprite collision
;      36  FC70    SPACE (trigger 0)
;      39  FC73    joystick 1 button 1 (trigger 1)
;      39  FC76    joystick 2 button 1 (trigger 2)
;      39  FC79    joystick 1 button 2 (trigger 3)
;      39  FC7C    joystick 2 button 2 (trigger 4)
;      39  FC7F    interval
TRPTBL:         EQU $FC4C

; FC9A: usage unknown
RTYCNT:         EQU $FC9A

; FC9B: STOP indication
; 0 = nothing; 3 = CTRL+STOP, 4 = STOP
INTFLG:         EQU $FC9B

; FC9C: last read Y-position of a touchpad
PADY:           EQU $FC9C

; FC9D: last read X-position of a touchpad
PADX:           EQU $FC9D

; FC9E-FC9F: software clock, updated at each VDP interrupt
JIFFY:          EQU $FC9E                         ; timer counter

; FCA0-FCA1: initial value of INTCNT, used when INTCNT
; reaches 0; used for ON INTERVAL GOSUB
INTVAL:         EQU $FCA0

; FCA2-FCA3: interrupt counter; lowered at each VDP interrupt;
; reset with value of INTVAL when it reaches zero; if interval
; interrupt is needed, it is generated
INTCNT:         EQU $FCA2

; FCA4-FCA5: parameter used at tap input, given a value during
; reading of a headerblock from tape
LOWLIM:         EQU $FCA4
WINWID:         EQU $FCA5

; FCA6: flag indicating if the previous character written
; to the screen was an extension character for graphical signs
; (ASCII 1); 0 = no, 1 = yes
GRPHED:         EQU $FCA6

; FCA7 ESCCNT State of a state machine that handles the printing of escape
; sequences. A subset of the VT52 escape sequences is supported.
; values:
; $00: not inside an escape sequence
; $01: seen <ESC>x
; $02: seen <ESC>y
; $03: seen <ESC>Y<row>
; $04: seen <ESC>Y
; $FF: seen <ESC>
ESCCNT:         EQU $FCA7

; FCA8: switch indicating insert or overwrite mode
; $00 = overwrite; $FF = insert
; the value of INSFLG is changed each time the INS key is pressed
INSFLG:         EQU $FCA8

; FCA9: show cursor; 0 = no, 1 = yes
; can be changed with escape sequences x5 and y5
CSRSW:          EQU $FCA9

; FCAA: shape of cursor; 0 = block; 1 = insert
; pressing the INS key changes the value of CSTYLE
; can be changed with escape sequences x4 and y4
CSTYLE:         EQU $FCAA

; switch indicating if the CAPS-LOCK is on
; $00 = off, $FF = on (unofficial: $80 = perma-on)
CAPST:          EQU $FCAB

; FCAC: dead key control in non-japanese MSX models
; ad.ds a mark on the next char pressed, if applicable
;  0 = no dead key
;  1 = dead key                => accent grave
;  2 = SHIFT + dead key        => accent aigu
;  3 = CODE + dead key         => accent circumflex
;  4 = SHIFT + CODE + dead key => trema
; in japanese models it controls the charset used
KANAST:         EQU $FCAC

; FCAD: only used in japanese MSX models; it defines
; the used typeset (ini: $40)
KANAMD:         EQU $FCAD

; ----
; misc
; ----

FLBMEM:         EQU $FCAE
SCRMOD:         EQU $FCAF
OLDSCR:         EQU $FCB0
CASPRV:         EQU $FCB1
BRDATR:         EQU $FCB2
GXPOS:          EQU $FCB3
GYPOS:          EQU $FCB5
GRPACX:         EQU $FCB7
GRPACY:         EQU $FCB9
DRWFLG:         EQU $FCBB
DRWANG:         EQU $FCBD
RUNBNF:         EQU $FCBE
SAVENT:         EQU $FCBF

; ---------------------------
; storage of slot information
; ---------------------------

; FCC1-FCC4: Information for each primary slot. The most significant bit is
; set if the primary slot is found to be expanded.
EXPTBL:         EQU $FCC1

; FCC5-FCC8: Duplicate the contents of the four possible secondary slot
; registers.
SLTTBL:         EQU $FCC5

; FCC9-FD08: Information for any extension ROMs found during the power-up
; ROM search.
; FCC9-FCCC: primary slot 0, secondary slot 0
; FCCD-FCD0: primary slot 0, secondary slot 1
; FCD1-FCD4: primary slot 0, secondary slot 2
; FCD5-FCD8: primary slot 0, secondary slot 3
; FCD9-FCE8: primary slot 1
; FCE9-FCF8: primary slot 2
; FCF9-FD08: primary slot 3
; The information is stored as below.
; bit 7 (set): BASIC program
; bit 6 (set): device handler
; bit 5 (set): statement handler
SLTATR:         EQU $FCC9

SLTWRK:         EQU $FD09

; ------------------------------
; storage of ROM-page parameters
; ------------------------------

PROCNM:         EQU $FD89
DEVICE:         EQU $FD99
; ------------
; system hooks
; ------------

; system hooks are defined in hooks.asm

; ------------------
; storage of VDP8-23
; ------------------

; FFE7-FFF6: storage of VDP 8-23
RG8SAV:         EQU $FFE7

; ----------------------
; extra slot information
; ----------------------

; FFF7: slot address of main-rom
;?????:         equ     $FFF7

; ------------------
; storage of VDP25-27
; ------------------

; FFFA-FFFC: storage of VDP 25-27
RG25SAV:        EQU $FFFA

; ---------------------------
; subslot switching addresses
; ---------------------------

; FFFF: subslot switching address
; This is not actually a system variable, it is a har.dware register:
;   SSL_REGS (see har.dware.asm).

; -------
; the end
; -------

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:

; $Id: inlin.asm 525 2008-12-22 22:16:42Z mthuurne $
; INLIN/PINLIN/QINLIN routines for C-BIOS
;
; Copyright (c) 2007 Eric Boon.  All rights reserved.
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
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;--------------------------------
; $00AE PINLIN
; Function : Stores in the specified buffer the character codes input
;           until the return key or STOP key is pressed
; Output   : HL - for the starting address of the buffer -1
;            C-flag set when it ends with the STOP key
; Registers: All
; TODO: call H_PINL
pinlin:
        CALL    H_PINL
        LD      a,(AUTFLG)                        ; If AUTO is active
        AND     a
        JP      z,inlin                           ; then start line input
        LD      a,1                               ; else set cursor
        LD      (CSRX),a                          ;    to left border first
        JP      inlin                             ;    and then start line input

;--------------------------------
; $00B4 QINLIN
; Function : Prints a questionmark and one space and continues with INLIN
; Output   : HL - for the starting address of the buffer -1
;            C-flag set when it ends with the STOP key
; Registers: All
qinlin_prompt:
        DB      "? ",0
qinlin:
        CALL    H_QINL
        LD      hl,qinlin_prompt
        CALL    prn_text
; continue with inlin

;--------------------------------
; $00B1 INLIN
; Function : Main line input routine
; Output   : HL - for the starting address of the buffer -1
;            C-flag set when it ends with the STOP key
; Registers: All

inlin:
        LD      IX,BUF                            ; RESET TO BEGINNING OF BUFFER
        LD      A,0                               ;
        LD      (ENDBUF),A                        ;
inlin_LOOP1:
        LD      hl,(CSRX)                         ; loads CSRX and CSRY
        LD      (FSTPOS),hl                       ; save in FSTPOS
        LD      de,LINTTB-2                       ; break logical line
        LD      h,0                               ; above cursor pos
        LD      a,l
        ADD     hl,de
        LD      (hl),a
inlin_loop:
        CALL    chget                             ; get a character from the kbd

        CP      $7F
        JP      z,inlin_del
        CP      $20
        JR      nc,inlin_printable

        LD      b,20
        LD      hl,inlin_table
        CALL    jump_table

        XOR     a                                 ; we just put out a ctrl char
        LD      (INSFLG),a                        ; switch insert mode off
        LD      (CSTYLE),a
        JR      inlin_loop

inlin_printable:                                  ; else...
        PUSH    af
        LD      a,(INSFLG)
        AND     a
        CALL    nz,inlin_insert
        POP     af
        LD      (IX),A
        INC     IX
        LD      B,A
        LD      A,(ENDBUF)
        INC     A
        LD      (ENDBUF),A
        JP      Z,inlin_cr
        LD      A,B
        RST     18h
        JR      inlin_LOOP1
; ----------------------------------------------
inlin_insert:
        CALL    chput_remove_cursor
        LD      hl,(CSRY)                         ; save cursorpos
        LD      (TEMP2),hl

        LD      a,' '                             ; oldchar = space
        LD      (TEMP),a

inlin_insert_loop:                                ; REPEAT
        CALL    curs2hl                           ;   get char under curpos
        CALL    rdvrm

        CP      ' '                               ;   IF is space
        JR      nz,inlin_insert_cont

        LD      hl,(CSRY)                         ;   AND at end of line
        LD      a,(LINLEN)
        CP      h
        JR      nz,inlin_insert_cont1

        LD      h,0                               ;   AND logical line does
        LD      de,LINTTB-1                       ;     not continue
        ADD     hl,de
        LD      a,(hl)
        OR      a
        JR      z,inlin_insert_cont1

        LD      a,(TEMP)                          ;   THEN
        CALL    curs2hl
        CALL    wrtvrm                            ;     put old char
        LD      hl,(TEMP2)                        ;     restore cursor pos
        LD      (CSRY),hl
        RET

inlin_insert_cont1:
        LD      a,' '
inlin_insert_cont:
        PUSH    af                                ;   ELSE
        LD      a,(TEMP)                          ;     put old char
        RST     18h
        POP     af
        LD      (TEMP),a                          ;   oldchar = character read
        JR      inlin_insert_loop                 ; ENDREP

; ----------------------------------------------
inlin_wback:
        RET

; ----------------------------------------------
inlin_break:
        SCF                                       ; C
        POP     hl                                ; do not return to INLIN
        RET                                       ; but to caller of INLIN

; ----------------------------------------------
inlin_clear:
        RET

; ----------------------------------------------
inlin_wfwd:
        RET

; ----------------------------------------------
inlin_bs:
        LD      B,A
        LD      A,(ENDBUF)
        INC     A
        DEC     A
        RET     Z
        DEC     IX
        LD      A,(ENDBUF)
        DEC     A
        LD      (ENDBUF),A
        LD      A,B
        CALL    $0018
        RET

; ----------------------------------------------
inlin_cr:
        XOR     a                                 ; NZ, NC
        LD      (IX),A                            ; NULL TERM
        CALL    chput_remove_cursor               ;
        POP     hl                                ; do not return to INLIN
        LD      HL,BUF                            ; RETURN BUFFER
        RET

; ----------------------------------------------
inlin_end:
        XOR     a                                 ; NZ, NC
        POP     hl                                ; do not return to INLIN
        LD      HL,BUF                            ; RETURN BUFFER
        RET                                       ; but to caller of INLIN

; ----------------------------------------------
inlin_ins:
        RET

; ----------------------------------------------
inlin_clrlin:
        RET

; -- ESCAPE
inlin_esc:
        RET                                       ; Do nothing

; -- DELETE
inlin_del:
        RET

; -- Jump table. Control chars not handled in one of the routines above
;    are simply forwarded to OUTDO
inlin_table:
        DW      $0018                             ; @
        DW      $0018                             ; A -
        DW      inlin_wback                       ; B word back
        DW      inlin_break                       ; C stop, abort, quit
        DW      $0018                             ; D
        DW      inlin_clear                       ; E: clear to end of line
        DW      inlin_wfwd                        ; F: word fwd
        DW      $0018                             ; G
        DW      inlin_bs                          ; H BACKSP: erase char left
        DW      $0018                             ; I
        DW      $0018                             ; J
        DW      $0018                             ; K
        DW      $0018                             ; L
        DW      inlin_cr                          ; M ENTER : confirm, yes, ok
        DW      inlin_end                         ; N to end of line
        DW      $0018                             ; O
        DW      $0018                             ; P
        DW      $0018                             ; Q
        DW      inlin_ins                         ; R INSERT: toggle insert mode
        DW      $0018                             ; S
        DW      $0018                             ; T
        DW      inlin_clrlin                      ; U clear line
        DW      $0018                             ; V
        DW      $0018                             ; W
        DW      $0018                             ; X
        DW      $0018                             ; Y
        DW      $0018                             ; Z
        DW      inlin_esc                         ; ESCAPE: ignore
        DW      $0018                             ; (28)
        DW      $0018                             ; (29)
        DW      $0018                             ; (30)
        DW      $0018                             ; (31)

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:

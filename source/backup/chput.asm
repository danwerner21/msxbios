; $Id: chput.asm 525 2008-12-22 22:16:42Z mthuurne $
; CHPUT routine for C-BIOS
;
; Copyright (c) 2006 Eric Boon.  All rights reserved.
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
; -------------------------------------
; $00A2 CHPUT
; Function : Output character in A to screen
; Input    : A = character code
; Output   : -
; Changes  : -

chput:
        PUSH    hl                                ; save all regs
        PUSH    de
        PUSH    bc
        PUSH    af
        CALL    H_CHPU                            ; call hook
        LD      a,(SCRMOD)                        ; this only works in
        CP      2                                 ; screen modes 0 and 1
        JR      nc,chput_exit
        POP     af                                ; restore char to put in A
        PUSH    af
        CALL    chput_remove_cursor
        POP     af                                ; restore char to put in A
        PUSH    af
        CALL    chput_decode_char
        CALL    chput_restore_cursor
        LD      a,(CSRX)                          ; CSRX -> TTYPOS
        LD      (TTYPOS),a
chput_exit:
        POP     af
        POP     bc
        POP     de
        POP     hl
        RET

; -- decode character in A
chput_decode_char:
        CALL    cnvchr                            ; Graphic character?
        RET     nc                                ; NC  -> graphic extension hdr
        JR      z,chput_putchar                   ; C,Z -> graphic character
        PUSH    af                                ; (C,NZ -> normal char)
        LD      a,(ESCCNT)                        ; ESC sequence?
        OR      a
        JP      nz,chput_escape
        POP     af
        CP      $20                               ; Control code?
        JR      c,chput_ctrl_search
        CP      127                               ; DEL?
        JP      z,chput_erase

; -- print a normal character and move cursor to next position
chput_putchar:
        CALL    curs2hl                           ; output character to screen
        CALL    wrtvrm

        LD      hl,(CSRY)                         ; h = (CSRX), l = (CSRY)
        LD      a,(LINLEN)
        INC     h
        CP      h
        JR      c,chput_continue_line
        LD      (CSRY),hl
        RET

chput_continue_line:
        LD      de,LINTTB-1                       ; make logical line continue
        LD      h,0
        ADD     hl,de
        XOR     a
        LD      (hl),a

        LD      hl,(CSRY)                         ; move cursor to start of
        CALL    chput_ctrl_cr                     ; new line
        JP      chput_ctrl_lf

; -- Handle control code
chput_ctrl_search:
        LD      b,12
        LD      hl,chput_ctrl_table
        JP      search_table

; -- Fill with spaces until next TAB stop
chput_ctrl_tab:
        LD      a,$20
        CALL    chput_putchar
        LD      a,(CSRX)
        AND     7
        CP      1
        JR      nz,chput_ctrl_tab
        RET

; -- Line Feed.
chput_ctrl_lf:
        LD      hl,(CSRY)
        LD      a,(CRTCNT)
        INC     l
        CP      l
        JR      nc,chput_ctrl_lf_done

        PUSH    hl
        CALL    chput_ctrl_home                   ; home cursor
        CALL    chput_esc_m                       ; delete top line (scroll!)
        POP     hl
        DEC     l

chput_ctrl_lf_done:
        LD      (CSRY),hl
        RET

; -- Home cursor
chput_ctrl_home:
chput_esc_h:
        LD      hl,$0101
        LD      (CSRY),hl
        RET

; -- Form Feed / Cls
chput_ctrl_ff:  EQU cls
chput_esc_e:    EQU cls
chput_esc_j:    EQU cls

; -- Clear till end of screen
chput_esc_jj:
        CALL    chput_esc_k                       ; clear till end of line
        LD      hl,(CSRY)                         ; save current cursor pos
        PUSH    hl
        CALL    chput_ctrl_cr                     ; move to line start
chput_esc_jj_loop:
        LD      a,(CSRY)                          ; while no at end of screen
        LD      hl,CRTCNT
        CP      (hl)
        JR      nc,chput_esc_jj_done
        CALL    chput_ctrl_lf                     ;   move to next line
        CALL    chput_esc_k                       ;   clear till end of line
        JR      chput_esc_jj_loop                 ; loop
chput_esc_jj_done:
        POP     hl                                ; restore cursor pos
        LD      (CSRY),hl
        RET

; -- Carriage return
chput_ctrl_cr:
        LD      a,1
        LD      (CSRX),a
        RET

; -- Escape
chput_ctrl_esc:
        LD      a,$FF
        LD      (ESCCNT),a
        RET

; -- Cursor right
chput_ctrl_right:
        LD      a,(CSRX)
        LD      hl,LINLEN
        CP      (hl)
        JR      nc,chput_ctrl_right_next
        INC     a
        JR      chput_right_left_ok
chput_ctrl_right_next:
        LD      a,(CSRY)
        LD      hl,CRTCNT
        CP      (hl)
        RET     nc
        INC     a
        LD      (CSRY),a
        JR      chput_ctrl_cr

; -- Cursor left
chput_ctrl_bs:
chput_ctrl_left:
        LD      a,(CSRX)
        DEC     a
        JR      nz,chput_right_left_ok
        LD      a,(CSRY)
        DEC     a
        RET     z
        LD      (CSRY),a
        LD      a,(LINLEN)
chput_right_left_ok:
        LD      (CSRX),a
        RET

; -- Cursor up
chput_ctrl_up:
chput_esc_a:
        LD      a,(CSRY)
        DEC     a
        RET     z
        LD      (CSRY),a
        RET

; -- Cursor down
chput_ctrl_down:
chput_esc_b:
        LD      a,(CSRY)
        LD      hl,CRTCNT
        CP      (hl)
        RET     nc
        INC     a
        LD      (CSRY),a
        RET

; -- Handle ESC mode (ESCCNT in A and != 0)
chput_escape:
        LD      b,a                               ; b := (ESCCNT)
        INC     a                                 ; (ESCCNT) == -1 ?
        JR      nz,chput_escape_1
        LD      (ESCCNT),a
        POP     af                                ; restore character in A
        LD      b,15                              ; search in table
        LD      hl,chput_esc_table
        JP      search_table

chput_escape_1:                                   ; ----------------------------
        POP     af
        DJNZ    chput_escape_2

; -- ESCCNT == 1: 'ESC x <n>'
        LD      c,0                               ; CSTYLE/CSRSW := 0
        JR      chput_esc_xy

chput_escape_2:                                   ; ----------------------------
        DJNZ    chput_escape_3

; -- ESCCNT == 2: 'ESC y <n>'
        LD      c,1                               ; CSTYLE/CSRSW := 1

chput_esc_xy:
        CP      '4'
        JR      z,chput_esc_xy_4
        CP      '5'
        JR      z,chput_esc_xy_5
        JR      chput_escape_reset
chput_esc_xy_4:
        LD      a,c
        LD      (CSTYLE),a
        JR      chput_escape_reset
chput_esc_xy_5:
        LD      a,c
        LD      (CSRSW),a
        JR      chput_escape_reset

chput_escape_3:                                   ; ----------------------------
        DJNZ    chput_escape_4

; -- ESCCNT == 3: 'ESC Y <n> <m>'
        LD      b,$1F
        SUB     b
        LD      (CSRX),a
        JR      chput_escape_reset

chput_escape_4:                                   ; ----------------------------
        DJNZ    chput_escape_reset

; -- ESCCNT == 4: 'ESC Y <n>'
        LD      b,$1F
        SUB     b
        LD      (CSRY),a
        LD      a,3
        JR      chput_escape_set

; -- ESCCNT := 1
chput_esc_x:
        LD      a,1
        JR      chput_escape_set

; -- ESCCNT := 2
chput_esc_y:
        LD      a,2
        JR      chput_escape_set

; -- ESCCNT := 4
chput_esc_yy:
        LD      a,4
        JR      chput_escape_set

chput_escape_reset:
        XOR     a
chput_escape_set:
        LD      (ESCCNT),a
        RET

; -- Cursor right, no wrap
chput_esc_c:
        LD      a,(CSRX)
        LD      hl,LINLEN
        CP      (hl)
        RET     nc
        INC     a
        LD      (CSRX),a
        RET

; -- Cursor left, no wrap
chput_esc_d:
        LD      a,(CSRX)
        DEC     a
        RET     z
        LD      (CSRX),a
        RET

; -- clear line
chput_esc_l:
        CALL    chput_ctrl_cr

; -- Clear till end of line
chput_esc_k:
        LD      hl,LINTTB-1                       ; update LINTTB
        LD      a,(CSRY)
        LD      e,a
        LD      d,0
        ADD     hl,de
; a != 0, which is OK
        LD      (hl),a

        LD      a,(LINLEN)
        INC     a                                 ; because CSRX is 1-based
        LD      hl,CSRX
        SUB     (hl)
        LD      c,a
        LD      b,0
        LD      a,32
        CALL    curs2hl
        JP      filvrm

; -- Insert line
chput_esc_ll:
        CALL    chput_ctrl_cr                     ; move to start of line
        LD      hl,(CSRY)                         ; save current cursor pos
        PUSH    hl
        LD      b,l
        LD      a,(CRTCNT)
        LD      (CSRY),a
        SUB     b
        LD      b,a
        INC     b
        LD      a,(CSRY)
        JR      chput_esc_ll_loop_end

chput_esc_ll_loop:
        CALL    curs2hl
        EX      de,hl
        DEC     a
        LD      (CSRY),a
        CALL    curs2hl
        CALL    chput_copy_line
chput_esc_ll_loop_end:
        DJNZ    chput_esc_ll_loop

        POP     hl                                ; restore cursor position
        LD      (CSRY),hl
        LD      h,0
        LD      a,(CRTCNT)                        ; update LINTTB
        LD      d,a                               ; DE := (CRTCNT)
        LD      e,0
        SUB     l                                 ; BC := (CRTCNT) - (CSRY) - 1
        DEC     a
        LD      c,a
        LD      b,0
        LD      hl,LINTTB-1                       ; DE := LINTTB + (CRTCNT)
        ADD     hl,de
        EX      de,hl
        LD      h,d                               ; HL := DE - 1
        LD      l,e
        DEC     hl
        LDDR
        JP      chput_esc_k

; -- Delete line (and scroll rest up)
chput_esc_m:
        CALL    chput_ctrl_cr                     ; move to start of line
        LD      hl,(CSRY)
        PUSH    hl                                ; save cursor pos
        LD      b,l
        LD      a,(CRTCNT)
        SUB     b
        LD      b,a
        INC     b
        LD      a,(CSRY)
        JR      chput_esc_m_loop_end

chput_esc_m_loop:
        CALL    curs2hl                           ;   Copy 1 line:
        EX      de,hl                             ;     de = dest in VRAM
        INC     a                                 ;     next line
        LD      (CSRY),a
        CALL    curs2hl                           ;     hl = src in VRAM
        CALL    chput_copy_line
chput_esc_m_loop_end:
        DJNZ    chput_esc_m_loop                  ; endloop

        CALL    chput_esc_k                       ; clear till end of line
        POP     hl                                ; restore cursor position
        LD      (CSRY),hl

        LD      h,0                               ; update LINTTB
        LD      a,(CRTCNT)                        ; BC := (CRTCNT) - (CRSY) - 1
        SUB     l
        DEC     a
        LD      c,a
        LD      b,0
        LD      de,LINTTB-1                       ; DE := LINTTB + (CSRY)
        ADD     hl,de
        LD      d,h
        LD      e,l
        INC     hl                                ; HL := DE + 1
        LDIR
        RET

; -- Copy line: from HL to DE
chput_copy_line:
        PUSH    af
        PUSH    bc
        LD      b,0
        LD      a,(LINLEN)
        LD      c,a

        IF      MODEL_MSX != MODEL_MSX1
            CP      41
            JR      c,chput_copy_line_2
            LD      c,40
            CALL    chput_copy_line_copy
            LD      a,(LINLEN)
            SUB     40
            LD      c,a
chput_copy_line_2:
        ENDIF

        CALL    chput_copy_line_copy
        POP     bc
        POP     af
        RET

chput_copy_line_copy:
        PUSH    hl
        PUSH    de
        PUSH    bc
        LD      de,LINWRK
        CALL    ldirmv
        POP     bc
        POP     de
        PUSH    de
        PUSH    bc
        LD      hl,LINWRK
        CALL    ldirvm
        POP     bc
        POP     hl
        ADD     hl,bc
        EX      de,hl
        POP     hl
        ADD     hl,bc
        RET

; -- Erase
chput_erase:
        LD      a,(CSRX)
        CP      1
        RET     z
        LD      a,32
        CALL    chput_putchar
        JP      chput_ctrl_left

; -- disable cursor
chput_remove_cursor:
        LD      a,(CSRSW)                         ; Cursor visible?
        CP      1
        RET     nz
        LD      a,(SCRMOD)                        ; Are we in text mode?
        CP      2
        RET     nc

        LD      a,(CURSAV)                        ; get saved character
        CALL    curs2hl                           ; and drop it at the
        JP      wrtvrm

; -- enable cursor
chput_restore_cursor:
        LD      a,(CSRSW)                         ; Cursor visible?
        CP      1
        RET     nz
        LD      a,(SCRMOD)
        CP      2
        RET     nc

        CALL    curs2hl                           ; get character at cursor
        CALL    rdvrm                             ; and store at CURSAV
        LD      (CURSAV),a

        AND     a                                 ; reset carry
        LD      d,0                               ; de := 8 * a
        LD      e,a
        RL      e
        RL      d
        RL      e
        RL      d
        RL      e
        RL      d
        XOR     a                                 ; get pattern table address
        LD      hl,SCRMOD
        CP      (hl)
        JR      nz,chput_restore_cursor_t32

        LD      hl,(TXTCGP)
        JR      chput_restore_cursor_getpattern

chput_restore_cursor_t32:
        LD      hl,(T32CGP)

chput_restore_cursor_getpattern:
        PUSH    hl
        ADD     hl,de                             ; add offset of character
        LD      de,LINWRK                         ; copy pattern to LINWRK
        LD      bc,8
        CALL    ldirmv

        LD      a,(CSTYLE)                        ; depending on CSTYLE
        CP      0
        JR      nz,chput_restore_cursor_ins
        LD      hl,LINWRK                         ; invert the complete pattern
        LD      b,8
        JR      chput_restore_cursor_invert
chput_restore_cursor_ins:
        LD      hl,LINWRK+6                       ; or only the lower 2 lines
        LD      b,2

chput_restore_cursor_invert:
        LD      a,(hl)                            ; invert!
        CPL
        LD      (hl),a
        INC     hl
        DJNZ    chput_restore_cursor_invert
        POP     hl                                ; copy inverted pattern to
        LD      de,255*8                          ; pattern 255
        ADD     hl,de
        EX      de,hl
        LD      hl,LINWRK
        LD      bc,8
        CALL    ldirvm

        CALL    curs2hl                           ; place char 255 at cursor pos
        LD      a,255
        JP      wrtvrm

; -- Control character search table
chput_ctrl_table:
        DB      7
        DW      beep                              ; chput_ctrl_beep
        DB      8
        DW      chput_ctrl_bs
        DB      9
        DW      chput_ctrl_tab
        DB      10
        DW      chput_ctrl_lf
        DB      11
        DW      chput_ctrl_home
        DB      12
        DW      chput_ctrl_ff
        DB      13
        DW      chput_ctrl_cr
        DB      27
        DW      chput_ctrl_esc
        DB      28
        DW      chput_ctrl_right
        DB      29
        DW      chput_ctrl_left
        DB      30
        DW      chput_ctrl_up
        DB      31
        DW      chput_ctrl_down

; -- Escape character search table
chput_esc_table:
        DB      'j'
        DW      chput_esc_j
        DB      'E'
        DW      chput_esc_e
        DB      'K'
        DW      chput_esc_k
        DB      'J'
        DW      chput_esc_jj
        DB      'l'
        DW      chput_esc_l
        DB      'L'
        DW      chput_esc_ll
        DB      'M'
        DW      chput_esc_m
        DB      'Y'
        DW      chput_esc_yy
        DB      'A'
        DW      chput_esc_a
        DB      'B'
        DW      chput_esc_b
        DB      'C'
        DW      chput_esc_c
        DB      'D'
        DW      chput_esc_d
        DB      'H'
        DW      chput_esc_h
        DB      'x'
        DW      chput_esc_x
        DB      'y'
        DW      chput_esc_y

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:

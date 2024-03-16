; $Id: video.asm 563 2009-12-20 20:26:40Z bifimsx $
; C-BIOS video routines
;
; Copyright (c) 2002-2005 BouKiCHi.  All rights reserved.
; Copyright (c) 2003 Reikan.  All rights reserved.
; Copyright (c) 2004-2006 Maarten ter Huurne.  All rights reserved.
; Copyright (c) 2004-2005 Albert Beevendorp.  All rights reserved.
; Copyright (c) 2004 Manuel Bilderbeek.  All rights reserved.
; Copyright (c) 2004 Joost Yervante Damad.  All rights reserved.
; Copyright (c) 2004-2005 Jussi Pitkänen.  All rights reserved.
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
; $0041 DISSCR
; Function : inhibits the screen display
; Registers: AF, BC
disscr:
        LD      a,(RG1SAV)
        AND     $BF
        LD      b,a
        LD      c,1
        CALL    wrtvdp
        RET

;--------------------------------
; $0044 ENASCR
; Function : displays the screen
; Registers: AF, BC
enascr:
        LD      a,(RG1SAV)
        OR      $40
        LD      b,a
        LD      c,1
        CALL    wrtvdp
        RET

;--------------------------------
; 0047$ WRTVDP
; Function : write data in the VDP-register
; Input    : B  - data to write
;            C  - number of the register
; Output   : RG0SAV(F3DF)-RG7SAV(F3E6)
; Registers: AF, BC
wrtvdp:
        DI
        RES     7,c                               ; fixes High Way Star
        LD      a,b
        OUT     (VDP_ADDR),a
        LD      a,c
        OR      $80
        OUT     (VDP_ADDR),a
        EI

        PUSH    hl
        LD      hl,RG0SAV
wrtvdp_sav:
        LD      a,b
        LD      b,0
        ADD     hl,bc
        LD      (hl),a
wrtvdp_nosav:
        POP     hl
        RET

;--------------------------------
; $004A RDVRM
; Function : Reads the content of VRAM
; Input    : HL - address read
; Output   : A  - value which was read
; Registers: AF
rdvrm:
        CALL    setrd
        IN      a,(VDP_DATA)
        RET


;--------------------------------
; $004D WRTVRM
; Function : Writes data in VRAM
; Input    : HL - address write
;            A  - value write
; Registers: AF
wrtvrm:
        PUSH    af
        CALL    setwrt
        POP     af
        OUT     (VDP_DATA),a
        RET

;--------------------------------
; $0050 SETRD
; Function : Enable VDP to read
; Input    : HL - for VRAM-address
; Registers: AF
setrd:
        DI

        LD      a,l
        OUT     (VDP_ADDR),a
        LD      a,h
        AND     $3F
        OUT     (VDP_ADDR),a
        EI
        RET

;--------------------------------
; $0053 SETWRT
; Function : Enable VDP to write
; Input    : HL - Address
; Registers: AF
setwrt:
        DI

        LD      a,l
        OUT     (VDP_ADDR),a
        LD      a,h
        AND     $3F
        OR      $40
        OUT     (VDP_ADDR),a
        EI
        RET

;--------------------------------
; $0056 FILVRM
; Function : fill VRAM with value
; Input    : A  - data byte
;            BC - length of the area to be written
;            HL - start address:
;                 * SCREEN 0..4 -> 14-bit address
;                 * SCREEN 5+ -> 17-bit address (uses ACPAGE)
;                 Using 14-bit address for SCREEN4 doesn't really make sense,
;                 but that's what we have to follow to be compatible.
; Registers: AF, BC
filvrm:
        PUSH    af
        CALL    setwrt
        DEC     bc
        INC     c
        LD      a,b
        LD      b,c
        LD      c,a
        INC     c
        POP     af
; Note: Interrupts should be enabled here.
;       Penguin Adventure can hang on boot if the interrupt
;       comes just after our RET, which is certain if the
;       memory block written is large enough.
filvrm_lp:
        OUT     (VDP_DATA),a
        NOP                                       ;
        NOP                                       ;
        DJNZ    filvrm_lp
        DEC     c
        JR      nz,filvrm_lp
        RET

;--------------------------------
; $0059 LDIRMV
; Function : Block transfer from VRAM to memory
; Input    : BC - blocklength
;            DE - Start address of memory
;            HL - Start address of VRAM
; Registers: AF BC DE
; Note     : the function doesn't destroy HL
; Note     : the routine doesn't change IM
ldirmv:
        CALL    setrd
        PUSH    hl

ldirmv_lp:
        IN      A,(VDP_DATA)
        NOP
        NOP
        LD      (DE),A
        INC     DE
        DEC     BC
        LD      A,B
        CP      0
        JR      nz,ldirmv_lp
        LD      A,C
        CP      0
        JR      nz,ldirmv_lp
        POP     hl
        RET

;--------------------------------
; $005C LDIRVM
; Function : Block transfer from memory to VRAM
; Input    : BC - blocklength
;            DE - Start address of VRAM
;            HL - Start address of memory
; Note     : the routine doesn't change IM
; Registers: All
ldirvm:
        EX      de,hl
        CALL    setwrt
        EX      de,hl
ldirvm_lp:
        LD      A,(HL)
        OUT     (VDP_DATA),A
        NOP
        NOP
        INC     HL
        DEC     BC
        LD      A,B
        CP      0
        JR      nz,ldirvm_lp
        LD      A,C
        CP      0
        JR      nz,ldirvm_lp
; Note: Without this, Quinpl shows glitches.
; TODO: Investigate why.
        EX      de,hl
        RET

;----------------------------------
; $005F CHGMOD  Changes screen mode
; Function : Switches to given screenmode
; Input    : A  - screen mode
; Registers: All
chgmod:
        CP      4
        RET     nc
; Redirect to initialisation routine.
        LD      hl,chgmod_tbl
        JP      jump_table
chgmod_tbl:
        DW      initxt                            ; SCREEN0
        DW      init32                            ; SCREEN1
        DW      inigrp                            ; SCREEN2
        DW      inimlt                            ; SCREEN3
; TODO: Now that we rewrite most regs at the end of CHGMOD,
;       the ini* routines can just update RG?SAV instead of calling wrtvdp.
chgmod_finish:
; Generic state resets.
; Write new values from system RAM to the VDP.
        DI
; Write R#0 - R#7.
        LD      hl,RG0SAV
        LD      bc,8 * $100 + VDP_ADDR
        LD      d,$80
chgmod_finish_lp:
        OUTI
        LD      a,b
        OUT     (c),d
        INC     d
        OR      a
        JR      nz,chgmod_finish_lp

        EI
        JP      enascr

;--------------------------------
; $0062 CHGCLR
; Function : Changes the screencolors
; Input    : Foregroundcolor in FORCLR
;            Backgroundcolor in BAKCLR
;            Bordercolor in BDRCLR
; Registers: All
chgclr:
        LD      a,(SCRMOD)
        CP      8
        JR      z,chgclr_sc8
        DEC     a
        PUSH    af
        LD      a,(FORCLR)
        RLCA
        RLCA
        RLCA
        RLCA
        AND     $F0
        LD      l,a
        LD      a,(BDRCLR)
        OR      l

        LD      b,a
        LD      c,7
        CALL    wrtvdp
        POP     af
        RET     nz

; SCREEN1
        LD      a,(FORCLR)
        RLCA
        RLCA
        RLCA
        RLCA
        AND     $F0
        LD      hl,BAKCLR
        OR      (hl)
        LD      hl,(T32COL)
        LD      bc,$0020
        PUSH    af
        CALL    setwrt
cclr_lp:
        POP     af
        OUT     (VDP_DATA),a
        PUSH    af
        DEC     bc
        LD      a,b
        OR      c
        JR      nz,cclr_lp
        POP     af
        RET

chgclr_sc8:
; SCREEN8
        LD      a,(BDRCLR)
        LD      b,a
        LD      c,7
        JP      wrtvdp

;--------------------------------
; $0069 CLRSPR
; Function : Initialises all sprites
; Input    : SCRMOD
; Registers: All
clrspr:
; Check screen mode.
        LD      a,(SCRMOD)
        OR      a
        RET     z                                 ; no sprites in SCREEN0

; Clear sprite attribute table.
        CALL    clrspr_attr

; Clear sprite colour table.
        LD      a,(SCRMOD)
        CP      4                                 ; sprite mode 1?
        JR      c,clrspr_col_skip
        LD      hl,(ATRBAS)
        DEC     h
        DEC     h                                 ; HL = (ATRBAS) - $200
        LD      bc,32 * 16
        LD      a,(FORCLR)
        AND     $0F
        CALL    bigfil
clrspr_col_skip:

; Clear sprite pattern generator table.
        LD      hl,(PATBAS)
        LD      bc,256 * 8
        XOR     a
        CALL    filvrm
        RET

;--------------------------------
; Clear sprite attribute table.
clrspr_attr:
        LD      a,(SCRMOD)
        CP      4
        JR      c,clrspr_attr_spritemode1

; Note: This label is called directly by external routines.
clrspr_attr_spritemode2:
        LD      e,217                             ; Y coordinate
        JR      clrspr_attr_spritemode_start

; Note: This label is called directly by external routines.
clrspr_attr_spritemode1:
        LD      e,209                             ; Y coordinate

clrspr_attr_spritemode_start:
        LD      hl,(ATRBAS)
        CALL    setwrt
        LD      a,(FORCLR)
        LD      d,a
        LD      bc,$2000                          ; B = 32 = counter, C = pattern index
        DI
clrspr_attr_lp:
        LD      a,e
        OUT     (VDP_DATA),a                      ; Y coordinate
        LD      a,0
        OUT     (VDP_DATA),a                      ; X coordinate
        LD      a,c
        OUT     (VDP_DATA),a                      ; pattern number
        INC     c
        CALL    gspsiz
        JR      nc,clrspr_attr_8
        INC     c
        INC     c
        INC     c
clrspr_attr_8:
        LD      a,d
        OUT     (VDP_DATA),a                      ; color
        DJNZ    clrspr_attr_lp
        EI
        RET

;--------------------------------
; $006C INITXT
; Function : Switch to SCREEN 0
; Input    : TXTNAM, TXTCGP
; Output   : NAMBAS, CGPBAS, LINLEN, SCRMOD, OLDSCR
; Registers: All
initxt:
; Disable video output.
        CALL    disscr

; New screen mode.
        LD      a,$00
        LD      (SCRMOD),a
        LD      (OLDSCR),a


; Line length.
        LD      a,(LINL40)
        LD      (LINLEN),a

; Cursor position: top-left.
        LD      a,1
        LD      (CSRY),a
        LD      (CSRX),a

; Table base addresses.
        LD      hl,(TXTNAM)                       ; name table
        LD      (NAMBAS),hl
        LD      hl,(TXTCGP)                       ; pattern table
        LD      (CGPBAS),hl
        LD      hl,(TXTATR)                       ; sprite attribute table (unused)
        LD      (ATRBAS),hl
        LD      hl,(TXTPAT)                       ; sprite pattern table (unused)
        LD      (PATBAS),hl

; Update VDP regs and VRAM.
        CALL    chgclr
        CALL    settxt
        IF      COMPILE_FONT != NO
            CALL    init_font
        ENDIF
        CALL    cls_screen0
        JP      chgmod_finish

;--------------------------------
; $006F INIT32
; Function : Switches to SCREEN 1 (text screen with 32*24 characters)
; Input    : T32NAM, T32CGP, T32COL, T32ATR, T32PAT
; Output   : NAMBAS, CGPBAS, LINLEN, SCRMOD, OLDSCR
; Registers: All
init32:
; Disable video output.
        CALL    disscr

        LD      a,$01                             ; SCREEN1
        LD      (SCRMOD),a
        LD      (OLDSCR),a

        LD      a,1
        LD      (CSRY),a
        LD      (CSRX),a

        CALL    chgclr

        LD      hl,(T32NAM)
        LD      (NAMBAS),hl
        LD      hl,(T32CGP)
        LD      (CGPBAS),hl
        LD      hl,(T32PAT)
        LD      (PATBAS),hl
        LD      hl,(T32ATR)
        LD      (ATRBAS),hl

        IF      COMPILE_FONT != NO
            CALL    init_font
        ENDIF

        LD      a,(LINL32)
        LD      (LINLEN),a

        CALL    sett32
        CALL    clrspr_attr_spritemode1
        CALL    cls_screen1
        JP      chgmod_finish

;--------------------------------
; $0072 INIGRP
; Function : Switches to SCREEN 2 (high resolution screen with 256*192 pixels)
; Input    : GRPNAM, GRPCGP, GRPCOL, GRPATR, GRPPAT
; Output   : NAMBAS-ATRBAS, SCRMOD
; Registers: All
inigrp:
; Disable video output.
        CALL    disscr

        LD      a,$02
        LD      (SCRMOD),a

        CALL    chgclr

        LD      hl,(GRPNAM)
        LD      (NAMBAS),hl
        CALL    setwrt
        LD      b,3
        XOR     a
        DI
inigrp_lp:
        OUT     (VDP_DATA),a
        INC     a
        JR      nz,inigrp_lp
        DJNZ    inigrp_lp
        EI

        LD      hl,(GRPCGP)
        LD      (CGPBAS),hl

        LD      hl,(GRPATR)
        LD      (ATRBAS),hl

        LD      hl,(GRPPAT)
        LD      (PATBAS),hl

        CALL    setgrp
        CALL    clrspr_attr_spritemode1
        CALL    cls_screen2
        JP      chgmod_finish

;------------------------------
; $0075 INIMLT
; Function : Switches to SCREEN 3 (multi-color screen 64*48 pixels)
; Input    : MLTNAM, MLTCGP, MLTCOL, MLTATR, MLTPAT
; Output   : NAMBAS-ATRBAS, SCRMOD
; Registers: All
inimlt:
; Disable video output.
        CALL    disscr

        LD      a,$03
        LD      (SCRMOD),a

        CALL    chgclr

        LD      hl,(MLTNAM)
        LD      (NAMBAS),hl
        CALL    setwrt
        XOR     a
        LD      c,6
        DI
inimlt_loop1:
        PUSH    af
        LD      e,4
inimlt_loop2:
        PUSH    af
        LD      b,32
inimlt_loop3:
        OUT     (VDP_DATA),a
        INC     a
        DJNZ    inimlt_loop3
        POP     af
        DEC     e
        JR      nz,inimlt_loop2
        POP     af
        ADD     a,32
        DEC     c
        JR      nz,inimlt_loop1
        EI

        LD      hl,(MLTCGP)
        LD      (CGPBAS),hl
        LD      hl,(MLTATR)
        LD      (ATRBAS),hl
        LD      hl,(MLTPAT)
        LD      (PATBAS),hl

        CALL    setmlt
        CALL    clrspr_attr_spritemode1
        CALL    cls_screen3
        JP      chgmod_finish


;------------------------------
; $0078 SETTXT
; Function : Switches to VDP in SCREEN 0 mode
; Input    : TXTNAM, TXTCGP
; Registers: All
settxt:

        LD      a,(RG0SAV)
        AND     $F1                               ; MASK 11110001
        LD      b,a
        LD      c,0
        CALL    wrtvdp                            ; write VDP R#0

        LD      a,(RG1SAV)
        AND     $E7                               ; MASK 11100111
        OR      $10
        LD      b,a
        INC     c
        CALL    wrtvdp                            ; write VDP R#1

; Set the VDP base address registers. This works because
; TXTNAM, TXTCOL and TXTCGP are in same order as the VDP
; base address registers.
        LD      de,TXTNAM
        LD      c,2

        XOR     a
        CALL    set_base_address
        INC     de                                ; Skip TXTCOL.
        INC     de
        INC     c
        XOR     a
        CALL    set_base_address

        RET


;------------------------------
; $007B SETT32
; Function : Switches VDP to SCREEN 1 mode
; Input    : T32NAM, T32COL, T32CGP, T32ATR, T32PAT
; Registers: All
sett32:
        LD      a,(RG0SAV)
        AND     $F1                               ; MASK 11110001
        LD      b,a
        LD      c,0
        CALL    wrtvdp                            ; write VDP R#0

        LD      a,(RG1SAV)
        AND     $E7                               ; MASK 11100111
        LD      b,a
        INC     c
        CALL    wrtvdp                            ; write VDP R#1

; Set the base address registers. This works because T32NAM,
; T32COL, T32CGP, T32ATR and T32PAT are in same order as the
; VDP base address registers.
        LD      de,T32NAM
        LD      c,2

        XOR     a
        CALL    set_base_address
        XOR     a
        CALL    set_base_address
        XOR     a
        CALL    set_base_address
        XOR     a
        CALL    set_base_address
        XOR     a
        CALL    set_base_address

        RET

;------------------------------
; $007E SETGRP
; Function : Switches VDP to SCREEN 2 mode
; Input:     GRPNAM, GRPCOL, GRPCGP, GRPATR, GRPPAT
; Registers: All
setgrp:
        LD      a,(RG0SAV)
        AND     $F1                               ; MASK 11110001
        OR      $02                               ; M3 = 1
        LD      b,a
        LD      c,0
        CALL    wrtvdp                            ; write VDP R#0

        LD      a,(RG1SAV)
        AND     $E7                               ; MASK 11100111
        LD      b,a
        INC     c
        CALL    wrtvdp                            ; write VDP R#1

; Set the base address registers. This works because GRPNAM,
; GRPCOL, GRPCGP, GRPATR and GRPPAT are in same order as the
; VDP base address registers.
        LD      de,GRPNAM
        LD      c,2

        XOR     a
        CALL    set_base_address
        LD      a,$7F
        CALL    set_base_address
        LD      a,$03
        CALL    set_base_address
        XOR     a
        CALL    set_base_address
        XOR     a
        CALL    set_base_address

        RET

;------------------------------
; $0081 SETMLT
; Function : Switches VDP to SCREEN 3 mode
; Input    : MLTNAM, MLTCGP, MLTCOL, MLTATR, MLTPAT
; Registers: All
setmlt:
        LD      a,(RG0SAV)
        AND     $F1
        LD      b,a
        LD      c,0
        CALL    wrtvdp

        LD      a,(RG1SAV)
        AND     $E7
        OR      $08                               ; M2 = 1
        LD      b,a
        INC     c
        CALL    wrtvdp

; Set the base address registers. This works because MLTNAM,
; MLTCOL, MLTCGP, MLTATR and MLTPAT are in same order as the
; VDP base address registers.
        LD      de,MLTNAM
        LD      c,2

        XOR     a
        CALL    set_base_address
        XOR     a
        CALL    set_base_address                  ; TODO: Should we ignore MLTCOL?
        XOR     a
        CALL    set_base_address
        XOR     a
        CALL    set_base_address
        XOR     a
        CALL    set_base_address

        RET

;------------------------------
; Get an address from a base address table, convert it into a register value,
; and set the corresponding VDP base address register.
; Input:     DE = pointer to a base address table
;             C = VDP base address register
;             A = OR-mask over the converted address
; Output:    DE = DE + 2
;             C =  C + 1
; Changes:   AF, B, HL
set_base_address:
        PUSH    de
        PUSH    af

; Get the shift value.
        LD      hl,set_base_address_table
        LD      b,0
        ADD     hl,bc
        LD      b,(hl)

; Get the address from (HL) to HL.
        EX      de,hl
        LD      a,(hl)
        INC     hl
        LD      h,(hl)
        LD      l,a

; Shift it to left in register A. After this A contains the
; converted address.
set_base_address_loop:
        ADD     hl,hl
        ADC     a,a
        DJNZ    set_base_address_loop
        LD      b,a

; Set the base address register.
        POP     af
        OR      b
        LD      b,a
        CALL    wrtvdp

; Increase pointer and register number.
        POP     de
        INC     de
        INC     de
        INC     c

        RET

set_base_address_table:
        DB      $00,$00,$06,$0A,$05,$09,$05

;------------------------------
; $0084 CALPAT
; Returns the address of a sprite pattern in the sprite pattern table.
; Input:     A  = pattern number
; Output:    HL = address
; Changes:   AF, DE, HL
calpat:
        LD      h,0
        LD      l,a
        ADD     hl,hl
        ADD     hl,hl
        ADD     hl,hl
        CALL    gspsiz
        JR      nc,calpat_8
        ADD     hl,hl
        ADD     hl,hl
calpat_8:
        LD      de,(PATBAS)
        ADD     hl,de
        RET

;------------------------------
; $0087 CALATR
; Returns the address of a sprite in the sprite attribute table.
; Input:     A  = sprite number
; Output:    HL = address
; Changes:   AF, DE, HL
calatr:
        ADD     a,a
        ADD     a,a
        LD      hl,(ATRBAS)
        LD      d,0
        LD      e,a
        ADD     hl,de
        RET

;------------------------------
; $008A GSPSIZ
; Returns the current sprite-size in bytes.
; Output:    A  = sprite-size in bytes
;            CF = set when size is 16x16, otherwise reset
; Changes:   AF
gspsiz:
        LD      a,(RG1SAV)
        RRCA
        RRCA
        LD      a,8
        RET     nc
        LD      a,32
        RET

;------------------------------
; $008D GRPPRT
; Function:  Places a character on graphic screen
; Input:     A  - Character
;            GRPACX , GRPACY : X, Y coordinate
;            FORCLR
; Input (SCREEN 5 and above) :
;            LOGOPR for logical operator
; NOTE : the function doesn't support without SCREEN 2
;        and also slower yet.
; Register : AF ???
grpprt:
        PUSH    af

; Printable character or control character?
        CP      $20
        JR      c,grpprt_control

; Different implementation depending on screen mode.
        LD      a,(SCRMOD)
        CP      2
        JR      z,grpprt_sc2
        CP      5
        JR      nc,grpprt_sc5                     ; SCRMOD >= 5
grpprt_end:
        POP     af
        RET

grpprt_control:
; Ignore everything except carriage return ($0D).
        CP      $0D
        JR      nz,grpprt_end

        POP     af
; Handle carriage return.
        PUSH    hl
        PUSH    bc
        LD      hl,(GRPACY)
        LD      bc,$0008
        ADD     hl,bc
        LD      (GRPACY),hl
        LD      hl,$0000
        LD      (GRPACX),hl
        POP     bc
        POP     hl
        RET

grpprt_sc5:
        POP     af

; TODO: should these routines be merged?

        RET

grpprt_sc2:
        POP     af
        PUSH    hl
        PUSH    de
        PUSH    bc
        PUSH    af

        CALL    getpat

        LD      de,(GRPACY)
        LD      bc,(GRPACX)
        CALL    mapxy

        LD      a,(FORCLR)
        LD      (ATRBYT),a

        LD      hl,(CLOC)
        LD      bc,(GRPCGP)
        ADD     hl,bc


        LD      de,PATWRK
        LD      a,(GRPACY)
        AND     $07

        LD      b,$00
        LD      c,a
        ADD     hl,bc
        CALL    grpprt_chr_x

        LD      bc,$00F0
        ADD     hl,bc
        LD      a,(GRPACY)
        CPL
        AND     $07
        LD      c,a

        CALL    grpprt_chr_x

        LD      hl,(GRPACX)
        LD      bc,$0008
        ADD     hl,bc
        LD      (GRPACX),hl

        POP     af
        POP     bc
        POP     de
        POP     hl

        RET

grpprt_chr_x:
        LD      a,(GRPACX)
        AND     $07
        PUSH    af
        PUSH    bc
        PUSH    de
        PUSH    hl
        CALL    grpprt_chr                        ; half left
        LD      a,(GRPACX)
        AND     $07
        JR      z,grpprt_skip_hr
        LD      a,(CMASK)
        CPL
        LD      (CMASK),a
        POP     hl
        LD      bc,$0008
        ADD     hl,bc
        POP     de
        POP     bc
        POP     af

        CALL    grpprt_chr                        ; half right
        LD      a,(CMASK)
        CPL
        LD      (CMASK),a
        RET
grpprt_skip_hr:
        POP     bc                                ; HL = the result of last grpprt_chr
        LD      bc,$0008
        ADD     hl,bc
        POP     bc                                ; DE = the result of last grpprt_chr
        POP     bc
        POP     af
        RET



; A = Pattern , B = Pattern in VRAM
grpprt_attr:
        PUSH    af
        PUSH    hl
        PUSH    bc
        PUSH    de

        LD      d,a                               ; D = Pattern of charactor
        LD      e,b                               ; E = Pattern in VRAM

        LD      bc,(GRPCOL)
        ADD     hl,bc
        LD      c,a
        LD      a,(ATRBYT)
        AND     $0f
        LD      b,a
        CALL    rdvrm

        PUSH    af                                ; A = an attribute in VRAM
        AND     $0f
        CP      b
        JR      z,grpprt_attr_black
        POP     af
        PUSH    af
        RRCA
        RRCA
        RRCA
        RRCA
        AND     $0f
        CP      b
        JR      nz,grpprt_attr_nomatch
        POP     af
grpprt_attr_end:
        POP     de
        POP     bc
        POP     hl
        POP     af
        RET
grpprt_attr_black:
        POP     af
grpprt_attr_blk_end:
        POP     de
        POP     bc
        POP     hl
        POP     af
        CPL
        RET
grpprt_attr_nomatch:
        LD      a,d
        OR      e
        CP      $ff
        JR      z,grpprt_attr_make_black
        POP     af
        RLC     b
        RLC     b
        RLC     b
        RLC     b
        AND     $0f
        OR      b
        CALL    wrtvrm
        JR      grpprt_attr_end
grpprt_attr_make_black:
        POP     af
        AND     $f0
        OR      b
        CALL    wrtvrm
        JR      grpprt_attr_blk_end


; A = X MOD 8,C = Y MOD 8, HL = CLOC
grpprt_chr:
        LD      b,c
        INC     a
        LD      c,a

        LD      a,$07
        XOR     b
        INC     a
        LD      b,a

grpprt_lp:
        PUSH    bc
        CALL    rdvrm
        LD      b,a
        LD      a,(de)
        CALL    grpprt_attr
grpprt_sft_lp:
        DEC     c
        JR      z,grpprt_sft_ed
        RRCA
        JR      grpprt_sft_lp
grpprt_sft_ed:
        LD      c,a
        LD      a,(CMASK)
        AND     c
        LD      c,a                               ; charactor with mask

        LD      a,b                               ; B = pattern in VRAM
        OR      c

        CALL    wrtvrm
        INC     hl
        INC     de
        POP     bc
        DJNZ    grpprt_lp
        RET


grpprt_text:
        DB      "GRPPRT",0

;--------------------------------
; 0165h CHKNEW
; Is the current screen mode a bitmap mode?
; Output:  Carry flag set if current screen mode is SCREEN 5 or higher.
; Changes: AF
chknew:
        LD      a,(SCRMOD)
        CP      5
        RET

;--------------------------------
; 016Bh BIGFIL
; Fills VRAM with a fixed value.
; Like FILVRM, but supports 128K of VRAM.
; Input:   HL = VRAM start address
;    (ACPAGE) = active VRAM page
;          BC = number of bytes to fill
;          A  = value to fill VRAM with
; Changes: AF, BC
bigfil:
        PUSH    af
        CALL    nsetwr
        DEC     bc
        INC     c
        LD      a,b
        LD      b,c
        LD      c,a
        INC     c
        POP     af
        DI
bigfil_lp:
        OUT     (VDP_DATA),a
        DJNZ    bigfil_lp
        DEC     c
        JR      nz,bigfil_lp
        EI
        RET

;--------------------------------
; 016Eh NSETRD
; Set VRAM address and read mode.
; Like SETRD, but supports 128K of VRAM.
; Input:   HL = VRAM address
;    (ACPAGE) = active VRAM page
; Changes: AF
; Note: If an odd-numbered 32K page is active and HL >= $8000,
;       16-bit wrap around occurs.
nsetrd:
        CALL    nset_addr
        LD      a,h
        AND     $3F
        OUT     (VDP_ADDR),a                      ; A13..A8
        EI
        RET

;--------------------------------
; 0171h NSETWR
; Set VRAM address and write mode.
; Like SETWRT, but supports 128K of VRAM.
; Input:   HL = VRAM address
;    (ACPAGE) = active VRAM page
; Changes: AF
; Note: If an odd-numbered 32K page is active and HL >= $8000,
;       16-bit wrap around occurs.
nsetwr:
        CALL    nset_addr
        LD      a,h
        AND     $3F
        OR      $40
        OUT     (VDP_ADDR),a                      ; A13..A8
        EI
        RET

nset_addr:
        LD      a,(ACPAGE)
        OR      a
        JR      z,nset_32k

        LD      a,(SCRMOD)
        CP      5
        JP      c,setwrt
        CP      7
        LD      a,(ACPAGE)
        JR      c,nset_32k                        ; SCREEN5/6 -> 32K pages
        ADD     a,a                               ; SCREEN7/8 -> 64K pages
nset_32k:
        PUSH    hl
        AND     $03                               ; A  =  0   0   0   0   0   0   P1  P0
        RRCA
        LD      l,a                               ; L  =  P0  0   0   0   0   0   0   P1
        AND     $80                               ; A  =  P0  0   0   0   0   0   0   0
        XOR     h                                 ; A  = A15 A14 A13 A12 A11 A10  A9  A8
        RLA                                       ; CF = A15
        RL      l                                 ; L  =  0   0   0   0   0   0   P1 A15
        RLA                                       ; CF = A14
        LD      a,l
        RLA                                       ; A  =  0   0   0   0   0   P1 A15 A14
        DI
        OUT     (VDP_ADDR),a                      ; A16..A14
        LD      a,$8E
        OUT     (VDP_ADDR),a                      ; R#14
        POP     hl
        LD      a,l
        OUT     (VDP_ADDR),a                      ; A7..A0
        RET



;--------------------------------
; 0174h NRDVRM
; Read a byte from VRAM.
; Leaves the VRAM in read mode at the byte after the one read.
; Like RDVRM, but supports 128K of VRAM.
; Input:   HL = VRAM address
;    (ACPAGE) = active VRAM page
; Output:   A = the byte read
nrdvrm:
        CALL    nsetrd
        IN      a,(VDP_DATA)
        RET

;--------------------------------
; 0177h NWRVRM
; Write a byte to VRAM.
; Leaves the VRAM in write mode at the byte after the one written.
; Like WRTVRM, but supports 128K of VRAM.
; Input:   HL = VRAM address
;    (ACPAGE) = active VRAM page
;           A = the byte to write
nwrvrm:
        PUSH    af
        CALL    nsetwr
        POP     af
        OUT     (VDP_DATA),a
        RET


; VDP routines which only exist in sub rom, but are useful for C-BIOS internal
; use as well:

;-------------------------------------
; $0131(sub) VDPSTA
; Read VDP status register.
; Input:   A = number of status register
; Output:  A = value read
; Changes: F
vdpsta:
        DI
; Select desired status register.
        OUT     (VDP_ADDR),a
        LD      a,$80 + 15
        OUT     (VDP_ADDR),a
; Read status register.
        IN      a,(VDP_STAT)
        PUSH    af
; Restore status register 0.
        XOR     a
        OUT     (VDP_ADDR),a
        LD      a,$80 + 15
        OUT     (VDP_ADDR),a
        EI
        POP     af
        RET


;--------------------
;Initializes VDP routine
;--------------------

init_vdp:
        IN      a,(VDP_STAT)                      ; reset latch

        LD      bc,$0000                          ; R#0
        CALL    wrtvdp
        LD      bc,$E001                          ; R#1
        CALL    wrtvdp
        LD      bc,$0002                          ; R#2
        CALL    wrtvdp
        LD      bc,$8003                          ; R#3
        CALL    wrtvdp
        LD      bc,$0104                          ; R#4
        CALL    wrtvdp

        LD      a,1
        LD      (CSRY),a
        LD      (CSRX),a
        CALL    cls_screen1

        LD      a ,$00
        LD      hl,$0800
        LD      bc,$0800
        CALL    filvrm

; for screen 1 color table
        LD      a ,$F5
        LD      hl,$2000
        LD      bc,$0020
        CALL    filvrm


; PatGenTbl
;        76543210 76543210
;        00000100 00000000
;             04h      00h

        LD      bc,$F507                          ; R#7
        CALL    wrtvdp

        IF      COMPILE_FONT != NO
            LD      hl,B_Font
            LD      de,$0800
            LD      bc,$0800
            CALL    ldirvm
        ENDIF
        RET



        IF      COMPILE_FONT != NO
;------------------------------
; Initialise font.
; Uploads font to VRAM address specified by CGPBAS.
init_font:
            LD      hl,B_Font
            LD      de,(CGPBAS)
            LD      bc,$0800
            JP      ldirvm
        ENDIF


;--------------------------------
; $00C3 CLS
; Clears the screen.
; Input:   BAKCLR,
;          Z-Flag has to be low if the main ROM version of CLS is called;
;          in the sub ROM version of CVS the Z-Flag is ignored.
; Changes: AF, BC, DE
;TODO: add optional borders to text based screens
;      -> Should that happen in CLS?
cls_z:
        RET     nz
cls:
        LD      a,(SCRMOD)
        CP      4
        RET     nc                                ; Out of range?
        PUSH    hl
        LD      hl,cls_table
        CALL    jump_table
        POP     hl
        RET
cls_table:
        DW      cls_screen0
        DW      cls_screen1
        DW      cls_screen2
        DW      cls_screen3

cls_screen0:
        LD      a,(LINLEN)
        CP      40
        LD      bc,40*24
        JR      c,cls_text
        LD      bc,80*24
        JR      cls_text

cls_screen1:
        LD      bc,32*24

cls_text:
        LD      hl,(NAMBAS)
        LD      a,$20
        CALL    filvrm

        LD      a,1
        LD      hl,LINTTB
        LD      (hl),a
        LD      de,LINTTB+1
        LD      bc,23
        LDIR
        JP      chput_ctrl_home

cls_screen2:
        XOR     a
        LD      bc,$1800
        LD      hl,(CGPBAS)
        LD      l,a
        PUSH    bc
        CALL    filvrm
        POP     bc

        LD      a,(BAKCLR)
        LD      hl,(GRPCOL)
        JP      filvrm

cls_screen3:
        LD      a,(BAKCLR)
        AND     $0F
        LD      b,a
        RLCA
        RLCA
        RLCA
        RLCA
        OR      b
        LD      bc,$800
        LD      hl,(CGPBAS)
        JP      filvrm


; $0105 GETPAT
; Function : Returns current pattern of a character
; Input    : A  - ASCII code of character
; Output   : Pattern in PATWRK starting from address #FC40
; Registers: All
; Remark   : Same as routine in MSX1-BIOS, but there it doesn't exist as
;            a BIOS-call
getpat:
        LD      bc,(CGPNT+1)
        LD      l,a
        LD      h,0
        ADD     hl,hl
        ADD     hl,hl
        ADD     hl,hl
        ADD     hl,bc
        LD      b,8
        LD      de,PATWRK
getpat_loop:
        PUSH    bc
        PUSH    de
        PUSH    hl
        LD      a,(CGPNT)
        CALL    rdslt
        POP     hl
        POP     de
        POP     bc
        LD      (de),a
        INC     de
        INC     hl
        DJNZ    getpat_loop
        RET

;--------------------------------
; $00FC RIGHTC
; Function : Shifts screenpixel to the right
; Registers: AF
; NOTE     : This implementation is still a stub!
rightc:
        PUSH    hl
        PUSH    af
        LD      hl,rightc_text
;                call    print_debug
        POP     af
        POP     hl
        RET
rightc_text:
        DB      "RIGHTC",0

;--------------------------------
; $00FF LEFTC
; Function : Shifts screenpixel to the left
; Registers: AF
; NOTE     : This implementation is still a stub!
leftc:
        PUSH    hl
        PUSH    af
        LD      hl,leftc_text
;                call    print_debug
        POP     af
        POP     hl
        RET
leftc_text:
        DB      "LEFTC",0

;--------------------------------
; $0102 UPC
; Function : Shifts screenpixel up
; Registers: AF
; NOTE     : This implementation is still a stub!
upc:
        PUSH    hl
        PUSH    af
        LD      hl,upc_text
;                call    print_debug
        POP     af
        POP     hl
        RET
upc_text:
        DB      "UPC",0

;--------------------------------
; $0105 TUPC
; Function : Tests whether UPC is possible, if possible, execute UPC
; Output   : C-flag set if operation would end outside the screen
; Registers: AF
; NOTE     : This implementation is still a stub!
tupc:
        PUSH    hl
        PUSH    af
        LD      hl,tupc_text
;                call    print_debug
        POP     af
        POP     hl
        RET
tupc_text:
        DB      "TUPC",0

;--------------------------------
; $0108 DOWNC
; Function : Shifts screenpixel down
; Registers: AF
; NOTE     : This implementation is still a stub!
downc:
        PUSH    hl
        PUSH    af
        LD      hl,downc_text
;                call    print_debug
        POP     af
        POP     hl
        RET
downc_text:
        DB      "DOWNC",0

;--------------------------------
; $010B TDOWNC
; Function : Tests whether DOWNC is possible, if possible, execute DOWNC
; Output   : C-flag set if operation would end outside the screen
; Registers: AF
; NOTE     : This implementation is still a stub!
tdownc:
        PUSH    hl
        PUSH    af
        LD      hl,tdownc_text
;                call    print_debug
        POP     af
        POP     hl
        RET
tdownc_text:
        DB      "TDOWNC",0

;--------------------------------
; $010E SCALXY
; Function : Scales X and Y coordinates
; NOTE     : This implementation is still a stub!
scalxy:
        LD      bc,(GRPACX)
        LD      de,(GRPACY)
        RET
scalxy_text:
        DB      "SCALXY",0

;--------------------------------
; $0111 MAPXYC
; Function : Places cursor at current cursor address
; Input    : BC = X coordinate,DE=Y coordinate
; Register : AF,D,HL
; NOTE     : This is a test version
mapxy:
        PUSH    bc
        LD      (GRPACX),bc
        LD      (GRPACY),de
        LD      hl,(GRPACY)
        ADD     hl,hl
        ADD     hl,hl
        ADD     hl,hl
        ADD     hl,hl
        ADD     hl,hl
        LD      l,$00
        LD      b,$00

        LD      a,$ff
        LD      (CMASK),a
        LD      a,c
        AND     $07
        JR      z,mapxy_mask_ed
        LD      b,a
        LD      a,$ff
mapxy_mask_lp:
        AND     a
        RRA
        DJNZ    mapxy_mask_lp
        LD      (CMASK),a
mapxy_mask_ed:
        LD      a,c
        AND     $F8
        LD      c,a
        LD      b,$00
        ADD     hl,bc

        LD      (CLOC),hl
        POP     bc
        RET
mapxy_text:
        DB      "MAPXY",0

;--------------------------------
; $0114 FETCHC
; Function : Gets current cursor addresses mask pattern
; Output   : HL - Cursor address
;            A  - Mask pattern
fetchc:
        LD      a,(CMASK)
        LD      hl,(CLOC)
        RET
fetchc_text:
        DB      "FETCHC",0

;--------------------------------
; $0117 STOREC
; Function : Record current cursor addresses mask pattern
; Input    : HL - Cursor address
;            A  - Mask pattern
; NOTE     : This implementation is still a stub!
storec:
        PUSH    hl
        PUSH    af
        LD      hl,storec_text
;                call    print_debug
        POP     af
        POP     hl
        RET
storec_text:
        DB      "STOREC",0

;--------------------------------
; $011A SETATR
; Function : Set attribute byte
; NOTE     : This implementation is still a stub!
setatr:
        PUSH    hl
        PUSH    af
        LD      hl,setatr_text
;                call    print_debug
        POP     af
        POP     hl
        RET
setatr_text:
        DB      "SETATR",0

;--------------------------------
; $011D READC
; Function : Reads attribute byte of current screenpixel
; NOTE     : This implementation is still a stub!
readc:
        PUSH    hl
        PUSH    af
        LD      hl,readc_text
;                call    print_debug
        POP     af
        POP     hl
        RET
readc_text:
        DB      "READC",0

;--------------------------------
; $0120 SETC
; Function : Returns currenct screenpixel of specificed attribute byte
; NOTE     : This implementation is still a stub!
setc:
        PUSH    hl
        PUSH    af
        LD      hl,setc_text
;                call    print_debug
        POP     af
        POP     hl
        RET
setc_text:
        DB      "SETC",0

;--------------------------------
; $0123 NSETCX
; Function : Set horizontal screenpixels
; NOTE     : This implementation is still a stub!
nsetcx:
        PUSH    hl
        PUSH    af
        LD      hl,nsetcx_text
;                call    print_debug
        POP     af
        POP     hl
        RET
nsetcx_text:
        DB      "NSETCX",0

;--------------------------------
; $0126 GTASPC
; Function : Gets screen relations
; Output   : DE, HL
; Registers: DE, HL
; NOTE     : This implementation is still a stub!
gtaspc:
        PUSH    hl
        PUSH    af
        LD      hl,gtaspc_text
;                call    print_debug
        POP     af
        POP     hl
        RET
gtaspc_text:
        DB      "GTASPC",0

;--------------------------------
; $0129 PNTINI
; Function : Initalises the PAINT instruction
; NOTE     : This implementation is still a stub!
pntini:
        PUSH    hl
        PUSH    af
        LD      hl,pntini_text
;                call    print_debug
        POP     af
        POP     hl
        RET
pntini_text:
        DB      "PNTINI",0

;--------------------------------
; $012C SCANR
; Function : Scans screenpixels to the right
; NOTE     : This implementation is still a stub!
scanr:
        PUSH    hl
        PUSH    af
        LD      hl,scanr_text
;                call    print_debug
        POP     af
        POP     hl
        RET
scanr_text:
        DB      "SCANR",0

;--------------------------------
; $012F SCANL
; Function : Scans screenpixels to the left
; NOTE     : This implementation is still a stub!
scanl:
        PUSH    hl
        PUSH    af
        LD      hl,scanl_text
;                call    print_debug
        POP     af
        POP     hl
        RET
scanl_text:
        DB      "SCANL",0

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:

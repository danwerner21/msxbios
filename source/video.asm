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
                ld      a,(RG1SAV)
                and     $BF
                ld      b,a
                ld      c,1
                call    wrtvdp
                ret

;--------------------------------
; $0044 ENASCR
; Function : displays the screen
; Registers: AF, BC
enascr:
                ld      a,(RG1SAV)
                or      $40
                ld      b,a
                ld      c,1
                call    wrtvdp
                ret

;--------------------------------
; 0047$ WRTVDP
; Function : write data in the VDP-register
; Input    : B  - data to write
;            C  - number of the register
; Output   : RG0SAV(F3DF)-RG7SAV(F3E6)
; Registers: AF, BC
wrtvdp:
                di
                res     7,c             ; fixes High Way Star
                ld      a,b
                out     (VDP_ADDR),a
                ld      a,c
                or      $80
                out     (VDP_ADDR),a
                ei

                push    hl
                ld      hl,RG0SAV
wrtvdp_sav:
                ld      a,b
                ld      b,0
                add     hl,bc
                ld      (hl),a
wrtvdp_nosav:
                pop     hl
                ret

;--------------------------------
; $004A RDVRM
; Function : Reads the content of VRAM
; Input    : HL - address read
; Output   : A  - value which was read
; Registers: AF
rdvrm:
                call    setrd
                in      a,(VDP_DATA)
                ret


;--------------------------------
; $004D WRTVRM
; Function : Writes data in VRAM
; Input    : HL - address write
;            A  - value write
; Registers: AF
wrtvrm:
                push    af
                call    setwrt
                pop     af
                out     (VDP_DATA),a
                ret

;--------------------------------
; $0050 SETRD
; Function : Enable VDP to read
; Input    : HL - for VRAM-address
; Registers: AF
setrd:
                di

                ld      a,l
                out     (VDP_ADDR),a
                ld      a,h
                and     $3F
                out     (VDP_ADDR),a
                ei
                ret

;--------------------------------
; $0053 SETWRT
; Function : Enable VDP to write
; Input    : HL - Address
; Registers: AF
setwrt:
                di

                ld      a,l
                out     (VDP_ADDR),a
                ld      a,h
                and     $3F
                or      $40
                out     (VDP_ADDR),a
                ei
                ret

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
                push    af
                call    setwrt
                dec     bc
                inc     c
                ld      a,b
                ld      b,c
                ld      c,a
                inc     c
                pop     af
                ; Note: Interrupts should be enabled here.
                ;       Penguin Adventure can hang on boot if the interrupt
                ;       comes just after our RET, which is certain if the
                ;       memory block written is large enough.
filvrm_lp:
                out     (VDP_DATA),a
                NOP				;
                NOP				;
                djnz    filvrm_lp
                dec     c
                jr      nz,filvrm_lp
                ret

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
                call    setrd
                push    hl
                
ldirmv_lp:
		IN 	A,(VDP_DATA)
		NOP
		NOP
		LD	(DE),A
		INC	DE
		DEC	BC
		LD	A,B
		CP	0
                jr      nz,ldirmv_lp
		LD	A,C
		CP	0
                jr      nz,ldirmv_lp                
                pop     hl
                ret

;--------------------------------
; $005C LDIRVM
; Function : Block transfer from memory to VRAM
; Input    : BC - blocklength
;            DE - Start address of VRAM
;            HL - Start address of memory
; Note     : the routine doesn't change IM
; Registers: All
ldirvm:
                ex      de,hl
                call    setwrt
                ex      de,hl
ldirvm_lp:
		LD	A,(HL)
		OUT 	(VDP_DATA),A
		NOP
		NOP
		INC	HL
		DEC	BC
		LD	A,B
		CP	0
                jr      nz,ldirvm_lp
		LD	A,C
		CP	0
                jr      nz,ldirvm_lp                
                ; Note: Without this, Quinpl shows glitches.
                ; TODO: Investigate why.
                ex      de,hl
                ret

;----------------------------------
; $005F CHGMOD  Changes screen mode
; Function : Switches to given screenmode
; Input    : A  - screen mode
; Registers: All
chgmod:
                cp      4
                ret     nc
                ; Redirect to initialisation routine.
                ld      hl,chgmod_tbl
                jp      jump_table
chgmod_tbl:
                .dw      initxt          ; SCREEN0
                .dw      init32          ; SCREEN1
                .dw      inigrp          ; SCREEN2
                .dw      inimlt          ; SCREEN3
; TODO: Now that we rewrite most regs at the end of CHGMOD,
;       the ini* routines can just update RG?SAV instead of calling wrtvdp.
chgmod_finish:
                ; Generic state resets.
                ; Write new values from system RAM to the VDP.
                di
                ; Write R#0 - R#7.
                ld      hl,RG0SAV
                ld      bc,8 * $100 + VDP_ADDR
                ld      d,$80
chgmod_finish_lp:
                outi
                ld      a,b
                out     (c),d
                inc     d
                or      a
                jr      nz,chgmod_finish_lp

                ei
                jp      enascr

;--------------------------------
; $0062 CHGCLR
; Function : Changes the screencolors
; Input    : Foregroundcolor in FORCLR
;            Backgroundcolor in BAKCLR
;            Bordercolor in BDRCLR
; Registers: All
chgclr:
                ld      a,(SCRMOD)
                cp      8
                jr      z,chgclr_sc8
                dec     a
                push    af
                ld      a,(FORCLR)
                rlca
                rlca
                rlca
                rlca
                and     $F0
                ld      l,a
                ld      a,(BDRCLR)
                or      l

                ld      b,a
                ld      c,7
                call    wrtvdp
                pop     af
                ret     nz

                ; SCREEN1
                ld      a,(FORCLR)
                rlca
                rlca
                rlca
                rlca
                and     $F0
                ld      hl,BAKCLR
                or      (hl)
                ld      hl,(T32COL)
                ld      bc,$0020
                push    af
                call    setwrt
cclr_lp:
                pop     af
                out     (VDP_DATA),a
                push    af
                dec     bc
                ld      a,b
                or      c
                jr      nz,cclr_lp
                pop     af
                ret

chgclr_sc8:
                ; SCREEN8
                ld      a,(BDRCLR)
                ld      b,a
                ld      c,7
                jp      wrtvdp

;--------------------------------
; $0069 CLRSPR
; Function : Initialises all sprites
; Input    : SCRMOD
; Registers: All
clrspr:
; Check screen mode.
                ld      a,(SCRMOD)
                or      a
                ret     z               ; no sprites in SCREEN0

; Clear sprite attribute table.
                call    clrspr_attr

; Clear sprite colour table.
                ld      a,(SCRMOD)
                cp      4               ; sprite mode 1?
                jr      c,clrspr_col_skip
                ld      hl,(ATRBAS)
                dec     h
                dec     h               ; HL = (ATRBAS) - $200
                ld      bc,32 * 16
                ld      a,(FORCLR)
                and     $0F
                call    bigfil
clrspr_col_skip:

; Clear sprite pattern generator table.
                ld      hl,(PATBAS)
                ld      bc,256 * 8
                xor     a
                call    filvrm
                ret

;--------------------------------
; Clear sprite attribute table.
clrspr_attr:
                ld      a,(SCRMOD)
                cp      4
                jr      c,clrspr_attr_spritemode1

; Note: This label is called directly by external routines.
clrspr_attr_spritemode2:
                ld      e,217           ; Y coordinate
                jr      clrspr_attr_spritemode_start

; Note: This label is called directly by external routines.
clrspr_attr_spritemode1:
                ld      e,209           ; Y coordinate

clrspr_attr_spritemode_start:
                ld      hl,(ATRBAS)
                call    setwrt
                ld      a,(FORCLR)
                ld      d,a
                ld      bc,$2000        ; B = 32 = counter, C = pattern index
                di
clrspr_attr_lp:
                ld      a,e
                out     (VDP_DATA),a    ; Y coordinate
                ld      a,0
                out     (VDP_DATA),a    ; X coordinate
                ld      a,c
                out     (VDP_DATA),a    ; pattern number
                inc     c
                call    gspsiz
                jr      nc,clrspr_attr_8
                inc     c
                inc     c
                inc     c
clrspr_attr_8:
                ld      a,d
                out     (VDP_DATA),a    ; color
                djnz    clrspr_attr_lp
                ei
                ret

;--------------------------------
; $006C INITXT
; Function : Switch to SCREEN 0
; Input    : TXTNAM, TXTCGP
; Output   : NAMBAS, CGPBAS, LINLEN, SCRMOD, OLDSCR
; Registers: All
initxt:
                ; Disable video output.
                call    disscr

                ; New screen mode.
                ld      a,$00
                ld      (SCRMOD),a
                ld      (OLDSCR),a


                ; Line length.
                ld      a,(LINL40)
                ld      (LINLEN),a

                ; Cursor position: top-left.
                ld      a,1
                ld      (CSRY),a
                ld      (CSRX),a

                ; Table base addresses.
                ld      hl,(TXTNAM)     ; name table
                ld      (NAMBAS),hl
                ld      hl,(TXTCGP)     ; pattern table
                ld      (CGPBAS),hl
                ld      hl,(TXTATR)     ; sprite attribute table (unused)
                ld      (ATRBAS),hl
                ld      hl,(TXTPAT)     ; sprite pattern table (unused)
                ld      (PATBAS),hl

                ; Update VDP regs and VRAM.
                call    chgclr
                call    settxt
        .IF COMPILE_FONT != NO
                call    init_font
        .ENDIF
                call    cls_screen0
                jp      chgmod_finish

;--------------------------------
; $006F INIT32
; Function : Switches to SCREEN 1 (text screen with 32*24 characters)
; Input    : T32NAM, T32CGP, T32COL, T32ATR, T32PAT
; Output   : NAMBAS, CGPBAS, LINLEN, SCRMOD, OLDSCR
; Registers: All
init32:
                ; Disable video output.
                call    disscr

                ld      a,$01           ; SCREEN1
                ld      (SCRMOD),a
                ld      (OLDSCR),a

                ld      a,1
                ld      (CSRY),a
                ld      (CSRX),a

                call    chgclr

                ld      hl,(T32NAM)
                ld      (NAMBAS),hl
                ld      hl,(T32CGP)
                ld      (CGPBAS),hl
                ld      hl,(T32PAT)
                ld      (PATBAS),hl
                ld      hl,(T32ATR)
                ld      (ATRBAS),hl

        .IF COMPILE_FONT != NO
                call    init_font
        .ENDIF

                ld      a,(LINL32)
                ld      (LINLEN),a

                call    sett32
                call    clrspr_attr_spritemode1
                call    cls_screen1
                jp      chgmod_finish

;--------------------------------
; $0072 INIGRP
; Function : Switches to SCREEN 2 (high resolution screen with 256*192 pixels)
; Input    : GRPNAM, GRPCGP, GRPCOL, GRPATR, GRPPAT
; Output   : NAMBAS-ATRBAS, SCRMOD
; Registers: All
inigrp:
                ; Disable video output.
                call    disscr

                ld      a,$02
                ld      (SCRMOD),a

                call    chgclr

                ld      hl,(GRPNAM)
                ld      (NAMBAS),hl
                call    setwrt
                ld      b,3
                xor     a
                di
inigrp_lp:
                out     (VDP_DATA),a
                inc     a
                jr      nz,inigrp_lp
                djnz    inigrp_lp
                ei

                ld      hl,(GRPCGP)
                ld      (CGPBAS),hl

                ld      hl,(GRPATR)
                ld      (ATRBAS),hl

                ld      hl,(GRPPAT)
                ld      (PATBAS),hl

                call    setgrp
                call    clrspr_attr_spritemode1
                call    cls_screen2
                jp      chgmod_finish

;------------------------------
; $0075 INIMLT
; Function : Switches to SCREEN 3 (multi-color screen 64*48 pixels)
; Input    : MLTNAM, MLTCGP, MLTCOL, MLTATR, MLTPAT
; Output   : NAMBAS-ATRBAS, SCRMOD
; Registers: All
inimlt:
                ; Disable video output.
                call    disscr

                ld      a,$03
                ld      (SCRMOD),a

                call    chgclr

                ld      hl,(MLTNAM)
                ld      (NAMBAS),hl
                call    setwrt
                xor     a
                ld      c,6
                di
inimlt_loop1:
                push    af
                ld      e,4
inimlt_loop2:
                push    af
                ld      b,32
inimlt_loop3:
                out     (VDP_DATA),a
                inc     a
                djnz    inimlt_loop3
                pop     af
                dec     e
                jr      nz,inimlt_loop2
                pop     af
                add     a,32
                dec     c
                jr      nz,inimlt_loop1
                ei

                ld      hl,(MLTCGP)
                ld      (CGPBAS),hl
                ld      hl,(MLTATR)
                ld      (ATRBAS),hl
                ld      hl,(MLTPAT)
                ld      (PATBAS),hl

                call    setmlt
                call    clrspr_attr_spritemode1
                call    cls_screen3
                jp      chgmod_finish


;------------------------------
; $0078 SETTXT
; Function : Switches to VDP in SCREEN 0 mode
; Input    : TXTNAM, TXTCGP
; Registers: All
settxt:

                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001
                ld      b,a
                ld      c,0
                call    wrtvdp          ; write VDP R#0

                ld      a,(RG1SAV)
                and     $E7             ; MASK 11100111
                or      $10
                ld      b,a
                inc     c
                call    wrtvdp          ; write VDP R#1

                ; Set the VDP base address registers. This works because
                ; TXTNAM, TXTCOL and TXTCGP are in same order as the VDP
                ; base address registers.
                ld      de,TXTNAM
                ld      c,2

                xor     a
                call    set_base_address
                inc     de              ; Skip TXTCOL.
                inc     de
                inc     c
                xor     a
                call    set_base_address

                ret


;------------------------------
; $007B SETT32
; Function : Switches VDP to SCREEN 1 mode
; Input    : T32NAM, T32COL, T32CGP, T32ATR, T32PAT
; Registers: All
sett32:
                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001
                ld      b,a
                ld      c,0
                call    wrtvdp          ; write VDP R#0

                ld      a,(RG1SAV)
                and     $E7             ; MASK 11100111
                ld      b,a
                inc     c
                call    wrtvdp          ; write VDP R#1

                ; Set the base address registers. This works because T32NAM,
                ; T32COL, T32CGP, T32ATR and T32PAT are in same order as the
                ; VDP base address registers.
                ld      de,T32NAM
                ld      c,2

                xor     a
                call    set_base_address
                xor     a
                call    set_base_address
                xor     a
                call    set_base_address
                xor     a
                call    set_base_address
                xor     a
                call    set_base_address

                ret

;------------------------------
; $007E SETGRP
; Function : Switches VDP to SCREEN 2 mode
; Input:     GRPNAM, GRPCOL, GRPCGP, GRPATR, GRPPAT
; Registers: All
setgrp:
                ld      a,(RG0SAV)
                and     $F1             ; MASK 11110001
                or      $02             ; M3 = 1
                ld      b,a
                ld      c,0
                call    wrtvdp          ; write VDP R#0

                ld      a,(RG1SAV)
                and     $E7             ; MASK 11100111
                ld      b,a
                inc     c
                call    wrtvdp          ; write VDP R#1

                ; Set the base address registers. This works because GRPNAM,
                ; GRPCOL, GRPCGP, GRPATR and GRPPAT are in same order as the
                ; VDP base address registers.
                ld      de,GRPNAM
                ld      c,2

                xor     a
                call    set_base_address
                ld      a,$7F
                call    set_base_address
                ld      a,$03
                call    set_base_address
                xor     a
                call    set_base_address
                xor     a
                call    set_base_address

                ret

;------------------------------
; $0081 SETMLT
; Function : Switches VDP to SCREEN 3 mode
; Input    : MLTNAM, MLTCGP, MLTCOL, MLTATR, MLTPAT
; Registers: All
setmlt:
                ld      a,(RG0SAV)
                and     $F1
                ld      b,a
                ld      c,0
                call    wrtvdp

                ld      a,(RG1SAV)
                and     $E7
                or      $08             ; M2 = 1
                ld      b,a
                inc     c
                call    wrtvdp

                ; Set the base address registers. This works because MLTNAM,
                ; MLTCOL, MLTCGP, MLTATR and MLTPAT are in same order as the
                ; VDP base address registers.
                ld      de,MLTNAM
                ld      c,2

                xor     a
                call    set_base_address
                xor     a
                call    set_base_address; TODO: Should we ignore MLTCOL?
                xor     a
                call    set_base_address
                xor     a
                call    set_base_address
                xor     a
                call    set_base_address

                ret

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
                push    de
                push    af

                ; Get the shift value.
                ld      hl,set_base_address_table
                ld      b,0
                add     hl,bc
                ld      b,(hl)

                ; Get the address from (HL) to HL.
                ex      de,hl
                ld      a,(hl)
                inc     hl
                ld      h,(hl)
                ld      l,a

                ; Shift it to left in register A. After this A contains the
                ; converted address.
set_base_address_loop:
                add     hl,hl
                adc     a,a
                djnz    set_base_address_loop
                ld      b,a

                ; Set the base address register.
                pop     af
                or      b
                ld      b,a
                call    wrtvdp

                ; Increase pointer and register number.
                pop     de
                inc     de
                inc     de
                inc     c

                ret

set_base_address_table:
                .db      $00,$00,$06,$0A,$05,$09,$05

;------------------------------
; $0084 CALPAT
; Returns the address of a sprite pattern in the sprite pattern table.
; Input:     A  = pattern number
; Output:    HL = address
; Changes:   AF, DE, HL
calpat:
                ld      h,0
                ld      l,a
                add     hl,hl
                add     hl,hl
                add     hl,hl
                call    gspsiz
                jr      nc,calpat_8
                add     hl,hl
                add     hl,hl
calpat_8:       ld      de,(PATBAS)
                add     hl,de
                ret

;------------------------------
; $0087 CALATR
; Returns the address of a sprite in the sprite attribute table.
; Input:     A  = sprite number
; Output:    HL = address
; Changes:   AF, DE, HL
calatr:
                add     a,a
                add     a,a
                ld      hl,(ATRBAS)
                ld      d,0
                ld      e,a
                add     hl,de
                ret

;------------------------------
; $008A GSPSIZ
; Returns the current sprite-size in bytes.
; Output:    A  = sprite-size in bytes
;            CF = set when size is 16x16, otherwise reset
; Changes:   AF
gspsiz:
                ld      a,(RG1SAV)
                rrca
                rrca
                ld      a,8
                ret     nc
                ld      a,32
                ret

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
                push    af

                ; Printable character or control character?
                cp      $20
                jr      c,grpprt_control

                ; Different implementation depending on screen mode.
                ld      a,(SCRMOD)
                cp      2
                jr      z,grpprt_sc2
                cp      5
                jr      nc,grpprt_sc5 ; SCRMOD >= 5
grpprt_end:
                pop     af
                ret

grpprt_control:
                ; Ignore everything except carriage return ($0D).
                cp      $0D
                jr      nz,grpprt_end

                pop     af
                ; Handle carriage return.
                push    hl
                push    bc
                ld      hl,(GRPACY)
                ld      bc,$0008
                add     hl,bc
                ld      (GRPACY),hl
                ld      hl,$0000
                ld      (GRPACX),hl
                pop     bc
                pop     hl
                ret

grpprt_sc5:
                pop     af

        ; TODO: should these routines be merged?

                ret

grpprt_sc2:
                pop     af
                push    hl
                push    de
                push    bc
                push    af

                call    getpat

                ld      de,(GRPACY)
                ld      bc,(GRPACX)
                call    mapxy

                ld      a,(FORCLR)
                ld      (ATRBYT),a

                ld      hl,(CLOC)
                ld      bc,(GRPCGP)
                add     hl,bc


                ld      de,PATWRK
                ld      a,(GRPACY)
                and     $07

                ld      b,$00
                ld      c,a
                add     hl,bc
                call    grpprt_chr_x

                ld      bc,$00F0
                add     hl,bc
                ld      a,(GRPACY)
                cpl
                and     $07
                ld      c,a

                call    grpprt_chr_x

                ld      hl,(GRPACX)
                ld      bc,$0008
                add     hl,bc
                ld      (GRPACX),hl

                pop     af
                pop     bc
                pop     de
                pop     hl

                ret

grpprt_chr_x:
                ld      a,(GRPACX)
                and     $07
                push    af
                push    bc
                push    de
                push    hl
                call    grpprt_chr ; half left
                ld      a,(GRPACX)
                and     $07
                jr      z,grpprt_skip_hr
                ld      a,(CMASK)
                cpl
                ld      (CMASK),a
                pop     hl
                ld      bc,$0008
                add     hl,bc
                pop     de
                pop     bc
                pop     af

                call    grpprt_chr ; half right
                ld      a,(CMASK)
                cpl
                ld      (CMASK),a
                ret
grpprt_skip_hr:
                pop     bc ; HL = the result of last grpprt_chr
                ld      bc,$0008
                add     hl,bc
                pop     bc ; DE = the result of last grpprt_chr
                pop     bc
                pop     af
                ret



; A = Pattern , B = Pattern in VRAM
grpprt_attr:
                push    af
                push    hl
                push    bc
                push    de

                ld      d,a   ; D = Pattern of charactor
                ld      e,b   ; E = Pattern in VRAM

                ld      bc,(GRPCOL)
                add     hl,bc
                ld      c,a
                ld      a,(ATRBYT)
                and     $0f
                ld      b,a
                call    rdvrm

                push    af    ; A = an attribute in VRAM
                and     $0f
                cp      b
                jr      z,grpprt_attr_black
                pop     af
                push    af
                rrca
                rrca
                rrca
                rrca
                and     $0f
                cp      b
                jr      nz,grpprt_attr_nomatch
                pop     af
grpprt_attr_end:
                pop     de
                pop     bc
                pop     hl
                pop     af
                ret
grpprt_attr_black:
                pop     af
grpprt_attr_blk_end:
                pop     de
                pop     bc
                pop     hl
                pop     af
                cpl
                ret
grpprt_attr_nomatch:
                ld      a,d
                or      e
                cp      $ff
                jr      z,grpprt_attr_make_black
                pop     af
                rlc     b
                rlc     b
                rlc     b
                rlc     b
                and     $0f
                or      b
                call    wrtvrm
                jr      grpprt_attr_end
grpprt_attr_make_black:
                pop     af
                and     $f0
                or      b
                call    wrtvrm
                jr      grpprt_attr_blk_end


; A = X MOD 8,C = Y MOD 8, HL = CLOC
grpprt_chr:
                ld      b,c
                inc     a
                ld      c,a

                ld      a,$07
                xor     b
                inc     a
                ld      b,a

grpprt_lp:
                push    bc
                call    rdvrm
                ld      b,a
                ld      a,(de)
                call    grpprt_attr
grpprt_sft_lp:
                dec     c
                jr      z,grpprt_sft_ed
                rrca
                jr      grpprt_sft_lp
grpprt_sft_ed:
                ld      c,a
                ld      a,(CMASK)
                and     c
                ld      c,a       ; charactor with mask

                ld      a,b       ; B = pattern in VRAM
                or      c

                call    wrtvrm
                inc     hl
                inc     de
                pop     bc
                djnz    grpprt_lp
                ret


grpprt_text:    .db      "GRPPRT",0

;--------------------------------
; 0165h CHKNEW
; Is the current screen mode a bitmap mode?
; Output:  Carry flag set if current screen mode is SCREEN 5 or higher.
; Changes: AF
chknew:
                ld      a,(SCRMOD)
                cp      5
                ret

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
                push    af
                call    nsetwr
                dec     bc
                inc     c
                ld      a,b
                ld      b,c
                ld      c,a
                inc     c
                pop     af
                di
bigfil_lp:
                out     (VDP_DATA),a
                djnz    bigfil_lp
                dec     c
                jr      nz,bigfil_lp
                ei
                ret

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
                call    nset_addr
                ld      a,h
                and     $3F
                out     (VDP_ADDR),a    ; A13..A8
                ei
                ret

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
                call    nset_addr
                ld      a,h
                and     $3F
                or      $40
                out     (VDP_ADDR),a    ; A13..A8
                ei
                ret

nset_addr:
                ld      a,(ACPAGE)
                or      a
                jr      z,nset_32k

                ld      a,(SCRMOD)
                cp      5
                jp      c,setwrt
                cp      7
                ld      a,(ACPAGE)
                jr      c,nset_32k      ; SCREEN5/6 -> 32K pages
                add     a,a             ; SCREEN7/8 -> 64K pages
nset_32k:       push    hl
                and     $03             ; A  =  0   0   0   0   0   0   P1  P0
                rrca
                ld      l,a             ; L  =  P0  0   0   0   0   0   0   P1
                and     $80             ; A  =  P0  0   0   0   0   0   0   0
                xor     h               ; A  = A15 A14 A13 A12 A11 A10  A9  A8
                rla                     ; CF = A15
                rl      l               ; L  =  0   0   0   0   0   0   P1 A15
                rla                     ; CF = A14
                ld      a,l
                rla                     ; A  =  0   0   0   0   0   P1 A15 A14
                di
                out     (VDP_ADDR),a    ; A16..A14
                ld      a,$8E
                out     (VDP_ADDR),a    ; R#14
                pop     hl
                ld      a,l
                out     (VDP_ADDR),a    ; A7..A0
                ret



;--------------------------------
; 0174h NRDVRM
; Read a byte from VRAM.
; Leaves the VRAM in read mode at the byte after the one read.
; Like RDVRM, but supports 128K of VRAM.
; Input:   HL = VRAM address
;    (ACPAGE) = active VRAM page
; Output:   A = the byte read
nrdvrm:
                call    nsetrd
                in      a,(VDP_DATA)
                ret

;--------------------------------
; 0177h NWRVRM
; Write a byte to VRAM.
; Leaves the VRAM in write mode at the byte after the one written.
; Like WRTVRM, but supports 128K of VRAM.
; Input:   HL = VRAM address
;    (ACPAGE) = active VRAM page
;           A = the byte to write
nwrvrm:
                push    af
                call    nsetwr
                pop     af
                out     (VDP_DATA),a
                ret


; VDP routines which only exist in sub rom, but are useful for C-BIOS internal
; use as well:

;-------------------------------------
; $0131(sub) VDPSTA
; Read VDP status register.
; Input:   A = number of status register
; Output:  A = value read
; Changes: F
vdpsta:
                di
                ; Select desired status register.
                out     (VDP_ADDR),a
                ld      a,$80 + 15
                out     (VDP_ADDR),a
                ; Read status register.
                in      a,(VDP_STAT)
                push    af
                ; Restore status register 0.
                xor     a
                out     (VDP_ADDR),a
                ld      a,$80 + 15
                out     (VDP_ADDR),a
                ei
                pop     af
                ret


;--------------------
;Initializes VDP routine
;--------------------

init_vdp:
                in      a,(VDP_STAT)    ; reset latch

                ld      bc,$0000        ; R#0
                call    wrtvdp
                ld      bc,$E001        ; R#1
                call    wrtvdp
                ld      bc,$0002        ; R#2
                call    wrtvdp
                ld      bc,$8003        ; R#3
                call    wrtvdp
                ld      bc,$0104        ; R#4
                call    wrtvdp

                ld      a,1
                ld      (CSRY),a
                ld      (CSRX),a
                call    cls_screen1

                ld      a ,$00
                ld      hl,$0800
                ld      bc,$0800
                call    filvrm

                ; for screen 1 color table
                ld      a ,$F5
                ld      hl,$2000
                ld      bc,$0020
                call    filvrm


; PatGenTbl
;        76543210 76543210
;        00000100 00000000
;             04h      00h

                ld      bc,$F507        ; R#7
                call    wrtvdp

        .IF COMPILE_FONT != NO
                ld      hl,B_Font
                ld      de,$0800
                ld      bc,$0800
                call    ldirvm
        .ENDIF
                ret



        .IF COMPILE_FONT != NO
;------------------------------
; Initialise font.
; Uploads font to VRAM address specified by CGPBAS.
init_font:
                ld      hl,B_Font
                ld      de,(CGPBAS)
                ld      bc,$0800
                jp      ldirvm
        .ENDIF


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
                ret     nz
cls:
                ld      a,(SCRMOD)
                cp      4
                ret     nc                      ; Out of range?
                push    hl
                ld      hl,cls_table
                call    jump_table
                pop     hl
                ret
cls_table:
                .dw      cls_screen0
                .dw      cls_screen1
                .dw      cls_screen2
                .dw      cls_screen3

cls_screen0:
                ld      a,(LINLEN)
                cp      40
                ld      bc,40*24
                jr      c,cls_text
                ld      bc,80*24
                jr      cls_text

cls_screen1:
                ld      bc,32*24

cls_text:
                ld      hl,(NAMBAS)
                ld      a,$20
                call    filvrm

                ld      a,1
                ld      hl,LINTTB
                ld      (hl),a
                ld      de,LINTTB+1
                ld      bc,23
                ldir
                jp      chput_ctrl_home

cls_screen2:
                xor     a
                ld      bc,$1800
                ld      hl,(CGPBAS)
                ld      l,a
                push    bc
                call    filvrm
                pop     bc

                ld      a,(BAKCLR)
                ld      hl,(GRPCOL)
                jp      filvrm

cls_screen3:
                ld      a,(BAKCLR)
                and     $0F
                ld      b,a
                rlca
                rlca
                rlca
                rlca
                or      b
                ld      bc,$800
                ld      hl,(CGPBAS)
                jp      filvrm


; $0105 GETPAT
; Function : Returns current pattern of a character
; Input    : A  - ASCII code of character
; Output   : Pattern in PATWRK starting from address #FC40
; Registers: All
; Remark   : Same as routine in MSX1-BIOS, but there it doesn't exist as
;            a BIOS-call
getpat:
                ld      bc,(CGPNT+1)
                ld      l,a
                ld      h,0
                add     hl,hl
                add     hl,hl
                add     hl,hl
                add     hl,bc
                ld      b,8
                ld      de,PATWRK
getpat_loop:    push    bc
                push    de
                push    hl
                ld      a,(CGPNT)
                call    rdslt
                pop     hl
                pop     de
                pop     bc
                ld      (de),a
                inc     de
                inc     hl
                djnz    getpat_loop
                ret

;--------------------------------
; $00FC RIGHTC
; Function : Shifts screenpixel to the right
; Registers: AF
; NOTE     : This implementation is still a stub!
rightc:
                push    hl
                push    af
                ld      hl,rightc_text
;                call    print_debug
                pop     af
                pop     hl
                ret
rightc_text:    .db      "RIGHTC",0

;--------------------------------
; $00FF LEFTC
; Function : Shifts screenpixel to the left
; Registers: AF
; NOTE     : This implementation is still a stub!
leftc:
                push    hl
                push    af
                ld      hl,leftc_text
;                call    print_debug
                pop     af
                pop     hl
                ret
leftc_text:     .db      "LEFTC",0

;--------------------------------
; $0102 UPC
; Function : Shifts screenpixel up
; Registers: AF
; NOTE     : This implementation is still a stub!
upc:
                push    hl
                push    af
                ld      hl,upc_text
;                call    print_debug
                pop     af
                pop     hl
                ret
upc_text:       .db      "UPC",0

;--------------------------------
; $0105 TUPC
; Function : Tests whether UPC is possible, if possible, execute UPC
; Output   : C-flag set if operation would end outside the screen
; Registers: AF
; NOTE     : This implementation is still a stub!
tupc:
                push    hl
                push    af
                ld      hl,tupc_text
;                call    print_debug
                pop     af
                pop     hl
                ret
tupc_text:      .db      "TUPC",0

;--------------------------------
; $0108 DOWNC
; Function : Shifts screenpixel down
; Registers: AF
; NOTE     : This implementation is still a stub!
downc:
                push    hl
                push    af
                ld      hl,downc_text
;                call    print_debug
                pop     af
                pop     hl
                ret
downc_text:     .db      "DOWNC",0

;--------------------------------
; $010B TDOWNC
; Function : Tests whether DOWNC is possible, if possible, execute DOWNC
; Output   : C-flag set if operation would end outside the screen
; Registers: AF
; NOTE     : This implementation is still a stub!
tdownc:
                push    hl
                push    af
                ld      hl,tdownc_text
;                call    print_debug
                pop     af
                pop     hl
                ret
tdownc_text:    .db      "TDOWNC",0

;--------------------------------
; $010E SCALXY
; Function : Scales X and Y coordinates
; NOTE     : This implementation is still a stub!
scalxy:
                ld      bc,(GRPACX)
                ld      de,(GRPACY)
                ret
scalxy_text:    .db      "SCALXY",0

;--------------------------------
; $0111 MAPXYC
; Function : Places cursor at current cursor address
; Input    : BC = X coordinate,DE=Y coordinate
; Register : AF,D,HL
; NOTE     : This is a test version
mapxy:
                push    bc
                ld      (GRPACX),bc
                ld      (GRPACY),de
                ld      hl,(GRPACY)
                add     hl,hl
                add     hl,hl
                add     hl,hl
                add     hl,hl
                add     hl,hl
                ld      l,$00
                ld      b,$00

                ld      a,$ff
                ld      (CMASK),a
                ld      a,c
                and     $07
                jr      z,mapxy_mask_ed
                ld      b,a
                ld      a,$ff
mapxy_mask_lp:
                and     a
                rra
                djnz    mapxy_mask_lp
                ld      (CMASK),a
mapxy_mask_ed:
                ld      a,c
                and     $F8
                ld      c,a
                ld      b,$00
                add     hl,bc

                ld      (CLOC),hl
                pop     bc
                ret
mapxy_text:    .db      "MAPXY",0

;--------------------------------
; $0114 FETCHC
; Function : Gets current cursor addresses mask pattern
; Output   : HL - Cursor address
;            A  - Mask pattern
fetchc:
                ld      a,(CMASK)
                ld      hl,(CLOC)
                ret
fetchc_text:    .db      "FETCHC",0

;--------------------------------
; $0117 STOREC
; Function : Record current cursor addresses mask pattern
; Input    : HL - Cursor address
;            A  - Mask pattern
; NOTE     : This implementation is still a stub!
storec:
                push    hl
                push    af
                ld      hl,storec_text
;                call    print_debug
                pop     af
                pop     hl
                ret
storec_text:    .db      "STOREC",0

;--------------------------------
; $011A SETATR
; Function : Set attribute byte
; NOTE     : This implementation is still a stub!
setatr:
                push    hl
                push    af
                ld      hl,setatr_text
;                call    print_debug
                pop     af
                pop     hl
                ret
setatr_text:    .db      "SETATR",0

;--------------------------------
; $011D READC
; Function : Reads attribute byte of current screenpixel
; NOTE     : This implementation is still a stub!
readc:
                push    hl
                push    af
                ld      hl,readc_text
;                call    print_debug
                pop     af
                pop     hl
                ret
readc_text:     .db      "READC",0

;--------------------------------
; $0120 SETC
; Function : Returns currenct screenpixel of specificed attribute byte
; NOTE     : This implementation is still a stub!
setc:
                push    hl
                push    af
                ld      hl,setc_text
;                call    print_debug
                pop     af
                pop     hl
                ret
setc_text:      .db      "SETC",0

;--------------------------------
; $0123 NSETCX
; Function : Set horizontal screenpixels
; NOTE     : This implementation is still a stub!
nsetcx:
                push    hl
                push    af
                ld      hl,nsetcx_text
;                call    print_debug
                pop     af
                pop     hl
                ret
nsetcx_text:    .db      "NSETCX",0

;--------------------------------
; $0126 GTASPC
; Function : Gets screen relations
; Output   : DE, HL
; Registers: DE, HL
; NOTE     : This implementation is still a stub!
gtaspc:
                push    hl
                push    af
                ld      hl,gtaspc_text
;                call    print_debug
                pop     af
                pop     hl
                ret
gtaspc_text:    .db      "GTASPC",0

;--------------------------------
; $0129 PNTINI
; Function : Initalises the PAINT instruction
; NOTE     : This implementation is still a stub!
pntini:
                push    hl
                push    af
                ld      hl,pntini_text
;                call    print_debug
                pop     af
                pop     hl
                ret
pntini_text:    .db      "PNTINI",0

;--------------------------------
; $012C SCANR
; Function : Scans screenpixels to the right
; NOTE     : This implementation is still a stub!
scanr:
                push    hl
                push    af
                ld      hl,scanr_text
;                call    print_debug
                pop     af
                pop     hl
                ret
scanr_text:     .db      "SCANR",0

;--------------------------------
; $012F SCANL
; Function : Scans screenpixels to the left
; NOTE     : This implementation is still a stub!
scanl:
                push    hl
                push    af
                ld      hl,scanl_text
;                call    print_debug
                pop     af
                pop     hl
                ret
scanl_text:     .db      "SCANL",0

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:
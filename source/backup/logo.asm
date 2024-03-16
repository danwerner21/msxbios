; C-BIOS logo ROM
;
; Copyright (c) 2004-2005 Maarten ter Huurne.  All rights reserved.
; Copyright (c) 2004-2005 Albert Beevendorp.  All rights reserved.
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

        INCLUDE "systemva.asm"

;
logo_ident:
        DB     "C-BIOS Logo ROM",$FF

logo_show:
        IF     VDP = TMS99X8
        CALL    $6f

        LD      a,5
        LD      (BAKCLR),a
        LD      (BDRCLR),a
        CALL    $62

        LD      hl,(NAMBAS)
        LD      bc,768
        LD      a,$00
        CALL    $56

; Set up SCREEN 2 mirrored
        LD      bc,0 +(256* 2)
        CALL    $47
        LD      bc,3 +(256* 159)
        CALL    $47
        LD      bc,4 +(256* 0)
        CALL    $47

; Fill the color table
        LD      a,(FORCLR)
        AND     15
        RLCA
        RLCA
        RLCA
        RLCA
        LD      b,a
        LD      a,(BAKCLR)
        AND     15
        OR      b
        LD      bc,2048
        LD      hl,(GRPCOL)
        CALL    $56

        LD      hl,(CGPBAS)
        LD      bc,8 * logo_patoffset
        ADD     hl,bc
        EX      de,hl
        LD      bc,8 * logo_npatterns
        LD      hl,logo_patterns
        CALL    $5c

        LD      hl,(CGPBAS)
        LD      bc,8 * 32
        ADD     hl,bc
        EX      de,hl
        LD      hl,(4)
        ADD     hl,bc
        LD      bc,8 * 96
        CALL    $5c

        LD      hl,(GRPCOL)
        LD      bc,8 * logo_patoffset
        ADD     hl,bc
        EX      de,hl
        LD      bc,8 * logo_ncolors
        LD      hl,logo_colors
        CALL    $5c

        LD      hl,(GRPCOL)
        LD      de,8 * 32
        ADD     hl,de
        LD      bc,8 * 96
        LD      a,$f1
        CALL    $56

        LD      hl,(NAMBAS)
        LD      bc,logo_namoffset
        ADD     hl,bc
        EX      de,hl
        LD      hl,logo_names
        LD      b,logo_height
plot_logo_nam:
        PUSH    bc
        PUSH    hl
        PUSH    de
        LD      bc,logo_width
        CALL    $5c
        POP     hl                                ; value of DE
        LD      bc,32
        ADD     hl,bc
        EX      de,hl
        POP     hl                                ; value of HL
        LD      bc,logo_width
        ADD     hl,bc
        POP     bc
        DJNZ    plot_logo_nam

        RET
;
logo_patoffset  EQU    128
logo_namoffset  EQU    (4 *32)+ 4                        ; Y *32+ 4
;
logo_patterns:
        DB     $00,$00,$00,$00,$00,$00,$00,$00
logo_patlength EQU    $ - logo_patterns
        DB     $00,$00,$00,$00,$00,$00,$FE,$F8
        DB     $00,$00,$FE,$F8,$1F,$7F,$00,$00
        DB     $FE,$3F,$00,$00,$00,$00,$00,$00
        DB     $00,$00,$00,$00,$00,$00,$00,$00
        DB     $1F,$00,$00,$00,$00,$00,$00,$00
        DB     $00,$00,$1F,$F8,$FE,$00,$00,$00
        DB     $00,$00,$00,$00,$00,$7F,$1F,$F0
        DB     $00,$00,$00,$00,$FE,$FC,$F8,$F0
        DB     $1F,$3F,$7F,$00,$00,$00,$00,$00
        DB     $00,$00,$00,$00,$FE,$FC,$F8,$F0
        DB     $00,$F8,$1F,$7F,$00,$00,$00,$00
        DB     $00,$00,$00,$00,$00,$00,$00,$00
        DB     $00,$1F,$F8,$FE,$00,$00,$00,$00
        DB     $00,$00,$00,$00,$00,$7F,$3F,$1F
        DB     $F8,$FC,$FE,$00,$00,$00,$00,$00
        DB     $00,$00,$00,$00,$7F,$3F,$1F,$F0
        DB     $00,$00,$00,$00,$00,$00,$FE,$FE
        DB     $1F,$3F,$7F,$7F,$00,$00,$00,$00
        DB     $00,$00,$00,$00,$00,$FE,$FE,$FC
        DB     $1F,$3F,$7F,$00,$00,$00,$00,$00
        DB     $F8,$F8,$F8,$F8,$F0,$F0,$F0,$F0
        DB     $1F,$1F,$1F,$1F,$3F,$3F,$3F,$3F
        DB     $F8,$F8,$F8,$F8,$00,$00,$00,$00
        DB     $F8,$F8,$F0,$F0,$F0,$1F,$1F,$1F
        DB     $00,$00,$00,$00,$00,$00,$00,$00
        DB     $F8,$F8,$F0,$F0,$F0,$1F,$1F,$1F
        DB     $F0,$F0,$F0,$F0,$1F,$1F,$1F,$1F
        DB     $3F,$C7,$DF,$00,$00,$F8,$F0,$1F
        DB     $00,$00,$00,$00,$00,$00,$00,$00
        DB     $00,$00,$00,$00,$F8,$00,$00,$00
        DB     $00,$1F,$F8,$FC,$FE,$00,$7F,$3F
        DB     $FE,$FE,$FE,$FE,$FC,$FC,$FC,$7C
        DB     $F0,$F0,$F0,$F0,$1F,$1F,$1F,$1F
        DB     $00,$00,$FC,$F8,$1F,$3F,$7F,$FE
        DB     $1F,$00,$00,$00,$F0,$7F,$00,$00
        DB     $1F,$FC,$00,$00,$00,$1F,$F0,$F8
        DB     $00,$00,$00,$7F,$3F,$1F,$1F,$F0
        DB     $FE,$F8,$F0,$1F,$7F,$7C,$FC,$F8
        DB     $3F,$3F,$7F,$7F,$7F,$7F,$7F,$7F
        DB     $00,$00,$00,$00,$00,$00,$00,$00
        DB     $3F,$3F,$7F,$7F,$7F,$7F,$7F,$7F
        DB     $1F,$1F,$1F,$1F,$3F,$3F,$3F,$3F
        DB     $3F,$7F,$7F,$7F,$00,$00,$00,$00
        DB     $1F,$F0,$F8,$F8,$F8,$F8,$F8,$F8
        DB     $7C,$3C,$3C,$3C,$C7,$C7,$C7,$C7
        DB     $1F,$1E,$E3,$E3,$C7,$C7,$C7,$C7
        DB     $FC,$F8,$F0,$F0,$1F,$3F,$3F,$3F
        DB     $FC,$FE,$00,$00,$00,$00,$00,$00
        DB     $F0,$F8,$F8,$F8,$F8,$F8,$F8,$F8
        DB     $F8,$F8,$F8,$F8,$F8,$FE,$7F,$3F
        DB     $00,$00,$00,$00,$00,$00,$00,$00
        DB     $00,$00,$00,$00,$00,$00,$7F,$1F
        DB     $00,$00,$00,$00,$00,$00,$00,$00
        DB     $3F,$3F,$3F,$3F,$7E,$7E,$7E,$7E
        DB     $F8,$F8,$F8,$F0,$1F,$1F,$3F,$7E
        DB     $C7,$C7,$C7,$C7,$F0,$F0,$F0,$F0
        DB     $C7,$C7,$C7,$C7,$78,$78,$78,$7C
        DB     $3F,$3F,$3F,$3F,$3F,$3F,$1F,$F0
        DB     $00,$00,$00,$FE,$FC,$FC,$F8,$F0
        DB     $F8,$F8,$F8,$F8,$F0,$F0,$1F,$1F
        DB     $1F,$F8,$00,$00,$00,$00,$00,$00
        DB     $00,$00,$00,$00,$00,$00,$00,$00
        DB     $00,$00,$FE,$00,$00,$00,$00,$00
        DB     $F0,$F8,$FC,$7C,$7C,$7C,$7C,$7C
        DB     $00,$00,$00,$7F,$7F,$7F,$3F,$3F
        DB     $00,$00,$00,$00,$00,$00,$00,$00
        DB     $00,$00,$00,$7F,$7F,$7F,$3F,$3F
        DB     $7E,$7E,$7E,$7E,$00,$00,$00,$00
        DB     $00,$00,$00,$00,$00,$00,$00,$00
        DB     $00,$FE,$FC,$1F,$00,$00,$FE,$F0
        DB     $FE,$FC,$F8,$F0,$3C,$8F,$1F,$7F
        DB     $F0,$F0,$F0,$F0,$9F,$9F,$9F,$9F
        DB     $7C,$7E,$7E,$7F,$E3,$3E,$3F,$3F
        DB     $F8,$FC,$FE,$00,$7F,$3F,$F0,$3E
        DB     $00,$00,$00,$3F,$00,$00,$00,$00
        DB     $00,$00,$00,$FC,$00,$00,$00,$FE
        DB     $1F,$3F,$7F,$FE,$F8,$F1,$3C,$F0
        DB     $3F,$7F,$00,$00,$7F,$00,$00,$00
        DB     $00,$00,$00,$FC,$00,$00,$00,$FE
        DB     $7C,$FC,$F8,$F8,$1F,$3F,$7F,$00
        DB     $3F,$3F,$1F,$1F,$F0,$F0,$F8,$FC
        DB     $3F,$1F,$1F,$F0,$F8,$F8,$FC,$FE
        DB     $FC,$FE,$00,$00,$00,$00,$00,$00
        DB     $00,$00,$00,$7F,$3F,$1F,$F0,$F8
        DB     $FE,$00,$00,$00,$00,$00,$00,$00
        DB     $00,$00,$7F,$3F,$1F,$F8,$FE,$00
        DB     $00,$00,$00,$00,$00,$00,$00,$3F
        DB     $00,$00,$00,$00,$00,$FE,$F8,$3F
        DB     $F0,$1F,$3F,$7F,$00,$00,$00,$00
        DB     $00,$00,$00,$00,$00,$00,$FE,$FC
        DB     $F8,$F0,$1F,$3F,$7F,$00,$00,$00
        DB     $FC,$FE,$00,$00,$00,$00,$00,$00
        DB     $00,$00,$7F,$1F,$F8,$FE,$00,$00
        DB     $00,$00,$00,$00,$00,$00,$3F,$FE
        DB     $00,$00,$00,$00,$00,$00,$00,$1F
        DB     $00,$00,$00,$FE,$F8,$1F,$00,$00
        DB     $F0,$1F,$7F,$00,$00,$00,$00,$00
        DB     $00,$00,$00,$00,$00,$00,$00,$00
        DB     $00,$7F,$7F,$7F,$7F,$7F,$7F,$7F
        DB     $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F
        DB     $7F,$00,$00,$00,$00,$00,$00,$00
logo_npatterns  EQU    ($ - logo_patterns) / logo_patlength
;
logo_colors:
        DB     $00,$00,$00,$00,$00,$00,$00,$00
logo_collength EQU    $ - logo_colors
        DB     $00,$00,$00,$00,$00,$00,$09,$09
        DB     $00,$00,$09,$09,$90,$90,$09,$09
        DB     $09,$90,$09,$09,$09,$09,$09,$09
        DB     $09,$09,$09,$09,$09,$09,$09,$09
        DB     $09,$09,$09,$09,$09,$09,$09,$09
        DB     $00,$00,$09,$90,$90,$09,$09,$09
        DB     $00,$00,$00,$00,$00,$09,$09,$90
        DB     $01,$01,$01,$01,$19,$19,$19,$19
        DB     $91,$91,$91,$09,$09,$09,$09,$09
        DB     $09,$09,$09,$09,$91,$91,$91,$91
        DB     $09,$91,$19,$19,$01,$01,$01,$01
        DB     $01,$01,$01,$01,$01,$01,$01,$01
        DB     $09,$91,$19,$19,$01,$01,$01,$01
        DB     $09,$09,$09,$09,$09,$91,$91,$91
        DB     $91,$91,$91,$09,$09,$09,$09,$09
        DB     $01,$01,$01,$01,$19,$19,$19,$91
        DB     $00,$00,$00,$00,$00,$00,$09,$09
        DB     $91,$91,$91,$91,$09,$09,$09,$09
        DB     $09,$09,$09,$09,$09,$91,$91,$91
        DB     $19,$19,$19,$01,$01,$01,$01,$01
        DB     $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F
        DB     $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F
        DB     $F1,$F1,$F1,$F1,$01,$01,$01,$01
        DB     $08,$09,$09,$09,$08,$90,$90,$90
        DB     $08,$09,$09,$09,$08,$09,$09,$09
        DB     $81,$91,$91,$91,$81,$19,$19,$19
        DB     $1F,$1F,$1F,$1F,$F1,$F1,$F1,$F1
        DB     $1F,$F1,$F1,$0F,$0F,$F1,$F1,$1F
        DB     $0F,$0F,$0F,$0F,$01,$01,$01,$01
        DB     $0F,$0F,$0F,$0F,$1F,$01,$01,$01
        DB     $01,$1F,$F1,$F1,$F1,$0F,$F1,$F1
        DB     $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F
        DB     $F1,$F1,$F1,$F1,$1F,$1F,$1F,$1F
        DB     $01,$01,$1F,$1F,$F1,$F1,$F1,$F1
        DB     $F1,$0F,$0F,$0F,$F1,$1F,$01,$01
        DB     $1F,$F1,$0F,$0F,$0F,$F1,$1F,$1F
        DB     $01,$01,$01,$1F,$1F,$1F,$1F,$F1
        DB     $1F,$1F,$1F,$F1,$F1,$F1,$F1,$F1
        DB     $80,$90,$90,$80,$90,$80,$80,$90
        DB     $08,$09,$09,$08,$09,$08,$08,$09
        DB     $18,$19,$19,$18,$19,$18,$18,$19
        DB     $F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1
        DB     $1F,$1F,$1F,$1F,$01,$01,$01,$01
        DB     $F1,$1F,$1F,$1F,$1F,$1F,$1F,$1F
        DB     $1F,$1F,$1F,$1F,$F1,$F1,$F1,$F1
        DB     $1F,$1F,$F1,$F1,$F1,$F1,$F1,$F1
        DB     $F1,$F1,$F1,$F1,$1F,$1F,$1F,$1F
        DB     $1F,$1F,$01,$01,$01,$01,$01,$01
        DB     $F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1
        DB     $F1,$F1,$F1,$F1,$F1,$F1,$F1,$F1
        DB     $01,$01,$01,$01,$01,$01,$0F,$0F
        DB     $01,$01,$01,$01,$01,$01,$1F,$1F
        DB     $08,$08,$08,$09,$08,$08,$08,$09
        DB     $F1,$F1,$E1,$F1,$F1,$E1,$E1,$F1
        DB     $1F,$1F,$1E,$1F,$F1,$E1,$E1,$F1
        DB     $F1,$F1,$E1,$F1,$1F,$1E,$1E,$1F
        DB     $F1,$F1,$E1,$F1,$1F,$1E,$1E,$1F
        DB     $1F,$1F,$1E,$1F,$1F,$1E,$1E,$F1
        DB     $01,$01,$01,$1F,$1F,$1E,$1E,$1F
        DB     $F1,$F1,$E1,$F1,$F1,$E1,$1E,$1F
        DB     $F1,$1F,$01,$01,$01,$01,$01,$01
        DB     $0F,$0F,$01,$01,$01,$01,$01,$01
        DB     $0F,$0F,$1E,$01,$01,$01,$01,$01
        DB     $F1,$F1,$E1,$F1,$F1,$E1,$E1,$F1
        DB     $08,$08,$08,$80,$80,$80,$80,$80
        DB     $08,$08,$08,$08,$08,$08,$08,$08
        DB     $01,$01,$01,$18,$18,$18,$18,$18
        DB     $E1,$E1,$E1,$E1,$0E,$0E,$0E,$0E
        DB     $01,$01,$01,$01,$0E,$0E,$0E,$0E
        DB     $01,$1E,$1E,$E1,$0E,$0E,$E1,$E1
        DB     $E1,$E1,$E1,$E1,$1E,$E1,$E1,$E1
        DB     $1E,$1E,$1E,$1E,$E1,$E1,$E1,$E1
        DB     $1E,$1E,$1E,$1E,$1E,$E1,$E1,$E1
        DB     $E1,$E1,$E1,$0E,$E1,$E1,$1E,$1E
        DB     $01,$01,$01,$1E,$0E,$0E,$0E,$0E
        DB     $01,$01,$01,$1E,$0E,$0E,$0E,$E1
        DB     $E1,$E1,$E1,$E1,$E1,$E1,$1E,$1E
        DB     $1E,$1E,$01,$01,$E1,$0E,$0E,$0E
        DB     $01,$01,$01,$1E,$0E,$0E,$0E,$E1
        DB     $E1,$E1,$E1,$E1,$1E,$1E,$1E,$01
        DB     $80,$80,$80,$80,$08,$08,$08,$08
        DB     $18,$18,$18,$81,$81,$81,$81,$81
        DB     $08,$08,$00,$00,$00,$00,$00,$00
        DB     $08,$08,$08,$81,$81,$81,$18,$18
        DB     $81,$08,$08,$08,$08,$08,$08,$08
        DB     $01,$01,$18,$18,$18,$81,$81,$08
        DB     $01,$01,$01,$01,$01,$01,$01,$18
        DB     $01,$01,$01,$01,$01,$18,$18,$81
        DB     $18,$81,$81,$81,$08,$08,$08,$08
        DB     $08,$08,$08,$08,$08,$08,$81,$81
        DB     $81,$81,$18,$18,$18,$01,$01,$01
        DB     $08,$08,$00,$00,$00,$00,$00,$00
        DB     $08,$08,$80,$80,$08,$08,$00,$00
        DB     $08,$08,$08,$08,$08,$08,$80,$08
        DB     $08,$08,$08,$08,$08,$08,$08,$08
        DB     $08,$08,$08,$80,$80,$08,$00,$00
        DB     $84,$08,$08,$00,$00,$00,$00,$00
        DB     $04,$00,$00,$00,$00,$00,$00,$00
        DB     $00,$04,$04,$04,$04,$04,$04,$04
        DB     $04,$04,$04,$04,$04,$04,$04,$04
        DB     $04,$00,$00,$00,$00,$00,$00,$00
logo_ncolors    EQU    ($ - logo_colors) / logo_collength
;
logo_names:
        DB     $80,$80,$81,$82,$83,$84,$85,$86,$87,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
logo_width     EQU    $ - logo_names
        DB     $80,$88,$89,$8A,$8B,$8C,$8D,$8E,$8F,$90,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$E3
        DB     $91,$92,$93,$94,$8C,$95,$96,$8C,$8C,$8C,$8C,$97,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$E4
        DB     $98,$99,$9A,$8C,$8C,$9B,$9C,$9D,$9E,$9F,$A0,$A1,$A2,$A3,$9D,$A4,$A5,$A6,$9D,$9D,$9D,$9D,$8C,$E4
        DB     $A7,$A8,$A9,$8C,$8C,$AA,$AB,$8C,$8C,$AC,$AD,$AE,$AF,$8C,$8C,$B0,$B1,$B2,$B3,$B3,$B3,$B4,$8C,$E4
        DB     $B5,$B5,$8C,$8C,$8C,$B6,$8C,$8C,$8C,$B7,$B8,$B9,$BA,$8C,$8C,$BB,$BC,$BD,$BE,$BE,$BF,$C0,$8C,$E4
        DB     $C1,$C2,$C3,$8C,$8C,$C4,$C5,$C5,$C6,$C7,$C8,$C9,$CA,$CB,$CC,$CD,$CE,$C5,$C5,$C5,$CF,$D0,$8C,$E4
        DB     $D1,$C2,$D2,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$E4
        DB     $D3,$D4,$D5,$D6,$D7,$D8,$D9,$DA,$DB,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C,$8C
        DB     "N"
        DB     "8VEM"
        DB     $E4
        DB     $80,$DC,$DD,$DE,$C2,$DF,$E0,$E1,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E2,$E5
logo_height    EQU    ($ - logo_names) / logo_width
;
        ENDIF
;
        IF     VDP = V9938
        LD      de,$c000
        LD      hl,msx2logodata
        CALL    unPack

        LD      hl,0
        LD      (BAKCLR),hl
        LD      a,5
        CALL    $5f

        LD      hl,palette1
        CALL    setpalette

        CALL    $41

        LD      hl,256
        LD      (BAKCLR),hl
        CALL    $62

        LD      a,(RG8SAV+1)
        AND     127
        LD      b,a
        LD      c,9
        CALL    $47

wait_ce_logo:
        LD      a,2
        LD      ix,$131
        CALL    $15f
        BIT     0,a
        JR      nz,wait_ce_logo

        PUSH    de
        LD      bc,15
        LD      hl,logo_hmmc
        LDIR

        POP     hl
        LD      bc,13
        ADD     hl,bc
        LD      de,$c000
        LD      a,(de)
        INC     de
        LD      (hl),a
        EX      de,hl

        DI
        LD      a,32
        OUT     ($99),a
        LD      a,128+ 17
        OUT     ($99),a
        PUSH    hl
        LD      bc,15 *256+ $9b
        LD      hl,logo_hmmc
        OTIR
        POP     hl
        LD      a,128+ 44
        OUT     ($99),a
        LD      a,128+ 17
        OUT     ($99),a
        EI

        LD      b,255
        OTIR

loop_logo:
        LD      a,2
        LD      ix,$131
        CALL    $15f
        BIT     0,a
        JR      z,done_logo

        OTIR
        JR      loop_logo

done_logo:
        LD      bc,32
        LD      de,$c000
        LD      hl,palette1
        LDIR

        LD      hl,22 *8
        LD      (GRPACX),hl
        LD      hl,12 *8 +1
        LD      (GRPACY),hl
        LD      a,7
        LD      (FORCLR),a
        LD      a,8
        LD      (LOGOPR),a
        LD      hl,logo_ver
        CALL    prn_text
        LD      (LOGOPR),a

        CALL    $44

palette_loop:
        LD      b,16
        LD      de,palette2
        LD      hl,$c000
palette_color:
        LD      a,(de)                            ; change red
        AND     240
        LD      c,a
        LD      a,(hl)
        AND     240
        CP      c
        JR      z,palette_red_done
        JR      nc,palette_red_down
        ADD     a,16
        JR      palette_red_done
palette_red_down:
        SUB     16
palette_red_done:
        LD      c,a
        LD      a,(hl)
        AND     15
        OR      c
        LD      (hl),a

        LD      a,(de)
        AND     15
        LD      c,a
        LD      a,(hl)
        AND     15
        CP      c
        JR      z,palette_blue_done
        JR      nc,palette_blue_down
        INC     a
        JR      palette_blue_done
palette_blue_down:
        DEC     a
palette_blue_done:
        LD      c,a
        LD      a,(hl)
        AND     240
        OR      c
        LD      (hl),a
        INC     de
        INC     hl

        LD      a,(de)
        LD      c,a
        LD      a,(hl)
        CP      c
        JR      z,palette_green_done
        JR      nc,palette_green_down
        INC     a
        JR      palette_green_done
palette_green_down:
        DEC     a
palette_green_done:
        LD      (hl),a
        INC     de
        INC     hl
        DJNZ    palette_color

        LD      hl,$c000
        CALL    setpalette

        LD      b,6
palette_wait:
        HALT
        DJNZ    palette_wait

        LD      b,32
        LD      de,palette2
        LD      hl,$c000
palette_check:
        LD      a,(de)
        CP      (hl)
        JR      nz,palette_loop
        INC     de
        INC     hl
        DJNZ    palette_check

        LD      b,9
        LD      hl,glare
glare_loop:
        LD      e,(hl)
        INC     hl
        LD      d,(hl)
        INC     hl
        PUSH    bc
        PUSH    hl
        EX      de,hl
        CALL    setpalette
        POP     hl
        POP     bc

        HALT
        HALT

        DJNZ    glare_loop

        RET

setpalette:
        DI
        XOR     a
        OUT     ($99),a
        LD      a,128+ 16
        OUT     ($99),a
        LD      bc,32 *256+ $9a
        OTIR
        EI
        RET

prn_text:
        LD      a,(SCRMOD)
        CP      5
        JR      nc,prn_text_graph
prn_text_char:
        LD      a,(hl)
        OR      a
        RET     z
        CALL    $a2
        INC     hl
        JR      prn_text_char
prn_text_graph:
        LD      a,(hl)
        OR      a
        RET     z
        LD      ix,$0089
        CALL    $15f
        INC     hl
        JR      prn_text_graph

logo_hmmc:
        DW     0
        DW     0
        DW     0
        DW     31
        DW     256
        DW     85
col:
        DB     0
        DB     0
        DB     $f0

palette1:
        DW     $000,$327,$327,$327,$327,$327,$327,$327
        DW     $327,$327,$327,$327,$327,$327,$327,$327

palette2:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$563,$573,$572,$672,$772,$470,$270

palette3:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$563,$573,$572,$672,$772,$470,$777

palette4:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$563,$573,$572,$672,$772,$777,$270

palette5:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$563,$573,$572,$672,$777,$470,$270

palette6:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$563,$573,$572,$777,$772,$470,$270

palette7:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$563,$573,$777,$672,$772,$470,$270

palette8:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$563,$777,$572,$672,$772,$470,$270

palette9:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $777,$563,$573,$572,$672,$772,$470,$270

palette10:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$777,$573,$572,$672,$772,$470,$270

glare:
        DW     palette3,palette4,palette5,palette6
        DW     palette7,palette8,palette9,palette10,palette2
;
logo_ver:
        DB     "V"
        DB     "N8VEM 1.24"
        DB     0

;
; Bitbuster by Team Bomba
;

;
; In: HL = source
;     DE = destination
;
unPack:
        EXX
        LD      bc,128                            ; b' = 0 (register loading optimize)
                                                  ; c' = bits from bitstream
        EXX

unPack_loop:
        EXX
        CALL    getBit
        EXX
        JR      c,unPack_outCompress              ; if set, we got LZ77 compression

unPack_outLiteral:
        LDI                                       ; copy byte from compressed data to destination
        JR      unPack_loop                       ; handle more compressed data

unPack_outCompress:
        LD      a,(hl)                            ; get lowest 7 bits of offset, plus the offset
                                                  ; extension bit
        INC     hl

        OR      a
        JR      z,unPack_outRle                   ; offset = 0, RLE compression used

unPack_outMatch:
        EXX
        LD      e,a
        LD      d,b                               ; b' should be always clear when entering this part
        RLCA                                      ; offset extension bit set?
        JR      nc,unPack_outMatch1               ; no need to get extra bits if carry not set

        CALL    getBit                            ; get offset bit 10
        RL      d
        CALL    getBit                            ; get offset bit 9
        RL      d
        CALL    getBit                            ; get offset bit 8
        RL      d
        CALL    getBit                            ; get offset bit 7

        JR      c,unPack_outMatch1                ; since extension mark already makes bit 7 set
        RES     7,e                               ; only clear it if the bit should be cleared
unPack_outMatch1:
        INC     de
        CALL    getGammaValue_0                   ; get the match length
                                                  ; HL' = length

        PUSH    hl                                ; save compressed data pointer
        EXX
        PUSH    hl                                ; save match length
        PUSH    de                                ; save match offset
        EXX

        LD      h,d                               ; destination in HL
        LD      l,e
        POP     bc                                ; load match offset length
        SBC     hl,bc                             ; calculate source address
        POP     bc                                ; load match length
        LDIR

        POP     hl                                ; load compressed data pointer
        JR      unPack_loop

unPack_outRle:
        CALL    getGammaValue
        RET     c                                 ; HL' = repeat length

        PUSH    hl                                ; save compressed data pointer
        EXX
        PUSH    hl                                ; save repeat length
        EXX
        POP     bc                                ; load repeat length

        LD      h,d                               ; source = destination - 1
        LD      l,e
        DEC     hl
        LDIR

        POP     hl                                ; load compressed data pointer
        JR      unPack_loop
;
getBit:
        SLA     c                                 ; shift out new bit
        RET     nz                                ; if remaining value != 0, we're done

        EXX
        LD      a,(hl)                            ; get 8 bits from the compressed stream
        INC     hl
        EXX

        LD      c,a                               ; 8 bits in C'
        SLA     c                                 ; shift out new bit
        INC     c                                 ; set bit 0 so C' will be zero after shifting 8 times
        RET
;
getGammaValue:
        EXX                                       ; get number of bits used to encode value
getGammaValue_0:
        LD      hl,1                              ; initial length
        LD      b,1                               ; bitcount

getGammaValue_size:
        CALL    getBit                            ; get more bits
        JR      nc,getGammaValue_sizeEnd          ; if bit is not set, bit length is known
        INC     b                                 ; increase bitcount
        JR      getGammaValue_size

getGammaValue_bits:
        CALL    getBit                            ; get next bit of value from the compressed stream
        ADC     hl,hl                             ; insert new bit in HL
getGammaValue_sizeEnd:
        DJNZ    getGammaValue_bits                ; repeat if more bits to go

getGammaValue_end:
        INC     hl                                ; correct HL (was stored as length - 2)
        EXX
        RET
;
msx2logodata:
        DB     $7E,$00,$00,$01,$04,$33,$00,$C1,$40,$00,$00,$FD,$A2,$43,$79,$36
        DB     $A9,$99,$00,$07,$7F,$34,$7D,$63,$FB,$10,$3A,$99,$9A,$6A,$BB,$00
        DB     $BA,$85,$24,$93,$83,$BF,$61,$7E,$0F,$AA,$AA,$BB,$00,$02,$AA,$A9
        DB     $80,$3F,$AE,$C7,$7E,$AB,$7E,$8C,$83,$AC,$80,$7F,$59,$7E,$EF,$80
        DB     $1F,$D7,$67,$7E,$7D,$AC,$80,$7F,$5B,$7E,$E2,$AA,$D9,$00,$89,$6A
        DB     $80,$2B,$85,$7E,$98,$05,$43,$E8,$72,$BA,$AA,$B5,$FD,$57,$89,$88
        DB     $63,$BA,$93,$50,$7A,$F9,$FC,$EF,$66,$F6,$D9,$B6,$F7,$61,$0D,$38
        DB     $98,$9A,$80,$1D,$4D,$81,$00,$FB,$1F,$7F,$96,$39,$BF,$F6,$0E,$3C
        DB     $91,$88,$80,$D2,$A9,$8E,$37,$01,$F5,$5F,$20,$7F,$94,$99,$F6,$7E
        DB     $8D,$80,$9E,$90,$26,$8E,$FE,$7F,$8C,$39,$9B,$E1,$7F,$B9,$6C,$E6
        DB     $88,$97,$80,$DF,$FC,$7F,$A4,$99,$EC,$F3,$EB,$1E,$82,$38,$8A,$BB
        DB     $80,$5F,$77,$7F,$F2,$63,$FE,$B8,$EA,$F5,$96,$80,$66,$93,$7F,$93
        DB     $7F,$41,$FE,$1F,$1C,$80,$AE,$8A,$31,$16,$EF,$7F,$FC,$8C,$FE,$7C
        DB     $B3,$01,$89,$89,$F8,$9F,$01,$93,$7F,$CF,$1C,$99,$CC,$00,$57,$E9
        DB     $EC,$FE,$7F,$E9,$39,$9C,$7E,$C1,$C9,$93,$7F,$FD,$B4,$FE,$7C,$2B
        DB     $66,$00,$4E,$E1,$7C,$36,$14,$7F,$7F,$C1,$B9,$9C,$7F,$8F,$FE,$21
        DB     $57,$77,$76,$4E,$F0,$BE,$35,$14,$3F,$7F,$E0,$A5,$FE,$E2,$7C,$56
        DB     $7F,$8F,$67,$7F,$E5,$07,$04,$3C,$CC,$7F,$28,$33,$FF,$F3,$7C,$66
        DB     $7F,$47,$55,$56,$7F,$F2,$64,$43,$CC,$FE,$FC,$11,$E9,$14,$77,$63
        DB     $FF,$FD,$39,$04,$33,$FE,$1D,$3F,$93,$7F,$B5,$3A,$03,$EE,$00,$7F
        DB     $33,$7F,$6B,$FE,$2F,$17,$7F,$F5,$94,$FD,$D5,$FD,$B9,$F5,$7F,$EF
        DB     $79,$01,$C4,$8A,$77,$09,$E7,$01,$CE,$B0,$3A,$7A,$01,$3F,$63,$7F
        DB     $21,$38,$7F,$F3,$CE,$44,$77,$00,$7B,$67,$4B,$57,$94,$51,$97,$5C
        DB     $1E,$18,$23,$76,$90,$C7,$11,$AF,$01,$0F,$7F,$C8,$27,$43,$3E,$FE
        DB     $A7,$E3,$7F,$B7,$52,$95,$67,$7F,$E7,$7E,$78,$66,$C7,$80,$59,$2B
        DB     $23,$79,$00,$FE,$7F,$53,$DD,$00,$96,$D9,$E9,$7A,$C8,$59,$67,$02
        DB     $65,$55,$DC,$00,$6E,$38,$76,$63,$7F,$E7,$06,$0F,$39,$19,$46,$56
        DB     $A1,$23,$66,$91,$8C,$2B,$7A,$55,$00,$FE,$7F,$38,$04,$43,$8D,$F8
        DB     $7F,$92,$56,$DC,$E7,$67,$73,$66,$FE,$9D,$45,$98,$19,$DE,$8F,$48
        DB     $67,$99,$E5,$66,$87,$2D,$D3,$FF,$E8,$DD,$FD,$FF,$1D,$3D,$C9,$7F
        DB     $D2,$66,$8D,$E3,$5F,$6F,$66,$7F,$31,$EB,$BE,$27,$3C,$0C,$DB,$AD
        DB     $CF,$7F,$F8,$69,$3B,$FE,$F8,$B6,$7F,$EB,$59,$F0,$66,$F0,$7F,$8E
        DB     $8B,$63,$99,$CB,$18,$8F,$7F,$D8,$23,$43,$3D,$FE,$97,$DC,$FB,$FA
        DB     $4B,$F1,$5E,$66,$4E,$66,$7F,$A3,$FE,$D2,$99,$37,$7A,$3B,$97,$E8
        DB     $53,$20,$FE,$E4,$BD,$DC,$38,$3B,$FE,$1E,$63,$0F,$DD,$7F,$FA,$19
        DB     $DF,$A8,$1F,$7F,$B8,$AE,$D7,$FD,$86,$FF,$38,$F7,$F9,$7E,$66,$A5
        DB     $89,$8F,$29,$47,$66,$7F,$ED,$D7,$FE,$CB,$7F,$F0,$5F,$FA,$1A,$66
        DB     $36,$A7,$EA,$7B,$A9,$AF,$14,$7F,$DC,$8F,$FF,$91,$57,$7F,$EE,$56
        DB     $D8,$7F,$EA,$C9,$FE,$76,$9E,$94,$7F,$7F,$69,$44,$7F,$F2,$4E,$D5
        DB     $3E,$73,$7F,$1E,$03,$65,$FE,$D7,$7F,$F8,$0B,$FE,$F9,$3E,$7F,$B7
        DB     $73,$7F,$C4,$56,$DE,$A0,$BF,$7F,$4A,$FE,$FE,$4F,$7F,$CE,$D3,$A1
        DB     $FD,$7D,$BF,$7F,$29,$3A,$7F,$FC,$1C,$80,$9D,$FB,$E9,$8D,$2E,$1F
        DB     $7F,$D3,$F2,$A2,$47,$FD,$97,$94,$35,$7F,$F8,$A9,$38,$FF,$E9,$93
        DB     $7F,$FB,$8E,$33,$FB,$F4,$D2,$94,$80,$5A,$FC,$7F,$04,$04,$FF,$7C
        DB     $A8,$34,$CE,$9E,$53,$66,$A3,$45,$66,$43,$03,$AF,$18,$69,$2A,$64
        DB     $F6,$4F,$00,$44,$46,$64,$7E,$43,$7F,$E4,$FF,$7C,$0C,$36,$FF,$D2
        DB     $57,$76,$FF,$3E,$2D,$8F,$77,$97,$AD,$0F,$7F,$DB,$CF,$FE,$81,$66
        DB     $E5,$9D,$FB,$7F,$EB,$8F,$C1,$7F,$F9,$F3,$FF,$F2,$48,$E5,$65,$97
        DB     $FE,$7C,$A8,$25,$E9,$81,$45,$56,$53,$FE,$2E,$F2,$EA,$CC,$AD,$FC
        DB     $11,$FF,$FB,$7E,$7F,$E6,$34,$46,$50,$70,$7F,$44,$27,$56,$55,$EA
        DB     $D5,$66,$FE,$3F,$0F,$7F,$F0,$32,$FF,$EF,$FE,$29,$FE,$DD,$53,$99
        DB     $B4,$FE,$A6,$F9,$7C,$72,$FF,$F9,$68,$AE,$FF,$FC,$18,$FE,$92,$65
        DB     $FE,$47,$FF,$29,$34,$1A,$55,$FE,$1E,$84,$54,$43,$FF,$7E,$E9,$33
        DB     $8E,$FF,$CF,$17,$7F,$B7,$65,$36,$E9,$CD,$80,$04,$75,$53,$98,$7B
        DB     $7C,$01,$11,$A4,$5B,$FF,$E4,$F7,$FF,$C9,$7F,$EB,$65,$37,$55,$65
        DB     $7F,$90,$44,$55,$67,$55,$8F,$98,$14,$65,$FE,$7C,$98,$93,$E1,$7F
        DB     $F8,$6F,$8A,$7F,$BF,$CF,$2A,$FC,$B6,$65,$55,$8E,$85,$24,$80,$18
        DB     $65,$55,$53,$A5,$CA,$55,$67,$8F,$FE,$92,$AF,$47,$44,$7F,$E4,$AB
        DB     $38,$80,$E2,$CD,$7F,$CD,$72,$95,$FD,$A7,$53,$85,$28,$06,$88,$C9
        DB     $80,$36,$E1,$EA,$19,$25,$C7,$67,$44,$00,$C3,$94,$4E,$1B,$FF,$F8
        DB     $56,$80,$F2,$67,$62,$01,$8C,$E3,$8C,$44,$7E,$52,$64,$D4,$CD,$89
        DB     $0A,$BC,$EC,$E0,$55,$8D,$8E,$BB,$9A,$00,$E0,$CF,$30,$7F,$DE,$F3
        DB     $00,$A7,$22,$F5,$71,$7F,$55,$7F,$62,$ED,$C4,$80,$45,$E6,$18,$66
        DB     $CE,$0C,$0B,$BC,$01,$FC,$7E,$3F,$7F,$11,$38,$80,$5F,$14,$CD,$BA
        DB     $39,$43,$0F,$CB,$04,$7F,$0E,$C7,$00,$67,$EA,$7E,$00,$37,$33,$7F
        DB     $E4,$DF,$80,$CD,$01,$FC,$37,$7F,$E1,$DB,$80,$83,$E3,$7F,$FB,$6B
        DB     $80,$6D,$EE,$7F,$FD,$AD,$80,$38,$3F,$EF,$7F,$B3,$52,$00,$AD,$3A
        DB     $8A,$7F,$7F,$F3,$7F,$6B,$80,$3E,$0F,$FF,$7F,$EC,$E7,$80,$E9,$3A
        DB     $3F,$FF,$00,$FE,$7F,$7E,$80,$7E,$64,$FF,$96,$FF,$03,$7F,$F8,$53
        DB     $80,$A4,$39,$9F,$E9,$7C,$51,$FE,$FC,$17,$7F,$E2,$4A,$80,$BA,$99
        DB     $6A,$8F,$EA,$3E,$7E,$EB,$95,$F6,$FF,$7F,$1E,$80,$5E,$28,$FE,$FE
        DB     $0F,$7F,$F1,$2F,$81,$65,$80,$C1,$FF,$FE,$1F,$C6,$7E,$7F,$2F,$82
        DB     $24,$80,$B8,$BD,$FF,$7E,$96,$FF,$FF,$FC,$FC,$80,$AE,$29,$11,$9A
        DB     $FF,$F0,$E6,$3F,$F4,$0D,$FE,$7F,$7D,$04,$29,$83,$38,$AF,$FF,$00
        DB     $E3,$63,$F4,$FF,$F4,$14,$81,$CE,$83,$24,$80,$7C,$87,$34,$22,$00
        DB     $EE,$7E,$7F,$33,$04,$83,$12,$80,$3E,$41,$33,$40,$00,$00,$FD,$22
        DB     $44,$80,$3E,$5F,$7E,$E9,$E5,$80,$63,$80,$E2,$3F,$33,$30,$7E,$A6
        DB     $96,$80,$23,$80,$5E,$AA,$7D,$43,$AF,$33,$34,$7E,$FA,$88,$80,$EC
        DB     $38,$AA,$4B,$AA,$83,$23,$FA,$AA,$F3,$7E,$FA,$C8,$80,$E7,$88,$16
        DB     $83,$4C,$00,$63,$AF,$08,$44,$7E,$FA,$C8,$80,$EA,$83,$53,$00,$83
        DB     $34,$44,$7E,$FA,$F8,$80,$EF,$01,$BF,$7E,$EC,$2D,$85,$9E,$01,$8F
        DB     $40,$00,$00,$ED,$63,$FB,$FA,$2F,$00,$FF,$F8
;
        ENDIF
;
        IF     VDP = V9958
        CALL    $17a
        RLA
        RET     c

        LD      a,$80
        CALL    $17d
; MSX2+ logo version

        LD      de,$c000
        LD      hl,msx2logodata_1
        CALL    unPack_1

        LD      hl,0
        LD      (BAKCLR),hl
        LD      a,5
        CALL    $5f

        LD      hl,palette1_1
        CALL    setpalette_1

        CALL    $41

        LD      hl,256
        LD      (BAKCLR),hl
        CALL    $62

        LD      a,(RG8SAV+1)
        AND     127
        LD      b,a
        LD      c,9
        CALL    $47

wait_ce_logo_1:
        LD      a,2
        LD      ix,$131
        CALL    $15f
        BIT     0,a
        JR      nz,wait_ce_logo_1

        PUSH    de
        LD      bc,15
        LD      hl,logo_hmmc_1
        LDIR

        POP     hl
        LD      bc,13
        ADD     hl,bc
        LD      de,$c000
        LD      a,(de)
        INC     de
        LD      (hl),a
        EX      de,hl

        DI
        LD      a,32
        OUT     ($99),a
        LD      a,128+ 17
        OUT     ($99),a
        PUSH    hl
        LD      bc,15 *256+ $9b
        LD      hl,logo_hmmc_1
        OTIR
        POP     hl
        LD      a,128+ 44
        OUT     ($99),a
        LD      a,128+ 17
        OUT     ($99),a
        EI

        LD      b,255
        OTIR

loop_logo_1:
        LD      a,2
        LD      ix,$131
        CALL    $15f
        BIT     0,a
        JR      z,done_logo_1

        OTIR
        JR      loop_logo_1

done_logo_1:
        LD      bc,32
        LD      de,$c000
        LD      hl,palette1_1
        LDIR

        LD      hl,22 *8
        LD      (GRPACX),hl
        LD      hl,12 *8 +1
        LD      (GRPACY),hl
        LD      a,7
        LD      (FORCLR),a
        LD      a,8
        LD      (LOGOPR),a
        LD      hl,logo_ver_1
        CALL    prn_text_1
        LD      (LOGOPR),a

        CALL    $44

palette_loop_1:
        LD      b,16
        LD      de,palette2_1
        LD      hl,$c000
palette_color_1:
        LD      a,(de)                            ; change red
        AND     240
        LD      c,a
        LD      a,(hl)
        AND     240
        CP      c
        JR      z,palette_red_done_1
        JR      nc,palette_red_down_1
        ADD     a,16
        JR      palette_red_done_1
palette_red_down_1:
        SUB     16
palette_red_done_1:
        LD      c,a
        LD      a,(hl)
        AND     15
        OR      c
        LD      (hl),a

        LD      a,(de)
        AND     15
        LD      c,a
        LD      a,(hl)
        AND     15
        CP      c
        JR      z,palette_blue_done_1
        JR      nc,palette_blue_down_1
        INC     a
        JR      palette_blue_done_1
palette_blue_down_1:
        DEC     a
palette_blue_done_1:
        LD      c,a
        LD      a,(hl)
        AND     240
        OR      c
        LD      (hl),a
        INC     de
        INC     hl

        LD      a,(de)
        LD      c,a
        LD      a,(hl)
        CP      c
        JR      z,palette_green_done_1
        JR      nc,palette_green_down_1
        INC     a
        JR      palette_green_done_1
palette_green_down_1:
        DEC     a
palette_green_done_1:
        LD      (hl),a
        INC     de
        INC     hl
        DJNZ    palette_color_1

        LD      hl,$c000
        CALL    setpalette_1

        LD      b,6
palette_wait_1:
        HALT
        DJNZ    palette_wait_1

        LD      b,32
        LD      de,palette2_1
        LD      hl,$c000
palette_check_1:
        LD      a,(de)
        CP      (hl)
        JR      nz,palette_loop_1
        INC     de
        INC     hl
        DJNZ    palette_check_1

        LD      b,9
        LD      hl,glare_1
glare_loop_1:
        LD      e,(hl)
        INC     hl
        LD      d,(hl)
        INC     hl
        PUSH    bc
        PUSH    hl
        EX      de,hl
        CALL    setpalette_1
        POP     hl
        POP     bc

        HALT
        HALT

        DJNZ    glare_loop_1

        RET

setpalette_1:
        DI
        XOR     a
        OUT     ($99),a
        LD      a,128+ 16
        OUT     ($99),a
        LD      bc,32 *256+ $9a
        OTIR
        EI
        RET

prn_text_1:
        LD      a,(SCRMOD)
        CP      5
        JR      nc,prn_text_graph_1
prn_text_char_1:
        LD      a,(hl)
        OR      a
        RET     z
        CALL    $a2
        INC     hl
        JR      prn_text_char_1
prn_text_graph_1:
        LD      a,(hl)
        OR      a
        RET     z
        LD      ix,$0089
        CALL    $15f
        INC     hl
        JR      prn_text_graph_1

logo_hmmc_1:
        DW     0
        DW     0
        DW     0
        DW     31
        DW     256
        DW     85
col_1:
        DB     0
        DB     0
        DB     $f0

palette1_1:
        DW     $000,$327,$327,$327,$327,$327,$327,$327
        DW     $327,$327,$327,$327,$327,$327,$327,$327

palette2_1:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$563,$573,$572,$672,$772,$470,$270

palette3_1:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$563,$573,$572,$672,$772,$470,$777

palette4_1:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$563,$573,$572,$672,$772,$777,$270

palette5_1:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$563,$573,$572,$672,$777,$470,$270

palette6_1:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$563,$573,$572,$777,$772,$470,$270

palette7_1:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$563,$573,$777,$672,$772,$470,$270

palette8_1:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$563,$777,$572,$672,$772,$470,$270

palette9_1:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $777,$563,$573,$572,$672,$772,$470,$270

palette10_1:
        DW     $000,$327,$117,$000,$111,$333,$555,$777
        DW     $674,$777,$573,$572,$672,$772,$470,$270

glare_1:
        DW     palette3_1,palette4_1,palette5_1,palette6_1
        DW     palette7_1,palette8_1,palette9_1,palette10_1,palette2_1
;
logo_ver_1:
        DB     "V"
        DB     "N8VEM 1.24"
        DB     0

;
; Bitbuster by Team Bomba
;

;
; In: HL = source
;     DE = destination
;
unPack_1:
        EXX
        LD      bc,128                            ; b' = 0 (register loading optimize)
                                                  ; c' = bits from bitstream
        EXX

unPack_loop_1:
        EXX
        CALL    getBit_1
        EXX
        JR      c,unPack_outCompress_1            ; if set, we got LZ77 compression

unPack_outLiteral_1:
        LDI                                       ; copy byte from compressed data to destination
        JR      unPack_loop_1                     ; handle more compressed data

unPack_outCompress_1:
        LD      a,(hl)                            ; get lowest 7 bits of offset, plus the offset
                                                  ; extension bit
        INC     hl

        OR      a
        JR      z,unPack_outRle_1                 ; offset = 0, RLE compression used

unPack_outMatch_1:
        EXX
        LD      e,a
        LD      d,b                               ; b' should be always clear when entering this part
        RLCA                                      ; offset extension bit set?
        JR      nc,unPack_outMatch1_1             ; no need to get extra bits if carry not set

        CALL    getBit_1                          ; get offset bit 10
        RL      d
        CALL    getBit_1                          ; get offset bit 9
        RL      d
        CALL    getBit_1                          ; get offset bit 8
        RL      d
        CALL    getBit_1                          ; get offset bit 7

        JR      c,unPack_outMatch1_1              ; since extension mark already makes bit 7 set
        RES     7,e                               ; only clear it if the bit should be cleared
unPack_outMatch1_1:
        INC     de
        CALL    getGammaValue_0_1                 ; get the match length
; HL' = length

        PUSH    hl                                ; save compressed data pointer
        EXX
        PUSH    hl                                ; save match length
        PUSH    de                                ; save match offset
        EXX

        LD      h,d                               ; destination in HL
        LD      l,e
        POP     bc                                ; load match offset length
        SBC     hl,bc                             ; calculate source address
        POP     bc                                ; load match length
        LDIR

        POP     hl                                ; load compressed data pointer
        JR      unPack_loop_1

unPack_outRle_1:
        CALL    getGammaValue_1
        RET     c                                 ; HL' = repeat length

        PUSH    hl                                ; save compressed data pointer
        EXX
        PUSH    hl                                ; save repeat length
        EXX
        POP     bc                                ; load repeat length

        LD      h,d                               ; source = destination - 1
        LD      l,e
        DEC     hl
        LDIR

        POP     hl                                ; load compressed data pointer
        JR      unPack_loop_1
;
getBit_1:
        SLA     c                                 ; shift out new bit
        RET     nz                                ; if remaining value != 0, we're done

        EXX
        LD      a,(hl)                            ; get 8 bits from the compressed stream
        INC     hl
        EXX

        LD      c,a                               ; 8 bits in C'
        SLA     c                                 ; shift out new bit
        INC     c                                 ; set bit 0 so C' will be zero after shifting 8 times
        RET
;
getGammaValue_1:
        EXX                                       ; get number of bits used to encode value
getGammaValue_0_1:
        LD      hl,1                              ; initial length
        LD      b,1                               ; bitcount

getGammaValue_size_1:
        CALL    getBit_1                          ; get more bits
        JR      nc,getGammaValue_sizeEnd_1        ; if bit is not set, bit length is known
        INC     b                                 ; increase bitcount
        JR      getGammaValue_size_1

getGammaValue_bits_1:
        CALL    getBit_1                          ; get next bit of value from the compressed stream
        ADC     hl,hl                             ; insert new bit in HL
getGammaValue_sizeEnd_1:
        DJNZ    getGammaValue_bits_1              ; repeat if more bits to go

getGammaValue_end_1:
        INC     hl                                ; correct HL (was stored as length - 2)
        EXX
        RET
;
msx2logodata_1:
        DB     $7E,$00,$00,$01,$04,$33,$00,$C1,$40,$00,$00,$FD,$A2,$43,$79,$36
        DB     $A9,$99,$00,$07,$7F,$34,$7D,$63,$FB,$10,$3A,$99,$9A,$6A,$BB,$00
        DB     $BA,$85,$24,$93,$83,$BF,$61,$7E,$0F,$AA,$AA,$BB,$00,$02,$AA,$A9
        DB     $80,$3F,$AE,$C7,$7E,$AB,$7E,$8C,$83,$AC,$80,$7F,$59,$7E,$EF,$80
        DB     $1F,$D7,$67,$7E,$7D,$AC,$80,$7F,$5B,$7E,$E2,$AA,$D9,$00,$89,$6A
        DB     $80,$2B,$85,$7E,$98,$05,$43,$E8,$72,$BA,$AA,$B5,$FD,$57,$89,$88
        DB     $63,$BA,$93,$50,$7A,$F9,$FC,$EF,$66,$F6,$D9,$B6,$F7,$61,$0D,$38
        DB     $98,$9A,$80,$1D,$4D,$81,$00,$FB,$1F,$7F,$96,$39,$BF,$F6,$0E,$3C
        DB     $91,$88,$80,$D2,$A9,$8E,$37,$01,$F5,$5F,$20,$7F,$94,$99,$F6,$7E
        DB     $8D,$80,$9E,$90,$26,$8E,$FE,$7F,$8C,$39,$9B,$E1,$7F,$B9,$6C,$E6
        DB     $88,$97,$80,$DF,$FC,$7F,$A4,$99,$EC,$F3,$EB,$1E,$82,$38,$8A,$BB
        DB     $80,$5F,$77,$7F,$F2,$63,$FE,$B8,$EA,$F5,$96,$80,$66,$93,$7F,$93
        DB     $7F,$41,$FE,$1F,$1C,$80,$AE,$8A,$31,$16,$EF,$7F,$FC,$8C,$FE,$7C
        DB     $B3,$01,$89,$89,$F8,$9F,$01,$93,$7F,$CF,$1C,$99,$CC,$00,$57,$E9
        DB     $EC,$FE,$7F,$E9,$39,$9C,$7E,$C1,$C9,$93,$7F,$FD,$B4,$FE,$7C,$2B
        DB     $66,$00,$4E,$E1,$7C,$36,$14,$7F,$7F,$C1,$B9,$9C,$7F,$8F,$FE,$21
        DB     $57,$77,$76,$4E,$F0,$BE,$35,$14,$3F,$7F,$E0,$A5,$FE,$E2,$7C,$56
        DB     $7F,$8F,$67,$7F,$E5,$07,$04,$3C,$CC,$7F,$28,$33,$FF,$F3,$7C,$66
        DB     $7F,$47,$55,$56,$7F,$F2,$64,$43,$CC,$FE,$FC,$11,$E9,$14,$77,$63
        DB     $FF,$FD,$39,$04,$33,$FE,$1D,$3F,$93,$7F,$B5,$3A,$03,$EE,$00,$7F
        DB     $33,$7F,$6B,$FE,$2F,$17,$7F,$F5,$94,$FD,$D5,$FD,$B9,$F5,$7F,$EF
        DB     $79,$01,$C4,$8A,$77,$09,$E7,$01,$CE,$B0,$3A,$7A,$01,$3F,$63,$7F
        DB     $21,$38,$7F,$F3,$CE,$44,$77,$00,$7B,$67,$4B,$57,$94,$51,$97,$5C
        DB     $1E,$18,$23,$76,$90,$C7,$11,$AF,$01,$0F,$7F,$C8,$27,$43,$3E,$FE
        DB     $A7,$E3,$7F,$B7,$52,$95,$67,$7F,$E7,$7E,$78,$66,$C7,$80,$59,$2B
        DB     $23,$79,$00,$FE,$7F,$53,$DD,$00,$96,$D9,$E9,$7A,$C8,$59,$67,$02
        DB     $65,$55,$DC,$00,$6E,$38,$76,$63,$7F,$E7,$06,$0F,$39,$19,$46,$56
        DB     $A1,$23,$66,$91,$8C,$2B,$7A,$55,$00,$FE,$7F,$38,$04,$43,$8D,$F8
        DB     $7F,$92,$56,$DC,$E7,$67,$73,$66,$FE,$9D,$45,$98,$19,$DE,$8F,$48
        DB     $67,$99,$E5,$66,$87,$2D,$D3,$FF,$E8,$DD,$FD,$FF,$1D,$3D,$C9,$7F
        DB     $D2,$66,$8D,$E3,$5F,$6F,$66,$7F,$31,$EB,$BE,$27,$3C,$0C,$DB,$AD
        DB     $CF,$7F,$F8,$69,$3B,$FE,$F8,$B6,$7F,$EB,$59,$F0,$66,$F0,$7F,$8E
        DB     $8B,$63,$99,$CB,$18,$8F,$7F,$D8,$23,$43,$3D,$FE,$97,$DC,$FB,$FA
        DB     $4B,$F1,$5E,$66,$4E,$66,$7F,$A3,$FE,$D2,$99,$37,$7A,$3B,$97,$E8
        DB     $53,$20,$FE,$E4,$BD,$DC,$38,$3B,$FE,$1E,$63,$0F,$DD,$7F,$FA,$19
        DB     $DF,$A8,$1F,$7F,$B8,$AE,$D7,$FD,$86,$FF,$38,$F7,$F9,$7E,$66,$A5
        DB     $89,$8F,$29,$47,$66,$7F,$ED,$D7,$FE,$CB,$7F,$F0,$5F,$FA,$1A,$66
        DB     $36,$A7,$EA,$7B,$A9,$AF,$14,$7F,$DC,$8F,$FF,$91,$57,$7F,$EE,$56
        DB     $D8,$7F,$EA,$C9,$FE,$76,$9E,$94,$7F,$7F,$69,$44,$7F,$F2,$4E,$D5
        DB     $3E,$73,$7F,$1E,$03,$65,$FE,$D7,$7F,$F8,$0B,$FE,$F9,$3E,$7F,$B7
        DB     $73,$7F,$C4,$56,$DE,$A0,$BF,$7F,$4A,$FE,$FE,$4F,$7F,$CE,$D3,$A1
        DB     $FD,$7D,$BF,$7F,$29,$3A,$7F,$FC,$1C,$80,$9D,$FB,$E9,$8D,$2E,$1F
        DB     $7F,$D3,$F2,$A2,$47,$FD,$97,$94,$35,$7F,$F8,$A9,$38,$FF,$E9,$93
        DB     $7F,$FB,$8E,$33,$FB,$F4,$D2,$94,$80,$5A,$FC,$7F,$04,$04,$FF,$7C
        DB     $A8,$34,$CE,$9E,$53,$66,$A3,$45,$66,$43,$03,$AF,$18,$69,$2A,$64
        DB     $F6,$4F,$00,$44,$46,$64,$7E,$43,$7F,$E4,$FF,$7C,$0C,$36,$FF,$D2
        DB     $57,$76,$FF,$3E,$2D,$8F,$77,$97,$AD,$0F,$7F,$DB,$CF,$FE,$81,$66
        DB     $E5,$9D,$FB,$7F,$EB,$8F,$C1,$7F,$F9,$F3,$FF,$F2,$48,$E5,$65,$97
        DB     $FE,$7C,$A8,$25,$E9,$81,$45,$56,$53,$FE,$2E,$F2,$EA,$CC,$AD,$FC
        DB     $11,$FF,$FB,$7E,$7F,$E6,$34,$46,$50,$70,$7F,$44,$27,$56,$55,$EA
        DB     $D5,$66,$FE,$3F,$0F,$7F,$F0,$32,$FF,$EF,$FE,$29,$FE,$DD,$53,$99
        DB     $B4,$FE,$A6,$F9,$7C,$72,$FF,$F9,$68,$AE,$FF,$FC,$18,$FE,$92,$65
        DB     $FE,$47,$FF,$29,$34,$1A,$55,$FE,$1E,$84,$54,$43,$FF,$7E,$E9,$33
        DB     $8E,$FF,$CF,$17,$7F,$B7,$65,$36,$E9,$CD,$80,$04,$75,$53,$98,$7B
        DB     $7C,$01,$11,$A4,$5B,$FF,$E4,$F7,$FF,$C9,$7F,$EB,$65,$37,$55,$65
        DB     $7F,$90,$44,$55,$67,$55,$8F,$98,$14,$65,$FE,$7C,$98,$93,$E1,$7F
        DB     $F8,$6F,$8A,$7F,$BF,$CF,$2A,$FC,$B6,$65,$55,$8E,$85,$24,$80,$18
        DB     $65,$55,$53,$A5,$CA,$55,$67,$8F,$FE,$92,$AF,$47,$44,$7F,$E4,$AB
        DB     $38,$80,$E2,$CD,$7F,$CD,$72,$95,$FD,$A7,$53,$85,$28,$06,$88,$C9
        DB     $80,$36,$E1,$EA,$19,$25,$C7,$67,$44,$00,$C3,$94,$4E,$1B,$FF,$F8
        DB     $56,$80,$F2,$67,$62,$01,$8C,$E3,$8C,$44,$7E,$52,$64,$D4,$CD,$89
        DB     $0A,$BC,$EC,$E0,$55,$8D,$8E,$BB,$9A,$00,$E0,$CF,$30,$7F,$DE,$F3
        DB     $00,$A7,$22,$F5,$71,$7F,$55,$7F,$62,$ED,$C4,$80,$45,$E6,$18,$66
        DB     $CE,$0C,$0B,$BC,$01,$FC,$7E,$3F,$7F,$11,$38,$80,$5F,$14,$CD,$BA
        DB     $39,$43,$0F,$CB,$04,$7F,$0E,$C7,$00,$67,$EA,$7E,$00,$37,$33,$7F
        DB     $E4,$DF,$80,$CD,$01,$FC,$37,$7F,$E1,$DB,$80,$83,$E3,$7F,$FB,$6B
        DB     $80,$6D,$EE,$7F,$FD,$AD,$80,$38,$3F,$EF,$7F,$B3,$52,$00,$AD,$3A
        DB     $8A,$7F,$7F,$F3,$7F,$6B,$80,$3E,$0F,$FF,$7F,$EC,$E7,$80,$E9,$3A
        DB     $3F,$FF,$00,$FE,$7F,$7E,$80,$7E,$64,$FF,$96,$FF,$03,$7F,$F8,$53
        DB     $80,$A4,$39,$9F,$E9,$7C,$51,$FE,$FC,$17,$7F,$E2,$4A,$80,$BA,$99
        DB     $6A,$8F,$EA,$3E,$7E,$EB,$95,$F6,$FF,$7F,$1E,$80,$5E,$28,$FE,$FE
        DB     $0F,$7F,$F1,$2F,$81,$65,$80,$C1,$FF,$FE,$1F,$C6,$7E,$7F,$2F,$82
        DB     $24,$80,$B8,$BD,$FF,$7E,$96,$FF,$FF,$FC,$FC,$80,$AE,$29,$11,$9A
        DB     $FF,$F0,$E6,$3F,$F4,$0D,$FE,$7F,$7D,$04,$29,$83,$38,$AF,$FF,$00
        DB     $E3,$63,$F4,$FF,$F4,$14,$81,$CE,$83,$24,$80,$7C,$87,$34,$22,$00
        DB     $EE,$7E,$7F,$33,$04,$83,$12,$80,$3E,$41,$33,$40,$00,$00,$FD,$22
        DB     $44,$80,$3E,$5F,$7E,$E9,$E5,$80,$63,$80,$E2,$3F,$33,$30,$7E,$A6
        DB     $96,$80,$23,$80,$5E,$AA,$7D,$43,$AF,$33,$34,$7E,$FA,$88,$80,$EC
        DB     $38,$AA,$4B,$AA,$83,$23,$FA,$AA,$F3,$7E,$FA,$C8,$80,$E7,$88,$16
        DB     $83,$4C,$00,$63,$AF,$08,$44,$7E,$FA,$C8,$80,$EA,$83,$53,$00,$83
        DB     $34,$44,$7E,$FA,$F8,$80,$EF,$01,$BF,$7E,$EC,$2D,$85,$9E,$01,$8F
        DB     $40,$00,$00,$ED,$63,$FB,$FA,$2F,$00,$FF,$F8
;
        ENDIF
;
        DS     $c000 - $
        DB     $ff

;_CBIOS_N8VEM______________________________________________________________________________________________________
;
;
;  The following code is a heavily modified version of the C-BIOS ROM. (See below disclaimer)
;  It has been modified for use with the N8VEM, and the N8VEM HC series of computers.   Strict MSX1 compatibility
;  is no longer maintained in this version, but, it aims to be as compatible as possible within the contraints
;  of the N8VEM hardware.   Extra features not supported by the N8VEM hardware were removed to conserve ROM space
;  and improve maintainability of the system.
;
;  Some features have been added to further support the N8VEM system
;
;  Conversion done by:   Dan Werner	11/1/2010
;__________________________________________________________________________________________________________________
;
;
;
; $Id: main.asm 570 2010-05-24 11:36:23Z bifimsx $
; C-BIOS main ROM
;
; Copyright (c) 2002-2005 BouKiCHi.  All rights reserved.
; Copyright (c) 2003 Reikan.  All rights reserved.
; Copyright (c) 2004-2005 Maarten ter Huurne.  All rights reserved.
; Copyright (c) 2004-2009 Albert Beevendorp.  All rights reserved.
; Copyright (c) 2004 Manuel Bilderbeek.  All rights reserved.
; Copyright (c) 2004-2005 Joost Yervante Damad.  All rights reserved.
; Copyright (c) 2004-2005 Jussi Pitkänen.  All rights reserved.
; Copyright (c) 2006-2007 Eric Boon.  All rights reserved.
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
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONS.equENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOO.ds OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;

                .include "systemva.asm"
                .include "hooks.asm"

;-----------------
; A memory address for debug
;-----------------

COMPILE_FONT:   .equ     YES

;---------------------
; Jump table
;---------------------

; $0000 CHKRAM
                .org     $0000
                di
                jp      chkram

; Pointer to font
; $0004 CGTABL  Base address of the MSX character set in ROM
                .ds      $0004 - $
                .dw      B_Font

                .ds      $0006 - $

; $0006 VDP.DR  Base port address for VDP data read
vdp_dr:         .db      VDP_DATA        ; VDP read port
; $0007 VDP.WR  Base port address for VDP data write
vdp_dw:         .db      VDP_DATA        ; VDP write port

; $0008 SYNCHR
                .ds      $0008 - $
                jp      synchr

; $000C RDSLT   Read memory from an optional slot
                .ds      $000C - $
                jp      rdslt

; $0010 CHRGTR
                .ds      $0010 - $
                jp      chrgtr

; $0014 WRSLT   Write memory to an optional slot
                .ds      $0014 - $
                jp      wrslt

; $0018 OUTDO
                .ds      $0018 - $
                jp      outdo

; $001C CALSLT   inter slot call routine
                .ds      $001C - $
                jp      calslt

; $0020 DCOMPR  Compare HL to DE
                .ds      $0020 - $
                jp      dcompr

; $0024 ENASLT  Change slot
                .ds      $0024 - $
                jp      enaslt

; $0028 GETYPR
                .ds      $0028 - $
                jp      getypr

; $002B I.dbYT1
                .ds      $002B - $
i.dbyt1:
; Basic ROM version
; 7 6 5 4 3 2 1 0
; | | | | +-+-+-+-- Character set
; | | | |           0 = Japanese, 1 = International (ASCII), 2=Korean
; | +-+-+---------- Date format
; |                 0 = Y-M-D, 1 = M-D-Y, 2 = D-M-Y
; +---------------- Default interrupt fr.equency
;                   0 = 60Hz, 1 = 50Hz
                .db      LOCALE_CHSET + LOCALE_DATE + LOCALE_INT
; $002C I.dbYT2
i.dbyt2:
; Basic ROM version
; 7 6 5 4 3 2 1 0
; | | | | +-+-+-+-- Keyboard type
; | | | |           0 = Japanese, 1 = International (QWERTY)
; | | | |           2 = French (AZERTY), 3 = UK, 4 = German (DIN)
; +-+-+-+---------- Basic version
;                   0 = Japanese, 1 = International
                .db      LOCALE_KBD + LOCALE_BASIC

; $002D Version ID
romid:
                .ds      $002D - $
; version ID
; MSX version number
;  0 = MSX 1
;  1 = MSX 2
;  2 = MSX 2+
;  3 = MSX turbo R
                .db      0
                .db      0


; Reserved
                .db      0

; $0030 CALLF    Call interslot routine(RST30h version)
                .ds      $0030 - $
                jp      callf

; $0038 KEYINT   Interrupt routines(RST38,VBlank,Timer...)
                .ds      $0038 - $
                jp      keyint

; $003B INITIO  Initialize I/O
                .ds      $003B - $
                jp      initio

; $003E INIFNK
                .ds      $003E - $
                jp      inifnk

; $0041 DISSCR  Disable screen display
                .ds      $0041 - $
                jp      disscr

; $0044 ENASCR  Enable screen display
                .ds      $0044 - $
                jp      enascr

;---------------
;VDP routines
;---------------

; $0047 WRTVDP
                .ds      $0047 - $
                jp      wrtvdp

; $004A RDVRM
                .ds      $004A - $
                jp      rdvrm

; $004D WRTVRM
                .ds      $004D - $
                jp      wrtvrm

; $0050 SETRD
                .ds      $0050 - $
                jp      setrd

; $0053 SETWRT  Set VRAM Write Address
                .ds      $0053 - $
                jp      setwrt

; $0056 FILVRM  Fill VRAM
                .ds      $0056 - $
                jp      filvrm

; $0059 LDIRMV  Copy VRAM to RAM
                .ds      $0059 - $
                jp      ldirmv

; $005C LDIRVM  Copy RAM to VRAM
                .ds      $005C - $
                jp      ldirvm

; $005F CHGMOD Change VDP screen mode
                .ds      $005F - $
                jp      chgmod

; $0062 CHGCLR
                .ds      $0062 - $
                jp      chgclr

; $0066 NMI     Non-maskable interrupt
                .ds      $0066 - $
                jp      nmi

; $0069 CLRSPR  Clear sprites
                .ds      $0069 - $
                jp      clrspr

; $006C INITXT  Initialize display to mode TEXT1    (SCREEN 0)
                .ds      $006C - $
                jp      initxt

; $006F INIT32  Initialize display to mode GRAPHIC1 (SCREEN 1)
                .ds      $006F - $
                jp      init32

; $0072 INITGRP Initialize display to mode GRAPHIC2 (SCREEN 2)
                .ds      $0072 - $
                jp      inigrp

; $0075 INIMLT  Initialize display to mode MULTI    (SCREEN 3)
                .ds      $0075 - $
                jp      inimlt

; $0078 SETTXT
                .ds      $0078 - $
                jp      settxt

; $007B SETT32
                .ds      $007B - $
                jp      sett32

; $007E SETGRP
                .ds      $007E - $
                jp      setgrp

; $0081 SETMLT
                .ds      $0081 - $
                jp      setmlt

; $0084 CALPAT
                .ds      $0084 - $
                jp      calpat

; $0087 CALATR
                .ds      $0087 - $
                jp      calatr

; $008A GSPSIZ
                .ds      $008A - $
                jp      gspsiz

; $008D GRPPRT
                .ds      $008D - $
                jp      grpprt

; $0090 GICINI  initialize sound IC
                .ds      $0090 - $
                jp      gicini
; $0093 WRTPSG
                .ds      $0093 - $
                jp      wrtpsg
; $0096 RDPSG
                .ds      $0096 - $
                jp      rdpsg

; $0099 STRTMS
                .ds      $0099 - $
                jp      strtms

; $009C CHSNS  .. check key buffer
                .ds      $009C - $
                jp      chsns

; $009F CHGET .. Get data from keyboard buffer
                .ds      $009F - $
                jp      chget

; $00A2 CHPUT .. Output charactor to display
                .ds      $00A2 - $
                jp      chput

; $00A5 LPTOUT
                .ds      $00A5 - $
                jp      lptout

; $00A8 LPTSTT
                .ds      $00A8 - $
                jp      lptstt

; $00AB CNVCHR
                .ds      $00AB - $
                jp      cnvchr

; $00AE PINLIN
                .ds      $00AE - $
                jp      pinlin

; $00B1 INLIN
                .ds      $00B1 - $
                jp      inlin

; $00B4 QINLIN
                .ds      $00B4 - $
                jp      qinlin

; $00B7 BREAKX
                .ds      $00B7 - $
                jp      breakx

; $00BA ISCNTC
                .ds      $00BA - $
                jp      iscntc

; $00BD CKCNTC
                .ds      $00BD - $
                jp      ckcntc

; $00C0 BEEP
                .ds      $00C0 - $
                jp      beep

; $00C3 CLS
                .ds      $00C3 - $
                jp      cls_z

; $00C6 POSIT
                .ds      $00C6 - $
                jp      posit

; $00C9 FNKSB
                .ds      $00C9 - $
                jp      fnksb

; $00CC ERAFNK
                .ds      $00CC - $
                jp      erafnk

; $00CF DSPFNK
                .ds      $00CF - $
                jp      dspfnk

; $00D2 TOTEXT
                .ds      $00D2 - $
                jp      totext

; $00D5 GTSTCK .. Get joystick infomation
                .ds      $00D5 - $
                jp      gtstck

; $00D8 GTTRIG .. Get trigger infomation
                .ds      $00D8 - $
                jp      gttrig

; $00db GTPAD
                .ds      $00db - $
                jp      gtpad

; $00DE GTPDL
                .ds      $00DE - $
                jp      gtpdl

; $00E1 TAPION
                .ds      $00E1 - $
                jp      tapion

; $00E4 TAPIN
                .ds      $00E4 - $
                jp      tapin

; $00E7 TAPIOF
                .ds      $00E7 - $
                jp      tapiof

; $00EA TAPOON
                .ds      $00EA - $
                jp      tapoon

; $00ED TAPOUT
                .ds      $00ED - $
                jp      tapout

; $00F0 TAPOOF
                .ds      $00F0 - $
                jp      tapoof

; $00F3 STMOTR
                .ds      $00F3 - $
                jp      stmotr

; $00F6 LFTQ
                .ds      $00F6 - $
                jp      lftq

; $00F9 PUTQ
                .ds      $00F9 - $
                jp      putq

; $00FC RIGHTC
                .ds      $00FC - $
                jp      rightc

; $00FF LEFTC
                .ds      $00FF - $
                jp      leftc

; $0102 UPC
                .ds      $0102 - $
                jp      upc

; $0105 TUPC
                .ds      $0105 - $
                jp      tupc

; $0108 DOWNC
                .ds      $0108 - $
                jp      downc

; $010B TDOWNC
                .ds      $010B - $
                jp      tdownc

; $010E SCALXY
                .ds      $010E - $
                jp      scalxy

; $0111 MAPXY
                .ds      $0111 - $
                jp      mapxy

; $0114 FETCHC
                .ds      $0114 - $
                jp      fetchc

; $0117 STOREC
                .ds      $0117 - $
                jp      storec

; $011A SETATR
                .ds      $011A - $
                jp      setatr

; $011D READC
                .ds      $011D - $
                jp      readc

; $0120 SETC
                .ds      $0120 - $
                jp      setc

; $0123 NSETCX
                .ds      $0123 - $
                jp      nsetcx

; $0126 GTASPC
                .ds      $0126 - $
                jp      gtaspc

; $0129 PNTINI
                .ds      $0129 - $
                jp      pntini

; $012C SCANR
                .ds      $012C - $
                jp      scanr

; $012F SCANL
                .ds      $012F - $
                jp      scanl

; $0132 CHGCAP
                .ds      $0132 - $
                jp      chgcap

; $0135 CHGSND
                .ds      $0135 - $
                jp      chgsnd

; $0138 RSLREG  Read infomation of primary slot
                .ds      $0138 - $
                jp      rslreg

; $013B WSLREG  Write infomation to primary slot
                .ds      $013B - $
                jp      wslreg

; $013E RDVDP   Read VDP status
                .ds      $013E - $
                jp      rdvdp

; $0141 SNSMAT  Get key matrix
                .ds      $0141 - $
                jp      snsmat

; $0144 PHYDIO
                .ds      $0144 - $
                jp      phydio

; $0147 FORMAT
                .ds      $0147 - $
                jp      format

; $014A ISFLIO
                .ds      $014A - $
                jp      isflio

; $014D OUTDLP
                .ds      $014D - $
                jp      outdlp

; $0150 GETVCP
                .ds      $0150 - $
                jp      getvcp

; $0153 GETVC2
                .ds      $0153 - $
                jp      getvc2

; $0156 KILBUF  Clear keyboard buffer
                .ds      $0156 - $
                jp      kilbuf

; $0159 CALBAS  Call BASIC interpreter
                .ds      $0159 - $
                jp      calbas


; fake EXTROM call, fixes Nemesis 3 reset bug
                .ds      $015f - $
                ret

                .ds      $0200 - $

                .include "util.asm"
                .include "video.asm"

;
;	AROUND 800 BYTES FREE HERE
;
;

; The game "Hacker" jumps directly to this location($0D02).
; Star force calls $0D0E.

                .ds      $0D01 - $
; for all wrong jumper,put RET instruction there
                ret
                pop     ix  ; $0D02
                pop     iy
                pop     af
                pop     bc
                pop     de
                pop     hl
                exx
                ex      af,af'
                pop     af
                pop     bc
                pop     de
                pop     hl
                ei
                ret

; $0000 CHKRAM
; Function : Tests RAM and sets RAM slot for the system
; Registers: All
; Remark   : After this, a jump must be made to INIT, for further initialisation.
chkram:
		LD A,$FD
		OUT (VDP_ACR),A		; INITIALIZE ACR REGISTER FOR N8VEM HARDWARE

;----------------------
; User interface
;----------------------

                ld      hl,$F300
                ld      sp,hl           ; set $F300 to stack pointer

                call    init_ram

                call    init_vdp

                ei

                call    initio

                ld      a,15
                ld      (FORCLR),a
                ld      a,5
                ld      (BAKCLR),a
                ld      (BDRCLR),a
                ld      a,29
                ld      (LINL32),a
                call    init32

                ld      hl,str_proginfo
                call    prn_text

                call    search_roms
                call    H_STKE

                ; Set up hooks and system vars so NMS8250 disk ROM will try
                ; to load and execute the boot sector.
                ld      a,1
                ld      (DEVICE),a
                xor     a
                ; TODO: Find out or invent name for $FB29.
                ld      ($FB29),a

                ; This is the hook the disk ROM uses for booting.
                call    H_RUNC

                CALL	$006C
                CALL	$0078
                CALL	$00C3

                JP	STARTBASIC

;                ld      hl,str_nocart
;                call    prn_text

 ;               jp      hang_up_mode



;----------------------
; Search for any extension ROMs and initialize them.
search_roms:
                LD      DE,($8000)
                ld      hl,$4241        ; "AB"
                call    dcompr          ; ZF is set if the ROM is present.
                jp      z,search_roms_8k
                LD      DE,($4000)
                ld      hl,$4241        ; "AB"
                call    dcompr          ; ZF is set if the ROM is present.
                jp      z,search_roms_4k
                ret
search_roms_8k:
               LD      A,($8002)
               LD      L,A
               LD      A,($8003)
               LD      H,A
               JP      (HL)
search_roms_4k:
               LD      A,($4002)
               LD      L,A
               LD      A,($4003)
               LD      H,A
               JP      (HL)

;
;		LD	HL,$0000
;		LD	A,%00000101
;		CALL	rdslt
;		LD	E,A
;		LD	HL,$0001
;		LD	A,%00000101
;		CALL	rdslt
;		LD	D,A
;		CALL	search_roms_check
;		JP	Z,search_roms_FOUND_P1
;		LD	HL,$0000
;		LD	A,%00000110
;		CALL	rdslt
;		LD	E,A
;		LD	HL,$0001
;		LD	A,%00000110
;		CALL	rdslt
;		LD	D,A
;		CALL	search_roms_check
;		JP	Z,search_roms_FOUND_P2
;		RET
;s;earch_roms_check:
;		LD	($8500),DE
;                ld      hl,$4241        ; "AB"
; ;               call    dcompr          ; ZF is set if the ROM is present.
;                ret
;search_roms_FOUND_P1:;
;		LD	HL,$0000
;		LD	BC,$8000
;		LD	DE,$4000
;search_roms_FOUND_P1_LOOP:
;		LD	A,%00000101
;		CALL	rdslt
;		LD	(DE),A
;		INC	HL
;		INC	DE
;		DEC	BC
;		LD	A,B
;		CP	$00
;		JP	NZ,search_roms_FOUND_P1_LOOP
;		ADD	A,C
;		CP	$00
;		JP	NZ,search_roms_FOUND_P1_LOOP
;		LD	A,($4002)
;		LD	L,A
;		LD	A,($4003)
;		LD	H,A
;		JP	(HL)

;search_roms_FOUND_P2:
;		LD	HL,$0000
;		LD	BC,$4000
;		LD	DE,$8000
;search_roms_FOUND_P2_LOOP:
;		LD	A,%00000110
;		CALL	rdslt
;		LD	(DE),A
;		INC	HL
;		INC	DE
;		DEC	BC
;		LD	A,B
;		CP	$00
;		JP	NZ,search_roms_FOUND_P2_LOOP
;		ADD	A,C
;		CP	$00
;		JP	NZ,search_roms_FOUND_P2_LOOP
;		LD	A,($8002)
;		LD	L,A
;		LD	A,($8003)
;		LD	H,A
;		JP	(HL)

;------------------------
; Initialize RAM

init_ram:

; Initialize workarea
                ld      a,$00
                ld      hl,$F380
                ld      (hl),a
                ld      de,$F381
                ld      bc,$0C7D
                ldir

; Initialize Disk work
                ld      a,$C9
                ld      hl,$F300
                ld      (hl),a
                ld      de,$F301
                ld      bc,$007F
                ldir

; initialize hook area with $C9 (assembler code for ret)
                ld      a,$C9           ; ret code
                ld      hl,H_KEYI
                ld      (hl),a
                ld      de,H_KEYI+1
                ld      bc,$024D        ; shouldn't this be $0235 ?
                ldir

; Initialize key matrix
                ld      a,$FF
                ld      hl,OLDKEY
                ld      (hl),a
                ld      de,OLDKEY+1
                ld      bc,21
                ldir

; Initialize Key buffer
                ld      a,$00
                ld      hl,KEYBUF
                ld      (hl),a
                ld      de,KEYBUF+1
                ld      bc,39
                ldir


; Set address pointer
                ld      hl,KEYBUF
                ld      (PUTPNT),hl
                ld      (GETPNT),hl

;                ld      hl,$8000
                exx
                ld      (BOTTOM),hl     ; Page1 and 2 is ROM,Page3 and 4 is RAM.
                exx

                ; I don't know exactly what is stored between $F168 and $F380,
                ; but the disk ROM nee.ds some space there, so I'll just
                ; reserve all of it.
                ld      hl,$F380        ; was $F168, but nee.ds to be changed by disk ROM
                ld      (HIMEM),hl      ; limit of usable memory
                ld      (STKTOP),hl     ; position of BASIC stack


; Initialize table of screen 0
                ld      hl,$0000
                ld      (TXTNAM),hl
                ld      hl,$0800
                ld      (TXTCGP),hl

; Initialize table of screen 1
                ld      hl,$1800
                ld      (T32NAM),hl
                ld      hl,$2000
                ld      (T32COL),hl
                ld      hl,$0000
                ld      (T32CGP),hl
                ld      hl,$1B00
                ld      (T32ATR),hl
                ld      hl,$3800
                ld      (T32PAT),hl

; Initialize table of screen 2

                ld      hl,$1800
                ld      (GRPNAM),hl
                ld      hl,$2000
                ld      (GRPCOL),hl
                ld      hl,$0000
                ld      (GRPCGP),hl
                ld      hl,$1B00
                ld      (GRPATR),hl
                ld      hl,$3800
                ld      (GRPPAT),hl

; Initialize table fo screen 3
                ld      hl,$0800
                ld      (MLTNAM),hl
                ld      hl,$0000
                ld      (MLTCGP),hl
                ld      hl,$1B00
                ld      (MLTATR),hl
                ld      hl,$3800
                ld      (MLTPAT),hl

; Initialise QUETAB.
                ld      hl,QUETAB
                ld      (QUEUES),hl
                ld      hl,VOICAQ
                ld      ($FFFF &(QUETAB+0*6+4)),hl
                ld      hl,VOICBQ
                ld      ($FFFF &(QUETAB+1*6+4)),hl
                ld      hl,VOICCQ
                ld      ($FFFF &(QUETAB+2*6+4)),hl
                ld      a,$7F
                ld      ($FFFF &(QUETAB+0*6+3)),a
                ld      ($FFFF &(QUETAB+1*6+3)),a
                ld      ($FFFF &(QUETAB+2*6+3)),a

; other settings
                ld      a,39
                ld      (LINL40),a
                ld      a,32            ; Set to 29 after splash screen.
                ld      (LINL32),a
                ;TODO: Rely on call to INIT32 instead.
                ld      a,(LINL32)
                ld      (LINLEN),a
                ld      a,24
                ld      (CRTCNT),a

                ld      a,$04
                ld      (BDRCLR),a
                ld      (BAKCLR),a
                ld      a,$0F
                ld      (FORCLR),a

                ld      a,$A0
                ld      (RG1SAV),a

                ld      a,(EXPTBL)
                ld      (CGPNT),a
                ld      hl,(4)
                ld      (CGPNT+1),hl

; set up hook
                ld      a,$c3
                ld      hl,chput
                ld      (H_OUTD+1),hl
                ld      (H_OUTD),a

                ret


;------------------------
; wait routine
; caution,already EI when call the rouine
; B = frequency of loop
wait_b:
                halt
                djnz    wait_b
                ret

;------------------------
;prn_text
; HL = string with null termination

prn_text:
                ld      a,(SCRMOD)
                cp      5
                jr      nc,prn_text_graph
prn_text_char:
                ld      a,(hl)
                or      a
                ret     z
                call    chput
                inc     hl
                jr      prn_text_char
prn_text_graph:
                ld      a,(hl)
                or      a
                ret     z
                ld      ix,$0089
                call    extrom
                inc     hl
                jr      prn_text_graph

;--------------------------------
; Determine bytes per line in the current text mode.
; Input:   SCRMOD, LINLEN
; Output:  C = number of bytes per line
; Changes: AF
text_bytes_per_line:
                ld      c,32            ; text32
                ld      a,(SCRMOD)
                or      a
                ret     nz
                ld      c,40            ; text40
                ld      a,(LINLEN)
                cp      41
                ret     c
                ld      c,80            ; text80
                ret

;--------------------------------
; Calculate the VRAM address that correspon.ds to the current cursor position.
; Input:   CSRX, CSRY
; Output:  HL = VRAM address
; Changes: none
curs2hl:
                push    bc
                push    af

                call    text_bytes_per_line

                ; Calculate left border.
                ld      a,(LINLEN)
                neg
                add     a,c             ; A = bytes_per_line - LINLEN
                inc     a               ; round up
                srl     a               ; A = A / 2
                ld      l,a             ; L = size of left border

                ; Add X coordinate.
                ld      a,(CSRX)
                dec     a               ; from 1-based to 0-based
                add     a,l             ; add border size
                ld      l,a

                ; Convert to 16-bits counters.
                ld      h,0
                ld      b,h

                ; Add Y * bytes_per_line.
                ld      a,(CSRY)
                dec     a               ; from 1-based to 0-based
curs2hl_mult_loop:
                srl     a
                jr      nc,curs2hl_mult_skip
                add     hl,bc
curs2hl_mult_skip:
                sla     c               ; BC = BC * 2
                rl      b
                or      a
                jr      nz,curs2hl_mult_loop

                ; Add base address.
                ld      bc,(NAMBAS)
                add     hl,bc

                pop     af
                pop     bc
                ret


;---------------------------
; Subroutines
;---------------------------

; the extensive descriptions were taken with permission from http://map.tni.nl/

;-------------------------------------
;0008h SYNCHR
;Function:  tests whether the character of [HL] is the specified character
;           if not, it generates SYNTAX ERROR, otherwise it goes to CHRGTR
;           (#0010)
;Input:     set the character to be tested in [HL] and the character to be
;           compared next to RST instruction which calls this routine (inline
;           parameter)
;Output:    HL is increased by one and A receives [HL], When the tested character
;           is numerical, the CY flag is set the end of the statement (00h or
;           3Ah) causes the Z flag to be set
;Registers: AF, HL
;NOTE: this implementation is still a stub!
synchr:
                push    hl
                push    af
                ld      hl,synchr_text
;                call    print_debug
                pop     af
                pop     hl
                ret
synchr_text:    .db      "SYNCHR",0

;-------------------------------------
; $0010 CHRGTR
; Read the next program character.
; In:      HL = pointer to the program text
; Out:     A  = the next program character
;          HL = pointer to the next program character
;          ZF = set if it's the end of statement
;          CF = set if it's a number
; Changes: AF, HL
chrgtr:
                call    H_CHRG
chrgtr_lp:
                ld      a,(hl)
                inc     hl
                ; Check for the end of statement.
                cp      $00             ; end of line
                ret     z
                cp      $3A             ; statement separator
                ret     z
                ; Check for digits.
                cp      '0'
                jr      c,chrgtr_no_digit
                cp      '9'+1
                ret     c
chrgtr_no_digit:
                ; Skip whitespace.
                cp      $20             ; space
                jr      z,chrgtr_lp
                cp      $09             ; tab
                jr      z,chrgtr_lp
                ; Otherwise it's a normal program character.
                or      a               ; Clear CF and ZF.
                ret

;-------------------------------------
; $0018 OUTDO
; Function : Output to current outputchannel (printer, diskfile, etc.)
; Input    : A  - PRTFIL, PRTFLG
; Remark   : Used in basic, in ML it's pretty difficult.
outdo:
                push    af
                call    H_OUTD      ; H_OUTD does the real outputting
                pop     af
                ret

;--------------------------------
; $0020 DCOMPR
; Function : Compared HL to DE
; Output   : flags influenced like CP instruction
; Registers: A
dcompr:
                ld      a,h
                cp      d
                ret     nz
                ld      a,l
                cp      e
                ret

;--------------------------------
; $0028 GETYPR
; Function : Returns Type of DAC
; Input    : VALTYP(F663)
; Output   : C, Z, S
;       C       Z       S       Type    VALTYP
;       low     -       -       double  8
;       high    high    low     string  3
;       high    low     high    integer 2
;       high    low     low     float   4
; Registers: AF
;NOTE: this implementation is still a stub!
getypr:
                push    hl
                push    af
                ld      hl,getypr_text
;                call    print_debug
                pop     af
                pop     hl
                ret
getypr_text:    .db      "GETYPR",0

;--------------------------------
; $0030 CALLF
callf:
                ex      af,af'
                exx
                pop     hl              ; Get data from return address.
                ld      a,(hl)
                inc     hl
                ld      e,(hl)
                inc     hl
                ld      d,(hl)
                inc     hl
                push    de              ; IX = call address
                pop     ix
                push    af              ; IY = slot
                pop     iy
                push    hl              ; Update return address.
                ex      af,af'
                exx
                jp      calslt          ; Perform inter-slot call.

;--------------------------------
; $003B INITIO
;Function:  Initialises the device
;Registers: All
initio:
                .IF KB_USE_PS2=1
		call	KB_INITIALIZE
		.ENDIF
                ld      e,$FF           ; strobe off, triggers on
                ld      a,$0F
                call    wrtpsg
                ; TODO: What else must be initialized here?

                jp      gicini

;--------------------------------
; $003E INIFNK
; Function : Initialises the contents of the function keys
; Registers: All
;NOTE: this implementation is still a stub!
inifnk:
                push    hl
                push    af
                ld      hl,inifnk_text
;                call    print_debug
                pop     af
                pop     hl
                ret
inifnk_text:    .db      "INIFNK",0

;--------------------------------
; $0099 STRTMS
; Function : Tests whether the PLAY statement is being executed as a background
;            task. If not, begins to execute the PLAY statement
; Registers: All
;NOTE: this implementation is still a stub!
strtms:
                push    hl
                push    af
                ld      hl,strtms_text
;                call    print_debug
                pop     af
                pop     hl
                ret
strtms_text:    .db      "STRTMS",0


;--------------------------------
; $009C CHSNS
; Function : Tests the status of the keyboard buffer
; Output   : Z-flag set if buffer is filled
; Registers: AF
chsns:
                ei
                push    hl
                push    de
                ld      hl,(GETPNT)
                ld      de,(PUTPNT)
                rst     20h
                ld      a,$ff
                jr      nz,chsns_inbuf
                xor     a
chsns_inbuf:
                pop     de
                pop     hl
                ret

;--------------------------------
; $009F CHGET
; Function : One character input (waiting)
; Output   : A  - ASCII-code of the input character
; Registers: AF

chget:
                call    H_CHGE
                push    hl
                push    de
chget_wait:
                ld      hl,(GETPNT)
                ld      de,(PUTPNT)
                rst     20h
                jr      nz,chget_char
                ei
                halt
                jr      chget_wait
chget_char:
                ld      a,(hl)          ; HL = (GETPNT)
                push    af
                inc     hl
                ; See comment in keyint (below label key_store).
                ld      a,l
                ; Currently, tniASM doesn't support "&" and SjASM doesn't
                ; support "AND", so we have to hardcode the result.
;                cp      $00FF & (KEYBUF + 40)
                cp      $18
                jr      nz,chget_nowrap
                ld      hl,KEYBUF
chget_nowrap:
                ld      (GETPNT),hl
                pop     af
                pop     de
                pop     hl
                ret

;--------------------------------
; $00A2 CHPUT
; Input:   A = character code
; Changes: none

                .include "chput.asm"

;--------------------------------
; $00A5 LPTOUT
; Function : Sen.ds one character to printer
; Input    : A  - ASCII-code of character to send
; Output   : C-flag set if failed
; Registers: F

lptout:
                call    H_LPTO
                push    af
lptout_wait:
                call    breakx
                jr      c,lptout_abort
                call    lptstt
                jr      z,lptout_wait
                pop     af
                jr      lptout_write

lptout_abort:
                ld      a,13
                call    lptout_write
                xor     a
                ld      (LPTPOS),a
                pop     af
                scf
                ret

lptout_write:
                push    af
                out     (PRN_DATA),a
                ld      a,0
                out     (PRN_STAT),a
                cpl
                out     (PRN_STAT),a
                pop     af
                and     a
                ret

;--------------------------------
; $00A8 LPTSTT
; Function : Tests printer status
; Output   : A  - #FF and Z-flag reset if printer is ready
;                 #00 and Z-flag set if not ready
; Registers: AF

lptstt:
                call    H_LPTS
                in      a,(PRN_STAT)
                rra
                rra
                ld      a,$FF
                jr      nc,lptstt_end
                cpl
lptstt_end:
                and     a
                ret

;--------------------------------
; $00AB CNVCHR
; Function : tests for the graphic header and transforms the code
; Input    : A  - charactercode
;            GRPHED(FCA6): indicates if previous char was an extension code
; Output:                               C-flag  Z-flag  A
;       if byte is extension byte       low     high    1
;       if byte is normal ASCII         high    low     ASCII code
;       if byte is graphical extension  high    high    extension code
;       GRPHED is updated
; Registers: AF

cnvchr:
                push    hl
                push    af
                ld      hl,GRPHED
                xor     a
                cp      (hl)
                ld      (hl),a                  ; reset GRPHED in advance
                jr      nz,cnvchr_handlegfx

                pop     af                      ; we're not in graphic mode
                cp      1                       ; graphic header?
                jr      nz,cnvchr_normal

                ld      (hl),a                  ; yes! -> Set GRPHED
                jr      cnvchr_normal_exit      ; we've got NC and Z - perfect!

cnvchr_handlegfx:
                pop     af
                cp      $40
                jr      c,cnvchr_nogfx
                cp      $60
                jr      nc,cnvchr_nogfx
                sub     $40                     ; graphic char
                cp      a                       ; set Z (and NC)
                jr      cnvchr_normal

cnvchr_nogfx:
                cp      $50                     ; A is definitely not #50
                                                ; so this sets NZ :-)
cnvchr_normal:
                scf                             ; NZ/Z already ok, now set C
cnvchr_normal_exit:
                pop     hl
                ret

                .include "inlin.asm"

;--------------------------------
; $00B7 BREAKX
; Tests status of CTRL-STOP.
; This routine reads the keyboard status from the hardware, so its result
; will be accurate even if interrupts have been disabled for a while.
; Output:  CF set if CTRL-STOP is pressed
; Changes: AF
breakx:
		LD	A,(BREAKFLAG)
		CP	$FF
		JR	Z,BREAKX_1

BREAKX_0:       CCF
                ret
BREAKX_1:
		LD	A,0
		LD	(BREAKFLAG),A
		scf
		ret

;--------------------------------
; $00BA ISCNTC
; Function: Test status of STOP or CTRL-STOP; if BASIC is in a ROM (see BASROM),
;           then check for STOP or CTRL-STOP is not done. Otherways:
;       INTLFLG: 0 => no action
;       INTLFLG: 3 => CTRL-STOP pressed => break program, if "STOP-interrupts not on"??
;       INTLFLG: 4 => STOP pressed => wait in ISCNTC till stop pressed again
; Input: INTFLG, BASROM
; Registers: AF
; NOTE: this implementation is still a stub!
iscntc:
                push    hl
                push    af
                ld      hl,iscntc_text
;                call    print_debug
                pop     af
                pop     hl
                ret
iscntc_text:    .db      "ISCNTC",0

;--------------------------------
; $00BD CKCNTC
; Function : Same as ISCNTC. used in Basic
ckcntc:
                jp      iscntc

;--------------------------------
; $00C0 BEEP
; Function : play a short beep, and reset sound system via GICINI
; Registers: All
; NOTE: this implementation is still a stub!
beep:
; Note: Called by CHPUT; if you need to change more regs than AF, HL, DE, BC
;       then update CHPUT.
                push    hl
                push    af
                ld      hl,beep_text
;                call    print_debug
                pop     af
                pop     hl
                ret
beep_text:      .db      "BEEP",0

;--------------------------------
; $00C6 POSIT
; Sets cursor position.
; Input:   H = column
;          L = row
; Changes: AF
posit:
                ; Note: this works because CSRX == CSRY + 1
                ld      (CSRY),hl
                ret

;--------------------------------
; $00C9 FNKSB
; Tests whether the function key display is active (FNKFLG),
; if so, displays them, otherwise erases them.
; Input:   FNKFLG (#FBCE)
; Changes: all
; NOTE: This implementation is still a stub!
fnksb:
                push    hl
                push    af
                ld      hl,fnksb_text
;                call    print_debug
                pop     af
                pop     hl
                ret
fnksb_text:     .db      "FNKSB",0

;--------------------------------
; $00CC ERAFNK
; Erase function key display.
; Changes: all
; NOTE: This implementation is still a stub!
; TODO: call H_ERAF
erafnk:
;               call    H_ERAF
                push    hl
                push    af
                ld      hl,erafnk_text
;                call    print_debug
                pop     af
                pop     hl
                ret
erafnk_text:    .db      "ERAFNK",0

;--------------------------------
; $00CF dsPFNK
; Display function keys.
; Changes: all
; NOTE: This implementation is still a stub!
; TODO: call H_dsPF
dspfnk:
;               call    H_dsPF
                push    hl
                push    af
                ld      hl,dspfnk_text
;                call    print_debug
                pop     af
                pop     hl
                ret
dspfnk_text:    .db      "DSPFNK",0

;--------------------------------
; $00D2 TOTEXT
; Forces the screen to be in the text mode.
; Input: SCRMOD, OLdsCR
; Changes: all
totext:
                ld      a,(SCRMOD)
                cp      2
                ret     c
                ld      a,(OLDSCR)
                call    H_TOTE
                or      a
                jp      z,initxt
                jp      init32

;--------------------------------
; $00E1 TAPION
; Rea.ds the header block after turning the cassette motor on.
; Output:  CF = set if failed
; Changes: all
; NOTE: This implementation is still a stub!
tapion:
                push    hl
                push    af
                ld      hl,tapion_text
;                call    print_debug
                pop     af
                pop     hl
                ; TODO: not implemented -> always fail
                scf
                ret
tapion_text:    .db      "TAPION",0

;--------------------------------
; $00E4 TAPIN
; Read data from the tape.
; Output:  A = data read
; Changes: all
; NOTE: This implementation is still a stub!
tapin:
                push    hl
                push    af
                ld      hl,tapin_text
;                call    print_debug
                pop     af
                pop     hl
                ; TODO: not implemented -> always fail
                scf
                ret
tapin_text:     .db      "TAPIN",0

;--------------------------------
; $00E7 TAPIOF
; Stops reading from the tape.
; NOTE: This implementation is still a stub!
tapiof:
                push    hl
                push    af
                ld      hl,tapiof_text
;                call    print_debug
                pop     af
                pop     hl
                ret
tapiof_text:    .db      "TAPIOF",0

;--------------------------------
; $00EA TAPOON
; Turns on the cassette motor and writes the header.
; Input:   A  = zero for short header, non-zero for long header
; Output:  CF = set if failed
; Changes: all
; NOTE: This implementation is still a stub!
tapoon:
                push    hl
                push    af
                ld      hl,tapoon_text
;                call    print_debug
                pop     af
                pop     hl
                ; TODO: not implemented -> always fail
                scf
                ret
tapoon_text:    .db      "TAPOON",0

;--------------------------------
; $00ED TAPOUT
; Writes data to the tape.
; Input:   A  = data to write
; Output:  CF = set if failed
; Changes: all
; NOTE: This implementation is still a stub!
tapout:
                push    hl
                push    af
                ld      hl,tapout_text
;                call    print_debug
                pop     af
                pop     hl
                ; TODO: not implemented -> always fail
                scf
                ret
tapout_text:    .db      "TAPOUT",0

;--------------------------------
; $00F0 TAPOOF
; Stops writing on the tape.
; NOTE: This implementation is still a stub!
tapoof:
                push    hl
                push    af
                ld      hl,tapoof_text
;                call    print_debug
                pop     af
                pop     hl
                ret
tapoof_text:    .db      "TAPOOF",0

;--------------------------------
; $00F3 STMOTR
; Changes the cassette motor state.
; Input:   A = action: #00 stops motor, #01 starts motor,
;                      #FF inverts current state
; Changes: AF
stmotr:
                push    bc
                ld      b,a
                in      a,(GIO_REGS)
                inc     b
                jr      z,stmotr_inv
                set     4,a
                dec     b
                jr      z,stmotr_set
                res     4,a
                dec     b
                jr      z,stmotr_set
                pop     bc
                ret

stmotr_inv:     xor     16
stmotr_set:     out     (GIO_REGS),a
                pop     bc
                ret

;--------------------------------
; $0090 GICINI  Initialize Sound IC
; Function : Initialises PSG and sets initial value for the PLAY statement
; Registers: All
gicini:
                ld      e,$00
                ld      a,$08
                call    wrtpsg
                inc     a
                call    wrtpsg
                inc     a
                call    wrtpsg
                inc     a

                ld      e,$B8
                ld      a,$07
                call    wrtpsg

                ret

;--------------------------------
; $0093 WRTPSG
; Function : Writes data to PSG-register
; Input    : A  - PSG register number
;            E  - data write
wrtpsg:
                di
                out     (PSG_REGS),a
                push    af
                ld      a,e
                out     (PSG_DATA),a
                ei
                pop     af
                ret

;--------------------------------
; $0096 RDPSG
; Function : Rea.ds value from PSG-register
; Input    : A  - PSG-register read
; Output   : A  - value read
rdpsg:
                out     (PSG_REGS),a
                in      a,(PSG_STAT)
                ret

;--------------------------------
; $0135 CHGSND
; Write to the 1-bit sound port.
; Input:   A = zero to set sound state to 0, non-zero to set sound state to 1
; Changes: AF
chgsnd:
                or      a
                ld      a,$0E           ; $0E = command to reset bit 7
                jr      z,chgsnd_write
                inc     a               ; $0F = command to set bit 7
chgsnd_write:
                out     (PPI_REGS),a    ; set/reset bit of port C
                ret

;--------------------------------
; $0138 RSLREG
; Function : Reads the primary slot register
; Output   : A  - for the value which was read
;            33221100
;            ||||||- Pagina 0 (#0000-#3FFF)
;            ||||--- Pagina 1 (#4000-#7FFF)
;            ||----- Pagina 2 (#8000-#BFFF)
;            ------- Pagina 3 (#C000-#FFFF)
; Registers: A
rslreg:
                in      a,(PSL_STAT)
                ret

;--------------------------------
; $013B WSLREG
; Function : Writes value to the primary slot register
; Input    : A  - value value to (see RSLREG)
wslreg:
                out     (PSL_STAT),a
                ret

;--------------------------------
; $013E RDVDP
; Function : Reads VDP status register
; Output   : A  - Value which was read
; Registers: A
rdvdp:
                in      a,(VDP_STAT)
                ret

;--------------------------------
;0141h SNSMAT
; Function : Returns the value of the specified line from the keyboard matrix
; Input    : A  - for the specified line
; Output   : A  - for data (the bit corresponding to the pressed key will be 0)
; Registers: AF
snsmat:
		LD	A,$FF
		RET

;--------------------------------
; $0144 PHYDIO
; Executes I/O for mass-storage media like diskettes.
; All this routine does is call H_PHYD, which should be installed by the main
; disk ROM.
; Input:     B  = number of sectors to save/load
;            C  = media ID of the disk
;            DE = begin sector
;            HL = begin address in memory
; Changes:   all
; Remark:    Before the call is called, the Z-flag must be reset, and the
;            execution address which was in HL must be at the last stack address
phydio:
                call    H_PHYD
                ret

;--------------------------------
; $0147 FORMAT
; Initialises mass-storage media like formatting of diskettes.
; All this routine does is call H_FORM, which should be installed by the main
; disk ROM.
; Changes:   all
format:
                call    H_FORM
                ret

;--------------------------------
; $014A ISFLIO
; Function : Tests if I/O to device is taking place
; Output   : A  - #00 if not taking place
;             not #00 if taking place
; Registers: AF
; TODO: call H_ISFL
isflio:
;                call    H_ISFL
                ld      a,(PTRFIL)
                and     a               ; adjust flags
                ret

;--------------------------------
; $00D5 GTSTCK
; Function : Returns the joystick status
; Input    : A  - Joystick number to test (0 = cursors, 1 = port 1, 2 = port 2)
; Output   : A  - Direction,D = $00(when A=0)
; Registers: All
gtstck:
                push    bc
                cp      $00
                jr      nz,joy_stc1

                .IF KB_USE_SERIAL=1
     		CALL	key_in

		LD	A,(N8VEM_KB_STICK_CACHE)
		AND	$0F
		JP	Z,GSTICK_N8VEM_END	; IF NO DATA END
		LD	B,A
		LD	A,(N8VEM_KB_STICK_CACHE)
		AND	$F0
		LD	(N8VEM_KB_STICK_CACHE),A
		LD	A,B
GSTICK_N8VEM_END:
                push    hl
                ld      hl,joypos_kbd_tbl
                ld      d,0
                ld      e,a
                add     hl,de
                ld      a,(hl)
                pop     hl
                .ENDIF
                pop     bc
                and     a
                ret
joy_stc1:
;PSG reg 15h
;0J001111
;PSG reg 14h
;00BARLDU

                push    hl
                push    de

                ld      e,$00
                dec     a
                jr      z,sel_stc1
	; N8VEM SITCK 2
                ld      a,$0F
		JR	N8_STKREAD
sel_stc1:
		LD	A,$0E
N8_STKREAD:
                di
                call    rdpsg
                ei
                CPL
                and     $0F
                ld      hl,joypos_joy_tbl
                ld      b,0
                ld      c,a
                add     hl,bc
                ld      a,(hl)
                pop     de
                pop     hl

                pop     bc
                and     a
                ret

joy_end:
                ld      a,$00
                pop     bc
                and     a
                ret

joypos_joy_tbl:
                ;         0   1   2   3   4   5   6   7
                .db      $00,$01,$05,$00,$07,$08,$06,$07
                ;         8   9   A   B   C   D   E   F
                .db      $03,$02,$04,$03,$00,$01,$05,$00

joypos_kbd_tbl:
                ;         0   1   2   3   4   5   6   7
                .db      $00,$07,$01,$08,$05,$06,$00,$07
                ;         8   9   A   B   C   D   E   F
                .db      $03,$00,$02,$01,$04,$05,$03,$00


N8VEM_KB_STICK:
		LD	A,(N8VEM_KB_STICK_CACHE)
		CP	00
		JP	NZ,N8VEM_KB_STICK_END
		LD	A,B
		CP	'S'			;
		JP	Z,N8VEM_KB_STICK_R
		CP	'Z'			;
		JP	Z,N8VEM_KB_STICK_D
		CP	'W'			;
		JP	Z,N8VEM_KB_STICK_U
		CP	'A'			;
		JP	Z,N8VEM_KB_STICK_L
		CP	' '			;
		JP	Z,N8VEM_KB_STICK_B
		JP	N8VEM_KB_STICK_NONE
N8VEM_KB_STICK_R:
		LD	A,%00001000
		LD	(N8VEM_KB_STICK_CACHE),A
		RET
N8VEM_KB_STICK_D:
		LD	A,%00000100
		LD	(N8VEM_KB_STICK_CACHE),A
		RET
N8VEM_KB_STICK_U:
		LD	A,%00000010
		LD	(N8VEM_KB_STICK_CACHE),A
		RET
N8VEM_KB_STICK_L:
		LD	A,%00000001
		LD	(N8VEM_KB_STICK_CACHE),A
		RET
N8VEM_KB_STICK_B:
		LD	A,%00010000
		LD	(N8VEM_KB_STICK_CACHE),A
		RET
N8VEM_KB_STICK_NONE:
		LD	A,%00000000
N8VEM_KB_STICK_END:
		RET
N8VEM_KB_STICK_CACHE:
                .DB	00

;--------------------------------
; $00D8 GTTRIG
; Function : Returns current trigger status
; Input    : A  - trigger button to test
;            0 = spacebar(included A-1 = minus)
;            1 = port 1, button A
;            2 = port 2, button A
;            3 = port 1, button B
;            4 = port 2, button B
; Output   : A  - #00 trigger button not pressed
;                 #FF trigger button pressed
; Note     : Some programs rely on ZF to be set according to the value in A.
; Registers: All
gttrig:
                cp      5
                jr      nc,gttrig_space ; if value of A is above 5,go space routine
                or      a
                jr      nz,joy_trig
gttrig_space:
; Keyboard (spacebar)
		.IF KB_USE_SERIAL=1
		CALL	key_in
		LD	A,(N8VEM_KB_STICK_CACHE)
		AND	$F0
		RET	Z		; IF NO DATA END
		LD	A,(N8VEM_KB_STICK_CACHE)
		AND	$0F
		LD	(N8VEM_KB_STICK_CACHE),A
		LD	A,$FF
		RET
		.ELSE
		LD	A,$00
		RET
		.ENDIF

; Joystick triggers
joy_trig:
                di
                dec     a
                push    de
                ld      e,$03   ; enable trig A+B of stick 1
                ld      b,a
                and     $01
                jr      z,sel_trig1
; TRIGGER 2
                ld      a,$0F
		JR	N8_TRGREAD
sel_trig1:
		LD	A,$0E
N8_TRGREAD:
                di
                call    rdpsg
                ei
                LD	E,A
                LD	A,B
                AND	02
		JR	Z,istrg_a
		LD	A,E
                POP	DE
		AND	$80
                jr      z,trig_on
                jr      trig_off
istrg_a:
		LD	A,E
		POP	DE
		AND	$10
                jr      z,trig_on
                jr      trig_off

trig_on:
                ld      a,$FF
                ret
trig_off:
                xor     a
                ret


;--------------------------------
; $00db GTPAD
; Function : Returns current touch pad status
; Input    : A  - Touchpad number to test
; Output   : A  - Value
; Registers: All
; NOTE     : This implementation is still a stub!
gtpad:
                push    hl
                push    af
                ld      hl,gtpad_text
;                call    print_debug
                pop     af
                pop     hl
                xor     a  ; haywire
                ret
gtpad_text:     .db      "GTPAD",0

;--------------------------------
; $00DE GTPDL
; Function : Returns currenct value of paddle
; Input    : A  - Paddle number
; Output   : A  - Value
; Registers: All
; NOTE     : This implementation is still a stub!
gtpdl:
                push    hl
                push    af
                ld      hl,gtpdl_text
;                call    print_debug
                pop     af
                pop     hl
                ret
gtpdl_text:     .db      "GTPDL",0

;--------------------------------
; $00F6 LFTQ
; Give the number of bytes left in a queue.
; In:      A  = queue number
; Out:     HL = number of bytes left
; Changes: AF, BC, HL
lftq:
                call    calc_queue_address
                ld      b,(hl)          ; B = put position
                inc     b
                inc     hl
                ld      a,(hl)          ; A = get position
                sub     b               ; (getpos - putpos) & size
                inc     hl
                inc     hl
                and     (hl)
                ld      l,a
                ld      h,$00           ; Queues are smaller than 256 bytes.
                ret

;--------------------------------
; $00F9 PUTQ
; Put a byte in a queue.
; In:      A  = queue number
;          E  = data
; Out:     ZF = set if the queue is full
; Changes: AF, BC, HL
putq:
                ; Check whether the queue is full.
                call    calc_queue_address
                ld      a,(hl)
                inc     a
                ld      b,a             ; B = put position + 1
                inc     hl
                sub     (hl)
                ret     z

                ; Save the new put position.
                ld      a,b
                inc     hl
                inc     hl
                push    hl
                and     (hl)            ; (putpos + 1) & size
                dec     hl
                dec     hl
                dec     hl
                ld      (hl),a

                ; Put the data byte in the queue.
                pop     hl
                inc     hl
                ld      a,(hl)          ; Get the buffer address.
                inc     hl
                ld      h,(hl)
                ld      l,a
                dec     b               ; Add putpos.
                ld      c,b
                ld      b,0
                add     hl,bc
                ld      (hl),e
                or      1
                ret

                ; Calculate the address to the start of queue control block.
                ; A = queue number
calc_queue_address:
                ld      hl,(QUEUES)     ; See QUETAB in systemvars.asm.
                ld      b,a             ; (queue number * 6)
                rlca
                rlca
                add     a,b
                add     a,b
                ld      c,a
                ld      b,0
                add     hl,bc
                ret

;--------------------------------
; $0132 CHGCAP
; Function : Alternates the CAP lamp status
; Input    : A  - #00 is lamp on
;             not #00 is lamp off
; Registers: AF
chgcap:
;// TODO: n8vem code
                ret

;--------------------------------
; $014D OUTDLP
; Function : Printer output
; Input    : A  - code to print
; Registers: F
; Remark   : Differences with LPTOUT:
;            1. TAB is expanded to spaces
;            2. For non-MSX printers, Hiragana is transformed to katakana
;               and graphic characters are transformed to 1-byte characters
;            3. If failed, device I/O error occurs
; TODO     : This implementation is still a stub!
outdlp:
                push    hl
                push    af
                ld      hl,outdlp_text
;                call    print_debug
                pop     af
                pop     hl
                ret
outdlp_text:    .db      "OUTDLP",0

;--------------------------------
; $0150 GETVCP
; Returns pointer to a variable at offset 2 in a voice structure.
; TODO: find out the purpose of this variable.
; Address  : #0150
; Function : Returns pointer to play queue
; Input    : A  - Channel number
; Output   : HL - Pointer
; Registers: AF
; Remark   : Only used to play music in background
getvcp:
                ld      l,2
                jr      getvc2_a

;--------------------------------
; $0153 GETVC2
; Returns pointer to a given variable in a voice structure.
; Input    : L        - Pointer in play buffer
;            (VOICEN) - Voice structure number
; Output   : HL - Pointer
; Registers: AF
getvc2:
                ld      a,(VOICEN)
getvc2_a:
                push    de
                ld      d,0
                ld      e,l
                ld      hl,VCBA
                add     hl,de
                ld      e,37            ; Size of a structure
getvc2_loop:
                or      a
                jr      z,getvc2_exit
                add     hl,de
                dec     a
                jr      getvc2_loop
getvc2_exit:
                pop     de
                ret

;--------------------------------
; $0156 KILBUF
; Empties the keyboard buffer.
; Changes: HL
kilbuf:
                ld      hl,(GETPNT)
                ld      (PUTPNT),hl
                ret

;--------------------------------
; Interrupt routine ($0038h)
;--------------------------------
; some games uses Reg.R and the routine affects the register's value.
; if you want to add something to the routine,please try the following first
;
; Riseout , Replicart

keyint:
                push    hl
                push    de
                push    bc
                push    af
                exx
                ex      af,af'
                push    hl
                push    de
                push    bc
                push    af
                push    iy
                push    ix

                call    H_KEYI
                in      a,(VDP_STAT)
                or      a
                ld      (STATFL),a      ; save status
                jp      p,int_end       ; exit if this is not the VDP int
                call    H_TIMI

                ; TODO: (BASIC related stuff)
                ;       Check sprite collision
                ;       Update INTCNT

                ld      hl,(JIFFY)
                inc     hl
                ld      (JIFFY),hl

                ; TODO: MUSICF

                ; TODO: It seems unsafe to me to already allow interrupts
                ;       while this one is still busy: possible interference
                ;       between two interrupts and also the amount of stack
                ;       space claimed is a lot.
                ;ei

                ; Riseout needs that count of RegR in the routine is not
                ; even number
                ; nop

                xor     a
                ld      (CLIKFL),a



                ; Scan the keyboard every three interrupts.

                ld      a,(SCNCNT)
                dec     a
                ld      (SCNCNT),a
                jr      nz,int_end
                ld      a,3
                ld      (SCNCNT),a

                ; TODO read joystick triggers and space for TRGFLG
                xor     a
                call    gttrig
                cpl
                and     $01
                ld      (TRGFLG),a

                .IF KB_USE_SERIAL=1
                call    key_in
                .ENDIF

                .IF KB_USE_PS2=1
		call 	KB_PROCESS
		.ENDIF


int_end:
                pop     ix
                pop     iy
                pop     af
                pop     bc
                pop     de
                pop     hl
                exx
                ex      af,af'
                pop     af
                pop     bc
                pop     de
                pop     hl
                ei
                ret

;--------------------------------
; 0066h NMI interrupt
nmi:
                call    H_NMI
                retn

;--------------------------------
; Get buffer from keyboard input
key_in:
        	IN	A,(UART5)	; READ LINE STATUS REGISTER
		BIT	0,A		; TEST IF DATA IN RECEIVE BUFFER
		JP	Z,N8VEM_KEY_END	; IF NO DATA END
		IN	A,(UART0)	; THEN READ THE CHAR FROM THE UART
		PUSH	AF		;
		PUSH	BC		;
		LD	B,A		;
		CALL	N8VEM_KB_STICK	;
		POP	BC		;
		POP	AF		;
                push    hl		;
                CP	03		;
		JR	NZ,KEY_IN1      ;
		LD	A,$FF		;
		LD	(BREAKFLAG),A	;
		JR	KEY_IN2		;
KEY_IN1:        call    key_put_into_buf;
KEY_IN2:        pop     hl		;
N8VEM_KEY_END:
		RET


;--------------------------------
key_put_into_buf:
                ld      hl,(PUTPNT)
                ld      (hl),a
                ; Note: Ashguine 2 has a bug: it puts KEYBUF at FDF0 iso FBF0
                ;       in the name input routine. This writes keys in memory
                ;       reserved for hooks, but since those hooks are only used
                ;       by BASIC, the game doesn't suffer. When PUTPNT reaches
                ;       FE18, it wraps back to FBF0.
                inc     hl
                ld      a,l
;                cp      $00FF & (KEYBUF + 40)
                cp      $18
                jr      nz,key_store_nowrap
                ld      hl,KEYBUF
key_store_nowrap:
                ; Check whether the buffer is full.
                push    de
                ld      de,(GETPNT)
                rst     20h
                pop     de
                ret     z
                ld      (PUTPNT),hl
                ret


;--------------------------------
; $015C SUBROM
; Function : Calls a routine in SUB-ROM
; Input    : IX - Address of routine in SUB-ROM
; Output   : Depen.ds on the routine
; Registers: Alternative registers, IY
; Remark   : Use of EXTROM or CALSLT is more convenient.
;            You have to use this routine like this:
;               push    ix
;               jp      subrom
;            The purpose is unclear
subrom:
                pop     ix
                ret


;--------------------------------
; $015F EXTROM
; Function : Calls a routine in SUB-ROM. Most common way
; Input    : IX - Address of routine in SUB-ROM
; Output   : Depends on the routine
; Registers: Alternative registers, IY
; Remark   : Use: LD IX,address
;                 CALL EXTROM
extrom:
                ; EXTROM needs to save alternative registers
                ; and when call with certain status, returns with EI
                ret

;------------------------------------
hang_up_mode:
                jr      $

;------------------------------------
; Called if the stack underflows.
stack_error:
                call    H_STKE
                ld      de,str_stack_error
                jp      print_error

;------------------------------------
; $0159 CALBAS
; Function : Executes inter-slot call to the routine in BASIC interpreter
; Input    : IX - for the calling address
; Output   : Depen.ds on the called routine
; Registers: Depen.ds on the called routine
calbas:
                push    hl
                push    af
                ld      hl,calbas_text
;                call    print_debug
                pop     af
                pop     hl
                ld      de,str_no_basic_intr
                jp      print_error
calbas_text:    .db      "CALBAS",0

;------------------------------------
;Display error
;in DE= message address

print_error:
                in      a,(VDP_STAT) ; reset Latch
                ld      hl,vdp_bios
                ld      b,$0C
                ld      c,VDP_ADDR
                otir

                ld      bc,$0800
lp_clearmem:
                xor     a
                out     (VDP_DATA),a
                dec     bc
                ld      a,b
                or      c
                jr      nz,lp_clearmem

                ld      hl,B_Font
                ld      bc,$0800
lp_fontset:
                ld      a,(hl)
                out     (VDP_DATA),a
                inc     hl
                dec     bc
                ld      a,b
                or      c
                jr      nz,lp_fontset

;set cursor to (0,0)
                ld      a,$00
                out     (VDP_ADDR),a
                ld      a,$40
                out     (VDP_ADDR),a

                ld      hl,str_error_prompt

                ld      a,(hl)
lp_errprn:
                out     (VDP_DATA),a
                inc     hl
                ld      a,(hl)
                and     a
                jr      nz,lp_errprn

                ld      a,(de)
lp_strprn:
                out     (VDP_DATA),a
                inc     de
                ld      a,(de)
                and     a
                jr      nz,lp_strprn

                jp      hang_up_mode

                .ds      $1bbf - $
                .include "font.asm"

                .include "slot.asm"

;---------------------------------
; system messages
;---------------------------------

str_proginfo:
                ;       [01234567890123456789012345678]
                .db     "N8VEM BIOS 0.1     MSX C-BIOS"
                ;include "../derived/asm/version.asm"
                .db      $0D,$0A,$0D,$0A,$0D,$0A,$00


;-------------------------------------
; error messages
str_error_prompt:
                .db      "ERROR:",$00

str_no_basic_intr:
                .db      "CALLED NON EXISTING BASIC.",$00

str_stack_error:
                .db      "STACK ERROR.",$00


;-------------------------------------
vdp_bios:
                .db      $00,$80,$70,$81,$00,$82,$01,$84
                .db      $F5,$87,$00,$40
		.include "PS2KEYB.ASM"
                .include "ROMMON.asm"
                .include "BASIC.asm"

                .ds      $3FFF - $
                .db      $00

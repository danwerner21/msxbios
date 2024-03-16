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
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;

        INCLUDE "systemva.asm"
        INCLUDE "hooks.asm"

;-----------------
; A memory address for debug
;-----------------

COMPILE_FONT    EQU 1

;---------------------
; Jump table
;---------------------

; $0000 CHKRAM
        ORG     $0000
        DI
        JP      chkram

; Pointer to font
; $0004 CGTABL  Base address of the MSX character set in ROM
        DS      $0004 - $
        DW      B_Font

        DS      $0006 - $

; $0006 VDP.DR  Base port address for VDP data read
vdp_dr:
        DB      VDP_DATA                          ; VDP read port
; $0007 VDP.WR  Base port address for VDP data write
vdp_dw:
        DB      VDP_DATA                          ; VDP write port

; $0008 SYNCHR
        DS      $0008 - $
        JP      synchr

; $000C RDSLT   Read memory from an optional slot
        DS      $000C - $
        JP      rdslt

; $0010 CHRGTR
        DS      $0010 - $
        JP      chrgtr

; $0014 WRSLT   Write memory to an optional slot
        DS      $0014 - $
        JP      wrslt

; $0018 OUTDO
        DS      $0018 - $
        JP      outdo

; $001C CALSLT   inter slot call routine
        DS      $001C - $
        JP      calslt

; $0020 DCOMPR  Compare HL to DE
        DS      $0020 - $
        JP      dcompr

; $0024 ENASLT  Change slot
        DS      $0024 - $
        JP      enaslt

; $0028 GETYPR
        DS      $0028 - $
        JP      getypr

; $002B IDBYT1
        DS      $002B - $
iDByt1:
; Basic ROM version
; 7 6 5 4 3 2 1 0
; | | | | +-+-+-+-- Character set
; | | | |           0 = Japanese, 1 = International (ASCII), 2=Korean
; | +-+-+---------- Date format
; |                 0 = Y-M-D, 1 = M-D-Y, 2 = D-M-Y
; +---------------- Default interrupt fr.equency
;                   0 = 60Hz, 1 = 50Hz
        DB      LOCALE_CHSET + LOCALE_DATE + LOCALE_INT
; $002C IDBYT2
iDByt2:
; Basic ROM version
; 7 6 5 4 3 2 1 0
; | | | | +-+-+-+-- Keyboard type
; | | | |           0 = Japanese, 1 = International (QWERTY)
; | | | |           2 = French (AZERTY), 3 = UK, 4 = German (DIN)
; +-+-+-+---------- Basic version
;                   0 = Japanese, 1 = International
        DB      LOCALE_KBD + LOCALE_BASIC

; $002D Version ID
romid:
        DS      $002D - $
; version ID
; MSX version number
;  0 = MSX 1
;  1 = MSX 2
;  2 = MSX 2+
;  3 = MSX turbo R
        DB      0
        DB      0


; Reserved
        DB      0

; $0030 CALLF    Call interslot routine(RST30h version)
        DS      $0030 - $
        JP      callf

; $0038 KEYINT   Interrupt routines(RST38,VBlank,Timer...)
        DS      $0038 - $
        JP      keyint

; $003B INITIO  Initialize I/O
        DS      $003B - $
        JP      initio

; $003E INIFNK
        DS      $003E - $
        JP      inifnk

; $0041 DISSCR  Disable screen display
        DS      $0041 - $
        JP      disscr

; $0044 ENASCR  Enable screen display
        DS      $0044 - $
        JP      enascr

;---------------
;VDP routines
;---------------

; $0047 WRTVDP
        DS      $0047 - $
        JP      wrtvdp

; $004A RDVRM
        DS      $004A - $
        JP      rdvrm

; $004D WRTVRM
        DS      $004D - $
        JP      wrtvrm

; $0050 SETRD
        DS      $0050 - $
        JP      setrd

; $0053 SETWRT  Set VRAM Write Address
        DS      $0053 - $
        JP      setwrt

; $0056 FILVRM  Fill VRAM
        DS      $0056 - $
        JP      filvrm

; $0059 LDIRMV  Copy VRAM to RAM
        DS      $0059 - $
        JP      ldirmv

; $005C LDIRVM  Copy RAM to VRAM
        DS      $005C - $
        JP      ldirvm

; $005F CHGMOD Change VDP screen mode
        DS      $005F - $
        JP      chgmod

; $0062 CHGCLR
        DS      $0062 - $
        JP      chgclr

; $0066 NMI     Non-maskable interrupt
        DS      $0066 - $
        JP      nmi

; $0069 CLRSPR  Clear sprites
        DS      $0069 - $
        JP      clrspr

; $006C INITXT  Initialize display to mode TEXT1    (SCREEN 0)
        DS      $006C - $
        JP      initxt

; $006F INIT32  Initialize display to mode GRAPHIC1 (SCREEN 1)
        DS      $006F - $
        JP      init32

; $0072 INITGRP Initialize display to mode GRAPHIC2 (SCREEN 2)
        DS      $0072 - $
        JP      inigrp

; $0075 INIMLT  Initialize display to mode MULTI    (SCREEN 3)
        DS      $0075 - $
        JP      inimlt

; $0078 SETTXT
        DS      $0078 - $
        JP      settxt

; $007B SETT32
        DS      $007B - $
        JP      sett32

; $007E SETGRP
        DS      $007E - $
        JP      setgrp

; $0081 SETMLT
        DS      $0081 - $
        JP      setmlt

; $0084 CALPAT
        DS      $0084 - $
        JP      calpat

; $0087 CALATR
        DS      $0087 - $
        JP      calatr

; $008A GSPSIZ
        DS      $008A - $
        JP      gspsiz

; $008D GRPPRT
        DS      $008D - $
        JP      grpprt

; $0090 GICINI  initialize sound IC
        DS      $0090 - $
        JP      gicini
; $0093 WRTPSG
        DS      $0093 - $
        JP      wrtpsg
; $0096 RDPSG
        DS      $0096 - $
        JP      rdpsg

; $0099 STRTMS
        DS      $0099 - $
        JP      strtms

; $009C CHSNS  .. check key buffer
        DS      $009C - $
        JP      chsns

; $009F CHGET .. Get data from keyboard buffer
        DS      $009F - $
        JP      chget

; $00A2 CHPUT .. Output charactor to display
        DS      $00A2 - $
        JP      chput

; $00A5 LPTOUT
        DS      $00A5 - $
        JP      lptout

; $00A8 LPTSTT
        DS      $00A8 - $
        JP      lptstt

; $00AB CNVCHR
        DS      $00AB - $
        JP      cnvchr

; $00AE PINLIN
        DS      $00AE - $
        JP      pinlin

; $00B1 INLIN
        DS      $00B1 - $
        JP      inlin

; $00B4 QINLIN
        DS      $00B4 - $
        JP      qinlin

; $00B7 BREAKX
        DS      $00B7 - $
        JP      breakx

; $00BA ISCNTC
        DS      $00BA - $
        JP      iscntc

; $00BD CKCNTC
        DS      $00BD - $
        JP      ckcntc

; $00C0 BEEP
        DS      $00C0 - $
        JP      beep

; $00C3 CLS
        DS      $00C3 - $
        JP      cls_z

; $00C6 POSIT
        DS      $00C6 - $
        JP      posit

; $00C9 FNKSB
        DS      $00C9 - $
        JP      fnksb

; $00CC ERAFNK
        DS      $00CC - $
        JP      erafnk

; $00CF DSPFNK
        DS      $00CF - $
        JP      dspfnk

; $00D2 TOTEXT
        DS      $00D2 - $
        JP      totext

; $00D5 GTSTCK .. Get joystick infomation
        DS      $00D5 - $
        JP      gtstck

; $00D8 GTTRIG .. Get trigger infomation
        DS      $00D8 - $
        JP      gttrig

; $00db GTPAD
        DS      $00db - $
        JP      gtpad

; $00DE GTPDL
        DS      $00DE - $
        JP      gtpdl

; $00E1 TAPION
        DS      $00E1 - $
        JP      tapion

; $00E4 TAPIN
        DS      $00E4 - $
        JP      tapin

; $00E7 TAPIOF
        DS      $00E7 - $
        JP      tapiof

; $00EA TAPOON
        DS      $00EA - $
        JP      tapoon

; $00ED TAPOUT
        DS      $00ED - $
        JP      tapout

; $00F0 TAPOOF
        DS      $00F0 - $
        JP      tapoof

; $00F3 STMOTR
        DS      $00F3 - $
        JP      stmotr

; $00F6 LFTQ
        DS      $00F6 - $
        JP      lftq

; $00F9 PUTQ
        DS      $00F9 - $
        JP      putq

; $00FC RIGHTC
        DS      $00FC - $
        JP      rightc

; $00FF LEFTC
        DS      $00FF - $
        JP      leftc

; $0102 UPC
        DS      $0102 - $
        JP      upc

; $0105 TUPC
        DS      $0105 - $
        JP      tupc

; $0108 DOWNC
        DS      $0108 - $
        JP      downc

; $010B TDOWNC
        DS      $010B - $
        JP      tdownc

; $010E SCALXY
        DS      $010E - $
        JP      scalxy

; $0111 MAPXY
        DS      $0111 - $
        JP      mapxy

; $0114 FETCHC
        DS      $0114 - $
        JP      fetchc

; $0117 STOREC
        DS      $0117 - $
        JP      storec

; $011A SETATR
        DS      $011A - $
        JP      setatr

; $011D READC
        DS      $011D - $
        JP      readc

; $0120 SETC
        DS      $0120 - $
        JP      setc

; $0123 NSETCX
        DS      $0123 - $
        JP      nsetcx

; $0126 GTASPC
        DS      $0126 - $
        JP      gtaspc

; $0129 PNTINI
        DS      $0129 - $
        JP      pntini

; $012C SCANR
        DS      $012C - $
        JP      scanr

; $012F SCANL
        DS      $012F - $
        JP      scanl

; $0132 CHGCAP
        DS      $0132 - $
        JP      chgcap

; $0135 CHGSND
        DS      $0135 - $
        JP      chgsnd

; $0138 RSLREG  Read infomation of primary slot
        DS      $0138 - $
        JP      rslreg

; $013B WSLREG  Write infomation to primary slot
        DS      $013B - $
        JP      wslreg

; $013E RDVDP   Read VDP status
        DS      $013E - $
        JP      rdvdp

; $0141 SNSMAT  Get key matrix
        DS      $0141 - $
        JP      snsmat

; $0144 PHYDIO
        DS      $0144 - $
        JP      phydio

; $0147 FORMAT
        DS      $0147 - $
        JP      format

; $014A ISFLIO
        DS      $014A - $
        JP      isflio

; $014D OUTDLP
        DS      $014D - $
        JP      outdlp

; $0150 GETVCP
        DS      $0150 - $
        JP      getvcp

; $0153 GETVC2
        DS      $0153 - $
        JP      getvc2

; $0156 KILBUF  Clear keyboard buffer
        DS      $0156 - $
        JP      kilbuf

; $0159 CALBAS  Call BASIC interpreter
        DS      $0159 - $
        JP      calbas


; fake EXTROM call, fixes Nemesis 3 reset bug
        DS      $015f - $
        RET

        DS      $0200 - $

        INCLUDE "util.asm"
        INCLUDE "video.asm"

;
;	AROUND 800 BYTES FREE HERE
;
;

; The game "Hacker" jumps directly to this location($0D02).
; Star force calls $0D0E.

        DS      $0D01 - $
; for all wrong jumper,put RET instruction there
        RET
        POP     ix                                ; $0D02
        POP     iy
        POP     af
        POP     bc
        POP     de
        POP     hl
        EXX
        EX      af,af'
        POP     af
        POP     bc
        POP     de
        POP     hl
        EI
        RET

; $0000 CHKRAM
; Function : Tests RAM and sets RAM slot for the system
; Registers: All
; Remark   : After this, a jump must be made to INIT, for further initialisation.
chkram:

        IF     PLATFORM=1
        LD      A,$FE
        OUT     (VDP_ACR),A                       ; INITIALIZE ACR REGISTER FOR DUODYNE HARDWARE
        ENDIF

        IF     PLATFORM=3
        LD      A,$FD
        OUT     (VDP_ACR),A                       ; INITIALIZE ACR REGISTER FOR N8VEM HARDWARE
        ENDIF

;----------------------
; User interface
;----------------------

        LD      hl,$F300
        LD      sp,hl                             ; set $F300 to stack pointer

        CALL    init_ram

        CALL    init_vdp

        EI

        CALL    initio

        LD      a,15
        LD      (FORCLR),a
        LD      a,5
        LD      (BAKCLR),a
        LD      (BDRCLR),a
        LD      a,29
        LD      (LINL32),a
        CALL    init32

        LD      hl,str_proginfo
        CALL    prn_text

        CALL    search_roms
        CALL    H_STKE

; Set up hooks and system vars so NMS8250 disk ROM will try
; to load and execute the boot sector.
        LD      a,1
        LD      (DEVICE),a
        XOR     a
; TODO: Find out or invent name for $FB29.
        LD      ($FB29),a

; This is the hook the disk ROM uses for booting.
        CALL    H_RUNC

        CALL    $006C
        CALL    $0078
        CALL    $00C3

;   JP	STARTBASIC

        LD      hl,str_nocart
        CALL    prn_text

        JP      hang_up_mode



;----------------------
; Search for any extension ROMs and initialize them.
search_roms:
        LD      DE,($8000)
        LD      hl,$4241                          ; "AB"
        CALL    dcompr                            ; ZF is set if the ROM is present.
        JP      z,search_roms_8k
        LD      DE,($4000)
        LD      hl,$4241                          ; "AB"
        CALL    dcompr                            ; ZF is set if the ROM is present.
        JP      z,search_roms_4k
        RET
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
        LD      a,$00
        LD      hl,$F380
        LD      (hl),a
        LD      de,$F381
        LD      bc,$0C7D
        LDIR

; Initialize Disk work
        LD      a,$C9
        LD      hl,$F300
        LD      (hl),a
        LD      de,$F301
        LD      bc,$007F
        LDIR

; initialize hook area with $C9 (assembler code for ret)
        LD      a,$C9                             ; ret code
        LD      hl,H_KEYI
        LD      (hl),a
        LD      de,H_KEYI+1
        LD      bc,$024D                          ; shouldn't this be $0235 ?
        LDIR

; Initialize key matrix
        LD      a,$FF
        LD      hl,OLDKEY
        LD      (hl),a
        LD      de,OLDKEY+1
        LD      bc,21
        LDIR

; Initialize Key buffer
        LD      a,$00
        LD      hl,KEYBUF
        LD      (hl),a
        LD      de,KEYBUF+1
        LD      bc,39
        LDIR


; Set address pointer
        LD      hl,KEYBUF
        LD      (PUTPNT),hl
        LD      (GETPNT),hl

;                ld      hl,$8000
        EXX
        LD      (BOTTOM),hl                       ; Page1 and 2 is ROM,Page3 and 4 is RAM.
        EXX

; I don't know exactly what is stored between $F168 and $F380,
; but the disk ROM neeDS some space there, so I'll just
; reserve all of it.
        LD      hl,$F380                          ; was $F168, but neeDS to be changed by disk ROM
        LD      (HIMEM),hl                        ; limit of usable memory
        LD      (STKTOP),hl                       ; position of BASIC stack


; Initialize table of screen 0
        LD      hl,$0000
        LD      (TXTNAM),hl
        LD      hl,$0800
        LD      (TXTCGP),hl

; Initialize table of screen 1
        LD      hl,$1800
        LD      (T32NAM),hl
        LD      hl,$2000
        LD      (T32COL),hl
        LD      hl,$0000
        LD      (T32CGP),hl
        LD      hl,$1B00
        LD      (T32ATR),hl
        LD      hl,$3800
        LD      (T32PAT),hl

; Initialize table of screen 2

        LD      hl,$1800
        LD      (GRPNAM),hl
        LD      hl,$2000
        LD      (GRPCOL),hl
        LD      hl,$0000
        LD      (GRPCGP),hl
        LD      hl,$1B00
        LD      (GRPATR),hl
        LD      hl,$3800
        LD      (GRPPAT),hl

; Initialize table fo screen 3
        LD      hl,$0800
        LD      (MLTNAM),hl
        LD      hl,$0000
        LD      (MLTCGP),hl
        LD      hl,$1B00
        LD      (MLTATR),hl
        LD      hl,$3800
        LD      (MLTPAT),hl

; Initialise QUETAB.
        LD      hl,QUETAB
        LD      (QUEUES),hl
        LD      hl,VOICAQ
        LD      ($FFFF &(QUETAB+0*6+4)),hl
        LD      hl,VOICBQ
        LD      ($FFFF &(QUETAB+1*6+4)),hl
        LD      hl,VOICCQ
        LD      ($FFFF &(QUETAB+2*6+4)),hl
        LD      a,$7F
        LD      ($FFFF &(QUETAB+0*6+3)),a
        LD      ($FFFF &(QUETAB+1*6+3)),a
        LD      ($FFFF &(QUETAB+2*6+3)),a

; other settings
        LD      a,39
        LD      (LINL40),a
        LD      a,32                              ; Set to 29 after splash screen.
        LD      (LINL32),a
;TODO: Rely on call to INIT32 instead.
        LD      a,(LINL32)
        LD      (LINLEN),a
        LD      a,24
        LD      (CRTCNT),a

        LD      a,$04
        LD      (BDRCLR),a
        LD      (BAKCLR),a
        LD      a,$0F
        LD      (FORCLR),a

        LD      a,$A0
        LD      (RG1SAV),a

        LD      a,(EXPTBL)
        LD      (CGPNT),a
        LD      hl,(4)
        LD      (CGPNT+1),hl

; set up hook
        LD      a,$c3
        LD      hl,chput
        LD      (H_OUTD+1),hl
        LD      (H_OUTD),a

        RET


;------------------------
; wait routine
; caution,already EI when call the rouine
; B = frequency of loop
wait_b:
        HALT
        DJNZ    wait_b
        RET

;------------------------
;prn_text
; HL = string with null termination

prn_text:
        LD      a,(SCRMOD)
        CP      5
        JR      nc,prn_text_graph
prn_text_char:
        LD      a,(hl)
        OR      a
        RET     z
        CALL    chput
        INC     hl
        JR      prn_text_char
prn_text_graph:
        LD      a,(hl)
        OR      a
        RET     z
        LD      ix,$0089
        CALL    extrom
        INC     hl
        JR      prn_text_graph

;--------------------------------
; Determine bytes per line in the current text mode.
; Input:   SCRMOD, LINLEN
; Output:  C = number of bytes per line
; Changes: AF
text_bytes_per_line:
        LD      c,32                              ; text32
        LD      a,(SCRMOD)
        OR      a
        RET     nz
        LD      c,40                              ; text40
        LD      a,(LINLEN)
        CP      41
        RET     c
        LD      c,80                              ; text80
        RET

;--------------------------------
; Calculate the VRAM address that corresponDS to the current cursor position.
; Input:   CSRX, CSRY
; Output:  HL = VRAM address
; Changes: none
curs2hl:
        PUSH    bc
        PUSH    af

        CALL    text_bytes_per_line

; Calculate left border.
        LD      a,(LINLEN)
        NEG
        ADD     a,c                               ; A = bytes_per_line - LINLEN
        INC     a                                 ; round up
        SRL     a                                 ; A = A / 2
        LD      l,a                               ; L = size of left border

; Add X coordinate.
        LD      a,(CSRX)
        DEC     a                                 ; from 1-based to 0-based
        ADD     a,l                               ; add border size
        LD      l,a

; Convert to 16-bits counters.
        LD      h,0
        LD      b,h

; Add Y * bytes_per_line.
        LD      a,(CSRY)
        DEC     a                                 ; from 1-based to 0-based
curs2hl_mult_loop:
        SRL     a
        JR      nc,curs2hl_mult_skip
        ADD     hl,bc
curs2hl_mult_skip:
        SLA     c                                 ; BC = BC * 2
        RL      b
        OR      a
        JR      nz,curs2hl_mult_loop

; Add base address.
        LD      bc,(NAMBAS)
        ADD     hl,bc

        POP     af
        POP     bc
        RET


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
        PUSH    hl
        PUSH    af
        LD      hl,synchr_text
;                call    print_debug
        POP     af
        POP     hl
        RET
synchr_text:
        DB      "SYNCHR",0

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
        CALL    H_CHRG
chrgtr_lp:
        LD      a,(hl)
        INC     hl
; Check for the end of statement.
        CP      $00                               ; end of line
        RET     z
        CP      $3A                               ; statement separator
        RET     z
; Check for digits.
        CP      '0'
        JR      c,chrgtr_no_digit
        CP      '9'+1
        RET     c
chrgtr_no_digit:
; Skip whitespace.
        CP      $20                               ; space
        JR      z,chrgtr_lp
        CP      $09                               ; tab
        JR      z,chrgtr_lp
; Otherwise it's a normal program character.
        OR      a                                 ; Clear CF and ZF.
        RET

;-------------------------------------
; $0018 OUTDO
; Function : Output to current outputchannel (printer, diskfile, etc.)
; Input    : A  - PRTFIL, PRTFLG
; Remark   : Used in basic, in ML it's pretty difficult.
outdo:
        PUSH    af
        CALL    H_OUTD                            ; H_OUTD does the real outputting
        POP     af
        RET

;--------------------------------
; $0020 DCOMPR
; Function : Compared HL to DE
; Output   : flags influenced like CP instruction
; Registers: A
dcompr:
        LD      a,h
        CP      d
        RET     nz
        LD      a,l
        CP      e
        RET

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
        PUSH    hl
        PUSH    af
        LD      hl,getypr_text
;                call    print_debug
        POP     af
        POP     hl
        RET
getypr_text:
        DB      "GETYPR",0

;--------------------------------
; $0030 CALLF
callf:
        EX      af,af'
        exx
        POP     hl                                ; Get data from return address.
        LD      a,(hl)
        INC     hl
        LD      e,(hl)
        INC     hl
        LD      d,(hl)
        INC     hl
        PUSH    de                                ; IX = call address
        POP     ix
        PUSH    af                                ; IY = slot
        POP     iy
        PUSH    hl                                ; Update return address.
        EX      af,af'
        EXX
        JP      calslt                            ; Perform inter-slot call.

;--------------------------------
; $003B INITIO
;Function:  Initialises the device
;Registers: All
initio:
        IF      KB_USE_PS2=1
            CALL    KB_INITIALIZE
        ENDIF
        LD      e,$FF                             ; strobe off, triggers on
        LD      a,$0F
        CALL    wrtpsg
; TODO: What else must be initialized here?

        JP      gicini

;--------------------------------
; $003E INIFNK
; Function : Initialises the contents of the function keys
; Registers: All
;NOTE: this implementation is still a stub!
inifnk:
        PUSH    hl
        PUSH    af
        LD      hl,inifnk_text
;                call    print_debug
        POP     af
        POP     hl
        RET
inifnk_text:
        DB      "INIFNK",0

;--------------------------------
; $0099 STRTMS
; Function : Tests whether the PLAY statement is being executed as a background
;            task. If not, begins to execute the PLAY statement
; Registers: All
;NOTE: this implementation is still a stub!
strtms:
        PUSH    hl
        PUSH    af
        LD      hl,strtms_text
;                call    print_debug
        POP     af
        POP     hl
        RET
strtms_text:
        DB      "STRTMS",0


;--------------------------------
; $009C CHSNS
; Function : Tests the status of the keyboard buffer
; Output   : Z-flag set if buffer is filled
; Registers: AF
chsns:
        EI
        PUSH    hl
        PUSH    de
        LD      hl,(GETPNT)
        LD      de,(PUTPNT)
        RST     20h
        LD      a,$ff
        JR      nz,chsns_inbuf
        XOR     a
chsns_inbuf:
        POP     de
        POP     hl
        RET

;--------------------------------
; $009F CHGET
; Function : One character input (waiting)
; Output   : A  - ASCII-code of the input character
; Registers: AF

chget:
        CALL    H_CHGE
        PUSH    hl
        PUSH    de
chget_wait:
        LD      hl,(GETPNT)
        LD      de,(PUTPNT)
        RST     20h
        JR      nz,chget_char
        EI
        HALT
        JR      chget_wait
chget_char:
        LD      a,(hl)                            ; HL = (GETPNT)
        PUSH    af
        INC     hl
; See comment in keyint (below label key_store).
        LD      a,l
; Currently, tniASM doesn't support "&" and SjASM doesn't
; support "AND", so we have to hardcode the result.
;                cp      $00FF & (KEYBUF + 40)
        CP      $18
        JR      nz,chget_nowrap
        LD      hl,KEYBUF
chget_nowrap:
        LD      (GETPNT),hl
        POP     af
        POP     de
        POP     hl
        RET

;--------------------------------
; $00A2 CHPUT
; Input:   A = character code
; Changes: none

        INCLUDE "chput.asm"

;--------------------------------
; $00A5 LPTOUT
; Function : SenDS one character to printer
; Input    : A  - ASCII-code of character to send
; Output   : C-flag set if failed
; Registers: F

lptout:
        CALL    H_LPTO
        PUSH    af
lptout_wait:
        CALL    breakx
        JR      c,lptout_abort
        CALL    lptstt
        JR      z,lptout_wait
        POP     af
        JR      lptout_write

lptout_abort:
        LD      a,13
        CALL    lptout_write
        XOR     a
        LD      (LPTPOS),a
        POP     af
        SCF
        RET

lptout_write:
        PUSH    af
        OUT     (PRN_DATA),a
        LD      a,0
        OUT     (PRN_STAT),a
        CPL
        OUT     (PRN_STAT),a
        POP     af
        AND     a
        RET

;--------------------------------
; $00A8 LPTSTT
; Function : Tests printer status
; Output   : A  - #FF and Z-flag reset if printer is ready
;                 #00 and Z-flag set if not ready
; Registers: AF

lptstt:
        CALL    H_LPTS
        IN      a,(PRN_STAT)
        RRA
        RRA
        LD      a,$FF
        JR      nc,lptstt_end
        CPL
lptstt_end:
        AND     a
        RET

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
        PUSH    hl
        PUSH    af
        LD      hl,GRPHED
        XOR     a
        CP      (hl)
        LD      (hl),a                            ; reset GRPHED in advance
        JR      nz,cnvchr_handlegfx

        POP     af                                ; we're not in graphic mode
        CP      1                                 ; graphic header?
        JR      nz,cnvchr_normal

        LD      (hl),a                            ; yes! -> Set GRPHED
        JR      cnvchr_normal_exit                ; we've got NC and Z - perfect!

cnvchr_handlegfx:
        POP     af
        CP      $40
        JR      c,cnvchr_nogfx
        CP      $60
        JR      nc,cnvchr_nogfx
        SUB     $40                               ; graphic char
        CP      a                                 ; set Z (and NC)
        JR      cnvchr_normal

cnvchr_nogfx:
        CP      $50                               ; A is definitely not #50
                                                  ; so this sets NZ :-)
cnvchr_normal:
        SCF                                       ; NZ/Z already ok, now set C
cnvchr_normal_exit:
        POP     hl
        RET

        INCLUDE "inlin.asm"

;--------------------------------
; $00B7 BREAKX
; Tests status of CTRL-STOP.
; This routine reads the keyboard status from the hardware, so its result
; will be accurate even if interrupts have been disabled for a while.
; Output:  CF set if CTRL-STOP is pressed
; Changes: AF
breakx:
        LD      A,(BREAKFLAG)
        CP      $FF
        JR      Z,BREAKX_1

BREAKX_0:
        CCF
        RET
BREAKX_1:
        LD      A,0
        LD      (BREAKFLAG),A
        SCF
        RET

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
        PUSH    hl
        PUSH    af
        LD      hl,iscntc_text
;                call    print_debug
        POP     af
        POP     hl
        RET
iscntc_text:
        DB      "ISCNTC",0

;--------------------------------
; $00BD CKCNTC
; Function : Same as ISCNTC. used in Basic
ckcntc:
        JP      iscntc

;--------------------------------
; $00C0 BEEP
; Function : play a short beep, and reset sound system via GICINI
; Registers: All
; NOTE: this implementation is still a stub!
beep:
; Note: Called by CHPUT; if you need to change more regs than AF, HL, DE, BC
;       then update CHPUT.
        PUSH    hl
        PUSH    af
        LD      hl,beep_text
;                call    print_debug
        POP     af
        POP     hl
        RET
beep_text:
        DB      "BEEP",0

;--------------------------------
; $00C6 POSIT
; Sets cursor position.
; Input:   H = column
;          L = row
; Changes: AF
posit:
; Note: this works because CSRX == CSRY + 1
        LD      (CSRY),hl
        RET

;--------------------------------
; $00C9 FNKSB
; Tests whether the function key display is active (FNKFLG),
; if so, displays them, otherwise erases them.
; Input:   FNKFLG (#FBCE)
; Changes: all
; NOTE: This implementation is still a stub!
fnksb:
        PUSH    hl
        PUSH    af
        LD      hl,fnksb_text
;                call    print_debug
        POP     af
        POP     hl
        RET
fnksb_text:
        DB      "FNKSB",0

;--------------------------------
; $00CC ERAFNK
; Erase function key display.
; Changes: all
; NOTE: This implementation is still a stub!
; TODO: call H_ERAF
erafnk:
;               call    H_ERAF
        PUSH    hl
        PUSH    af
        LD      hl,erafnk_text
;                call    print_debug
        POP     af
        POP     hl
        RET
erafnk_text:
        DB      "ERAFNK",0

;--------------------------------
; $00CF dsPFNK
; Display function keys.
; Changes: all
; NOTE: This implementation is still a stub!
; TODO: call H_dsPF
dspfnk:
;               call    H_dsPF
        PUSH    hl
        PUSH    af
        LD      hl,dspfnk_text
;                call    print_debug
        POP     af
        POP     hl
        RET
dspfnk_text:
        DB      "DSPFNK",0

;--------------------------------
; $00D2 TOTEXT
; Forces the screen to be in the text mode.
; Input: SCRMOD, OLdsCR
; Changes: all
totext:
        LD      a,(SCRMOD)
        CP      2
        RET     c
        LD      a,(OLDSCR)
        CALL    H_TOTE
        OR      a
        JP      z,initxt
        JP      init32

;--------------------------------
; $00E1 TAPION
; ReaDS the header block after turning the cassette motor on.
; Output:  CF = set if failed
; Changes: all
; NOTE: This implementation is still a stub!
tapion:
        PUSH    hl
        PUSH    af
        LD      hl,tapion_text
;                call    print_debug
        POP     af
        POP     hl
; TODO: not implemented -> always fail
        SCF
        RET
tapion_text:
        DB      "TAPION",0

;--------------------------------
; $00E4 TAPIN
; Read data from the tape.
; Output:  A = data read
; Changes: all
; NOTE: This implementation is still a stub!
tapin:
        PUSH    hl
        PUSH    af
        LD      hl,tapin_text
;                call    print_debug
        POP     af
        POP     hl
; TODO: not implemented -> always fail
        SCF
        RET
tapin_text:
        DB      "TAPIN",0

;--------------------------------
; $00E7 TAPIOF
; Stops reading from the tape.
; NOTE: This implementation is still a stub!
tapiof:
        PUSH    hl
        PUSH    af
        LD      hl,tapiof_text
;                call    print_debug
        POP     af
        POP     hl
        RET
tapiof_text:
        DB      "TAPIOF",0

;--------------------------------
; $00EA TAPOON
; Turns on the cassette motor and writes the header.
; Input:   A  = zero for short header, non-zero for long header
; Output:  CF = set if failed
; Changes: all
; NOTE: This implementation is still a stub!
tapoon:
        PUSH    hl
        PUSH    af
        LD      hl,tapoon_text
;                call    print_debug
        POP     af
        POP     hl
; TODO: not implemented -> always fail
        SCF
        RET
tapoon_text:
        DB      "TAPOON",0

;--------------------------------
; $00ED TAPOUT
; Writes data to the tape.
; Input:   A  = data to write
; Output:  CF = set if failed
; Changes: all
; NOTE: This implementation is still a stub!
tapout:
        PUSH    hl
        PUSH    af
        LD      hl,tapout_text
;                call    print_debug
        POP     af
        POP     hl
; TODO: not implemented -> always fail
        SCF
        RET
tapout_text:
        DB      "TAPOUT",0

;--------------------------------
; $00F0 TAPOOF
; Stops writing on the tape.
; NOTE: This implementation is still a stub!
tapoof:
        PUSH    hl
        PUSH    af
        LD      hl,tapoof_text
;                call    print_debug
        POP     af
        POP     hl
        RET
tapoof_text:
        DB      "TAPOOF",0

;--------------------------------
; $00F3 STMOTR
; Changes the cassette motor state.
; Input:   A = action: #00 stops motor, #01 starts motor,
;                      #FF inverts current state
; Changes: AF
stmotr:
        PUSH    bc
        LD      b,a
        IN      a,(GIO_REGS)
        INC     b
        JR      z,stmotr_inv
        SET     4,a
        DEC     b
        JR      z,stmotr_set
        RES     4,a
        DEC     b
        JR      z,stmotr_set
        POP     bc
        RET

stmotr_inv:
        XOR     16
stmotr_set:
        OUT     (GIO_REGS),a
        POP     bc
        RET

;--------------------------------
; $0090 GICINI  Initialize Sound IC
; Function : Initialises PSG and sets initial value for the PLAY statement
; Registers: All
gicini:
        LD      e,$00
        LD      a,$08
        CALL    wrtpsg
        INC     a
        CALL    wrtpsg
        INC     a
        CALL    wrtpsg
        INC     a

        LD      e,$B8
        LD      a,$07
        CALL    wrtpsg

        RET

;--------------------------------
; $0093 WRTPSG
; Function : Writes data to PSG-register
; Input    : A  - PSG register number
;            E  - data write
wrtpsg:
        DI
        OUT     (PSG_REGS),a
        PUSH    af
        LD      a,e
        OUT     (PSG_DATA),a
        EI
        POP     af
        RET

;--------------------------------
; $0096 RDPSG
; Function : ReaDS value from PSG-register
; Input    : A  - PSG-register read
; Output   : A  - value read
rdpsg:
        OUT     (PSG_REGS),a
        IN      a,(PSG_STAT)
        RET

;--------------------------------
; $0135 CHGSND
; Write to the 1-bit sound port.
; Input:   A = zero to set sound state to 0, non-zero to set sound state to 1
; Changes: AF
chgsnd:
        OR      a
        LD      a,$0E                             ; $0E = command to reset bit 7
        JR      z,chgsnd_write
        INC     a                                 ; $0F = command to set bit 7
chgsnd_write:
        OUT     (PPI_REGS),a                      ; set/reset bit of port C
        RET

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
        IN      a,(PSL_STAT)
        RET

;--------------------------------
; $013B WSLREG
; Function : Writes value to the primary slot register
; Input    : A  - value value to (see RSLREG)
wslreg:
        OUT     (PSL_STAT),a
        RET

;--------------------------------
; $013E RDVDP
; Function : Reads VDP status register
; Output   : A  - Value which was read
; Registers: A
rdvdp:
        IN      a,(VDP_STAT)
        RET

;--------------------------------
;0141h SNSMAT
; Function : Returns the value of the specified line from the keyboard matrix
; Input    : A  - for the specified line
; Output   : A  - for data (the bit corresponding to the pressed key will be 0)
; Registers: AF
snsmat:
        LD      A,$FF
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
        CALL    H_PHYD
        RET

;--------------------------------
; $0147 FORMAT
; Initialises mass-storage media like formatting of diskettes.
; All this routine does is call H_FORM, which should be installed by the main
; disk ROM.
; Changes:   all
format:
        CALL    H_FORM
        RET

;--------------------------------
; $014A ISFLIO
; Function : Tests if I/O to device is taking place
; Output   : A  - #00 if not taking place
;             not #00 if taking place
; Registers: AF
; TODO: call H_ISFL
isflio:
;                call    H_ISFL
        LD      a,(PTRFIL)
        AND     a                                 ; adjust flags
        RET

;--------------------------------
; $00D5 GTSTCK
; Function : Returns the joystick status
; Input    : A  - Joystick number to test (0 = cursors, 1 = port 1, 2 = port 2)
; Output   : A  - Direction,D = $00(when A=0)
; Registers: All
gtstck:
        PUSH    bc
        CP      $00
        JR      nz,joy_stc1

        IF      KB_USE_SERIAL=1
            CALL    key_in

            LD      A,(N8VEM_KB_STICK_CACHE)
            AND     $0F
            JP      Z,GSTICK_N8VEM_END                ; IF NO DATA END
            LD      B,A
            LD      A,(N8VEM_KB_STICK_CACHE)
            AND     $F0
            LD      (N8VEM_KB_STICK_CACHE),A
            LD      A,B
GSTICK_N8VEM_END:
            PUSH    hl
            LD      hl,joypos_kbd_tbl
            LD      d,0
            LD      e,a
            ADD     hl,de
            LD      a,(hl)
            POP     hl
        ENDIF
        POP     bc
        AND     a
        RET
joy_stc1:
;PSG reg 15h
;0J001111
;PSG reg 14h
;00BARLDU

        PUSH    hl
        PUSH    de

        LD      e,$00
        DEC     a
        JR      z,sel_stc1
; N8VEM SITCK 2
        LD      a,$0F
        JR      N8_STKREAD
sel_stc1:
        LD      A,$0E
N8_STKREAD:
        DI
        CALL    rdpsg
        EI
        CPL
        AND     $0F
        LD      hl,joypos_joy_tbl
        LD      b,0
        LD      c,a
        ADD     hl,bc
        LD      a,(hl)
        POP     de
        POP     hl

        POP     bc
        AND     a
        RET

joy_end:
        LD      a,$00
        POP     bc
        AND     a
        RET

joypos_joy_tbl:
;         0   1   2   3   4   5   6   7
        DB      $00,$01,$05,$00,$07,$08,$06,$07
;         8   9   A   B   C   D   E   F
        DB      $03,$02,$04,$03,$00,$01,$05,$00

joypos_kbd_tbl:
;         0   1   2   3   4   5   6   7
        DB      $00,$07,$01,$08,$05,$06,$00,$07
;         8   9   A   B   C   D   E   F
        DB      $03,$00,$02,$01,$04,$05,$03,$00


N8VEM_KB_STICK:
        LD      A,(N8VEM_KB_STICK_CACHE)
        CP      00
        JP      NZ,N8VEM_KB_STICK_END
        LD      A,B
        CP      'S'                               ;
        JP      Z,N8VEM_KB_STICK_R
        CP      'Z'                               ;
        JP      Z,N8VEM_KB_STICK_D
        CP      'W'                               ;
        JP      Z,N8VEM_KB_STICK_U
        CP      'A'                               ;
        JP      Z,N8VEM_KB_STICK_L
        CP      ' '                               ;
        JP      Z,N8VEM_KB_STICK_B
        JP      N8VEM_KB_STICK_NONE
N8VEM_KB_STICK_R:
        LD      A,%00001000
        LD      (N8VEM_KB_STICK_CACHE),A
        RET
N8VEM_KB_STICK_D:
        LD      A,%00000100
        LD      (N8VEM_KB_STICK_CACHE),A
        RET
N8VEM_KB_STICK_U:
        LD      A,%00000010
        LD      (N8VEM_KB_STICK_CACHE),A
        RET
N8VEM_KB_STICK_L:
        LD      A,%00000001
        LD      (N8VEM_KB_STICK_CACHE),A
        RET
N8VEM_KB_STICK_B:
        LD      A,%00010000
        LD      (N8VEM_KB_STICK_CACHE),A
        RET
N8VEM_KB_STICK_NONE:
        LD      A,%00000000
N8VEM_KB_STICK_END:
        RET
N8VEM_KB_STICK_CACHE:
        DB      00

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
        CP      5
        JR      nc,gttrig_space                   ; if value of A is above 5,go space routine
        OR      a
        JR      nz,joy_trig
gttrig_space:
; Keyboard (spacebar)
        IF      KB_USE_SERIAL=1
            CALL    key_in
            LD      A,(N8VEM_KB_STICK_CACHE)
            AND     $F0
            RET     Z                                 ; IF NO DATA END
            LD      A,(N8VEM_KB_STICK_CACHE)
            AND     $0F
            LD      (N8VEM_KB_STICK_CACHE),A
            LD      A,$FF
            RET
            .ELSE
            LD      A,$00
            RET
        ENDIF

; Joystick triggers
joy_trig:
        DI
        DEC     a
        PUSH    de
        LD      e,$03                             ; enable trig A+B of stick 1
        LD      b,a
        AND     $01
        JR      z,sel_trig1
; TRIGGER 2
        LD      a,$0F
        JR      N8_TRGREAD
sel_trig1:
        LD      A,$0E
N8_TRGREAD:
        DI
        CALL    rdpsg
        EI
        LD      E,A
        LD      A,B
        AND     02
        JR      Z,istrg_a
        LD      A,E
        POP     DE
        AND     $80
        JR      z,trig_on
        JR      trig_off
istrg_a:
        LD      A,E
        POP     DE
        AND     $10
        JR      z,trig_on
        JR      trig_off

trig_on:
        LD      a,$FF
        RET
trig_off:
        XOR     a
        RET


;--------------------------------
; $00db GTPAD
; Function : Returns current touch pad status
; Input    : A  - Touchpad number to test
; Output   : A  - Value
; Registers: All
; NOTE     : This implementation is still a stub!
gtpad:
        PUSH    hl
        PUSH    af
        LD      hl,gtpad_text
;                call    print_debug
        POP     af
        POP     hl
        XOR     a                                 ; haywire
        RET
gtpad_text:
        DB      "GTPAD",0

;--------------------------------
; $00DE GTPDL
; Function : Returns currenct value of paddle
; Input    : A  - Paddle number
; Output   : A  - Value
; Registers: All
; NOTE     : This implementation is still a stub!
gtpdl:
        PUSH    hl
        PUSH    af
        LD      hl,gtpdl_text
;                call    print_debug
        POP     af
        POP     hl
        RET
gtpdl_text:
        DB      "GTPDL",0

;--------------------------------
; $00F6 LFTQ
; Give the number of bytes left in a queue.
; In:      A  = queue number
; Out:     HL = number of bytes left
; Changes: AF, BC, HL
lftq:
        CALL    calc_queue_address
        LD      b,(hl)                            ; B = put position
        INC     b
        INC     hl
        LD      a,(hl)                            ; A = get position
        SUB     b                                 ; (getpos - putpos) & size
        INC     hl
        INC     hl
        AND     (hl)
        LD      l,a
        LD      h,$00                             ; Queues are smaller than 256 bytes.
        RET

;--------------------------------
; $00F9 PUTQ
; Put a byte in a queue.
; In:      A  = queue number
;          E  = data
; Out:     ZF = set if the queue is full
; Changes: AF, BC, HL
putq:
; Check whether the queue is full.
        CALL    calc_queue_address
        LD      a,(hl)
        INC     a
        LD      b,a                               ; B = put position + 1
        INC     hl
        SUB     (hl)
        RET     z

; Save the new put position.
        LD      a,b
        INC     hl
        INC     hl
        PUSH    hl
        AND     (hl)                              ; (putpos + 1) & size
        DEC     hl
        DEC     hl
        DEC     hl
        LD      (hl),a

; Put the data byte in the queue.
        POP     hl
        INC     hl
        LD      a,(hl)                            ; Get the buffer address.
        INC     hl
        LD      h,(hl)
        LD      l,a
        DEC     b                                 ; Add putpos.
        LD      c,b
        LD      b,0
        ADD     hl,bc
        LD      (hl),e
        OR      1
        RET

; Calculate the address to the start of queue control block.
; A = queue number
calc_queue_address:
        LD      hl,(QUEUES)                       ; See QUETAB in systemvars.asm.
        LD      b,a                               ; (queue number * 6)
        RLCA
        RLCA
        ADD     a,b
        ADD     a,b
        LD      c,a
        LD      b,0
        ADD     hl,bc
        RET

;--------------------------------
; $0132 CHGCAP
; Function : Alternates the CAP lamp status
; Input    : A  - #00 is lamp on
;             not #00 is lamp off
; Registers: AF
chgcap:
;// TODO: n8vem code
        RET

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
        PUSH    hl
        PUSH    af
        LD      hl,outdlp_text
;                call    print_debug
        POP     af
        POP     hl
        RET
outdlp_text:
        DB      "OUTDLP",0

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
        LD      l,2
        JR      getvc2_a

;--------------------------------
; $0153 GETVC2
; Returns pointer to a given variable in a voice structure.
; Input    : L        - Pointer in play buffer
;            (VOICEN) - Voice structure number
; Output   : HL - Pointer
; Registers: AF
getvc2:
        LD      a,(VOICEN)
getvc2_a:
        PUSH    de
        LD      d,0
        LD      e,l
        LD      hl,VCBA
        ADD     hl,de
        LD      e,37                              ; Size of a structure
getvc2_loop:
        OR      a
        JR      z,getvc2_exit
        ADD     hl,de
        DEC     a
        JR      getvc2_loop
getvc2_exit:
        POP     de
        RET

;--------------------------------
; $0156 KILBUF
; Empties the keyboard buffer.
; Changes: HL
kilbuf:
        LD      hl,(GETPNT)
        LD      (PUTPNT),hl
        RET

;--------------------------------
; Interrupt routine ($0038h)
;--------------------------------
; some games uses Reg.R and the routine affects the register's value.
; if you want to add something to the routine,please try the following first
;
; Riseout , Replicart

keyint:
        DI
        PUSH    hl
        PUSH    de
        PUSH    bc
        PUSH    af
        EXX
        EX      af,af'
        push    hl
        PUSH    de
        PUSH    bc
        PUSH    af
        PUSH    iy
        PUSH    ix

        CALL    H_KEYI
        IN      a,(VDP_STAT)
        OR      a
        LD      (STATFL),a                        ; save status
        JP      p,int_end                         ; exit if this is not the VDP int
        CALL    H_TIMI

; TODO: (BASIC related stuff)
;       Check sprite collision
;       Update INTCNT

        LD      hl,(JIFFY)
        INC     hl
        LD      (JIFFY),hl

; TODO: MUSICF

; TODO: It seems unsafe to me to already allow interrupts
;       while this one is still busy: possible interference
;       between two interrupts and also the amount of stack
;       space claimed is a lot.
;ei

; Riseout needs that count of RegR in the routine is not
; even number
; nop

        XOR     a
        LD      (CLIKFL),a



; Scan the keyboard every three interrupts.

        LD      a,(SCNCNT)
        DEC     a
        LD      (SCNCNT),a
        JR      nz,int_end
        LD      a,3
        LD      (SCNCNT),a

; TODO read joystick triggers and space for TRGFLG
        XOR     a
        CALL    gttrig
        CPL
        AND     $01
        LD      (TRGFLG),a

      ;  IF      KB_USE_SERIAL=1
      ;      CALL    key_in
      ;  ENDIF

      ;  IF      KB_USE_PS2=1
      ;      CALL    KB_PROCESS
      ;  ENDIF


int_end:
        POP     ix
        POP     iy
        POP     af
        POP     bc
        POP     de
        POP     hl
        EXX
        EX      af,af'
        pop     af
        POP     bc
        POP     de
        POP     hl
        EI
        RET

;--------------------------------
; 0066h NMI interrupt
nmi:
        CALL    H_NMI
        RETN

;--------------------------------
; Get buffer from keyboard input
key_in:
        IN      A,(UART5)                         ; READ LINE STATUS REGISTER
        BIT     0,A                               ; TEST IF DATA IN RECEIVE BUFFER
        JP      Z,N8VEM_KEY_END                   ; IF NO DATA END
        IN      A,(UART0)                         ; THEN READ THE CHAR FROM THE UART
        PUSH    AF                                ;
        PUSH    BC                                ;
        LD      B,A                               ;
        CALL    N8VEM_KB_STICK                    ;
        POP     BC                                ;
        POP     AF                                ;
        PUSH    hl                                ;
        CP      03                                ;
        JR      NZ,KEY_IN1                        ;
        LD      A,$FF                             ;
        LD      (BREAKFLAG),A                     ;
        JR      KEY_IN2                           ;
KEY_IN1:
        CALL    key_put_into_buf                  ;
KEY_IN2:
        POP     hl                                ;
N8VEM_KEY_END:
        RET


;--------------------------------
key_put_into_buf:
        LD      hl,(PUTPNT)
        LD      (hl),a
; Note: Ashguine 2 has a bug: it puts KEYBUF at FDF0 iso FBF0
;       in the name input routine. This writes keys in memory
;       reserved for hooks, but since those hooks are only used
;       by BASIC, the game doesn't suffer. When PUTPNT reaches
;       FE18, it wraps back to FBF0.
        INC     hl
        LD      a,l
;                cp      $00FF & (KEYBUF + 40)
        CP      $18
        JR      nz,key_store_nowrap
        LD      hl,KEYBUF
key_store_nowrap:
; Check whether the buffer is full.
        PUSH    de
        LD      de,(GETPNT)
        RST     20h
        POP     de
        RET     z
        LD      (PUTPNT),hl
        RET


;--------------------------------
; $015C SUBROM
; Function : Calls a routine in SUB-ROM
; Input    : IX - Address of routine in SUB-ROM
; Output   : DepenDS on the routine
; Registers: Alternative registers, IY
; Remark   : Use of EXTROM or CALSLT is more convenient.
;            You have to use this routine like this:
;               push    ix
;               jp      subrom
;            The purpose is unclear
subrom:
        POP     ix
        RET


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
        RET

;------------------------------------
hang_up_mode:
        JR      $

;------------------------------------
; Called if the stack underflows.
stack_error:
        CALL    H_STKE
        LD      de,str_stack_error
        JP      print_error

;------------------------------------
; $0159 CALBAS
; Function : Executes inter-slot call to the routine in BASIC interpreter
; Input    : IX - for the calling address
; Output   : DepenDS on the called routine
; Registers: DepenDS on the called routine
calbas:
        PUSH    hl
        PUSH    af
        LD      hl,calbas_text
;                call    print_debug
        POP     af
        POP     hl
        LD      de,str_no_basic_intr
        JP      print_error
calbas_text:
        DB      "CALBAS",0

;------------------------------------
;Display error
;in DE= message address

print_error:
        IN      a,(VDP_STAT)                      ; reset Latch
        LD      hl,vdp_bios
        LD      b,$0C
        LD      c,VDP_ADDR
        OTIR

        LD      bc,$0800
lp_clearmem:
        XOR     a
        OUT     (VDP_DATA),a
        DEC     bc
        LD      a,b
        OR      c
        JR      nz,lp_clearmem

        LD      hl,B_Font
        LD      bc,$0800
lp_fontset:
        LD      a,(hl)
        OUT     (VDP_DATA),a
        INC     hl
        DEC     bc
        LD      a,b
        OR      c
        JR      nz,lp_fontset

;set cursor to (0,0)
        LD      a,$00
        OUT     (VDP_ADDR),a
        LD      a,$40
        OUT     (VDP_ADDR),a

        LD      hl,str_error_prompt

        LD      a,(hl)
lp_errprn:
        OUT     (VDP_DATA),a
        INC     hl
        LD      a,(hl)
        AND     a
        JR      nz,lp_errprn

        LD      a,(de)
lp_strprn:
        OUT     (VDP_DATA),a
        INC     de
        LD      a,(de)
        AND     a
        JR      nz,lp_strprn

        JP      hang_up_mode

        DS      $1bbf - $
        INCLUDE "font.asm"

        INCLUDE "slot.asm"

;---------------------------------
; system messages
;---------------------------------

str_proginfo:
;       [01234567890123456789012345678]
        DB      "N8VEM BIOS 0.1     MSX C-BIOS"
;include "../derived/asm/version.asm"
        DB      $0D,$0A,$0D,$0A,$0D,$0A,$00


;-------------------------------------
; error messages
str_error_prompt:
        DB      "ERROR:",$00

str_no_basic_intr:
        DB      "CALLED NON EXISTING BASIC.",$00

str_stack_error:
        DB      "STACK ERROR.",$00
str_nocart:
        DB      "NO CARTRIDGE FOUND.",$00

;-------------------------------------
vdp_bios:
        DB      $00,$80,$70,$81,$00,$82,$01,$84
        DB      $F5,$87,$00,$40
;	INCLUDE "PS2KEYB.ASM"
;   INCLUDE "ROMMON.asm"
;   INCLUDE "BASIC.asm"

BREAKFLAG:
        DB      $00

        DS      $3FFF - $
        DB      $00

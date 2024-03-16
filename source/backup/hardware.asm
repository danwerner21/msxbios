; $Id: hardware.asm 525 2008-12-22 22:16:42Z mthuurne $
; C-BIOS hardware related declarations
;
; Copyright (c) 2002-2005 BouKiCHi.  All rights reserved.
; Copyright (c) 2003 Reikan.  All rights reserved.
; Copyright (c) 2004-2005 Maarten ter Huurne.  All rights reserved.
; Copyright (c) 2004 Manuel Bilderbeek.  All rights reserved.
; Copyright (c) 2004-2006 Albert Beevendorp.  All rights reserved.
; Copyright (c) 2004-2005 Joost Yervante Damad.  All rights reserved.
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

;---------------------------------------------------
; I/O ports

        IF      PLATFORM=1
DBG_CTRL:           EQU $01                           ; openMSX debugdevice control (mode)
DBG_DATA:           EQU $01                           ; openMSX debugdevice data

PRN_STAT:           EQU $01                           ; printer status
PRN_DATA:           EQU $01                           ; printer data

PSL_STAT:           EQU $01                           ; slot status
KBD_STAT:           EQU $01                           ; keyboard status
GIO_REGS:           EQU $01                           ; General IO Register
PPI_REGS:           EQU $01                           ; PPI register

RTC_ADDR:           EQU $01                           ; RTC address
RTC_DATA:           EQU $01                           ; RTC data

MAP_REG1:           EQU $01                           ; memory mapper: bank in $0000-$3FFF
MAP_REG2:           EQU $01                           ; memory mapper: bank in $4000-$7FFF
MAP_REG3:           EQU $01                           ; memory mapper: bank in $8000-$BFFF
MAP_REG4:           EQU $01                           ; memory mapper: bank in $C000-$FFFF
        ELSE
DBG_CTRL:           EQU $2E                           ; openMSX debugdevice control (mode)
DBG_DATA:           EQU $2F                           ; openMSX debugdevice data

PRN_STAT:           EQU $90                           ; printer status
PRN_DATA:           EQU $91                           ; printer data

PSL_STAT:           EQU $A8                           ; slot status
KBD_STAT:           EQU $A9                           ; keyboard status
GIO_REGS:           EQU $AA                           ; General IO Register
PPI_REGS:           EQU $AB                           ; PPI register

RTC_ADDR:           EQU $B4                           ; RTC address
RTC_DATA:           EQU $B5                           ; RTC data

MAP_REG1:           EQU $FC                           ; memory mapper: bank in $0000-$3FFF
MAP_REG2:           EQU $FD                           ; memory mapper: bank in $4000-$7FFF
MAP_REG3:           EQU $FE                           ; memory mapper: bank in $8000-$BFFF
MAP_REG4:           EQU $FF                           ; memory mapper: bank in $C000-$FFFF
        ENDIF

        IF      PLATFORM=1
VDP_DATA:           EQU $A0                           ; VDP data port (VRAM read/write)
VDP_ADDR:           EQU $A1                           ; VDP address (write only)
VDP_STAT:           EQU $A1                           ; VDP status (read only)
VDP_ACR:            EQU $A6                           ; VDP ACCESS CONTROL REGISTER
        ELSE
VDP_DATA:           EQU $98                           ; VDP data port (VRAM read/write)
VDP_ADDR:           EQU $99                           ; VDP address (write only)
VDP_STAT:           EQU $99                           ; VDP status (read only)
VDP_PALT:           EQU $9A                           ; VDP palette latch (write only)
VDP_REGS:           EQU $9B                           ; VDP register access (write only)
VDP_ACR:            EQU $9C                           ; VDP ACCESS CONTROL REGISTER
        ENDIF

        IF      PLATFORM=1
PSG_REGS:           EQU $A4                           ; PSG register write port
PSG_DATA:           EQU $A5                           ; PSG value write port
PSG_STAT:           EQU $A4                           ; PSG value read port

        ELSE
;PSG_REGS:       EQU     $A0             ; PSG register write port
;PSG_DATA:       EQU     $A1             ; PSG value write port
;PSG_STAT:       EQU     $A2             ; PSG value read port

PSG_REGS:           EQU $9A                           ; PSG register write port
PSG_DATA:           EQU $9B                           ; PSG value write port
PSG_STAT:           EQU $9A                           ; PSG value read port
        ENDIF

        IF      PLATFORM=1
UART0               EQU $58                           ; DATA IN/OUT
UART1               EQU $59                           ; INTERRUPT ENABLE
UART2               EQU $5A                           ; INTERRUPT IDENTIFICATION/FIFO CONTROL
UART3               EQU $5B                           ; LINE CONTROL
UART4               EQU $5C                           ; MODEM CONTROL
UART5               EQU $5D                           ; LINE STATUS
UART6               EQU $5E                           ; MODEM STATUS
UART7               EQU $5F                           ; SCRATCH REG.
        ELSE
KBPPIA              EQU 0F4h                          ; PPI PORT A
KBPPIB              EQU 0F5h                          ; PPI PORT B
KBPPIC              EQU 0F6h                          ; PPI PORT C
KBPPICONT           EQU 0F7h                          ; PPI CONTROL PORT

UART0:              EQU 068H                          ;   DATA IN/OUT
UART1:              EQU 069H                          ;   CHECK RX
UART2:              EQU 06AH                          ;   INTERRUPTS
UART3:              EQU 06BH                          ;   LINE CONTROL
UART4:              EQU 06CH                          ;   MODEM CONTROL
UART5:              EQU 06DH                          ;   LINE STATUS
        ENDIF



;---------------------------------------------------
; memory mapped I/O

SSL_REGS:       EQU $FFFF                         ; secondary slot select registers

;---------------------------------------------------
; Constants used to define which hardware the BIOS will run on.
; Used by the main_<model>.asm sources.

; VDP models:
TMS99X8         EQU $9918
V9938           EQU $9938
V9958           EQU $9958

; MSX models:
MODEL_MSX1      EQU 0
MODEL_MSX2      EQU 1
MODEL_MSX2P     EQU 2
MODEL_MSXTR     EQU 3

MODEL_SUBROM    EQU 4

; Locales:
; -- ID byte 0
LOCAL_CHSET_JP  EQU $00
LOCAL_CHSET_US  EQU $01
LOCAL_CHSET_KO  EQU $02

LOCAL_DATE_YMD  EQU $00
LOCAL_DATE_MDY  EQU $10
LOCAL_DATE_DMY  EQU $20

LOCAL_INT_60HZ  EQU $00
LOCAL_INT_50HZ  EQU $80

; -- ID byte 1
LOCAL_KBD_JP    EQU $00
LOCAL_KBD_US    EQU $01
LOCAL_KBD_FR    EQU $02
LOCAL_KBD_UK    EQU $03
LOCAL_KBD_DE    EQU $04

LOCAL_BASIC_JP  EQU $00
LOCAL_BASIC_US  EQU $01

KB_USE_SERIAL   EQU 1                             ; SET TO 1 FOR SERIAL INPUT
KB_USE_PS2      EQU 0                             ; SET TO 1 FOR PS/2 INPUT

; BOOLEAN VALUES
YES             EQU 1
NO              EQU 0

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:

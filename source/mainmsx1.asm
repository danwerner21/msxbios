; $Id: main_msx1.asm 525 2008-12-22 22:16:42Z mthuurne $
; C-BIOS main ROM for a European MSX1 machine
;
; Copyright (c) 2005 Maarten ter Huurne.  All rights reserved.
; Copyright (c) 2005 Joost Yervante Damad.  All rights reserved.
; Copyright (C) 2005 BouKiCHi. All rights reserved.
; Copyright (C) 2008 Eric Boon. All rights reserved.
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

                .include "hardware.asm"

VDP:            .equ     TMS99X8
MODEL_MSX:      .equ     MODEL_MSX1
N8VEM		.equ	 1

; -- generic US config (60Hz, US keyboard layout)
LOCALE_CHSET:   .equ     LOCAL_CHSET_US
LOCALE_DATE:    .equ     LOCAL_DATE_MDY
LOCALE_INT:     .equ     LOCAL_INT_60HZ
LOCALE_KBD:     .equ     LOCAL_KBD_US
LOCALE_BASIC    .equ     LOCAL_BASIC_US

CALL_SUB:       .equ     NO

                .include "main.asm"

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:

	.END

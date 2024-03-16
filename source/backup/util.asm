; $Id: util.asm 525 2008-12-22 22:16:42Z mthuurne $
; C-BIOS utility routines
;
; Copyright (c) 2004 Maarten ter Huurne.  All rights reserved.
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

;----------------------------------
; Generic routine that implements a jump table.
; Input:   HL = address of jump table
;          A  = index in table
; Changes: F, HL, BC (these are changed *before* jumping)
jump_table:
        LD      c,a
        LD      b,0
        ADD     hl,bc
        ADD     hl,bc
        JP      table_jump

;----------------------------------
; Generic routine that implements a search & jump table
; Entries in the table consist of (character, jump address) pairs.
; If the entry is not found, the routine will just RETurn.
;
; Input:   HL = address of search table
;          A  = item to search for
;          B  = number of entries in the table
; Changes: F, BC, HL
search_table:
        CP      (hl)
        INC     hl
        JR      z,table_jump
        INC     hl
        INC     hl
        DJNZ    search_table
        RET
table_jump:
        LD      c,(hl)
        INC     hl
        LD      h,(hl)
        LD      l,c
        JP      (hl)

; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:

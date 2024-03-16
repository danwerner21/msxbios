; $Id: hooks.asm 525 2008-12-22 22:16:42Z mthuurne $
; C-BIOS hook declarations
;
; Copyright (c) 2002-2003 BouKiCHi.  All rights reserved.
; Copyright (c) 2003 Reikan.  All rights reserved.
; Copyright (c) 2004 Maarten ter Huurne.  All rights reserved.
; Copyright (c) 2004 Manuel Bilderbeek.  All rights reserved.
; Copyright (c) 2004-2006 Joost Yervante Damad.  All rights reserved.
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
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSequENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;

; called at start of interrupt subroutine, before it has been checked if
; the interrupt was from VDP; used by e.g. RS232
H_KEYI:         EQU $FD9A

; called at start of interrupt subroutine, when it is clear that
; the interrupt is from the VDP
H_TIMI:         EQU $FD9F

; called at start of CHPUT(00A2)
H_CHPU:         EQU $FDA4

; called at start of the subroutine drawing the cursor
H_DSPC:         EQU $FDA9

; called at start of the subroutine that removes the cursor
H_ERAC:         EQU $FDAE

; called at start of DSPFNK(00CF)
H_DSPF:         EQU $FDB3

; called at start of ERAFNK(00CC)
H_ERAF:         EQU $FDB8

; called at start of TOTEXT(00D2)
H_TOTE:         EQU $FDBD

; called at start of CHGET(009F)
H_CHGE:         EQU $FDC2

; called at start of the subroutine that fills the pattern-table
; can be used to override the default ASCII patterns
H_INIP:         EQU $FDC7

; called at part of subroutine that decodes combined keystrokes
; like with CTRL/CODE/GRAPH/SHIFT to an ASCII code; can be used
; to override the working of the keyboard
H_KEYC:         EQU $FDCC

; called at start of the subroutine that decodes single keystrokes;
; can be used to override the working of the keyboard
H_KYEA:         EQU $FDD1

; called at start of NMI interrupt subroutine
H_NMI:          EQU $FDD6

; called at start of PINLIN(00AE)
H_PINL:         EQU $FDDB

; called at start of QINLIN(00B4)
H_QINL:         EQU $FDE0

; called at start of INLIN(00B1)
H_INLI:         EQU $FDE5

; BASIC interpreter hook
H_ONGO:         EQU $FDEA

; implementation hook for DSKO$
H_DSKO:         EQU $FDEF

; implementation hook for SET
H_SETS:         EQU $FDF4

; implementation hook for NAME
H_NAME:         EQU $FDF9

; implementation hook for KILL
H_KILL:         EQU $FDFE

; implementation hook for IPL
H_IPL:          EQU $FE03

; implementation hook for COPY
H_COPY:         EQU $FE08

; implementation hook for CMD
H_CMD:          EQU $FE0D

; implementation hook for DSKF
H_DSKF:         EQU $FE12

; implementation hook for DSKI$
H_DSKI:         EQU $FE17

; implementation hook for ATTR$
H_ATTR:         EQU $FE1C

; implementation hook for LSET
H_LSET:         EQU $FE21

; implementation hook for RSET
H_RSET:         EQU $FE26

; implementation hook for FIELD
H_FIEL:         EQU $FE2B

; implementation hook for MKI$
H_MKIS:         EQU $FE30

; implementation hook for MKS$
H_MKSS:         EQU $FE35

; implementation hook for MKD$
H_MKDS:         EQU $FE3A

; implementation hook for CVI
H_CVI:          EQU $FE3F

; implementation hook for CVS
H_CVS:          EQU $FE44

; implementation hook for CVD
H_CVD:          EQU $FE49

; called when looking up the value of PTRFIL(F864) for DISKBASIC
H_GETP:         EQU $FE4E

; called when PTRFIL(F864) is being given a new value
H_SETF:         EQU $FE53

; called when an OPEN statement was issued without a FOR-part
; part of DISKBASIC
H_NOFO:         EQU $FE58

; called for an operation for file-buffer 0, in DISKBASIC
H_NULO:         EQU $FE5D

; called from DISKBASIC for a call with file-buffer not 0
H_NTFL:         EQU $FE62

; called when doing a MERGE command for disks
H_MERG:         EQU $FE67

; called when doing a SAVE commands for disks
H_SAVE:         EQU $FE6C

; called when doing a BSAVE command for disks
H_BINS:         EQU $FE71

; called when doing a BLOAD command for disks
H_BINL:         EQU $FE76

; implementation hook for FILES
H_FILE:         EQU $FE7B

;  DISKBASIC hook
H_DGET:         EQU $FE80

;  DISKBASIC hook
H_FILO:         EQU $FE85

;  DISKBASIC hook
H_INDS:         EQU $FE8A

; DISKBASIC entry for selecting the previous disk station for disk IO
H_RSLF:         EQU $FE8F

; DISKBASIC entry for remembering the current disk station
H_SAVD:         EQU $FE94

; implementation hook for LOC
H_LOC:          EQU $FE99

; implementation hook for LOF
G_LOF:          EQU $FE9E

; called when doing EOF for a disk in DISKBASIC
H_EOF:          EQU $FEA3

; implementation hook for FPOS
H_FPOS:         EQU $FEA8

; DISKBASIC hook
H_BAKU:         EQU $FEAD

; called when BASIC interpreter is decoding the device part of a filename
H_PARD:         EQU $FEB2

; called when BASIC interpreter finds a file without a device part
H_NODE:         EQU $FEB7

; DISKBASIC hook
H_POSD:         EQU $FEBC

; called when searching a device by name
H_DEVN:         EQU $FEC1

; BASIC interpreter hook
H_GEND:         EQU $FEC6

; Called when clearing variables during the preparation of a RUN statement.
; Also used by the disk ROM to start boot sequence.
H_RUNC:         EQU $FECB

; called when doing CLEAR
H_CLEA:         EQU $FED0

; BASIC interpreter hook
H_LOPD:         EQU $FED5

; BASIC interpreter hook; called at stack error
H_STKE:         EQU $FEDA

; called at the start of ISFLIO(014A)
H_ISFL:         EQU $FEDF

; called at the start of OUTDO(0018)
H_OUTD:         EQU $FEE4

; BASIC interpreter hook
H_CRDO:         EQU $FEE9

; BASIC interpreter hook
H_DSKC:         EQU $FEEE

; called at the end of a BASIC program
H_PRGE:         EQU $FEF8

; BASIC interpreter hook
H_ERRP:         EQU $FEFD

; BASIC interpreter hook
H_ERRF:         EQU $FF02

; BASIC interpreter hook
H_READ:         EQU $FF07

; BASIC interpreter hook
H_MAIN:         EQU $FF0C

; called when executing a BASIC statement in direct mode
H_DIRD:         EQU $FF11

; BASIC interpreter hook
H_FINI:         EQU $FF16

; BASIC interpreter hook
H_FINE:         EQU $FF1B

; called while encoding a just typed BASIC statement
H_CRUN:         EQU $FF20

; called while encoding a just typed BASIC statement
H_CRUS:         EQU $FF25

; called when a keyword has been found while encoding a just typed
; BASIC statement
H_ISRE:         EQU $FF2A

; called when a function has been found while encoding a just typed
; BASIC statement
H_NTFN:         EQU $FF2F

; called when a non-keyword has been found while encoding a just
; typed BASIC statement
H_NOTR:         EQU $FF34

; BASIC interpreter hook
H_SNGF:         EQU $FF39

; BASIC interpreter hook
H_NEWS:         EQU $FF3E

; BASIC interpreter hook
H_GONE:         EQU $FF43

; called at start of CHRGTR(0010)
H_CHRG:         EQU $FF48

; BASIC interpreter hook
H_RETU:         EQU $FF4D

; BASIC interpreter hook
H_PRTF:         EQU $FF52

; BASIC interpreter hook
H_COMP:         EQU $FF57

; BASIC interpreter hook
H_FINP:         EQU $FF5C

; BASIC interpreter hook
H_TRMN:         EQU $FF61

; BASIC interpreter hook
H_FRME:         EQU $FF66

; BASIC interpreter hook
H_NTPL:         EQU $FF6B

; called when calculating the value of an expression in BASIC
H_EVAL:         EQU $FF70

; BASIC interpreter hook
H_OKNO:         EQU $FF75

; BASIC interpreter hook
H_FING:         EQU $FF7A

; called when setting a value to a substring with MID$
H_ISMI:         EQU $FF7F

; called when executing the WIDTH statement
H_WIDT:         EQU $FF84

; called when executing the LIST statement
H_LIST:         EQU $FF89

; BASIC interpreter hook
H_BUFL:         EQU $FF8E

; BASIC interpreter hook
H_FRQI:         EQU $FF93

; BASIC interpreter hook
H_SCNE:         EQU $FF98

; BASIC interpreter hook
H_FRET:         EQU $FF9D

; called when looking up a variable in BASIC
H_PTRG:         EQU $FFA2

; called from within PHYDIO(0144), to allow its implementation
H_PHYD:         EQU $FFA7

; called from within FORMAT(147), to allow its implementation
H_FORM:         EQU $FFAC

; called form the error-handling routine of the BASIC interpreter
H_ERRO:         EQU $FFB1

; called at start of LPTOUT(00A5)
H_LPTO:         EQU $FFB6

; called at start of LPTSTT(00A8)
H_LPTS:         EQU $FFBB

; called when executing SCREEN
H_SCRE:         EQU $FFC0

; called when executing PLAY
H_PLAY:         EQU $FFC5

; allows for installation of expansion devices that contain extra OS subroutines
H_BEXT:         EQU $FFCA


; vim:ts=8:expandtab:filetype=z8a:syntax=z8a:

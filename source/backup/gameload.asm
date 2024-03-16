;___GAME_LOADER________________________________________________________________________________________
;
;
;  CREATED BY : 	ANDREW LYNCH 15-FEB-2007
;  MODIFIED BY : 	DAN WERNER 11.7.2010
;
;______________________________________________________________________________________________________




;*******************************************************************
;*	START AFTER RESET
;*	FUNCTION	: READY SYSTEM, LOAD MONITOR INTO RAM AND START
;*******************************************************************

        ORG     00100h

        DI                                        ; DISABLE INTERRUPT

        LD      HL,BEGIN_CODE                     ; FROM
        LD      DE,$FF00                          ; TO
        LD      BC,$0030                          ; NUMBER OF BYTES TO MOVE
        LDIR                                      ; PERFORM BLOCK COPY
        JP      $FF00                             ; JUMP TO START


BEGIN_CODE:
        LD      HL,END_CODE                       ; FROM
        LD      DE,$0000                          ; TO
        LD      BC,$D000                          ; NUMBER OF BYTES TO MOVE
        LDIR                                      ; PERFORM BLOCK COPY
        JP      $0000                             ; JUMP TO START
END_CODE:

CART_SLOT_1_ADDR_HI EQU $10
CART_SLOT_1_ADDR_LO EQU $11
CART_SLOT_1_ADDR_CONT EQU $12
CART_SLOT_1_ADDR_READ EQU $13


;-------------------------------------
; 000Ch RDSLT
; Reads a value from an address in another slot.
; Input:   A  = slot ID: E000SSPP
;          HL = address to read
; Output:  A  = value read
;          Interrupts disabled.
; Changes: F, C, DE
rdslt:
        PUSH    bc
        PUSH    hl
        DI
        PUSH    AF
        AND     %00001100                         ; N8VEM SUPPORTS CART READER IN SLOT 1
        CP      %00000100
        JP      NZ,RDSLTEXIT                      ;
        POP     AF
        AND     %00000011                         ; N8VEM SUPPORTS CART PAGES 1 AND 2
        CP      %00000001                         ; SLOT 1?
        JP      NZ,RDSLTNOTP1                     ;
        LD      A,H                               ;
        OUT     (CART_SLOT_1_ADDR_HI),A
        LD      A,L                               ;
        OUT     (CART_SLOT_1_ADDR_LO),A
        LD      A,%00000110
        OUT     (CART_SLOT_1_ADDR_CONT),A
        IN      A,(CART_SLOT_1_ADDR_READ)
        JP      RDSLTEXIT2
RDSLTNOTP1:
        CP      %00000010                         ; SLOT 2?
        JP      NZ,RDSLTEXIT1                     ;
        LD      A,H                               ;
        OUT     (CART_SLOT_1_ADDR_HI),A
        LD      A,L                               ;
        OUT     (CART_SLOT_1_ADDR_LO),A
        LD      A,%00000101
        OUT     (CART_SLOT_1_ADDR_CONT),A
        IN      A,(CART_SLOT_1_ADDR_READ)
        JP      RDSLTEXIT2
RDSLTEXIT:
        POP     AF
RDSLTEXIT1:
        LD      A,$FF
RDSLTEXIT2:
; TODO: SUPPORT READING SLOTS 0 AND 2 (FROM ALT BANKS ON HC)
        EI
        POP     hl                                ; HL = address
        POP     bc
        RET

rdsft:
        INC     b
        DEC     b
        RET     z
rdsft_lp:
        RLCA
        RLCA
        DJNZ    rdsft_lp
        RET

;-------------------------------------
; $0014 WRSLT
; Writes a value to an address in another slot.
; Input:   A  = slot ID: E000SSPP
;          HL = address to write
;          E  = value to write
; Output:  Interrupts disabled.
; Changes: AF, BC, D
wrslt:
;  WRITING TO A SLOT IS NOT SUPPORTED WITH N8VEM HARDWARE
        RET

;-------------------------------------
; $001C CALSLT
; Function : Executes inter-slot call.
; Input    : IY - High byte with input for A in RDSLT
;            IX - The address that will be called
; Remark   : Variables can never be given in alternative registers
;            of the Z-80 or IX and IY

calslt:
; EXECUTING CODE FROM CARTS IS NOT SUPPORTED ON THE N8VEM
; TODO: USE BANK SWITCHING TO COPY CODE INTO A RAM BANK AND EXECUTE THERE
        RET

;--------------------------------
; 0024h ENASLT
; in .. hl=address, a=slot�ԍ�
; A = FxxxEESS
; RegA �ڍ�
; F = �g���X���b�g�̃t���O
; E = �g���X���b�g�ԍ�
; S = �X���b�g�ԍ�
; Dest. AF,BC,DE,DI

enaslt:
; TODO: USE BANK SWITCHING TO COPY CODE INTO A RAM BANK AND THEN ENABLE
        RET

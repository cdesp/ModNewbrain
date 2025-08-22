;Color screen
		GLOBAL NBCOLSCR
		
		GLOBAL MDLINT_SHOWCUR
		GLOBAL MDLINT_PRNEDT
                GLOBAL MDLINT_PUT31
                GLOBAL MDLINT_BLANK
                GLOBAL MDLINT_ZERO
                GLOBAL MDLINT_BLANK2
                GLOBAL MDLINT_SCROLL
                GLOBAL MDLINT_GRPLT
                GLOBAL MDLINT_GRWIP
                GLOBAL MDLINT_GRFIL
                GLOBAL MDLINT_GRCLS
		
		TVCUR EQU $005A
                CHAR EQU $0E
                EL EQU $06
                BLANK EQU $20
                ADHLA EQU $EC85

VidStart equ 642
ColorVidStart equ 0

		
;MDLINT_SHOWCUR:	LD HL,(TVCUR)           ;e1f1
 ;               RET
                ;todo: save previous color when cursor <> char and restore after
;	JR MDL_SETCOLOR
		
MDLINT_PRNEDT:	LD A,(IX+CHAR)          ;e9f1
                JR MDL_SETCOLOR
		RET

MDLINT_PUT31:	LD A,(IX+EL)            ;ECD7
                JR MDL_SETCOLOR
		RET

MDLINT_BLANK:	LD (HL),BLANK           ;ECDF
                CALL MDL_SETCOLOR                         
                INC HL                
		RET

;MDLINT_ZERO:	CALL ADHLA              ;E8FB
                ;RET
		;JR MDL_SETCOLOR
               
;MDLINT_BLANK2:	INC HL                  ;E91B
                ;LD (HL),BLANK
                ;RET
		;JR MDL_SETCOLOR

MDLINT_SCROLL:  PUSH HL                 ;EBE6
                INC D
                DEC D
                JR NZ, NOSCROLL         ; DE SHOULD BE ZERO TO SCROLL SCREEN
                LD HL,$FFFF
                CALL MDL_SETCOLOR
NOSCROLL:       POP HL                
                PUSH HL                 ;ORIG CODE
                PUSH DE
                PUSH BC
                JP $EBE9        ;RET TO THE NB rom


MDL_SETCOLOR:   PUSH AF
		PUSH HL
                PUSH DE
                PUSH BC
                    ; --- Relative monochrome byte index ---
                LD DE,VidStart
                OR A                  ; clear carry
                SBC HL,DE             ; HL = HL - VidStart
                    ; --- Add ColorVidStart ---
                LD DE,ColorVidStart
                ADD HL,DE             ; HL = final color byte address
                LD B,H
                LD A,L
                LD C,DEVCLR+1
                OUT (C),A
                POP BC
                POP DE
                POP HL
                POP AF
                RET

NOPOINT EQU $B1BE

;BEFORE RST8 A HAS THE BIT TO SET
MDLINT_GRPLT:   JP Z,NOPOINT    ;B1BA orig code
                CALL MDL_SETGCLR
                RST 8 ;orig code plot point according to colour	
                ;HL THE ADDR TO WRITE, A THE pixel (1,2,4,...) 
                JP $B1BD ;CONTINUE

MDLINT_GRFIL:   LD A,C; ORIG CODE        ;$AD56
                CALL MDL_SETGCLR
                RST 8 ;orig code
                LD (HL),A  ;orig code               
                RET

MDLINT_GRWIP:   PUSH HL ;ADDRESS
                PUSH BC ;BYTES TO WRITE
                LDIR ;original  code  $A7B3
                ;ourcode
                POP BC
                POP HL
                INC BC
NXTADDR:        CALL MDL_SETGCOLOR
                INC HL
                DEC BC
                LD A,B
                OR C 
                JR NZ,NXTADDR
                ;end our code
                RET  ;original code

;AFTER CLOSING SCREEN WE MUST PUT THE TEXT COLORS
MDLINT_GRCLS:   PUSH HL
                PUSH BC
                DEC DE ;orig code
                LDDR ;ORIG CODE
                POP BC
                POP HL
                INC BC
                INC BC
NXTADDR2:       CALL MDL_SETCOLOR       ;THIS IS CORRECT SETING TEXT COLORS ON CLOSE GRPH
                DEC HL
                DEC BC
                LD A,B
                OR C 
                JR NZ,NXTADDR2
                ;end our code
                RET  


;HL THE ADDRESS
MDL_SETGCOLOR:  PUSH AF
                PUSH BC
                LD B,H
                LD A,L
                LD C, DEVCLR + 2
                OUT (C),A
                LD C, DEVCLR + 4
                OUT (C),A
                POP BC
                POP AF
                RET



MDL_SETGCLR:
    PUSH AF
    PUSH BC
    PUSH HL
    PUSH DE

    ; --- Find bit index ---
    LD D,A
    LD C,0
BITIDX_LOOP:
    SRL A
    JR C,BITIDX_FOUND
    INC C
    JR BITIDX_LOOP
BITIDX_FOUND:
    LD B,C                ; B = bit index 0..7

    ; --- Relative monochrome byte index ---
    LD DE,VidStart
    OR A                  ; clear carry
    SBC HL,DE             ; HL = HL - VidStart

    ; --- Multiply by 4 ---
    ADD HL,HL             ; *2
    ADD HL,HL             ; *4

    ; --- Add (bit_index >> 1) ---
    SRL B                 ; B = bit_index / 2
    LD E,B
    LD D,0
    ADD HL,DE

    ; --- Add ColorVidStart ---
    LD DE,ColorVidStart
    ADD HL,DE             ; HL = final color byte address

    ; --- PixelSide = bit_index AND 1 ---
    LD A,C                ; original bit index in C
    AND 1
    LD D,A                ; D = PixelSide

    ; --- Select port based on pixel side ---
    LD A,D            ; Pixel side: 0 = right, 1 = left
    OR A
    JR Z, PIXEL_RIGHT

PIXEL_LEFT:
    LD C, DEVCLR + 2
    JR PIXEL_OUT

PIXEL_RIGHT:
    LD C, DEVCLR + 4

PIXEL_OUT:
    ; --- Output 16-bit address ---
    LD B,H
    LD A,L
    OUT (C),A

    ; --- Restore ---
    POP DE
    POP HL
    POP BC
    POP AF
    RET
     
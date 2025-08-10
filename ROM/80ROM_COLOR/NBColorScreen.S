;Color screen
		GLOBAL NBCOLSCR
		
		GLOBAL MDLINT_SHOWCUR
		GLOBAL MDLINT_PRNEDT
                GLOBAL MDLINT_PUT31
                GLOBAL MDLINT_BLANK
                GLOBAL MDLINT_ZERO
                GLOBAL MDLINT_BLANK2
		
		TVCUR EQU $005A
                CHAR EQU $0E
                EL EQU $06
                BLANK EQU $20
                ADHLA EQU $EC85
		
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
                                        


MDL_SETCOLOR:   PUSH AF
		;PUSH HL
                ;PUSH DE
                PUSH BC
                LD B,H
                LD A,L
                LD C,DEVCLR+1
                OUT (C),A
                POP BC
                ;POP DE
                ;POP HL
                POP AF
                RET

                ;STORAGE DEVICE DRIVER
                ;
                
                GLOBAL SG2IO

                WMKBUFF EQU 037EFH


;A=DRIVER NUMBER
;D=PORT NUMBER
;E=STREAM NUMBER
;HL=PARAMETER STRING
;BC=LENGTH OF PARAMETER STRING
;RET CY SET,A=ERROR CODE
;RET CY CLR,BCHL=RETURNED PARAMETER STRING
;PRESERVE IY AND ALT REGS
;RET BC=0 IF NOT USED IS BETTER


SG2IO:		EQU $
		DEFB 4                                                          ;COUNT OF DISPLACEMENTS-1
		DEFB SG2OIN-$ 							;OPEN IN
		DEFB SG2OOT-$ 							;OPEN OUT
		DEFB SG2INP-$ 							;INPUT BYTE
		DEFB SG2OTP-$ 							;OUTPUT BYTE
		DEFB SG2CLS-$ 							;CLOSE

;COPIES THE FNAME ON HL AND BC THE FILENAME CHAR COUNT TO THE BUFFER
CPYFNAME:       LD DE,STRGBUFF
                LDIR
                RET

;STRUCT FOR FILE OPEN
;       1 PORT NUMBER                   1 BYTE
;       2 CURRENT POSITION ON FILE      2 BYTES
;       3 FILENAME                      20 BYTES        
;

SG2OIN:										;OPENIN
		
SG2OOT:		PUSH HL
                PUSH BC 							;OPENOUT
		LD BC, 23 							;NO OF BYTES REQUIRED
		DEFW WMKBUFF 							;
		POP BC                 
		JR C, ERREXIT                
;HL HAS THE BUFFER ADDR
		LD (HL), E 							;STREAM NUMBER
                INC HL
                LD (HL),0                                                       ;POS ZERO
                INC HL
                LD (HL),0
                INC HL
                PUSH HL
                POP DE                                                          ;DE OUR BUFFER+3        
                POP HL                                                          ;HL THE FILE NAME
                PUSH DE                                                         ;SAVE THE START OF FILENAME        
                LDIR                                                            ;TRANSFER THE FNAME TO OUR STRUCT        
                XOR A
                LD (DE),A                                                       ;NULL TERMINATED STRING
                ;TEST IF FILE EXISTS
                POP HL                                                          ;START OF FNAME
                LD BC,0
                INC DE
                CALL STRG_LOADFILE
                LD A,201                                                        ;ERROR NO IF FILE ERROR
                RET C
                XOR A
                RET


ERREXIT:        POP HL
                SCF
                RET

SG2CLS:		XOR A 								; CLOSE STREAM
		LD B A 								;
		LD C A 								;
		RET 								;

;OPENS THE FILE
;INPUTS A BYTE FROM THE FILE
;CLOSE THE FILE
SG2INP:         INC HL                
                PUSH HL                                                         ;SAVE OFFSET POS
                LD C,(HL)
                INC HL
                LD B,(HL)
                INC HL
                PUSH HL
                POP DE
                EX DE,HL
                PUSH BC
                LD BC,18
                ADD HL,BC
                POP BC
                EX DE,HL
                PUSH DE
                CALL STRG_READBYTE
                POP DE                                                          ;THE BUFFER TO READ THE BYTE
                LD A,202                                                        ;ERROR CODE CAN'T READ FROM FILE
                JR C,ERREXIT
                ;INCREASE THE OFFSET BY 1
                POP HL  
                LD C,(HL)                                                        ;OFFSET OF FILE
                INC HL
                LD B,(HL)
                INC BC
                LD (HL),B
                DEC HL
                LD (HL),C
                XOR A
                LD A,(DE)                                                       ;READ THE BYTE TO A
                RET


SG2OTP:         RET
		NAME SCRBuffer
		
		GLOBAL SCCLS 							; CLEAR SCREEN
		GLOBAL SCCLL 							; CLEAR LINE AT A
		GLOBAL SCPRN 							; PRINT TO SCREEN AT LINE = A MESSAGE AT HL
		GLOBAL PRNRGA 							; PRINT REGISTER A AS BINARY AT COL,LINE AT DE
		GLOBAL PRNCL 							; PRINT TO SCREEN COL,LINE AT DE
		GLOBAL TXTRAM
		
		
;TXTRAM EQU 642
		
; CLEAR LINE
SCCLL:		PUSH HL
		PUSH AF
		LD HL, STCL4 							;40 LINE CLEAR
		CALL SCPRN1
		POP AF
		POP HL
		RET
		
SCMSG1:		
		XOR A 								;LINE 0
		LD HL, SCRDY
		CALL SCPRN
		RET
		
;CLEAR SCREEN
		
SCCLS:		LD B, 25
		XOR A 								;; PRINT STR0 AT LINE 1
TSTLP1: 	PUSH BC
		CALL SCCLL
		INC A 								; NEXT LINE
		POP BC
		DJNZ TSTLP1
		CALL SCMSG1
		RET
		
		
;-------------------------PRINT REGISTER A as binary at col,line
		
PRNRGA: 	PUSH AF
		LD B, 8
LPN5:		RLC A 								; BIT 7 FIRST
		PUSH AF
		LD A, 0
		JR NC, SKP5
		LD A, 1
SKP5: 		ADD A, 48
		CALL PRNCL 							; PRINT A AT COL,LINE
		INC E
		POP AF
		DJNZ LPN5
		POP AF
		RET
		
;-------------------------PRINT AT COL,LINE
PRNCL:		PUSH HL
		PUSH DE
		PUSH AF
		PUSH BC
		LD HL, TXTRAM
		LD B, D 							; D HAS THE LINE
		PUSH DE
		LD DE, 40 + 24 							; FOR 40 COLUMN SCREEN
LPN2:		ADD HL, DE
		DJNZ LPN2
		POP DE
		LD B, E 							; E HAS THE COLUMN
LPN3:		INC HL
		DJNZ LPN3
		LD (HL), A 							; A HAS THE CHAR TO PRINT
		POP BC
		POP AF
		POP DE
		POP HL
		RET
		
;-------------------------PRINT TO SCREEN BUFFER A HAS THE LINE TO PRINT
SCPRN:		CALL SCCLL 							; CLEAR THE LINE
SCPRN1:		PUSH HL
		LD HL, TXTRAM
		OR A
		JR Z, SCSKIP 							;SKIP FOR LINE 0
		LD B, A
		LD DE, 40 + 24 							; FOR 40 COLUMN SCREEN
LPN1:		ADD HL, DE
		DJNZ LPN1
SCSKIP:		EX DE, HL
		POP HL
SCLP:		LD A, (HL)
		OR A
		JR Z, SCEXT
		CP 31
		JR C,SCLPSKIP
		LD (DE), A
		INC DE
SCLPSKIP:	INC HL
		JR SCLP
SCEXT:		RET
		
STCL4:		DEFM "                                                                                         "
		DB 0
		
STCL8:		DEFM "                                                                                         "
		DEFM "                                                                                         "
		DB 0
		
SCRDY:		DEFM "SCREEN READY!!!"
		DB 0
		
		
		END

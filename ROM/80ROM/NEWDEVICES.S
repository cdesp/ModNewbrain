                GLOBAL STNEWD

		DEFSTOR EQU $13 						;19 DECIMAL = 1
		
		TVIO EQU 0E683H
		TP1IO EQU 0EDEEH
		TP2IO EQU 0EDF4H
		LIIO EQU 0E650H
		TLIO EQU 0E675H
		NKBWIO EQU 0A136H
		NKBIIO EQU 0A11AH
		UPIO EQU 0E41FH
		LPIO EQU 0E43CH
		JGIO EQU 0E42FH
		DUMMY EQU 0E419H
		GRAPH EQU 0A596H


STNEWD:		LD HL, $58 							;DEVTAB
		LD DE, DEVTAB 							; NEW TABLE SET
		LD (HL), E
		INC HL
		LD (HL), D
		LD HL, DEFSTOR
		LD A, 12 							;DEFAULT STORE DEVICE IS 12	NB STORAGE MODULE
		LD (HL), A
		RET
		
		
;START OF NEW DEVICE TABLE
DEVTAB:		DEFB 17 							;number of entries	;number of entries
		DEFW TVIO 							;
		DEFW TP1IO 							;
		DEFW TP2IO 							;
		DEFW LIIO 							;
		DEFW TLIO 							;
		DEFW NKBWIO 							;
		DEFW NKBIIO 							;
		DEFW UPIO 							;
		DEFW LPIO 							;
		DEFW JGIO 							;
		DEFW DUMMY 							;
		DEFW GRAPH 							;		
                DEFW SG2IO 							;MY STORAGE DEVICE 13 BDISCIO BINARY SERIAL
                DEFW SG2IO                                                      ;MY STORAGE DEVICE 14 TDISCIO SAME AS 13 FOR ME
                DEFW SG2IO                                                      ;MY STORAGE DEVICE 15 RDISCIO RANDOM ACCESS
                DEFW SG2IO                                                      ;MY STORAGE DEVICE 16 SDISCIO DIR COMMANDS
                DEFW RS2IO 							;MY SERIAL DEVICE 17
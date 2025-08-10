Org 25000

INCLUDE ..\80ROM\BOOTLD_8000.SYM





ld a,0
out (128),a
IN A, (128)          ; THE CPU SPEED IN MHZ e.g. 12
CALL SOUTAS

LD A,1
OUT (127),A
;LD B,1000/5
;CALL delay5mics

;ld b,100
;call DELms

LD B,1
CALL DELAYSEC2

LD A,0
OUT (127),A


JP MENU


LD B,250
AGN1: PUSH BC

LD B,250
CALL DELAYMICRO2
LD B,250
CALL DELAYMICRO2
LD B,250
CALL DELAYMICRO2
LD B,250
CALL DELAYMICRO2

POP BC
DJNZ AGN1

JP $8010  ;MENU


LD C,128
LD A,1
OUT (C),A
JP $8010  ;MENU


;B THE NUMBER OF MS TO DELAY
DELAYMILI:
DELms:		;PUSH AF
		;PUSH DE
		;PUSH HL
NXTMS:		PUSH BC
		;1000 us IS 1ms
		LD B,950/5		;FIX SHOULD BE 1000
		CALL delay5mics
		POP BC
		DJNZ NXTMS
		;POP HL
		;POP DE
		;POP AF
		RET


;B THE NUMBER OF SECS TO DELAY
@DELAYSEC2:	PUSH BC
		LD B,250
		CALL DELAYMILI
		LD B,250
		CALL DELAYMILI
		LD B,250
		CALL DELAYMILI
		LD B,250
		CALL DELAYMILI
		POP BC
		DJNZ DELAYSEC2
		RET



DELAYRTN:
m18:		nop         ; 4 cycles
		nop         ; 4 cycles
m16:   		nop         ; 4 cycles
     		nop         ; 4 cycles
     		nop         ; 4 cycles
m14:   		nop         ; 4 cycles
		nop         ; 4 cycles
m12:   		nop         ; 4 cycles
     		nop         ; 4 cycles
     		nop         ; 4 cycles
m10:   		nop         ; 4 cycles
     		nop         ; 4 cycles
m8:   		nop         ; 4 cycles
     		nop         ; 4 cycles
   		nop         ; 4 cycles
m6:    		nop         ; 4 cycles
   		nop         ; 4 cycles
m4:		nop         ; 4 cycles
	    	nop         ; 4 cycles
     		nop         ; 4 cycles
		jp dm3cont
	       ; ret         ; 10 cycles		

;idx from delay5us 18 is for 4mhz
		;MHZ  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
;DMHELPIDX:	db 0,20,19,18,18,17,16,16,13,13,11,11,08,08,06,06,03,03,01,01,01  ;
DMHELPIDX:	db 0,20,19,18,18,17,16,16,13,13,11,11,11,08,06,06,03,03,01,01,01 ;FIXED FOR DELAYS

; b has the 5us multiple to delay
; so for 250us b should be 250/5=50
delay5mics:	IN A, (128)          ; THE CPU SPEED IN MHZ e.g. 12
		ld hl,DMHELPIDX
		ld e,a
		ld d,0
		add hl,de
		ld a,(hl)
		ld e,a
		ld hl,DELAYRTN
		add hl,de
dm3agn:		jp (hl)
dm3cont:	djnz dm3agn
		ret


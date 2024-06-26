;;DEVICE DRIVER FOR THE PS/2 KEYBOARD MODULE
;DEVKBI IS THE DEVICE PORT
;KEYCTR VARIABLE FLAG FOR SHIFT, CONTROL, ALT AND CAPS
;LASTKEY VARIABLE LAST KEY PRESSED


@PS2KBINIT:	LD HL,KEYCTR
		LD (HL),0
		RET

;ZF SET MEANS KEY IS AVAILABLE
@PS2KEYAVAIL:	IN A,(6)
		CP 56  ;38H =0011 1000  0011 IS 3 MEANS KBD INPUT 1000 MEANS REAL KEYBOARD
		RET

@READKBCHAR:	CALL GETKBKEY		;GET HARDWARE KEY WITH CHECKS ON SHIFT...
		OR A	;
		RET Z
		LD (LASTKEY),A
		CALL PS2LOK		;RETURN ASCII CHAR
		RET

;RETURNS THE KEY CODE PRESSED ALSO CHECKS THE EXTENDED KEYS AND SETS SHIFT CONTROL ETC
GETKBKEY:	CALL GETPS2HWKEY	;GET SCANCODE FROM PS/2 KEYB
		OR A	
		RET Z			;IF KEYBRD KEY IS ZERO
		CALL CHECKEXT		;CHECKEXT SHIFTS CTRL ALT CAPS
		RET

;READS THE KEY AND ENABLES THE KEYBOARD
;kEYBOARD GETS DISABLED AUTOMATICALLY WHEN A KEY IS READ FROM I2C
GETPS2HWKEY:	DI
		IN A,(DEVKBI)		
		LD C,A
		CALL KBENABLE
		LD A,C
		EI
		RET

@KBDISABLE:	LD A, 2
		OUT (DEVKBI), A
		RET
		
@KBENABLE:	XOR A
		OUT (DEVKBI), A
		OUT (DEVKBI), A
		OUT (DEVKBI), A
		RET




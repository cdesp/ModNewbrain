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



;============================================SDISCIO=====================
;SERIAL DISC ACCESS
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
; OFFS  AA DESC                        BYTES  
;   0    1 OPENTYPE 0-RD,1-WR,2-RW,4-CR    1   BYTE
;   1    2 CURRENT POSITION ON FILE        2   BYTES
;   3    3 POSITION ON CURRENT BUFFER      2   BYTES WHEN THIS GOES 500 WE LOAD AGAIN
;   5    4 FILENAME                       20   BYTES        
;  25    5 BYTES READ ON BUFFER            2   BYTES
;  27    6 FILEBUF                       500   BYTES     

                SPCNEED EQU 527
                FILEOFS EQU 1
                BUFFOFS EQU FILEOFS+2
                FNAMEIX EQU BUFFOFS+2
                BYTREAD EQU FNAMEIX+20
                FBUFFIX EQU BYTREAD+2
                


                ERROPF  EQU 150                 ;OPEN FILE ERROR FILE NOT EXISTS
                ERREOF  EQU 151                 ;EOF REACHED
                ERRRDF  EQU 152                 ;BLOCK READ ERROR
                ERRWTF  EQU 153                 ;BLOCK WRITE ERROR
                ERRTRA  EQU 155                 ;TRANSACTION ERROR INPUT ON OPENOUT OR VICE VERSA
                

SG2OIN:		LD A,0								;OPENIN
                JR SG2OPN

		
SG2OOT:		LD A,1
SG2OPN:         PUSH HL
                PUSH BC 							;OPENOUT
                PUSH AF
		LD BC, SPCNEED 							;NO OF BYTES REQUIRED
		DEFW WMKBUFF 							;
                POP DE
		POP BC                                 
		JR C, ERREXIT                
                PUSH HL                                                         ;SAVE START OF STRUCT
                LD A,D
;HL HAS THE BUFFER ADDR
		LD (HL), A 							;OPEN TYPE
                INC HL
                LD (HL),0                                                       ;FILE POS ZERO
                INC HL
                LD (HL),0
                INC HL
                LD (HL),0                                                     ;BUF POS 0 
                INC HL
                LD (HL),0
                INC HL
                PUSH HL
                PUSH BC
                LD BC,BYTREAD-FNAMEIX                                           ;SKIP FILENAME
                ADD HL,BC
                POP BC
                LD (HL),0                                                       ;BYTES READ TO 0 
                INC HL
                LD (HL),0                                                     
                POP DE                                                          ;DE AT FNAMEIX       
                POP IX                                                          ;IX THE START OF STRUCT                
                POP HL                                                          ;HL THE FILE NAME
                PUSH DE                                                         ;SAVE THE START OF FILENAME        
                LDIR                                                            ;TRANSFER THE FNAME TO OUR STRUCT        
                XOR A
                LD (DE),A                                                       ;NULL TERMINATED STRING
                POP HL                                                          ;START OF FNAME
                LD A,(IX)
                CP 0
                JR NZ,SG2EX                                                      ;IF FILE OPEN OUT EXIT
                ;TEST IF FILE EXISTS
                LD BC,0
                INC DE
                CALL STRG_LOADFILE
                LD A,ERROPF                                                     ;ERROR NO IF FILE ERROR
                RET C
SG2EX:          XOR A
                RET


ERREXIT:        POP HL
                SCF
                RET

SG2CLS:		LD A,(HL)
                CP 1                                                            ;CHECK IF OPENOUT
                JR NZ,SCL_EX
                ;SAVE REMAINING BYTES  
                ;PUSH HL 
		;CALL OS_PRINTMSG
		;DEFM " CLOSE & SAVE"
		;DB 10,13,0
                ;POP HL
                PUSH IX
                PUSH HL
                POP IX
                LD BC,FNAMEIX
                ADD HL,BC
                LD C,(IX+BUFFOFS)
                LD B,(IX+BUFFOFS+1)
                CALL SO_STFILE
SCL_EX:         XOR A 								; CLOSE STREAM
		LD B A 								;
		LD C A 								;
		RET 								;
                

;OPENS THE FILE
;INPUTS A BYTE FROM THE FILE
;CLOSE THE FILE
;HL POINTS TO OUR BUFFER
SG2INP:         PUSH IX
                CALL SG2INP2
                POP IX
                RET

SG2INP2:        LD A,(HL)
                CP 0
                LD A,ERRTRA
                SCF
                RET NZ                                                          ;IF NOT INPUT THEN ERROR                
                CALL SGCHKEOF                                                   ;TEST EOF
                INC HL                
                PUSH HL                                                         ;SAVE OFFSET POS
                LD C,(HL)
                INC HL
                LD B,(HL)                                                       ;BC THE FILE OFFS
                INC HL
                LD E,(HL)
                INC HL
                LD D,(HL)                                                       ;DE THE BUFFER OFFS
                INC HL                                                          ;NOW ON FILENAME OFFS
                ;CALL PRINTDBG
                LD A,ERREOF
                JP C,ERREXIT                                                    ;ON C EOF REACHED                
                JR Z,RDFMFILE                                                   ;ON Z=EOB READ FROM FILE
                ;READ BYTE FROM THE BUFFER
                ;TEST IF LAST BYTE (500TH )WAS READ     500=$01F4        
              ;  LD A,D
              ;  SUB $01
              ;  JR NZ, RDFMBUFF
              ;  LD A,E
              ;  SUB $F4
              ;  JR Z, RDFMFILE
RDFMBUFF:       LD BC,FBUFFIX-FNAMEIX                                           ;500 BYTES BUFFER OFFSET
                ADD HL,BC                                                       ;HL ON BUFFER        
                ADD HL,DE                                                       ;HL ON NXT BYTE INSIDE BUFFER
                ;CALL PRINTDBG
                LD A,(HL)                                                       ;READ BYTE TO A                
                POP HL                                                          ;HL AT THE FILE OFFSET
                INC HL
                INC HL                                                          ;AT THE BUFFER OFFSET
                INC DE                                                          ;POINT TO NEXT BYTE
                LD (HL),E                                                       ;SAVE 
                INC HL
                LD (HL),D
                RET
RDFMFILE:       PUSH HL                                                         ;HL FILENAME        
                POP DE                                                          ;DE FILENAME
                EX DE,HL
                PUSH BC
                LD BC,FBUFFIX-FNAMEIX                                           ;500 BYTES BUFFER OFFSET
                ADD HL,BC
                POP BC
                EX DE,HL                                                        ;HL TO FNAME OFFSET, DE TO BUFFER OFFSET
                ;CALL PRINTDBG
                ;CALL CLRBUFR                                                    ;ZEROES THE BUFFER ON DE
                PUSH DE                                                         ;BC THE FILE OFFSET TO READ FROM
                                                                                ;DE THE DEST BUFFER
                                                                                ;HL THE FILENAME
                CALL STRG_READBUFR                                              ;READ 500 BYTES FROM FILE TO BUFFER BC= BYTES READ
                POP DE                                                          ;THE BUFFER TO READ THE BYTE
                LD A,ERRRDF                                                     ;ERROR CODE CAN'T READ FROM FILE
                JP C,ERREXIT
                ;INCREASE THE OFFSET BY BC
                POP HL                                                          ;GET FILE OFFSET POS ADDR
                PUSH DE                                                         ;SAVE START OF BUFFER
                EX DE,HL                                                        ;DE THE OFFSET ADDR
                LD A,(DE)                                                       ;OFFSET OF FILE 
                LD L,A
                INC DE
                LD A,(DE)
                LD H,A
                ADD HL,BC                                                       ;ADD NEW READ BYTES TO PREVIOUS OFFSET
                EX DE,HL                                                        ;HL THE OFFSET ADDR, DE THE NEW OFFSET
                LD (HL),D                                                       ;SAVE BACK
                DEC HL
                LD (HL),E
                INC HL
                INC HL                                                          ;TO THE BUF OFFSET 
                LD A,1
                LD (HL),A                                                       ;SET THE BUF OFFS TO 1
                INC HL
                XOR A
                LD (HL),A     
                INC HL                                                  
                ;SET BYTES READ ON OFFSET
                ;SKIP FILENAME
                PUSH BC
                LD BC,20
                ADD HL,BC                                                       ;NOW AT BYTREAD
                POP BC
                LD (HL),C                                                       ;SAVE BYTES READ FROM FILE
                INC HL
                LD (HL),B
                POP DE                                                          ;GET START OF BUFFER
                XOR A                                                           ;CLEAR CARRY FLAG
                LD A,(DE)                                                       ;READ THE 1ST BYTE TO A
                RET



;OPENS THE FILE
;WRITES THE BUFFER TO THE FILE
;CLOSE THE FILE
;HL POINTS TO OUR STRUCT
;RET ZF FOR SUCCESS ELSE THE ERROR NUMBER ON A AND CF
SG2OTP:         PUSH IX
                PUSH HL
                POP IX
                LD D,A                                                          ;SAVE BYTE TO BE WRITTEN
                LD A,(HL)
                CP 0
                LD A,ERRTRA
                SCF
                JR Z,SO_EX
                LD A,D                                                          ;RESTORE A BYTE TO WRITE
                LD BC,FNAMEIX
                ADD HL,BC                
                PUSH IX
                POP DE
                EX DE,HL
                LD BC,FBUFFIX
                ADD HL,BC                
                LD C,(IX+BUFFOFS)
                LD B,(IX+BUFFOFS+1)
                ADD HL,BC
                EX DE,HL                                
                LD (DE),A
                ;CALL PRINTDBG
                LD A,C
                CP $F3                                                          ;499 WE START AT 0  
                JR NZ,SO_BUFFNXT
                LD A,B
                CP $01
                JR NZ,SO_BUFFNXT
                INC BC                                                          ;500
                ;LD BC,500 ;BC IS 500 ALREADY
                ;THATS THE 500TH BYTE ON THE BUFFER SO
                ;WRITE THE 500 BYTES TO FILE AND ZERO THE BUFFOFFS
                ;IX THE START OF STRUCT
                ;HL THE FILENAME                
                ;BC THE NUMBER OF BYTES TO WRITE  
                ;PUSH IX BEFORE CALL      
 SO_STFILE:     EX DE,HL
                PUSH IX
                POP HL
                PUSH BC
                LD BC,FBUFFIX
                ADD HL,BC
                POP BC
                EX DE,HL
                ;CALL PRINTDBG
                ;DE THE BUFFER
                CALL STRG_WRITEBUFR                             ;WRITES 500 BYTES FROM BUFFER DE TO THE END OF THE FILE
                LD A,ERROPF
                JR C,SO_ERROUT
                LD A,ERRWTF
                SCF
                JR NZ,SO_ERROUT
                XOR A
SO_ERROUT:      LD BC,0                                         ;ZERO BUFFER OFFSET
                DB $16                                          ;SKIP INC BC LD D,NN
SO_BUFFNXT:     INC BC                                          
                LD (IX+BUFFOFS),C
                LD (IX+BUFFOFS+1),B
SO_EX           POP IX
                CP 0
                RET


;HL AT THE START OF THE STRUCT
;C IS SET IF EOF END OF FILE
;Z IS SET IF EOB END OF BUFFER
;NZ IF NEITHER
SGCHKEOF:       PUSH IX
                PUSH DE
                PUSH BC

                PUSH HL
                POP IX
                LD E,(IX+BUFFOFS)
                LD D,(IX+BUFFOFS+1)
                LD C,(IX+BYTREAD)
                LD B,(IX+BYTREAD+1)
                ;check IF BYTES READ=0 THEN FORCE LOAD FROM FILE
                LD A,B
                OR C
                LD A,3
                JR Z, SGEFEXIT                                                  ;Z=EOB
                EX DE,HL                                                        ;SAVE HL SET HL=DE
                OR A
                SBC HL,BC
                LD A,H
                OR L
                EX DE,HL                                                        ;RESTORE HL
                LD A,1                                                          ;NO EOF NO EOB = NZ
                JR NZ,SGEFEXIT
                ;ON Z IF 500 THEN END OF BUFFER ELSE EOF
                LD A,B
                SUB $01
                JR NZ, SGDOEOF
                LD A,C
                SUB $F4
                JR NZ, SGDOEOF
                LD A,2                                                          ;Z MEANS EOB END OF BUFFER                
                JR  SGEFEXIT

SGDOEOF:        LD A,ERREOF                                                           ;RETURN 0 ON EOF
                SCF         
SGEFEXIT:       POP BC
                POP DE
                POP IX
                RET


PRNHL:          PUSH HL
                PUSH BC
                PUSH DE
		CALL B2D16
		LD HL,B2DBUF
		CALL SKIPSPC
		CALL RS_TXT
                POP DE
                POP BC
                POP HL
                RET
PRINTDBG:       PUSH HL
                PUSH BC
                PUSH DE
                PUSH AF

                CALL PRNHL
                PUSH DE
                PUSH BC
		CALL OS_PRINTMSG
		DEFM " HL"
		DB 10,13,0
                POP BC
                POP DE

                PUSH DE
                POP HL
                CALL PRNHL
                PUSH DE
                PUSH BC
		CALL OS_PRINTMSG
		DEFM " DE"
		DB 10,13,0
                POP BC
                POP DE

                PUSH BC
                POP HL
                CALL PRNHL
                PUSH DE
                PUSH BC
		CALL OS_PRINTMSG
		DEFM " BC"
		DB 10,13,0
                POP BC
                POP DE


                POP AF
                POP DE
                POP BC
                POP HL
                RET

;ZEROES 500 BYTES ON BUFFER DE
;ALL REGS PRESERVED
CLRBUFR:        PUSH AF
                PUSH HL
                PUSH DE
                PUSH BC
                XOR A
                LD (DE),A
                PUSH DE
                POP HL
                INC DE
                LD BC,499
                LDIR        
                POP BC
                POP DE
                POP HL
                POP AF
                RET

;============================================RDISCIO=====================
;RANDOM DISC ACCESS
RG2IO:		EQU $
		DEFB 4                                                          ;COUNT OF DISPLACEMENTS-1
		DEFB SG2OIN-$ 							;OPEN IN
		DEFB SG2OOT-$ 							;OPEN OUT
		DEFB SG2INP-$ 							;INPUT BYTE
		DEFB SG2OTP-$ 							;OUTPUT BYTE
		DEFB SG2CLS-$ 							;CLOSE

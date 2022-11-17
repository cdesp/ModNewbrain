        NAME MOD5
 
;Various compile and execute routines
 
;INTERNALLY DEFINED GLOBALS

        GLOBAL XTOP
        GLOBAL XREPORT
        GLOBAL XERRLIN
        GLOBAL CUSR
        GLOBAL XERRNO
        GLOBAL XFREE
        GLOBAL XINSTR
        GLOBAL XINST1
        GLOBAL XASC
        GLOBAL XCONT
        GLOBAL XLEN
        GLOBAL XSTSTRS
        GLOBAL XMID
        GLOBAL XMID1
        GLOBAL XLEFT
        GLOBAL XRIGHT
        GLOBAL XNUM
        GLOBAL XPOKE
        GLOBAL XREST
        GLOBAL XUSR
        GLOBAL XVAL
        GLOBAL XBEGRD
        GLOBAL XENDRD
        GLOBAL XSEPRD
        GLOBAL DELSTR
        GLOBAL DELST2
 
;EXTERNALLY DEFINED GLOBALS

        GLOBAL CPSH
        GLOBAL XRESU2
        GLOBAL LINPTR
        GLOBAL GTOP
        GLOBAL GINPTR
        GLOBAL LDUSTP
        GLOBAL GUSTP
        GLOBAL XRUN1C
        GLOBAL XRUN1D

        EJECT
;W & Z codes

        GLOBAL ZLPSTR1
        GLOBAL ZROOMQ
        GLOBAL ZSETS2
        GLOBAL ZLNTT
        GLOBAL ZCHECK
        GLOBAL ZLLN2
        GLOBAL ZLAST
        GLOBAL ZBVARIA
        GLOBAL ZCHKCR
        GLOBAL ZCHKCOM
        GLOBAL ZCMTHRE
        GLOBAL ZCM2HL
        GLOBAL ZDLUSS
        GLOBAL ZFLT
        GLOBAL ZFPMONE
        GLOBAL ZFPZERO
        GLOBAL ZINP
        GLOBAL ZINTIN
        GLOBAL ZINUSS
        GLOBAL ZLDF
        GLOBAL ZNCONST
        GLOBAL ZNLNT
        GLOBAL ZNUMEXP
        GLOBAL ZOUT
        GLOBAL ZSTINST
        GLOBAL ZUNST2
        GLOBAL ZUNST6
        GLOBAL ZUNSTSS
        GLOBAL ZNFIND
        GLOBAL WCHKCR
        GLOBAL WINUSS
        GLOBAL WSTF
        GLOBAL WUNSTSS

;Offsets from IY
 
INCON   EQU 007H
ERRNO   EQU 8
ERRLIN  EQU 39H
DTINPT  EQU 02FH        ;CURRENT 'DATA' POINTER
DTLN    EQU 031H        ;CURRENT 'DATA' L.N.T.ADR.
CLINNO  EQU 27H
 
;Y codes

YREST   EQU 80H+10
YUSR    EQU 80H+12

;T codes
 
TSTOP   EQU 80H+16
TDATA   EQU 80H+29

;ERROR NUMBERS
 
CONTERR EQU 87          ;continue error
DATERR  EQU 99          ;data run out

        EJECT
XCONT   EQU $
;EXECUTE CODE FOR -CONTINUE-
        PUSH IY
        POP HL
        LD BC,CLINNO
        ADD HL,BC
        PUSH HL
        DEFW ZLNTT
        POP HL
        LD A,CONTERR
        RET C
        LD A,(HL)
        INC HL
        LD H,(HL)
        LD L,A
        PUSH HL
        POP IX
        PUSH HL
        DEFW ZSTINST
        LD B,TSTOP
        DEFW ZCHECK
        POP HL
        LD A,254
        RET
         
XREPORT: CALL XRESU2
        RET C
        LD HL,0
        LD E,ERRLIN
        DEFW ZSETS2
        CALL XCONT
        LD A,(IY+ERRNO)
        SCF
        RET
 

XPOKE:  EQU $
        DEFW ZUNSTSS    ;NO. TO POKE TO E
        PUSH DE
        DEFW WUNSTSS    ;STORE ADDR.TO DE 
        POP HL
        LD A,0
        SBC A,H         ;Set CY if already set or
        LD A,38         ;number >255. Load poke errno
        RET C           ;and ret if poke error.
        EX DE,HL
        LD (HL),E       ;POKE NO. FROM E
        RET

 

CUSR:   EQU $
 
;USER SUBROUTINE (ASSEMBLER CODE)
 
        DEFW ZNUMEXP
        RET C           ;R.IF EXPN.ERROR
        LD C,0          ;SET PAR. CNT. TO 0
CUSR20: EQU $
        DEFW ZCHKCR
        JR NC,CUSR50    ;J.IF FINISHED
        PUSH BC         ;SAVE PAR.COUNT
        DEFW ZCHKCOM
        CALL NC CONVAR
        POP BC
        RET C           ;R.IF ERROR
        INC C
        JR CUSR20

CUSR50: EQU $
 
;PARAMETERS COMPILED
;SET UP ENTRY CONDITIONS TO INSERT
 
        LD L,YUSR
        LD H,C
        DEFW ZCM2HL
        RET



XUSR:   EQU $
 
;GET NO. OF PARAMETERS AND ADDRESS FROM OBJECT CODE
;PARAMETER ADDRESSES ON USER STACK
;HL>NEXT BYTE OF OBJECT CODE
 
        LD A,(HL)
        PUSH AF         ;NO. OF PARAMETERS
        RLCA
        LD C,A
        LD B,0
        CALL GUSTP
        JR Z,XUSR10
        DEFW ZDLUSS     ;DELETE FROM USER STACK
XUSR10: DEC HL
        PUSH HL
        DEFW ZUNSTSS
        POP HL
        POP BC
        RET C           ;R.IF ERROR (FIX)
        PUSH DE

;HL>ADDR. PARAMETER LIST
;B>NO. OF PARAMETERS IN LIST
;TOS>ADDR. OF INSERT

        RET     ;ENTER ASSEMBLER INSERT



CONVAR: EQU $
        DEFW ZNCONST
        JP NC,CPSH      ;J.IF NUMERIC CONSTANT
        DEFW ZBVARIA
        RET


XBEGRD: PUSH HL
        LD (IY+INCON),127
        CALL GDTLN
        DEFW ZNFIND
        JR C STDP7
STDP9:  DEFW ZLAST
        JR Z STDP9
        LD C,(IY+DTINPT)
        LD B,(IY+DTINPT+1)
        LD A,C
        OR B
        JR Z STDP4
        PUSH HL
        DEFW ZSTINST
        ADD HL,BC
        CALL LINPTR
        POP HL
STDP3:  LD (IY+DTLN+1),H
        LD (IY+DTLN),L
        DEFW ZCHKCR
        JR NC STDP5
        OR A
STDP6:  JP XRUN1D
 
STDP7:  DEFW ZLLN2
        JR C STDP2
STDP4:  PUSH HL
        DEFW ZSTINST
        LD B,TDATA
        DEFW ZCHECK
        POP HL
        JR NC STDP3
STDP5:  DEFW ZNLNT
        JR NC STDP4
STDP2:  POP HL
        LD A,DATERR
        RET
 
XENDRD: DEFW ZCHKCOM
        JR NC XRND1
        DEFW ZCHKCR
        RET C
XRND1:  CALL GINPTR
        EX DE,HL
        CALL GDTLN
        LD C,(HL)
        INC HL
        LD B,(HL)
        DEC HL
        LD (IY+DTLN),C
        LD (IY+DTLN+1),B
        DEFW ZSTINST
        EX DE,HL
        OR A
        SBC HL,DE
XRND2:  LD (IY+DTINPT+1),H
        LD (IY+DTINPT),L
        RET
 
XSEPRD: PUSH HL
        DEFW ZCHKCOM
        JR NC STDP6
        CALL GDTLN
        JR STDP3
 
GDTLN:  LD H,(IY+DTLN+1)
        LD L,(IY+DTLN)
        RET
 
XREST:  EQU $
 
;EXECUTE CODE FOR RESTORE COMMAND
 
        LD A,(HL)
        LD (IY+DTLN),A
        INC HL
        LD A,(HL)
        LD (IY+DTLN+1),A
        XOR A
        LD H,A
        LD L,A
        JR XRND2
 
XINSTR: PUSH HL
        DEFW ZUNSTSS
        JR C STDP6 
        LD A,D
        OR E
        JR Z XINS8
        LD B,D
        LD C,E
        JR XINS9
 
XINST1: PUSH HL
XINS8:  LD BC,1
XINS9:  DEFW ZUNST2
        PUSH DE
        CALL DELST2
        PUSH HL
        ADD HL,BC
        EX (SP),HL
        PUSH HL
        CALL DELSTR
        EX DE,HL
        SBC HL,BC       ; carry unset by DELSTR
        CCF
        POP DE
        POP BC
        JR NC XINS5
        PUSH BC
XINS3:  POP BC
        INC BC
        CALL GUSTP
        OR A
        SBC HL,DE
        OR A 
        SBC HL,BC
        EX DE,HL
        EX (SP),HL
        EX DE,HL
        ADD HL,DE
        EX DE,HL
XINS5:  POP DE
        JR NC XASC70
        PUSH HL
        PUSH BC
XINS2:  OR A
        SBC HL,DE
        JR Z XINS4
        ADD HL,DE       ;restore HL
        LD A,(BC)
        CP (HL)
        JR NZ XINS3
        INC BC
        INC HL
        JR XINS2
 
XINS4:  POP HL
        POP BC
        SBC HL,DE
        DEC HL
        JR XTOP1
 
XFREE:  PUSH HL
        LD HL,0
        DEFW ZROOMQ
        JR XASC50
 
XERRNO: PUSH HL
        LD E,(IY+ERRNO)
        JR XASC99

XERRLIN: PUSH HL
        LD E,(IY+CLINNO)
        LD D,(IY+CLINNO+1)
        JR XASC50
 
XTOP:   PUSH HL
        CALL GTOP
XTOP1:  EX DE,HL
        JR XASC50
 
        EJECT
; "STRING FUNCTION    X MODULES"

; ARITH.STD.FNS. WITH 1 STRING PARAMETER
; ENTERED WITH STRING PAR.ON USER STACK
; EXIT WITH F.P. NO. ON USER STACK



XASC:   EQU $
 
; CONVERT FIRST CHAR.OF STRING TO A NUMBER (0 - 255)
; REPLY 0 IF NULL STRING
 
        PUSH HL
        DEFW ZUNST2     ;HL=STRING LENGTH
                        ;DE=STRING ADR.
        LD A,H
        OR L
        JR Z,XASC70     ;J.IF NULL STRING
        CALL DELST2
        LD A,(DE)
        LD E,A
XASC99: LD D,0
XASC50: DEFW ZFLT       ;FLOAT VALUE
        DEFB 21H        ;code for LD HL,.. i.e. skip
XASC70: EQU $           ;ANS IS 0 SO LOAD F.P.0 ON STACK
        DEFW ZFPZERO    ;LOAD 0
XASC80: EQU $           ;NOW MOVE RESULT TO USER STACK
        LD BC,6
        DEFW WINUSS     ;RESERVE STORAGE
        DEFW WSTF       ;STORE RESULT (IF ROOM)
XASC90: JP XRUN1D       ;CONTINUE WITH REST OF LINE




XLEN:   EQU $
 
; REPLACE STRING ON STACK WITH F.P. VALUE OF
; LENGTH OF STRING
 
        PUSH HL
        CALL DELSTR
        JR XASC50       ;FLOAT & STACK ANSWER




XVAL:   EQU $
 
; REPLACE STRING ON STACK WITH NUMERIC VALUE OF STRING
; ERRORS AS FOR MATHS PACKAGE "INP" ROUTINE
;oh no they're not !!

        PUSH HL
        CALL VALNUM     ;Routine common to VAL & NUM
                        ;TO PRESENT STRING TO MATHS PACK
        LD A,95
        JR C,XASC90     ;J.IF ERROR TO XRUN1D
        JR XASC80       ;STACK ANSWER (VALID NUMBER)




XNUM:   EQU $
 
; REPLACE STRING ON STACK WITH :-
;                       0 IF STRING IS NOT A VALID NO.
;                      -1 IF STRING IS A VALID NO.

        PUSH HL
        CALL VALNUM
        JR C XASC70     ;j. if not valid number
        DEFW ZFPMONE
        JR XASC80       ;J.IF VALID ANSWER




VALNUM: EQU $
 
; CONVERT STRING NO.ON USER STACK TO F.P.NO.IN FACC
; EXIT :- CY SET IF M.P. REPORTS ERROR IN STRING
; ELSE ANSWER IN FACC
; IN ALL CASES STRING IS UNSTACKED

        DEFW ZUNST2     ;HL=STRING LENGTH
                        ;DE=STRING ADR.
        LD A,H
        OR L
        SCF
        RET Z           ;R.IF NULL STRING 
 
; NOT NULL SO DISCARD FROM STACK & PRESENT TO MATHS PACK
 
        LD B,H
        LD C,L
        CALL DELST2
 
; INSERT TERMINATOR ON STRING
; BC=LENGTH OF STRING
; HL=START OF STRING
;  TERMINATOR MAY BE ADDED BY MOVING STRING 1 BYTE
;  DOWN STORE & INSERTING "CR" ON END
;  THIS IS O.K. BECAUSE 2 BYTES ARE SPARE FROM
;  THE UNSTACKED STRING COUNT

        LD H,D
        LD L,E
        DEC DE
        PUSH DE         ;ptr to where strinq will be
        LDIR            ;MOVE STRING DOWN 1 BYTE
        EX DE,HL        ;HL=ADR.FOR TERMINATOR
        LD (HL),0DH     ;INSERT "CR"
        POP DE          ;ptr for maths pack
        PUSH HL         ;ptr to terminator
        DEFW ZINP       ;CONVERT TO F.P.
        POP HL
        RET C           ;R.IF ERROR
        SBC HL,DE       ;reached terminator?
        RET Z           ;if have
        SCF
        RET



        EJECT
; STRING STD.FNS. WITH VARIOUS PARAMETERS
; ENTERED WITH RELEVANT STRING/F.P.NO(S)
; ON USER STACK


XSTSTRS:EQU $
 
; REPLACE F.P.NO. ON STACK WITH STRING PRODUCED
; BY PRESENTING THIS NO. TO THE MATHS PACK
; OUTPUT ROUTINE
; PICK UP FORMAT

        LD C,(HL)
        INC HL
        LD B,(HL)
        INC HL
        PUSH HL
        PUSH BC         ;SAVE FORMAT
        DEFW ZUNST6     ;GET NUMBER
        EX DE,HL
        DEFW ZLDF
        POP BC          ;RESTORE FORMAT
        DEFW ZOUT
        LD B,0
 
; BC=STRING LENGTH (EXCLUSIVE)
; HL=STRING ADR.IN MATH PACK BUFFER
 
        DEFW ZLPSTR1
        JR XASC90

*DELSTR is used by string functions to delete a string
*and gather info about it. Other entry points are for 
*lesser services.
 
DELSTR: DEFW ZUNST2
        EX DE,HL
DELST2: ADD HL,DE
        JP LDUSTP

SSTRST:                 ;substring start
        EX (SP),HL      ;save HL below ret. add.
        PUSH HL
        DEFW ZUNSTSS
        CALL GUSTP
        LD B,(HL)
        INC HL
        LD H,(HL)
        LD L,B
        RET NC          ;if no unstss fault
        JR STSUB1       ;carry set

XLEFT:  CALL SSTRST
        JR XRT1A
 
XRIGHT: CALL SSTRST
        SBC HL,DE
        JR NC XRT1      ;if len>=n
XRT1A:  LD HL,0
XRT1:   INC HL
        EX DE,HL
        JR SSUBIB
 
XMID1:  CALL SSTRST
        JR SSUBIB

XMID:   EQU $
 
; REPLACE 2 F.P.NO'S & STRING ON STACK WITH
; THE SUBSTRING OF THE STRING PRODUCED BY
; STARTING AT THE  TOSM'th CHARACTER AND TAKING
; TOS CHARACTERS.
; RETURN NULL-STRING IF TOSM > STRING LENGTH
; RETURN RIGHT$ IF  TOS > STRING LENGTH - TOSM

        PUSH HL
        DEFW ZUNSTSS    ;DE=NO.CHARS.(TOS)
        PUSH DE
        DEFW WUNSTSS    ;DE=1st CHAR POS'N (TOSM)
STSUB1: POP HL
        JR C,STSUB9     ;J.IF PARAMETER ERROR
SSUBIB: PUSH HL
        PUSH DE
        CALL DELSTR     ;throw away string
        POP BC
        DEC HL
        EX (SP),HL

; TOS=ADR.END OF STRING 
; BC =1st CHAR.POS'N REQUIRED
; DE =STRING LENGTH (EXCLUSIVE)
; HL =NO.CHARS.REQUIRED
; IS BC > STRING LENGTH
        EX DE,HL
        OR A
        SBC HL,BC
        LD BC,0
        JR C,STSUB8     ;J.IF NULL-STRING REQ'D

; HL=NO.CHARS.REMAINING IN STRING - 1
; DE=NO.CHARS.REQ'D
;  IS THERE MORE THAN REQUIRED
 
        INC HL          ;NO.CHARS REMAINING
        SBC HL,DE       ;SET CY IF TOO MANY REQ'D 
        EX DE,HL
        JR NC,STSUB2    ;J.IF NOT TRUNCATED
        ADD HL,DE
        LD DE,0
STSUB2: EQU $
 
; HL=NO.CHARS TO BE RETURNED IN SUBSTR
; DE=NO.CHARS ABOVE USRSTP NOT REQ'D
 
        LD B,H
        LD C,L          ;BC=NO.CHARS
        POP HL          ;(=USRSTP-1)
                        ;ADR.LAST CHAR.OF STRING
        PUSH HL         ;=DEST. FOR LAST CHAR OF SUBSTR
        OR A
        SBC HL,DE       ;HL=ADR.LAST CHAR.OF SUBSTR
STSUB8: EQU $
        POP DE
        PUSH BC
        INC BC          ;BC=INCLUSIVE COUNT - 1 (NEVER 0)
        LDDR            ;MOVE REQUIRED CHARS.
        EX DE,HL        ;HL->WHERE LENGTH IS TO BE
        POP BC
        LD (HL),C
        INC HL
        LD (HL),B       ;COUNT SET ON STRING
        DEC HL
        OR A
STSUB9: JP XRUN1C
 
        END


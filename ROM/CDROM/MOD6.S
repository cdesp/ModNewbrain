
        NAME MOD6
 
;Compile expressions etc
 
;INTERNALLY DEFINED GLOBALS

        GLOBAL NUMEXP
        GLOBAL STREXP
        GLOBAL NEWSTR
        GLOBAL EXPN
        GLOBAL MKNULL
        GLOBAL MKSTAB
        GLOBAL NUMFN
        GLOBAL STRCON
        GLOBAL STRSFN
        GLOBAL VARIA
        GLOBAL VARIAB
        GLOBAL NMF7
        GLOBAL NMF1
        GLOBAL ELNUM
        GLOBAL MKADJ
        GLOBAL LMN1
        GLOBAL VARIA9

;EXTERNALLY DEFINED GLOBALS USED BY THIS MODULE

        GLOBAL EXGLB
        GLOBAL @DIOP
        GLOBAL @MONOP
        GLOBAL GSBAS
        GLOBAL GLBAS
        GLOBAL CUFN

;W & Z codes

        GLOBAL ZMKRL
        GLOBAL ZMKAB
        GLOBAL ZELNUM
        GLOBAL ZSTKTST
        GLOBAL WADDSYM
        GLOBAL WCHKCLS
        GLOBAL WCHKCOM
        GLOBAL WFORMAT
        GLOBAL WNEWSTR
        GLOBAL WNUMEXP
        GLOBAL WSTREXP
        GLOBAL ZSSFNM
        GLOBAL ZASFNM
        GLOBAL ZQSTR
        GLOBAL ZNUMFN
        GLOBAL ZXDIM
        GLOBAL ZCHKCOM
        GLOBAL ZCM2HL
        GLOBAL ZBVARIA
        GLOBAL ZCHKOPN
        GLOBAL ZCHKQUO
        GLOBAL ZCMONE
        GLOBAL ZCMTHRE
        GLOBAL ZEXPN
        GLOBAL ZGETVNM
        GLOBAL ZINSERT
        GLOBAL ZLDSTR
        GLOBAL ZMKSTAB
        GLOBAL ZNCONST
        GLOBAL ZNUMEXP
        GLOBAL ZRDNCH
        GLOBAL ZRDSCH
        GLOBAL ZRSINST
        GLOBAL ZSCAN
        GLOBAL ZSTRCON
        GLOBAL ZSTREXP
        GLOBAL ZSTRSFN
        GLOBAL ZVARIA
        GLOBAL ZRDCH
        GLOBAL ZASFNM
        GLOBAL ZSSFNM
        GLOBAL ZDELT
        GLOBAL ZCPSH

;Offsets from IY

BASFLG  EQU 2AH
FLAGS   EQU 22H
@TYPE   EQU 2BH
STBAS   EQU 18H
ARBAS   EQU 1AH
SBAS    EQU 1CH
SCONNO  EQU 25H
FPAR    EQU 37H
 
;Y-codes
 
YAATOF  EQU 93H
YSTCHR  EQU 0F2H
YSTSTR  EQU 88H
YMID    EQU 0A9H
YDEREF  EQU 0C0H
YAR1AD  EQU 0EFH
YAR2AD  EQU 0F0H
YPSHFP  EQU 0F1H
YPSHST  EQU 0F3H
YSTROP  EQU 0E7H

;ERROR NUMBERS

ERRNOT  EQU 20
ERRNOF  EQU 21
ERRNOP  EQU 22
 
        EJECT
STRCON: EQU $           ;enter with opening quote 
        DEFW ZQSTR      ;already checked
        RET C           ;R.IF NOT "
 
;BC=STRING LENGTH
;NOW SEE IF ALREADY EXISTS
 
        CALL GSBAS
STRCN3: CALL EXGLB
        OR A
        SBC HL,DE       ;TOP OF SYM.TAB. - SYM.TAB.PTR.
        JR Z,STRCN6     ;J.IF SYM.TAB.SEARCHED
        EX DE,HL
        INC HL
        LD A,(HL)
        INC HL
        PUSH HL         ;SAVE SYM.TAB.ADR.
        PUSH BC         ;SAVE LENGTH
        CP 040H
        JR NZ,STRCN5    ;J.IF NOT STR.CON.
        PUSH BC
        DEFW ZMKSTAB
        EX (SP),HL
        AND A
        SBC HL,BC
        POP HL
        JR NZ,STRCN5    ;J.IF WRONG LENGTH
        DEFW ZRSINST
 
;NOW COMPARE BC CHARS.AT HL WITH INSTREAM
 
STRCN4: LD A,C
        OR B
        POP DE
        JR Z,STRCN8     ;J.IF STRINGS MATCH
        PUSH DE
        DEFW ZRDCH
        CP 022H
        JR NZ,STRC41
        DEFW ZRDNCH     ;SKIP 1ST QUOTE OF Q IMAGE
STRC41  CPI
        JR NZ,STRCN5    ;J.IF NOT EQUAL
        DEFW ZRDNCH
        JR STRCN4
 
STRCN5: EQU $
 
;CHECK NEXT ENTRY IN SYM.TAB.
 
        POP BC
        POP HL
        LD DE,6
        ADD HL,DE
        JR STRCN3
 
STRCN6: EQU $
 
;NEW STRING CONST.
 
        PUSH BC         ;SAVE LENGTH
        LD BC,4000H     ;name-type for str. con.
        DEFW ZINSERT    ;ADD NEW SYM.TAB.ENTRY
        POP BC          ;BC=LENGTH
        RET C           ;R.IF NOT ROOM
        INC HL
        INC HL
        PUSH HL         ;SAVE SYM.TAB.ADR.
        DEFW WNEWSTR
STRCN8: POP HL
        RET C
        DEFW ZRDNCH
        DEFW ZCPSH
        RET
 
NEWSTR: EQU $   ;COPY STRING FROM SOURCE TO STRING AREA
                ;   AND ASSIGN TO SYMBOL
                ;BC=LENGTH, HL->3RD BYTE OF S T ENTRY
        DEFW ZRSINST
        LD A,C
        OR      B
        RET Z           ;NOTHING TO DO IF NULL STRING
        DEFW ZMKRL      ;make relative if necessary
        DEFW ZLDSTR
        RET C           ;If out of room
        EX DE,HL
        DEFW ZMKAB      ;restore
 
;NOW MOVE BC CHARS.FROM INSTREAM TO (HL)
 
        DEFW ZRDCH
LNS7:   CP 022H
        JR NZ,LNS8
        DEFW ZRDNCH     ;SKIP 1ST QUOTE OF Q IMAGE
LNS8:   LD (HL),A
        DEFW ZRDNCH
        CPI             ;INCR HL, DECR BC, TEST BC
        RET PO          ;DONE IF BC=0
        JR LNS7
 
 
MKSTAB: EQU $
 
;CONVERT STRING S.T.ADR.TO STRING ADR.(ABS.)
;ON ENTRY
;       HL=LOC(SYMBOL TABLE ENTRY FOR STRING)+2
;ON EXIT
;      Z SET IF NULL STRING (& THEN DE=0 & BC=0)
;   OTHERWISE
;      HL=LOC(STRING)
;       DE=REL ADDR.
;      BC=SIZE (STRING) (LENGTH)
 
        LD E,(HL)
        INC HL
        LD D,(HL)       ;DE=LOC(STRING) (REL)
        INC HL
        LD C,(HL)
        INC HL
        LD  B,(HL)      ;BC=SIZE
        LD A,E
        OR D
        RET Z           ;R.IF NULL
        LD L,(IY+ARBAS)
        LD H,(IY+ARBAS+1)
        ADD HL,DE       ;HL=LOC(STRING) (ABS)
        RET
 
MKNULL: EQU $
 
;MAKE STRING NULL
;ON ENTRY
;      HL=LOC(STRING S.T.ENTRY)+2
;ON EXIT
;      STORE DE-ALLOCATED
;      SYMBOL TABLE ENTRY SET TO NULL STRING
 
        PUSH IX
        PUSH HL
        DEFW ZMKSTAB
        JR Z,@M25       ;IF NULL ALREADY
        PUSH DE
        LD E,STBAS
        DEFW ZDELT
        POP HL
        EX (SP),HL
        XOR A
        LD (HL),A
        INC HL
        LD (HL),A
        INC HL
        LD  (HL),A
        INC HL
        LD  (HL),A      ;'SIZE'=0 FOR NULL STRING
 
;UPDATE STRING POINTERS
 
        CALL GSBAS
@M2:    EQU $   ;here with HL -> next symbol,
                ;(SP) = rel addr of deleted string, 
                ;BC = length of deleted string
        CALL EXGLB
        AND A
        SBC HL,DE
        EX DE,HL
@M25:   POP DE
        POP IX
        RET Z
        PUSH IX
        INC HL
        LD A,(HL)
        INC HL
        AND 7CH
        CP 40H
        JR Z,LMN6       ;if scalar
        AND 4CH
        CP 40H
        JR NZ LMN7
                ;here if array:
                ;base addr.=(HL)+(SBAS)
                ;# elements=(HL+2)*(HL+4)
                ;  BC=lnth.del'd str.
                ;  DE=rel addr.of del'd str.
        PUSH DE
        POP IX
        PUSH HL
        LD E,(HL)
        INC HL
        LD D,(HL)
        PUSH BC
        PUSH DE
        INC HL
        DEFW ZELNUM
                ; now HL=# of elements
                ;  (SP)=rel base addr
                ;  (SP+2)=length of del'd string
                ;  (SP+4)=addr of 3rd bythe of symbol
                ;  IX=rel addr of del'd string
        POP DE
        LD B,(IY+SBAS+1)
        LD C,(IY+SBAS)
        EX DE,HL
        ADD HL,BC
        POP BC
                ; now HL=abs base addr
                ;  DE=# of elements
                ;  BC=length of del'd string
                ;  (SP)=addr of 3rd byte of symbol
                ;  IX=rel addr of del'd string
        PUSH DE
        PUSH HL
        EX (SP),IX      ;IX->lowest byte of array area
LMN4:   POP DE          ;DE=rel addr. of del'd string
        CALL MKADJ
        POP HL
        DEC HL
        LD A,H
        OR L
        JR Z,LMN5       ;if no more elements
        PUSH HL         
        PUSH DE         ; save addr of del'd string
        LD DE,6
        ADD IX,DE
        JR LMN4
 
LMN6:   PUSH HL
        PUSH HL
        POP IX
        CALL MKADJ
LMN5:   POP HL
LMN7:   PUSH DE
        LD DE,6
        ADD HL,DE
        JR @M2
 
MKADJ:  EQU $
                ; adjust rel addr of string
                ; enter with IX-> ls byte of rel addr
                ; DE=rel addr of del string
                ; BC=length of del'd string
                ; if rel addr<DE adds BC to it
                ; preserves BCDE
        LD L,(IX+0)
        LD H,(IX+1)     ;HL:= rel addr in symbol
        LD A,H
        OR L
        RET Z
        SBC HL,DE
        RET NC
        ADD HL,DE
        ADD HL,BC
        LD (IX+0),L
        LD (IX+1),H
        RET
 
ELNUM:          ;calculate # elements in array
                ;enter: HL->ls byte of 1st el in ST
                ;exit:HL=#of elements
                ;BCDE destroyed
        LD C,(HL)
        INC HL
        LD B,(HL)
        INC HL
        LD E,(HL)
        INC HL
        LD D,(HL)
LMN1:   LD HL,0         ;BC*DE -> HL
LMN2:   ADD HL,BC
        RET C           ;if overflow(not poss.in ELNUM)
        DEC DE
        LD A,D
        OR E
        JR NZ,LMN2
        RET
 
        EJECT
;COMPILE NUMERIC EXPRESSION
;CALL NUMEXP
;EXIT - CARRY CLEAR - O.K.
;     - CARRY SET - FAIL (ERROR CODE IN A)
 
NUMEXP: DEFW ZEXPN      ;COMPILE EXPRESSION
        RET C           ;RETURN IF FAILED
        OR A
        RET Z           ;RETURN IF NUMERIC
        JR @TYPER-$     ;SET ERROR
 
;COMPILE STRING EXPRESSION
;CALL STREXP
;EXIT - CARRY CLEAR - O.K.
;     - CARRY SET - FAIL (CODE IN A)
 
STREXP: DEFW ZEXPN      ;COMPILE EXPRESSION
        RET C           ;RETURN IF FAILED
        OR A
        RET NZ          ;RETURN IF STRING
@TYPER: LD A,ERRNOT
        SCF
        RET
 
;COMPILE EXPRESSION
;CALL EXPN
;EXIT - CARRY CLEAR AND A=0, NUMERIC
;                   AND A=10H, STRING
;     - CARRY SET - FAIL, CODE IN A
 
EXPN:   LD BC,64        ;perform stack test
        DEFW ZSTKTST    ;64 is plucked from air
        LD A,85         ;return ERR 85 if check
        RET C           ;fails
        SUB A                   ;MAKE A ZERO BINDING POWER
        PUSH    AF              ;STACK IT AS TERMINATOR
@OPRND: LD      HL,@MONOP       ;SELECT MONADIC OP. LIST
        DEFW ZSCAN              ;HAVE WE A MONADIC OP.
        JR      C,@E1-$         ;JUMP IF NOT FOUND
        PUSH    BC              ;STACK MONADIC DATA
        JR      @OPRND
 
;NOT MONADIC
 
@E1:    DEFW ZCHKOPN            ;OPEN BRACKET ?
        JR      C,@E3-$         ;NO
        DEFW ZEXPN              ;COMPILE SUB-EXPRESSION
                                ;@TYPE SET INTERNALLY.
        DEFW WCHKCLS    ;CHECK IF CLOSED
        JR      NC,@OPRTR-$     ;IF YES - GET OPERATOR
;HERE ON FAILURE
@EFAIL: LD      B,A             ;SAVE FAIL CODE
        SUB     A       ;WE'RE LOOKING FOR TERMINATOR = 0
@EF2:   POP     DE      ;GET OPERATOR OFF STACK
        CP      D               ;IS IT THE LAST
        JR      NZ,@EF2       ;NO
        LD      A,B             ;YES - GET ERROR CODE
        SCF
        RET
 
;HERE IF NOT OPEN BRACKET
 
@E3:    DEFW ZSSFNM             ;CHECK FOR STRING FUNCTION
        JR C,@E32
        DEFW ZSTRSFN
        LD (IY+@TYPE),10H
        JR @E8

@E32:   DEFW ZCHKQUO            ;OR STRING CONSTANT
        JR      C,@E4           ;NOT STRING OPERAND
        DEFW  ZSTRCON
        JR C,@EFAIL
        JR      @E65            ;GO GET OPERATOR
 
;HERE IF NOT STRING OPERAND
 
@E4:    LD      (IY+@TYPE),0    ;;MUST BE NUMERIC
        DEFW ZNCONST            ;NUMERIC CONSTANT
        JR      C,@E5-$         ;NO
        LD      A,YPSHFP        ;TO STACK IT
        DEFW ZCMTHRE
        JR      @OPRTR-$        ;GO GET OPERATOR
 
;NOT NUMERIC CONSTANT
 
@E5:    DEFW ZASFNM             ;CHECK FOR ARITH FN.
        JR C,@E6
        DEFW ZNUMFN             ;here if found
        LD (IY+@TYPE),0
        JR @E8

@E6:    DEFW ZBVARIA            ;IS IT A VARIABLE?
        LD A,ERRNOF
        JR C,@EFAIL
        LD A,YDEREF
        BIT 6,B
        JR Z,@E7
@E65:   LD (IY+@TYPE),10H       ;here if string
        LD A,YPSHST
@E7:    DEFW ZCMONE             ;GET VALUE
 
;NOW TO FIND AN OPERATOR
 
@E8:    JR C,@EFAIL
@OPRTR: LD HL,@DIOP             ;SELECT LIST OF DIADIC OPRS
        DEFW ZSCAN              ;SEARCH FOR IT
        POP HL                  ;GET LAST OPR
        JR      C,@E9           ;IF HAVE TERMINATOR
 
;HAVE A NEW OPERATOR - CODE IS IN BC
 
        LD A,H
        OR      0FH     ;A=BINDING POWER & "F
        CP      B               ;TIGHTER BINDING
        JR      NC,@E9-$        ;NO
        PUSH HL                 ;ELSE PUT IT BACK
 
;BINDING IS TIGHTER
 
        LD      A,(IY+@TYPE)    ;TYPE OF LEFT OPERAND
        OR      A               ;IS IT NUMERIC
        JR      Z,@E10-$        ;YES
        LD      A,B             ;SAVE BINDING POWER
        RL      C
        RLA
        RL      C
        RLA
        LD      C,A             ;TO BOTTOM OF C
@E10:   PUSH    BC              ;STACK OPERATOR
        LD      A,C
        XOR     (IY+@TYPE)  ;CHECK CURRENT & R OPD TYPES
        BIT     4,A             ;DO THEY AGREE?
        JP      Z,@OPRND        ;YES GET OPERAND
@E14:   LD      A,ERRNOP        ;TYPE ERROR
        JR      @EFAIL
 
;HERE IF NO OPERATOR OR LESS THAN OR EQUAL BINDING POWER
 
@E9:    DEFW ZRSINST    ;MOVE BACK BEFORE OPERATOR(IF ANY)
        LD      A,H             ;BINDING POWER
        OR      A               ;CHECK IF ZERO
        LD      A,(IY+@TYPE)    ;GET TYPE OF RESULT
        RET     Z               ;RETURN IF A TERMINATOR
        XOR     L               ;DO TYPES AGREE?
        BIT     4,A
        JR      NZ,@E14         ;NO - ERROR
        LD      (IY+@TYPE),0    ;SET TYPE NUMERIC
        BIT     5,L             ;IF RESULT IS NUMERIC
        JR      Z,@E11-$        ;BRANCH IF YES
        LD      (IY+@TYPE),10H  ;ELSE TYPE=STRING
@E11:   LD      A,L
        AND     0FH             ;REMOVE TOP 4 BITS
        BIT     4,L             ;IS OPERATOR NUMERIC
        JR      Z,@E12-$        ;YES
        CP 0FH
        JR Z,@E14               ;IF "ERROR" CODE
 
;STRING OPERATOR
 
        ADD     A,YSTROP        ;POINTER TO OP. LIST
        JR      @E7
 
;HERE IF NUMERIC OPERATOR
 
@E12:   ADD     A,40H           ;SET AS CLASS 1
        CP      43H             ;ASSUMING MONADIC
        JR      C,@E7           ;BRANCH IF IT IS
        SUB 23H         ;MAKE CLASS 2 & ADJ DISPLACEMENT
        JR      @E7             
 
NUMFN:  ;Syntax check num. fn. args and compile
        ;numeric function call.
        ;Enter with BC = fn. data
 
        BIT 7,B
        JP NZ CUFN
        BIT 0,B         ;1 argument?
        JR Z NMF9       ;if no - must be class 0 arith
        DEFW ZCHKOPN    ;compile one
        BIT 1,B
NMF7:   PUSH BC         ;save op code
        JR Z NMF2
NMF6:   DEFW WSTREXP
        DEFB 21H        ;code for LD HL,.. i.e. skip
NMF2:   DEFW WNUMEXP    ;numeric arg.
        POP BC
NMF1:   DEFW WCHKCLS    ;preserves op-code in C
NMF4:   RET C           ;if error
NMF8:   LD A,C
NMF41:  DEFW ZCMONE
        RET
 
NMF9:   BIT 1,B         ;for INSTR
        JR Z NMF8
        PUSH BC
        DEFW ZCHKOPN
        DEFW WSTREXP
        DEFW WCHKCOM
        DEFW WSTREXP
        JR LSTF3
 
STRSFN: ;Syntax check arguments for string
        ;valued function and compile fn. call
 
        BIT 7,B         ;is it a user fn?
        JP NZ,CUFN      ;jump if it is
        LD A,C          ;Y-code
        OR A
        JR Z LSTF6      ;is it FILE$
        DEFW ZCHKOPN
        RET C           ;if no '('
        LD A,C          ;Y-code
        PUSH BC         ;save fn data
        CP YSTCHR
        JR Z NMF2       ;if CHR$ (in 8K BASIC)
        CP YSTSTR
        JR Z LSTF1      ;if STR$
        CP YMID
        JR Z LSTF2      ;if MID$
        DEFW ZSTREXP    ;here if LEFT$ or RIGHT$
        DEFW WCHKCOM
        JR NMF2
 
LSTF2:  DEFW ZSTREXP    ;here if MID$
        DEFW WCHKCOM
        DEFW WNUMEXP
LSTF3:  JR C NMF2       ;if error so far
        DEFW WCHKCOM
        JR NC NMF2      ;if 3 arguments
        POP BC
        INC C           ;increment to YMID1
        OR A            ;clear carry
        JR NMF1         ;if 2 arguments
 
LSTF1:  DEFW ZNUMEXP
        DEFW WFORMAT    ;arg for STR$-check for
        POP HL          ;formatted conversion
        RET C           ;if invalid fmt
        LD H,C          ;Compile op-code and
        DEFW ZCM2HL     ;lsb of fmt.
        LD C,B
        JR NMF1         ;Chk ')' and compile
                        ;msb of fmt.
 
LSTF6:  LD BC,8000H+4000H+1023
        CALL VARIA9
        LD C,YPSHST
        JR NMF4
 
        EJECT
VARIAB: EQU     $
;COMPILE CODE FOR VARIABLE ADDRESS
;ON EXIT
;      CY SET IF NOT A VARIABLE
;   OTHERWISE
;      BC=NAME-TYPE
        DEFW ZVARIA
VARIA9: DEFW WADDSYM
        RET     C
        BIT 4,(IY+FLAGS)
        JR Z VARIA0     ;if not compiling DEF
        LD A,C
        CP (IY+FPAR)
        JR NZ VARIA0
        LD A,B
        CP (IY+FPAR+1)
        LD A,YAATOF
        JR Z NMF41
VARIA0: INC     HL
        INC     HL
        DEFW ZCPSH
        RET     C
        LD      A,B
        AND     030H
        RET     Z               ;RETURN IF NOT ARRAY
        PUSH    HL
        PUSH    BC
        DEFW ZRDNCH     ;SKIP (
        DEFW ZNUMEXP
        POP     BC
        POP     HL
        RET     C               ;RET IF SUB-SCRIPT ERROR
        DEFW    ZCHKCOM
        LD      A,19
        JR      NC,VARIA1       ;J.IF 2ND SUB-SCR.
        BIT     4,B
        RET     Z               ;IF 2ND SUB-SCR. MISSING
        RES     5,B
        LD      A,YAR1AD
        JR      VARIA2
 
VARIA1: BIT     5,B
        SCF
        RET     Z       ;IF 2ND SUB-SCR. NOT WANTED
        PUSH    HL
        RES     4,B
        PUSH BC
        DEFW ZNUMEXP
        POP     BC
        POP     HL
        RET     C               ;RET IF SUB-SCRIPT ERROR
        LD      A,YAR2AD
VARIA2: EQU     $
        DEFW    ZCMONE
        DEFW    WCHKCLS
        RET     C
        DEC     HL
        LD      (HL),B  ;IN     CASE    1ST.REF.
        DEC     HL
        PUSH    BC
        LD      DE,     10
        BIT     5,B
        LD      B,D
        LD      C,E
        JR      NZ,VARIA3               ;if     2-D
        LD      A,(IY+BASFLG)
        AND     1
        LD      E,A
VARIA3: DEFW    ZXDIM
        POP     BC
        RET     NC
        CP      71              ;clear carry unless was error 10
        RET
;
VARIA:  EQU     $
;READ VARIABLE NAME TO BC, SET BITS 12 & 13 IF
;FOLLOWED BY '('
;PRESERVES DE
        DEFW ZGETVNM
        LD A,26
        RET     C
        DEFW ZRDSCH
        XOR     '('
        RET     NZ
        SET     4,B
        SET     5,B
        RET
        END

#
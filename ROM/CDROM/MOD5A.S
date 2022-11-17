
        NAME MOD5A
 
;Compile and execute User-Defined Functions
 
;INTERNALLY DEFINED GLOBALS
 
        GLOBAL CDEF
        GLOBAL RDFN
        GLOBAL XNUFN
        GLOBAL XSTFN
        GLOBAL XNOFN
        GLOBAL CUFN
        GLOBAL TYFN8
        GLOBAL CDEF8
 
;EXTERNALLY DEFINED GLOBALS
 
        GLOBAL LXR21
        GLOBAL XRUN1D
        GLOBAL NMF1
        GLOBAL NMF7
        GLOBAL GUSTP
        GLOBAL GINPTR
        GLOBAL LINPTR
 
;W & Z codes
 
        GLOBAL ZDLUSS
        GLOBAL ZCHECK
        GLOBAL ZREPORT
        GLOBAL ZCMOBJ
        GLOBAL ZCPSH
        GLOBAL ZINUSS
        GLOBAL ZADDSYM
        GLOBAL ZCHKCLS
        GLOBAL ZCHKOPN
        GLOBAL ZCMONE
        GLOBAL ZFLNT
        GLOBAL ZNLNT
        GLOBAL ZRSINST
        GLOBAL ZRDFN
        GLOBAL ZSTINST
        GLOBAL ZSVINST
        GLOBAL ZVARIA
        GLOBAL WSTREXP
        GLOBAL WCPSH0
        GLOBAL WRDFN
        GLOBAL WCHKCLS
        GLOBAL WCHKCR
        GLOBAL WCHKEQ
        GLOBAL WCREM
        GLOBAL WNUMEXP
        GLOBAL WVARIA
 
;Offsets from IY
 
FPAR    EQU 037H        ;RAM
APAR    EQU 035H        ;RAM
FLAGS   EQU 022H        ;RAM
ARBAS   EQU 01AH
OMOVE   EQU 02CH
 
;Y codes
 
YNUFN   EQU 94H
YNOFN   EQU 96H
YSTFN   EQU 97H
 
;T codes
 
TFN     EQU 80H+56
TDEF    EQU 80H+33
 
;ERROR NUMBERS
 
RDFNE1  EQU 80          ;Can't find 'FN'
RDFNE2  EQU 81          ;Invalid function name
FNERR   EQU 83          ;Can't compile function
TYFNER  EQU 84          ;Formal par got new type
                        ;parameter expression
 
        EJECT
CDEF:   EQU $
 
;COMPILE DEF STATEMENT
;
;SYM.TAB.ENTRY FOR FN : 2 BYTES NAME/TYPE
;                       2 BYTES SOURCE LINE NO.
;                       4 BYTES FREE
;
;COMPILED CODE IS <YREM>,<NUMEXP>,<YREM>
 
        DEFW ZRDFN      ;LOOK FOR 'FN'<NUM.VAR.>
        RET C           ;R.invalid fmt./no room 
        INC HL
        LD A,B
        AND 030H
        LD E,B
        LD (IY+FPAR+1),A
        JR Z CDEF3
        CALL TYFN
        LD (IY+FPAR),C
        LD (IY+FPAR+1),B ;save n/t formal param.
CDEF3:  DEFW WCHKEQ
        DEFW WCREM      ;COMPILE LEADING RET
        SET 4,(IY+FLAGS);SET 'COMPILING DEF' FLAG
        BIT 6,E         ;what type?
        JR NZ CDEF1
        DEFW WNUMEXP
        DEFB 21H        ;code for LD HL,.. i.e. skip
CDEF1:  DEFW WSTREXP
        RES 4,(IY+FLAGS)
CDEF8:  DEFW WCREM      ;comp. trail. user ret
        DEFW WCHKCR
        RET
 
 
TYFN:   DEFW ZCHKOPN
        PUSH HL
        DEFW WVARIA
        POP HL
        DEFW WCHKCLS
        RET C           ;J.IF NOT '('<VAR.>
        LD A,B
        AND 00FCH       ;A=TYPE OF VARIA
TYFN8:  CP 0080H
        JR Z,TYFN1      ;J.if num.scalar
        CP 00C0H
        JR NZ TYFN3
        BIT 5,E
        JR Z TYFN3
        RES 4,(HL)
        RET
 
TYFN1:  BIT 4,E
        JR Z TYFN3
        RES 5,(HL)
        RET
 
TYFN3:  SCF
        LD A,TYFNER
        RET
 
 
RDFN:   EQU $
 
;ROUTINE TO READ 'FN'<NUM.VAR.>
;EXIT:  CY IF NO ROOM FROM ADDSYM  A=ERROR CODE
;       CY IF INVALID FN FORMAT    A=ERROR CODE
;       CY IF NOT 'FN'             A=ERROR CODE
;       M  IF NEW ENTRY IN SYM.TAB
;       HL -> name/type in ST,BC=name/type
 
        DEFW ZSVINST
        LD B,TFN
        DEFW ZCHECK
        LD A,RDFNE1
        RET C
        DEFW  ZVARIA
        JR C,RDFNB      ;J.IF NOT <VAR.>
        SET 3,B         ;set n/t TYPE for Usr.def
        DEFW ZADDSYM
        RET             ;CY IF NO ROOM & A=10
                        ;M  IF NEW ENTRY IN SYM.TAB
                        ;HL=SYM.TAB.ENTRY ADR.
 
RDFNB:  DEFW ZRSINST    ;RESTORE INSTREAM STATE
        SCF
        LD A,RDFNE2     ;INVALID FN FORMAT
        RET                       
 
 
CUFN:   CALL GINPTR
        PUSH HL
        DEFW ZFLNT      ;HL=-> first lnt entry
FNEXPC: JR C,FNEXPJ
        PUSH HL
        PUSH BC
        DEFW ZSTINST
        LD B,TDEF
        DEFW ZCHECK
        DEFW WRDFN      ;LOOK FOR 'FNnumvar.'
        JR NC,FNEXPG    ;J.if no error
FNEXPD: POP BC
        POP HL
        DEFW ZNLNT
        JR FNEXPC
 
FNEXPG: INC HL
        LD E,(HL)
        LD A,E
        AND 030H
        JR Z FNEXP2
        PUSH BC
        CALL TYFN
        POP BC
        JP C XUFNF
FNEXP2: POP DE
        PUSH DE
        LD A,D
        CP B
        JR NZ,FNEXPD    ;if n/t does not match
        LD A,C
        CP E
        JR NZ,FNEXPD    ;J.IF NOT NAME/TYPE MATCH
        PUSH HL
        LD B,(IY+FLAGS)
        PUSH BC
        SET 5,(IY+FLAGS)
        LD E,D
        CALL CDEF3
        POP BC
        LD (IY+FLAGS),B
        POP DE
        POP BC
        POP HL
        EX (SP),HL
        CALL LINPTR
        POP HL
        JR NC LCFN1
        CP 85
        SCF
        RET Z           ;return only error 85
LCFN1:  EQU $           ;HL->to the LNT entry
        DEFW ZCPSH
        RET C
        LD A,(DE)
        AND 030H
        JR Z FNEXP5
        LD B,A
        DEFW ZCHKOPN
        LD C,YNUFN
        BIT 5,B
        JP Z NMF7
        DEFW WSTREXP
        DEFW WCPSH0
        LD C,YSTFN
        JP NMF1
 
FNEXP5: LD A,YNOFN
        DEFW ZCMONE
        RET
 
FNEXPJ: LD A,FNERR              
        POP HL
        RET
 
XSTFN:  PUSH HL
        CALL GUSTP
        PUSH HL
        INC HL
        INC HL
        LD E,(IY+ARBAS)
        LD D,(IY+ARBAS+1)
        LD C,(HL)
        INC HL
        LD B,(HL)
        OR A
        INC HL
        SBC HL,DE
        EX DE,HL
        POP HL
        LD A,B
        OR C
        JR Z XSTF1
        LD (HL),E
        INC HL
        LD (HL),D
XSTF1:  LD HL,6
        ADD HL,BC
        EX (SP),HL
        POP BC
        JR XUFN
 
XNOFN:  LD BC,2
        JR XUFN
 
XNUFN:  LD BC,8
XUFN:           ;RUN ROUTINE FOR USER.FN ..THE TRICKY BIT       
                ;enters:HL->obj code for UFN
                ;  IX->line # byte of lnt
        PUSH BC
        EX DE,HL
        DEFW ZCMOBJ
        EX DE,HL
        OR A            ;CLEAR CY
        SBC HL,DE       ;HL=CURR.DISP.IN OBJ.LINE
        PUSH HL         ;SAVE DISP. ON M/S
        CALL GUSTP
        PUSH HL
        LD E,(IY+APAR)
        LD D,(IY+APAR+1)
        LD (IY+APAR),L
        LD (IY+APAR+1),H
        PUSH IX
        PUSH DE
        ADD HL,BC       ;HL=adr.of line # of DEF
        DEC HL
        LD A,(HL)
        DEC HL
        LD L,(HL)
        LD H,A          ;HL->LNT of DEF
        PUSH HL
        POP IX          ;IX=lnt.entry addr of DEF
        DEFW ZCMOBJ
        INC HL          ;skip over YREM
        CALL NC LXR21 
        JR C,XUFND
        POP HL          ;DE=displ.of APAR
        LD (IY+APAR),L
        LD (IY+APAR+1),H
        LD (IY+OMOVE+1),0
        POP IX
        DEFW ZCMOBJ
        JR C,XUFNF      ;if no room to compile
        POP DE
        POP BC
        ADD HL,BC       ;HL->obj code to be 
        POP BC
        PUSH HL
        EX DE,HL
        DEFW ZDLUSS     ; dispose of excess in user stack
                        ; i.e. LNT entry and act. par.
                        ; and put answer in right place
XFRETC: JP XRUN1D                 
 
XUFND:  POP HL
        PUSH AF
        DEFW ZREPORT
        POP AF
        POP IX
XUFNF:  POP BC
        POP BC
        POP BC
        RET
 
        END

#
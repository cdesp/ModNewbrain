
        NAME MOD2
 
;Various compile and execute routines
 
;INTERNALLY DEFINED GLOBALS

        GLOBAL XRUN
        GLOBAL XRET
        GLOBAL XRESU1
        GLOBAL XRESU
        GLOBAL CRESU
        GLOBAL LXG2
        GLOBAL LXG1
        GLOBAL CGOSUB
        GLOBAL CGOTO
        GLOBAL CLABEL
        GLOBAL XGOSUB
        GLOBAL XGOTO
        GLOBAL XLIST
        GLOBAL XLISTN
        GLOBAL CIF
        GLOBAL XON
        GLOBAL XONSUB
        GLOBAL XRESU2
        GLOBAL SETS2
        GLOBAL SETSN

;EXTERNALLY DEFINED GLOBALS 

        GLOBAL XRUN1D
        GLOBAL GGSP
        GLOBAL KWDS
        GLOBAL OKWDS
        GLOBAL CHKSTB

        EJECT
;W & Z codes

        GLOBAL ZCHECK
        GLOBAL ZTSTPPF
        GLOBAL ZUPDPPF
        GLOBAL ZPTNL
        GLOBAL WNBKTST
        GLOBAL WCHKCR
        GLOBAL WFIND
        GLOBAL ZCMTHRE
        GLOBAL WCM2HL
        GLOBAL WCLABEL
        GLOBAL ZRDSCH
        GLOBAL ZDIGIT
        GLOBAL ZINTIN
        GLOBAL ZFIND
        GLOBAL ZCHKCR
        GLOBAL ZINSS
        GLOBAL ZDELT
        GLOBAL ZNUMEXP
        GLOBAL ZINITRAND
        GLOBAL ZCMONE
        GLOBAL ZSCAN
        GLOBAL ZUNSTSS
        GLOBAL ZNLNT
        GLOBAL ZPTCHAR
        GLOBAL ZPTNL
        GLOBAL ZPT#HL
        GLOBAL ZLSTATE
        GLOBAL ZLAST
        GLOBAL ZINITST
        GLOBAL ZFLNT
        GLOBAL WRDNCH
        GLOBAL ZSTINST
 
        EJECT
;Offsets from IY

GSSPTR  EQU 16H
PPFLGS  EQU 2EH
FLAGS   EQU 22H
 
;Y codes
 
YRESU1  EQU 80H
YRESU   EQU 92H
YGOSUB  EQU 0C3H
YIF     EQU 0C5H
YGOTO   EQU 0C2H
 
;T codes
 
TBASE   EQU 80H+48
TTHEN   EQU 80H+52
TINVTH  EQU 80H+53      ;the infamous invisible THEN
 
        EJECT
 
CLABEL: DEFW ZINTIN
        DEFW WFIND
        LD A,29
        DEFW WCM2HL
        RET

XGOSUB: EQU $
        LD E,(HL)
        INC HL
        LD D,(HL)
LXG1:   PUSH DE
        LD BC,3
        LD E,GSSPTR
        DEFW ZINSS
        POP DE
        RET C
        PUSH IX
        POP BC
        LD (HL),C
        INC HL
        LD (HL),B
        JR LXG3
 
XRESU:  CALL XRESU2
        RET C
        JR LXG2
 
XRESU1: PUSH HL
        CALL XRESU2
        POP HL
        RET C
XGOTO:  EQU $
        LD E,(HL)
        INC HL
        LD D,(HL)
LXG3:   EX DE,HL
LXG2:   LD A,254
        SCF
        RET
 
XRESU2: RES 1,(IY+FLAGS)
XRET:   EQU $
        CALL GGSP
        EX DE,HL
        PUSH HL
        CALL CHKSTB
        POP HL
        LD A,33
        SCF
        RET Z
        LD E,(HL)
        INC HL
        LD D,(HL)
        DEC HL
        LD BC,3
        PUSH DE
        LD E,GSSPTR
        DEFW ZDELT
        POP HL
        PUSH HL         ;return addr in HL & IX !
        POP IX
        RET
 
XRUN:   EQU $
        DEFW ZINITST    ;initialise symbol table
        DEFW ZINITRAND
        DEFW ZFLNT
        JR NC LXG2
        LD A,3
        RET
 
CRESU:          ;compile code for RESUME
        DEFW ZCHKCR
        LD A,YRESU1
        JR C GO
        LD A,YRESU
        DEFW ZCMONE
        RET
 
CGOSUB: EQU $
        LD A,YGOSUB
        DEFB 21H        ;op-code for LD HL,nn-i.e. skip
CGOTO:  EQU $
        LD A,YGOTO
GO:     DEFW ZCMONE
        DEFW WCLABEL
        DEFW WCHKCR
        RET
 
CIF:    EQU $
        DEFW ZNUMEXP
        RET C           ;if not numeric exp.
        PUSH IX
        POP HL
CIFL1:  DEFW ZLAST
        JR Z CIFL1
        LD A,YIF
        DEFW ZCMTHRE    ;comp.cond.ret
        LD HL,CNSQLS
        DEFW ZSCAN
        LD A,63
        RET C           ;if no THEN (visible or not)
        DEFW ZRDSCH
        DEFW ZDIGIT
        JR NC,CGOTO     ;if 'then' <line#>
        DEFW ZLSTATE    ;if 'then' otherwise
        RET
 
CNSQLS: EQU $
        DEFB TTHEN,2    ;includes the notrious
                        ;invisible THEN
 
 
XONSUB: PUSH HL
        DEFW ZUNSTSS
        LD BC,XGOSUB
        JR LXON2
XON:    EQU $
        PUSH HL
        DEFW ZUNSTSS
        LD BC,XGOTO
LXON2:  POP HL
        RET C
XON1:   LD A,(HL)
        INC HL
        OR (HL)
        LD A,28
        SCF
        RET Z           ;if no more labels
        INC HL
        DEC DE
        LD A,D
        OR E
        JR NZ,XON1
        DEC HL
        DEC HL
        PUSH BC
        RET
 
 
 
SETS2:          ;set 2 2-byte locs at offsets E and
                ;   E+2 from IY each to HL
                ;returns HL->offset E+4,sets A=D=0
                ;BC=original HL,preserves E
        LD A,2
SETSN:          ;as SETS2 but for N 2-byte locs
                ;enter with A=N
        PUSH IY
        EX (SP),HL
        POP BC
        LD D,0
        ADD HL,DE
COPYN:          ;copy BC to each of N 2-byte locs
                ;starting at (HL)
        LD (HL),C
        INC HL
        LD (HL),B
        INC HL
        DEC A
        JR NZ,COPYN
        RET
 
 
XLISTN: LD DE,0         ;LIST WITH NO LINE NUMBERS
        LD BC,-1        ;   SPECIFIED (VIZ LIST ALL)
        JR LIST
 
XLIST:  LD C,(HL)
        INC HL
        LD B,(HL)       ;BC:='TO' line #
        INC HL
        LD E,(HL)
        INC HL
        LD D,(HL)       ;DE:='FROM' line #
LIST:   PUSH BC
        EX DE,HL
        DEFW ZFIND 
        LD DE,-6
        ADD HL,DE       ;move back so NLNT will test for
LXL2:   POP BC
        DEFW ZPTNL
        DEFW WNBKTST
        RET C
        DEFW ZNLNT      ;   end of prog.
        CCF
        RET NC          ;if end of prog.
        LD E,(HL)
        INC HL
        LD D,(HL)
        DEC HL
        EX DE,HL
        SBC HL,BC       ;carry set above
        RET NC          ;if beyond 'TO' line
        ADC HL,BC       ;*BS* LINE# NOW IN HL
        PUSH BC
        PUSH DE
        DEFW ZPT#HL     ;*BS* PRINT LINE#
        POP HL
        JR C LXLC
        PUSH HL         ;ptr to LNT
        DEFW ZSTINST    ;now HL points to source
        LD A,' '
        CP (HL)
        JR Z LXL21      ;if first chr is space
        DEFW ZPTCHAR    ;print a space
LXL21:  POP HL
        JR C LXLC
LXL5:   PUSH HL
        DEFW ZSTINST
        LD A,(IY+FLAGS)
        AND 4
        LD (IY+PPFLGS),A
        LD A,(HL)
LXL3:   CP 15Q
        JR NZ,LXL6
        POP HL
        DEFW ZLAST
        JR NZ LXL2
        LD A,':'
        DEFW ZPTCHAR
        JR C LXLC
        JR LXL5
 
LXL6:   DEFW ZTSTPPF
        PUSH AF
        DEFW ZUPDPPF
        POP AF
        JR C LXL4
        LD HL,KWDS
        JR Z LXL0
        LD HL,OKWDS
        CP TINVTH       ;the stupendous invisible THEN
        JR Z LXL8
LXL0:   BIT 7,A
        JR Z LXL4
        SUB (HL)
        INC HL
        INC HL
        JR Z LXL7
LXL1:   BIT 7,(HL)
        INC HL
        JR Z LXL1
        DEC A
        JR NZ LXL1
LXL7:   LD A,(HL)
        BIT 7,A
        JR NZ LXL9
        INC HL
        DEFW ZPTCHAR
        JR C LXLA
        JR LXL7
 
LXL9:   AND 7FH
LXL4:   DEFW ZPTCHAR
LXL8:   DEFW WRDNCH
        JR NC LXL3
LXLA:   POP HL
LXLC:   POP HL
        RET
 
        END

#
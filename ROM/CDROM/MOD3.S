        NAME MOD3
 
;Various compile and execute routines
 
;INTERNALLY DEFINED GLOBALS

        GLOBAL STCIN
        GLOBAL CPRINT
        GLOBAL FORMAT
        GLOBAL CDEVO
        GLOBAL CPINT
        GLOBAL XSTDFO
        GLOBAL XSTICN
        GLOBAL XSTOCN
        GLOBAL XNUMIN
        GLOBAL XSTRIN
        GLOBAL QSTR
        GLOBAL XQDEF
        GLOBAL XQSTR
        GLOBAL XPCOM
        GLOBAL XPFFF
        GLOBAL XPSTR
        GLOBAL XPTAB
        GLOBAL XSEP
        GLOBAL XENDIN
        GLOBAL LDSTR
        GLOBAL PRNSTR
        GLOBAL PRSTR
        GLOBAL LDUSTP
        GLOBAL XLINP

;EXTERNALLY DEFINED GLOBALS

        GLOBAL GINPTR
        GLOBAL LINPTR
        GLOBAL XRUN1D
        GLOBAL PHPOS
        GLOBAL PLLEN
        GLOBAL PZLEN
        GLOBAL DISPOSE
        GLOBAL ASSSTR
        GLOBAL LXG2
        GLOBAL DEFNF

        EJECT
;W & Z codes

        GLOBAL ZMKRL
        GLOBAL ZMKAB
        GLOBAL ZPUSH2
        GLOBAL ZNOTBC
        GLOBAL ZCHECK
        GLOBAL ZPTNL
        GLOBAL ZREPORT
        GLOBAL WRDLINE
        GLOBAL ZINP
        GLOBAL WCHKCLS
        GLOBAL WCHKCOM
        GLOBAL WNUMEXP
        GLOBAL WNEWSTR
        GLOBAL WPOP
        GLOBAL WPTCHAR
        GLOBAL WRDNCH
        GLOBAL WSTF
        GLOBAL ZBVARIA
        GLOBAL ZCHKCOM
        GLOBAL ZCHKCR
        GLOBAL ZCHKHSH
        GLOBAL ZCHKOPN
        GLOBAL ZCHKQUO
        GLOBAL ZCDEVO
        GLOBAL ZCM2HL
        GLOBAL ZCMONE
        GLOBAL ZFORMAT
        GLOBAL ZCPINT
        GLOBAL ZEXPN
        GLOBAL ZINTIN
        GLOBAL ZINSS
        GLOBAL ZLDF
        GLOBAL ZMKNULL
        GLOBAL ZNUMEXP
        GLOBAL ZOUT
        GLOBAL ZPRSTR
        GLOBAL ZPRNSTR
        GLOBAL ZPTCHAR
        GLOBAL ZPTMSG
        GLOBAL ZPOP
        GLOBAL ZQSTR
        GLOBAL ZRDNCH
        GLOBAL ZRDSCH
        GLOBAL ZRDNSCH
        GLOBAL ZSTREXP
        GLOBAL ZSVINST
        GLOBAL ZTTCAPS
        GLOBAL ZUNSTSS
        GLOBAL ZUNST6
        GLOBAL ZUNST2
        GLOBAL WRDCH
        GLOBAL ZRDCH

;Offsets from IY

STBAS   EQU 18H
ARBAS   EQU 1AH
INCON   EQU 7
OUTCON  EQU 6
USRSTP  EQU 12H

;Y codes

YPFFF   EQU 0D0H
YREM    EQU 0E5H
YPTNL   EQU 0D2H
YPCOM   EQU 0D1H
YSTICN  EQU 0CAH
YQDEF   EQU 0D4H
YQSTR   EQU 0D3H
YSEP    EQU 0D7H
YENDIN  EQU 0EEH
YPTAB   EQU 0CEH
YPSTR   EQU 0CFH
YSTDFO  EQU 0CCH
YSTOCN  EQU 0CBH
 
;T code
 
TTAB    EQU 80H+57
 
        EJECT
CDEVO:          ;Compile O/P device # or select
                ;   default O/P device
        DEFW ZCHKHSH
        LD A,YSTDFO
        JR C,CDEVO1
        DEFW ZNUMEXP
        RET C
        DEFW ZCHKCR
        JR NC LCDV1
        DEFW ZCHKCOM
        RET C
LCDV1:  LD A,YSTOCN
CDEVO1  DEFW ZCMONE
        RET
 
 
CPRINT: DEFW ZCDEVO
        RET C
        LD B,YPTNL
LCP2:   CALL CPSEP
LCP3:   LD B,YREM
        JR NC,LCP2
        LD B,TTAB
        DEFW ZCHECK
        JR C,CPRIN2
        DEFW ZCHKOPN
        DEFW WNUMEXP
        DEFW WCHKCLS
        RET C
        LD A,YPTAB
        JR CPRIN4
 
CPRIN2: DEFW ZEXPN
        RET C
        OR A
        LD A,YPSTR
        JR NZ,CPRIN4    ;if string
        DEFW ZFORMAT    ;else numeric: check for
        RET C           ;formatted print option
        LD L,YPFFF
        LD H,C
        DEFW ZCM2HL
        LD A,B
CPRIN4: DEFW ZCMONE
        LD B,YPTNL
        CALL CPSEP
        JR NC,LCP3
        LD A,31
        RET
 
CPSEP:          ;enter with B=YREM or YPTNL
                ;if newline, emits B & exits caller
                ;if comma, emits YPCOM, calls RDCHAR
                ;& sets NC
                ;if semicolon, calls RDCHARS & sets NC
                ;else returns carry set
                ;preserves BCDEHL
        DEFW ZRDSCH
        CP ','
        JR Z,LCPS4
        CP ';'
        JR Z,LCPS5
        CP 15Q
        SCF
        RET NZ
        POP AF          ;lose ret addr
        LD A,B
        DEFW ZCMONE
        RET
 
LCPS4:  LD A,YPCOM
        DEFW ZCMONE
LCPS5:  DEFW WRDNCH
        RET
 
FORMAT:         ;Syntax check square bracket notation
                ;for formatted numeric output. Returns
                ;BC = format param. for maths pack. If
                ;no '[' found returns with significant
                ;instream unchanged, CY clear and BC =
                ;default value. Else either returns 
                ;with CY clear, BC = value, instream
                ;one past ']', or CY set and A = errno.
                ;ABCDEFHL destroyed.
        DEFW ZRDSCH
        CP '['
        LD BC,(DEFNF)           ;DEFAULT VALUE
        SCF
        CCF
        RET NZ
        DEFW ZCPINT
        RET C
        LD B,C
        DEFW ZRDSCH
        DEFW ZTTCAPS
        CP 'F'
        JR NZ,LFMT2
        LD B,40H
        JR LFMT4
 
LFMT2:  CP '.'
        JR NZ LFMT3
        DEFW ZCPINT
        RET C
        LD A,B
        ADD A,C
        JR C,LFMT8
        LD C,A
        DEFW ZRDSCH
        DEFW ZTTCAPS
LFMT3:  CP 'E'
        JR NZ,LFMT5
        SET 7,B
LFMT4:  DEFW ZRDNSCH
LFMT5:  CP ']'
        SCF
        JR NZ,LFMT8
        DEFW ZRDNSCH
        LD A,C
        DEC A
        CP 10
        CCF
LFMT8:  LD A,74
        RET
 
CPINT:          ;Compile integer follg cur char:
                ;   check < 256
                ;result in C, error 75 if too big
                ;preserves BDE
        DEFW ZRDNCH
        DEFW ZINTIN
        LD A,0
        SBC A,H         ;set carry if H nonzero or
        LD C,L          ;   if carry was already set
        LD A,75
        RET
 
 
XSTOCN: EQU $
        PUSH HL
        DEFW ZUNSTSS
        POP HL
        LD A,0
        SBC A,D         ;now CY set iff DE>255
        LD A,90         ;errno for illegal stream no.
        RET C           ;if illegal stream no.
        DEFB 01H        ;op-code for LD BC,nn-i.e.skip
XSTDFO: LD E,0          ;default stream number
XSTOC1: LD (IY+OUTCON),E
        SCF
        SBC A,A
        RET
 
 
XPFFF: LD C,(HL)
        INC HL
        LD B,(HL)
        INC HL
        PUSH HL ;REMEMBER OBJ CODE ADDR. FORGET
        PUSH BC         ;-RET ADDR.
        DEFW ZUNST6
        EX DE,HL
        DEFW ZLDF
        EX DE,HL
        POP BC
        DEFW ZOUT
        LD B,0          ;BC=CHAR.CT.
        DEFW ZPRSTR
        JR XPSTR1
 
XPSTR:  PUSH HL
        DEFW ZPRNSTR
XPSTR1: JP XRUN1D
 
PRNSTR: EQU $
        DEFW ZUNST2
        LD B,H
        LD C,L
        EX DE,HL
        DEFW ZPRSTR
LDUSTP: LD (IY+USRSTP),L        ;Load USRSTP from HL
        LD (IY+USRSTP+1),H
        RET
 
PRSTR:  EQU $
        CALL PHPR
        JR C LPS5       ;if PHPOS not supported
        PUSH HL
        LD HL,(PHPOS)
        LD H,0
        LD D,H
        LD E,A          ;so DE = line length
        DEC L           ;so HL = (PHPOS)-1
        JR Z LPS4       ;if at start of line
        ADD HL,BC
        JR C LPS3       ;if very long string
        EX DE,HL
        SBC HL,DE
        JR NC LPS4      ;if fits
LPS3:   DEFW ZPTNL
LPS4:   POP HL
        RET C           ;if PTNL error
LPS5:   LD A,C
        OR B
        RET Z
        LD A,(HL)
        DEFW ZPTCHAR
        RET C
        INC HL
        DEC BC
        JR LPS5
 
XPCOM:          ;Print a comma separator
        PUSH HL
        CALL PHPR
        JR C LPC6       ;if not PHPOS adj.
        LD A,(PZLEN)
        LD B,A
        XOR A
LPC2:   ADD A,B
        CALL TSTPHP
        JR C,LPC2
        INC A           ;now A = req'd col #
        LD HL,PLLEN
        CP (HL)
        LD L,A
        JR C,TAB        ;unless beyond end of line
        LD L,1
        JR TAB
 
LPC6:   LD A,09H        ;Print TAB cntrl char.
        DEFW ZPTCHAR
        JR LTAB9
 
XPTAB:  PUSH HL
        DEFW ZUNSTSS
        JR C,LTAB9
        CALL PHPR
        JR C LPC6
        LD HL,(PLLEN)
        LD H,0
        EX DE,HL
        SCF
PRTAB1: SBC HL,DE
        JR NC,PRTAB1
        ADC HL,DE
        LD A,37         ;NB carry still set
        JR Z,LTAB9      ;if UNSTSS yielded zero
TAB:            ;tab to col # L then go to XRUN1D
        LD A,L
        CALL TSTPHP
        JR NC LTAB2     ;@@@@@@@
        DEFW ZPTNL      ;if already beyond col L
        RET C
LTAB2:  LD A,L
        CALL TSTPHP
        JR Z,LTAB9      ;if at col L
        LD A,' '
        DEFW WPTCHAR
        JR NC,LTAB2     ;unless overshot or PTCHAR error
LTAB9:  JP XRUN1D
 
 
TSTPHP:         ;Set Z if printhead at column A,
                ;CY if beyond.
                ;Preserves ABCDEHL
        PUSH HL
        LD HL,PHPOS
        CP (HL)
        POP HL
        RET
 
PHPR:           ;Set CY if OUTCON stream is not the 
                ;console, or if it is but doesn't 
                ;support PHPOS. If returns CY clear
                ;then A= line length.
                ;Preserves BCDEHL.
        LD A,(IY+OUTCON)
        OR A
        SCF
        RET NZ          ;if not the console
        LD A,(PLLEN)
        OR A
        RET NZ          ;non-zero line length implies
        SCF             ;PHPOS is supported.
        RET
 
        EJECT
 
STCIN   EQU $
        DEFW ZCHKHSH
        JR C,CINP1C
        DEFW ZNUMEXP
        DEFW WCHKCOM
        RET C
        LD A,YSTICN
        JR CINP1B
 
CINP1C: EQU $
        DEFW ZCHKOPN
        LD A,YQDEF
        JR C,CINP1B             ;IF NO USER I/P PROMPT
        DEFW ZSTREXP
        DEFW WCHKCLS
        RET C
        LD A,YQSTR
CINP1B: EQU $
        DEFW ZCMONE
        RET
 
 
XLINP:  DEFW ZUNST2     ;retrieve variable
        EX DE,HL
        PUSH DE
        LD A,0DH
        LD BC,0
        CPIR            ;BC=-length-1
        DEFW ZNOTBC     ;BC=length
        LD H,B
        LD L,C
        DEFW ZPUSH2     ;bung on length
        POP DE          ;HL=variable again
        CALL ASSSTR     ;assign string
        JR XEND1
 
XSTICN: PUSH HL
        DEFW ZUNSTSS
        LD A,0
        SBC A,D         ;Set CY iff DE>255
        LD A,90         ;Err no. for illegal stream no.
        JR LQL8
 
XQSTR:  PUSH HL
        LD (IY+OUTCON),0
        DEFW ZPRNSTR
        JR QRYLIN
 
XQDEF:  PUSH HL
        DEFW ZPTMSG
           DEFB 002Q
           DEFM '? '
QRYLIN: LD E,0
LQL8:   DEFW WRDLINE
        DEFW WRDCH
LQL9:   JP XRUN1D
 
XENDIN: DEFW ZCHKCR
XEND1:  JP NC DISPOSE
        JR LID2
 
XSEP:   PUSH HL
        LD B,09H
        DEFW ZCHECK
        JR NC LQL9
        DEFW ZCHKCOM
INDONE: JR NC,LQL9
        POP HL
LID2:   LD E,(IY+INCON)
        DEC E           ;NB carry still set
        INC E
        RET NZ          ;If not console stream
        AND A
        SCF
        RET Z
        DEFW ZREPORT
        RET C           ;in case ON ERR is set(16K)
        PUSH IX
        POP  HL         ;Get IX into HL so redo line
        JP LXG2
 
XNUMIN: PUSH HL
        DEFW ZRDSCH     ;get rid of LF's
        CALL GINPTR
        EX DE,HL
        DEFW ZINP
        EX DE,HL
        CALL LINPTR
        LD A,30
        DEFW WPOP
        DEFW WSTF
        JR INDONE
 
XSTRIN: PUSH HL
        DEFW ZPOP
        PUSH HL
        DEFW ZMKRL
        DEFW ZMKNULL
        DEFW ZMKAB
        DEFW ZCHKQUO
        JR C,LSI5
        DEFW ZQSTR      ; IF QUOTED STRING
        POP HL
        DEFW WNEWSTR
        DEFW WRDNCH
        JR INDONE
 
LSI5:   DEFW ZSVINST    ; HERE IF UNQUOTED STRING
        LD BC,0
        DEFW ZRDCH
LSI54:  CP 22H
        JR Z,LSI8
        CP 15Q
        JR Z,LSI7
        CP ','
        JR Z,LSI7
        INC BC
        DEFW ZRDNCH
        JR LSI54
 
LSI8:   SCF
        LD A,36
LSI7:   POP HL
        DEFW WNEWSTR
        JR INDONE
 
 
QSTR:           ;SCAN QUOTED STRING
                ;SETS RSINST TO FIRST CHAR, BC=LENGTH
                ;SETS CARRY IF NO CLOSING QUOTE
        DEFW ZSVINST
        LD BC,0
        DEFW ZRDCH
LQS2:   CP 15Q
        JR Z,LQS5
        CP 22H
        JR NZ,LQS4
        DEFW ZRDNCH
        XOR 22H
        RET NZ          ;RETURN NC UNLESS QUOTE IMAGE
LQS4:   INC BC
        DEFW ZRDNCH
        JR LQS2
 
LQS5:   LD A,65
        SCF
        RET
 
 
LDSTR:          ;LOAD STRING INTO VARIABLE
                ;HL->DEST, BC=LENGTH
                ;sets DE -> where to write in string
                ;   area, preserves BC
        PUSH HL
        PUSH BC
        LD E,STBAS
        DEFW ZINSS
        POP BC
        POP DE
        RET C
        PUSH HL
        PUSH DE
        LD D,(IY+ARBAS+1)
        LD E,(IY+ARBAS)
        SBC HL,DE
        EX DE,HL
        POP HL                  ;HL->DEST,DE=REL ADDR. OF
        LD (HL),E               ;THE STRING
        INC HL
        LD (HL),D
        INC HL
        LD (HL),C
        INC HL
        LD (HL),B
        POP DE
        CP A            ;set NC
        RET
 
        END

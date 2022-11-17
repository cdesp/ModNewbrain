
        NAME MOD4
 
;Various compile and execute routines

;INTERNALLY DEFINED GLOBALS

        GLOBAL XIF
        GLOBAL CRESER
        GLOBAL CCONT
        GLOBAL XMERGF
        GLOBAL XVERFF
        GLOBAL CVERIF
        GLOBAL CMERGE
        GLOBAL CREPORT
        GLOBAL CFOR
        GLOBAL XSAVEH
        GLOBAL XSAVE1
        GLOBAL GFNAME
        GLOBAL XFOR1
        GLOBAL XFOR2
        GLOBAL CSAVE
        GLOBAL CNEXT
        GLOBAL XNEXT
        GLOBAL CREST
        GLOBAL CLIST
        GLOBAL CDELET
        GLOBAL CNON
        GLOBAL COPEN
        GLOBAL XOPEN
        GLOBAL CCLOSE
        GLOBAL XCLOSE
        GLOBAL CPUT
        GLOBAL CDIM
        GLOBAL XDIM
        GLOBAL CEND
        GLOBAL XEND
        GLOBAL CLOAD
        GLOBAL XLOADF
        GLOBAL XLOADH
        GLOBAL CRAND
        GLOBAL XRAND
        GLOBAL CSTOP
        GLOBAL CSTOP1
        GLOBAL XREM
        GLOBAL CRUN
        GLOBAL CRETN
        GLOBAL CNEW
        GLOBAL XNEW
        GLOBAL XSAVEF
        GLOBAL GSBAS
        GLOBAL LCL2
        GLOBAL XLDLP
        GLOBAL XDIMX
        GLOBAL CLET
        GLOBAL CINPUT
        GLOBAL CPOKE
        GLOBAL CLINP
        GLOBAL CREAD
        GLOBAL CGET
 
        EJECT
;EXTERNALLY DEFINED GLOBALS

        GLOBAL XRUN1D
        GLOBAL DELST2
        GLOBAL DEV2
        GLOBAL FACC
        GLOBAL LPSTR1
        GLOBAL ASSSTR
        GLOBAL VARIA9
        GLOBAL GUSTP
        GLOBAL LDUSTP
        GLOBAL GGSP
        GLOBAL LMN1
        GLOBAL CDEF8
        GLOBAL GFORP
        GLOBAL TYFN8
        GLOBAL XVERIF
        GLOBAL XMERGE
        GLOBAL UNST
        GLOBAL GTOP
        GLOBAL M6A
        GLOBAL DISPOSE

;W & Z codes

        GLOBAL WLPSTR1
        GLOBAL WCHKEQ
        GLOBAL ZUNST6
        GLOBAL ZFPZERO
        GLOBAL WSTREXP
        GLOBAL WBVARIA
        GLOBAL ZSTCIN
        GLOBAL WCHKCOM
        GLOBAL ZRANDOM
        GLOBAL WSCAN
        GLOBAL ZSETS2
        GLOBAL ZINITST
        GLOBAL ZMKSTAB
        GLOBAL ZCLOSE
        GLOBAL ZCPSH0
        GLOBAL ZOPNOUT
        GLOBAL ZOPENIN
        GLOBAL ZGFNAME
        GLOBAL WPTEOF
        GLOBAL ZXLISN
        GLOBAL WCLOSE
        GLOBAL ZRDSCH
        GLOBAL ZCHKCR
        GLOBAL WCHKCLS
        GLOBAL ZCDEVO
        GLOBAL ZINTIN
        GLOBAL WCLABEL
        GLOBAL WINTIN
        GLOBAL WCHKCR
        GLOBAL ZUNST2
        GLOBAL WADDSYM
        GLOBAL WCHKOPN
        GLOBAL ZCLOSE
        GLOBAL WCM2HL
        GLOBAL WNUMEXP
        GLOBAL WRDLINE
        GLOBAL WUNSTSS
        GLOBAL WXDIM
        GLOBAL ZADD
        GLOBAL ZAMENDL
        GLOBAL ZBVARIA
        GLOBAL ZCHKEQ
        GLOBAL ZCHKCOM
        GLOBAL ZCHKHSH
        GLOBAL ZCMONE
        GLOBAL ZCM2HL
        GLOBAL ZCMTHRE
        GLOBAL ZCOMP
        GLOBAL ZDLUSS
        GLOBAL ZEXPN
        GLOBAL WEXPN
        GLOBAL ZFPZERO
        GLOBAL ZINSS
        GLOBAL ZINUSS
        GLOBAL ZLDF
        GLOBAL ZNLNT
        GLOBAL ZNOTBC
        GLOBAL ZNUMASS
        GLOBAL ZNUMEXP
        GLOBAL ZSCAN
        GLOBAL ZSTINST
        GLOBAL ZSTF
        GLOBAL ZSTREXP
        GLOBAL ZUNSTSS
        GLOBAL ZVARIA
        GLOBAL ZXNEW
        GLOBAL ZCHECK
        GLOBAL ZRDNCH
        GLOBAL ZCDEVO
 
;Offsets from IY
 
OUTCON  EQU 6
INCON   EQU 7
FORPTR  EQU 14H
ARBAS   EQU 1AH
SBAS    EQU 1CH
LBAS    EQU 1EH
FORVAR  EQU 33H
BASFLG  EQU 2AH
FLAGS   EQU 22H
SOBAS   EQU 3DH
STOP    EQU 0EH

;Y codes
 
YASC    EQU 82H
YPUT    EQU 81H
YGET    EQU 86H
YREST   EQU 8AH
YLETS   EQU 0C8H
YLETN   EQU 0C9H
YNUMIN  EQU 0D5H
YSTRIN  EQU 0D6H
YENDIN  EQU 0EEH
YSEP    EQU 0D7H
YPOKE   EQU 80H+11
YBEGRD  EQU 80H+14
YENDRD  EQU 80H+15
YSEPRD  EQU 80H+16
YLINP   EQU 87H
YRESER  EQU 0A4H
YCONT   EQU 8DH
YVERIF  EQU 9CH
YMERGE  EQU 9AH
YREPORT EQU 0A3H
YFOR1   EQU 0D8H
YPUSH3  EQU 9FH
YFOR2   EQU 0D9H
YRAND   EQU 0DCH
YSAVEH  EQU 0DDH
YLIST   EQU 0C7H
YLISTN  EQU 0CDH
YDELET  EQU 09EH
YON     EQU 0C6H
YONSUB  EQU 0F4H
YERR    EQU 098H
YLOADH  EQU 0DFH
YOPEN   EQU 0E1H
YCLOSE  EQU 0E2H
YSTOP   EQU 0E3H
YEND    EQU 0E4H
YRUN    EQU 0C1H
YRET    EQU 0C4H
YSTICN  EQU 0CAH
YSTOCN  EQU 0CBH
YNEW    EQU 0E6H
YNEXT   EQU 0DBH
YDIM    EQU 0F5H
 
;T codes
 
TNEXT   EQU 80H+12
TTO     EQU 80H+54
TSTEP   EQU 80H+55
TIN#    EQU 80H+49
TMINUS  EQU 80H+2
TGOTO   EQU 80H+5
TERR    EQU 80H+58
TINVTH  EQU 80H+53      ;The ineffable invisible THEN
 
;CHARACTER CODE
 
EOF     EQU 4
 
        EJECT
CFOR:   EQU $
        DEFW ZBVARIA
        RET C
        LD A,B
        AND 0FCH
        CP 080H
        SCF
        LD A,44
        RET NZ
        LD (IY+FORVAR),C
        LD (IY+FORVAR+1),B      ;SAVE C.V.NAME TYPE
        DEFW ZCHKEQ
        DEFW WNUMEXP
        RET C
        LD A,YFOR1
        DEFW ZCMONE
        LD B,TTO
        DEFW ZCHECK
        LD A,46
        DEFW WNUMEXP
        RET C
        DEFW ZCHKCR
        JR NC,CFOR1     ;J. IF NO STEP
        LD B,TSTEP
        DEFW ZCHECK
        LD A,47
        DEFW WNUMEXP
        DEFW WCHKCR
        RET C
        JR CFOR2
 
CFOR1:  LD A,3          ;CLASS 0 OP CODE FOR "PUSH 1.0"
        DEFW ZCMONE
;NOW FIND 'NEXT' LINE
CFOR2:  LD A,YPUSH3
        PUSH IX
        POP HL
        PUSH HL
        DEFW ZCMTHRE
        LD E,1          ;level 1 after 1st FOR
CFOR6:  POP HL          ;now find corresp NEXT instr
CFOR3:  LD C,E
        DEFW ZNLNT
        LD A,48
        RET C
        PUSH HL                 ;SAVE L.N.T ENTRY ADRR.
        DEFW ZSTINST
        PUSH BC
        POP DE
        DEFW ZRDSCH
        SUB TNEXT
        JR Z CFOR4
        INC A
        JR NZ CFOR6
        INC E           ;add 2 to level if FOR
        INC E
CFOR4:  DEFW ZRDNCH
        DEFW ZVARIA
        POP HL
        LD A,45
        RET C
        LD A,C
        CP (IY+FORVAR)
        JR NZ,CFOR45
        LD A,B
        CP (IY+FORVAR+1)
        JR Z,CFOR8
CFOR45:         ;here if different ctl var
        DEC E           ;net +1 if FOR, -1 if NEXT
        JR NZ,CFOR3     ;unless is matching NEXT
CFOR7:  LD A,49         ;here if first with same ctl
        SCF             ;   var doesn't coincide with
        RET             ;   first "level := 0"
 
CFOR8:  DEC E           ;here if same
        JR NZ,CFOR7     ;unless is matching NEXT
        LD A,YPUSH3
        DEFW ZCMTHRE
        LD A,YFOR2
        DEFW ZCMONE
        RET
 
CNEXT:  EQU $
        DEFW ZVARIA
        RET C
        LD A,YNEXT
        JP CSTOP1
 
XFOR1:  PUSH HL
;MAKE INITIAL ASSIGNMENT TO C.V.
        DEFW ZNUMASS
        LD BC,2
        DEFW ZINUSS             ;RETRIEVE ADDR OF CTL VAR
        JP XRUN1D
 
XFOR2:  EQU $
;MAKE ANY PREVIOUS EXECUTION INACTIVE
        CALL GUSTP
        INC HL
        INC HL
        INC HL
        LD C,(HL)
        INC HL
        LD B,(HL)       ;BC=LNT addr of current FOR line
FORINA: EQU $
;HL=LOC(PREV. FOR ENTRY ON STACK)+3
;BC=CURRENT STATEMENT L.N.T. ENTRY ADDR.
        LD DE,20-3+2
        ADD HL,DE
        CALL XFOR3
        JR NC,FORIN2    ;J. IF STACK ALL CHECKED
        LD A,(HL)
        INC HL
        CP C
        JR NZ,FORINA
        LD A,(HL)
        CP B
        JR NZ,FORINA    ;J. IF NOT SAME FOR LOOP
;SAME LOOP FOUND - Make it inactive
        DEC HL
        DEC HL
        DEC HL
        DEC HL
        LD BC,20
        DEFW ZDLUSS
;      +
FORIN2: EQU $
;DETERMINE IF AN(OTHER) EXECUTION REQUIRED
        DEFW ZFPZERO
        CALL GUSTP
        LD (IY+FORPTR),L
        LD (IY+FORPTR+1),H
        LD BC,3
        ADD HL,BC
        PUSH HL         ;ADDR OF L.N.T. ENTRY FOR "FOR"
        ADD HL,BC
        PUSH HL
        EX DE,HL
        LD HL,FACC
        DEFW ZCOMP      ;compare step against zero
        RLA             ;shift sign of step into
        POP HL          ;   bit 0 of A
        LD DE,6
        ADD HL,DE
        PUSH HL         ;SAVE LOC(LIMIT)
        ADD HL,DE
        LD E,(HL)
        INC HL
        LD D,(HL)       ;DE=LOC(C.V.)
        POP HL          ;HL=LOC(LIMIT)
        RRA
        JR C,LFI3       ;IF STEP -VE
        EX DE,HL
LFI3:   DEFW ZCOMP
;CY SET IF CV>LIMIT (STEP +VE), LIMIT>CV (STEP -VE)
;CY SET IF LOOP EXHAUSTED
        POP DE
        JR NC,LFI5      ;IF REQ DO BODY OF LOOP
        LD BC,20
        CALL UNST
        LD (IY+FORPTR+1),H
        LD (IY+FORPTR),L
LFI5:   EX DE,HL
LFI6:   LD E,(HL)
        INC HL
        LD D,(HL)
        AND A           ;clear carry
        PUSH DE
        POP IX
        RET
 
 
XIF:    PUSH    HL      ;SAVE PTR.TO OBJ. CODE
        DEFW ZUNST6     ;DE-> RESULT OF COMPARISON
        PUSH    DE
        DEFW ZFPZERO    ;LOAD FACC WITH -1
        POP     DE
        DEFW ZCOMP      ;SET Z IF FALSE 
        POP HL
        JR Z LFI6       ;False - goto next line number
        INC HL
        INC HL
        SCF
        SBC A,A
        RET
 
 
 
 
XFOR3:          ;Set carry if HL < GSSPTR, clear else
                ;Preservesc ABCDEHL
        PUSH DE
        CALL GGSP
        AND A
        SBC HL,DE
        ADD HL,DE
        POP DE
        RET
 
XNEXT:  EQU $
        CALL GFORP
        PUSH IX
        POP BC
        LD A,41
FORST2: CALL XFOR3
        CCF
        RET C           ;if FOR block not found
        LD E,(HL)
        INC HL
        LD D,(HL)
        EX DE,HL
        OR A
        SBC HL,BC
        EX DE,HL
        JR Z,FORST3
        LD DE,20-1
        ADD HL,DE
        JR FORST2
 
FORST3: DEC HL
        CALL LDUSTP
        LD DE,6
        ADD HL,DE
        PUSH HL
        DEFW ZLDF       ;FPA=STEP
        POP HL
        LD DE,12
        ADD HL,DE
        LD E,(HL)
        INC HL
        LD D,(HL)       ;DE=LOC(C.V.)
        PUSH DE
        DEFW ZADD
        LD A,2          ;set error code to 2
        POP HL
        RET     C       ;R.IF ADD ERROR
        DEFW ZSTF       ;C.V. UPDATED
        JP FORIN2
 
CPUT:   DEFW ZCDEVO
CPUT1:  DEFW WEXPN
        RET C
        OR A
        JR Z CPUT2
        LD A,YASC
        DEFW ZCMONE
        RET C
CPUT2:  LD A,YPUT
        DEFW ZCMONE
        RET C
        DEFW ZCHKCOM
        JR NC CPUT1
        JR CDIM2
 
CGET:   DEFW ZCDEVO
CGET1:  DEFW WBVARIA
        LD L,YGET
        LD H,B
        DEFW WCM2HL
        RET C
        DEFW ZCHKCOM
        JR NC CGET1
        JR CDIM2
 
CDIM:   EQU $
        DEFW ZVARIA
        DEFW WCHKOPN
        DEFW WADDSYM
        PUSH HL         ;SAVE S.T.ADDR.
        DEFW WNUMEXP
        JR C,CDIM7      ;IF HAVE HAD AN ERROR
        DEFW ZCHKCOM
        LD H,80H
        JR C,CDIM1
        DEFW ZNUMEXP
        JR C CDIM7
        LD H,0C0H
CDIM1:  LD L,YDIM
        DEFW ZCM2HL
        POP HL
        DEFW WCM2HL
        DEFW WCHKCLS
        RET C
        DEFW ZCHKCOM
        JR NC,CDIM
CDIM2:  OR A
        JP CDEF8
 
CDIM7:  POP HL
        RET
 
XDIMX:  LD A,(HL)
        INC HL
        LD E,(HL)
        INC HL
        LD D,(HL)
        INC HL
        PUSH HL
        EX DE,HL
        INC HL
        LD E,(HL)
        CALL TYFN8
        JR C CDIM7
        LD C,(HL)
        DEC HL
        PUSH HL
        LD D,0
        LD A,(IY+BASFLG)
        AND 1
        LD E,A
        BIT 5,C
        JR Z,LXDX3
        DEFW ZUNSTSS
LXDX3:  PUSH DE         ;2nd dim.
        DEFW WUNSTSS
        LD B,D
        LD C,E
        POP DE
        POP HL
        DEFW WXDIM
        JP XRUN1D
 
XDIM:   EQU $
;ON ENTRY
;       HL=SYM.TAB.ADR.
;       BC=1ST DIM
;       DE=2ND DIM
;ON EXIT
;       CY SET IF NOT ROOM
;
        INC HL
        INC HL
        LD A,(HL)
        INC HL
        OR (HL)
        LD A,71
        SCF             ;ERROR IF STORAGE ALREADY
        RET NZ
        SET 7,(IY+BASFLG)       ;mustn't change base now
        BIT 0,(IY+BASFLG)
        JR NZ,DIM3
        INC DE
        INC BC
DIM3:   LD A,D
        OR E
        JR Z,DIM2
        LD A,B
        OR C
DIM2:   LD A,73
        SCF
        RET Z           ;dim 0 with base 1
        PUSH HL
        INC HL
        LD (HL),C
        INC HL
        LD (HL),B
        INC HL
        LD (HL),E
        INC HL
        LD (HL),D
        CALL LMN1       ;HL*DE -> HL
        JR C DIM6       ;if overflow
        LD A,20
        CP H
DIM6:   LD A,72
        JR C,DIM8       ;UNLESS DEFINITELY <32768 BYTES
        LD B,H          ;NOW MULTIPLY BY 6 TO GET
        LD C,L          ;   # BYTES
        CALL M6A        ;Now BC = amt store req'd
        LD E,ARBAS
        DEFW ZINSS
DIM8:   POP DE
        RET C
        PUSH DE
        PUSH HL
        LD (HL),0
        LD D,H
        LD E,L
        INC DE
        DEFW ZNOTBC
        LDIR            ;clear elements to zero
        CALL GSBAS
        EX DE,HL
        POP HL
        SBC HL,DE       ;NB CARRY STILL CLEAR
        EX DE,HL
        OR A
        POP HL
        LD (HL),D
        DEC HL
        LD (HL),E
XREM:   RET
 
GSBAS:  EQU $           ;Get SBAS into HL
        LD L,(IY+SBAS)
        LD H,(IY+SBAS+1)
        RET
 
 
XRAND:  EQU $
        DEFW ZRANDOM
        RET
 
 
GFNAME: DEFW ZUNST2
        PUSH HL
        CALL DELST2
        EX DE,HL
        POP BC
        LD DE,128
        LD A,(DEV2)
        RET
 
INFILE: DEFW ZGFNAME
        LD (IY+INCON),E
        DEFW ZOPENIN
        RET
 
XSAVEH: SET 2,(IY+FLAGS)        ;leave tokens in
        DEFW ZXLISN
        RES 2,(IY+FLAGS)
        DEFW WPTEOF
        RET
 
XMERGF: CALL INFILE
        CALL NC XMERGE
        JR XSAVE1
 
XVERFF: CALL INFILE
        CALL NC XVERIF
        JR XSAVE1
 
XSAVEF: DEFW ZGFNAME
        LD (IY+OUTCON),E
        DEFW ZOPNOUT
        CALL NC XSAVEH
XSAVE1: LD E,128
        JR NC XCLO1
        PUSH AF
        DEFW ZCLOSE
        POP AF
        RET
 
XCLOSE: EQU $
        DEFW ZUNSTSS
        LD A,0
        SBC A,D
        LD A,90         ;stream number error
        RET C
        XOR A
        SUB E
        CCF
        LD A,92         ;close stream zero error
XCLO1:  DEFW WCLOSE
        RET
 
XLOADF: CALL INFILE
        CALL NC XLOADH
        JR XSAVE1
 
XLOADH: DEFW ZXNEW
        OR A
XLDLP:  LD E,(IY+INCON)
        DEFW WRDLINE
        RET C
        DEFW ZRDSCH
        CP EOF
        CALL Z DISPOSE  ; preserves AF
        JR Z XEND
        CP 0DH
        JR Z XLDLP
        DEFW ZAMENDL
        JR XLDLP
 
XNEW:   EQU $
        CALL GTOP
        PUSH HL
        POP IX
        LD E,SBAS
        DEFW ZSETS2     ;Set SBAS and LBAS to (B4)
        DEFW ZINITST    ;Initialise upper store areas
                        ;and Object code
        LD E,SOBAS-LBAS
        ADD HL,DE
        LD E,STOP
        DEFW ZSETS2     ;Initialise STOP and OTOP
XEND:   SCF
        LD A,1
        RET
 
XOPEN:  LD B,(HL)       ;control byte
        DEFW ZUNST2     ;ST addr of FILE$
        EX DE,HL        ;so HL = USRSTP
        PUSH DE         ;save FILE$
        PUSH BC         ;save control byte
        LD C,(HL)
        INC HL
        LD B,(HL)
        INC HL          ;HLBC = filename
        CALL ASSSTR     ;assign it to FILE$
        POP BC
        LD DE,0
        BIT 5,B
        JR Z LXO1
        PUSH BC
        DEFW WUNSTSS
        POP BC
LXO1:   PUSH DE
        LD A,(DEV2)
        LD E,A
        LD D,0
        BIT 3,B
        PUSH BC
        JR Z LXO2
        DEFW WUNSTSS
LXO2:   PUSH DE
        DEFW WUNSTSS
        POP BC
        POP HL
        EX (SP),HL
        JR C LXO7
        LD A,B
        OR D
        OR H
        ADD A,255       ;set CY iff A~=0
        LD A,90
        JR C LXO7
        LD A,C
        LD D,L
LXO7:   POP BC
        POP HL
        RET C           ;if error via LXO7
        PUSH HL
        BIT 0,B
        PUSH AF
        PUSH DE
        DEFW ZMKSTAB
        POP DE
        XOR A
        SUB E           ;set CY iff E~=0
        DEFW WCLOSE
        POP AF
        JR Z LXO4
        DEFW ZOPNOUT
        DEFB 11H        ;code for LD DE,.. i.e. skip
LXO4:   DEFW ZOPENIN
        DEFW WLPSTR1
        POP DE
        RET C
        CALL GUSTP
        LD C,(HL)
        INC HL
        LD B,(HL)
        JP ASSSTR
 
 
CCLOSE: EQU $
        DEFW ZCHKHSH
        LD D,YCLOSE
        JR CLET1
 

CPOKE:  EQU $
        DEFW    ZNUMEXP
        DEFW    WCHKCOM
        LD      D,YPOKE
        JR CLET1

CRESER: LD D,YRESER     ;note carry unset from STATEL
        JR CLET1
 

 
CLET:   EQU $
        DEFW ZBVARIA
        DEFW WCHKEQ
        BIT 6,B
        JR NZ,STRLET    ;J. if string -let
        LD D,YLETN
CLET1:  PUSH DE
        DEFW WNUMEXP
        JR LCL6
        
STRLET: DEFW WSTREXP
        RET C
        LD A,YLETS
        DEFB 21H        ;op-code for LD HL,nn-i.e. skip
CRAND:  LD A,YRAND
        DEFB 21H
CRUN:   LD A,YRUN
        DEFB 21H
CEND:   LD A,YEND
        DEFB 21H
CREPORT: LD A,YREPORT
        DEFB 21H
CCONT:  LD A,YCONT
        DEFB 21H
CRETN:  LD A,YRET
        DEFB 21H
CNEW:   LD A,YNEW
        DEFB 21H
CSTOP:  LD A,YSTOP
CSTOP1: DEFW ZCMONE
        DEFW WCHKCR
        RET
 
CLINP:  DEFW ZSTCIN
        DEFW WBVARIA
        RET C
        BIT 6,B
        LD A,96
        SCF
        RET Z
        LD A,YLINP
        JR CSTOP1
 
CINPUT: DEFW ZSTCIN
        LD HL,YSEP+(YENDIN.SHL.8)
        JR CRE3
 
CREAD   EQU $
        LD A,YBEGRD
        LD HL,YSEPRD+(YENDRD.SHL.8)
CRE2:   DEFW ZCMONE
CRE3:   PUSH HL
        DEFW WBVARIA
        JR C CRE4
        BIT 6,B
        LD A,YNUMIN
        JR Z CRE1
        LD A,YSTRIN
CRE1:   DEFW ZCMONE
CRE4:   POP HL
        RET C
        DEFW ZCHKCOM
        LD A,L
        JR NC CRE2
        LD A,H
        JR CSTOP1
 
 
LCL5:   DEFW ZSTREXP
LCL6:   POP DE
        RET C
        LD A,D
        JR CSTOP1
 
CMERGE: LD HL,YSTICN+(YMERGE.SHL.8)
        JR LCL2
 
CVERIF:         ;compile code for VERIFY
        LD HL,YSTICN+(YVERIF.SHL.8)
        JR LCL2
 
CSAVE:  LD HL,YSTOCN+(YSAVEH.SHL.8)
        JR LCL2
 
CLOAD:  LD HL,YSTICN+(YLOADH.SHL.8)
LCL2:   DEFW ZCHKHSH
        JR C,LCL4               ;if no #
        PUSH HL
        DEFW ZNUMEXP
        POP HL
        JR CLOAD8
 
LCL4:   INC H   ;H=next code i.e. Y***F
        PUSH HL
        DEFW ZCHKCR
        JR C LCL5
        DEFW ZCPSH0
        JR LCL6
 
 
KWGOTO: DEFB TGOTO,4
        DEFW YON,YON,YONSUB
        DEFB YONSUB
 
CNON:   LD B,TERR
        DEFW ZCHECK
        JR NC CNL1
        INC B
        DEFW ZCHECK
        JR C CON
CNL1:   PUSH BC
        LD B,TINVTH
        DEFW ZCHECK
        LD HL,KWGOTO
        DEFW WSCAN
        LD A,93
        DEFW WINTIN
        POP BC
        RET C
        LD A,B
        SUB TERR-YERR
        JR CREST8
 
CON:    EQU $
        DEFW ZNUMEXP
        RET C
        LD B,TINVTH
        DEFW ZCHECK
        LD HL,KWGOTO
        DEFW WSCAN
        LD A,34
        RET C
        LD A,C
        DEFW ZCMONE
CON1:   DEFW WCLABEL
        RET C
        DEFW ZCHKCOM
        JR NC,CON1
        XOR A
        LD H,A
        LD L,A
CLOAD8: DEFW WCM2HL
        DEFW WCHKCR
        RET
 
CREST:  EQU $
;COMPILE CODE FOR RESTORE COMMAND
        DEFW    ZCHKCR
        LD HL,0
        JR NC CREST2
        DEFW ZINTIN
        RET C
CREST2: LD A,YREST
CREST8: DEFW ZCMTHRE
        DEFW WCHKCR
        RET

CLIST:  EQU $
        DEFW ZCDEVO
        RET C
        DEFW ZCHKCR
        LD A,YLIST
        JR C,CLIS1A     ;IF LINE NUMBERS
        LD A,YLISTN
        DEFW ZCMONE
        RET
 
CDELET: LD A,YDELET
CLIS1A: DEFW ZCMONE
        RET C
        DEFW ZINTIN
        JR NC CLI3
        LD HL,0
CLI3:   LD E,L
        LD D,H
        DEFW ZRDSCH
        CP 0DH
        JR Z CLIST1
        CP TMINUS
        LD A,35
        SCF
        RET NZ
        DEFW ZRDNCH
        DEFW ZINTIN
        JR NC CLIST1
        LD HL,-1
CLIST1: DEFW ZCM2HL
        EX DE,HL
CLIST8: JR CLOAD8
 
COPEN:  EQU $
        LD HL,FMTAB
        DEFW ZSCAN
        LD A,14
        PUSH BC
        DEFW WNUMEXP            ;stream number
        POP HL
        RET C
        DEFW ZCHKCOM
        JR C,LCO1       ;no parameters
        PUSH HL
        DEFW ZEXPN      ;device number or filename
        POP HL
        RET C
        ADD A,H
        LD H,A
        BIT 4,H
        JR NZ LCO1      ;filename only
        SET 3,H         ;device found
        DEFW ZCHKCOM
        JR C LCO1       ;only device found
        PUSH HL
        DEFW ZEXPN
        POP HL
        RET C
        ADD A,H
        LD H,A
        BIT 4,H
        JR NZ LCO1
        SET 5,H         ;port found (yum yum)
        DEFW ZCHKCOM
        JR C LCO1       ;no filename
        PUSH HL
        DEFW ZSTREXP
        POP HL
        SET 4,H
        RET C
LCO1:   BIT 4,H
        JR NZ LCO2
        PUSH HL
        DEFW ZCPSH0
        POP HL
        RET C
LCO2:   LD L,YOPEN
        PUSH HL
        LD BC,8000H+4000H+1023
        CALL VARIA9     ;compile stack ST addr of FILE$
        POP HL
        JR CLIST8
 
FMTAB:  EQU $
        DEFB TIN#,3
        DEFW 0,256,0
 
        END

#
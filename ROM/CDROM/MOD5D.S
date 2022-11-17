
        NAME MOD5D
 
;Various Compile, Execute and Utility routines
 
;INTERNALLY DEFINED GLOBALS
 
        GLOBAL XRESER
        GLOBAL CCLEAR
        GLOBAL XERR
        GLOBAL XBRK
        GLOBAL XVERIF
        GLOBAL XMERGE
        GLOBAL GGSP
        GLOBAL GFORP
        GLOBAL TETOLN
        GLOBAL LNTOTE
        GLOBAL TTLN
        GLOBAL LNTT
        GLOBAL XDELET
        GLOBAL SLNTT
        GLOBAL LAST
        GLOBAL TSTPPF
        GLOBAL UPDPPF
        GLOBAL CHKSTB
 
;EXTERNALS
 
        GLOBAL LAM1
        GLOBAL EXGLB
        GLOBAL AMEND8
        GLOBAL CSTOP1
        GLOBAL MKADJ
        GLOBAL XLDLP
        GLOBAL LINPTR
        GLOBAL GSBAS
        GLOBAL GLBAS
        GLOBAL DISPOSE
        GLOBAL XRUN1D
 
;W and Z codes
 
        GLOBAL ZELNUM
        GLOBAL ZPTMSG
        GLOBAL WINSS
        GLOBAL ZPUSH2
        GLOBAL ZLAM1
        GLOBAL ZTSTPPF
        GLOBAL ZUPDPPF
        GLOBAL ZLLN2
        GLOBAL ZNFIND
        GLOBAL ZADDSYM
        GLOBAL ZCHKCR
        GLOBAL ZVARIA
        GLOBAL ZFINDV
        GLOBAL ZDELT
        GLOBAL ZCMONE
        GLOBAL ZCHKOPN
        GLOBAL ZFIND
        GLOBAL WFIND
        GLOBAL ZFLNT
        GLOBAL ZLAST
        GLOBAL ZLANG
        GLOBAL ZMKNULL
        GLOBAL ZINITOB
        GLOBAL ZNLNT
        GLOBAL ZINTIN
        GLOBAL WINPUT
        GLOBAL ZRDSCH
        GLOBAL ZRDLIN
        GLOBAL WCHKCOM
        GLOBAL WCHKCLS
        GLOBAL WCHKCR
        GLOBAL WNUMEXP
        GLOBAL WEXPN
        GLOBAL WBVARIA
        GLOBAL ZCHKHSH
        GLOBAL ZPOP
        GLOBAL ZDLSTAB
        GLOBAL ZUNSTSS
        GLOBAL WUNSTSS
        GLOBAL ZTETOLN
        GLOBAL ZLNTT
        GLOBAL ZTTLN
        GLOBAL ZLNTOTE
        GLOBAL ZRDNCH
        GLOBAL ZRDCH
        GLOBAL WLNTOT
        GLOBAL ZOUTPUT
        GLOBAL ZXCLE2
 
;Offsets from IY
 
INCON   EQU 07H
OUTCON  EQU 06H
ARBAS   EQU 1AH
SBAS    EQU 1CH
STBAS   EQU 18H
FORPTR  EQU 14H
TOP     EQU 20H
GSSPTR  EQU 16H
FLAGS   EQU 22H
BRKLIN  EQU 3BH         ;'ON BRK' line no
ERRLIN  EQU 39H                 ;'ON ERR' line #
PPFLGS  EQU 2EH
 
;Y codes
 
YASC    EQU 82H
YCLEA2  EQU 91H
YCLEAR  EQU 95H
 
;T codes
 
TTHEN   EQU 80H+52      ;token numbers
TREM    EQU 80H+14
TDATA   EQU 80H+29
TINVTH  EQU 80H+53
 
;CHARACTER CODE
 
EOF     EQU 4
 
;ERROR NUMBERS
 
NCLERR  EQU 88
VERERR  EQU 91          ;verify error
 
        EJECT
XERR:   LD A,(HL)
        LD (IY+ERRLIN),A
        INC HL
        LD A,(HL)
        LD (IY+ERRLIN+1),A
        RET
 
XBRK:   LD A,(HL)
        LD (IY+BRKLIN),A
        INC HL
        LD A,(HL)
        LD (IY+BRKLIN+1),A
        RES 1,(IY+FLAGS)
        RET
 
XVERIF:         ;execute code for VERIFY
        DEFW ZFLNT
LXV2:   PUSH HL
        PUSH AF
LXV5:   LD E,(IY+INCON) ;here at start of line
        DEFW ZRDLINE
        JP C AMEND8
        CALL DISPOSE
        DEFW ZRDSCH
        CP 0DH
        JR Z LXV5
        POP AF
        JR C LXV1
        DEFW ZINTIN
        JR C LXV8
        POP DE          ;LNT ptr
        PUSH DE
        EX DE,HL
        LD C,(HL)
        INC HL
        LD B,(HL)       ;BC = line number in source
        EX DE,HL        ;HL = line number from instream
        OR A            ;DE->2nd byte of LNT entry
        SBC HL,BC
        JR NZ LXV8
        EX DE,HL
LXV9:   INC HL
        LD A,(HL)       ;HL->source ptr in LNT entry
        INC HL
        LD H,(HL)
        LD L,A          ;HL-> source
LXV91:  LD A,(HL)       ;advance HL to pt at first
        INC HL          ;non-space char.
        CP ' '
        JR Z LXV91
        DEC HL
        LD (IY+PPFLGS),0
        DEFW ZRDSCH     ;advance instream to first
LXV3:   DEFW ZTSTPPF    ;non-space char.
        JR C LXV0
        CP ':'
        JR Z LXV7
LXV0:   DEFW ZUPDPPF
        CP (HL)
        JR NZ LXV8
        CP 0DH
        JR Z LXV4
        INC HL
        DEFW ZRDNCH
        JR LXV3
 
LXV4:   POP HL          ;here if CR in instream
        DEFW ZNLNT
        JR LXV2
 
LXV7:   LD A,(HL)       ;here if colon in instream
        CP 0DH
        JR NZ LXV8
        POP HL
        DEFW ZLAST
        JR NZ LXVE
        PUSH HL         ;LNT ptr
        DEFW ZRDNCH     ;advance past the colon
        INC HL
        JR LXV9
 
LXV1:   DEFW ZRDCH
        CP EOF
LXV8:   POP HL
LXVE:   LD A,VERERR
        SCF
        RET NZ          ;Note jump to LXVE above
        DEFW ZPTMSG
        DEFB 9
        DEFM 'VERIFIED'
        DEFB 13
        RET
 
TSTPPF: LD C,(IY+PPFLGS)
        LD B,A
        LD A,C
        AND 7
        JR Z TSTPL1
        SCF
TSTPL1: BIT 3,C
        LD A,B
        RET
 
UPDPPF: CP 22H
        LD C,(IY+PPFLGS)
        JR NZ UPDPL1
        LD A,1
        XOR C
        LD C,A
        LD A,22H
        JR UPDPL2
UPDPL1: BIT 3,C
        JR Z UPDPL3
        CP TTHEN
        JR Z UPDPL6
        CP TINVTH
        JR NZ UPDPL2
UPDPL6: RES 3,C
        JR UPDPL2
UPDPL3: CP TREM
        JR Z UPDPL4
        CP TDATA
        JR NZ UPDPL5
UPDPL4: SET 1,C
        JR UPDPL2
UPDPL5: CP ' '
        JR Z UPDPL2
        SET 3,C
UPDPL2: LD (IY+PPFLGS),C
        RET
        
CCLEAR: ;compile routine for CLEAR command
        ;if params clear these variables only
        DEFW ZCHKCR
        LD A,YCLEAR
        JP NC CSTOP1    ;if no params
        DEFW ZINITOB
        DEFW ZTETOLN
        PUSH IX
LCCL:   DEFW ZVARIA
        JR C,LCCL21
        DEFW ZFINDVC    ;here if param
        LD A,NCLERR     ;BC=name/type,HL->ST entry
        JR C,LCCL21
        PUSH HL
        INC HL
        INC HL          ;HL->ls byte of value
        LD A,030H
        AND B
        JR NZ,LCCL3     ;jp.if array
        BIT 6,B
        JR Z LCCL2      ;if not string
        DEFW ZMKNULL
LCCL2:  POP HL          ;here if var/const
        DEFW ZDLSTAB
        DEFW WCHKCOM
        JR NC,LCCL
        DEFW ZCHKCR
        JR C,LCCL21
        CALL GSBAS      ;code to delete constants
CCL1:   CALL EXGLB
        OR A
        SBC HL,DE
        JR Z LCCL8
        PUSH DE
        EX DE,HL
        INC HL
        BIT 7,(HL)
        JR NZ CCL2
        BIT 6,(HL)
        JR Z CCL3
        INC HL
        DEFW ZMKNULL
CCL3:   POP HL
        PUSH HL
        DEFW ZDLSTAB
CCL2:   POP HL
        LD BC,8
        ADD HL,BC
        JR CCL1
 
LCCL8:  LD A,YCLEA2
        DEFW ZCMONE
LCCL21: POP IX
SLNTT:  PUSH AF         ;LNTOTE saving AF
        DEFW ZLNTOTE
        POP AF
        RET
 
LCCL3:  DEFW ZCHKOPN    ;here if array
        DEFW WCHKCLS
        JR C,LCCL31
        LD E,(HL)
        INC HL
        LD D,(HL)       ;DE=rel addr.in array area
        LD A,D
        OR E
        JR Z LCCL2
        PUSH DE
        PUSH BC         ;save name/type
        LD B,(IY+SBAS+1)
        LD C,(IY+SBAS)
        EX DE,HL
        ADD HL,BC       ;HL=abs addr.in array area
        PUSH HL
        EX DE,HL
        INC HL
        DEFW ZELNUM     ;calc no of els
                        ;HL=# of els.
        POP DE
        EX (SP),HL      ;HL=name/type,(SP)=# els
        BIT 6,H
        JR NZ,LCCL32
LCCL30: POP HL          ;# of els
        PUSH DE
        LD D,H          ;here if not str.array
        LD E,L
        ADD HL,HL
        ADD HL,DE
        ADD HL,HL       ;HL=# bytes to move from array
        LD B,H
        LD C,L
        POP HL
        LD E,ARBAS
        DEFW ZDELT
        POP DE
        CALL ARNULL
        JR LCCL2
LCCL31: POP HL
        JR LCCL21
 
 
LCCL32:         ;here if string array
                ; (SP)=# els
                ; DE=abs addr.of array
        POP BC
        PUSH BC
        PUSH DE
        EX DE,HL
LCCL35: PUSH BC
        PUSH HL
        DEFW ZMKNULL
        POP HL
        POP BC
        DEC BC
        POP DE
        LD A,B
        OR C
        JR Z,LCCL30
        PUSH DE
        LD DE,6
        ADD HL,DE
        JR LCCL35
 
ARNULL:         ;delete array elements
                ;enter HL=abs addr.of ls byte of 1st el.
                ; BC=# bytes deleted
                ; DE=rel addr.of del'd array 
                ;now to move ST pointers
        PUSH DE
        CALL GSBAS
LAR1:   CALL EXGLB
        EX DE,HL
        AND A
        SBC HL,DE
        ADD HL,DE
        POP DE
        RET Z
        INC HL
        LD A,(HL)
        AND 030H
        JR Z,LAR3
        LD A,(HL)
        AND 0CH
        JR NZ LAR3
        PUSH HL
        INC HL
        PUSH HL
        POP IX
        CALL MKADJ
        POP HL
LAR3:   PUSH DE
        LD DE,7
        ADD HL,DE
        JR LAR1
 
GGSP:   LD D,(IY+GSSPTR+1)
        LD E,(IY+GSSPTR)
        RET
 
 
TETOLN: CALL GFORP
TET1:   OR A
        CALL GGSP
        SBC HL,DE
        JR Z TET2
        ADD HL,DE
        DEFW ZTTLN
        DEFW ZTTLN
        LD BC,12
        ADD HL,BC
        PUSH HL
        LD A,(HL)
        INC HL
        LD H,(HL)
        LD L,A
        DEC HL
        LD D,(HL)
        DEC HL
        LD E,(HL)
        POP HL
        LD (HL),E
        INC HL
        LD (HL),D
        INC HL
        JR TET1
 
TET2:   EX DE,HL
TET3:   CALL CHKSTB
        RET Z
        ADD HL,DE
        DEFW ZTTLN
        JR TET3
 
CHKSTB: LD D,(IY+STBAS+1)       ;load string base to DE
        LD E,(IY+STBAS)         ;and subtract from HL
        OR A
        SBC HL,DE
        RET
 
LNTOTE: CALL GFORP
LNT1:   OR A
        CALL GGSP
        SBC HL,DE
        JR Z LNT2
        ADD HL,DE
        DEFW ZLNTT
        DEFW ZLNTT
        LD BC,12
        ADD HL,BC
        PUSH HL
        LD C,(HL)
        INC HL
        LD B,(HL)
        DEFW ZADDSYM
        INC HL
        INC HL
        EX DE,HL
        POP HL
        JP P LNT4
        LD BC,-8
        ADD HL,BC
LNT4:   LD (HL),E
        INC HL
        LD (HL),D
        INC HL
        JR LNT1
 
LNT2:   EX DE,HL
LNT3:   CALL CHKSTB
        RET Z
        ADD HL,DE
        DEFW ZLNTT
        JR LNT3
 
GFORP:  LD H,(IY+FORPTR+1)
        LD L,(IY+FORPTR)
        RET
 
TTLN:   PUSH HL
        LD A,(HL)
        INC HL
        LD H,(HL)
        LD L,A
        LD E,(HL)
        INC HL
        LD D,(HL)
        DEC HL
        PUSH DE
        DEFW ZLANG
        POP HL
        PUSH HL
        PUSH BC
        DEFW ZNFIND
        DEFW ZLANG
        LD A,B
        POP BC
        SUB B
        POP DE
TTLN1:  POP HL
        LD (HL),E
        INC HL
        LD (HL),D
        INC HL
        LD (HL),A
        INC HL
        RET
 
LNTT:   PUSH HL
        LD E,(HL)
        INC HL
        LD D,(HL)
        INC HL
        LD B,(HL)
        EX DE,HL
        PUSH BC
        DEFW ZNFIND
        POP BC
        JR C LNTT2
        XOR A
        OR B
        JR Z LNTT2
LNTT3:  DEFW ZLAST
        DJNZ LNTT3
LNTT2:  EX DE,HL
        JR TTLN1
 
XRESER: CALL XTRA
        DEFW ZUNSTSS
        LD B,D
        LD C,E
        LD E,TOP
        DEFW WINSS
        JR XDLT2
 
XMERGE: CALL XTRA
        CALL XLDLP
        CP 1
        JR Z XDLT2
        SCF
        JR XDLT2
 
XDELET: CALL XTRA
        PUSH HL
        LD HL,CARRET
        CALL LINPTR
        POP HL
        LD E,(HL)
        INC HL
        LD D,(HL)
        INC HL
        PUSH HL
        EX DE,HL
        DEFW ZFIND
        JR C XDLT1
        DEFW ZLANG
XDLT1:  EX (SP),HL
        LD E,(HL)
        INC HL
        LD D,(HL)
        EX DE,HL
        DEFW ZFIND
XDLT4:  POP DE
        OR A
        SBC HL,DE
        JR NC XDLT2
        ADD HL,DE
        PUSH DE
        PUSH HL
        DEFW ZLANG
        EX (SP),HL
        LD A,(HL)
        INC HL
        LD H,(HL)
        LD L,A
        DEFW ZLAM1
        POP HL
        JR XDLT4
 
XDLT2:  PUSH AF
        DEFW ZLNTOTE
        LD HL,2
        ADD HL,SP
        DEFW ZLNTT
        POP BC
        POP IX
        POP DE
        JR NC XDLT3
        PUSH IX
        POP HL
        DEFW ZLLN2
        LD A,254
        JR NC XDLT9
        LD B,3
XDLT8:  LD DE,-6
        ADD IX,DE
XDLT7:  SCF
        LD A,B
        RET
 
XDLT9:  SCF
XDLT3:  BIT 0,C
        RET Z
        JR NC XDLT7
        JR XDLT8
 
CARRET: DEFB 13
        
XTRA:   POP BC
        EX DE,HL
        PUSH HL
        PUSH IX
        LD HL,0
        ADD HL,SP
        PUSH BC
        PUSH DE
        DEFW ZTTLN
        DEFW ZTETOLN
        DEFW ZXCLE2
        POP HL
        RET
 
LAST:   PUSH HL
        DEFW ZNLNT
        POP DE
        EX DE,HL
        SBC A,A
        RET NZ
        LD A,(DE)
        CP (HL)
        RET NZ
        INC HL
        INC DE
        LD A,(DE)
        CP (HL)
        DEC HL 
        RET NZ
        DEC DE
        EX DE,HL
        RET
 
        END

#
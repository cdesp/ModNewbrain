
        NAME MOD7
 
;Symbols, Lines and Line Numbers
 
;INTERNALLY DEFINED GLOBALS

        GLOBAL DISPOSE
        GLOBAL GTOP
        GLOBAL CODFTK
        GLOBAL CODSTK
        GLOBAL SCAN
        GLOBAL FINDVC
        GLOBAL ADDSYM
        GLOBAL GETVNM
        GLOBAL CHROOM
        GLOBAL NCROOM
        GLOBAL INITOB
        GLOBAL INSERT
        GLOBAL INITST
        GLOBAL AMENDL
        GLOBAL INTIN
        GLOBAL PT#HL
        GLOBAL NCONST
        GLOBAL ROOMQ
        GLOBAL LOPTR
        GLOBAL LOPTR1
        GLOBAL AMEND8
        GLOBAL XCLEA2
        GLOBAL M6A
        GLOBAL FIND
        GLOBAL NFIND
        GLOBAL FLNT
        GLOBAL NLNT
        GLOBAL LANG
        GLOBAL SSFNM
        GLOBAL ASFNM
        GLOBAL LLN2
        GLOBAL LAM1

;EXTERNALS
 
        GLOBAL ASFLST
        GLOBAL SSFLST
        GLOBAL FACC
        GLOBAL KWDS
        GLOBAL OKWDS
        GLOBAL GOTOP
        GLOBAL GLBAS
        GLOBAL GSBAS
        GLOBAL GOPTR
        GLOBAL EXGLB
        GLOBAL GUSTP
        GLOBAL GINPTR
        GLOBAL LINPTR
        GLOBAL GFORP
        GLOBAL LDUSTP
 
;W and Z codes

        GLOBAL ZMKAB
        GLOBAL ZMKRL
        GLOBAL ZINSS
        GLOBAL ZUPDLNT
        GLOBAL ZTSTPPF
        GLOBAL ZUPDPPF
        GLOBAL WCODFTK
        GLOBAL ZCODSTK
        GLOBAL ZINSERT
        GLOBAL ZFINDVC
        GLOBAL WDIGIT
        GLOBAL ZDIGIT
        GLOBAL ZFLNT
        GLOBAL ZINITOB
        GLOBAL ZINP
        GLOBAL ZINTIN
        GLOBAL ZLAST
        GLOBAL ZLANG
        GLOBAL WLANG
        GLOBAL ZNFIND
        GLOBAL ZLLN2
        GLOBAL ZNLNT
        GLOBAL ZPTCHAR
        GLOBAL ZRDNCH
        GLOBAL ZRDCH
        GLOBAL ZRDFN
        GLOBAL ZRDSCH
        GLOBAL ZROOMQ
        GLOBAL ZRSINST
        GLOBAL ZSETS2
        GLOBAL ZSETSN
        GLOBAL ZSVINST
        GLOBAL WSTF
        GLOBAL ZCOMP
        GLOBAL ZTTCAPS
        GLOBAL ZDELT
 
;Offsets from IY

TOP     EQU 20H
STOP    EQU 0EH
OTOP    EQU 10H
FLAGS   EQU 22H
USRSTP  EQU 12H
NCONNO  EQU 23H
BASFLG  EQU 2AH
OMOVE   EQU 02CH
DTINPT  EQU 2FH
ERRLIN  EQU 39H
PPFLGS  EQU 2EH
LBAS    EQU 1EH
SBAS    EQU 1CH
 
;External error number
 
RDFNE1  EQU 80          ;"FN" not found
 
;T code
 
TINVTH  EQU 80H+53      ;the amazing invisible THEN
 
;OTHER EQUATES
 
SAFETY: EQU 20          ;safety margin when CHROOMing
SAFMRG: EQU 3+SAFETY    ;room for minimum obj code
                        ;plus a reasonable amount
 
        EJECT
;VARIABLES
 
INITST: EQU $
                ;Initialise symbol table and all areas
                ;below it.
                ;sets HL = IY+LBAS+2, D = 0
        LD HL,0
        LD E,DTINPT
        DEFW ZSETS2
        LD H,B
        LD L,C
        LD E,ERRLIN
        DEFW ZSETS2
        LD  (IY+BASFLG),B
        LD (IY+FLAGS),B
        DEFW ZINITOB    ;obj code has pointers into s.t.
        CALL GLBAS
        LD E,USRSTP
        LD A,6
        DEFW ZSETSN
        RET
 
 
FINDVC: EQU $
                ;Find symbol table entry corresp.
                ;to name-type BC. (If searching for
                ;array set both array bits.)
                ;On Exit CY set if not found, Else
                ;NC AND P SET
                ;BC=NAME&TYPE FROM SYM TAB
                ;HL=LOC(SYM.TAB.ENTRY)
        CALL GSBAS
FINDV1: CALL EXGLB      ;DE->start of Sym. Tab.
        SCF
        SBC HL,DE
        RET C
        LD A,(DE)
        INC  DE
        CP C
        JR NZ,FINDV3    ;J. IF LS BYTE NOT MATCHED
        LD A,(DE)
        AND 030H
        LD A,(DE)
        JR Z,FINDV2     ;J.IF NOT ARRAY ENTRY
        OR 030H         ;SET BOTH ARRAY BITS
FINDV2: CP B
        JR Z,FINDV4     ;J.IF FOUND
FINDV3: LD HL,7
        ADD HL,DE       ;HL SET FOR NEXT ENTRY
        JR FINDV1
 
FINDV4: EX DE,HL
        LD B,(HL)
        DEC HL          ;HL=LOC(SYM.TAB.ENTRY)
        RET
 
 
ADDSYM: EQU $
                ;Add entry to symbol table for 
                ;name-type BC, if not alredy present.
                ;Returns HL = addr of entry, CY set
                ;if no room, M set if new entry.
                ;Preserves BC.
        DEFW ZFINDVC
        RET     NC              ;IF ALREADY EXISTS
INSERT: EQU     $
                ;Add new entry to symbol table for
                ;name-type BC. Returns HL = addr of
                ;entry, M set to show new entry
                ;and value of entry zero, or CY set
                ;if no room. Preserves BC.
        PUSH    BC
        LD      BC,8
        LD E,SBAS
        DEFW ZINSS
        POP     BC
        RET     C
        PUSH HL
        LD (HL),C
        INC     HL
        LD      (HL),B
        LD A,5
LINS3:  INC HL
        LD (HL),0       ; CLEAR ALL 6 VALUE BYTES
        DEC A
        JP P,LINS3
        POP HL
        RET             ; WITH NC (SINCE "RET C") AND M
 
 
 
GETVNM: EQU $
                ;Get standard BASIC name-type to BC
                ;(including string mark). CY set if
                ;syntax error. Preserves DE.
        DEFW ZRDSCH
        CALL LETTER
        RET C
        LD B,0
        PUSH BC
        CALL M36
        POP BC
        ADD HL,BC
        DEFW ZDIGIT
        JR      C,GETVN2
        ADD A,26
        LD      C,A
        DEFW ZRDNCH
        JR GETVN1
GETVN2: DEFW ZRDCH
        CALL LETTER
        JR NC GETVN1
        LD C,36
GETVN1: LD      B,0
        ADD HL,BC
        LD B,H
        LD C,L
        SET     7,B
        DEFW ZRDCH
        XOR     '$'
        RET     NZ
        SET     6,B
        DEFW    ZRDNCH
        RET
 
LETTER: DEFW ZTTCAPS
        SUB     41H
        RET     C               ;IF BELOW 'A'
        CP      26
        CCF
        RET     C               ;IF ABOVE 'Z'
        LD C,A
        DEFW ZRDNCH
        RET
 
M36:    CALL M6
M6:     LD H,B
        LD L,C
M6A:    EQU $   ;Multiply 16 bit unsigned no. by 6.
                ;Argument req'd in HL and BC !!!!!
                ;Overflow ignored. Answer ret'd in
                ;HL and BC. Preserves ADE.
        ADD HL,HL
        ADD HL,BC
        ADD HL,HL
        LD B,H
        LD C,L
        RET
        EJECT
 
SSFNM:  EQU $
                ;Scan for String Standard Fn. Name
        DEFW ZRDFN      ;Is it a string user fn.
        JR C LSTR1
        BIT 6,B         ;make sure it is a string
        RET NZ
        DEFW ZRSINST    ;reset input from
                        ;scan in RDFN
        SCF
        RET
 
LSTR1:  CP RDFNE1
        SCF
        RET NZ
        LD HL,SSFLST
        JR SCAN
 
ASFNM:  EQU $
                ;Scan for Arith. Stnd. Fn. Name
                ;  take account of user DEF'd fns
        DEFW    ZRDFN           ;LOOK FOR 'FN'<NUM.VAR.>
        RET NC                  ;RET IF NO ERRORS
        CP      RDFNE1
        SCF             ;J.IF NO ROOM/INVALID FN FORMAT
        RET NZ
        LD HL,ASFLST
;       +
SCAN:   DEFW ZSVINST
        DEFW ZRDSCH
        SUB (HL)
        RET C
        INC HL
        CP (HL)
        CCF
        RET C
        INC HL
        EX DE,HL
        LD H,0
        LD L,A
        ADD HL,HL
        ADD HL,DE
        LD C,(HL)
        INC HL
        LD B,(HL)
        DEFW ZRDNCH
        RET
        
 
 
NCONST: EQU     $
        CALL GINPTR
        EX DE,HL
        DEFW ZINP
        RET C           ;if not arith. const.
        EX DE,HL
        CALL LINPTR
        CALL GSBAS      ;Scan S.T.for num.const.in FPA
LOOKU1: CALL EXGLB
        SCF
        SBC     HL,DE
        EX      DE,HL
        JR      C,NCONS1
        INC     HL
        LD      A,(HL)
        INC     HL
        PUSH HL
        AND     0C0H
        JR      NZ,LOOKU4
        LD      DE,FACC
        DEFW ZCOMP
LOOKU4: POP HL
        RET Z           ;with carry unset
        LD DE,6
        ADD     HL,DE
        JR      LOOKU1
 
 
NCONS1: LD BC,0         ;name-type for numeric const.
        DEFW    ZINSERT
        INC     HL
        INC     HL
        DEFW WSTF       ;HL preserved
        RET
 
        EJECT
*PROGRAM LINE MODULE
 
AMENDL: EQU $   ;Amend program line
                ;Call with instream set to new line
                ;(first char digit,last CR).
                ;Returns CY set if no room.
                ;Object code should be deleted by
                ;calling program.
        DEFW ZINTIN     ;Read line no. to HL
        RET C           ;If line no. > 65535
        LD A,H
        OR L
        SCF
        LD A,94
        RET Z           ;If line number = 0
LAM1:           ;As AMENDL but enter with HL = line no.
                ;and instream at first char of source.
        PUSH HL
        DEFW ZNFIND
        JR C,AMEND1     ;J.IF NO OLD LINE
        PUSH HL         ;Else delete old line
        INC     HL
        INC     HL
        LD      E,(HL)
        INC     HL
        LD      D,(HL)
        EX      DE,HL
        LD      A,15Q
        PUSH    HL
        LD      BC,0
        CPIR            ;SET BC = -(LINE LENGTH)
        DEC HL          ;SET HL -> CR AT END OF LINE
        CALL EXGLB
LAM3:   INC     HL      ;   LNT ENTRY
        INC     HL      ;MOVE LNT ENTRIES FOR LINES ABOVE
        DEFW ZUPDLNT
        INC HL
        INC HL
        PUSH DE
        DEFW ZLLN2
        POP DE
        JR      NC,LAM3
        INC     DE
        LD      H,(IY+STOP+1)
        LD      L,(IY+STOP)
        AND     A
        SBC     HL,DE
        LD      B,H
        LD      C,L     ;BC:=AMOUNT TO MOVE
        EX      DE,HL
        POP     DE
        JR Z,LAM5       ;IF NOTHING TO MOVE
        LDIR            ;DELETE SOURCE LINE
LAM5:   EX DE,HL
        LD E,STOP
        DEFW ZSETS2
        DEFW ZMKRL
        POP     HL
        LD      BC,6
        LD E,LBAS
        DEFW ZDELT
        DEFW ZMKAB
        POP HL
        JR LAM1
AMEND1: EQU     $
;INSERT NEW LINE(IF NOT EMPTY)
        LD (IY+PPFLGS),0
        DEFW ZRDCH
        CP      15Q
        JP      Z,AMEND9        ;IF NEW LINE EMPTY
        POP BC
        LD H,(IY+STOP+1)
        LD L,(IY+STOP)
        PUSH HL
        PUSH BC
        LD BC,SAFMRG+7
AMEND2: PUSH BC
        EX (SP),HL
        DEFW ZROOMQ
        POP HL
        JR C,AMEND8             ; IF OUT OF SPACE
        INC BC
        PUSH BC
        DEFW ZRDCH
        DEFW ZTSTPPF
        JR C AMEND7
        BIT 4,(IY+PPFLGS)
        JR NZ AMENDE
        PUSH HL
        DEFW ZTSTPPF    ;will leave NC
        JR Z AMENDD
        DEFW ZCODSTK
        CCF
AMENDA: DEFW WCODFTK
        JR C AMENDB
        DEFW ZTSTPPF
        JR Z AMENDB
        DEFW ZRSINST
        POP HL
        LD A,TINVTH     ;the remorseless invisible THEN
        JR AMENDC
 
AMENDD: BIT 7,A
        JR Z AMENDA
        SET 4,(IY+PPFLGS)
AMENDB: POP HL
AMENDE: CP ':'
        JR Z AMEND4
AMEND7: LD B,A
        DEFW ZRDNCH
        LD A,B
AMENDC: DEFW ZUPDPPF
        POP BC
        LD      (HL),A
        INC     HL
        CP      15Q
        JR      Z,AMEND3
        JR      AMEND2
 
;HL=NEW TOP OF SOURCE
AMEND4: LD (HL),0DH
        INC HL
        POP BC
        DEFW ZRDNCH
AMEND3: LD E,STOP
        DEFW ZSETS2
        DEFW ZMKRL
        POP     HL
        PUSH    HL
        DEFW ZNFIND
        DEFW WLANG
        LD      BC,-6
        LD E,LBAS
        DEFW ZDELT              ;MUST BE ROOM BECAUSE
        POP BC                  ;CHECKED ABOVE
        POP DE
        PUSH BC
        PUSH HL
        LD (HL),C
        INC HL
        LD (HL),B
        INC HL
        LD (HL),E
        INC HL
        LD (HL),D               ;SOURCE ADDR.
        INC HL
        XOR A
        LD (HL),A
        INC HL
        LD (HL),A               ;OBJ. ADDR.
        DEFW ZMKAB
        POP HL
        JP AMEND1
 
AMEND8: POP HL
AMEND9: POP HL
DISPOSE: CALL GFORP
        RES 6,(IY+FLAGS)
        JP LDUSTP
 
CODFTK: LD HL,KWDS
        JR CODTOK
 
CODSTK: LD HL,OKWDS
CODTOK: LD C,(HL)
        INC HL
        LD B,(HL)
        INC HL
        DEFW ZSVINST    ;important for invisible THEN
        PUSH DE
        EX DE,HL
        CALL GINPTR
        EX DE,HL
CODL1:  PUSH DE
CODL8:  LD A,(DE)
        BIT 7,A
        JR NZ CODL3
        CP 'a'
        JR C CODL9
        CP 'z'+1
        JR NC CODL9
        ADD A,'A'-'a'
CODL9:  XOR (HL)
        AND 7FH
        JR Z CODL2
CODL3:  BIT 7,(HL)
        INC HL
        JR Z CODL3
        EX (SP),HL
        CALL LINPTR
        EX (SP),HL
        POP DE
        INC C
        DJNZ CODL1
        DEFW ZRDCH
        SCF
        POP DE
        RET
 
CODL2:  BIT 7,(HL)
        INC HL
        LD A,C
        JR NZ CODL4
        INC DE
        JR CODL8
 
CODL4:  EX DE,HL
        CALL LINPTR
        POP DE
        POP DE
        RET
 
 
 
XCLEA2:         ;A common entry pt to INITOB
        RES 0,(IY+FLAGS)
INITOB:         ;DELETE OBJ CODE, EXCEPT FOR CURRENT LINE
        LD A,(IY+FLAGS)
        LD D,(IY+STOP+1)
        LD E,(IY+STOP)
        RRCA
        JR NC LI4       ;if no obj. code to preserve
        PUSH DE         ;save STOP ptr
        PUSH AF         ;to fix Basil's bug
        CALL GOPTR      ;obj. ptr for cur. line to HL
        POP AF
        DEC HL
        OR A            ;clear CY
        LD B,(HL)
        DEC HL          ;HL -> obj. record for cur. line
        LD C,(HL)       ;BC = obj. record size
        BIT 2,A         ;test bit 3 as shifted
        JR NZ LI2       ;if compiling
        EX DE,HL        ;so HL->STOP,DE->obj. record
        SBC HL,DE       ;HL= -dist. obj. is to be moved
        LD (IY+OMOVE+1),H
        LD (IY+OMOVE),L
        ADD HL,DE       ;so HL-> STOP
        EX DE,HL        ;so HL->obj. record,DE->STOP
        JR LI3
 
LI2:            ;Here if compiling - so record extends
                ;from (HL) to OTOP
        PUSH HL         ;save obj. record ptr
        LD B,H
        LD C,L
        CALL GOTOP
        SBC HL,BC       ;so HL = length of record
        EX (SP),HL      ;so HL->obj. rec.,TOS = length
        POP BC          ;BC = length
        JR Z LI4        ;if length = 0
LI3:    LDIR            ;copy record to new position
LI4:    LD (IY+OTOP+1),D
        LD (IY+OTOP),E
        CALL GLBAS      ;now adjust line no. tab. entries
        DEFW ZLLN2
        JR C LI6        ;if there aren't any
LI5:    DEFW ZNLNT
        DEC HL
        LD (HL),0
        DEC HL          ;clear obj. ptr in prev. entry
        LD (HL),0
        INC HL
        INC HL
        JR NC LI5
LI6:    RLCA            ;here when all LNT entries clear
        RET NC          ;if no current line
        POP HL          ;else rewrite LNT entry for
LOPTR:  INC HL          ;cur. line (object now at STOP)
        INC HL
LOPTR1: LD (IX+5),H
        LD (IX+4),L
        RET
 
 
 
INTIN   EQU $
                ;READ INTEGER FROM INSTREAM TO HL
                ;BC,DE PRESERVED
        LD HL,0
        DEFW ZRDSCH
        DEFW WDIGIT
        JR C,RDLNN6     ;J.IF 1st CHAR.NOT DIGIT
        PUSH BC
RDLNN1: LD C,L
        LD B,H          ;BC=ACCUMULATED VALUE
        LD L,A
        LD H,0          ;HL=VALUE OF NEW DIGIT
        LD A,10
RDLNN2: ADD HL,BC
        JR C,RDLNN3     ;J.IF INTEGER OVERFLOW
        DEC A
        JR NZ,RDLNN2
        DEFW ZRDNCH     ;HL=NEW ACCUMULATED VALUE
        DEFW ZDIGIT
        JR NC,RDLNN1
        CCF
RDLNN3: POP BC
RDLNN6  LD A,4          ;IN CASE ERROR
        RET
 
 
 
CHROOM: EQU $
                ;Check room for HL bytes
                ;If insufficient initalise object
                ;to make more space then check
                ;again. Returns CY set if still
                ;not enough. Preserves BC.
        LD DE,SAFETY
        ADD HL,DE
NCROOM: CALL ROOMQ
        RET     NC
        PUSH    HL
        PUSH    BC
        DEFW ZINITOB
        POP     BC
        POP     HL
ROOMQ:  EQU     $
                ;CHECK ROOM FOR HL BYTES
                ;CY SET IF NOT, BCHL PRESERVED
        PUSH    HL
        CALL GOTOP
        EX DE,HL
        CALL GUSTP
        OR      A
        SBC     HL,DE
        POP     DE
        SBC     HL,DE
        EX      DE,HL
        LD A,10                 ; ERROR CODE IF CY SET
        RET
 
 
 
PT#HL:  EQU $   ;PRINT UNSIGNED 2-BYTE NUMBER FROM HL
        LD BC,256*4+0
                ;Note - could enter here with B=4 and
                ;C = char. to replace leading zeroes
                ;or C = 0 to suppress them.
LPH2:   PUSH HL
        LD A,B
        LD HL,1
LPH3:   ADD HL,HL
        LD D,H
        LD E,L
        ADD HL,HL
        ADD HL,HL
        ADD HL,DE       ;NB CLEARS CARRY
        DEC A
        JR NZ,LPH3
        EX DE,HL        ;DE := 10**B
        POP HL
LPH4:   INC A           ;DIVIDE BY DE, QUOTIENT IN A
        SBC HL,DE
        JR NC,LPH4
        ADD HL,DE       ;RESTORE AFTER ONE SUBTR'N TOO
        DEC A           ;   MANY
        JR Z,LPH5       ;IF ZERO DIGIT
        LD C,'0'        ;CHAR FOR ANY FUTURE ZEROES ELSE
LPH5:   ADD A,C
        JR Z,LPH6
        DEFW ZPTCHAR
        RET C
LPH6    DJNZ LPH2
        LD A,L
        ADD A,'0'
        DEFW ZPTCHAR
        RET

;
;LINE-NUMBER MODULE
;
FLNT:           ;SET HL -> FIRST LNT ENTRY
                ;SETS CY AND NZ IF TABLE IS EMPTY
                ;PRESERVES ABC
        CALL GLBAS
        PUSH BC
        DEFW ZLANG
        POP BC
        JR LLN2
 
LANG:   LD B,0
LANG1:  INC B
        DEFW ZLLN2
        RET C
        DEFW ZLAST
        JR Z LANG1
NLNT:           ;SET HL -> LNT ENTRY
                ; FOLLOWING ENTRY POINTED TO BY HL ON
                ; ENTRY.
                ;SETS CY AND NZ IFF NO MORE ENTRIES
                ;PRESERVES ABC
        LD DE,6
        ADD HL,DE
LLN2:   EX DE,HL
        CALL GTOP
        SCF
        SBC HL,DE       ;SET CY IF ADDR. GE (B4)
        EX DE,HL
        RET
 
GTOP:   LD H,(IY+TOP+1)
        LD L,(IY+TOP)
        RET
 
NFIND:  PUSH HL
        CALL GLBAS
        DEFW ZLLN2
        JR LF1
 
FIND:   EQU $
;FIND LNT ENTRY
;ON ENTRY
;       HL=REQD LINE #
;ON EXIT
;       IF FOUND HL->LNT ENTRY,CY CLEAR
;       IF > ALL LINE #'S IN TABLE, HL=(B4), CY SET
;       ELSE HL->LNT ENTRY FOR NEXT LARGEST#,CY SET
;SETS BC=RQD LINE # ON ALL EXITS
        PUSH HL
        DEFW ZFLNT
LF1:    POP BC
LF2:    RET C           ;IF OFF END OF TABLE
        LD E,(HL)       ;ELSE CY IS CLEAR
        INC HL
        LD D,(HL)
        DEC HL
        EX DE,HL
        SBC HL,BC
        EX DE,HL
        RET Z           ;IF FOUND
        CCF
        RET C           ;IF GONE PAST WHERE IT WOULD BE
        DEFW ZNLNT      ;ELSE TRY NEXT
        JR LF2
 
;END OF LINE NUMBER MODULE
 
        END

#
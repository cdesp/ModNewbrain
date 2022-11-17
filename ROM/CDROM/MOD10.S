
        NAME    MOD10

;Utilities
 
;INTERNALLY DEFINED GLOBALS

        GLOBAL EXGLB
        GLOBAL MKRL
        GLOBAL MKAB
        GLOBAL XSTOP
        GLOBAL MKSPC
        GLOBAL RETSPC
        GLOBAL UPDLNT
        GLOBAL NBKTST
        GLOBAL CHECK
        GLOBAL LINPTR
        GLOBAL UPDE
        GLOBAL UP@HL
        GLOBAL CHKCLS
        GLOBAL CHKOPN
        GLOBAL DELT
        GLOBAL DLUSS
        GLOBAL DLSTAB
        GLOBAL RDNCH
        GLOBAL RDCH
        GLOBAL RSINST
        GLOBAL CHKCOM
        GLOBAL CHKQUO
        GLOBAL NOTBC
        GLOBAL PTEOF
        GLOBAL PTCHAR
        GLOBAL PTMSG
        GLOBAL STINST
        GLOBAL RDLINE
        GLOBAL RDLIN2
        GLOBAL CHKCR
        GLOBAL DIGIT
        GLOBAL INUSS
        GLOBAL RDNSCH
        GLOBAL CHKHSH
        GLOBAL CHKEQ
        GLOBAL PTNL
        GLOBAL RDSCH
        GLOBAL SVINST
        GLOBAL NEGBC
        GLOBAL GINPTR
        GLOBAL GLBAS
        GLOBAL GUSTP
        GLOBAL GOTOP
        GLOBAL INSS

        EJECT
 
;EXTERNALLY DEFINED GLOBALS

        GLOBAL LDUSTP
        GLOBAL CHROOM
 
;W & Z codes
 
        GLOBAL ZLLN2
        GLOBAL ZUPDLNT
        GLOBAL ZBRKTST
        GLOBAL WNBKTST
        GLOBAL ZCHROOM
        GLOBAL ZNCROOM
        GLOBAL WINPUT
        GLOBAL ZOUTPUT
        GLOBAL ZPTCHAR
        GLOBAL WPTCHAR
        GLOBAL ZRDNCH
        GLOBAL ZRDSCH
 
;Offsets from IY
 
INCON   EQU 7
OUTCON  EQU 6
INPTR   EQU 9
SVINPT  EQU 0CH
USRSTP  EQU 12H
SBAS    EQU 1CH
LBAS    EQU 1EH
ERRNO   EQU 8
FLAGS   EQU 22H
STOP    EQU 0EH
OTOP    EQU 10H
 
;T codes
 
TEQ     EQU 80H+12
THASH   EQU 80H+51
 
        EJECT
 
*INPUT-STREAM MODULE
*
*RDCH
*READ CURRENT CHAR. TO A
*FLAGS AND OTHER REGS PRESERVED
*
*RDNCH
*READ NEXT CHARACTER(IF NOT AT END OF LINE)
*ON EXIT
*      A=CHAR(CR IF AT END OF LINE)
*       CY CLEAR , OTHER REGS PRESERVED
*
*SVINST
*SAVE CURRENT INSTREAM STATE
*      ALL REGS PRESERVED(ONLY 1 SAVE CAN BE ACTIVE)
*
*RSINST
*RESTORE INSTREAM STATE (FOLLOWING SVINST)
*      ALL REGISTERS PRESERVED
*
*RDSCH
*READ CHAR.IGNORING SPACES & LINE FEEDS
*ON EXIT
*      A=CHAR.
*       CARRY CLEAR, OTHER REGS PRESERVED
*
*RDNSCH
*READ NEXT SIG. CHAR. TO A (IF NOT AT END OF LINE)
*CLEARS CARRY, OTHER REGS PRESERVED
*
*STINST
*SET UP TO READ SOURCE LINE
*ON ENTRY HL->LNT ENTRY
*ON EXIT HL AND INSTREAM->START OF SOURCE LINE
*BCDE AND CY PRESERVED
*
*RDLINE
*READS LINE OF INPUT FROM STREAM (E)
*ONTO USER-STACK UPSIDE DOWN
*UNTIL CR FOUND OR RUN OUT OF STORE WHICH ISNT NICE
*AND THEN TURNS THE LINE THE RIGHT WAY UP  
*FUN ISNT IT ?
*PRESERVES DE
*ALL OTHER REGS DESTROYED
*CARRY SET IF BREAK-IN OR ERROR
*INSTREAM SET TO POINT TO START OF LINE
*
*MKRL IF TOP>INPTR>=USRSTP THEN INPTR -:= USRSTP
*    AND SET 6,FLAGS
*MKAB IF 6,FLAGS THEN INPTR +:= USRSTP AND RES 6,FLAGS
 
        EJECT
 
STINST: EQU $           ;set up to read source line
        INC HL          ;HL -> LNT entry
        INC HL
        LD A,(HL)
        INC HL
        LD H,(HL)
        LD L,A
LINPTR:                 ;Load instream pter from HL
                        ;All regs preserved
        LD (IY+INPTR),L
        LD (IY+INPTR+1),H
        RET
 
GINPTR:                 ;Converse of LINPTR
        LD L,(IY+INPTR)
        LD H,(IY+INPTR+1)
        RET

MKRL:   BIT 6,(IY+FLAGS)
        RET Z
        PUSH HL
        CALL GUSTP
        EX DE,HL
        CALL GINPTR
        OR A
        SBC HL,DE
        JR RSL1
 
MKAB:   BIT 6,(IY+FLAGS)
        RET Z
        PUSH HL
        CALL GINPTR
        EX DE,HL
        CALL GUSTP
        ADD HL,DE
        JR RSL1
 
RDLINE: EQU     $
        LD (IY+INCON),E
        CALL GUSTP
        LD B,H
        LD C,L
RDLIN1: PUSH HL
        PUSH DE
        LD HL,1         ;or whatever safe margin 
        DEFW ZNCROOM
        POP DE
        DEFW WNBKTST    ;check for break
        DEFW WINPUT     ;input char if OK
        POP HL
        RET C           ;ret if error of any kind
        DEC HL          ;HL is now where char is to go
        CALL LDUSTP
        LD (HL),A
        XOR 15Q
        JR NZ RDLIN1    ;if not CR loop
        CALL LINPTR     ;set up INPTR
        SET 6,(IY+FLAGS)
RDLIN2: DEC BC
        OR A
        SBC HL,BC       ;finished?
        RET NC
        ADD HL,BC       ;restore HL
        LD A,(BC)
        LD D,(HL)       ;do swap
        LD (HL),A
        LD A,D
        LD (BC),A
        INC HL
        JR RDLIN2       ;loop round
 
SVINST: EQU     $
        PUSH HL
        CALL GINPTR
        LD (IY+SVINPT),L
        LD (IY+SVINPT+1),H
        POP HL
        RET

RSINST: EQU     $
        PUSH HL
        LD L,(IY+SVINPT)
        LD H,(IY+SVINPT+1)
RSL1:   CALL LINPTR
        POP HL
        RET

RDCH:   EQU $
        PUSH HL
        CALL GINPTR
        LD A,(HL)
        POP HL
        RET
 
RDNCH:  EQU $
        PUSH HL
        CALL GINPTR
        LD A,(HL)
        CP 15Q
        JR Z RDNC1
        INC HL
        CALL LINPTR
        LD A,(HL)
        OR A
RDNC1:  POP HL
        RET
 
RDSCH:  CALL RDCH
        JR RDNSC1
 
RDNSCH: CALL RDNCH
RDNSC1: CP ' '
        JR Z,RDNSCH     ;ignore space
        CP 12Q
        JR Z,RDNSCH     ;ignore line feed
        AND     A
        RET

**END OF INPUT-STREAM MODULE

        EJECT
*OUTPUT-STREAM MODULE
*
*PTCHAR:ADD CHARACTER IN A TO THE OUTPUT STREAM
*       PRESERVES BCDEHL
*
*PTMSG: Select default output device and write text
*       SR.RET.ADR.=LOC(TEXT)
*       TEXT IN FORM:  1 byte = n, n chars
*       preserves CDE
*       returns to next location after text
*
*PTNL:  ADD NEW LINE TO OUTPUT STREAM
*       PRESERVES BCDEHL
*
*PTEOF: ADD END OF FILE MARK TO OUTPUT STREAM
*       PRESERVES BCDEHL
 
 
PTMSG:  XOR A
        LD (IY+OUTCON),A
        POP     HL
        LD      B,(HL)
        INC     HL
TEXOT1: JR C TEXOT2
        LD      A,(HL)
        DEFW WPTCHAR
TEXOT2: INC     HL
        DJNZ    TEXOT1
        JP (HL)
 
 
PTEOF   EQU $   ;Print EOF CR LF
        LD A,4
        DEFW ZPTCHAR
        RET C
PTNL    EQU $   ;Print CR 
        LD A,15Q
PTCHAR: EQU     $
        PUSH DE
        LD E,(IY+OUTCON)
        DEFW ZOUTPUT
        POP DE
        RET
 
**END OF OUTPUT-STREAM MODULE
 
        EJECT
*THINGY CLASS MODULE
*
*NBKTST:New BRKTST to only do the test if the 'suppress
*       interrupts' flag is unset. Loads interrupt code
*       into ERRNO.
*
*UPDLNT:Adds BC to (HL) if IY<=(HL)<=OTOP. HL := HL+2.
*
*MKSPC and RETSPC are the required routines for NB O/S.
*
NBKTST: OR A
        BIT 1,(IY+FLAGS)
        RET NZ
        DEFW ZBRKTST
        RET NC
        LD (IY+ERRNO),A
XSTOP:  XOR A
        SCF
        RET
 
UPDLNT: PUSH HL
        PUSH DE
        LD E,(HL)
        INC HL
        LD D,(HL)
        CALL GOTOP
        OR A
        SBC HL,DE
        EX DE,HL
        POP DE
        JR C UPDL8
        SBC HL,DE
        JR C UPDL8
        ADD HL,DE
        ADD HL,BC
        EX DE,HL
        EX (SP),HL
        LD (HL),E
        INC HL
        LD (HL),D
        INC HL
        POP DE
        RET
 
UPDL8:  POP HL
        INC HL
        INC HL
        RET
 
MKSPC:  PUSH HL
        PUSH DE
        PUSH BC
        LD H,B
        LD L,C
        DEFW ZCHROOM
        JR C MKL1
        PUSH BC
        CALL GSBRAM
        DEC HL
        EX DE,HL
        POP HL
        PUSH HL
        ADD HL,DE
        EX DE,HL
        LDDR
MKL2:   POP BC
        PUSH IY
        ADD IY,BC
        CALL GLBAS
MKL3:   DEFW ZLLN2
        POP DE
        JR C MKL4
        INC HL
        INC HL
        DEFW ZUPDLNT
        DEFW ZUPDLNT
        PUSH DE
        JR MKL3
MKL4:   LD HL,STOP
        ADD HL,DE
        ADD HL,BC
        DEFW ZUPDLNT
        DEFW ZUPDLNT
        OR A
MKL1:   POP BC
        POP DE
        POP HL
        RET
 
RETSPC: PUSH HL
        PUSH DE
        PUSH BC
        CALL NEGBC
        PUSH BC
        CALL GSBRAM
        POP HL
        PUSH HL
        ADD HL,DE
        EX DE,HL
        LDIR
        JR MKL2
 
GSBRAM: CALL GOTOP
        PUSH IY
        POP DE
        PUSH HL
        OR A
        SBC HL,DE
        EX (SP),HL
        POP BC
        RET
        EJECT
 
*CHARACTER CLASS MODULE
*
*DIGIT: Convert ASCII code to the digit value it
*       represents. Enter with code in A.
*       Preserves BCDEHL
*       On exit carry set and A destroyed if code
*       is not a digit, otherwise carry clear and
*       A contains digit value.
*
*CHKXX: Checks input stream for indicatated character
*       If char. checks calls RDNCH and returns with
*       carry clear and A = next character. Otherwise
*       returns with carry set and A=error number.
*       Preserves BCDEHL.
 
DIGIT:  EQU     $
        SUB     030H
        RET C
        CP      10
        CCF
        RET

CHECK:  DEFW ZRDSCH     ;Chris's bit
        CP B
        JR CHKL1
 
CHKCR:  EQU     $
        PUSH BC
        LD BC,0DH*256+53
        JR CHK
 
CHKCLS: EQU     $
        PUSH    BC
        LD      BC,')'*256+57
        JR      CHK-$

CHKCOM: EQU     $
        PUSH    BC
        LD      BC,02CH*256+52
        JR      CHK-$

CHKEQ:  PUSH    BC
        LD      BC,TEQ.SHL.8+55
        JR      CHK

CHKHSH: EQU     $
        PUSH    BC
        LD      BC,THASH.SHL.8+54
        JR      CHK-$

CHKQUO: EQU     $
        PUSH    BC
        LD      BC,022H*256+50
        JR      CHK-$

CHKOPN: EQU     $
        PUSH    BC
        LD      BC,'('*256+56
;      +
CHK:    EQU     $
        DEFW ZRDSCH
        CP      B
        LD      A,C
        POP     BC
CHKL1:  SCF
        RET     NZ
        DEFW ZRDNCH
        RET
 
**END OF CHARACTER CLASS MODULE
 
        EJECT
*WORKING STORE MANIPULATIONS
*
*DLXXX: Delete BC bytes from indicated upper store
*       area at and above address HL.
*       If BC<0 then make room for -BC bytes below
*       HL. On exit either Carry set if inserting
*       and no room, or Carry clear, BC preserved,
*       HL=HL+BC, DE= new USRSTP, A destroyed.
*
*INSXX: Insert BC bytes at start (i.e. base) of upper
*       store area. On exit either carry set if no
*       room, or carry clear, BC negated, HL= new
*       base pointer, DE= new user stack pter, A
*       destroyed.
*
*GXXX:  Get indicated store ptr into HL.
*       Preserves flags and registers
*       GOLN and GSBAS are at appropriate 
*       places in other modules
*
*LDXXX: Load indicated store pter from HL.
*       Preserves flags and registers.
*       LDOLN and LDUSTP are at appropriate
*       places in other modules.
 
EXGLB:  EX DE,HL
GLBAS:  LD L,(IY+LBAS)
        LD H,(IY+LBAS+1)
        RET
 
GUSTP:  LD L,(IY+USRSTP)
        LD H,(IY+USRSTP+1)
        RET
 
GOTOP:  LD H,(IY+OTOP+1)
        LD L,(IY+OTOP)
        RET
 
DLSTAB:         ;delete 8 bytes from symbol
                ;table
        LD BC,8
        LD E,SBAS
        JR DELT
 
INUSS:          ;Insert at start of user stack
                ;On exit HL=DE=new USSPTR
        LD H,B
        LD L,C
        CALL CHROOM
        RET C
        CALL GUSTP
        CALL NEGBC
        ADD HL,BC
        CALL LDUSTP
        LD D,H
        LD E,L
        OR A
        RET
 
INSS:   CALL GETS
        JR DELT
 
DLUSS:          ;"delete" for user stack
        LD E,USRSTP
;       +
DELT:   EQU     $
                ;DELETE 'BC' BYTES FROM UPPER STORE 
                ;AREA AT ADDRESS HL
                ;AND MOVE PTRS USRSTP TO E INCLUSIVE
                ;IF BC<0 THEN MAKE ROOM FOR -BC BYTES
                ;ON EXIT CY SET IF INSERTING & NO ROOM
                ;OTHERWISE CY CLEAR, BC PRESERVED
                ;HL=HL+BC, DE= NEW USER STACK PTER
        PUSH    DE
        PUSH    HL
        LD      A,B
        RLA
        JR      NC,LDEL3        ;IF DELETING
        XOR     A               ;ELSE CHECK ENOUGH ROOM
        LD      H,A
        LD      L,A
        SBC     HL,BC
        DEFW ZCHROOM
        POP     HL
        JR      C,LDEL8 
        PUSH    HL
LDEL3:  LD      D,(IY+USRSTP+1) ;HERE WITH CARRY CLEAR IF
        LD      E,(IY+USRSTP)   ;  OK TO PROCEED
        SBC     HL,DE
        JR Z,LDEL52             ;IF NOTHING TO MOVE
        PUSH BC                 ;SAVE BC
        PUSH BC
        EX (SP),HL              ;SWAP BC AND HL
        POP BC
        BIT 7,H
        JR      NZ,LDEL4
        EX      DE,HL           ;HERE IF DELETING
        ADD     HL,BC           ;COPY FROM TOP DOWNWARDS
        DEC     HL
        EX      DE,HL
        ADD     HL,DE
        EX      DE,HL
        LDDR
        JR      LDEL5
 
LDEL4:  ADD     HL,DE           ;HERE IF INSERTING
        EX      DE,HL
        LDIR
LDEL5:  POP     BC
LDEL52: POP     HL
        ADD     HL,BC
        POP     DE
        PUSH    IY      ;NOW ADD BC TO EACH POINTER
        EX      (SP),HL
        LD      D,0
        ADD     HL,DE   ;HL->1ST PTR TO MOVE
        LD      A,E
        SUB     USRSTP-2        ;NB. CLEARS CY
        RRA                     ;A:=#PTRS TO MOVE
LDEL6:  CALL UP@HL
        DEC     HL
        DEC     HL
        SUB     1               ;CLEARS CY
        JR      NZ,LDEL6
LDEL8:  POP     HL
        RET
 
 
GETS:           ;HL:=(IY+E);BC:=-BC
                ;PRESERVES E, SETS D=0
        PUSH    IY
        POP     HL
        LD      D,0
        ADD     HL,DE
        LD      A,(HL)
        INC     HL
        LD      H,(HL)
        LD      L,A
NEGBC:  EQU $           ; NEGATE BC
        DEC BC
NOTBC:  EQU $           ; 1'S COMPLEMENT BC
        LD      A,C
        CPL
        LD      C,A
        LD      A,B
        CPL
        LD      B,A
        RET
 
UP@HL           ;Add BC to location pointed to by HL
                ;Exit with DE=new value at location
                ;ABCHL preserved
        LD E,(HL)
        INC HL
        LD D,(HL)
UPDE:   EX DE,HL        ;Add BC to DE and store
        ADD HL,BC       ;result at (HL),(HL-1)
        EX DE,HL
        LD (HL),D
        DEC HL
        LD (HL),E
        RET
 
        END

#
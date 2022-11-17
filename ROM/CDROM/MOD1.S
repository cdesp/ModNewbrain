        NAME MOD1
 
;BASIC main program
;Atomic compile routines
;Break-in and Error routines
;Run-time interpreter
 
;INTERNALLY DEFINED GLOBALS

        GLOBAL XRUN1C
        GLOBAL XRUN1D
        GLOBAL BASIC
        GLOBAL COPT
        GLOBAL CREM
        GLOBAL CMONE
        GLOBAL CM2HL
        GLOBAL CMTHRE
        GLOBAL CPSH0
        GLOBAL CPSH
        GLOBAL COMMAN
        GLOBAL STATEL
        GLOBAL XAR1AD
        GLOBAL EXEC
        GLOBAL LXR21
        GLOBAL XAR2AD
        GLOBAL CMOBJ
        GLOBAL XCL3
        GLOBAL REPORT
        GLOBAL GOPTR

;EXTERNALLY DEFINED GLOBALS
 
        GLOBAL KWDLIS   ;Tables
        GLOBAL XTABLE
        GLOBAL XTABL2
        GLOBAL CLET     ;Routines
        GLOBAL GTOP
        GLOBAL GOTOP
        GLOBAL GUSTP
        GLOBAL GLBAS
        GLOBAL CHROOM
        GLOBAL LDUSTP
        GLOBAL SLNTT
        GLOBAL ZXCLE2
        GLOBAL LXG1
        GLOBAL AR1ADR
        GLOBAL LOPTR
        GLOBAL LOPTR1
        GLOBAL AR2ADR
        GLOBAL DISPOSE
        GLOBAL B3PRM    ;Sysytem Variables
        GLOBAL B4
        GLOBAL DEV0
        GLOBAL DPSP
        GLOBAL DPSL
        GLOBAL DEFNF
 
;W & Z codes
 
        GLOBAL ZCLOSE
        GLOBAL WPT#HL
        GLOBAL ZTTLN
        GLOBAL ZLLN2
        GLOBAL ZFIND
        GLOBAL ZMATHS
        GLOBAL ZCOMMAN
        GLOBAL ZXCL3
        GLOBAL ZCMOBJ
        GLOBAL ZREPORT
        GLOBAL ZPTCHAR
        GLOBAL ZPTNL
        GLOBAL ZPTMSG
        GLOBAL ZNLNT
        GLOBAL ZRDSCH
        GLOBAL ZDIGIT
        GLOBAL ZXNEW
        GLOBAL ZSTINST
        GLOBAL ZCMONE
        GLOBAL ZCM2HL
        GLOBAL ZNCROOM
        GLOBAL WLSTATE
        GLOBAL ZAMENDL
        GLOBAL ZSCAN
        GLOBAL ZNBKTST
        GLOBAL ZRDLINE
        GLOBAL WEXEC
        GLOBAL ZTETOLN
        GLOBAL ZLANG
        GLOBAL ZLAST
        GLOBAL ZCHKCR
        GLOBAL ZLNTOTE
        GLOBAL ZLAM1
        GLOBAL ZOPNOUT
        GLOBAL WREPORT
        GLOBAL ZCHECK
        GLOBAL ZINTIN
 
;Offsets from IY
 
OUTCON  EQU 6   ;put in 14/11/80
ERRNO   EQU 8   ;remember the ERRNO
TOP     EQU 20H
FLAGS   EQU 22H
OTOP    EQU 10H
CLINNO  EQU 27H
BASFLG  EQU 2AH
OMOVE   EQU 2CH
BRKLIN  EQU 3BH
ERRLIN  EQU 39H
 
;Y codes
 
YPUSH2  EQU 0DAH
YREM    EQU 0E5H
 
;T code
 
TBASE   EQU 80H+48
 
        EJECT
;MAIN PROGRAM - INITIAL ENTRY FROM O/S
 
BASIC:  EQU $
        LD IY,(B3PRM)
        LD HL,(B4)
        LD (IY+TOP),L
        LD (IY+TOP+1),H
        DEFW ZXNEW
        CALL STRTUP
;Now set up default format for maths out
        LD HL,040H*256+8        ;free format 8sfigs
        LD (DEFNF),HL
;Now print sign on message
        DEFW ZPTMSG
        DEFB 15
        DEFM 'NEWBRAIN BASIC'
        DEFB 13
;       +
READY   EQU $   ;blank lines return here
        DEFW ZPTMSG
        DEFB 5
           DEFM 'READY'
LMAIN2: EQU $   ;All commands return here eventually
        DEFW ZREPORT
LMAIN4: LD E,0
        CALL GTOP
        PUSH HL
        POP IX
        DEFW ZRDLINE
        JR C,LMAIN2
        DEFW ZRDSCH     ;Switch on 1st sig. chr.
        CP 15Q
        JR Z,READY              ;J. if empty line
        DEFW ZDIGIT
        JR C,LMAIN3
        DEFW ZAMENDL
        JR C LMAIN2
        JR LMAIN4
 
LMAIN3: DEFW ZCOMMAN
        JR LMAIN4
 
COMMAN: EQU $
        LD HL,0
        DEFW ZLAM1
        CALL SLNTT      ;LNTOTE saving AF
        CALL GLBAS
LCM3:   DEFW WEXEC
        JR C LCM4
        JR Z,LCM3
        CALL TSTCUR
        JR Z LCM5
        DEFW ZNLNT
        LD A,3
        JR LCM3
LCM5:   DEFW ZLAST
        JR Z LCM3
        OR A
LCM4:   DEFW ZREPORT
        CCF
        JR NC LCM3
        DEFW ZTETOLN
        RET
        EJECT
 
CMOBJ:  EQU $           ;compile object code
                        ;IX-> LNT entry
        CALL GOPTR
        RET NZ
        SET 3,(IY+FLAGS)
        PUSH IX
        POP HL
        DEFW ZSTINST            
        CALL GOTOP
        CALL LOPTR
        DEFW ZCM2HL             ;leave room for length
                                ; of obj code record
        DEFW WLSTATE
        RES 3,(IY+FLAGS)
        LD HL,0
        JP C LOPTR1
        CALL GOPTR
        EX DE,HL
        DEC DE
        DEC DE
        CALL GOTOP
        SBC HL,DE
        EX DE,HL
        LD (HL),E       ;HL->OBJ CODE RECORD,DE=LENGTH
        INC HL
        LD (HL),D
        INC HL
        RET
 
GOPTR:  LD H,(IX+5)
        LD L,(IX+4)
        LD A,H
        OR L
        RET
 
        EJECT
EXEC:                   ;execute statement
                        ;enter with HL->LNT entry
                        ;exits with flags:
                        ;Z if reqd jump to LNT entry
                        ;       ptd to by HL
                        ;C if error or stop(code in A)
        PUSH HL
        POP IX
        SET 0,(IY+FLAGS)
;FIRST CHECK FOR BREAK-IN
        DEFW ZNBKTST
        RET C
        CALL GOPTR
        JR NZ LXR21
        DEFW ZCMOBJ
        RET C
LXR21:  LD (IY+OMOVE+1),0
LXR215: LD A,(HL)       ;HERE WITH HL->OBJ CODE
        INC HL
        ADD A,A
        JR NC,LEX3
        DEFW ZXCL3
        JR LEX4
 
LEX3:   CALL XARITH
LEX4:   LD B,0
        INC B                   ; set NZ
        RET NC          ;if finished stmt.
        CP 254
        RET Z           ;if code 254
        RET C
        LD A,(IY+OMOVE+1)       ;Here if code 255.
        AND A
        JR Z,LXR215
        LD D,A          ;HERE IF OBJ CODE HAS MOVED
        LD E,(IY+OMOVE)
        ADD HL,DE
        JR LXR21
 
        EJECT
XAR2AD: PUSH HL
        CALL AR2ADR
        JR XRUN1D
 
XAR1AD: PUSH HL
        CALL AR1ADR
        JR XRUN1D
 
XARITH:         ;enter with A=2*code,flags set for value
                ; in A, HL->following byte
                ;exits as XCL3
        PUSH HL
        LD B,A          ;save op-code in B
        LD HL,6
        CALL CHROOM     ;check room for answer
        JR C XRUN1D     ;if no room
        LD A,B          ;restore op-code to A
        CALL GUSTP      ;USRSTP to HL
        DEFW ZMATHS     ;enter maths pack
XRUN1C: CALL NC LDUSTP  ;set USRSTP to answer
XRUN1D: POP HL
        RET C
        SCF
        SBC A,A         ;set A=255
        RET
 
        EJECT
 
REPORT:         ;report fault etc:
                ;    A=code number (0=stopped,
                ;    1=no message, else error code)
 
        LD (IY+OUTCON),0        ;to fix LFs on OUT!
        RES 0,(IY+FLAGS)
        CALL DISPOSE
        JP NC PTLN1
        CP 1
        JP Z PTLN1
        PUSH AF
        CALL TSTCUR
        JR Z LREP2
        DEFW ZLLN2
        JR C LREP2
        EX DE,HL
        PUSH IY
        POP HL
        LD BC,CLINNO
        ADD HL,BC
        LD (HL),E
        INC HL
        LD (HL),D
        DEC HL
        DEFW ZTTLN
        POP AF
        JR NC,LREP3
        LD H,(IY+BRKLIN+1)
        LD L,(IY+BRKLIN)
        LD A,L
        OR H
        JR NZ LREP1
LREP9:  DEFW ZPTMSG
            DEFB 8,13
            DEFM 'STOPPED'
        JR PTLN
 
LREP1:  SET 1,(IY+FLAGS)
        JR LREP8
 
LREP3:  LD (IY+ERRNO),A
        LD H,(IY+ERRLIN+1)
        LD L,(IY+ERRLIN)
        LD C,A
        LD A,H
        OR L
        JR Z LREP6
LREP8:  DEFW ZFIND
        LD A,97
        JR C LREP7
        EX DE,HL
        CALL LXG1
        CP 10
        JR Z LREP7
        SCF
        RET
 
LREP2:  POP AF
        JR C LREP9
LREP7:  LD C,A
LREP6:  DEFW ZPTMSG     ;code in C
           DEFB 7,13
           DEFM 'ERROR '
        LD L,C
        LD H,0           
        DEFW WPT#HL
PTLN:           ;if in run mode then unless IX 
                ;points to CMDLNE prints " AT <line#>"
        JR C LREP4
        CALL TSTCUR
        JR Z PTLN1
        DEFW ZLLN2
        JR C PTLN1
        DEFW ZPTMSG
           DEFB 4
           DEFM ' AT '
        LD H,(IY+CLINNO+1)
        LD L,(IY+CLINNO)
        PUSH HL
        DEFW WPT#HL
        POP HL
        JR C LREP4
        DEFW ZFIND
        DEFW ZLAST
        JR NZ PTLN1
        LD A,':'
        DEFW ZPTCHAR
        LD H,0
        LD L,(IY+CLINNO+2)
        INC L
        DEFW WPT#HL
        JR C LREP4
PTLN1:  DEFW ZXCLE2
        DEFW ZPTNL
        RET NC
LREP4:  LD E,0
        PUSH AF
LREP5:  DEFW ZCLOSE
        INC E
        JR NZ LREP5
        CALL STRTUP
        POP AF
        JR LREP7
 
STRTUP: LD A,(DEV0)
        LD DE,0
        LD BC,(DPSL)
        LD HL,(DPSP)
        DEFW ZOPNOUT
        RET
 
TSTCUR: PUSH IX
        POP HL
        LD A,(HL)
        INC HL
        OR (HL)
        DEC HL
        RET
 
        EJECT
;ATOMIC COMPILATION ROUTINES
 
CMONE:  EQU $   ;emit one byte to object code
                ;enter with byte in A
                ;preserves BCDEHL
        OR A
        BIT 5,(IY+FLAGS)
        RET NZ          ;if in no compile mode
        PUSH BC
        PUSH DE
        PUSH HL
        LD C,A
        LD HL,1
        DEFW ZNCROOM
                ;used to have LD A,24 here
        JR C,CMONE1
        CALL GOTOP
        LD (HL),C
        INC HL
        LD (IY+OTOP),L
        LD (IY+OTOP+1),H
CMONE1: POP HL
        POP DE
        POP BC
        RET

COPT:   EQU $
        LD B,TBASE
        DEFW ZCHECK
        LD A,68
        RET C
        DEFW ZINTIN
        LD A,L
        RRA             
        OR H
        LD A,70         ;unless HL=0 or 1 & INTIN left carry clear
        SCF
        RET NZ          
        DEC A
        BIT 7,(IY+BASFLG)
        RET NZ          ;if musntn't change base flag now
        DEFW    ZCHKCR
        RET C
        LD (IY+BASFLG),L
CREM    LD A,YREM       ;compile REM stmt
        JR LCM1
 
CPSH0:  LD HL,0
CPSH:   LD A,YPUSH2
CMTHRE: DEFW ZCMONE
CM2HL:  EQU $
        LD A,L
        DEFW ZCMONE
        LD A,H
LCM1:   DEFW ZCMONE
        RET
 
        EJECT
;XRUN 'subroutines' 
 
STATEL: DEFW ZCHKCR     ; if line is blank
        JR NC CREM      ; compile statement starting at
        LD HL,KWDLIS    ; current i/p position,
        DEFW ZSCAN
        JP C CLET       ; if keyword found jump to (BC)
        PUSH BC         ; instead
        RET
 
XCL3:   LD BC,XTABLE
        JP M ENTER
        LD BC,XTABL2
ENTER:          ;enter run-time routine:BC=table addr.
                ;DEHL hold parameters,A has code in d1-d6
        PUSH HL
        LD H,0
        LD L,A
        ADD HL,BC       ;n.b. clears carry
        LD C,(HL)       ;pick up addr of rt from table
        INC HL
        LD B,(HL)
        POP HL
        PUSH BC
        RET             ;enter routine with carry clear

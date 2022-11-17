
        NAME MOD9
 
;Stack etc

;INTERNALLY DEFINED GLOBALS

        GLOBAL XSTCHRS
        GLOBAL STRGE
        GLOBAL STRNE
        GLOBAL STREQ
        GLOBAL STRLE
        GLOBAL STRLT
        GLOBAL STRGT
        GLOBAL PUSH2
        GLOBAL LPSTR1
        GLOBAL AR2ADR
        GLOBAL AR1ADR
        GLOBAL CHKSS
        GLOBAL MKABS1
        GLOBAL MKABS2
        GLOBAL UNST
        GLOBAL UNST2
        GLOBAL NUMASS
        GLOBAL STRASS
        GLOBAL XAATOF
        GLOBAL ASSSTR
        GLOBAL UNST6
        GLOBAL POP
        GLOBAL XPSHST
        GLOBAL STRADD
        GLOBAL UNSTSS
        GLOBAL XPUSH2
        GLOBAL XPUSH3
        GLOBAL XPSHFP
        GLOBAL PUSHFP
        GLOBAL XDEREF
        GLOBAL XPUT
        GLOBAL XGET

;EXTERNALLY DEFINED GLOBALS USED BY THIS MODULE

        GLOBAL GSBAS
        GLOBAL GUSTP
        GLOBAL LDUSTP
        GLOBAL XRUN1D
        GLOBAL DELST2

;W & Z codes

        GLOBAL WCHKSS
        GLOBAL ZROUND
        GLOBAL ZMKNULL
        GLOBAL ZLDSTR
        GLOBAL WINUSS
        GLOBAL ZDLUSS
        GLOBAL ZRELBOO
        GLOBAL WUNSTSS
        GLOBAL ZCHKSS
        GLOBAL ZINUSS
        GLOBAL ZMKSTAB
        GLOBAL ZPOP
        GLOBAL ZPUSHFP
        GLOBAL ZUNST2
        GLOBAL WUNST2
        GLOBAL ZUNSTSS
        GLOBAL ZPUSH2
        GLOBAL ZRDLN2
        GLOBAL WFLT
        GLOBAL WPTCHAR
        GLOBAL ZINPUT

;Offsets from IY

OUTCON  EQU 06H
BASFLG  EQU 2AH
APAR    EQU 35H

        EJECT
 
POP:            ;Pop 2 bytes from user stack
                ;On exit HL = result, AFBCDE preserved
        PUSH DE
        CALL UNST2
        POP     DE
        RET
 
UNST2:  EQU $           ;AS "POP" BUT SETS DE=NEW(USRSTP)
        CALL GUSTP
        LD E,(HL)       ;PICK UP CONTENTS OF 2 BYTES
        INC HL          ;   JUST POPPED
        LD D,(HL)
        INC HL
        CALL LDUSTP
        EX DE,HL
        RET
 
XDEREF:         ;POP ADDRESS FROM USER STACK, PUSH 
                ;  F P VALUE IT POINTS TO
                ;NB CAN MAKE QUICKER BY CALLING INUSS
                ;  TO PUSH 4 MORE BYTES THEN COPYING
                ;  DIRECTLY:  18 MORE BYTES THOUGH
        PUSH HL
        CALL UNST2
        EX DE,HL
        JR LXPF6
 
XPSHFP: LD E,(HL)       ; PUSHFP called from obj code
        INC HL
        LD D,(HL)
        INC HL
        PUSH HL
LXPF6:  CALL PUSHFP
        JP XRUN1D
 
 
NUMASS: EQU $
        CALL GUSTP
        EX DE,HL
        LD HL,8
        ADD HL,DE
        CALL LDUSTP
        DEC HL
        LD A,(HL)
        DEC HL
        LD L,(HL)
        LD H,A
        JR PSHFP8
 
PUSHFP:         ;Push Floating pt value onto user stack
                ;On entry DE pts to value
                ;On exit CY set if no room
        PUSH DE
        LD BC,6
        DEFW ZINUSS
        POP DE
        RET C
PSHFP8: EX DE,HL
        LD BC,6
        LDIR
        RET
 
UNST6:          ;Discard Fl. pt. value from user stack
                ;On exit BC = 6, else as UNST.
        LD BC,6
UNST:           ;Discard BC bytes from user stack
                ;On exit ABC preserved
                ;DE = old USRSTP, HL = new USRSTP
        CALL GUSTP
        LD D,H
        LD E,L
        ADD HL,BC
        JP LDUSTP
 
        EJECT
;STRING STACK ROUTINES
 
XPSHST: PUSH HL
        DEFW ZUNST2
        CALL PUSHST
        JP XRUN1D
 
PUSHST:         ;Push string onto user stack
                ;On entry HL-> S.T. entry
        DEFW ZMKSTAB
LPSTR1: PUSH BC
        INC BC
        INC BC
        PUSH HL
        DEFW ZINUSS
        POP DE
        POP BC
        RET C
        LD (HL),C
        INC HL
        LD (HL),B
        INC HL
        EX DE,HL
        LD A,B
        OR C
        RET Z
        LDIR
        RET
 
 
STRASS:         ;Assign string on top of stack to
                ;variable next on stack.
        CALL GUSTP      ;HL=loc(string on stack)
        LD C,(HL)
        INC HL
        LD B,(HL)               ;BC=length(string)
        INC HL                  ;HL=loc(str.)
        ADD HL,BC               ;HL=loc(var. on stack)
        LD E,(HL)
        INC HL
        LD D,(HL)               ;DE=loc(var.s.t.entry)+2
        CALL ASSSTR
        INC HL
        INC HL
STRA1:  JP LDUSTP
 
ASSSTR: EX DE,HL                ;HL=" " " " " " " " " " "
        PUSH HL
        PUSH BC
        DEFW ZMKNULL            ;make destn.null
        POP BC
        POP HL
        LD A,C
        OR B
        JR Z,STRAS1             ;if source null
                ;BC=LENGTH
                ;HL=LOC(DEST.S.T.ENT)+2
        DEFW ZLDSTR
        RET C
                ;BC=SIZE
                ;DE=LOC(DEST STRING)
        CALL GUSTP              ;NB source string will
        INC HL                  ;   have been moved
        INC HL                  ;   by INSTR
        LDIR
        JR STRA1
 
STRAS1: EQU $
        CALL GUSTP
        INC HL
        INC HL
        JR STRA1
 
 
XSTCHRS:EQU $
                ;REPLACE FL.PT. BY STRING ON USER-STACK
        PUSH HL
        DEFW ZUNSTSS
        CALL STCH2
STCH1:  JP XRUN1D
 
STCH2:  PUSH DE
        LD BC,3
        DEFW WINUSS
        POP DE
        RET C
        LD (HL),1
        INC HL
        LD (HL),0
        INC HL
        LD (HL),E
        RET
 
;COMPARE STRINGS ON USER STACK AND REPLACE BY
;LOGICAL (FLOATING PT) VALUE
 
STRGT:  LD B,1
        DEFB 3AH
STRGE:  LD B,5
        DEFB 3AH
STRLT:  LD B,2
        DEFB 3AH
STRLE:  LD B,6
        DEFB 3AH
STRNE:  LD B,3
        DEFB 3AH
STREQ:  LD B,4
STRCMP:         ; Compare 2 strings on stack, push 
                ;   Boolean value in their place
                ; Enter with register B as for NUMCMP
                ; Returns to XRUN1D
        PUSH HL
        PUSH BC
        DEFW ZUNST2
        PUSH DE         ; addr of 2nd string
        PUSH HL         ; length of 2nd string
        CALL DELST2
        DEFW ZUNST2
        LD B,H          ;   left opd of comparison)
        LD C,L
        CALL DELST2
        POP HL
        AND A
        SBC HL,BC
        ADD HL,BC
        JR C,LSC3
        LD H,B
        LD L,C
LSC3:   POP BC
        EX DE,HL
        PUSH AF         ; remember which string is longer
LSC5:   LD A,D
        OR E            ; here DE = # chars left, HL ->
        JR Z,LSC6       ;   left opd, BC -> right opd
        LD A,(BC)
        CP (HL)
        JR NZ,LSC7
        INC BC
        INC HL
        DEC DE
        JR LSC5
 
LSC6:   POP AF          ; here if all chars the same:
        PUSH HL
LSC7:   POP HL
        CCF             ;compare lengths
                        ;Z SET IF IDENTICAL
                        ;CY SET IF <
        POP     BC      ;B=REQUIRED CONDITION CODE
        DEFW ZRELBOO
        EX DE,HL
        DEFW ZPUSHFP
LSC8:   JP XRUN1D
 
 
;CONCATENATE 2 STRINGS ON USER-STACK
STRADD: EQU $           ;CONCATENATE STRINGS
                        ;ADD STRING ON TOP OF USER-STACK
                        ; TO THE END OF THE ONE
                        ; UNDERNEATH IT
        PUSH HL
        DEFW ZUNST2
        PUSH DE
        EX DE,HL        ;DE=length of first string
                        ;HL points to first char of string
        ADD HL,DE       ;HL points to length of second 
        PUSH HL
        LD C,(HL)
        INC HL
        LD B,(HL)       ;BC=length of second string
        EX DE,HL
        ADD HL,BC
        EX DE,HL        ;DE=length of final string
        LD (HL),D
        DEC HL
        LD (HL),E       ;insert new length
        PUSH HL
        INC HL
        INC HL
        ADD HL,BC       ;HL=end of 2nd string
        LD B,H
        LD C,L
        EX (SP),HL
        DEFW ZRDLN2     ;reverse second string and
                        ;the count
        POP HL
        POP BC
        EX (SP),HL
        PUSH HL
        DEFW ZRDLN2     ;reverse first string
        POP HL
        POP BC
        DEFW ZRDLN2     ;reverse answer
        JR LSC8         ;jump to XRUN1D
 
        EJECT
;XPUT and XGET - single char I/O
;XGET uses OUTCON as a trick
 
XGET:   LD B,(HL)
        INC HL
        PUSH HL
        LD E,(IY+OUTCON)
        DEFW ZINPUT
        BIT 6,B
        LD E,A
        LD D,0
        JR Z XGET1
        CALL NC STCH2
        CALL NC STRASS
        JR LSC8
 
XGET1:  DEFW WFLT
        PUSH HL
        DEFW WUNST2
        POP DE
        CALL NC PSHFP8
        JR LSC8
 
XPUT:   PUSH HL
        DEFW ZUNSTSS
        LD A,0
        SBC A,D
        LD A,98
        JR C LSC8
        LD A,E
        DEFW WPTCHAR
        JR LSC8
 
        EJECT
;ARRAY ELEMENT ROUTINES
 
AR1ADR:         ;Replace array subscript and base on
                ;user stack by location of element
                ;CY set if subscript error
        DEFW ZUNSTSS    ;DE=S.S.
        RET C           ;R. IF S-S .LT. 0 OR .GT. 65535
        DEFW ZPOP       ;HL=LOC(BASE(REL))
        INC HL
        INC HL
        DEFW ZCHKSS     ;CHECK S.S. IN RANGE
        DEC HL
        DEC HL
        RET C
        JR MKABS1
 
UNSTSS: EQU $
;UNSTACK S.S. TO DE
        CALL UNST6
        EX DE,HL
        DEFW ZROUND
        LD A,6
        RET
 
CHKSS:          ;subtract base from DE then check DE<(HL)
                ;preserves BCHL, returns new DE
        BIT 0,(IY+BASFLG)
        JR Z,LCSS2
        DEC DE
LCSS2:  PUSH HL
        LD A,(HL)
        INC HL
        LD H,(HL)
        LD L,A
        SCF
        SBC HL,DE       ;limit-value-1
        POP HL
        LD A,7
        RET
 
AR2ADR: EQU $   ; REPLACE 2 SS & ARRAY BASE BY ABS LOC
                ;   (ON USER STACK)
                ; CARRY SET IF SUBSCRIPT ERROR
        DEFW ZUNSTSS
        PUSH DE         ; 2ND SUBSCRIPT
        DEFW WUNSTSS
        POP BC          ; BC := 2ND SS
        RET C           ; IF SS < 0 OR > 65535
        DEFW ZPOP       ; HL := BASE(REL)
        PUSH HL
        INC HL
        INC HL
        DEFW ZCHKSS     ; 1ST SS IN RANGE
        PUSH BC
        LD B,D
        LD C,E
        POP DE          ;DE=2ND S.S.
        INC HL
        INC HL          ;HL=LOC(2ND DIM)
        DEFW WCHKSS     ;CHECK 2ND S.S. IN RANGE
        POP HL          ;HL=LOC(BASE(REL))
        RET C
 
MKABS2: EQU $
                ;ON ENTRY
                ;       HL=LOC(BASE(REL TO SBAS))
                ;       BC=1ST S.S.
                ;       DE=2ND S.S.
                ;On exit usrstk contains abs locn
        PUSH HL
        PUSH DE
        INC HL
        INC HL
        INC HL
        INC HL          ;HL=LOC(2ND DIM)
        LD E,(HL)
        INC HL
        LD D,(HL)       ;DE=2ND DIM
        POP HL          ;HL=2ND S.S.
MKAB21: LD A,C
        OR B
        JR Z,MKAB22
        ADD HL,DE
        DEC BC
        JR MKAB21
 
MKAB22: EQU $
                ;HL=(1ST SS)*(2ND DIM)+2ND SS
        EX DE,HL        ;DE=DISP
        POP HL          ;HL=LOC(BASE)
MKABS1:         ;On entry HL->base (rel to SBAS)
                ;and DE = subscript, on exit
                ;usrstk contains abs locn.
        LD C,(HL)
        INC HL
        LD B,(HL)
        CALL GSBAS              ;HL=SBAS
        ADD HL,BC               ;HL=BASE(ABS.)
        LD B,6
MKAB11: ADD HL,DE
        DJNZ MKAB11
PUSH2:  EQU $   ;PUSH 2 BYTES FROM HL ONTO USER STACK
        PUSH HL
        LD BC,2
        DEFW ZINUSS
        POP BC
        RET C           ;IF NO ROOM
        LD (HL),C
        INC HL
        LD (HL),B
        RET
 
XPUSH3: PUSH HL
        LD BC,1
        DEFW ZINUSS
        POP HL
        RET C
XPUSH2: LD E,(HL)       ; PUSH2 called from obj code
        INC HL
        LD D,(HL)
        INC HL
        PUSH HL
        EX DE,HL
LXP21:  CALL PUSH2
        JP XRUN1D
 

XAATOF:         ;ASSIGN ACT PARAMETER TO FOR PARAMETER
                ;IE ASSIGN ACTUAL PARAMETER TO USER STACK
        PUSH HL
        LD L,(IY+APAR)
        LD H,(IY+APAR+1)
        JR LXP21
 
 
        END
        

#
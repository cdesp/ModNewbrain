
        NAME FOTAB
 
;FUNCTION AND OPERATOR TABLES
 
        GLOBAL  @MONOP
        GLOBAL  @DIOP
        GLOBAL  ASFLST
        GLOBAL  SSFLST
        GLOBAL KWDLIS
 
 
        EJECT
;BIT NUMBERS ETC
 
BIT8    EQU 256
BIT9    EQU 512
BIT14   EQU 4000H
BIT10   EQU 1024
BIT11   EQU 2048
ERROR   EQU 31
YSTCHR  EQU 0F2H
 
 
;OPERATOR DATA LISTS
;THERE ARE 2 LISTS, MONADIC & DIADIC
;THESE ARE SEARCHED USING SCAN (SEE SCAN FOR FORMAT
;   OF LISTS)
;THE VALUE FOR AN ITEM COMPRISES
;   (monadic)
;       Bits 0-5  data
;       Bits 6-11 zeroes
;   (diadic)
;       BITS 0 - 5  DATA IF LEFT OPERAND IS NUMERIC
;       BITS 6 - 11  DATA IF LEFT OPERAND IS A STRING
;   (both cases)
;       BITS 12 - 15  BINDING POWER (MUST BE NON-ZERO)
;WITHIN EACH 6 BITS OF DATA
;       BITS 0 - 3  ARE OPERATOR CODE (DISPLACEMENT)
;       BIT 4 INDICATES REQUIRED TYPE OF RIGHT OPERAND
;       BIT 5 INDICATES TYPE OF RESULT
;       TYPE 0 = NUMERIC, 1 = STRING
;If bit 4 = 0, codes 0-2 are monadic, 3-15 are diadic
;If bit 4 = 1, codes 0-6 are string opns, 15 = error
 
;MONADIC OPERATORS
 
@MONOP: DEFB  80H,3             ;# ENTRIES
        DEFW  (4*4096)+2
 
        DEFW  (9.SHL.12)+0
 
        DEFW  (9.SHL.12)+1
 
;DIADIC OPERATORS
 
@DIOP:  DEFB  81H,14    ;# ENTRIES
        DEFW  (6*4096)+BIT11+BIT10+(0*64)+3
 
        DEFW  (6*4096)+(ERROR*64)+4
 
        DEFW  (7*4096)+(ERROR*64)+5
 
        DEFW  (7*4096)+(ERROR*64)+6
 
        DEFW  (8.SHL.12)+(ERROR*64)+7
 
        DEFW  (6*4096)+BIT11+BIT10+(0*64)+ERROR
 
        DEFW  (3*4096)+(ERROR*64)+8
 
        DEFW  (2*4096)+(ERROR*64)+9
 
        DEFW    (5*4096)+BIT10+(4*64)+13
 
        DEFW    (5*4096)+BIT10+(5*64)+14
 
        DEFW    (5*4096)+BIT10+(6*64)+15
 
        DEFW  (5*4096)+BIT10+(1*64)+10
 
        DEFW  (5*4096)+BIT10+(2*64)+11
 
        DEFW  (5*4096)+BIT10+(3*64)+12
 
 
        EJECT
*IMPLEMENTATION-SUPPLIED FUNCTION LISTS
*Format of Data is      MSB: type
*                       LSB: op-code to compile
*Type info is:
*       BIT15:  0 always (=implementation supplied)
*       BIT14:  0=numeric valued
*               1=string valued
*       BIT13,12: 0 always (=not array function)
*       BIT11:  1 always (=function)
*       BIT10:  spare
*       BIT9:   0=only numeric arg(s) expected
*               1=other arg(s)
*       BIT8:   1=exactly one arg expected
*               0=otherwise
 

ASFLST: EQU     $
        DEFB 80H+15,27  ;# ENTRIES
 
AFB0:   EQU BIT11       ;FNS WITH NO PARMS
 
        DEFW    AFB0
 
        DEFW    AFB0+1
 
        DEFW    AFB0+2
 
AFB1:   EQU BIT11+BIT8+43H      ;FNS WITH ONE PARM.
 
        DEFW    AFB1

        DEFW    AFB1+1

        DEFW    AFB1+2

        DEFW    AFB1+3

        DEFW    AFB1+4

        DEFW    AFB1+5

        DEFW    AFB1+6

        DEFW    AFB1+7

        DEFW    AFB1+8

        DEFW    AFB1+9
 
        ;New implementation supplied fns with no param
 
AFB:    EQU BIT11
        
        DEFW AFB+4
 
        DEFW AFB+5
 
;Arithmetic standard functions with 1 string parameter
AFBS    EQU     BIT11+BIT9+BIT8
;Note...This uses BIT9 to say string parameter
 
YASC    EQU 82H
 
        DEFW    AFBS+YASC
 
        DEFW    AFBS+YASC+1
 
        DEFW    AFBS+YASC+2
 
        DEFW    AFBS+YASC+3
 
;Standard numeric functions
AFBA    EQU     BIT11+BIT8+43H+10
        DEFW AFBA
 
        DEFW AFBA+1
 
        DEFW    AFBA+2
 
YERRNO  EQU 0A0H
YINSTR  EQU 0A7H
YERRLIN EQU 0A2H
YTOP    EQU 0A5H
YFREE   EQU 0A6H
 
        DEFW BIT11+YERRNO
 
        DEFW BIT11+BIT9+YINSTR
 
        DEFW BIT11+YERRLIN
 
        DEFW BIT11+YTOP
 
        DEFW BIT11+YFREE
 
;STRING STANDARD FUNCTIONS (WITH ONE ARITH PAR.)

SFBA    EQU BIT14+BIT11
 
YSTSTR  EQU 88H
YRIGHT  EQU 89H
YLEFT   EQU 0A1H
YMID    EQU 0A9H
 
SSFLST: EQU     $
        DEFB 80H+42,6           ;# ENTRIES

        DEFW SFBA+BIT8+BIT9+YSTCHR
 
        DEFW    SFBA+BIT8+YSTSTR
 
        DEFW    SFBA+BIT9+YRIGHT
 
        DEFW SFBA+0     ;this has a dummy Y-code
 
        DEFW SFBA+BIT9+YLEFT
 
        DEFW SFBA+BIT9+YMID
 
 
 
KWDLIS: DEFB 80H,45
        DEFW CLET
        GLOBAL CLET
        DEFW CLIST
        GLOBAL CLIST
        DEFW CNEW
        GLOBAL CNEW
        DEFW CPRINT
        GLOBAL CPRINT
        DEFW CRUN
        GLOBAL CRUN
        DEFW CGOTO
        DEFW CGOTO
        GLOBAL CGOTO
        DEFW CGOSUB
        DEFW CGOSUB
        GLOBAL CGOSUB   ;with and without spaces
        DEFW CIF
        GLOBAL CIF
        DEFW CINPUT
        GLOBAL CINPUT
        DEFW CFOR
        GLOBAL CFOR
        DEFW CNEXT
        GLOBAL CNEXT
        DEFW CRETN
        GLOBAL CRETN
        DEFW CREM
        GLOBAL CREM
        DEFW CEND
        GLOBAL CEND
        DEFW CSTOP
        GLOBAL CSTOP
        DEFW CDIM
        GLOBAL CDIM
        DEFW CNON
        GLOBAL CNON
        DEFW COPT
        GLOBAL COPT
        DEFW CSAVE
        GLOBAL CSAVE
        DEFW CLOAD
        GLOBAL CLOAD
        DEFW CRAND
        GLOBAL CRAND
        DEFW COPEN
        GLOBAL COPEN
        DEFW CCLOSE
        GLOBAL CCLOSE
        DEFW CPRINT
        DEFW CRETN
        GLOBAL CRETN
        DEFW    CREST
        GLOBAL CREST
        DEFW    CREAD
        GLOBAL CREAD
        DEFW    CREM    ;Data line executes same as Rem
        DEFW    CPOKE
        GLOBAL CPOKE
        DEFW    CUSR
        GLOBAL CUSR
        DEFW    CCONT
        GLOBAL CCONT
        DEFW    CDEF
        GLOBAL CDEF
        DEFW    CCLEAR
        GLOBAL CCLEAR
        DEFW CMERGE
        GLOBAL CMERGE
        DEFW CVERIF
        GLOBAL CVERIF
        DEFW CGET
        GLOBAL CGET
        DEFW CRESU
        GLOBAL CRESU
        DEFW CDELET
        GLOBAL CDELET
        DEFW CPUT
        GLOBAL CPUT
        DEFW CLINP
        GLOBAL CLINP
        DEFW CREPORT
        GLOBAL CREPORT
        DEFW CCONT
        DEFW CRESER
        GLOBAL CRESER

        END

#
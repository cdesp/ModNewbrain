
        NAME XTABLE
 
;ENTRY TABLE FOR RUN-TIME ROUTINES
 
        GLOBAL XTABLE
        GLOBAL XTABL2
 
XTABLE  EQU -80H        ;TRANSLATIONS OF CODES WITH 1 
                        ;IN MS BIT
 
XBASE   EQU XTABLE-180H         ;So Ycode is table
                                ;displacement + C0H
 
YDEREF  EQU ($-XBASE)/2
        GLOBAL XDEREF
        DEFW XDEREF

YRUN    EQU ($-XBASE)/2
        GLOBAL XRUN
        DEFW XRUN

YGOTO   EQU ($-XBASE)/2
        GLOBAL XGOTO
        DEFW XGOTO

YGOSUB  EQU ($-XBASE)/2
        GLOBAL XGOSUB
        DEFW XGOSUB

YRET    EQU ($-XBASE)/2
        GLOBAL XRET
        DEFW XRET

YIF     EQU ($-XBASE)/2
        GLOBAL XIF
        DEFW XIF

YON     EQU ($-XBASE)/2
        GLOBAL XON
        DEFW XON

YLIST   EQU ($-XBASE)/2
        GLOBAL XLIST
        DEFW XLIST

YLETS   EQU ($-XBASE)/2
        GLOBAL STRASS
        DEFW STRASS

YLETN   EQU ($-XBASE)/2
        GLOBAL NUMASS
        DEFW NUMASS

YSTICN  EQU ($-XBASE)/2
        GLOBAL XSTICN
        DEFW XSTICN

YSTOCN  EQU ($-XBASE)/2
        GLOBAL XSTOCN
        DEFW XSTOCN

YSTDFO  EQU ($-XBASE)/2
        GLOBAL XSTDFO
        DEFW XSTDFO

YLISTN  EQU ($-XBASE)/2
        GLOBAL XLISTN
        DEFW XLISTN

YPTAB   EQU ($-XBASE)/2
        GLOBAL XPTAB
        DEFW XPTAB

YPSTR   EQU ($-XBASE)/2
        GLOBAL XPSTR
        DEFW XPSTR

YPFFF   EQU ($-XBASE)/2
        GLOBAL XPFFF
        DEFW XPFFF

YPCOM   EQU ($-XBASE)/2
        GLOBAL XPCOM
        DEFW XPCOM

YPTNL   EQU ($-XBASE)/2
        GLOBAL PTNL
        DEFW PTNL

YQSTR   EQU ($-XBASE)/2
        GLOBAL XQSTR
        DEFW XQSTR

YQDEF   EQU ($-XBASE)/2
        GLOBAL XQDEF
        DEFW XQDEF

YNUMIN  EQU ($-XBASE)/2
        GLOBAL XNUMIN
        DEFW XNUMIN

YSTRIN  EQU ($-XBASE)/2
        GLOBAL XSTRIN
        DEFW XSTRIN

YSEP    EQU ($-XBASE)/2
        GLOBAL XSEP
        DEFW XSEP

YFOR1   EQU ($-XBASE)/2
        GLOBAL XFOR1
        DEFW XFOR1

YFOR2   EQU ($-XBASE)/2
        GLOBAL XFOR2
        DEFW XFOR2

YPUSH2  EQU ($-XBASE)/2
        GLOBAL XPUSH2
        DEFW XPUSH2

YNEXT   EQU ($-XBASE)/2
        GLOBAL XNEXT
        DEFW XNEXT

YRAND   EQU ($-XBASE)/2
        GLOBAL XRAND
        DEFW XRAND
 
YSAVEH  EQU ($-XBASE)/2
        GLOBAL XSAVEH
        DEFW XSAVEH

YSAVEF  EQU ($-XBASE)/2
        GLOBAL XSAVEF
        DEFW XSAVEF

YLOADH  EQU ($-XBASE)/2
        GLOBAL XLOADH
        DEFW XLOADH

YLOADF  EQU ($-XBASE)/2
        GLOBAL XLOADF
        DEFW XLOADF

YOPEN   EQU ($-XBASE)/2
        GLOBAL XOPEN
        DEFW XOPEN

YCLOSE  EQU ($-XBASE)/2
        GLOBAL XCLOSE
        DEFW XCLOSE

YSTOP   EQU ($-XBASE)/2
        GLOBAL XSTOP
        DEFW XSTOP

YEND    EQU ($-XBASE)/2
        GLOBAL XEND
        DEFW XEND

YREM    EQU ($-XBASE)/2
        GLOBAL XREM
        DEFW XREM

YNEW    EQU ($-XBASE)/2
        GLOBAL XNEW
        DEFW XNEW

YSTROP  EQU ($-XBASE)/2
        GLOBAL STRADD
        DEFW STRADD             ;STRING CONCATENATE
        GLOBAL STREQ
        GLOBAL STRGT
        GLOBAL STRLT
        GLOBAL STRNE
        GLOBAL STRLE
        GLOBAL STRGE
        DEFW STREQ              ;STRING RELATIONS
        DEFW STRGT
        DEFW STRLT
        DEFW STRNE
        DEFW STRLE
        DEFW STRGE
 
YENDIN  EQU ($-XBASE)/2
        GLOBAL XENDIN
        DEFW XENDIN     ; CHECK FOR NL AT END OF I/P
 
YAR1AD  EQU ($-XBASE)/2
        GLOBAL XAR1AD
        DEFW XAR1AD             ;PUSH VALUE OF 1D ELEMENT
 
YAR2AD  EQU ($-XBASE)/2
        GLOBAL XAR2AD
        DEFW XAR2AD             ;PUSH VALUE OF 2D ELEMENT
 
YPSHFP  EQU ($-XBASE)/2
        GLOBAL XPSHFP
        DEFW XPSHFP             ;PUSH VALUE @ NEXT 2 
                                ;BYTES OF OBJECT
 
YSTCHR  EQU ($-XBASE)/2
        GLOBAL XSTCHRS
        DEFW XSTCHRS             ;CONVERT VALUE TO STRING
 
YPSHST  EQU ($-XBASE)/2
        GLOBAL XPSHST
        DEFW XPSHST             ;PUSH STRING @ NEXT 2
                                ;BYTES OF OBJECT
YONSUB  EQU ($-XBASE)/2
        GLOBAL XONSUB
        DEFW XONSUB
 
YDIM    EQU ($-XBASE)/2
        GLOBAL XDIMX
        DEFW XDIMX
 
 
;New XTABLE for OP codes from  HEX 80  to  HEX BF
 
XTABL2:
; X table for  10XXXXXX type OP codes
XBASEN  EQU XTABL2-100H ;so Ycode is table displ. +80H
 
YRESU1: EQU ($-XBASEN)/2
        GLOBAL XRESU1
        DEFW XRESU1
 
YPUT    EQU ($-XBASEN)/2
        GLOBAL XPUT
        DEFW XPUT
 
YASC    EQU     ($-XBASEN)/2
        GLOBAL  XASC    ;Numeric functions with
        DEFW    XASC    ;string parameters
 
YLEN    EQU     ($-XBASEN)/2
        GLOBAL XLEN
        DEFW    XLEN
 
YVAL    EQU     ($-XBASEN)/2
        GLOBAL  XVAL
        DEFW    XVAL
 
YNUM    EQU     ($-XBASEN)/2
        GLOBAL  XNUM
        DEFW    XNUM
 
YGET    EQU     ($-XBASEN)/2
        GLOBAL  XGET
        DEFW    XGET
 
YLINP   EQU     ($-XBASEN)/2
        GLOBAL  XLINP
        DEFW    XLINP
 
YSTSTR  EQU     ($-XBASEN)/2
        GLOBAL  XSTSTRS
        DEFW    XSTSTRS
 
YRIGHT  EQU     ($-XBASEN)/2
        GLOBAL  XRIGHT
        DEFW    XRIGHT
 
YREST   EQU     ($-XBASEN)/2
        GLOBAL  XREST
        DEFW    XREST
 
YPOKE   EQU     ($-XBASEN)/2
        GLOBAL  XPOKE
        DEFW    XPOKE
 
YUSR    EQU     ($-XBASEN)/2
        GLOBAL  XUSR
        DEFW    XUSR
 
YCONT   EQU     ($-XBASEN)/2
        GLOBAL  XCONT
        DEFW    XCONT
 
YBEGRD  EQU     ($-XBASEN)/2
        GLOBAL  XBEGRD
        DEFW    XBEGRD
 
YENDRD  EQU     ($-XBASEN)/2
        GLOBAL  XENDRD
        DEFW    XENDRD
 
YSEPRD  EQU     ($-XBASEN)/2
        GLOBAL  XSEPRD
        DEFW    XSEPRD
 
YCLEA2  EQU ($-XBASEN)/2
        GLOBAL XCLEA2
        DEFW XCLEA2
 
YRESU   EQU ($-XBASEN)/2
        GLOBAL XRESU
        DEFW XRESU
 
YAATOF  EQU ($-XBASEN)/2
        GLOBAL XAATOF
        DEFW XAATOF
 
YNUFN   EQU ($-XBASEN)/2
        GLOBAL XNUFN
        DEFW XNUFN
 
YCLEAR  EQU ($-XBASEN)/2
        GLOBAL INITST
        DEFW INITST
 
YNOFN   EQU ($-XBASEN)/2
        GLOBAL XNOFN
        DEFW XNOFN
 
YSTFN   EQU ($-XBASEN)/2
        GLOBAL XSTFN
        DEFW XSTFN
 
YERR:   EQU ($-XBASEN)/2
        GLOBAL XERR
        DEFW XERR
 
YBRK:   EQU ($-XBASEN)/2
        GLOBAL XBRK
        DEFW XBRK
 
YMERGE  EQU ($-XBASEN)/2
        GLOBAL XMERGE
        DEFW XMERGE
 
YMERGF: EQU ($-XBASEN)/2
        GLOBAL XMERGF
        DEFW XMERGF
 
YVERIF: EQU ($-XBASEN)/2
        GLOBAL XVERIF
        DEFW XVERIF
 
YVERFF: EQU ($-XBASEN)/2
        GLOBAL XVERFF
        DEFW XVERFF
 
YDELET  EQU ($-XBASEN)/2
        GLOBAL XDELET
        DEFW XDELET
 
YPUSH3  EQU ($-XBASEN)/2
        GLOBAL XPUSH3
        DEFW XPUSH3
 
YERRNO  EQU ($-XBASEN)/2
        GLOBAL XERRNO
        DEFW XERRNO
 
YLEFT   EQU ($-XBASEN)/2
        GLOBAL XLEFT
        DEFW XLEFT
 
YERRLIN EQU ($-XBASEN)/2
        GLOBAL XERRLIN
        DEFW XERRLIN
 
YREPORT EQU ($-XBASEN)/2
        GLOBAL XREPORT
        DEFW XREPORT
 
YRESER  EQU ($-XBASEN)/2
        GLOBAL XRESER
        DEFW XRESER
 
YTOP    EQU ($-XBASEN)/2
        GLOBAL XTOP
        DEFW XTOP
 
YFREE   EQU ($-XBASEN)/2
        GLOBAL XFREE
        DEFW XFREE
 
YINSTR  EQU ($-XBASEN)/2
        GLOBAL XINSTR
        DEFW XINSTR
 
YINST1  EQU ($-XBASEN)/2
        GLOBAL XINST1
        DEFW XINST1
 
YMID    EQU ($-XBASEN)/2
        GLOBAL XMID
        DEFW XMID
 
YMID1   EQU ($-XBASEN)/2
        GLOBAL XMID1
        DEFW XMID1
 
        END

#
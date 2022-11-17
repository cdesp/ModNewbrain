
        NAME M0
                ;declarations of contents of RAM 
                ;   working-space
                ;should not be linked with rest of
                ;   program but should be used to
                ;   check EQUates in other modules
 
        DEFS 6          ;spare
  
OUTCON  DEFS 1          ;channel # for output
INCON   DEFS 1          ;channel # for input
ERRNO   DEFS 1          ;for ERRNO function
INPTR   DEFS 2          ;addr of next I/P char
        DEFS 1          ;spare
SVINPT  DEFS 2          ;saves INPTR for RSINST
 
STOP    DEFS 2          ;byte after source code
OTOP    DEFS 2          ;byte after object code

USRSTP  DEFS 2          ;base of user stack area
FORPTR  DEFS 2          ;base of FOR blocks area
GSSPTR  DEFS 2          ;base of GOSUB stack area
STBAS   DEFS 2          ;base of string storage area
ARBAS   DEFS 2          ;base of array elements area
SBAS    DEFS 2          ;base of symbol table
LBAS    DEFS 2          ;base of line number table

TOP     DEFS 2          ;top of BASIC ram
 
FLAGS   DEFS 1  ;bit 0 set if obj code ptd to by IX
                ;       mustn't be thrown away 
                ;bit 1 set if interrupts suppressed
                ;bit 2 used by XSAVE to keep tokens
                ;bit 3 set if compiling
                ;bit 4 set if compiling DEF
                ;bit 5 set if no compilation
                ;bit 6 set if INPTR is relative 
                ;       to USRSTP currently

        DEFS 4          ;spare

CLINNO  DEFS 3          ;the last AT line where CONTINUE
                        ;goes from
 
BASFLG  DEFS 1          ;d0=base, d7=1 if mustn't alter
@TYPE   DEFS 1          ;used in compiling exprns
OMOVE   DEFS 2          ;amount object code been moved
 
PPFLGS  DEFS 1  ;pre-processor flags
                ;bit 0 set if in string
                ;bit 1 set if after REM or DATA
                ;bit 2 set during SAVE
                ;bit 3 unset when expecting
                ;initial kwd only
                ;bit 4 set during AMENDL if
                ;it thinks line is already
                ;entokened
 
DTINPT  DEFS 2          ;char pointer for READ
                        ;only the l.s. byte used but the
                        ;other may be zeroed periodically
DTLN    DEFS 2          ;line pointer for READ
 
FORVAR  DEFS 2          ; temp for CFOR
 
APAR    DEFS 2          ;points to act par in userstack
FPAR    DEFS 2          ;n/t of for par in DEF
 
ERRLIN  DEFS 2          ;contains line of ON ERR
BRKLIN  DEFS 2          ;contains line of ON BRK

SOBAS   EQU $           ;base of source code



#
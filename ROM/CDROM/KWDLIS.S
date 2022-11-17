
        NAME KWDLIS
 
        GLOBAL KWDS
        GLOBAL OKWDS
 
*This is to confuse you - the label KWDLIS is in
*FOTAB, but this contains the two keyword lists
*KWDS and OKWDS (stands for some keywords and
*other keywords)
 
 
KWDS:   DEFB 80H        ;base of tokens
        DEFB 45 ;size of list
 
        DEFM 'LE'
        DEFB 'T'+80H

        DEFM 'LIS'
        DEFB 'T'+80H

        DEFM 'NE'
        DEFB 'W'+80H

        DEFM 'PRIN'
        DEFB 'T'+80H

        DEFM 'RU'
        DEFB 'N'+80H

        DEFM 'GOT'
        DEFB 'O'+80H
 
        DEFM 'GO T'
        DEFB 'O'+80H
 
        DEFM 'GOSU'
        DEFB 'B'+80H
 
        DEFM 'GO SU'
        DEFB 'B'+80H

        DEFM 'I'
        DEFB 'F'+80H

        DEFM 'INPU'
        DEFB 'T'+80H

        DEFM 'FO'
        DEFB 'R'+80H

        DEFM 'NEX'
        DEFB 'T'+80H

        DEFM 'RETUR'
        DEFB 'N'+80H

        DEFM 'RE'
        DEFB 'M'+80H

        DEFM 'EN'
        DEFB 'D'+80H

        DEFM 'STO'
        DEFB 'P'+80H

        DEFM 'DI'
        DEFB 'M'+80H

        DEFM 'O'
        DEFB 'N'+80H

        DEFM 'OPTIO'
        DEFB 'N'+80H

        DEFM 'SAV'
        DEFB 'E'+80H

        DEFM 'LOA'
        DEFB 'D'+80H

        DEFM 'RANDOMIZ'
        DEFB 'E'+80H

        DEFM 'OPE'
        DEFB 'N'+80H

        DEFM 'CLOS'
        DEFB 'E'+80H

        DEFB '?'+80H
 
        DEFM 'RE'
        DEFB 'T'+80H
 
        DEFM    'RESTOR'
        DEFB 'E'+80H
 
        DEFM    'REA'
        DEFB 'D'+80H
 
        DEFM    'DAT'
        DEFB 'A'+80H
 
        DEFM    'POK'
        DEFB 'E'+80H
 
        DEFM    'CAL'
        DEFB 'L'+80H
 
        DEFM    'CONTINU'
        DEFB 'E'+80H
 
        DEFM    'DE'
        DEFB 'F'+80H
 
        DEFM    'CLEA'
        DEFB 'R'+80H
 
        DEFM 'MERG'
        DEFB 'E'+80H
 
        DEFM 'VERIF'
        DEFB 'Y'+80H
 
        DEFM 'GE'
        DEFB 'T'+80H
 
        DEFM 'RESUM'
        DEFB 'E'+80H
 
        DEFM 'DELET'
        DEFB 'E'+80H
 
        DEFM 'PU'
        DEFB 'T'+80H
 
        DEFM 'LINPU'
        DEFB 'T'+80H
 
        DEFM 'REPOR'
        DEFB 'T'+80H
 
        DEFM 'CON'
        DEFB 'T'+80H
 
        DEFM 'RESERV'
        DEFB 'E'+80H
 
OKWDS:  DEFB 80H,60 ;number of entries
 
;MONADIC OPERATORS
 
        DEFM  'NO'      ;LOGICAL NOT
        DEFB 'T'+80H
 
;+ and - are monadics too - leave alone
 
;DIADIC OPERATORS
 
        DEFB  '+'+80H
 
        DEFB  '-'+80H
 
        DEFB  '*'+80H
 
        DEFB  '/'+80H
 
        DEFB  '^'+80H
 
        DEFB  '&'+80H   ;AS ALTERNATIVE TO '+'
 
        DEFM  'AN'
        DEFB 'D'+80H
 
        DEFB  'O','R'+80H
 
        DEFB    '<','>'+80H
 
        DEFB    '<','='+80H
 
        DEFB    '>','='+80H
 
        DEFB  '='+80H
 
        DEFB '>'+80H
 
        DEFB  '<'+80H
 
 
        DEFM    'RN'
        DEFB 'D'+80H
 
        DEFM    'P'
        DEFB 'I'+80H
 
        DEFM    'PO'
        DEFB 'S'+80H
 
 
        DEFM    'AB'
        DEFB 'S'+80H

        DEFM    'AT'
        DEFB 'N'+80H

        DEFM    'CO'
        DEFB 'S'+80H

        DEFM    'EX'
        DEFB 'P'+80H

        DEFM    'IN'
        DEFB 'T'+80H

        DEFM    'LO'
        DEFB 'G'+80H

        DEFM    'SG'
        DEFB 'N'+80H

        DEFM    'SI'
        DEFB 'N'+80H

        DEFM    'SQ'
        DEFB 'R'+80H

        DEFM    'TA'
        DEFB 'N'+80H
 
        ;New implementation supplied fns with no param
 
        
        DEFM 'TRU'
        DEFB 'E'+80H
 
        DEFM 'FALS'
        DEFB 'E'+80H
 
;Arithmetic standard functions with 1 string parameter
;Note...This uses BIT9 to say string parameter
 
 
        DEFM    'AS'
        DEFB 'C'+80H
 
        DEFM    'LE'
        DEFB 'N'+80H
 
        DEFM    'VA'
        DEFB 'L'+80H
 
        DEFM    'NU'
        DEFB 'M'+80H
 
;Standard numeric functions
        DEFM 'AS'
        DEFB 'N'+80H
 
        DEFM 'AC'
        DEFB 'S'+80H
 
        DEFM    'PEE'
        DEFB 'K'+80H
 
        DEFM 'ERRN'
        DEFB 'O'+80H
 
        DEFM 'INST'
        DEFB 'R'+80H
 
        DEFM 'ERRLI'
        DEFB 'N'+80H
 
        DEFM 'TO'
        DEFB 'P'+80H
 
        DEFM 'FRE'
        DEFB 'E'+80H
 
;STRING STANDARD FUNCTIONS (WITH ONE ARITH PAR.)

 
 

        DEFM    'CHR'
        DEFB '$'+80H
 
        DEFM    'STR'
        DEFB '$'+80H
 
        DEFM    'RIGHT'
        DEFB '$'+80H
 
        DEFM 'FILE'
        DEFB '$'+80H
 
        DEFM 'LEFT'
        DEFB '$'+80H
 
        DEFM 'MID'
        DEFB '$'+80H
 
        DEFM 'BAS'      ;affects MOD2
        DEFB 'E'+80H
 
        DEFM 'IN'       ;affects MOD4
        DEFB '#'+80H
 
        DEFM 'OUT'
        DEFB '#'+80H
 
        DEFB '#'+80H
 
        DEFM 'THE'      ;affects MOD5D and MOD2
        DEFB 'N'+80H    ;is 80H+40
 
        DEFB 80H        ;the supreme invisible THEN
 
        DEFM 'T'
        DEFB 'O'+80H
 
        DEFM 'STE'
        DEFB 'P'+80H    ;affects MOD4
 
        DEFB 'F','N'+80H        ;affects MOD5A
 
        DEFM 'TA'       ;affects MOD3
        DEFB 'B'+80H
 
        DEFM 'ERRO'     ;affects MOD2
        DEFB 'R'+80H
 
        DEFM 'BREA'
        DEFB 'K'+80H
 
        END

#
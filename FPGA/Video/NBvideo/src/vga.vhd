--------------------------------------------------------------------------------
--
--   FileName:         vga_controller.vhd
--   Dependencies:     none
--   Design Software:  Quartus II 64-bit Version 12.1 Build 177 SJ Full Version
--
--   HDL CODE IS PROVIDED "AS IS."  DIGI-KEY EXPRESSLY DISCLAIMS ANY
--   WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING BUT NOT
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--   PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL DIGI-KEY
--   BE LIABLE FOR ANY INCIDENTAL, SPECIAL, INDIRECT OR CONSEQUENTIAL
--   DAMAGES, LOST PROFITS OR LOST DATA, HARM TO YOUR EQUIPMENT, COST OF
--   PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR SERVICES, ANY CLAIMS
--   BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY DEFENSE THEREOF),
--   ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER SIMILAR COSTS.
--
--   Version History
--   Version 1.0 05/10/2013 Scott Larson
--     Initial Public Release
--   Version 1.1 03/07/2018 Scott Larson
--     Corrected two minor "off-by-one" errors
--    
--------------------------------------------------------------------------------

LIBRARY ieee; 
USE ieee.std_logic_1164.all; 
USE ieee.numeric_std.all; 
--use ieee.math_real.all; 


--chip select: GW1N-1 FPGA Tang Nano (not 1K)  GW1N-LV1QN48C6/I5


--640x480x60Hz 25,175 MHZ clock
--polarity neg 
--    h_pulse  : INTEGER := 96;    --horiztonal sync pulse width in pixels
--    h_bp     : INTEGER := 48;    --horiztonal back porch width in pixels
--    h_pixels : INTEGER := 640;   --horiztonal display width in pixels
--    h_fp     : INTEGER := 16;    --horiztonal front porch width in pixels
--    h_pol    : STD_LOGIC := '0';  --horizontal sync pulse polarity (1 = positive, 0 = negative)
-- pol neg
--    v_pulse  : INTEGER := 2;      --vertical sync pulse width in rows
--    v_bp     : INTEGER := 33;--35;     --vertical back porch width in rows
--    v_pixels : INTEGER := 480; --400  --vertical display width in rows
--    v_fp     : INTEGER := 10;--12;      --vertical front porch width in rows
--    v_pol    : STD_LOGIC := '0');--'1'); --vertical sync pulse polarity (1 = positive, 0 = negative)

--VGA Signal 640 x 350 @ 70 Hz timing
--  polarity positive
--    h_pulse  : INTEGER := 96;    --horiztonal sync pulse width in pixels
--    h_bp     : INTEGER := 48;    --horiztonal back porch width in pixels
--    h_pixels : INTEGER := 640;   --horiztonal display width in pixels
--    h_fp     : INTEGER := 16;    --horiztonal front porch width in pixels
--    h_pol    : STD_LOGIC := '1';  --horizontal sync pulse polarity (1 = positive, 0 = negative)
-- polairy negative
--    v_pulse  : INTEGER := 2;      --vertical sync pulse width in rows
--    v_bp     : INTEGER := 60;     --vertical back porch width in rows
--    v_pixels : INTEGER := 350;   --vertical display width in rows
--    v_fp     : INTEGER := 37;      --vertical front porch width in rows
--   v_pol    : STD_LOGIC := '0'); --vertical sync pulse polarity (1 = positive, 0 = negative)

--VGA Signal 800 x 600 @ 60 Hz timing pixel clock 40MHz
--  polarity positive
--    h_pulse  : INTEGER := 128;    --horiztonal sync pulse width in pixels
--    h_bp     : INTEGER := 88;    --horiztonal back porch width in pixels
--    h_pixels : INTEGER := 800;   --horiztonal display width in pixels
--    h_fp     : INTEGER := 40;    --horiztonal front porch width in pixels
--    h_pol    : STD_LOGIC := '1';  --horizontal sync pulse polarity (1 = positive, 0 = negative)
-- polairy positive
--    v_pulse  : INTEGER := 4;      --vertical sync pulse width in rows
--    v_bp     : INTEGER := 23;     --vertical back porch width in rows
--    v_pixels : INTEGER := 600;   --vertical display width in rows
--    v_fp     : INTEGER := 1;      --vertical front porch width in rows
--   v_pol    : STD_LOGIC := '1'); --vertical sync pulse polarity (1 = positive, 0 = negative)




ENTITY vga_controller IS
  GENERIC(
    h_pulse  : INTEGER := 128;    --horiztonal sync pulse width in pixels
    h_bp     : INTEGER := 88;    --horiztonal back porch width in pixels
    h_pixels : INTEGER := 800;   --horiztonal display width in pixels
    h_fp     : INTEGER := 40;    --horiztonal front porch width in pixels
    h_pol    : STD_LOGIC := '1';  --horizontal sync pulse polarity (1 = positive, 0 = negative)
    v_pulse  : INTEGER := 4;      --vertical sync pulse width in rows
    v_bp     : INTEGER := 23;--33;--35;     --vertical back porch width in rows
    v_pixels : INTEGER := 600; --400  --vertical display width in rows
    v_fp     : INTEGER := 1;--12;      --vertical front porch width in rows
    v_pol    : STD_LOGIC := '1');--'1'); --vertical sync pulse polarity (1 = positive, 0 = negative)
  PORT(
    pixel_clk : IN   STD_LOGIC;  --pixel clock at frequency of VGA mode being used
    --reset_n   : IN   STD_LOGIC;  --active low asycnchronous reset
    h_sync    : OUT  STD_LOGIC;  --horizontal sync pulse
    v_sync    : OUT  STD_LOGIC;  --vertical sync pulse
   -- n_blank   : OUT  STD_LOGIC;  --direct blacking output to DAC
  --  n_sync    : OUT  STD_LOGIC; --sync-on-green output to DAC
  	 rgbi	    :  OUT  STD_LOGIC_VECTOR(3 DOWNTO 0) := (OTHERS => '0');  --red,green,blue,intensity magnitude output to DAC
     --scrend	 :  OUT   STD_LOGIC;  -- '0' IF DISPLAY FINISHED
     scrst    :  OUT STD_LOGIC;  -- '0' IF DISPLAY START
	 --istext	 :  IN   STD_LOGIC;  -- '1' IF WE DISPLAY TEXT or graphics
	 --islowres :  IN   STD_LOGIC;  -- '1' IF WE DISPLAY lowres or hires
	 datain	 :  IN   STD_LOGIC_VECTOR(7 DOWNTO 0);
	-- datace   :  OUT  STD_LOGIC := '1'; --'0' enables memory chip --maybe let it always on
	 addrout  :  OUT  STD_LOGIC_VECTOR(15 DOWNTO 0) -- A0 - A15 for addressing the 64k of memory chip
	 );
END vga_controller;

ARCHITECTURE behavior OF vga_controller IS
  CONSTANT h_period : INTEGER := h_pulse + h_bp + h_pixels + h_fp; --total number of pixel clocks in a row
  CONSTANT v_period : INTEGER := v_pulse + v_bp + v_pixels + v_fp; --total number of rows in column
  CONSTANT v_dbl : INTEGER := 2; --2 for doubling lines
  CONSTANT v_real : INTEGER := 250 * v_dbl; --real vertical lines 
  CONSTANT h_real : INTEGER := 640; --real horizontal pixels

--SIGNAL txcolumn:INTEGER RANGE 0 TO 40-1 := 0; --TEXT COLUMN
SIGNAL txfontline:INTEGER RANGE 0 TO 10-1 :=0; --WHICH LINE OF TEXT CHAR IS PRINTED
SIGNAL txfontpixel:INTEGER RANGE 0 TO 8-1 :=0; --WHICH PIXEL OF TEXT CHAR IS PRINTED
--SIGNAL charaddr:INTEGER RANGE 0 TO 800- 1:=0; --THE ADDRESS OF THE char

SIGNAL PXLOUT:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --DATA FROM VIDEO MEM
SIGNAL PXLLEFT:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --DATA FROM VIDEO MEM
SIGNAL PXLRIGHT:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --DATA FROM VIDEO MEM
SIGNAL PXLBYTE:STD_LOGIC_VECTOR(7 DOWNTO 0) ;  --DATA FROM VIDEO MEM
SIGNAL PXLBYTEnx:STD_LOGIC_VECTOR(7 DOWNTO 0) ;  --DATA FROM VIDEO MEM
SIGNAL memaddr:INTEGER RANGE 0 TO 32767 :=0;
SIGNAL COLRaddr:INTEGER RANGE 0 TO 2048-1 :=0;
SIGNAL PXLFORE:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0001";  --DATA FROM VIDEO MEM FORE COLOR
SIGNAL PXLBACK:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --DATA FROM VIDEO MEM BACK COLOR
SIGNAL PXLFOREnx:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0010";  --DATA FROM VIDEO MEM FORE COLOR
SIGNAL PXLBACKnx:STD_LOGIC_VECTOR(3 DOWNTO 0) := "0000";  --DATA FROM VIDEO MEM BACK COLOR
SIGNAL SETUPCHAR:INTEGER RANGE 0 TO 9:=0;
SIGNAL disp_ena  : STD_LOGIC;  --display enable ('1' = display time, '0' = blanking time)
SIGNAL vidset:STD_LOGIC_VECTOR(1 DOWNTO 0) := "01";  --video settings bit 1 graph=0/text=1 ,  bit 0 320=0,640=1
SIGNAL vidbuf: STD_LOGIC := '0'; --video settings bit 7 is buffer 0 start at 0 or 1 start at 32768
SIGNAL membuf: STD_LOGIC := '0'; -- memaddr high bit to control double buffering
SIGNAL Rpixel_clk  : STD_LOGIC;
SIGNAL RSTRT  : STD_LOGIC;
SIGNAL READREGISTER:INTEGER RANGE 0 TO 1024 :=0;  --THIS IS THE VIDEO MODE REGISTER BIT 0 AND 1 ARE USED
SIGNAL CONFIGREG:STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000";
SIGNAL ISATLAS : STD_LOGIC :='0'; --0 MEANS ATLAS ELSE NB


Signal dout_o: std_logic_vector(7 downto 0);
Signal clk_i : std_logic;
Signal oce_i : std_logic := '1';
Signal ce_i : std_logic := '1';
Signal reset_i : std_logic := '1';
Signal wre_i : std_logic := '1';
Signal ad_i : std_logic_vector(12 downto 0);
Signal din_i : std_logic_vector(7 downto 0);

Signal ReadFNT : STD_LOGIC; --FRO READING THE FONT TO INTERNAL RAM

--NB STUFF
			  --VIDEO SIGNALS
Signal sUCR     : STD_LOGIC; --10 PIXEL PER CHAR
Signal s80L     : STD_LOGIC; -- 80 CHARS PER LINE (MEANS 1 PIXEL HORIZ)
Signal s3240    : STD_LOGIC; -- narrow graphics screen
Signal sFS      : STD_LOGIC; --
Signal sRV      : STD_LOGIC; -- REVERSE FIELD
Signal TVP      : STD_LOGIC; -- eNABLE
Signal sSETADDR : STD_LOGIC;	
Signal sVIDEO9  : STD_LOGIC;  -- 1 WHEN low VIDEO ADDR start from 1
Signal TVCLK    : STD_LOGIC;
Signal TVRAM    : std_logic_vector(15 downto 0);    --tv ram struct
Signal TVOFFS   : std_logic_vector(7 downto 0);     --offset on struct
Signal TVDEP    : std_logic_vector(7 downto 0);     --TOTAL LINES
Signal TVEL     : std_logic_vector(7 downto 0);     --LINE + EXCESS
Signal TVFRM    : std_logic_vector(7 downto 0);     --LINES IN WINDOW on struct
Signal NBVIDAD  : std_logic_vector(15 downto 0);    --NB Video screen address
--Signal NBMEMAD  : INTEGER RANGE 0 TO 32767 :=0;    --NB Mem accees address
Signal ReadREGs : STD_LOGIC;
Signal ISGRAPH  : STD_LOGIC:='0';
Signal ISTEXT   : STD_LOGIC:='1';
Signal tvena    : STD_LOGIC:='1'; --1 means tv enabled
Signal PXLTEXT  :STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000";  --PATTERN OF TEXT CHAR DATA FROM VIDEO MEM
Signal PXLTEXTnx :STD_LOGIC_VECTOR(7 DOWNTO 0) := "00000000";  --PATTERN OF TEXT CHAR DATA FROM VIDEO MEM



type BTReadState is (BRS_IDLE,BRS_DataST,BRS_DataST2,BRS_DataST3,BRS_DataRD,BRS_DataSV);
SIGNAL sprstate : BTReadState := BRS_IDLE;



component Gowin_SPRAM
    port (
        dout: out std_logic_vector(7 downto 0);
        clk: in std_logic;
        oce: in std_logic;
        ce: in std_logic;
        reset: in std_logic;
        wre: in std_logic;
        ad: in std_logic_vector(12 downto 0);
        din: in std_logic_vector(7 downto 0)
    );
end component;

--
function is_even(val : integer) return boolean is
    constant vec: signed(31 downto 0) := to_signed(val, 32);
begin
   return vec(0) = '0';
end;


BEGIN


SPRMEM: Gowin_SPRAM
    port map (
        dout => dout_o,
        clk => clk_i,
        oce => oce_i,
        ce => ce_i,
        reset => reset_i,
        wre => wre_i,
        ad => ad_i,
        din => din_i
    );


 PROCESS(Rpixel_clk)
    VARIABLE h_count : INTEGER RANGE 0 TO h_period - 1 := 0;  --horizontal counter (counts the columns)
    VARIABLE v_count : INTEGER RANGE 0 TO v_period - 1 := 0;  --vertical counter (counts the rows)
    VARIABLE column    : INTEGER RANGE 0 TO h_pixels- 1:=0;    --horizontal pixel coordinate
    VARIABLE row       : INTEGER RANGE 0 TO v_pixels- 1:=0;    --vertical pixel coordinate	 
    VARIABLE LETCOL:INTEGER RANGE 0 TO 7 := 0;  --LETTER COLUMN    
    VARIABLE VIDEOaddr:INTEGER RANGE 0 TO 32767 :=0;
    VARIABLE GRSTLN: INTEGER RANGE 0 TO 32767:=0; --START OF EACH LINE ON GRAPHICS WE USE IT TO DBL THE VERT LINE
    VARIABLE PRLINES:INTEGER RANGE 0 TO 63; --TEXT LINES PRINTED ON SCREEN SO FAR
    VARIABLE TEXTCHAR:INTEGER RANGE 0 TO 256-1:=0 ; --DATA FROM VIDEO MEM WHICH CHAR
    VARIABLE GR1STLN:BOOLEAN;
    
  --  VARIABLE dattemp : INTEGER RANGE 0 to 255;   
  --  VARIABLE intram : integer range 0 to 32767; --keep track of the data we write on internal ram
    
    
    VARIABLE VIDDAT0 : INTEGER RANGE 0 to 255;   
    VARIABLE VIDDAT1 : INTEGER RANGE 0 to 255;   
    VARIABLE VIDDAT2 : INTEGER RANGE 0 to 255;   
    VARIABLE VIDDAT3 : INTEGER RANGE 0 to 255;   

    VARIABLE HORZPXL:INTEGER range 0 to 2; --1 OR 2 FOR DBL PIXELS
    VARIABLE VERTPXL:INTEGER range 0 to 2; --1 OR 2 FOR DBL PIXELS
    VARIABLE SKIPLINE:BOOLEAN :=FALSE;
    VARIABLE NBSCRFIN:BOOLEAN :=FALSE;
    VARIABLE  NXTPXL : STD_LOGIC;
    
    --VARIABLE LEFTGAP:INTEGER RANGE 0 TO 255;
    --VARIABLE EL:INTEGER RANGE 0 TO 255;

    VARIABLE BTCNT1:INTEGER  RANGE 0 TO 800;
    VARIABLE MYSTEP:INTEGER RANGE 0 TO 2047 ; 
    VARIABLE BYTCNT:INTEGER RANGE 0 TO 2565;
    VARIABLE EL:INTEGER RANGE 0 TO 255;   
    VARIABLE REVF:BOOLEAN; 



    PROCEDURE NBNEWCHAR(TC:INTEGER) IS    
    BEGIN        
         VIDDAT0:=VIDDAT1;
         VIDDAT1:=VIDDAT2;
         VIDDAT2:=VIDDAT3;
         VIDDAT3:=TC;            

         NBSCRFIN:=((VIDDAT0=0) AND (VIDDAT1=0) AND (VIDDAT2=32) AND (TC=32) ) AND ROW>15 ;-- AND VIDDAT2=32 AND TC=32;
        -- NBSCRFIN:=( (VIDDAT2=0) AND (TC=0)  ) ;-- AND VIDDAT2=32 AND TC=32;
         --ISTEXT:=NOT ((VIDDAT2=0) AND (VIDDAT3=0));--DOUBLE ZEROES ENDS TEXT
         SKIPLINE:=TC=0 OR SKIPLINE;
         IF PRLINES=to_integer(unsigned(TVDEP)) THEN
           IF (VIDDAT1=0) AND (VIDDAT2=0) AND (VIDDAT3=0) AND TC=0 THEN
             ISGRAPH<='1';
             GRSTLN:=0;   
             GR1STLN:=FALSE; --TRUE WHEN THE 1ST LINE IS COMPLETE AT COLUMN 639
             IF s3240='1' AND S80L='1' THEN 
                VIDEOaddr:=VIDEOaddr+5;                  
             END IF;     
--             IF s3240='0' AND S80L='0' THEN 
--                GRSTLN:=VIDEOaddr;                  
--             END IF;     
        
           END IF;
        END IF;         
    END PROCEDURE;

    PROCEDURE TESTPXLS IS
    BEGIN
        IF COLUMN=0 AND ROW=0 THEN PXLOUT<="1001"; END IF;        
        IF H_COUNT=1 AND V_COUNT=1 THEN PXLOUT<="1010"; END IF;
        IF H_COUNT=2 AND V_COUNT=2 THEN PXLOUT<="1011"; END IF;
        IF H_COUNT=1 AND V_COUNT=3 THEN PXLOUT<="1010"; END IF;
        IF H_COUNT=0 AND V_COUNT=4 THEN PXLOUT<="1001"; END IF;

        IF H_COUNT=639 AND V_COUNT=0 THEN PXLOUT<="1111"; END IF;
        IF H_COUNT=638 AND V_COUNT=1 THEN PXLOUT<="1111"; END IF;
        IF H_COUNT=637 AND V_COUNT=2 THEN PXLOUT<="1111"; END IF;
        IF H_COUNT=638 AND V_COUNT=3 THEN PXLOUT<="1111"; END IF;
        IF H_COUNT=639 AND V_COUNT=4 THEN PXLOUT<="1111"; END IF;
    END PROCEDURE;


    PROCEDURE CHECKCHARPAT IS
    BEGIN
       
    END PROCEDURE;


    PROCEDURE DONBTEXT IS   
    VARIABLE CHRSZ:INTEGER RANGE 0 TO 10;
    VARIABLE CHRCOL:INTEGER RANGE 0 TO 15;   
    VARIABLE TXFontline:INTEGER RANGE 0 TO 255;   
    VARIABLE TEXTline:INTEGER RANGE 0 TO v_pixels;       --480
    BEGIN

        CHRCOL:=COLUMN MOD (HORZPXL*8);--0..7 OR 0..15
        LETCOL := CHRCOL/HORZPXL; --0..7
		
        CHRSZ:=8; 
        IF sUCR='0' THEN
          CHRSZ:=10;           
        END IF;

        PRLINES:=integer(ROW/(VERTPXL*CHRSZ));       		        
		IF H_COUNT>H_PIXELS THEN --AFTER VISIBLE PIXELS                                
          TXFontline:= integer((ROW+1)/VERTPXL) MOD CHRSZ; -- 0..9 FOR 10 --ROW+1 CAUSE WE ARE ON THE PREVIOUS ROW AND SETTING UP NEXT ROW
          TEXTline := integer(((ROW+1)/VERTPXL)/CHRSZ);         
          CASE SETUPCHAR IS            
            WHEN 0 TO 2  =>  VIDEOaddr := to_integer(unsigned(NBVIDAD))+TEXTLINE*EL;--to_integer(unsigned(TVEL));                             
                             MEMADDR<= VIDEOaddr;                              
            WHEN 3 =>    SKIPLINE:=FALSE;   
                         MEMADDR<=VIDEOaddr;
                         TEXTCHAR:=to_integer(unsigned(DATAIN));
                         REVF:= sFS='0' AND TEXTCHAR>127;                           
            WHEN 4 =>    ad_i<= std_logic_vector(to_unsigned(TEXTCHAR+(TXFontline*256),ad_i'length));            
                         IF REVF THEN  ad_i<= std_logic_vector(to_unsigned(TEXTCHAR-128+(TXFontline*256),ad_i'length));   END IF;                        
            WHEN 7 =>    PXLTEXTnx<=dout_o;
                         IF REVF THEN  PXLTEXTnx<=dout_o XOR x"FF";  END IF;                        
                         PXLTEXT<=PXLTEXTNX;
                         IF REVF THEN  PXLTEXT<=PXLTEXTNX XOR x"FF";  END IF;                        
            WHEN 8 =>    
                         PXLTEXT<=dout_o; 
                         IF REVF THEN  PXLTEXT<=dout_o XOR x"FF";  END IF;
                         IF S80L='0' THEN
                           NBNEWCHAR(TEXTCHAR);                            
                         END IF;
          END CASE;
	    ELSE  --PRINT VISIBLE PIXELS FOR TEXT
          TXFontline:= integer((ROW/VERTPXL)) MOD CHRSZ; -- 0..9 FOR 10
          TEXTline := integer((ROW/VERTPXL)/CHRSZ);         

          case CHRCOL is    
            WHEN 0  =>  VIDEOaddr:= VIDEOaddr +1;
                        MEMADDR<=VIDEOaddr; 
            WHEN 1 TO 2 => MEMADDR<=VIDEOaddr;
            WHEN 3 =>   
                        MEMADDR<=VIDEOaddr;
                        TEXTCHAR:=to_integer(unsigned(DATAIN));	--GET TEXT CHARACTER 
                        REVF:= sFS='0' AND TEXTCHAR>127;                         
            WHEN 4 =>  ad_i<= std_logic_vector(to_unsigned(TEXTCHAR+(TXFontline*256),ad_i'length));
                       IF REVF THEN  ad_i<= std_logic_vector(to_unsigned(TEXTCHAR-128+(TXFontline*256),ad_i'length));   END IF;                        
            WHEN 6 =>  PXLTEXTnx<=dout_o;
                       IF REVF THEN  PXLTEXTnx<=dout_o XOR x"FF";  END IF;
            WHEN 7 =>  IF HORZPXL=1 THEN  --7 OR 15 
                            PXLTEXT<=PXLTEXTnx;           
                        END IF;
          end case;     
            
            IF (s80L='0' AND CHRCOL=15) OR (s80L='1' AND CHRCOL=0) THEN  --7 OR 15 
                       PXLTEXT  <= PXLTEXTnx;                        
                       NBNEWCHAR(TEXTCHAR);      
            END IF;
        END IF;

        MEMADDR<= VIDEOaddr;
        NXTPXL:=PXLTEXT(7-LETCOL);        

    END PROCEDURE;
    
    PROCEDURE DONBGRAPH  IS
    VARIABLE PXLCOL:INTEGER RANGE 0 TO 15;
    VARIABLE CHGLIN:BOOLEAN;
    VARIABLE LEFTGAP:INTEGER RANGE 0 TO 255;
    BEGIN
         LEFTGAP:=0;
       if s3240='1' then		      
	   --NARROW            
            LEFTGAP:=64;          
	   end if;
      
        PXLCOL := (COLUMN MOD (HORZPXL*8));
        LETCOL := PXLCOL/HORZPXL;
        
        IF S3240='1' AND GRSTLN<4  THEN --READ 1ST BYTE FOR NARROW SCREEN - NO BUFFER ON NARROW
         GRSTLN:=GRSTLN+1;  
           -- BTCNT1:=5;         
           case  GRSTLN is 
               WHEN 1 =>  MEMADDR <=VIDEOaddr+1;--+1;
               WHEN 3 =>  PXLTEXTnx<=DATAIN;
               WHEN 4 =>  PXLTEXT<=PXLTEXTnx;
                          VIDEOADDR:=VIDEOaddr+2;--2;
                          GRSTLN:=VIDEOaddr;
            END CASE;
        END IF;


          IF S3240='0' AND NOT GR1STLN  THEN  --ONLY ONCE HERE FOR WIDE TO REPEAT 1ST LINE
            GR1STLN:=TRUE;            
            GRSTLN:=VIDEOaddr-3;            
          END IF;
          
                
          IF COLUMN=636 THEN
            IF ((ROW+1) MOD VERTPXL=0)  THEN -- EVERY 2 ROWS ONCE MOVE TO NEXT ROW    
               BTCNT1:=0;
               IF S80L='0' THEN     
                GRSTLN:=VIDEOaddr;      --NEXT LINE
               ELSE
                GRSTLN:=VIDEOaddr;      --NEXT LINE
               END IF;
            ELSE    --REPEAT LINE
              IF S3240='1' THEN
                 VIDEOaddr:=GRSTLN-2;
                 GRSTLN:=0;
              ELSE 
                   VIDEOaddr:=GRSTLN;          --new start  SAMEROW        
              END IF;                
            END IF; --AT ROW+1   
            IF S3240='0' THEN
              MEMADDR<=VIDEOADDR;
            END IF;
          END IF; --COL
          IF COLUMN=638 AND S3240='0' THEN        --READ 1ST BYTE OF NEXT LINE (SAME OR NEW)
            PXLTEXTnx<=DATAIN;
          END IF;



        IF (COLUMN>=LEFTGAP) AND (COLUMN<h_real-LEFTGAP) THEN --h_real was h_pixels
          
          case  PXLCOL is           
             WHEN 1=> MEMADDR<= VIDEOaddr;
             WHEN 3=> PXLTEXTnx<=DATAIN;
          END CASE;
          IF PXLCOL=(HORZPXL*8)-1 THEN
            PXLTEXT<=PXLTEXTnx;
            VIDEOaddr:=VIDEOaddr+1;                
           -- BTCNT1:=BTCNT1+1;
          END IF;

          NXTPXL:=PXLTEXT(LETCOL);
          
          ELSE NXTPXL:='0';
        
            
        END IF;
       -- IF BTCNT1=0 THEN
        --   NXTPXL<='1';
        --END IF;
        
        

    END PROCEDURE;

    PROCEDURE PUTBACKPXL IS
    BEGIN
          IF sRV='1' THEN
            PXLOUT <= PXLFORE; 
          ELSE
	        PXLOUT <= PXLBACK; 
          END IF;        
    END;

    PROCEDURE PUTFOREPXL IS
    BEGIN
          IF sRV='1' THEN
            PXLOUT <= PXLBACK; 
          ELSE
	        PXLOUT <= PXLFORE; 
          END IF;
    END;



    procedure donbscreen is         
    begin
        --PRINT BACKGROUND
        PUTBACKPXL;
        MEMBUF<='1';
                


  --8 PIXELS PER BYTE
	    TXFontline<=TXFontline;
		PXLTEXT<=PXLTEXT;
        PXLTEXTnx<=PXLTEXTnx;
        MEMBUF<=MEMBUF;
		MEMADDR<= MEMADDR;	      
       
        EL:=64;
        IF s80L='1' THEN          
          HORZPXL := 1;
          VERTPXL := v_dbl; --1 for 350 vga else 2
          EL:=2*EL;  
        ELSE
          HORZPXL := 2; -- DOUBLE PIXELS
          VERTPXL := v_dbl; --1 for 350 vga else 2
        END IF;


        IF ISGRAPH='1' THEN
            ISTEXT<='0';
            SKIPLINE:=FALSE;
        END IF;


        IF ISTEXT='1' AND NOT NBSCRFIN THEN
          DONBTEXT;
        ELSIF ISGRAPH='1' THEN           
             DONBGRAPH;            
        END IF;

         if  NXTPXL='1' THEN --AND NOT SKIPLINE THEN --PRINT FOREGROUND
            PUTFOREPXL;
		 END IF;     
    
--        IF PRLINES=to_integer(unsigned(TVDEP)) THEN
--            PXLOUT <= "0101";
--        END IF;

--        IF PRLINES=12 THEN
--            PXLOUT <= "1101";
--        END IF;

--        IF PRLINES=11 THEN
--            PXLOUT <= "1011";
--        END IF;
      --  IF COLUMN MOD 16 =0 AND ROW<20 THEN
      --    PXLOUT <= "1011";
      --  END IF;
      --  IF ROW MOD 20=0 THEN
      --    PXLOUT <= "0101";
      --  END IF;
      --  IF ROW MOD 20=19 THEN
       --   PXLOUT <= "0010";
       -- END IF;

        IF H_COUNT>H_PIXELS  THEN SKIPLINE:=FALSE; END IF;

       IF SKIPLINE THEN
         PUTBACKPXL;  
       END IF;

       IF NBSCRFIN  THEN
          --PXLOUT <= "0000";  
          PUTBACKPXL  ;
        END IF;


    --   IF TEXTCHAR=0 THEN
     --    PXLOUT <= "0101";    
     --  END IF;
    --  IF ISGRAPH='1' AND COLUMN<10 THEN
        -- PXLOUT <= "0010";    
    --   END IF;
   --   IF ISTEXT='1' AND COLUMN<10 THEN
    --     PXLOUT <= "0001";    
    --   END IF;
--        IF ROW MOD 20 = 19 THEN 
--            PXLOUT <= "1011";    
--        END IF;
    --    IF ((ROW+1) MOD VERTPXL=0) AND  ISGRAPH='1' AND COLUMN<30 THEN 
       -- IF BTCNT1=2 AND ISGRAPH='1' AND COLUMN<30  THEN
      --      PXLOUT <= "1010";    
      --  END IF;

     --   IF BTCNT1=5 AND ISGRAPH='1' AND COLUMN<2*8  THEN
      --      PXLOUT <= "1110";    
      -- END IF;


--     IF ROW MOD VERTPXL=1 AND COLUMN<30 THEN
--         PXLOUT <= "1001";    
--       END IF;
    end procedure;


--READ NB REGISTERS
    PROCEDURE DOREADREGISTERS IS    
    BEGIN

        MEMBUF<='1';
        MYSTEP:=MYSTEP+1;
        case MYSTEP is
            when 1 =>  MEMADDR<= 92;      --SETUP ADDR REG                       
            when 3 =>  TVRAM(7 DOWNTO  0) <=DATAIN;       --READ ADDR TO REGISTER

            when 4 =>  MEMADDR<= 93;
            when 6 =>  TVRAM(15 DOWNTO  8) <=DATAIN;       --READ ADDR TO REGISTER

            when 7 =>  MEMADDR<= to_integer(unsigned(TVRAM));
            when 9 =>  TVOFFS<= DATAIN;

         --   when 10 =>  MEMADDR<= to_integer(unsigned(TVRAM))+6;   --EL
          --  when 12 =>  TVEL<= DATAIN;   
                        
            
            when 13 =>  MEMADDR<= to_integer(unsigned(TVRAM))+10;      --FRM
            when 15 =>  TVFRM<= DATAIN;

            when 16 =>  MEMADDR<= to_integer(unsigned(TVRAM))+1;       --TVMODE
            when 17 =>  sUCR  <= DATAIN(3); -- 0 = 10 lines , 1= 8 lines per char
                       s80L  <= DATAIN(6); -- 0 = 40 chars 1 = 80 chars
                       s3240 <= DATAIN(2); -- 0 = 320 or 640 pxl, 1=256 or 512 pxl
                       sFS   <= DATAIN(1); -- 0 = 128 nrm chars and 128 rvse field chrs, 1= 256 chars
                       sRV   <= DATAIN(0); -- 0 = white on black , 1 = black on white

            when 18 =>  MEMADDR<= to_integer(unsigned(TVRAM))+4;       --TVDEP
            when 20 =>  TVDEP<= DATAIN;
      
            when 21 =>  MEMADDR<= 36;       --enregmap $24=36 for tv enabled
            when 23 =>  tvena<= DATAIN(2);  --bit 2 is tv enabled or not

            when 26 =>  NBVIDAD<= std_logic_vector(to_unsigned( to_integer(unsigned(TVRAM)) + to_integer(unsigned(TVOFFS)) + to_integer(unsigned(TVFRM))*EL + 5 , NBVIDAD'length ));
            WHEN OTHERS=> MEMBUF<='1';
            
         end case;    


    END PROCEDURE;

--READ FONT TO INTERNAL MEMORY
PROCEDURE READFONT IS
VARIABLE FNTPATADDR:INTEGER RANGE 0 TO 4096;
BEGIN

   --IF ReadFNT='1'  THEN 
     FNTPATADDR:=0;
     IF sUCR='0' THEN
        FNTPATADDR:=4096;
     END IF;
     MEMBUF<='0';
     CASE sprstate IS
       WHEN BRS_IDLE=> BYTCNT:=0;
                      sprstate<=BRS_DataST;
       WHEN BRS_DataST=> MEMADDR<=FNTPATADDR+BYTCNT;
                         ad_i <= Std_logic_vector(to_unsigned(BYTCNT,ad_i'length));        
                         sprstate<=BRS_DataST2;
       WHEN BRS_DataST2=>sprstate<=BRS_DataST3; --JUST WAIT A COUPLE OF TICKS TO STABILIZE MEMADDR AND DATAOUT                         
       WHEN BRS_DataST3=>sprstate<=BRS_DataRD;
                         --wre_i <= '1';
       WHEN BRS_DataRD=> din_i <= datain;   --READ DATA
                         wre_i <= '1';
                         BYTCNT:=BYTCNT+1;
                         sprstate<=BRS_DataSV;
       WHEN BRS_DataSV=> wre_i <= '1';   --WHEN HIGH WRITE IS ENABLED         
                         sprstate<=BRS_DataST;                                                 
     END CASE;

 --  ELSE 
   -- BYTCNT:=0;
   -- sprstate<=BRS_IDLE;
 --  END IF; 
END PROCEDURE;

PROCEDURE READCOLORS IS
BEGIN
      IF READREGISTER=1 THEN
        MEMBUF<='0';
        MEMADDR<=32760;        
      ELSIF READREGISTER=2 THEN
        CONFIGREG<=DATAIN;
      ELSIF READREGISTER=3 THEN
        VIDSET<=CONFIGREG(1 DOWNTO  0);                        
      ELSIF READREGISTER=4 THEN
        MEMADDR<=32761;
      ELSIF READREGISTER=5 THEN
        CONFIGREG<=DATAIN;
      ELSIF READREGISTER=6 THEN
        PXLFORE<=CONFIGREG(3 DOWNTO  0);
        PXLBACK<=CONFIGREG(7 DOWNTO  4);
      END IF;
END;


  BEGIN
    
    clk_i <= Rpixel_clk;  
  

   IF rising_edge(Rpixel_clk)  THEN

	 
	 
      --counters
      IF(h_count < h_period - 1) THEN    --horizontal counter (pixels)
        h_count := h_count + 1;
      ELSE
        h_count := 0;
        IF(v_count < v_period - 1) THEN  --veritcal counter (rows)
          v_count := v_count + 1;
        ELSE
          v_count := 0;
        END IF;
      END IF;

      --horizontal sync signal
      IF(h_count < h_pixels + h_fp OR h_count >= h_pixels + h_fp + h_pulse) THEN
        h_sync <= NOT h_pol;    --deassert horiztonal sync pulse
      ELSE
        h_sync <= h_pol;        --assert horiztonal sync pulse
      END IF;
      
      --vertical sync signal
      IF(v_count < v_pixels + v_fp OR v_count >= v_pixels + v_fp + v_pulse) THEN
        v_sync <= NOT v_pol;    --deassert vertical sync pulse
      ELSE
        v_sync <= v_pol;        --assert vertical sync pulse
      END IF;
      
      --set pixel coordinates
      IF(h_count < h_pixels) THEN  --horiztonal display time
        column := h_count;           --set horiztonal pixel coordinate	     
		ELSE
          COLUMN :=0;		
      END IF;
      IF(v_count < v_pixels) THEN  --vertical display time
        row := v_count;              --set vertical pixel coordinate
		ELSE
		  ROW:=0;
      END IF;

      --set display enable output
      IF(h_count < h_pixels AND v_count < v_pixels) THEN  --display time
        disp_ena <= '1';                                    --enable display
      ELSE                                                --blanking time
        disp_ena <= '0';                                    --disable display
      END IF;
		   
---==========================================================================================

      PXLBACK<=PXLBACK;
	  PXLFORE<=PXLFORE;
      MEMADDR<=MEMADDR;
      MEMBUF<=MEMBUF;
      tvena<= tvena;


      wre_i<='0';
      reset_i <='0';
      ce_i <= '1';  
      oce_i <= '0';  



      --READ FONT TO INTERNAL RAM
      IF v_count>v_pixels+2 and v_count<v_pixels+20 THEN --
          MEMBUF<='0'; --READ FROM VIDEO BUFFER LOW FNT ADDR IS 0 AND 4096
          READFONT;
      ELSE
          sprstate<=BRS_IDLE;
      END IF;
      
      --READ DEFAULT COLORS FOR BACK AND FORE  
      IF v_count=v_period-5 THEN	
	    READREGISTER<=READREGISTER+1; 
        MEMBUF<='0';               
        READCOLORS;       
	  ELSE
	    READREGISTER<=0;        
	  END IF; 

      --for NB Read the registers INIT FRAME VARS
      if v_count=v_period-4 then 
         MEMBUF<='1';    
         DOREADREGISTERS;            
      ELSE
        MYSTEP:=0;
      end if;
     
        
      IF v_count=v_period-2 AND H_COUNT<2 then --NEW FRAME INITIALIZED                
         NBNEWCHAR(255);
         NBSCRFIN:=FALSE; 
         ISGRAPH<='0';  
         ISTEXT<='1';
         PRLINES:=0;
      END IF;
      
      IF (v_count<v_pixels OR V_COUNT=V_PERIOD-1) and H_COUNT>H_PIXELS AND H_COUNT<H_PIXELS+9 THEN
       SETUPCHAR<=SETUPCHAR+1;
      ELSE 
       SETUPCHAR<=0;
      END IF;     


     -- ******** SCREEN DISPLAY **********
                    --or (v_count=v_period-1)
     IF ((v_count<v_real) or (v_count=v_period-1))  AND tvena='1'  THEN  --v-real was V_PIXELS
        donbscreen;               
     END IF; --SCREEN DISPLAY END

    END IF; --RISINGEDGE CLOCK

    scrst <= '0';
  END PROCESS;

  Rpixel_clk<=pixel_clk;
  addrout <= MEMBUF & std_logic_vector(to_unsigned(memaddr, addrout'length -1 ));
  RGBI <= "0000" WHEN DISp_ena = '0' ELSE
            PXLOUT;
			  
END behavior;

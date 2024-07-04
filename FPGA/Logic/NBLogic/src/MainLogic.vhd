
LIBRARY ieee; 
USE ieee.std_logic_1164.all; 
USE ieee.numeric_std.all; 
use ieee.std_logic_unsigned.all;


ENTITY NBLOGIC IS  
  PORT(
    cpuclkO :   OUT     STD_LOGIC;  --main cpu clock
    sndclkO :   OUT     STD_LOGIC;  --sound clock up to 4MHz def 2.5MHz
    nRESET  :   IN     STD_LOGIC;  --main RESET
   -- nbclk20 :   OUT     STD_LOGIC;  --50hz   20ms clock
   -- nbclk13 :   OUT     STD_LOGIC;  --76,923 13ms cop clock
    DATAio	:   INOUT  STD_LOGIC_VECTOR(7 DOWNTO 0);
	ADDRin  :   IN      STD_LOGIC_VECTOR(7 DOWNTO 0); -- A0 - A7 for address decoding
    nMREQin :   IN      STD_LOGIC; 
    nIORQin :   IN      STD_LOGIC; 
    nRDin   :   IN      STD_LOGIC; 
    nWRin   :   IN      STD_LOGIC; 
    nWRout  :   OUT     STD_LOGIC;  
    nINTout :   OUT     STD_LOGIC;  
    A13     :   IN      STD_LOGIC; 
    A14     :   IN      STD_LOGIC; 
    A15     :   IN      STD_LOGIC; 
    EA13	:  OUT STD_LOGIC;
    EA14	:  OUT STD_LOGIC;
    EA15	:  OUT STD_LOGIC;
    nCE0	:  OUT STD_LOGIC;
    nCE1	:  OUT STD_LOGIC;
    nCE2	:  OUT STD_LOGIC;
    DIROUT  :  OUT STD_LOGIC;  --voltage chip direction select
    dvSERout:  OUT STD_LOGIC;  --serial pin
    dvSTRout:  OUT STD_LOGIC;  --storage pin
    dvKBout	:  OUT STD_LOGIC;  --keyboard pin
    dvKBin	:  IN STD_LOGIC;  --keyboard int input
    dvI2Cout:  OUT STD_LOGIC;  --i2c pin
    dvSNDout:  OUT STD_LOGIC;  --sound pin
    btn1_n  :   IN STD_LOGIC;  
    TSTVLT  :  OUT STD_LOGIC;  
    TSTOUT  :  OUT STD_LOGIC;
    TSTOUT2 :  OUT STD_LOGIC;
    TSTOUT3 :  OUT STD_LOGIC
    
	 );
END NBLOGIC;

ARCHITECTURE behavior OF NBLOGIC IS

--Constant KBPORT:std_logic_vector(8-1 downto 0) :=x"46";--70;
Constant KBPORT:std_logic_vector(8-1 downto 0) :=x"48";--72;
Constant i2cPORT:std_logic_vector(8-1 downto 0) :=x"70";--112;
Constant i2cPORT1:std_logic_vector(8-1 downto 0) :=x"71";--113;
Constant i2cPORT2:std_logic_vector(8-1 downto 0) :=x"72";--114;
Constant i2cPORT3:std_logic_vector(8-1 downto 0) :=x"73";--115;
Constant RS232PORT:std_logic_vector(8-1 downto 0) :=x"18";--24;
Constant RS232PORT1:std_logic_vector(8-1 downto 0) :=x"19";--25;
Constant RS232PORT2:std_logic_vector(8-1 downto 0) :=x"1A";--26;
Constant RS232PORT3:std_logic_vector(8-1 downto 0) :=x"1B";--27;
Constant RS232PORT4:std_logic_vector(8-1 downto 0) :=x"1C";--28;
Constant RS232PORT5:std_logic_vector(8-1 downto 0) :=x"1D";--29;
Constant RS232PORT6:std_logic_vector(8-1 downto 0) :=x"1E";--30;
Constant RS232PORT7:std_logic_vector(8-1 downto 0) :=x"1F";--31;
Constant OLRS232PORT:std_logic_vector(8-1 downto 0) :=x"20";--24;
Constant OLRS232PORT1:std_logic_vector(8-1 downto 0) :=x"21";--25;
Constant OLRS232PORT2:std_logic_vector(8-1 downto 0) :=x"22";--26;
Constant OLRS232PORT3:std_logic_vector(8-1 downto 0) :=x"23";--27;
Constant OLRS232PORT4:std_logic_vector(8-1 downto 0) :=x"24";--28;
Constant OLRS232PORT5:std_logic_vector(8-1 downto 0) :=x"25";--29;
Constant OLRS232PORT6:std_logic_vector(8-1 downto 0) :=x"26";--30;
Constant OLRS232PORT7:std_logic_vector(8-1 downto 0) :=x"27";--31;
Constant STORPORT:std_logic_vector(8-1 downto 0) :=x"30";--48;
Constant STORPORT1:std_logic_vector(8-1 downto 0) :=x"31";--49;
Constant SOUNDPORT:std_logic_vector(8-1 downto 0) :=x"38";--56;




signal DATAin       :std_logic_vector(8-1 downto 0);
SIGNAL DATAout      : std_logic_vector(8-1 downto 0); --byte to be read by the CPU in cmd
SIGNAL snd_clk      : STD_LOGIC;
SIGNAL cpu_clk      : STD_LOGIC;
SIGNAL NB20_clk     : STD_LOGIC;
SIGNAL NB13_clk     : STD_LOGIC;
SIGNAL sEA          :  std_logic_vector(21-1 downto 13);
SIGNAL nsCE0		:  STD_LOGIC:='1';
SIGNAL nsCE1		:  STD_LOGIC:='1';
SIGNAL nsCE2		:  STD_LOGIC:='1';
SIGNAL nsCE3		:  STD_LOGIC:='1';
SIGNAL nsCE4		:  STD_LOGIC:='1';
SIGNAL nINTMMU      :  STD_LOGIC:='1';
SIGNAL outputData   :  STD_LOGIC:='1';  --if we should ouput data to databus
SIGNAL nReset2      :  STD_LOGIC:='1';

SIGNAL FRMinton     :  STD_LOGIC:='1';
--INTERRUPTS
SIGNAL FRMint       :  STD_LOGIC:='1';  --drive the main INT z80 signal
SIGNAL COPint       :  STD_LOGIC:='1';  --drive the main INT z80 signal
--device enable signals
SIGNAL KBce         :  STD_LOGIC:='1';  --Keyboard device
SIGNAL RS232ce      :  STD_LOGIC:='1';  --RS232 Serial device
SIGNAL RS232ce2     :  STD_LOGIC:='1';  --RS232 Serial device
SIGNAL I2Cce        :  STD_LOGIC:='1';  --I2C devices
SIGNAL STRce        :  STD_LOGIC:='1';  --Storage device
SIGNAL SNDce        :  STD_LOGIC:='1';  --Storage device
--Device Interrupts
SIGNAL kbintIN      :  STD_LOGIC:='1';  --Keyboard Interrupt set when '0'


--REGISTERS
signal COPCTL :  std_logic_vector(8-1 downto 0);    
signal COPCTL2 :  std_logic_vector(8-1 downto 0);  --COP STATUS
signal COPCMD :  std_logic_vector(8-1 downto 0);    --COP COMMAND
signal ENABLEREG :  std_logic_vector(8-1 downto 0);
Signal CLRCOP:std_logic:='1';                      --
Signal CLRCOPnxt:std_logic:='1'; 
Signal CLRFRM:std_logic:='1';                      --
Signal INTEN:std_logic:='1';    
Signal NBEN:std_logic:='1';                       --
signal COPcount : std_logic_vector(4 DOWNTO 0);      -- counter to receive bytes for cop (lcd)



Signal KB_Stop:std_logic:='0';                      --STOP KEY PRESSED FROM KEYB
Signal KBint:std_logic:='1';                      --
signal KBcount : std_logic_vector(2 DOWNTO 0);      -- counter to check the keyboard
Signal CLRKBD:std_logic:='1';                      --
Signal CLRKBDnxt:std_logic:='1';                      --CLRKBDnxt
SIGNAL rtvon:std_logic:='0';
SIGNAL CTS:std_logic:='0';
SIGNAL RTS:std_logic:='0';
SIGNAL TX:std_logic:='0';

SIGNAL TS1:std_logic:='0';
SIGNAL TS2:std_logic:='0';

--			 CTS:in STD_LOGIC; -- v24 cts
--			 RTS:out STD_LOGIC; -- v24 rts
--			 RX:in STD_LOGIC; -- v24 rx
--			 TX:out STD_LOGIC; -- v24 tx
SIGNAL BNUM:std_logic_vector(3-1 downto 0);

--component sound_osc
--    port (
--        oscout: out std_logic
--    );
--end component;

component CPUOSC
    port (
        oscout: out std_logic
    );
end component;

component Clock_Divider
    port (
       clk: in std_logic;
       reset: in std_logic;
       FREQCNTR: integer;
       clock_out: out std_logic
    );
end component;

component MMU2
PORT
	(	   
		A13 		:  IN  STD_LOGIC;
		A14 		:  IN  STD_LOGIC;
		A15 		:  IN  STD_LOGIC;
		nMREQ 	    :  IN  STD_LOGIC;
		nINTMMU 	:  IN  STD_LOGIC;
		nWR 		:  IN  STD_LOGIC;
		nRESET 	    :  IN  STD_LOGIC;
		DATA 		:  IN  std_logic_vector(8-1 downto 0) ;  -- Data bus from CPU (Z80???)
        EA 		    :  OUT  std_logic_vector(21-1 downto 13);   -- Extended Address bus to mem chips	
    --ROM/RAM CHIPS ENABLE SIGNALS
        nCE0		:  OUT STD_LOGIC;
        nCE1		:  OUT STD_LOGIC;
        nCE2		:  OUT STD_LOGIC;
        nCE3		:  OUT STD_LOGIC;
        nCE4		:  OUT STD_LOGIC;
        MMUTST		:  OUT STD_LOGIC;
        MMUTST2		:  OUT STD_LOGIC
	);
end component;


BEGIN

--CLKSND: sound_osc
--    port map (
--        oscout => snd_clk
--    );


CLKCPU: CPUOSC
    port map (
        oscout => cpu_clk
    );

CLK20: Clock_Divider
    port map (
        clk => cpu_clk,
        reset => '1',
        FREQCNTR => 120000,     --200000 FOR 20MHZ MAIN CLOCK 20MS
        clock_out => NB20_clk   --146000 for 10Mhz
                                --120000   for 12Mhz
    );                          --40000 for 4mhz,  80000 for 8mhz

CLK13: Clock_Divider
    port map (
        clk => cpu_clk,
        reset => '1',
        FREQCNTR => 78000,     --130000 FOR 20MHZ MAIN CLOCK 13MS
        clock_out => NB13_clk  --95000 for 10MHz
                               --78000 for 12Mhz
    );                          --26000 for 4 mhz, 52000 for 8mhz

CLK25: Clock_Divider
    port map (
        clk => cpu_clk,
        reset => '1',
        FREQCNTR => 3,     --3 for 2.5MHz when main clk is 15mhz
        clock_out => snd_clk
    );


NBMMU: MMU2
    port map (
		A13 		=>  A13,
		A14 		=>  A14,
		A15 		=>  A15,
		nMREQ 	    =>  nMREQin,
		nINTMMU 	=>  nINTMMU,
		nWR 		=>  nWRin,
		nRESET 	    => nReset2,
		DATA 		=>  DATAin,   -- Data bus from CPU (Z80???)
        EA 		    =>  sEA, -- Extended Address bus to mem chips
	--ROM/RAM CHIPS ENABLE SIGNALS
        nCE0		=> nsCE0,
        nCE1		=> nsCE1,
        nCE2		=> nsCE2,
        nCE3		=> nsCE3,
        nCE4		=> nsCE4,
        MMUTST      => TS1,
        MMUTST2     => TS2
    );

    process (NB20_clk,CLRFRM,FRMinton) -- in out on port 6 clears clock int
	begin
 	  if  (CLRFRM='0') or FRMinton='1' then
		  FRMint<='1';
     elsif falling_edge(NB20_clk) then
		 if  FRMinton='0' then
		  FRMint<='0';		 
	     end if;
	  end if;
	end process;

	process (NB13_clk,CLRCOP)   --in from port 20=$14 clears cop int
	begin
                
      IF CLRCOP='0' THEN
          COPint<='1';
      ELSIF CLRKBD='0' THEN
          KBint<='1';		   
	  elsif falling_edge(NB13_clk) then
		  COPint<='0';		          
          KBcount <= KBcount + 1; 	
          IF KBcount="000" THEN 
              KBint<='0';
          END IF;
	  end if;
	end process;


PROCESS (cpu_clk)

BEGIN
  IF nRESET='0' THEN
       INTEN <='1';
       NBEN<='1'; --NB IS ENABLED FALSE       
  ELSIF rising_edge(cpu_clk) then
    IF nIORQin='0' and nWRin='0' AND ADDRin=x"E0"  THEN     --OUT E0,00b TO ENABLE INTERRUPT SERVICE & SET NBEN
       INTEN <=DATAIN(0);
       NBEN <= DATAIN(1);
    END IF;

   IF nIORQin='1' THEN
    CLRCOP<=CLRCOPnxt;
    CLRKBD<=CLRKBDnxt;--DELAY ONE CLOCK
   END IF;
   CLRCOPnxt<='1';
   IF nIORQin='0' and nWRin='0' AND  ADDRin=x"06" AND DATAin=x"D0" THEN -- COP INT ACKN
      CLRCOPnxt<='0';
   END IF;

   CLRKBDnxt <= '1';
   IF nIORQin='0' and nRDin='0' AND  ADDRin=x"06" THEN -- in 6 clrkbd
      CLRKBDnxt<='0';
   END IF;

   IF nIORQin='0' and nWRin='0'  AND  ADDRin=x"07" THEN  --WHEN OUT 7 ENREG
     NBEN<='0';
   END IF;
  end if;     --RISING

   




END PROCESS;

cpuclkO <= cpu_clk;
--sndclkO <= '1';--snd_clk; 
--nbclk20 <= NB20_clk;
--nbclk13 <= NB13_clk;
--commIN <= '0' WHEN nIORQin='0' and nRDin='0' ELSE '1';      --cpu issues an in when 0
--commOUT <= '0' WHEN nIORQin='0' and nWRin='0' ELSE '1';     --cpu issues an out when 0
nCE0 <= nsCE0;  --32KB rom chip
nCE1 <= nsCE1;  --32kb ram chip
nCE2 <= nsCE2;  --64kb ram chip video ram
EA13 <= sEA(13);
EA14 <= sEA(14);
EA15 <= sEA(15);
--nWRout <= nWRin;
nWRout <= nWRin WHEN NBEN='1'-- OR nMREQin='1'
        ELSE  '1' WHEN sEA(16 DOWNTO 13)=x"06" OR sEA(16 DOWNTO 13)=x"05" OR sEA(16 DOWNTO 13)=x"04" OR sEA(16 DOWNTO 13)=x"09"--CHECK IF PAGE IS ROM
        ELSE nWRin;
nReset2 <= nReset;


nINTMMU <= '0' when nIORQin='0' and nWRin='0' and ADDRin="00000000" and INTEN='1' --out 0 is MMU Dev , INTEN SAFE ON NB NO PAGING
        else '1'; 

nINTout <= FRMint and COPINT WHEN INTEN='0' ELSE '1';
--nINTout <= '1';

DIROUT <= outputData;--'1'; '1'=INPUT DATA FOR 74LVC4245 DIRECTION
outputData <= '0' WHEN nIORQin='0' and nRDin='0' AND (ADDRin=x"06" OR ADDRin=x"14"  OR ADDRin=x"03" OR ADDRin=x"16") -- WE GIVE DATA on ports 6,20,22 ,72(KB),3
	  ELSE '1';                                                                     --OR ADDRin=KBPORT
DATAio<=DATAout when outputData='0' else (OTHERS=>'Z');
DATAin<=DATAio; 

--outputData <= '1';
    -- KBInt<='0' WHEN KBcount="0000"
    --   ELSE '1' WHEN CLRKBD='0' -- WHEN READ
     --  ELSE KBInt;

 
--out 7 EnableReg:=Value;
	ENABLEREG<=DATAin WHEN nIORQin='0' and nWRin='0' AND (ADDRin=x"07" )
	  ELSE ENABLEREG;	
	FRMinton <= ENABLEREG(0);   --0 enables frame frequency interrupts
	rtvon  <=  ENABLEREG(2);    --1 enables display
	RTS <=  ENABLEREG(4);       --software rs232
	TX <=  ENABLEREG(5);        --software rs232


    
  
	  --out 6 cop control
    COPcount<= "00000" when nIORQin='0' and nWRin='0' and  (ADDRin=x"06") AND COPCMD/=x"A0" AND COPCMD/=x"B0"
            ELSE COPcount+1 WHEN nIORQin='0' and nWRin='0' and  (ADDRin=x"06") and (COPCMD=x"A0" OR COPCMD=x"B0")
            ELSE COPCOUNT;
            
    COPCMD<=DATAin WHEN nIORQin='0' and nWRin='0' and  (ADDRin=x"06") and COPcount=0
        ELSE COPCMD;
--CASSCOM	EQU 080H  ;COMMANDS
--PASSCOM EQU 090H
--DISPCOM EQU 0A0H  SENDS 18 BYTES --we disable the send
--TIMCOM  EQU 0B0H  SENDS 6 BYTES --we disable the send
--PDNCOM  EQU 0C0H
--NULLCOM EQU 0D0H
--RESCOM  EQU 0F0H

	COPCTL<=DATAin WHEN nIORQin='0' and nWRin='0' and  (ADDRin=x"06") and COPCMD/=x"A0"
		ELSE COPCTL;
	--COPCTL2<= 		  "00110"&KB_Stop&"00" WHEN nIORQin='0' and nRDin='0' AND  ADDRin=x"06" AND KBint='0' --3X AND COP80='0' --KEYB iNTERRUPT IN 6
	--			ELSE  "00000"&KB_Stop&"00" WHEN nIORQin='0' and nRDin='0' AND  ADDRin=x"06" AND KBInt='1'  AND COPCMD=x"80"	 --0X  0 at bits 4-7 means regint IN 6
	--			ELSE  "00000"&KB_Stop&"00" WHEN nIORQin='0' and nRDin='0' AND  ADDRin=x"06" AND KBInt='1'  AND COPCMD/=x"80"
	--			ELSE  "00000000";
    
    -- kbint is for rs232 keyboard input fires every several ms low enabled
    -- kbintIN is the real keyboard input interrupt low enabled
    -- BITS 4-6 SELECTS REGINT,CASSER,CASSIN,KBD INTERRUPT
    -- WHEN BITS 4-6 IS 000=REGINT, 001=CASSERR, 010=CASSIN, 011=KBD, 100=CASSOUT
	COPCTL2<= 		  "00111"&KB_Stop&"00" WHEN nIORQin='0' and nRDin='0' AND  ADDRin=x"06" AND kbintIN='1' --3X AND COP80='0' --KEYB iNTERRUPT IN 6 ==3X at bits 4-7 mean kbdint on IN 6
                ELSE  "00110"&KB_Stop&"00" WHEN nIORQin='0' and nRDin='0' AND  ADDRin=x"06" AND KBint='0' --3X AND COP80='0' --KEYB iNTERRUPT IN 6 ==3X at bits 4-7 mean kbdint on IN 6  
   			    ELSE  "00000"&KB_Stop&"00" WHEN nIORQin='0' and nRDin='0' AND  ADDRin=x"06" AND KBInt='1'  AND COPCMD=x"80"	 --0X  0 at bits 4-7 means regint IN 6
				ELSE  "00000"&KB_Stop&"00" WHEN nIORQin='0' and nRDin='0' AND  ADDRin=x"06" AND KBInt='1'  AND COPCMD/=x"80" -- all others 1x 2x 4x are for cassette control
				ELSE  "00000000";


--BRKKEY	EQU 2
--BRKOK	EQU 3	;IF RES ALLOWS BREAK
--TIMER0	EQU 0
--CBRK	EQU 1	;BREAK KEY BIT
  
   DATAout <= 
			 COPCTL2 WHEN nIORQin='0' and nRDin='0'  AND  ADDRin=x"06"
	  --ELSE mydata WHEN IRQ='0' AND RDin='0' AND ADDRin=KBPORT -- from ps/2 data	
	  ELSE COPint&'1'&FRMint&"11101" WHEN nIORQin='0' and nRDin='0' AND  ADDRin=x"14" --IN 20 STATUS REGISTER 9/9/2016
--COPINTBAR	EQU 7		;COP status bit
--CLKINTBAR	EQU 5		;Frame Clock status bit
--ACINTBAR	EQU 6       ;1 NOT INT
--UPTINTBAR	EQU 4       ;1 NOT INT
--POWTEST	EQU 1       ;0 FOR POWER
--EXTEST	EQU 0       ;1 MEANS 24

																				--bit 1 is pwrup should be 0 when we are ready
    -- ELSE COPCTL WHEN nIORQin='0' and nRDin='0' AND ADDRin=x"03"	
	 -- ELSE "101000"&CTS&RX WHEN commIN='0' AND ADDRin=x"16"	--IN 22 GET V24 SIGNALS (ZEROES NOT USED)
	  ELSE DATAout;--"00000000";
    

   CLRFRM<='0' WHEN nIORQin='0'  AND  ADDRin=x"04" 
       ELSE '1';
   

--DEVICE ENABLE SIGNALS
    --ATLAS RS232 PORT
 	RS232ce<='0' WHEN nIORQin='0' AND (ADDRin=RS232PORT or ADDRin=RS232PORT1 or ADDRin=RS232PORT2 or ADDRin=RS232PORT3
									 or ADDRin=RS232PORT4 or ADDRin=RS232PORT5 or ADDRin=RS232PORT6 or ADDRin=RS232PORT7)
     ELSE '1';
    --NB RS232 PORT
    RS232ce2<='0' WHEN nIORQin='0' AND (ADDRin=OLRS232PORT or ADDRin=OLRS232PORT1 or ADDRin=OLRS232PORT2 or ADDRin=OLRS232PORT3
									 or ADDRin=OLRS232PORT4 or ADDRin=OLRS232PORT5 or ADDRin=OLRS232PORT6 or ADDRin=OLRS232PORT7)
     ELSE '1';
    KBce<='0' WHEN nIORQin='0' AND (ADDRin=KBPORT) ELSE '1';
  
    I2Cce<='0' WHEN nIORQin='0' AND (ADDRin=i2cPORT or ADDRin=i2cPORT1 or ADDRin=i2cPORT2 or ADDRin=i2cPORT3) ELSE '1';
    STRce<='0' WHEN nIORQin='0' AND (ADDRin=STORPORT or ADDRin=STORPORT1 ) ELSE '1';
    SNDce<='0' WHEN nIORQin='0' AND (ADDRin=SOUNDPORT) ELSE '1';
  
    dvSERout <= RS232ce AND RS232ce2 ;--RS232ce --to RS232 Device
    dvSTRout <= STRce; --to from storage device;
    dvKBout  <= KBce; --to PS/2 Keyboard Device
    dvI2Cout <= I2Cce; --to i2c Devices 
    dvSNDout <= SNDce; --to sound device
    --kbintIN <='0' when dvKBin='1' and copint='0' and frmint='0' else kbintIN; -- 1 means a key is available on COP
    kbintIN <=  dvKBin ;--when copint='0' and frmint='0' else kbintIN; -- 1 means a key is available on COP


--TEST SIGNALS
 BNUM <= A15 & A14 & A13;
   --TSTOUT <='0' WHEN COPint='0' AND FRMINT='0' ELSE '1';
    --TSTOUT <= '0' WHEN nIORQin='0' and nWRin='0' AND  ADDRin=x"09" ELSE '1';
   -- TSTOUT <= '0' WHEN nIORQin='0'  AND  (ADDRin=x"06" OR ADDRin=x"14" )  ELSE '1';--'0' WHEN nIORQin='0' AND ( ADDRin=x"14" OR ADDRin=x"04"  ) ELSE '1' ;--OR ADDRin=x"07")   ELSE '1';  
                --'0' WHEN nMREQin='0' AND A15='1'  ELSE '1';
   -- TSTOUT <= '0' WHEN A15='1' AND NMREQIN='0' ELSE '1';
    --TSTOUT <= KBce WHEN nRDin='0' ELSE '1'; -- ONLY FOR IN DVKEYB
   
    TSTVLT <= NB13_clk;
    TSTOUT2 <= dvKBin;--not working on my board
    
    TSTOUT3 <= NB20_clk;
    sndclkO <= '1';--KBce WHEN nRDin='0' ELSE '1'; -- ONLY FOR IN DVKEYB
    --...
    TSTOUT <= '0';--kbintIn; --not working on my board
    --TSTOUT2 <= I2Cce;
   -- TSTOUT3 <= nRESET;

END behavior;

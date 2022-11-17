
LIBRARY ieee; 
USE ieee.std_logic_1164.all; 
USE ieee.numeric_std.all; 


ENTITY NBLOGIC IS  
  PORT(
    cpuclkO :   OUT     STD_LOGIC;  --main cpu clock
   -- nbclk20 :   OUT     STD_LOGIC;  --50hz   20ms clock
   -- nbclk13 :   OUT     STD_LOGIC;  --76,923 13ms cop clock
    DATAio	:   INOUT   STD_LOGIC_VECTOR(7 DOWNTO 0);
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
    dvSERout:  OUT STD_LOGIC;
    --dvKBout	:  OUT STD_LOGIC
    dvSTRout :  OUT STD_LOGIC
   -- dvI2Cout:  OUT STD_LOGIC
	 );
END NBLOGIC;

ARCHITECTURE behavior OF NBLOGIC IS

Constant KBPORT:std_logic_vector(8-1 downto 0) :=x"46";--70;
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
Constant STORPORT:std_logic_vector(8-1 downto 0) :=x"30";--48;
Constant STORPORT1:std_logic_vector(8-1 downto 0) :=x"31";--49;




signal DATAin       :std_logic_vector(8-1 downto 0);
SIGNAL DATAout      : std_logic_vector(8-1 downto 0); --byte to be read by the CPU in cmd
SIGNAL cpu_clk      : STD_LOGIC;
SIGNAL NB20_clk     : STD_LOGIC;
SIGNAL NB13_clk     : STD_LOGIC;
SIGNAL commIN       : STD_LOGIC;
SIGNAL commOUT      : STD_LOGIC;
SIGNAL EA           : std_logic_vector(21-1 downto 13);
SIGNAL nsCE0		:  STD_LOGIC:='1';
SIGNAL nsCE1		:  STD_LOGIC:='1';
SIGNAL nsCE2		:  STD_LOGIC:='1';
SIGNAL nsCE3		:  STD_LOGIC:='1';
SIGNAL nsCE4		:  STD_LOGIC:='1';
SIGNAL nINTMMU      :  STD_LOGIC:='1';
SIGNAL outputData   :  STD_LOGIC:='1';  --if we should ouput data to databus

SIGNAL FRMinton     :  STD_LOGIC:='1';
--INTERRUPTS
SIGNAL FRMint       :  STD_LOGIC:='1';  --drive the main INT z80 signal
SIGNAL COPint       :  STD_LOGIC:='1';  --drive the main INT z80 signal
--device enable signals
SIGNAL KBce         :  STD_LOGIC:='1';  --Keyboard device
SIGNAL RS232ce      :  STD_LOGIC:='1';  --RS232 Serial device
SIGNAL I2Cce        :  STD_LOGIC:='1';  --I2C devices
SIGNAL STRce       :  STD_LOGIC:='1';  --Storage device


--REGISTERS
signal COPCTL :  std_logic_vector(8-1 downto 0);
signal COPCTL2 :  std_logic_vector(8-1 downto 0);
signal ENABLEREG :  std_logic_vector(8-1 downto 0);

Signal KB_Stop:std_logic:='0';                      --STOP KEY PRESSED FROM KEYB
Signal KBint:std_logic:='1';                      --
SIGNAL rtvon:std_logic:='0';
SIGNAL CTS:std_logic:='0';
SIGNAL RTS:std_logic:='0';
SIGNAL TX:std_logic:='0';

--			 CTS:in STD_LOGIC; -- v24 cts
--			 RTS:out STD_LOGIC; -- v24 rts
--			 RX:in STD_LOGIC; -- v24 rx
--			 TX:out STD_LOGIC; -- v24 tx




component CPU_OSC
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
        nCE4		:  OUT STD_LOGIC
	);
end component;


BEGIN

CLKCPU: CPU_OSC
    port map (
        oscout => cpu_clk
    );

CLK20: Clock_Divider
    port map (
        clk => cpu_clk,
        reset => '1',
        FREQCNTR => 100000,     --200000 FOR 20MHZ MAIN CLOCK 20MS
        clock_out => NB20_clk
    );

CLK13: Clock_Divider
    port map (
        clk => cpu_clk,
        reset => '1',
        FREQCNTR => 65000,     --130000 FOR 20MHZ MAIN CLOCK 13MS
        clock_out => NB13_clk
    );

NBMMU: MMU2
    port map (
		A13 		=>  A13,
		A14 		=>  A14,
		A15 		=>  A15,
		nMREQ 	    =>  nMREQin,
		nINTMMU 	=>  nINTMMU,
		nWR 		=>  nWRin,
		nRESET 	    => '1',
		DATA 		=>  DATAio,   -- Data bus from CPU (Z80???)
        EA 		    =>  EA, -- Extended Address bus to mem chips
	--ROM/RAM CHIPS ENABLE SIGNALS
        nCE0		=> nsCE0,
        nCE1		=> nsCE1,
        nCE2		=> nsCE2,
        nCE3		=> nsCE3,
        nCE4		=> nsCE4
    );

    process (NB20_clk,cpu_clk,commIN,commOUT,ADDRin,FRMinton) -- in out on port 6 clears clock int
	begin
 	  if  (cpu_clk='1' and (commIN='0' or commOUT='0') and  ADDRin=x"04" ) or FRMinton='1' then
		  FRMint<='1';
     elsif falling_edge(NB20_clk) then
		 --if  FRMinton='0' then
		  FRMint<='0';		 
	    --end if;
	  end if;
	end process;

	process (NB13_clk,cpu_clk,commIN,ADDRin)   --in from port 20=$14 clears cop int
	begin
	  if  cpu_clk='1' and commIN='0' and  ADDRin=x"14"  then
		  COPint<='1';		  
	  elsif falling_edge(NB13_clk) then
		  COPint<='0';		 	    
	  end if;
	end process;


PROCESS (cpu_clk)

BEGIN


END PROCESS;

cpuclkO <= cpu_clk;
--nbclk20 <= NB20_clk;
--nbclk13 <= NB13_clk;
commIN <= '0' WHEN nIORQin='0' and nRDin='0' ELSE '1';      --cpu issues an in when 0
commOUT <= '0' WHEN nIORQin='0' and nWRin='0' ELSE '1';     --cpu issues an out when 0
nCE0 <= nsCE0;  --32KB rom chip
nCE1 <= nsCE1;  --32kb ram chip
nCE2 <= nsCE2;  --64kb ram chip video ram
EA13 <= EA(13);
EA14 <= EA(14);
EA15 <= EA(15);
nWRout <= nWRin;


nINTMMU <= '0' when commIN='0' and ADDRin="00000000" --out 0 is MMU Dev
        else '1'; 

--nINTout <= FRMint and COPINT;

outputData <= '0' WHEN commIN='0' AND (ADDRin=x"06" OR ADDRin=x"14" OR ADDRin=KBPORT OR ADDRin=x"03" OR ADDRin=x"16") -- WE GIVE DATA on ports 6,20,22
	  ELSE '1';
DATAio<=DATAout when outputData='0' else (others=>'Z');
DATAin<=DATAio; 


--out 7 EnableReg:=Value;
	ENABLEREG<=DATAin WHEN commOUT='0' AND (ADDRin=x"07" )
	  ELSE ENABLEREG;	
	FRMinton <= ENABLEREG(0);   --0 enables frame frequency interrupts
	rtvon  <=  ENABLEREG(2);    --1 enables display
	RTS <=  ENABLEREG(4);       --software rs232
	TX <=  ENABLEREG(5);        --software rs232

	  --out 6 cop control
	COPCTL<=DATAin WHEN commOUT='0' and  (ADDRin=x"06")
		ELSE COPCTL;
	COPCTL2<= 		  "00110"&KB_Stop&"00" WHEN commIN='0' AND  ADDRin=x"06" AND KBint='0' -- AND COP80='0' --KEYB iNTERRUPT IN 6
				ELSE  "00000"&KB_Stop&"01" WHEN commIN='0' AND  ADDRin=x"06" AND KBInt='1'  AND COPCTL=x"80"		--0 at bits 4-7 means regint IN 6
				ELSE  "00000"&KB_Stop&"00" WHEN commIN='0' AND  ADDRin=x"06" AND KBInt='1'  AND COPCTL/=x"80"
				ELSE  COPCTL2;

   DATAout <= 
			 COPCTL2 WHEN commIN='0'  AND  ADDRin=x"06"
	  --ELSE mydata WHEN IRQ='0' AND RDin='0' AND ADDRin=KBPORT -- from ps/2 data	
	  ELSE COPint&'1'&FRMint&"00101" WHEN commIN='0' AND  ADDRin=x"14" --IN 20 STATUS REGISTER 9/9/2016
																				--bit 1 is pwrup should be 0 when we are ready
     ELSE COPCTL WHEN commIN='0' AND ADDRin=x"03"	
	 -- ELSE "101000"&CTS&RX WHEN commIN='0' AND ADDRin=x"16"	--IN 22 GET V24 SIGNALS (ZEROES NOT USED)
	  ELSE "00000000";
 

--DEVICE ENABLE SIGNALS
 	RS232ce<='0' WHEN nIORQin='0' AND (ADDRin=RS232PORT or ADDRin=RS232PORT1 or ADDRin=RS232PORT2 or ADDRin=RS232PORT3
									 or ADDRin=RS232PORT4 or ADDRin=RS232PORT5 or ADDRin=RS232PORT6 or ADDRin=RS232PORT7)
     ELSE '1';
    KBce<='0' WHEN nIORQin='0' AND (ADDRin=KBPORT);
    I2Cce<='0' WHEN nIORQin='0' AND (ADDRin=i2cPORT or ADDRin=i2cPORT1 or ADDRin=i2cPORT2 or ADDRin=i2cPORT3);
    STRce<='0' WHEN nIORQin='0' AND (ADDRin=STORPORT or ADDRin=STORPORT1 )
     ELSE '1';
    
    nINTout <= '0' WHEN nIORQin='0' AND DATAin=x"0A" ELSE '1';
    dvSERout <= RS232ce;--RS232ce --to RS232 Device
    dvSTRout <= STRce; --STRce;

--dvKBout <= KBce; --to PS/2 Keyboard Device
--dvI2Cout <=i2cce; --to i2c Devices --no more pins avail

END behavior;

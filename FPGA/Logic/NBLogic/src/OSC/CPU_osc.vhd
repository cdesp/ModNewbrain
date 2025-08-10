--Copyright (C)2014-2022 Gowin Semiconductor Corporation.
--All rights reserved.
--File Title: IP file
--GOWIN Version: V1.9.8.06
--Part Number: GW1NR-LV9QN88PC6/I5
--Device: GW1NR-9
--Created Time: Tue Sep 13 19:13:24 2022

library IEEE;
use IEEE.std_logic_1164.all;

entity CPUOSC is
    port (
        oscout: out std_logic
    );
end CPUOSC;

architecture Behavioral of CPUOSC is

CONSTANT MAINCLOCK:INTEGER :=8;
--66=  4Mhz         CLOCK OK NEEDS CHANGE ON DELAYS FOR STORAGE PROBABLY
--44=  6MHz 
--32=  7.8MHz
--34=  8.2Mhz
--26= 10.2MHz            DELAYS???
--24= 11MHz         CLOCK OK RUN PERFECTLY
--22= 12MHz         CLOCK OK RUN PERFECTLY
--20= 13.2MHZ           DELAYS???
--18= 14.6MHZ         
--16= 16.4MHZ                     
--12= 20MHZ 


CONSTANT FDIV:INTEGER := 22; -- EVEN NUMBER ONLY  264 / MAINCLOCK???

    --component declaration
    component OSC
        generic (
            FREQ_DIV: in integer := 96;  
            DEVICE: in string := "GW1N-4"
        );
        port (
            OSCOUT: out std_logic
        );
    end component;
 
begin
    osc_inst: OSC
        generic map (
            FREQ_DIV => FDIV ,       
            DEVICE => "GW1NR-9C"     
        )
        port map (
            OSCOUT => oscout
        );

end Behavioral; --CPUOSC

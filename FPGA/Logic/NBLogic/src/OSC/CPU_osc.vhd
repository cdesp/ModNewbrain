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
            FREQ_DIV => 22,       --26=10 MHz --24= 11MHz  --22 = 12MHz -- 20 = 13MHZ -- 18 =14MHZ --13 = 20MHZ
            DEVICE => "GW1NR-9"     --66=4 Mhz 32=8 Mhz
        )
        port map (
            OSCOUT => oscout
        );

end Behavioral; --CPUOSC

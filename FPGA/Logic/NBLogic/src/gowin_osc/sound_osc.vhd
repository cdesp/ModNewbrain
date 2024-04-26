--Copyright (C)2014-2022 Gowin Semiconductor Corporation.
--All rights reserved.
--File Title: IP file
--GOWIN Version: V1.9.8.06
--Part Number: GW1NR-LV9QN88PC6/I5
--Device: GW1NR-9C
--Created Time: Wed Aug 23 17:05:14 2023

library IEEE;
use IEEE.std_logic_1164.all;

entity sound_osc is
    port (
        oscout: out std_logic
    );
end sound_osc;

architecture Behavioral of sound_osc is

    --component declaration
    component OSC
        generic (
            FREQ_DIV: in integer := 100;
            DEVICE: in string := "GW1N-4"
        );
        port (
            OSCOUT: out std_logic
        );
    end component;

begin
    osc_inst: OSC
        generic map (
            FREQ_DIV => 84,         --84 gives 2.5MHz
            DEVICE => "GW1NR-9C"
        )
        port map (
            OSCOUT => oscout
        );

end Behavioral; --sound_osc

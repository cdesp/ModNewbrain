--Copyright (C)2014-2024 Gowin Semiconductor Corporation.
--All rights reserved.
--File Title: Template file for instantiation
--Tool Version: V1.9.9.02
--Part Number: GW1NR-LV9QN88PC6/I5
--Device: GW1NR-9
--Device Version: C
--Created Time: Sat Jul  6 16:57:55 2024

--Change the instance name and port connections to the signal names
----------Copy here to design--------

component Gowin_OSC
    port (
        oscout: out std_logic
    );
end component;

your_instance_name: Gowin_OSC
    port map (
        oscout => oscout_o
    );

----------Copy end-------------------

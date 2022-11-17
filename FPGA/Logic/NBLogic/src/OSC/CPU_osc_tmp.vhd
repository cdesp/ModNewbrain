--Copyright (C)2014-2022 Gowin Semiconductor Corporation.
--All rights reserved.
--File Title: Template file for instantiation
--GOWIN Version: V1.9.8.06
--Part Number: GW1NR-LV9QN88PC6/I5
--Device: GW1NR-9
--Created Time: Tue Sep 13 19:13:24 2022

--Change the instance name and port connections to the signal names
----------Copy here to design--------

component CPUOSC
    port (
        oscout: out std_logic
    );
end component;

your_instance_name: CPUOSC
    port map (
        oscout => oscout_o
    );

----------Copy end-------------------

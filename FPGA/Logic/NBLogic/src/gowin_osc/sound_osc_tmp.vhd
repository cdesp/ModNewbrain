--Copyright (C)2014-2022 Gowin Semiconductor Corporation.
--All rights reserved.
--File Title: Template file for instantiation
--GOWIN Version: V1.9.8.06
--Part Number: GW1NR-LV9QN88PC6/I5
--Device: GW1NR-9C
--Created Time: Wed Aug 23 17:05:14 2023

--Change the instance name and port connections to the signal names
----------Copy here to design--------

component sound_osc
    port (
        oscout: out std_logic
    );
end component;

your_instance_name: sound_osc
    port map (
        oscout => oscout_o
    );

----------Copy end-------------------

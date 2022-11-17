--Copyright (C)2014-2022 Gowin Semiconductor Corporation.
--All rights reserved.
--File Title: Template file for instantiation
--GOWIN Version: V1.9.8.06
--Part Number: GW1N-LV1QN48C6/I5
--Device: GW1N-1
--Created Time: Thu Aug 25 13:04:23 2022

--Change the instance name and port connections to the signal names
----------Copy here to design--------

component CPU_OSC
    port (
        oscout: out std_logic
    );
end component;

your_instance_name: CPU_OSC
    port map (
        oscout => oscout_o
    );

----------Copy end-------------------

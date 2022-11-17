library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;
  
entity Clock_Divider is
    port ( 
       clk: in std_logic;
       reset: in std_logic;
       FREQCNTR: integer;
       clock_out: out std_logic
    );
end Clock_Divider;
  
architecture bhv of Clock_Divider is
  
signal count: integer:=1;
signal tmp : std_logic := '0';

       
begin
  
process(clk,reset,tmp)
begin
    if(reset='0') then
        count<=1;
        tmp<='0';
    elsif(clk'event and clk='1') then
        count <=count+1;
        if (count = FREQCNTR) then
            tmp <= NOT tmp;
            count <= 1;
        end if;
    end if;
    clock_out <= tmp;
end process;
  
end bhv;

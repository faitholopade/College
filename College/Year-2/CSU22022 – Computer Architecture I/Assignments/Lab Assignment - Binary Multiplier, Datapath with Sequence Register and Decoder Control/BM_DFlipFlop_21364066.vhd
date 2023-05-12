----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 09.11.2022 15:09:27
-- Design Name: 
-- Module Name: BM_DFlipFlop_21364066 - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity BM_DFlipFlop_21364066 is
    Port ( D : in STD_LOGIC;
           Reset:  in STD_LOGIC;
           Clock : in STD_LOGIC;
           Q : out STD_LOGIC);
end BM_DFlipFlop_21364066;

architecture Behavioral of BM_DFlipFlop_21364066 is

begin

process (Clock)
begin
   if Clock'event and Clock='1' then
      if Reset = '1' then
         Q <= '0';
      else
         Q <= D after 2ns;
      end if;
   end if;
end process;

end Behavioral;

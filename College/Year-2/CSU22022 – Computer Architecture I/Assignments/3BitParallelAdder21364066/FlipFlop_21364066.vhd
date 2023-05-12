----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 07.10.2022 09:26:54
-- Design Name: 
-- Module Name: FlipFlop_21364066 - Behavioral
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

entity FlipFlop_21364066 is
   Port ( D : in STD_LOGIC;
          CLK : in STD_LOGIC;
          Q : out STD_LOGIC);
end FlipFlop_21364066;

architecture Behavioral of FlipFlop_21364066 is

begin

process (CLK)
begin
   if CLK'event and CLK='1' then
      Q <= D after 2ns;
   end if;
end process;

end Behavioral;

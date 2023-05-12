----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 30.10.2022 22:27:53
-- Design Name: 
-- Module Name: BM_Register5Bit_21364066 - Behavioral
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

entity BM_Register5Bit_21364066 is
    Port ( D : in STD_LOGIC_VECTOR (4 downto 0);
           Clr  : in STD_LOGIC;
           Load : in STD_LOGIC;
           Clock : in STD_LOGIC;
           Q : out STD_LOGIC_VECTOR (4 downto 0));
end BM_Register5Bit_21364066;

architecture Behavioral of BM_Register5Bit_21364066 is

begin

process (Clock)
begin
   if (Clock'event AND Clock = '1') then
      if Clr = '1' then
        Q <= "00000" after 2 ns;
      elsif Load = '1' then
        Q <= D after 2ns;
      end if;
   end if;      
end process;

end Behavioral;

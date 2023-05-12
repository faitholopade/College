----------------------------------------------------------------------------------
-- Company: Trinity College Dublin 
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 08.11.2022 18:37:22
-- Design Name: 
-- Module Name: BM_Counter_21364066 - Behavioral
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
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity BM_Counter_21364066 is
    Port ( Clock : in STD_LOGIC;
           Reset : in STD_LOGIC;
           Decrement : in STD_LOGIC;
           Counter_Out : Out STD_LOGIC_VECTOR (2 downto 0));
end BM_Counter_21364066;

architecture Behavioral of BM_Counter_21364066 is

signal Count : std_logic_vector(2 downto 0);

begin

process (Clock)

begin
   if Clock = '1' and Clock'event then
      if Reset = '1' then
         Count <= "101";
      elsif Decrement = '1' then
         Count <= Count - '1';
      end if;
   end if;
end process;

Counter_Out <= Count;

end Behavioral;

----------------------------------------------------------------------------------
-- Company: Trinity College Dublin 
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 29.10.2022 19:57:17
-- Design Name: 
-- Module Name: BM_ShiftRegister_5Bit_21364066 - Behavioral
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

entity BM_ShiftRegister_5Bit_21364066 is
    Port ( D : in STD_LOGIC;
           Clock : in STD_LOGIC;
           Load : in STD_LOGIC;
           Clr : in STD_LOGIC;
           Shift : in STD_LOGIC;
           P : in STD_LOGIC_VECTOR (4 downto 0);
           Q : inout STD_LOGIC_VECTOR (4 downto 0));
end BM_ShiftRegister_5Bit_21364066;

architecture Behavioral of BM_ShiftRegister_5Bit_21364066 is

begin
 
    process(Clock)
    begin
        if (Clock'event AND Clock = '1') then
            if Clr = '1' then
                Q <= "00000" after 2 ns;
            elsif Load = '1' then
                Q <= P after 2ns;
            elsif Shift = '1' then 
                Q(4) <= D after 2 ns;
                Q(3 downto 0) <= Q(4 downto 1) after 2 ns;
            end if;
        end if;         
    end process; 

end Behavioral;

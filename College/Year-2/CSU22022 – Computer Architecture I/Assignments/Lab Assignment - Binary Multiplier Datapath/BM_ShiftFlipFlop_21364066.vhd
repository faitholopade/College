----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 30.10.2022 14:30:20
-- Design Name: 
-- Module Name: BM_ShiftFlipFlop_21364066 - Behavioral
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

entity BM_ShiftFlipFlop_21364066 is
    Port ( Clock : in STD_LOGIC;
           Load : in STD_LOGIC;
           Clr : in STD_LOGIC;
           Shift : in STD_LOGIC;
           P : in STD_LOGIC;
           Q : inout STD_LOGIC);
end BM_ShiftFlipFlop_21364066;

architecture Behavioral of BM_ShiftFlipFlop_21364066 is

begin

    process(Clock)
    begin
        if (Clock'event AND Clock = '1') then
            if Clr = '1' then
                Q <= '0' after 2 ns;
            elsif Load = '1' then
                Q <= P after 2ns;
            elsif Shift = '1' then 
                Q <= '0' after 2 ns;
            end if;
        end if;         
    end process; 
    
end Behavioral;

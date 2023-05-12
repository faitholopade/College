----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 22:35:00
-- Design Name: 
-- Module Name: CPU_DFlipFlop_21364066 - Behavioral
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

entity CPU_DFlipFlop_21364066 is
    port (
            Clock : in std_logic;
            D : in std_logic;
            Enable : in std_logic;
            Reset : in std_logic;

            Q: out std_logic
        );
end CPU_DFlipFlop_21364066;

architecture Behavioral of CPU_DFlipFlop_21364066 is
    begin
        process(Clock)
        begin 
        if rising_edge(Clock) then
            if Reset = '1' then 
                Q <= '0';
            elsif Enable = '1' then
                Q <= D;
            end if;
        end if;   
        end process;
end Behavioral;

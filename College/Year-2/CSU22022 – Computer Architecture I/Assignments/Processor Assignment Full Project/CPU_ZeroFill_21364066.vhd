----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 22:35:00
-- Design Name: 
-- Module Name: CPU_ZeroFill_21364066 - Behavioral
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

entity CPU_ZeroFill_21364066 is
    port (  
            SB : in std_logic_vector(4 downto 0);
            MuxB : out std_logic_vector(31 downto 0)
        );
end CPU_ZeroFill_21364066;

architecture Behavioral of CPU_ZeroFill_21364066 is
begin
    MuxB(4 downto 0) <= SB after 10ns;
    MuxB(31 downto 5) <= "000000000000000000000000000" after 10ns;
end Behavioral;

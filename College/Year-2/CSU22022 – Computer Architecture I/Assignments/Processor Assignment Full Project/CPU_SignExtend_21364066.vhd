----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 22:35:00
-- Design Name: 
-- Module Name: CPU_SignExtend_21364066 - Behavioral
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

entity CPU_SignExtend_21364066 is
    port (  Input : in std_logic_vector(9 downto 0);
    
            Output : out std_logic_vector(31 downto 0)
        );
end CPU_SignExtend_21364066;

architecture Behavioral of CPU_SignExtend_21364066 is
    begin
        Output (9 downto 0) <= Input;
        Output (31 downto 10) <= "0000000000000000000000" when Input(9) = '0' else "1111111111111111111111";
end Behavioral;

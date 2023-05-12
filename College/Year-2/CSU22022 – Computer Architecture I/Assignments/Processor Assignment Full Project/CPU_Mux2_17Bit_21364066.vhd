----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 22:35:00
-- Design Name: 
-- Module Name: CPU_Mux2_17Bit_21364066 - Behavioral
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

entity CPU_Mux2_17Bit_21364066 is
    port (  In0 : in std_logic_vector(16 downto 0);
            In1 : in std_logic_vector(16 downto 0);
            Sel : in std_logic;

            Z : out std_logic_vector(16 downto 0)
        );
end CPU_Mux2_17Bit_21364066;

architecture Behavioral of CPU_Mux2_17Bit_21364066 is
    begin
        Z <=    In0 after 10ns when Sel = '0' else
                In1 after 10ns when Sel = '1' else
                "00000000000000000" after 10ns;
            
end Behavioral;

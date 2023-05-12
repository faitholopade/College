----------------------------------------------------------------------------------
-- Company: Trinity College
-- Engineer: Faith Olopade
-- 
-- Create Date:    16:56:30 28/10/2022
-- Design Name: 
-- Module Name:    CPU_CPU_Mux2_32Bit_21364066_21364066
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
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
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity CPU_Mux2_32Bit_21364066 is
    port (
            In0 : in std_logic_vector(31 downto 0);
            In1 : in std_logic_vector(31 downto 0);
            Sel : in std_logic;
            Z : out std_logic_vector(31 downto 0)
            );
    end CPU_Mux2_32Bit_21364066;
    
    architecture Behavioral of CPU_Mux2_32Bit_21364066 is
        begin
            Z <= In0 when Sel ='0' else
            In1 when Sel ='1'else
            "00000000000000000000000000000000";
end Behavioral;
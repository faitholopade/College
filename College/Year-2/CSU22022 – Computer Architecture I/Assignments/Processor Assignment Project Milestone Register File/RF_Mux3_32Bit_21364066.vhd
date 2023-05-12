----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 22:35:00
-- Design Name: 
-- Module Name: RF_Mux3_32Bit_21364066 - Behavioral
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

entity RF_Mux3_32Bit_21364066 is
    port (
        In00 : in std_logic_vector(31 downto 0);
        In01 : in std_logic_vector(31 downto 0);
        In02 : in std_logic_vector(31 downto 0);
        A : in std_logic_vector(1 downto 0);
        Z : out std_logic_vector(31 downto 0)
        );
end RF_Mux3_32Bit_21364066;

architecture Behavioral of RF_Mux3_32Bit_21364066 is
    begin
    process(A, In00, In01, In02)
        begin 
        case A is
            when "00" => Z <= In00;
            when "01" => Z <= In01;
            when "10" => Z <= In02;
            when others => Z <=  "00000000000000000000000000000000";
        end case;
    end process;
end Behavioral;

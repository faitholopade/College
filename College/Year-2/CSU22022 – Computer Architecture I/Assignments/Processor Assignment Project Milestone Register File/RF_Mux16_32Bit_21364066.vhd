----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 18.10.2022 14:11:00
-- Design Name: 
-- Module Name: RF_Mux16_32Bit_21364066 - Behavioral
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

entity RF_Mux16_32Bit_21364066 is
    port (
        In00 : in std_logic_vector(31 downto 0);
        In01 : in std_logic_vector(31 downto 0);
        In02 : in std_logic_vector(31 downto 0);
        In03 : in std_logic_vector(31 downto 0);
        In04 : in std_logic_vector(31 downto 0);
        In05 : in std_logic_vector(31 downto 0);
        In06 : in std_logic_vector(31 downto 0);
        In07 : in std_logic_vector(31 downto 0);
        In08 : in std_logic_vector(31 downto 0);
        In09 : in std_logic_vector(31 downto 0);
        In10 : in std_logic_vector(31 downto 0);
        In11 : in std_logic_vector(31 downto 0);
        In12 : in std_logic_vector(31 downto 0);
        In13 : in std_logic_vector(31 downto 0);
        In14 : in std_logic_vector(31 downto 0);
        In15 : in std_logic_vector(31 downto 0);

        A : in std_logic_vector(3 downto 0);

        Z : out std_logic_vector(31 downto 0)
        );
end RF_Mux16_32Bit_21364066;

architecture Behavioral of RF_Mux16_32Bit_21364066 is
    begin
    process(A, In00, In01, In02, In03, In04, In05, In06, In07, In08, In09, In10, In11, In12, In13, In14, In15)
        begin 
        case A is
            when "0000" => Z <= In00;
            when "0001" => Z <= In01;
            when "0010" => Z <= In02;
            when "0011" => Z <= In03;
            when "0100" => Z <= In04;
            when "0101" => Z <= In05;
            when "0110" => Z <= In06;
            when "0111" => Z <= In07;
            when "1000" => Z <= In08;
            when "1001" => Z <= In09;
            when "1010" => Z <= In10;
            when "1011" => Z <= In11;
            when "1100" => Z <= In12;
            when "1101" => Z <= In13;
            when "1110" => Z <= In14;
            when "1111" => Z <= In15;
   
            when others => Z <=  "00000000000000000000000000000000";
        end case;
    end process;
end Behavioral;

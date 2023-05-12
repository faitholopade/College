----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 18.10.2022 20:50:00
-- Design Name: 
-- Module Name: RF_Mux32_32Bit_21364066 - Behavioral
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

entity RF_Mux32_32Bit_21364066 is
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
        In16 : in std_logic_vector(31 downto 0);
        In17 : in std_logic_vector(31 downto 0);
        In18 : in std_logic_vector(31 downto 0);
        In19 : in std_logic_vector(31 downto 0);
        In20 : in std_logic_vector(31 downto 0);
        In21 : in std_logic_vector(31 downto 0);
        In22 : in std_logic_vector(31 downto 0);
        In23 : in std_logic_vector(31 downto 0);
        In24 : in std_logic_vector(31 downto 0);
        In25 : in std_logic_vector(31 downto 0);
        In26 : in std_logic_vector(31 downto 0);
        In27 : in std_logic_vector(31 downto 0);
        In28 : in std_logic_vector(31 downto 0);
        In29 : in std_logic_vector(31 downto 0);
        In30 : in std_logic_vector(31 downto 0);
        In31 : in std_logic_vector(31 downto 0);

        A : in std_logic_vector(4 downto 0);

        Z : out std_logic_vector(31 downto 0)
        );
end RF_Mux32_32Bit_21364066;

architecture Behavioral of RF_Mux32_32Bit_21364066 is
    begin
    process(A, In00, In01, In02, In03, In04, In05, In06, In07, In08, In09, In10, In11, In12, In13, In14, In15, In16, In17, In18, In19, In20, In21, In22, In23, In24, In25, In26, In27, In28, In29, In30, In31)
        begin 
        case A is
            when "00000" => Z <= In00;
            when "00001" => Z <= In01;
            when "00010" => Z <= In02;
            when "00011" => Z <= In03;
            when "00100" => Z <= In04;
            when "00101" => Z <= In05;
            when "00110" => Z <= In06;
            when "00111" => Z <= In07;
            when "01000" => Z <= In08;
            when "01001" => Z <= In09;
            when "01010" => Z <= In10;
            when "01011" => Z <= In11;
            when "01100" => Z <= In12;
            when "01101" => Z <= In13;
            when "01110" => Z <= In14;
            when "01111" => Z <= In15;

            when "10000" => Z <= In16;
            when "10001" => Z <= In17;
            when "10010" => Z <= In18;
            when "10011" => Z <= In19;
            when "10100" => Z <= In20;
            when "10101" => Z <= In21;
            when "10110" => Z <= In22;
            when "10111" => Z <= In23;
            when "11000" => Z <= In24;
            when "11001" => Z <= In25;
            when "11010" => Z <= In26;
            when "11011" => Z <= In27;
            when "11100" => Z <= In28;
            when "11101" => Z <= In29;
            when "11110" => Z <= In30;
            when "11111" => Z <= In31;
   
            when others => Z <=  "00000000000000000000000000000000";
        end case;
    end process;
end Behavioral;

-- 32 bit truth table
-- 00000
-- 00001
-- 00010
-- 00011
-- 00100
-- 00101
-- 00110
-- 00111
-- 01000
-- 01001
-- 01010
-- 01011
-- 01100
-- 01101
-- 01110
-- 01111
-- 10000
-- 10001
-- 10010
-- 10011
-- 10100
-- 10101
-- 10110
-- 10111
-- 11000
-- 11001
-- 11010
-- 11011
-- 11100
-- 11101
-- 11110
-- 11111
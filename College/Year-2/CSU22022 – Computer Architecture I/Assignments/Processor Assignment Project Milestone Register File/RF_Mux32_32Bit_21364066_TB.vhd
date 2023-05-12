----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 18.10.2022 21:34:00
-- Design Name: 
-- Module Name: RF_Mux32_32Bit_21364066_TB - Sim
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

entity RF_Mux32_32Bit_21364066_TB is
--  Port ( ); We don't need ports
end RF_Mux32_32Bit_21364066_TB;

architecture Sim of RF_Mux32_32Bit_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component RF_Mux32_32Bit_21364066
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
end component;

--Inputs

    signal In00_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In01_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In02_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In03_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In04_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In05_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In06_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In07_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In08_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In09_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In10_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In11_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In12_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In13_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In14_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In15_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In16_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In17_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In18_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In19_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In20_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In21_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In22_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In23_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In24_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In25_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In26_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In27_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In28_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In29_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In30_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In31_TB : std_logic_vector(31 downto 0):= (others => '0');

    signal A_TB : std_logic_vector(4 downto 0):= (others => '0');
    
--Outputs

    signal Z_TB : std_logic_vector(31 downto 0):= (others => '0');
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: RF_Mux32_32Bit_21364066 port map (
          In00 => In00_TB,
          In01 => In01_TB,
          In02 => In02_TB,
          In03 => In03_TB,
          In04 => In04_TB,
          In05 => In05_TB,
          In06 => In06_TB,
          In07 => In07_TB,
          In08 => In08_TB,
          In09 => In09_TB,
          In10 => In10_TB,
          In11 => In11_TB,
          In12 => In12_TB,
          In13 => In13_TB,
          In14 => In14_TB,
          In15 => In15_TB,
          In16 => In16_TB,
          In17 => In17_TB,
          In18 => In18_TB,
          In19 => In19_TB,
          In20 => In20_TB,
          In21 => In21_TB,
          In22 => In22_TB,
          In23 => In23_TB,
          In24 => In24_TB,
          In25 => In25_TB,
          In26 => In26_TB,
          In27 => In27_TB,
          In28 => In28_TB,
          In29 => In29_TB,
          In30 => In30_TB,
          In31 => In31_TB,

          A => A_TB,
          Z => Z_TB
        );
        
   stim_proc: process

   begin
    In00_TB <= "00000001010001011111110101100010";
    In01_TB <= "00000001010001011111110101100011";
    In02_TB <= "00000001010001011111110101100100";
    In03_TB <= "00000001010001011111110101100101";
    In04_TB <= "00000001010001011111110101100110";
    In05_TB <= "00000001010001011111110101100111";
    In06_TB <= "00000001010001011111110101101000";
    In07_TB <= "00000001010001011111110101101001";
    In08_TB <= "00000001010001011111110101101010";
    In09_TB <= "00000001010001011111110101101011";
    In10_TB <= "00000001010001011111110101101100";
    In11_TB <= "00000001010001011111110101101101";
    In12_TB <= "00000001010001011111110101101110";
    In13_TB <= "00000001010001011111110101101111";
    In14_TB <= "00000001010001011111110101110000";
    In15_TB <= "00000001010001011111110101110001";
    In16_TB <= "00000001010001011111110101110010";
    In17_TB <= "00000001010001011111110101110011";
    In18_TB <= "00000001010001011111110101110100"; 
    In19_TB <= "00000001010001011111110101110101";
    In20_TB <= "00000001010001011111110101110110";
    In21_TB <= "00000001010001011111110101110111";
    In22_TB <= "00000001010001011111110101111000";
    In23_TB <= "00000001010001011111110101111001";
    In24_TB <= "00000001010001011111110101111010";
    In25_TB <= "00000001010001011111110101111011";
    In26_TB <= "00000001010001011111110101111100";
    In27_TB <= "00000001010001011111110101111101";
    In28_TB <= "00000001010001011111110101111110";
    In29_TB <= "00000001010001011111110101111111";
    In30_TB <= "00000001010001011111110110000000";
    In31_TB <= "00000001010001011111110110000001";

    wait for 5ns;
    A_TB <= "00000";

    wait for 5ns;
    A_TB <= "00001";

    wait for 5ns;
    A_TB <= "00010";

    wait for 5ns;
    A_TB <= "00011";

    wait for 5ns;
    A_TB <= "00100";

    wait for 5ns;
    A_TB <= "00101";

    wait for 5ns;
    A_TB <= "00110";

    wait for 5ns;
    A_TB <= "00111";

    wait for 5ns;
    A_TB <= "01000"; 

    wait for 5ns;
    A_TB <= "01001";

    wait for 5ns;
    A_TB <= "01010";

    wait for 5ns;
    A_TB <= "01011";

    wait for 5ns;
    A_TB <= "01100";

    wait for 5ns;
    A_TB <= "01101";

    wait for 5ns;
    A_TB <= "01110";

    wait for 5ns;
    A_TB <= "01111";

    --

    wait for 5ns;
    A_TB <= "10000";

    wait for 5ns;
    A_TB <= "10001";

    wait for 5ns;
    A_TB <= "10010";

    wait for 5ns;
    A_TB <= "10011";

    wait for 5ns;
    A_TB <= "10100";

    wait for 5ns;
    A_TB <= "10101";

    wait for 5ns;
    A_TB <= "10110";

    wait for 5ns;
    A_TB <= "10111";

    wait for 5ns;
    A_TB <= "11000"; 

    wait for 5ns;
    A_TB <= "11001";

    wait for 5ns;
    A_TB <= "11010";

    wait for 5ns;
    A_TB <= "11011";

    wait for 5ns;
    A_TB <= "11100";

    wait for 5ns;
    A_TB <= "11101";

    wait for 5ns;
    A_TB <= "11110";

    wait for 5ns;
    A_TB <= "11111";

   end process;
end Sim;
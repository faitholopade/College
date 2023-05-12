----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 23:12:00
-- Design Name: 
-- Module Name: RF_Mux16_32Bit_21364066_TB - Sim
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

entity RF_Mux16_32Bit_21364066_TB is
--  Port ( ); We don't need ports
end RF_Mux16_32Bit_21364066_TB;

architecture Sim of RF_Mux16_32Bit_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component RF_Mux16_32Bit_21364066
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

    signal A_TB : std_logic_vector(3 downto 0):= (others => '0');
    
--Outputs

    signal Z_TB : std_logic_vector(31 downto 0):= (others => '0');
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: RF_Mux16_32Bit_21364066 port map (
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

    wait for 5ns;
    A_TB <= "0000";

    wait for 5ns;
    A_TB <= "0001";

    wait for 5ns;
    A_TB <= "0010";

    wait for 5ns;
    A_TB <= "0011";

    wait for 5ns;
    A_TB <= "0100";

    wait for 5ns;
    A_TB <= "0101";

    wait for 5ns;
    A_TB <= "0110";

    wait for 5ns;
    A_TB <= "0111";

    wait for 5ns;
    A_TB <= "1000"; 

    wait for 5ns;
    A_TB <= "1001";

    wait for 5ns;
    A_TB <= "1010";

    wait for 5ns;
    A_TB <= "1011";

    wait for 5ns;
    A_TB <= "1100";

    wait for 5ns;
    A_TB <= "1101";

    wait for 5ns;
    A_TB <= "1110";

    wait for 5ns;
    A_TB <= "1111";

   end process;
end Sim;
----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 23:12:00
-- Design Name: 
-- Module Name: RF_Mux3_32Bit_21364066_TB - Sim
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

entity RF_Mux3_32Bit_21364066_TB is
--  Port ( ); We don't need ports
end RF_Mux3_32Bit_21364066_TB;

architecture Sim of RF_Mux3_32Bit_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component RF_Mux3_32Bit_21364066
port ( 
        In00 : in std_logic_vector(31 downto 0);
        In01 : in std_logic_vector(31 downto 0);
        In02 : in std_logic_vector(31 downto 0);
        A : in std_logic_vector(1 downto 0);
        Z : out std_logic_vector(31 downto 0)
      );
end component;

--Inputs

    signal In00_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In01_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal In02_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal A_TB : std_logic_vector(1 downto 0):= (others => '0');
    
--Outputs

    signal Z_TB : std_logic_vector(31 downto 0):= (others => '0');
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: RF_Mux3_32Bit_21364066 port map (
          In00 => In00_TB,
          In01 => In01_TB,
          In02 => In02_TB,
          A => A_TB,
          Z => Z_TB
        );

        
   stim_proc: process

   begin
    In00_TB <= "00000001010001011111110101100010";
    In01_TB <= "00000001010001011111110101100011";
    In02_TB <= "00000001010001011111110101100100";

    wait for 5ns;
    A_TB <= "00";

    wait for 5ns;
    A_TB <= "01";

    wait for 5ns;
    A_TB <= "10";

   end process;
end Sim;
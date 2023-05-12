----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 23:12:00
-- Design Name: 
-- Module Name: CPU_Mux2_17Bit_21364066_TB - Sim
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

entity CPU_Mux2_17Bit_21364066_TB is
--  Port ( ); We don't need ports
end CPU_Mux2_17Bit_21364066_TB;

architecture Sim of CPU_Mux2_17Bit_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component CPU_Mux2_17Bit_21364066
port ( 
        In0 : in std_logic_vector(16 downto 0);
        In1 : in std_logic_vector(16 downto 0);
        Sel : in std_logic;

        Z : out std_logic_vector(16 downto 0)
    );
end component;

--Inputs

    signal In0_TB : std_logic_vector(16 downto 0) := (others => '0');
    signal In1_TB : std_logic_vector(16 downto 0) := (others => '0');
    signal Sel_TB : std_logic := '0';
    
--Outputs

    signal Z_TB : std_logic_vector(16 downto 0) := (others => '0');
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: CPU_Mux2_17Bit_21364066 port map (
        In0 => In0_TB,
        In1 => In1_TB,
        Sel => Sel_TB,
        Z => Z_TB
   );

        
   stim_proc: process
   begin

    In0_TB <= "00000000000000000";
    In1_TB <= "11111111111111111";

    wait for 20ns;
    Sel_TB <= '0';
    
    wait for 20ns;
    Sel_TB <= '1';

    wait for 20ns;
    
   end process;
end Sim;
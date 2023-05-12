----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 03.11.2022 20:40:00
-- Design Name: 
-- Module Name: DP_ShifterCFlagMux2_1Bit_21364066_TB - Sim
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

entity DP_ShifterCFlagMux2_1Bit_21364066_TB is
--  Port ( ); We don't need ports
end DP_ShifterCFlagMux2_1Bit_21364066_TB;

architecture Sim of DP_ShifterCFlagMux2_1Bit_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component DP_ShifterCFlagMux2_1Bit_21364066
port ( 
        LSB : in std_logic;
        MSB : in std_logic;
        S1 : in std_logic;
        S2 : in std_logic;
        C : out std_logic
      );
end component;

--Inputs

    signal LSB_TB : std_logic:= '0';
    signal MSB_TB : std_logic:= '0';
    signal S1_TB : std_logic:= '0';
    signal S2_TB : std_logic:= '0';
--Outputs

    signal C_TB : std_logic:= '0';
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: DP_ShifterCFlagMux2_1Bit_21364066 port map (
          LSB => LSB_TB,
          MSB => MSB_TB,
          S1 => S1_TB,
          S2 => S2_TB,
          C => C_TB
        );

        
   stim_proc: process

   begin
    LSB_TB <= '0'; 
    MSB_TB <= '1'; 

    wait for 10 ns;	
    S1_TB <= '0';
    S2_TB <= '0';

    wait for 10 ns;	
		S1_TB <= '1';
		S2_TB <= '0';
		
	  wait for 10 ns;	
    S1_TB <= '0';
    S2_TB <= '1';

    wait for 10 ns;	
		S1_TB <= '1';
		S2_TB <= '1';

   end process;
end Sim;
----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 01.11.2022 14:30:00
-- Design Name: 
-- Module Name: DP_SingleBit_B_Logic_21364066_TB - Sim
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

entity DP_SingleBit_B_Logic_21364066_TB is
--  Port ( ); We don't need ports
end DP_SingleBit_B_Logic_21364066_TB;

architecture Sim of DP_SingleBit_B_Logic_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component DP_SingleBit_B_Logic_21364066
port ( 
           B : in  std_logic;
           S0, S1 : in  std_logic;
           Y : out  std_logic
      );
end component;

--Inputs

    signal B_TB : std_logic:= '0';
    signal S0_TB : std_logic:= '0';
    signal S1_TB : std_logic:= '0';
--Outputs

    signal Y_TB : std_logic:= '0';
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: DP_SingleBit_B_Logic_21364066 port map (
          B => B_TB,
          S0 => S0_TB,
          S1 => S1_TB,
          Y => Y_TB
        );

        
   stim_proc: process

   begin
   wait for 10ns;
    B_TB <= '0';

    wait for 10ns;
    S0_TB <= '0';
    S1_TB <= '0';

    wait for 10ns ;
    S0_TB <= '1';
    S1_TB <= '0';

    wait for 10ns;
    S0_TB <= '0';
    S1_TB <= '1';

    wait for 10ns;
    S0_TB <= '1';
    S1_TB <= '1';
    
    wait for 10ns;

    B_TB <= '1';

    wait for 10ns;
    S0_TB <= '0';
    S1_TB <= '0';

    wait for 10ns ;
    S0_TB <= '1';
    S1_TB <= '0';

    wait for 10ns;
    S0_TB <= '0';
    S1_TB <= '1';

    wait for 10ns;
    S0_TB <= '1';
    S1_TB <= '1';

   end process;
end Sim;

--B 1/0
--S0/S1- 00 01 10 11
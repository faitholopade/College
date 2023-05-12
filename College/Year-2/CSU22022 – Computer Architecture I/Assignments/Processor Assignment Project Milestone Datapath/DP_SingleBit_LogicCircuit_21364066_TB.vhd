----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 02.11.2022 15:24:00
-- Design Name: 
-- Module Name: DP_SingleBit_LogicCircuit_21364066_TB - Sim
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

entity DP_SingleBit_LogicCircuit_21364066_TB is
--  Port ( ); We don't need ports
end DP_SingleBit_LogicCircuit_21364066_TB;

architecture Sim of DP_SingleBit_LogicCircuit_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component DP_SingleBit_LogicCircuit_21364066
port ( 
        A : in std_logic;
        B : in std_logic;
        S0 : in std_logic;
        S1 : in std_logic;
        G : out std_logic
      );
end component;

--Inputs

    signal A_TB : std_logic := '0';
    signal B_TB : std_logic := '0';
    signal S0_TB : std_logic := '0';
    signal S1_TB : std_logic := '0';
    
--Outputs

    signal G_TB : std_logic := '0';
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: DP_SingleBit_LogicCircuit_21364066 port map (
          A => A_TB,
          B => B_TB,
          S0 => S0_TB,
          S1 => S1_TB,
          G => G_TB
        );

        
   stim_proc: process

   begin
    --00
--    A_TB  <= '0';
--    B_TB <= '0';

--    wait for 10ns;
--    S0_TB <= '0';
--    S1_TB <= '0';

--    wait for 10ns ;
--    S0_TB <= '1';
--    S1_TB <= '0';

--    wait for 10ns;
--    S0_TB <= '0';
--    S1_TB <= '1';

--    wait for 10ns;
--    S0_TB <= '1';
--    S1_TB <= '1';
    
--    wait for 10ns;

    --01
    wait for 10ns;
    
    A_TB  <= '0';
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
    
    wait for 10ns;

    --10
    A_TB  <= '1';
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

    --11
    A_TB  <= '1';
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
    
    wait for 10ns;


   end process;
end Sim;
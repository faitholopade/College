----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 05.10.2022 13:12:00
-- Design Name: 
-- Module Name: DP_Shifter_21364066_TB - Sim
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

entity DP_Shifter_21364066_TB is
--  Port ( ); We don't need ports
end DP_Shifter_21364066_TB;

architecture Sim of DP_Shifter_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component DP_Shifter_21364066
port ( 
        B : in std_logic_vector (31 downto 0);
        S1 : in std_logic;
        S2 : in std_logic;
        G : out std_logic_vector (31 downto 0);
        C : out std_logic
      );
end component;

--Inputs

    signal B_TB : std_logic_vector(31 downto 0) := (others => '0');
    signal S1_TB : std_logic := '0';
    signal S2_TB : std_logic := '0';
--Outputs

    signal G_TB : std_logic_vector(31 downto 0) := (others => '0');
    signal C_TB : std_logic := '0';

begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: DP_Shifter_21364066 port map (
          B => B_TB,
          S1 => S1_TB,
          S2 => S2_TB,
          G => G_TB,
          C => C_TB
        );

        
   stim_proc: process

   begin
    B_TB <= "00000001010001011111110101100010";

    -- no shift
    wait for 100ns;
		S1_TB <= '0';
		S2_TB <= '0';

		--shift left
		wait for 100ns;
		S1_TB <= '0';
		S2_TB <= '1';

    --shift right
		wait for 100ns;
		S1_TB <= '1';
		S2_TB <= '0';
		
    --undefined 
		wait for 100ns;
		S1_TB <= '1';
		S2_TB <= '1';

    ---modified student ID to demonstrate setting and unset C flag

    B_TB <= "10000001010001011111110101100011";

    -- no shift
    wait for 100ns;
		S1_TB <= '0';
		S2_TB <= '0';

    --shift left
		wait for 100ns;
		S1_TB <= '0';
		S2_TB <= '1';
		
    --shift right
		wait for 100ns;
		S1_TB <= '1';
		S2_TB <= '0';
		
    --undefined 
		wait for 100ns;
		S1_TB <= '1';
		S2_TB <= '1';
		
		
   end process;
end Sim;

-- S1 S2 Y3 Y2 Y1 Y0 Micro-ops
-- 0 0 D3 D2 D1 D0 No Rotate
-- 0 1 D2 D1 D0 D3 Rotate One
-- 1 0 D1 D0 D3 D2 Rotate Two
-- 1 1 D0 D3 D2 D1 Rotate Three
----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 03.11.2022 18:15:00
-- Design Name: 
-- Module Name: DP_Mux3_1Bit_21364066_TB - Sim
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

entity DP_Mux3_1Bit_21364066_TB is
--  Port ( ); We don't need ports
end DP_Mux3_1Bit_21364066_TB;

architecture Sim of DP_Mux3_1Bit_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component DP_Mux3_1Bit_21364066
port ( 
        Bi : in std_logic;
        slBi : in std_logic;
        srBi : in std_logic;
        S1 : in std_logic;
        S2 : in std_logic;
        Gi : out std_logic
      );
end component;

--Inputs

    signal Bi_TB : std_logic := '0';
    signal slBi_TB : std_logic := '0';
    signal srBi_TB : std_logic := '0';
    signal S1_TB : std_logic := '0';
    signal S2_TB : std_logic := '0';
    
--Outputs

    signal Gi_TB : std_logic := '0';
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: DP_Mux3_1Bit_21364066 port map (
          Bi => Bi_TB,
          slBi => slBi_TB,
          srBi => srBi_TB,
          S1 => S1_TB,
          S2 => S2_TB,
          Gi => Gi_TB
        );

        
   stim_proc: process

   begin
    -- --000
    --     wait for 10ns;
	-- 	Bi_TB <= '0';
	-- 	slBi_TB <= '0';
	-- 	srBi_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '0';
	-- 	S2_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '1';
	-- 	S2_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '0';
	-- 	S2_TB <= '1';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '1';
	-- 	S2_TB <= '1';

    --  --001
	-- 	wait for 10ns;
	-- 	Bi_TB <= '0';
	-- 	slBi_TB <= '0';
	-- 	srBi_TB <= '1';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '0';
	-- 	S2_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '1';
	-- 	S2_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '0';
	-- 	S2_TB <= '1';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '1';
	-- 	S2_TB <= '1';
    
    --   --010
	-- 	wait for 10ns;
	-- 	Bi_TB <= '0';
	-- 	slBi_TB <= '1';
	-- 	srBi_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '0';
	-- 	S2_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '1';
	-- 	S2_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '0';
	-- 	S2_TB <= '1';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '1';
	-- 	S2_TB <= '1';

    -- --011
	-- 	wait for 10ns;
	-- 	Bi_TB <= '0';
	-- 	slBi_TB <= '1';
	-- 	srBi_TB <= '1';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '0';
	-- 	S2_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '1';
	-- 	S2_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '0';
	-- 	S2_TB <= '1';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '1';
	-- 	S2_TB <= '1';

	-- 	--100
	-- 	wait for 10ns;
	-- 	Bi_TB <= '1';
	-- 	slBi_TB <= '0';
	-- 	srBi_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '0';
	-- 	S2_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '1';
	-- 	S2_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '0';
	-- 	S2_TB <= '1';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '1';
	-- 	S2_TB <= '1';

    --101
		wait for 10ns;
		Bi_TB <= '1';
		slBi_TB <= '0';
		srBi_TB <= '1';
		
		wait for 10ns;
		S1_TB <= '0';
		S2_TB <= '0';
		
		wait for 10ns;
		S1_TB <= '1';
		S2_TB <= '0';
		
		wait for 10ns;
		S1_TB <= '0';
		S2_TB <= '1';
		
		wait for 10ns;
		S1_TB <= '1';
		S2_TB <= '1';
		
    --     --110
	-- 	wait for 10ns;
	-- 	Bi_TB <= '1';
	-- 	slBi_TB <= '1';
	-- 	srBi_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '0';
	-- 	S2_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '1';
	-- 	S2_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '0';
	-- 	S2_TB <= '1';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '1';
	-- 	S2_TB <= '1';
		
    --     --111
	-- 	wait for 10ns;
	-- 	Bi_TB <= '1';
	-- 	slBi_TB <= '1';
	-- 	srBi_TB <= '1';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '0';
	-- 	S2_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '1';
	-- 	S2_TB <= '0';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '0';
	-- 	S2_TB <= '1';
		
	-- 	wait for 10ns;
	-- 	S1_TB <= '1';
	-- 	S2_TB <= '1';
		
   end process;
end Sim;
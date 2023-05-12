----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 23:12:00
-- Design Name: 
-- Module Name: CPU_ZeroFill_21364066_TB - Sim
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

entity CPU_ZeroFill_21364066_TB is
--  Port ( ); We don't need ports
end CPU_ZeroFill_21364066_TB;

architecture Sim of CPU_ZeroFill_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component CPU_ZeroFill_21364066
port ( 
        SB : in std_logic_vector(4 downto 0);
        MuxB : out std_logic_vector(31 downto 0)      
    );
end component;

--Inputs

    signal SB_TB : std_logic_vector(4 downto 0) := (others => '0');
    
--Outputs

    signal MuxB_TB : std_logic_vector(31 downto 0) := (others => '0');
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: CPU_ZeroFill_21364066 port map (
        SB => SB_TB,
        MuxB => MuxB_TB
    );

        
   stim_proc: process
   begin

        wait for 100ns;
        SB_TB <= "11111";
        
        wait for 100ns;
        SB_TB <= "11100";
        
        wait for 100ns;
        SB_TB <= "11011";
        
        wait for 100ns;
        SB_TB <= "10001";
        
        wait for 100ns;
        SB_TB <= "00000";
        
        wait for 100ns;
        SB_TB <= "10101";

        wait for 100ns;

   end process;
end Sim;
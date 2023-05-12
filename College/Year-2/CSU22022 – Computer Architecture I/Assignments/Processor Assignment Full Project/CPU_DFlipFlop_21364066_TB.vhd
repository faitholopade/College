----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 23:12:00
-- Design Name: 
-- Module Name: CPU_DFlipFlop_21364066_TB - Sim
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

entity CPU_DFlipFlop_21364066_TB is
--  Port ( ); We don't need ports
end CPU_DFlipFlop_21364066_TB;

architecture Sim of CPU_DFlipFlop_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component CPU_DFlipFlop_21364066
port ( 
        Clock : in std_logic;
        D : in std_logic;
        Enable : in std_logic;
        Reset : in std_logic;

        Q: out std_logic
     );
end component;

--Inputs
   signal Clock_TB : std_logic:= '0';
   signal D_TB : std_logic:= '0';
   signal Enable_TB : std_logic:= '0';
   signal Reset_TB : std_logic:= '0';

--Outputs
   signal Q_TB : std_logic:= '0';

   constant period : time := 10ns;

    
begin
	-- Instantiate the Unit Under Test (UUT)
	
  uut: CPU_DFlipFlop_21364066 port map (
        Clock => Clock_TB,
        D => D_TB,
        Enable => Enable_TB,
        Reset => Reset_TB,
        Q => Q_TB
    );

        
  Clock_TB <= not clock_TB after period/2;
   
   stim_proc: process
   begin
    
    wait until Clock_TB 'event and Clock_TB ='1';
    D_TB <= '1';
    Enable_TB <= '0';
      
    wait until Clock_TB 'event and Clock_TB ='1';
    D_TB <= '1';
    Enable_TB <= '1';

    wait until Clock_TB 'event and Clock_TB ='1';
    D_TB <= '1';
    Enable_TB <= '0';
    Reset_TB <= '1';
      
  end process;
end Sim;
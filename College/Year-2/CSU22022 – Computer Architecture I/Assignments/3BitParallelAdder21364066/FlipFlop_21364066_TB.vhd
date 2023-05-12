----------------------------------------------------------------------------------
-- Company: Trinity College Dublin 
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 07.10.2022 09:33:53
-- Design Name: 
-- Module Name: FlipFlop_21364066_TB - Behavioral
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

entity FlipFlop_21364066_TB is
--  Port ( ); We don't need ports
end FlipFlop_21364066_TB;

architecture Sim of FlipFlop_21364066_TB is

component FlipFlop_21364066
port ( 
    D : in STD_LOGIC;
    CLK : in STD_LOGIC;
    Q : out STD_LOGIC
      );
end component;

--Inputs

    signal D_TB : std_logic:= '0';
    signal CLK_TB : std_logic:= '0';
    
--Outputs

    signal Q_TB : std_logic := '0';
    
    constant PERIOD : time := 10ns;
    
begin

-- Instantiate the Unit Under Test (UUT)
	
   uut: FlipFlop_21364066
 port map (
          D => D_TB,
          CLK => CLK_TB,
          Q => Q_TB
        );
        
   CLK_TB <= not CLK_TB after PERIOD/2;
   
   stim_proc: process

   begin
   
      wait until CLK_TB'event and CLK_TB='1';
      D_TB <= '1';
      
      wait until CLK_TB'event and CLK_TB='1';
      D_TB <= '0';
      
   end process;

end Sim;

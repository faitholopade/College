----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 09.11.2022 15:14:17
-- Design Name: 
-- Module Name: BM_DFlipFlop_21364066_TB - Simulation
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

entity BM_DFlipFlop_21364066_TB is
--  Port ( ); We don't need prots
end BM_DFlipFlop_21364066_TB;

architecture Simulation of BM_DFlipFlop_21364066_TB is

-- Component Declaration for the Unit Under Test (UUT)
component BM_DFlipFlop_21364066
port ( 
      D : in STD_LOGIC;
      Reset:  in STD_LOGIC;
      Clock : in STD_LOGIC;
      Q : out STD_LOGIC
      );
end component;

--Inputs
    
    signal D_TB : STD_LOGIC := '0';
    signal Reset_TB : STD_LOGIC := '0';
    signal Clock_TB : STD_LOGIC := '0';
    
--Outputs

    signal Q_TB : STD_LOGIC := '0';
    
    constant PERIOD : time := 60ns;
    
begin

	-- Instantiate the Unit Under Test (UUT)
	
   uut: BM_DFlipFlop_21364066 port map (     
      D => D_TB,
      Reset => Reset_TB,
      Clock => Clock_TB,
      Q => Q_TB      
       );
        
   Clock_TB <= not Clock_TB after PERIOD/2;
   
     stim_proc: process

   begin

      -- Test vector 00 - Reset the D Flip Flop
   
      wait until Clock_TB'event and Clock_TB='1';
        
         D_TB <= '0' after PERIOD/4;
         Reset_TB <= '1' after PERIOD/4;
   
      -- Test vector 01 - unset D
   
      wait until Clock_TB'event and Clock_TB='1';
        
         D_TB <= '0' after PERIOD/4;
         Reset_TB <= '0' after PERIOD/4;
         
     -- Test vector 02 - set D
   
      wait until Clock_TB'event and Clock_TB='1';
        
         D_TB <= '1' after PERIOD/4;
         Reset_TB <= '0' after PERIOD/4;
         
     -- Test vector 03 - unset D
   
      wait until Clock_TB'event and Clock_TB='1';
        
         D_TB <= '0' after PERIOD/4;  
         Reset_TB <= '0' after PERIOD/4; 
                  
     -- Test vector 04 - unset D
   
      wait until Clock_TB'event and Clock_TB='1';
        
         D_TB <= '0' after PERIOD/4;
         Reset_TB <= '0' after PERIOD/4;            
         
     -- Test vector 05 - set D
   
      wait until Clock_TB'event and Clock_TB='1';
        
         D_TB <= '1' after PERIOD/4;
         Reset_TB <= '0' after PERIOD/4;           
         
     -- Test vector 06 - unset D
   
      wait until Clock_TB'event and Clock_TB='1';
        
         D_TB <= '0' after PERIOD/4;
         Reset_TB <= '0' after PERIOD/4;   
         
     -- Test vector 07 - set D
   
      wait until Clock_TB'event and Clock_TB='1';
        
         D_TB <= '1' after PERIOD/4;
         Reset_TB <= '0' after PERIOD/4;                                 
                      
   end process;

end Simulation;

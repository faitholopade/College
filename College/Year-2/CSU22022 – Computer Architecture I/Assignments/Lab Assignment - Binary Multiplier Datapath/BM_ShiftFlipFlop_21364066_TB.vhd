----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 30.10.2022 14:39:41
-- Design Name: 
-- Module Name: BM_ShiftFlipFlop_21364066_TB - Simulation
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

entity BM_ShiftFlipFlop_21364066_TB is
--  Port ( ); We don't need ports
end BM_ShiftFlipFlop_21364066_TB;

architecture Simulation of BM_ShiftFlipFlop_21364066_TB is

-- Component Declaration for the Unit Under Test (UUT)
component BM_ShiftFlipFlop_21364066
port ( 
      Clock : in STD_LOGIC;
      Load : in STD_LOGIC;
      Clr : in STD_LOGIC;
      Shift : in STD_LOGIC;
      P : in STD_LOGIC;
      Q : inout STD_LOGIC
      );
end component;

--Inputs

    signal Clock_TB : std_logic:= '0';
    signal Load_TB : std_logic:= '0';
    signal Clr_TB : std_logic:= '0';
    signal Shift_TB : std_logic:= '0';
    signal P_TB : std_logic;
    
--Outputs

    signal Q_TB : std_logic;
    
    constant PERIOD : time := 60ns;
    
begin


   -- Instantiate the Unit Under Test (UUT)
	
   uut: BM_ShiftFlipFlop_21364066 port map (     
      Clock => Clock_TB,
      Load => Load_TB,
      Clr => Clr_TB,
      Shift => Shift_TB,
      P => P_TB,
      Q => Q_TB      
       );
        
   CLOCK_TB <= not CLOCK_TB after PERIOD/2;
   
     stim_proc: process

   begin
   
      -- Test vector 01 - Reset All Inputs
   
      wait until CLOCK_TB'event and CLOCK_TB='1';
        
         Load_TB <= '0' after PERIOD/4;
         Clr_TB <= '0' after PERIOD/4;
         Shift_TB <= '0' after PERIOD/4;
         P_TB<= '1' after PERIOD/4;
         
         
      wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector 02 - Clear Shift Register

         Load_TB <= '0' after PERIOD/4;
         Clr_TB <= '1' after PERIOD/4;
         Shift_TB <= '0' after PERIOD/4;
         P_TB<= '1' after PERIOD/4;
   
         
      wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector 03 - Load Shift Register

         Load_TB <= '1' after PERIOD/4;
         Clr_TB <= '0' after PERIOD/4;
         Shift_TB <= '0' after PERIOD/4;
         P_TB<= '1' after PERIOD/4;
      
         
-- ++++++++++++++++++++++++++++++++++++++++++++++++++

      wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector 04 - Shift 1
        
         Load_TB <= '0' after PERIOD/4;
         Clr_TB <= '0' after PERIOD/4;
         Shift_TB <= '1' after PERIOD/4;
         P_TB<= '1' after PERIOD/4;      
         
       wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector 05 - Shift 2
        
         Load_TB <= '0' after PERIOD/4;
         Clr_TB <= '0' after PERIOD/4;
         Shift_TB <= '1' after PERIOD/4;
         P_TB<= '1' after PERIOD/4;         
                 
      wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector 06 - Shift 3
        
         Load_TB <= '0' after PERIOD/4;
         Clr_TB <= '0' after PERIOD/4;
         Shift_TB <= '1' after PERIOD/4;
         P_TB<= '1' after PERIOD/4;           
                  
      wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector 07 - Shift 4
        
         Load_TB <= '0' after PERIOD/4;
         Clr_TB <= '0' after PERIOD/4;
         Shift_TB <= '1' after PERIOD/4;
         P_TB<= '1' after PERIOD/4;         
                
 -- ++++++++++++++++++++++++++++++++++++++++++++++++++        

   end process;
   
end Simulation;

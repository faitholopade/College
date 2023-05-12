----------------------------------------------------------------------------------
-- Company: Trinity CollegeDublin 
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 09.11.2022 14:17:19
-- Design Name: 
-- Module Name: BM_Counter_21364066_TB - Simulation
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

entity BM_Counter_21364066_TB is
--  Port ( );
end BM_Counter_21364066_TB;

architecture Simulation of BM_Counter_21364066_TB is

-- Component Declaration for the Unit Under Test (UUT)
component BM_Counter_21364066
port ( 
      Clock : in STD_LOGIC;
      Reset : in STD_LOGIC;
      Decrement : in STD_LOGIC;
      Counter_Out : Out STD_LOGIC_VECTOR (2 downto 0)
      );
end component;

--Inputs
    
    signal Clock_TB : STD_LOGIC := '0';
    signal Reset_TB : STD_LOGIC := '0';
    signal Decrement_TB : STD_LOGIC := '0';
    
--Outputs
    
    signal Counter_Out_TB : STD_LOGIC_VECTOR (2 downto 0) := (others => '0');
    
    constant PERIOD : time := 60ns;
    
begin

	-- Instantiate the Unit Under Test (UUT)
	
   uut: BM_Counter_21364066 port map (     
      Clock => Clock_TB,
      Reset => Reset_TB,
      Decrement => Decrement_TB,
      Counter_Out => Counter_Out_TB      
       );
        
   Clock_TB <= not Clock_TB after PERIOD/2;
   
     stim_proc: process

   begin
   
      -- Test vector 01 - Reset All Inputs
   
      wait until Clock_TB'event and Clock_TB='1';
        
         Reset_TB <= '0' after PERIOD/4;
         Decrement_TB <= '0' after PERIOD/4;
         
       -- Test vector 02 - Reset Counter to "11"
   
      wait until Clock_TB'event and Clock_TB='1';
        
         Reset_TB <= '1' after PERIOD/4;
         Decrement_TB <= '0' after PERIOD/4;        
         
       -- Test vector 03 - Wait  for one clock cycle 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         Reset_TB <= '0' after PERIOD/4;
         Decrement_TB <= '0' after PERIOD/4;
         
       -- Test vector 04 - count down 1st clock cycle "101"
   
      wait until Clock_TB'event and Clock_TB='1';
        
         Reset_TB <= '0' after PERIOD/4;
         Decrement_TB <= '1' after PERIOD/4; 
         
       -- Test vector 05 - count down 2nd clock cycle "100"
   
      wait until Clock_TB'event and Clock_TB='1';
        
         Reset_TB <= '0' after PERIOD/4;
         Decrement_TB <= '1' after PERIOD/4; 
         
        -- Test vector 06 - count down 3rd clock cycle "011"
   
      wait until Clock_TB'event and Clock_TB='1';
        
         Reset_TB <= '0' after PERIOD/4;
         Decrement_TB <= '1' after PERIOD/4; 
         
        -- Test vector 07 - count down 4th clock cycle "010"
   
      wait until Clock_TB'event and Clock_TB='1';
        
         Reset_TB <= '0' after PERIOD/4;
         Decrement_TB <= '1' after PERIOD/4; 
         
        -- Test vector 08 - count down 5th clock cycle "001"
   
      wait until Clock_TB'event and Clock_TB='1';
        
         Reset_TB <= '0' after PERIOD/4;
         Decrement_TB <= '1' after PERIOD/4; 
         
        -- Test vector 09 - count down 6th clock cycle "000"
   
      wait until Clock_TB'event and Clock_TB='1';
        
         Reset_TB <= '0' after PERIOD/4;
         Decrement_TB <= '1' after PERIOD/4; 
                   
       -- Test vector 10 - Wait  for one clock cycle 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         Reset_TB <= '0' after PERIOD/4;
         Decrement_TB <= '0' after PERIOD/4;
                      
   end process;
   
end Simulation;

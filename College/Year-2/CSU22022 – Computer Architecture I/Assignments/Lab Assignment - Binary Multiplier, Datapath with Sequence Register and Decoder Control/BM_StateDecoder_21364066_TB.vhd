----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. MIchael Manzke
-- 
-- Create Date: 09.11.2022 15:47:39
-- Design Name: 
-- Module Name: BM_StateDecoder_21364066_TB - Simulation
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

entity BM_StateDecoder_21364066_TB is
--  Port ( ); We don't need ports
end BM_StateDecoder_21364066_TB;

architecture Simulation of BM_StateDecoder_21364066_TB is

component BM_StateDecoder_21364066
     Port ( 
           A0 : in STD_LOGIC;
           A1 : in STD_LOGIC;
           IDLE : out STD_LOGIC;
           MUL0 : out STD_LOGIC;
           MUL1 : out STD_LOGIC           
           );                      
end component;

--Inputs

    signal A0_TB, A1_TB : STD_LOGIC := '0';
  
--Outputs

    signal IDLE_TB, MUL0_TB, MUL1_TB : STD_LOGIC := '0';
    
begin

-- Instantiate the Unit Under Test (UUT)

   uut:BM_StateDecoder_21364066 PORT MAP (
       A0 => A0_TB,
       A1 => A1_TB,
       IDLE => IDLE_TB,
       MUL0 => MUL0_TB,
       MUL1 => MUL1_TB  
       );

 stim_proc: process
   begin
   
      
   	
   	  -- IDLE State	
      A1_TB <= '0'; A0_TB <= '0';
      wait for 10 ns;

   	  -- MUL0 State	
      A1_TB <= '0'; A0_TB <= '1';
      wait for 10 ns;

   	  -- MUL1 State	
      A1_TB <= '1'; A0_TB <= '0';
      wait for 10 ns;

   	  -- NO State	
      A1_TB <= '1'; A0_TB <= '1';
      wait for 10 ns;

 end process; 
             
end Simulation;

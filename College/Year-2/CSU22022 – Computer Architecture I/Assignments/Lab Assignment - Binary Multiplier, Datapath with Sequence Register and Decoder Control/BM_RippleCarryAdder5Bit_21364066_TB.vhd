----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 29.10.2022 22:54:32
-- Design Name: 
-- Module Name: BM_RippleCarryAdder5Bit_21364066_TB - Behavioral
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

entity BM_RippleCarryAdder5Bit_21364066_TB is
--  Port ( );
end BM_RippleCarryAdder5Bit_21364066_TB;

architecture Simulation of BM_RippleCarryAdder5Bit_21364066_TB is

-- Component Declaration for the Unit Under Test (UUT)
component BM_RippleCarryAdder5Bit_21364066
Port ( 
      A : in STD_LOGIC_VECTOR (4 downto 0);
      B : in STD_LOGIC_VECTOR (4 downto 0);
      C_IN : in STD_LOGIC;
      SUM : out STD_LOGIC_VECTOR (4 downto 0);
      C_OUT : out STD_LOGIC
      );
end component;

--Inputs

    signal A_TB : STD_LOGIC_VECTOR (4 downto 0) := (others => '0');
    signal B_TB : STD_LOGIC_VECTOR (4 downto 0) := (others => '0');
    signal C_IN_TB : STD_LOGIC := '0';
  
--Outputs

    signal SUM_TB : STD_LOGIC_VECTOR (4 downto 0)  := (others => '0');
    signal C_OUT_TB : STD_LOGIC := '0';
    
begin 
	-- Instantiate the Unit Under Test (UUT)
uut: BM_RippleCarryAdder5Bit_21364066 PORT MAP (
       A => A_TB,
       B => B_TB,
       C_IN => C_IN_TB,
       SUM => SUM_TB,
       C_OUT => C_OUT_TB
       );

 stim_proc: process
   begin
   
   -- Test Vector A
   		
      A_TB <= "00101";
      B_TB <= "11110";
      C_IN_TB <= '0';
      
      wait for 60 ns;	

   -- Test Vector B 
   
      A_TB <= "10101";
      B_TB <= "10111";
      C_IN_TB <= '0';     

      wait for 60 ns;
      
   -- Test Vector C  
         	
      A_TB <= "10001";
      B_TB <= "10011";
      C_IN_TB <= '0'; 
      
      wait for 60 ns;
      
   -- Test Vector D  	

      A_TB <= "10001";
      B_TB <= "10010";
      C_IN_TB <= '0';
      
      wait for 60 ns;	
      
   -- Test Vector E 
  
      A_TB <= "00010";
      B_TB <= "00110";
      C_IN_TB <= '0';
   
      wait for 60 ns;	

   -- Test Vector F, worst case propagation delay  
    
      A_TB <= "10110";
      B_TB <= "01001";
      C_IN_TB <= '1';     

      wait for 60 ns;
      	
 
   end process;
   
end Simulation;

----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 02.11.2022 02:51:26
-- Design Name: 
-- Module Name: DP_RippleCarryAdder32Bit_21364066_TB - Behavioral
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

entity DP_RippleCarryAdder32Bit_21364066_TB is
--  Port ( ); We don't need ports 
end DP_RippleCarryAdder32Bit_21364066_TB;

architecture Sim of DP_RippleCarryAdder32Bit_21364066_TB is

-- Component Declaration for the Unit Under Test (UUT)

component DP_RippleCarryAdder32Bit_21364066

Port ( A : in STD_LOGIC_VECTOR (31 downto 0);
       B : in STD_LOGIC_VECTOR (31 downto 0);
       C_IN : in STD_LOGIC;
       SUM : out STD_LOGIC_VECTOR (31 downto 0);
       C_OUT : out STD_LOGIC;
       V : out STD_LOGIC);
end component;

--Inputs

    signal A_TB : STD_LOGIC_VECTOR (31 downto 0) := (others => '0');
    signal B_TB : STD_LOGIC_VECTOR (31 downto 0) := (others => '0');
    signal C_IN_TB : STD_LOGIC := '0';
  
--Outputs

    signal SUM_TB : STD_LOGIC_VECTOR (31 downto 0)  := (others => '0');
    signal C_OUT_TB : STD_LOGIC := '0';
    signal V_TB : STD_LOGIC := '0';

begin

	-- Instantiate the Unit Under Test (UUT)
   uut: DP_RippleCarryAdder32Bit_21364066 PORT MAP (
       A => A_TB,
       B => B_TB,
       C_IN => C_IN_TB,
       SUM => SUM_TB,
       C_OUT => C_OUT_TB,
       V => V_TB
       );

 stim_proc: process
   begin
   
   wait for 20 ns;
   
   --Test Vector A--
   		
      A_TB <= "00000000000000000000000000000001";
      B_TB <= "00000000000000000000000000000010";
      C_IN_TB <= '0';
      
      wait for 20 ns;	
      
    --Test Vector B--
      A_TB <= "01000000000000000000000000000000";
      B_TB <= "11000000000000000000000000000000";
      C_IN_TB <= '0';     

      wait for 20 ns;
      	
     --Test Vector C--
      A_TB <= "11000000000000000000000000000000";
      B_TB <= "01100000000000000000000000000000";
      C_IN_TB <= '1'; 
      
      wait for 20 ns;	
    
    --Test Vector D--
      A_TB <= "10100000000000000000000000000000";
      B_TB <= "11000000000000000000000000000000";
      C_IN_TB <= '0';

      wait for 20 ns;

    --Test Vector E--
      A_TB <= "10100000000000000000000000000000";
      B_TB <= "11100000000000000000000000000000";
      C_IN_TB <= '0';

      wait for 20 ns;
      
    --Test Vector F--
      A_TB <= "00100000000000000000000000000000";
      B_TB <= "01100000000000000000000000000000";
      C_IN_TB <= '0';  
      
      wait for 20 ns;

    --Test Vector G (Student ID, to set C)--
      	
      A_TB <= "00000001010001011111110101100010";
      B_TB <= "11111111000000000000000000000000";
      C_IN_TB <= '0'; 

       --res: 00000000010001011111110101100010
      --1    00000000010001011111110101100010

      wait for 20 ns;

    --Test Vector H (Student ID, to set V)--

      A_TB <= "00000001010001011111110101100010";
      B_TB <= "01111111000000000000000000000111";
      C_IN_TB <= '0';

       --res: 10000000010001011111110101101001
      --     10000000010001011111110101101001
     
      wait for 20 ns;	

     --Test Vector I--
     A_TB <= "11111111111111111111111111111110";
     B_TB <= "00000000000000000000000000000001";
     C_IN_TB <= '1';  

     --wc prop delay-- 
     wait for 300ns;


   end process;
end Sim;

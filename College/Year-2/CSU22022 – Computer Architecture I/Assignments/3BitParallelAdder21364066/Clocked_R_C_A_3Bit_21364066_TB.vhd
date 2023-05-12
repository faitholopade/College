----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 07.10.2022 21:39
-- Design Name: 
-- Module Name: Clocked_R_C_A_3Bit_21364066_TB  - sim
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

entity Clocked_R_C_A_3Bit_21364066_TB is
--  Port ( );
end Clocked_R_C_A_3Bit_21364066_TB;

architecture Sim of Clocked_R_C_A_3Bit_21364066_TB is

component Clocked_R_C_A_3Bit_21364066
    PORT(
      Source_A : in STD_LOGIC_VECTOR (2 downto 0);
      EN_Source_A : in STD_LOGIC;
      Source_B : in STD_LOGIC_VECTOR (2 downto 0);
      EN_Source_B : in STD_LOGIC;
      C_IN : in STD_LOGIC;
      CLOCK : in STD_LOGIC;
      EN_SUM : in STD_LOGIC;
      SUM : out STD_LOGIC_VECTOR (2 downto 0);
      C : out STD_LOGIC;
      V : out STD_LOGIC
         ); 
end component;  

--Inputs

    signal Source_A_TB : std_logic_vector(2 downto 0):= (others => '0'); -- initializing to 0
    signal EN_Source_A_TB : std_logic:= '0';   -- enable signal for input (i.e. telling system to include/ exclude this input)
    signal Source_B_TB : std_logic_vector(2 downto 0):= (others => '0'); -- initializing to 0
    signal EN_Source_B_TB : std_logic:= '0';
    signal C_IN_TB : std_logic:= '0';
    signal CLOCK_TB : std_logic:= '0';
    signal EN_SUM_TB : std_logic:= '0';
     
--Outputs

    signal SUM_TB : std_logic_vector(2 downto 0):= (others => '0'); -- initializing to 0
    signal C_TB : std_logic:= '0';
    signal V_TB : std_logic:= '0';
    
    constant PERIOD : time := 60ns; -- period of wave for clock (similar to sin wave)

begin

	-- Instantiate the Unit Under Test (UUT)
	
   uut: Clocked_R_C_A_3Bit_21364066 port map (
      Source_A => Source_A_TB,
      EN_Source_A => EN_Source_A_TB,
      Source_B => Source_B_TB,
      EN_Source_B => EN_Source_B_TB,
      C_IN => C_IN_TB,
      CLOCK => CLOCK_TB,
      EN_SUM => EN_SUM_TB,
      SUM => SUM_TB,
      C => C_TB,
      V => V_TB
        );
        
   CLOCK_TB <= not CLOCK_TB after PERIOD/2;  -- clock on and clock off (wave for clock)
   
      stim_proc: process

   begin
   

   
      wait until CLOCK_TB'event and CLOCK_TB='1'; -- waiting for rising edge of clock

         EN_Source_A_TB <= '1' after PERIOD/2;        -- operations carried out on rising edge
         EN_Source_B_TB <= '1' after PERIOD/2;
         EN_SUM_TB <= '1' after PERIOD/2;
         C_IN_TB <= '0' after PERIOD/2;   -- setting carry in 
         
      wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector A 

         -- these test values will result in overflow due to sign bit being different to the sign 
         -- of values we initially added 
      

         Source_A_TB <= "101" after PERIOD/2;
         Source_B_TB <= "110" after PERIOD/2;
         C_IN_TB <= '0' after PERIOD/2;

      wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector B 

         Source_A_TB <= "101" after PERIOD/2;
         Source_B_TB <= "111" after PERIOD/2;
         C_IN_TB <= '0' after PERIOD/2;
         
      wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector C
 
         Source_A_TB <= "001" after PERIOD/2;
         Source_B_TB <= "011" after PERIOD/2;
         C_IN_TB <= '0' after PERIOD/2;
         
      wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector D 


         Source_A_TB <= "001" after PERIOD/2;
         Source_B_TB <= "010" after PERIOD/2;
         C_IN_TB <= '0' after PERIOD/2;

      wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector E

         Source_A_TB <= "010" after PERIOD/2;
         Source_B_TB <= "110" after PERIOD/2;
         C_IN_TB <= '0' after PERIOD/2;
         
      wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector F 

         Source_A_TB <= "110"  after PERIOD/2;
         Source_B_TB <= "001" after PERIOD/2;
         C_IN_TB <= '1' after PERIOD/2;
      
      wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector G
   
         Source_A_TB <= "111"  after PERIOD/2;
         Source_B_TB <= "111" after PERIOD/2;
         C_IN_TB <= '1' after PERIOD/2;
      
      wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector H
   
         Source_A_TB <= "000"  after PERIOD/2;
         Source_B_TB <= "000" after PERIOD/2;
         C_IN_TB <= '1' after PERIOD/2;

      wait until CLOCK_TB'event and CLOCK_TB='1';
      
      -- Test vector I 
   
         Source_A_TB <= "101"  after PERIOD/2;
         Source_B_TB <= "110" after PERIOD/2;
         C_IN_TB <= '1' after PERIOD/2;


   end process;

end Sim;

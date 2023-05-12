----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 10.11.2022 18:28:51
-- Design Name: 
-- Module Name: BM_SeqRegDec_Multipier_21364066_TB - Simulation
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

entity BM_SeqRegDec_Multipier_21364066_TB is
--  Port ( ); we don't need ports 
end BM_SeqRegDec_Multipier_21364066_TB;

architecture Simulation of BM_SeqRegDec_Multipier_21364066_TB is

-- Component Declaration for the Unit Under Test (UUT)
component BM_SeqRegDec_Multipier_21364066
Port ( 
      Clock : in STD_LOGIC;
      FlipFlopReset : in STD_LOGIC;
      RegB : in STD_LOGIC_VECTOR (4 downto 0);
      RegQ : in STD_LOGIC_VECTOR (4 downto 0);
      LoadB : in STD_LOGIC;
      LoadQ : in STD_LOGIC;
      G : in STD_LOGIC;
      Product : out STD_LOGIC_VECTOR (9 downto 0)
      );
end component;

--Inputs

    signal Clock_TB : STD_LOGIC := '0';
    signal FlipFlopReset_TB : STD_LOGIC := '0';
    signal RegB_TB : STD_LOGIC_VECTOR (4 downto 0) := (others => '0');
    signal RegQ_TB : STD_LOGIC_VECTOR (4 downto 0) := (others => '0');
    signal LoadB_TB : STD_LOGIC := '0';
    signal LoadQ_TB : STD_LOGIC := '0';
    Signal G_TB : STD_LOGIC := '0';
      
--Outputs

    signal Product_TB : STD_LOGIC_VECTOR (9 downto 0):= (others => '0');
    
    constant PERIOD : time := 100ns;
      
begin

-- Instantiate the Unit Under Test (UUT)
	
   uut: BM_SeqRegDec_Multipier_21364066 port map (     
      Clock  => Clock_TB,
      FlipFlopReset => FlipFlopReset_TB,
      RegB => RegB_TB,
      RegQ => RegQ_TB,
      LoadB => LoadB_TB,
      LoadQ => LoadQ_TB,
      G => G_TB,
      Product => Product_TB
      );
        
   Clock_TB <= not Clock_TB after PERIOD/2;
   
     stim_proc: process

   begin
   
      -- Test vector 00 - Reset All Inputs
   
      wait until Clock_TB'event and Clock_TB='1';

         FlipFlopReset_TB <= '0' after PERIOD/4;
        
         RegB_TB <= "11011" after PERIOD/4; -- Mutiplicand
         RegQ_TB <= "10010" after PERIOD/4; -- Multiplier 
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
                  
         G_TB <= '0' after PERIOD/4;
         
      -- Test vector 01 - Reset the Flip Flops M0 and M1
   
      wait until Clock_TB'event and Clock_TB='1';

         FlipFlopReset_TB <= '1' after PERIOD/4;
        
         RegB_TB <= "11011" after PERIOD/4; -- Mutiplicand
         RegQ_TB <= "10010" after PERIOD/4; -- Multiplier 
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
                  
         G_TB <= '0' after PERIOD/4;
         
      -- Test vector 02 - Load Register B with the Mutiplicand
   
      wait until Clock_TB'event and Clock_TB='1';

         FlipFlopReset_TB <= '0' after PERIOD/4;
        
         RegB_TB <= "11011" after PERIOD/4; -- Mutiplicand
         RegQ_TB <= "10010" after PERIOD/4; -- Multiplier 
         LoadB_TB <= '1' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
                  
         G_TB <= '0' after PERIOD/4;
         
      -- Test vector 03 - Load Register Q with the Multiplier
   
      wait until Clock_TB'event and Clock_TB='1';

         FlipFlopReset_TB <= '0' after PERIOD/4;
        
         RegB_TB <= "11011" after PERIOD/4; -- Mutiplicand
         RegQ_TB <= "10010" after PERIOD/4; -- Multiplier 
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '1' after PERIOD/4;
                  
         G_TB <= '0' after PERIOD/4;
         
      -- Test vector 04 - Start the Binary Multipier with the G signal
   
      wait until Clock_TB'event and Clock_TB='1';

         FlipFlopReset_TB <= '0' after PERIOD/4;
        
         RegB_TB <= "11011" after PERIOD/4; -- Mutiplicand
         RegQ_TB <= "10010" after PERIOD/4; -- Multiplier 
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
                  
         G_TB <= '1' after PERIOD/4;
         
      -- Test vector 05 - Clock cycle 1
   
      wait until Clock_TB'event and Clock_TB='1';

         FlipFlopReset_TB <= '0' after PERIOD/4;
        
         RegB_TB <= "11011" after PERIOD/4; -- Mutiplicand
         RegQ_TB <= "10010" after PERIOD/4; -- Multiplier 
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
                  
         G_TB <= '0' after PERIOD/4; 
 
       -- Test vector 06 - Clock cycle 2
   
      wait until Clock_TB'event and Clock_TB='1';

         FlipFlopReset_TB <= '0' after PERIOD/4;
        
         RegB_TB <= "11011" after PERIOD/4; -- Mutiplicand
         RegQ_TB <= "10010" after PERIOD/4; -- Multiplier 
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
                  
         G_TB <= '0' after PERIOD/4; 
         
       -- Test vector 07 - Clock cycle 3
   
      wait until Clock_TB'event and Clock_TB='1';

         FlipFlopReset_TB <= '0' after PERIOD/4;
        
         RegB_TB <= "11011" after PERIOD/4; -- Mutiplicand
         RegQ_TB <= "10010" after PERIOD/4; -- Multiplier 
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
                  
         G_TB <= '0' after PERIOD/4; 
         
       -- Test vector 08 - Clock cycle 4
   
      wait until Clock_TB'event and Clock_TB='1';

         FlipFlopReset_TB <= '0' after PERIOD/4;
        
         RegB_TB <= "11011" after PERIOD/4; -- Mutiplicand
         RegQ_TB <= "10010" after PERIOD/4; -- Multiplier 
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
                  
         G_TB <= '0' after PERIOD/4; 
         
       -- Test vector 09 - Clock cycle 5
   
      wait until Clock_TB'event and Clock_TB='1';

         FlipFlopReset_TB <= '0' after PERIOD/4;
        
         RegB_TB <= "11011" after PERIOD/4; -- Mutiplicand
         RegQ_TB <= "10010" after PERIOD/4; -- Multiplier 
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
                  
         G_TB <= '0' after PERIOD/4; 
 
        -- Test vector 10 - Clock cycle 6
   
      wait until Clock_TB'event and Clock_TB='1';

         FlipFlopReset_TB <= '0' after PERIOD/4;
        
         RegB_TB <= "11011" after PERIOD/4; -- Mutiplicand
         RegQ_TB <= "10010" after PERIOD/4; -- Multiplier 
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
                  
         G_TB <= '0' after PERIOD/4; 
         
       -- Test vector 11 - Clock cycle 7
   
      wait until Clock_TB'event and Clock_TB='1';

         FlipFlopReset_TB <= '0' after PERIOD/4;
        
         RegB_TB <= "11011" after PERIOD/4; -- Mutiplicand
         RegQ_TB <= "10010" after PERIOD/4; -- Multiplier 
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
                  
         G_TB <= '0' after PERIOD/4; 
         
       -- Test vector 12 - Clock cycle 8
   
      wait until Clock_TB'event and Clock_TB='1';

         FlipFlopReset_TB <= '0' after PERIOD/4;
        
         RegB_TB <= "11011" after PERIOD/4; -- Mutiplicand
         RegQ_TB <= "10010" after PERIOD/4; -- Multiplier 
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
                  
         G_TB <= '0' after PERIOD/4; 
 
        -- Test vector 13 - Clock cycle 9
   
      wait until Clock_TB'event and Clock_TB='1';

         FlipFlopReset_TB <= '0' after PERIOD/4;
        
         RegB_TB <= "11011" after PERIOD/4; -- Mutiplicand
         RegQ_TB <= "10010" after PERIOD/4; -- Multiplier 
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
                  
         G_TB <= '0' after PERIOD/4;    
             
   end process;         

end Simulation;

----------------------------------------------------------------------------------
-- Company: Trinity College Dublin 
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 31.10.2022 09:55:55
-- Design Name: 
-- Module Name: BM_Datapath_21364066_TB - Simulation
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

entity BM_Datapath_21364066_TB is
--  Port ( );
end BM_Datapath_21364066_TB;

architecture Simulation of BM_Datapath_21364066_TB is

-- Component Declaration for the Unit Under Test (UUT)
component BM_Datapath_21364066
Port ( 
      RegB : in STD_LOGIC_VECTOR (4 downto 0);
      RegQ : in STD_LOGIC_VECTOR (4 downto 0);
      LoadB : in STD_LOGIC;
      LoadQ : in STD_LOGIC;
      Clr : in STD_LOGIC;
      Clock : in STD_LOGIC;
      Shift : in STD_LOGIC;
      Add : in STD_LOGIC;
      Product : out STD_LOGIC_VECTOR (9 downto 0);
      Q0 : out STD_LOGIC
      );
end component;

--Inputs

    signal RegB_TB : STD_LOGIC_VECTOR (4 downto 0) := (others => '0');
    signal RegQ_TB : STD_LOGIC_VECTOR (4 downto 0) := (others => '0');
    signal LoadB_TB : STD_LOGIC := '0';
    signal LoadQ_TB : STD_LOGIC := '0';
    signal Clr_TB : STD_LOGIC := '0';
    signal Clock_TB : STD_LOGIC := '0';
    signal Shift_TB : STD_LOGIC := '0';
    signal Add_TB : STD_LOGIC := '0';

--Outputs

    signal Product_TB : STD_LOGIC_VECTOR (9 downto 0):= (others => '0');
    signal Q0_TB : STD_LOGIC := '0';
    
    constant PERIOD : time := 100ns;
      
begin

-- Instantiate the Unit Under Test (UUT)
	
   uut: BM_Datapath_21364066 port map (     
      RegB => RegB_TB,
      RegQ => RegQ_TB,
      LoadB => LoadB_TB,
      LoadQ => LoadQ_TB,
      Clr => Clr_TB,
      Clock => Clock_TB,
      Shift => Shift_TB,
      Add => Add_TB,
      Product => Product_TB,
      Q0 => Q0_TB  
      );
        
   Clock_TB <= not Clock_TB after PERIOD/2;
   
     stim_proc: process

   begin
   
      -- Test vector 01 - Reset All Inputs
   
      wait until Clock_TB'event and Clock_TB='1';
        
         RegB_TB <= "00000" after PERIOD/4; -- Mutiplicand "10111"
         RegQ_TB <= "00000" after PERIOD/4; -- Multiplier "10011"
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
         
         Clr_TB <= '0' after PERIOD/4;
         
         Shift_TB <= '0' after PERIOD/4;
         Add_TB <= '0' after PERIOD/4;
         
      -- Test vector 02 - Clear all registers 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         RegB_TB <= "00000" after PERIOD/4; -- Mutiplicand "10111" 
         RegQ_TB <= "00000" after PERIOD/4; -- Multiplier "10011"
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
         
         Clr_TB <= '1' after PERIOD/4;
         
         Shift_TB <= '0' after PERIOD/4;
         Add_TB <= '0' after PERIOD/4;
                  
      -- Test vector 03 - Load Register B with Mutiplicand 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         RegB_TB <= "10111" after PERIOD/4; -- Mutiplicand "10111"
         RegQ_TB <= "00000" after PERIOD/4; -- Multiplier "10011"
         LoadB_TB <= '1' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
         
         Clr_TB <= '0' after PERIOD/4;
         
         Shift_TB <= '0' after PERIOD/4;
         Add_TB <= '0' after PERIOD/4;
                 
      -- Test vector 04 - Load Register Q with Multiplier
   
      wait until Clock_TB'event and Clock_TB='1';
        
         RegB_TB <= "00000" after PERIOD/4; -- Mutiplicand "10111"
         RegQ_TB <= "10011" after PERIOD/4; -- Multiplier "10011"
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '1' after PERIOD/4;
         
         Clr_TB <= '0' after PERIOD/4;
         
         Shift_TB <= '0' after PERIOD/4;
         Add_TB <= '0' after PERIOD/4;
         
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

      -- Test vector - Add - Clock Cycle 1 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         RegB_TB <= "10111" after PERIOD/4; -- Mutiplicand "10111"
         RegQ_TB <= "10011" after PERIOD/4; -- Multiplier "10011"
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
         
         Clr_TB <= '0' after PERIOD/4;
         
         
         Add_TB <= '1' after PERIOD/4;  
         Shift_TB <= '0' after PERIOD/4;

      -- Test vector - Shift - Clock Cycle 2 - P0
   
      wait until Clock_TB'event and Clock_TB='1';
        
        RegB_TB <= "10111" after PERIOD/4; -- Mutiplicand "10111"
         RegQ_TB <= "10011" after PERIOD/4; -- Multiplier "10011"
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
         
         Clr_TB <= '0' after PERIOD/4;
         
         Shift_TB <= '1' after PERIOD/4;
         Add_TB <= '0' after PERIOD/4;           

      -- Test vector - Add - Clock Cycle 3 
   
      wait until Clock_TB'event and Clock_TB='1';
        
          RegB_TB <= "10111" after PERIOD/4; -- Mutiplicand "10111"
         RegQ_TB <= "10011" after PERIOD/4; -- Multiplier "10011"
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
         
         Clr_TB <= '0' after PERIOD/4;
         
         Shift_TB <= '0' after PERIOD/4;
         Add_TB <= '1' after PERIOD/4;  

      -- Test vector - Shift - Clock Cycle 4 - P0+P1 x2^1
   
      wait until Clock_TB'event and Clock_TB='1';
        
          RegB_TB <= "10111" after PERIOD/4; -- Mutiplicand "10111"
         RegQ_TB <= "10011" after PERIOD/4; -- Multiplier "10011"
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
         
         Clr_TB <= '0' after PERIOD/4;
         
         Shift_TB <= '1' after PERIOD/4;
         Add_TB <= '0' after PERIOD/4;           
         
      -- Test vector - Shift - Clock Cycle 5 - P0+P1 x2^1+P2 x2^2
   
      wait until Clock_TB'event and Clock_TB='1';
        
         RegB_TB <= "10111" after PERIOD/4; -- Mutiplicand "10111"
         RegQ_TB <= "10011" after PERIOD/4; -- Multiplier "10011"
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
         
         Clr_TB <= '0' after PERIOD/4;
         
         Shift_TB <= '1' after PERIOD/4;
         Add_TB <= '0' after PERIOD/4; 
         
       -- Test vector - Shift - Clock Cycle 6 - P0+P1 x2^1+P2 x2^2+P3 x2^3
   
      wait until Clock_TB'event and Clock_TB='1';
        
         RegB_TB <= "10111" after PERIOD/4; -- Mutiplicand "10111"
         RegQ_TB <= "10011" after PERIOD/4; -- Multiplier "10011"
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
         
         Clr_TB <= '0' after PERIOD/4;
         
         Shift_TB <= '1' after PERIOD/4;
         Add_TB <= '0' after PERIOD/4;         

      -- Test vector - Add - Clock Cycle 7 
   
      wait until Clock_TB'event and Clock_TB='1';
        
      RegB_TB <= "10111" after PERIOD/4; -- Mutiplicand "10111"
         RegQ_TB <= "10011" after PERIOD/4; -- Multiplier "10011"
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
         
         Clr_TB <= '0' after PERIOD/4;
         
         Shift_TB <= '0' after PERIOD/4;
         Add_TB <= '1' after PERIOD/4;  

      -- Test vector - Shift - Clock Cycle 8 - P0+P1 x2^1+P2 x2^2+P3 x2^3+P1 x2^4
   
      wait until Clock_TB'event and Clock_TB='1';
        
         RegB_TB <= "10111" after PERIOD/4; -- Mutiplicand "10111"
         RegQ_TB <= "10011" after PERIOD/4; -- Multiplier "10011"
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
         
         Clr_TB <= '0' after PERIOD/4;
         
         Shift_TB <= '1' after PERIOD/4;
         Add_TB <= '0' after PERIOD/4;           

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
            
      -- Test vector - Clear all registers 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         RegB_TB <= "10111" after PERIOD/4; -- Mutiplicand "10111"
         RegQ_TB <= "10011" after PERIOD/4; -- Multiplier "10011"
         LoadB_TB <= '0' after PERIOD/4;
         LoadQ_TB <= '0' after PERIOD/4;
         
         Clr_TB <= '1' after PERIOD/4;
         
         Shift_TB <= '0' after PERIOD/4;
         Add_TB <= '0' after PERIOD/4;             
                                                                                                  
   end process;



end Simulation;

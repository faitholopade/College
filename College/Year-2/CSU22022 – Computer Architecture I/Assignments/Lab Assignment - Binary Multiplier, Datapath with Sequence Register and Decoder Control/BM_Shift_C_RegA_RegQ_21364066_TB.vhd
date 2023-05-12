----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 30.10.2022 21:21:57
-- Design Name: 
-- Module Name: BM_Shift_C_RegA_RegQ_21364066_TB - Simulation
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

entity BM_Shift_C_RegA_RegQ_21364066_TB is
--  Port ( ); We don't need ports 
end BM_Shift_C_RegA_RegQ_21364066_TB;

architecture Simulation of BM_Shift_C_RegA_RegQ_21364066_TB is

-- Component Declaration for the Unit Under Test (UUT)
component BM_Shift_C_RegA_RegQ_21364066
port ( 
      C : in STD_LOGIC;
      A_In : in STD_LOGIC_VECTOR (4 downto 0);
      Q_In : in STD_LOGIC_VECTOR (4 downto 0);
      Clock : in STD_LOGIC;
      LoadCRegA : in STD_LOGIC;
      LoadRegQ : in STD_LOGIC;
      ClrCRegA : in STD_LOGIC;
      Shift : in STD_LOGIC;
      A_Out : out STD_LOGIC_VECTOR (4 downto 0);
      Product : out STD_LOGIC_VECTOR (9 downto 0)
      );
end component;

--Inputs

    signal C_TB : STD_LOGIC:= '0';
    signal A_In_TB : STD_LOGIC_VECTOR (4 downto 0):= (others => '0');
    signal Q_In_TB : STD_LOGIC_VECTOR (4 downto 0):= (others => '0');
    signal Clock_TB : STD_LOGIC:= '0';
    signal LoadCRegA_TB : STD_LOGIC:= '0';
    signal LoadRegQ_TB : STD_LOGIC:= '0';
    signal ClrCRegA_TB : STD_LOGIC:= '0';
    signal Shift_TB : STD_LOGIC:= '0';

--Outputs

    signal A_Out_TB : STD_LOGIC_VECTOR (4 downto 0):= (others => '0');
    signal Product_TB : STD_LOGIC_VECTOR (9 downto 0):= (others => '0');
        
    constant PERIOD : time := 60ns;
    
begin

	-- Instantiate the Unit Under Test (UUT)
	
   uut: BM_Shift_C_RegA_RegQ_21364066 port map (     
      C => C_TB,
      A_In => A_In_TB,
      Q_In => Q_In_TB,
      Clock => Clock_TB,
      LoadCRegA => LoadCRegA_TB,
      LoadRegQ => LoadRegQ_TB,
      ClrCRegA => ClrCRegA_TB,
      Shift => Shift_TB,
      A_Out => A_Out_TB,
      Product => Product_TB    
      );
        
   Clock_TB <= not Clock_TB after PERIOD/2;
   
     stim_proc: process

   begin
   
      -- Test vector 01 - Reset All Inputs
   
      wait until Clock_TB'event and Clock_TB='1';
        
         LoadCRegA_TB <= '0' after PERIOD/4;
         LoadRegQ_TB <= '0' after PERIOD/4;
         ClrCRegA_TB <= '0' after PERIOD/4;
         Shift_TB <= '0' after PERIOD/4;
         
         C_TB <= '1' after PERIOD/4;
         A_In_TB <= "10101" after PERIOD/4;
         Q_In_TB <= "01010" after PERIOD/4;         
   
      -- Test vector 02 - Clear C, Register A 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         LoadCRegA_TB <= '0' after PERIOD/4;
         LoadRegQ_TB <= '0' after PERIOD/4;
         ClrCRegA_TB <= '1' after PERIOD/4;
         Shift_TB <= '0' after PERIOD/4;
         
         C_TB <= '0' after PERIOD/4;
         A_In_TB <= "10101" after PERIOD/4;
         Q_In_TB <= "01010" after PERIOD/4;            
 
      -- Test vector 03 -  Load Register Q and C
   
      wait until Clock_TB'event and Clock_TB='1';
        
         LoadCRegA_TB <= '0' after PERIOD/4;
         LoadRegQ_TB <= '1' after PERIOD/4;
         ClrCRegA_TB <= '0' after PERIOD/4;
         Shift_TB <= '0' after PERIOD/4;
         
         C_TB <= '1' after PERIOD/4;
         A_In_TB <= "10101" after PERIOD/4;
         Q_In_TB <= "01010" after PERIOD/4;            
 
      -- Test vector 04 -  Load C and Register A  
   
      wait until Clock_TB'event and Clock_TB='1';
        
         LoadCRegA_TB <= '1' after PERIOD/4;
         LoadRegQ_TB <= '0' after PERIOD/4;
         ClrCRegA_TB <= '0' after PERIOD/4;
         Shift_TB <= '0' after PERIOD/4;
         
         C_TB <= '0' after PERIOD/4;
         A_In_TB <= "10101" after PERIOD/4;
         Q_In_TB <= "01010" after PERIOD/4;
         
      -- ++++++++++++++++++++++++++++++++++++++++++++
         
      -- Test vector 05 -  Shift 1
   
      wait until Clock_TB'event and Clock_TB='1';
        
         LoadCRegA_TB <= '0' after PERIOD/4;
         LoadRegQ_TB <= '0' after PERIOD/4;
         ClrCRegA_TB <= '0' after PERIOD/4;
         Shift_TB <= '1' after PERIOD/4;
         
         C_TB <= '0' after PERIOD/4;
         A_In_TB <= "10101" after PERIOD/4;
         Q_In_TB <= "01010" after PERIOD/4;         
         
       -- Test vector 05 -  Shift 2
   
      wait until Clock_TB'event and Clock_TB='1';
        
         LoadCRegA_TB <= '0' after PERIOD/4;
         LoadRegQ_TB <= '0' after PERIOD/4;
         ClrCRegA_TB <= '0' after PERIOD/4;
         Shift_TB <= '1' after PERIOD/4;
         
         C_TB <= '0' after PERIOD/4;
         A_In_TB <= "10101" after PERIOD/4;
         Q_In_TB <= "01010" after PERIOD/4;         
                    
       -- Test vector 05 -  Shift 3 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         LoadCRegA_TB <= '0' after PERIOD/4;
         LoadRegQ_TB <= '0' after PERIOD/4;
         ClrCRegA_TB <= '0' after PERIOD/4;
         Shift_TB <= '1' after PERIOD/4;
         
         C_TB <= '0' after PERIOD/4;
         A_In_TB <= "10101" after PERIOD/4;
         Q_In_TB <= "01010" after PERIOD/4;    
         
       -- Test vector 05 -  Shift 4
   
      wait until Clock_TB'event and Clock_TB='1';
        
         LoadCRegA_TB <= '0' after PERIOD/4;
         LoadRegQ_TB <= '0' after PERIOD/4;
         ClrCRegA_TB <= '0' after PERIOD/4;
         Shift_TB <= '1' after PERIOD/4;
         
         C_TB <= '0' after PERIOD/4;
         A_In_TB <= "10101" after PERIOD/4;
         Q_In_TB <= "01010" after PERIOD/4;    
         
       -- Test vector 05 -  Shift 5
   
      wait until Clock_TB'event and Clock_TB='1';
        
         LoadCRegA_TB <= '0' after PERIOD/4;
         LoadRegQ_TB <= '0' after PERIOD/4;
         ClrCRegA_TB <= '0' after PERIOD/4;
         Shift_TB <= '1' after PERIOD/4;
         
         C_TB <= '0' after PERIOD/4;
         A_In_TB <= "10101" after PERIOD/4;
         Q_In_TB <= "01010" after PERIOD/4;    
         
       -- Test vector 05 -  Shift 6
   
      wait until Clock_TB'event and Clock_TB='1';
        
         LoadCRegA_TB <= '0' after PERIOD/4;
         LoadRegQ_TB <= '0' after PERIOD/4;
         ClrCRegA_TB <= '0' after PERIOD/4;
         Shift_TB <= '1' after PERIOD/4;
         
         C_TB <= '0' after PERIOD/4;
         A_In_TB <= "10101" after PERIOD/4;
         Q_In_TB <= "01010" after PERIOD/4;    
         
       -- Test vector 05 -  Shift 7
   
      wait until Clock_TB'event and Clock_TB='1';
        
         LoadCRegA_TB <= '0' after PERIOD/4;
         LoadRegQ_TB <= '0' after PERIOD/4;
         ClrCRegA_TB <= '0' after PERIOD/4;
         Shift_TB <= '1' after PERIOD/4;
         
         C_TB <= '0' after PERIOD/4;
         A_In_TB <= "10101" after PERIOD/4;
         Q_In_TB <= "01010" after PERIOD/4;    
         
       -- Test vector 05 -  Shift 8
   
      wait until Clock_TB'event and Clock_TB='1';
        
         LoadCRegA_TB <= '0' after PERIOD/4;
         LoadRegQ_TB <= '0' after PERIOD/4;
         ClrCRegA_TB <= '0' after PERIOD/4;
         Shift_TB <= '1' after PERIOD/4;
         
         C_TB <= '0' after PERIOD/4;
         A_In_TB <= "10101" after PERIOD/4;
         Q_In_TB <= "01010" after PERIOD/4;    
         
       -- Test vector 05 -  Shift 9
   
      wait until Clock_TB'event and Clock_TB='1';
        
         LoadCRegA_TB <= '0' after PERIOD/4;
         LoadRegQ_TB <= '0' after PERIOD/4;
         ClrCRegA_TB <= '0' after PERIOD/4;
         Shift_TB <= '1' after PERIOD/4;
         
         C_TB <= '0' after PERIOD/4;
         A_In_TB <= "10101" after PERIOD/4;
         Q_In_TB <= "01010" after PERIOD/4;    
         
       -- Test vector 05 -  Shift 10
   
      wait until Clock_TB'event and Clock_TB='1';
        
         LoadCRegA_TB <= '0' after PERIOD/4;
         LoadRegQ_TB <= '0' after PERIOD/4;
         ClrCRegA_TB <= '0' after PERIOD/4;
         Shift_TB <= '1' after PERIOD/4;
         
         C_TB <= '0' after PERIOD/4;
         A_In_TB <= "10101" after PERIOD/4;
         Q_In_TB <= "01010" after PERIOD/4;  
                                                                                   
   end process;

end Simulation;

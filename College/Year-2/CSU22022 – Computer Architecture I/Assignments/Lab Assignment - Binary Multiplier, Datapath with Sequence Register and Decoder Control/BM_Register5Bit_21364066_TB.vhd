----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 30.10.2022 22:39:50
-- Design Name: 
-- Module Name: BM_Register5Bit_21364066_TB - Simulation
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

entity BM_Register5Bit_21364066_TB is
--  Port ( ); We don't need ports  
end BM_Register5Bit_21364066_TB;

architecture Simulation of BM_Register5Bit_21364066_TB is

-- Component Declaration for the Unit Under Test (UUT)
component BM_Register5Bit_21364066
port ( 
      D : in STD_LOGIC_VECTOR (4 downto 0);
      Clr  : in STD_LOGIC;
      Load : in STD_LOGIC;
      Clock : in STD_LOGIC;
      Q : out STD_LOGIC_VECTOR (4 downto 0) 
      );
end component;

--Inputs

    signal D_TB : STD_LOGIC_VECTOR (4 downto 0):= (others => '0');
    signal Clr_TB : STD_LOGIC:= '0';
    signal Load_TB : STD_LOGIC:= '0';
    signal Clock_TB : STD_LOGIC:= '0';
    
--Outputs

    signal Q_TB : std_logic_vector(4 downto 0):= (others => '0');
    
    constant PERIOD : time := 10ns;
   
begin

	-- Instantiate the Unit Under Test (UUT)
	
   uut: BM_Register5Bit_21364066 port map (
          D => D_TB,
          Clr => Clr_TB,
          Load => Load_TB,
          Clock => Clock_TB,
          Q => Q_TB           
        );
        
   Clock_TB <= not Clock_TB after PERIOD/2;
   
   stim_proc: process

   begin
   
      -- Reset all 
      wait until Clock_TB'event and Clock_TB = '1';
      
      Clr_TB <= '0' after PERIOD/4;
      Load_TB <= '0' after PERIOD/4;
      D_TB <= "11111" after PERIOD/4; 
      
      -- Reset all 
      wait until Clock_TB'event and Clock_TB = '1';
      
      Clr_TB <= '1' after PERIOD/4;
      Load_TB <= '0' after PERIOD/4;
      D_TB <= "11111" after PERIOD/4; 
      
      -- Reset all 
      wait until Clock_TB'event and Clock_TB = '1';
      
      Clr_TB <= '0' after PERIOD/4;
      Load_TB <= '1' after PERIOD/4;
      D_TB <= "11111" after PERIOD/4;       

   end process;        

end Simulation;

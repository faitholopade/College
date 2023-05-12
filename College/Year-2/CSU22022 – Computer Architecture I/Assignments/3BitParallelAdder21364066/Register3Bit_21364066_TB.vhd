----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 06.10.2022 20:41:58
-- Design Name: 
-- Module Name: Register3Bit_21364066_TB - Sim
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

entity Register3Bit_21364066_TB is
--  Port ( ); We don't need ports
end Register3Bit_21364066_TB;

architecture Sim of Register3Bit_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component Register3Bit_21364066
port ( 
    D : in STD_LOGIC_VECTOR (2 downto 0);
    CLK : in STD_LOGIC;
    Load : in STD_LOGIC;
    Q : out STD_LOGIC_VECTOR (2 downto 0)
      );
end component;

--Inputs

    signal D_TB : std_logic_vector(2 downto 0):= (others => '0');
    signal CLK_TB : std_logic:= '0';
    signal Load_TB : std_logic:= '0';
    
--Outputs

    signal Q_TB : std_logic_vector(2 downto 0):= (others => '0');
    
    constant PERIOD : time := 10ns;
   

begin

	-- Instantiate the Unit Under Test (UUT)
	
   uut: Register3Bit_21364066 port map (
          D => D_TB,
          CLK => CLK_TB,
          Load => Load_TB,
          Q => Q_TB
        );
        
   CLK_TB <= not CLK_TB after PERIOD/2;
   
   stim_proc: process

   begin
   
      wait until CLK_TB'event and CLK_TB='1';
      Load_TB <= '0';
      D_TB <= "001";
      
      wait until CLK_TB'event and CLK_TB='1';
      Load_TB <= '1';
      
      wait until CLK_TB'event and CLK_TB='1';
      Load_TB <= '0';
      D_TB <= "010";
      
      wait until CLK_TB'event and CLK_TB='1';
      Load_TB <= '1';
      
      wait until CLK_TB'event and CLK_TB='1';
      Load_TB <= '0';
      D_TB <= "011";
      
      wait until CLK_TB'event and CLK_TB='1';
      Load_TB <= '1';
      
      wait until CLK_TB'event and CLK_TB='1';
      Load_TB <= '0';
      D_TB <= "111";
      
      wait until CLK_TB'event and CLK_TB='1';
      Load_TB <= '1';
      
   end process;
   
        

end Sim;

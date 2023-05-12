----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 20.10.2022 23:19:00
-- Design Name: 
-- Module Name: RF_TempDestReg_Decoder_21364066_TB - Sim
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

entity RF_TempDestReg_Decoder_21364066_TB is
--  Port ( ); We don't need ports
end RF_TempDestReg_Decoder_21364066_TB;

architecture Sim of RF_TempDestReg_Decoder_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component RF_TempDestReg_Decoder_21364066
port ( 
        A : in std_logic_vector(3 downto 0);
        
        Q00 : out std_logic;
        Q01 : out std_logic;
        Q02 : out std_logic;
        Q03 : out std_logic;
        Q04 : out std_logic;
        Q05 : out std_logic;
        Q06 : out std_logic;
        Q07 : out std_logic;
        Q08 : out std_logic;
        Q09 : out std_logic;
        Q10 : out std_logic;
        Q11 : out std_logic;
        Q12 : out std_logic;
        Q13 : out std_logic;
        Q14 : out std_logic;
        Q15 : out std_logic
        
      );
end component;

--Inputs
    signal A_TB : std_logic_vector(3 downto 0) := (others => '0');
         
  --Outputs
    signal Q00_TB : std_logic := '0';
    signal Q01_TB : std_logic := '0';
    signal Q02_TB : std_logic := '0';
    signal Q03_TB : std_logic := '0';
    signal Q04_TB : std_logic := '0';
    signal Q05_TB : std_logic := '0';
    signal Q06_TB : std_logic := '0';
    signal Q07_TB : std_logic := '0';
    signal Q08_TB : std_logic := '0';
    signal Q09_TB : std_logic := '0';
    signal Q10_TB : std_logic := '0';
    signal Q11_TB : std_logic := '0';
    signal Q12_TB : std_logic := '0';
    signal Q13_TB : std_logic := '0';
    signal Q14_TB : std_logic := '0';
    signal Q15_TB : std_logic := '0';
    

    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: RF_TempDestReg_Decoder_21364066 port map (
            A => A_TB,
            
            Q00 => Q00_TB,
            Q01 => Q01_TB,
            Q02 => Q02_TB,
            Q03 => Q03_TB,
            Q04 => Q04_TB,
            Q05 => Q05_TB,
            Q06 => Q06_TB,
            Q07 => Q07_TB,
            Q08 => Q08_TB,
            Q09 => Q09_TB,
            Q10 => Q10_TB,
            Q11 => Q11_TB,
            Q12 => Q12_TB,
            Q13 => Q13_TB,
            Q14 => Q14_TB,
            Q15 => Q15_TB
            
        );

        
   stim_proc: process

   begin
        
        wait for 5ns;
        A_TB <= "0000";

        wait for 5ns;
        A_TB <= "0001";

        wait for 5ns;
        A_TB <= "0010";

        wait for 5ns;
        A_TB <= "0011";

        wait for 5ns;
        A_TB <= "0100";

        wait for 5ns;
        A_TB <= "0101";

        wait for 5ns;
        A_TB <= "0110";

        wait for 5ns;
        A_TB <= "0111";

        wait for 5ns;
        A_TB <= "1000"; 

        wait for 5ns;
        A_TB <= "1001";

        wait for 5ns;
        A_TB <= "1010";

        wait for 5ns;
        A_TB <= "1011";

        wait for 5ns;
        A_TB <= "1100";

        wait for 5ns;
        A_TB <= "1101";

        wait for 5ns;
        A_TB <= "1110";

        wait for 5ns;
        A_TB <= "1111";      

   end process;
end Sim;
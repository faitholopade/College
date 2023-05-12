----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 20.10.2022 14:12:00
-- Design Name: 
-- Module Name: RF_DestReg_Decoder_21364066_TB - Sim
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

entity RF_DestReg_Decoder_21364066_TB is
--  Port ( ); We don't need ports
end RF_DestReg_Decoder_21364066_TB;

architecture Sim of RF_DestReg_Decoder_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component RF_DestReg_Decoder_21364066
port ( 
        A : in std_logic_vector(4 downto 0);
        
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
        Q15 : out std_logic;
        Q16 : out std_logic;
        Q17 : out std_logic;
        Q18 : out std_logic;
        Q19 : out std_logic;
        Q20 : out std_logic;
        Q21 : out std_logic;
        Q22 : out std_logic;
        Q23 : out std_logic;
        Q24 : out std_logic;
        Q25 : out std_logic;
        Q26 : out std_logic;
        Q27 : out std_logic;
        Q28 : out std_logic;
        Q29 : out std_logic;
        Q30 : out std_logic;
        Q31 : out std_logic
        );
end component;

--Inputs
    signal A_TB : std_logic_vector(4 downto 0) := (others => '0');
         
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
    signal Q16_TB : std_logic := '0';
    signal Q17_TB : std_logic := '0';
    signal Q18_TB : std_logic := '0';
    signal Q19_TB : std_logic := '0';
    signal Q20_TB : std_logic := '0';
    signal Q21_TB : std_logic := '0';
    signal Q22_TB : std_logic := '0';
    signal Q23_TB : std_logic := '0';
    signal Q24_TB : std_logic := '0';
    signal Q25_TB : std_logic := '0';
    signal Q26_TB : std_logic := '0';
    signal Q27_TB : std_logic := '0';
    signal Q28_TB : std_logic := '0';
    signal Q29_TB : std_logic := '0';
    signal Q30_TB : std_logic := '0';
    signal Q31_TB : std_logic := '0';

    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: RF_DestReg_Decoder_21364066 port map (
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
            Q15 => Q15_TB,
            Q16 => Q16_TB,
            Q17 => Q17_TB,
            Q18 => Q18_TB,
            Q19 => Q19_TB,
            Q20 => Q20_TB,
            Q21 => Q21_TB,
            Q22 => Q22_TB,
            Q23 => Q23_TB,
            Q24 => Q24_TB,
            Q25 => Q25_TB,
            Q26 => Q26_TB,
            Q27 => Q27_TB,
            Q28 => Q28_TB,
            Q29 => Q29_TB,
            Q30 => Q30_TB,
            Q31 => Q31_TB
        );

        
   stim_proc: process

   begin

        wait for 2 ns;
          A_TB <= "00000";
            
        wait for 2 ns;
          A_TB <= "00001";
          
        wait for 2 ns;	
          A_TB <= "00010";
          
        wait for 2 ns;
          A_TB  <= "00011";
          
        wait for 2 ns;	
          A_TB <= "00100";
          
        wait for 2 ns;
          A_TB <= "00101";
          
        wait for 2 ns;	
          A_TB <= "00110";
          
        wait for 2 ns;
          A_TB <= "00111";
          
        wait for 2 ns;	
          A_TB <= "01000";
          
        wait for 2 ns;
          A_TB <= "01001";
          
        wait for 2 ns;	
          A_TB <= "01010";
          
        wait for 2 ns;
          A_TB <= "01011";
          
        wait for 2 ns;	
          A_TB <= "01100";
          
        wait for 2 ns;
          A_TB <= "01101";
          
        wait for 2 ns;	
          A_TB <= "01110";
          
        wait for 2 ns;
         A_TB <= "01111";
          
        wait for 2 ns;        
          A_TB <= "10000";
          
        wait for 2 ns;
          A_TB <= "10001";
          
        wait for 2 ns;	
          A_TB <= "10010";
          
        wait for 2 ns;
          A_TB <= "10011";
          
        wait for 2 ns;	
          A_TB <= "10100";
          
        wait for 2 ns;
          A_TB <= "10101";
          
        wait for 2 ns;	
          A_TB <= "10110";
          
        wait for 2 ns;
          A_TB <= "10111";
          
        wait for 2 ns;	
          A_TB <= "11000";
          
        wait for 2 ns;
          A_TB <= "11001";
          
        wait for 2 ns;	
          A_TB <= "11010";
          
        wait for 2 ns;
          A_TB <= "11011";
          
        wait for 2 ns;	
          A_TB <= "11100";
          
        wait for 2 ns;
          A_TB <= "11101";
          
        wait for 2 ns;	
          A_TB <= "11110";
          
        wait for 2 ns;
          A_TB <= "11111";      

   end process;
end Sim;
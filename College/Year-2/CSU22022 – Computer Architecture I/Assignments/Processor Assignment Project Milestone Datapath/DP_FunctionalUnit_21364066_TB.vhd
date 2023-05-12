----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 07.11.2022 00:32:00
-- Design Name: 
-- Module Name: DP_FunctionalUnit_21364066_TB - Sim
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

entity DP_FunctionalUnit_21364066_TB is
--  Port ( ); We don't need ports
end DP_FunctionalUnit_21364066_TB;

architecture Sim of DP_FunctionalUnit_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component DP_FunctionalUnit_21364066
port ( 
        B : in std_logic_vector(31 downto 0);
        A : in std_logic_vector(31 downto 0);
        FS : in std_logic_vector(4 downto 0);
        C : out std_logic;
        F : out std_logic_vector(31 downto 0);
        N : out std_logic;
        Z : out std_logic;
        V : out std_logic
      );
end component;

--Inputs

    signal B_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal A_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal FS_TB : std_logic_vector(4 downto 0):= (others => '0');
    
--Outputs

    signal C_TB : std_logic := '0';
    signal F_TB : std_logic_vector(31 downto 0) := (others => '0');
    signal N_TB : std_logic := '0';
    signal Z_TB : std_logic := '0';
    signal V_TB : std_logic := '0';
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: DP_FunctionalUnit_21364066 port map (
          B => B_TB,
          A => A_TB,
          FS => FS_TB,
          C => C_TB,
          F => F_TB,
          N => N_TB,
          Z => Z_TB,
          V => V_TB
        );

        
   stim_proc: process

   begin
    A_TB <= "00000001010001011111110101100010"; --ID
    B_TB <= "00000001010001011111110101101000"; --ID + 6

    wait for 200ns;
    FS_TB <= "01000"; --AND 

    wait for 200ns;
    FS_TB <= "01010"; --OR 

    wait for 200ns;
    FS_TB <= "00110"; --A - 1 

    wait for 200ns;
    FS_TB <= "10100"; --srB 

    wait for 200ns;
    FS_TB <= "00101"; --A + 1s c B + 1 

    wait for 200ns;
    FS_TB <= "01110"; --1s c A 

    wait for 200ns;
    FS_TB <= "00100"; --A + 1s c B 

    wait for 200ns;
    FS_TB <= "10000"; --B 

    wait for 200ns;
    FS_TB <= "00011"; --A + B + 1 

    wait for 200ns;
    FS_TB <= "11000"; --slB 

    wait for 200ns;
    FS_TB <= "00010"; --A + B 

    wait for 200ns;
    FS_TB <= "00111"; --A (00111) 

    wait for 200ns;
    FS_TB <= "00000"; --A (00000) 

    wait for 200ns;
    FS_TB <= "00001"; --A + 1 

    wait for 200ns;
    FS_TB <= "01100"; --A XOR B  

    wait for 200ns;

    

   end process;
end Sim;
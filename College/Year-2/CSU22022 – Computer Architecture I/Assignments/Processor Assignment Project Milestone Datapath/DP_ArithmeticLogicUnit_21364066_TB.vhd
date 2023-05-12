----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 03.11.2022 12:50:00
-- Design Name: 
-- Module Name: DP_ArithmeticLogicUnit_21364066_TB - Sim
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

entity DP_ArithmeticLogicUnit_21364066_TB is
--  Port ( ); We don't need ports
end DP_ArithmeticLogicUnit_21364066_TB;

architecture Sim of DP_ArithmeticLogicUnit_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component DP_ArithmeticLogicUnit_21364066
port ( 
        A : in std_logic_vector(31 downto 0);
        C_IN : in std_logic;
        B : in std_logic_vector(31 downto 0);
        S0 : in std_logic;
        S1 : in std_logic;
        S2 : in std_logic;
        C : out std_logic;
        V : out std_logic;
        G : out std_logic_vector(31 downto 0)
      );
end component;

--Inputs

    signal A_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal C_IN_TB : std_logic := '0';
    signal B_TB : std_logic_vector(31 downto 0):= (others => '0');
    signal S0_TB : std_logic := '0';
    signal S1_TB : std_logic := '0';
    signal S2_TB : std_logic := '0';
--Outputs

    signal C_TB : std_logic := '0';
    signal V_TB : std_logic := '0';
    signal G_TB : std_logic_vector(31 downto 0):= (others => '0');
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: DP_ArithmeticLogicUnit_21364066 port map (
          A => A_TB,
          C_IN => C_IN_TB,
          B => B_TB,
          S0 => S0_TB,
          S1 => S1_TB, 
          S2 => S2_TB, 
          C => C_TB, 
          V => V_TB, 
          G => G_TB
        );

        
   stim_proc: process

   begin
   wait for 100ns;
    A_TB <= "00000001010001011111110101100010"; --ID
    B_TB <= "00000001010001011111110101100011"; --ID + 1
    
    --0000
    wait for 100ns;
    S2_TB <= '0';
    S1_TB <= '0';
    S0_TB <= '0';
    C_IN_TB <= '0';

    --0001
    wait for 100ns;
    S2_TB <= '0';
    S1_TB <= '0';
    S0_TB <= '0';
    C_IN_TB <= '1';

    --0010
    wait for 100ns;
    S2_TB <= '0';
    S1_TB <= '0';
    S0_TB <= '1';
    C_IN_TB <= '0';

    --0011
    wait for 100ns;
    S2_TB <= '0';
    S1_TB <= '0';
    S0_TB <= '1';
    C_IN_TB <= '1';

    --0100
    wait for 100ns;
    S2_TB <= '0';
    S1_TB <= '1';
    S0_TB <= '0';
    C_IN_TB <= '0';

    --0101
    wait for 100ns;
    S2_TB <= '0';
    S1_TB <= '1';
    S0_TB <= '0';
    C_IN_TB <= '1';

    --0110
    wait for 100ns;
    S2_TB <= '0';
    S1_TB <= '1';
    S0_TB <= '1';
    C_IN_TB <= '0';
    

    --0111
    wait for 100ns;
    S2_TB <= '0';
    S1_TB <= '1';
    S0_TB <= '1';
    C_IN_TB <= '1';
    
    --100X
    wait for 100ns;
    S2_TB <= '1';
    S1_TB <= '0';
    S0_TB <= '0';
    C_IN_TB <= '0';

    --101X
    wait for 100ns;
    S2_TB <= '1';
    S1_TB <= '0';
    S0_TB <= '1';
    C_IN_TB <= '0';

    --110X
    wait for 100ns;
    S2_TB <= '1';
    S1_TB <= '1';
    S0_TB <= '0';
    C_IN_TB <= '0';

    --111X
    wait for 100ns;
    S2_TB <= '1';
    S1_TB <= '1';
    S0_TB <= '1';
    C_IN_TB <= '0';  
   
   wait for 10ns;

   end process;
end Sim;
----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 23:12:00
-- Design Name: 
-- Module Name: CPU_SMux_21364066_TB - Sim
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

entity CPU_SMux_21364066_TB is
--  Port ( ); We don't need ports
end CPU_SMux_21364066_TB;

architecture Sim of CPU_SMux_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component CPU_SMux_21364066
port ( 
        C_Flag : in std_logic;
        MS : in std_logic_vector(2 downto 0);
        N : in std_logic;
        Not_C_Flag : in std_logic;
        Not_Z_Flag : in std_logic;
        One : in std_logic;
        V_Flag : in std_logic;
        Z_Flag : in std_logic;
        Zero : in std_logic;

        CAR : out std_logic
    );
end component;

--Inputs
    signal C_Flag_TB : std_logic := '0';
    signal MS_TB : std_logic_vector(2 downto 0) := (others => '0');
    signal N_TB : std_logic := '0';
    signal Not_C_Flag_TB : std_logic := '0';
    signal Not_Z_Flag_TB : std_logic := '0';
    signal One_TB : std_logic := '0';
    signal V_Flag_TB : std_logic := '0';
    signal Z_Flag_TB : std_logic := '0';
    signal Zero_TB : std_logic := '0';
    
--Outputs
    signal CAR_TB : std_logic := '0';
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: CPU_SMux_21364066 port map (
        C_Flag => C_Flag_TB,
        MS => MS_TB,
        N => N_TB,
        Not_C_Flag => Not_C_Flag_TB,
        Not_Z_Flag => Not_Z_Flag_TB,
        One => One_TB,
        V_Flag => V_Flag_TB,
        Z_Flag => Z_Flag_TB,
        Zero => Zero_TB,
        
        CAR => CAR_TB
    );

        
   stim_proc: process
   begin

    C_Flag_TB <= '1';
    N_TB <= '0';
    Not_C_Flag_TB <= '0';
    Not_Z_Flag_TB <= '0';
    One_TB <= '1';
    V_Flag_TB <= '0';
    Z_Flag_TB <= '1';
    Zero_TB <= '0';

    wait for 100ns;
    MS_TB <= "000";

    wait for 100ns;
    MS_TB <= "001";

    wait for 100ns;
    MS_TB <= "010";

    wait for 100ns;
    MS_TB <= "011";

    wait for 100ns;
    MS_TB <= "100";

    wait for 100ns;
    MS_TB <= "101";

    wait for 100ns;
    MS_TB <= "110";

    wait for 100ns;
    MS_TB <= "111";

   end process;
end Sim;
----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 23:12:00
-- Design Name: 
-- Module Name: CPU_PC_21364066_TB - Sim
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

entity CPU_PC_21364066_TB is
--  Port ( ); We don't need ports
end CPU_PC_21364066_TB;

architecture Sim of CPU_PC_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component CPU_PC_21364066
port ( 
        Clock : in std_logic;
        Displacement : in std_logic_vector(31 downto 0);
        PI : in std_logic;
        PL : in std_logic;
        Reset : in std_logic;

        InstAdd : out std_logic_vector(31 downto 0)
    );
end component;

--Inputs

    signal Clock_TB : std_logic := '0';
    signal Displacement_TB : std_logic_vector(31 downto 0) := (others => '0');
    signal PI_TB : std_logic := '0';
    signal PL_TB : std_logic := '0';
    signal Reset_TB : std_logic := '0';
    
--Outputs

    signal InstAdd_TB : std_logic_vector(31 downto 0) := (others => '0');

    constant period : time := 60ns;
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: CPU_PC_21364066 port map (
        Clock => Clock_TB,
        Displacement => Displacement_TB,
        PI => PI_TB,
        PL => PL_TB, 
        Reset => Reset_TB,
        InstAdd => InstAdd_TB
    );

    Clock_TB <= not Clock_TB after period/2;

   stim_proc: process
   begin

    --RESET TO 6--
    wait until Clock_TB'event and Clock_TB = '1';
    Reset_TB <= '1';
    PL_TB <= '0';
    PI_TB <= '0';
    -- Displacement_TB <= "00000000000000000000000000000110";
    

    --INCREMENT--
    wait until Clock_TB'event and Clock_TB = '1';
    Reset_TB <= '0';
    PL_TB <= '0';
    PI_TB <= '1';
    

    --DISPLACEMENT = 6 + 6 = 12--
    wait until Clock_TB'event and Clock_TB = '1';
    Reset_TB <= '0';
    PI_TB <= '0';
    PL_TB <= '1';
    Displacement_TB <= "00000000000000000000000000001100";
    
    
   end process;
end Sim;
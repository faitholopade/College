----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 23:12:00
-- Design Name: 
-- Module Name: CPU_CAR_21364066_TB - Sim
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

use IEEE.std_logic_unsigned.all;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity CPU_CAR_21364066_TB is
--  Port ( ); We don't need ports
end CPU_CAR_21364066_TB;

architecture Sim of CPU_CAR_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component CPU_CAR_21364066
port ( 
        Address : in std_logic_vector(16 downto 0);
        Clock : in std_logic;
        LoadAdd : in std_logic;
        Reset : in std_logic;

        CMAdd : out std_logic_vector(16 downto 0)
        
    );
end component;

--Inputs

   signal Address_TB : std_logic_vector(16 downto 0) := (others => '0');
   signal Clock_TB : std_logic := '0'; 
   signal LoadAdd_TB : std_logic := '0'; 
   signal Reset_TB : std_logic := '0'; 
    
--Outputs

   signal CMAdd_TB : std_logic_vector(16 downto 0) := (others => '0');

   constant period : time := 200ns;
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
  uut: CPU_CAR_21364066 port map (
        Address => Address_TB,
        Clock => Clock_TB,
        LoadAdd => LoadAdd_TB,
        Reset => Reset_TB,
        CMAdd => CMAdd_TB    
    );

    Clock_TB <= not Clock_TB after period/2;

  stim_proc: process
  begin

    --RESET--
    wait until Clock_TB'event and Clock_TB = '1';
    Reset_TB <= '1';
    wait for 100ns;

    --INCREMENT--
    wait until Clock_TB'event and Clock_TB = '1';
    Reset_TB <= '0';
    LoadAdd_TB <= '0';
    wait for 100ns;

    --LOAD--
    wait until Clock_TB'event and Clock_TB = '1';
    Reset_TB <= '0';
    LoadAdd_TB <= '1';
    Address_TB <= "0" & X"0042";
    wait for 100ns;
    
  end process;
end Sim;
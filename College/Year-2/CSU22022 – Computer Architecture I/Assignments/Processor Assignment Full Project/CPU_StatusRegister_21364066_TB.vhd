----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 23:12:00
-- Design Name: 
-- Module Name: CPU_StatusRegister_21364066_TB - Sim
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

entity CPU_StatusRegister_21364066_TB is
--  Port ( ); We don't need ports
end CPU_StatusRegister_21364066_TB;

architecture Sim of CPU_StatusRegister_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component CPU_StatusRegister_21364066
port (  Clock : in std_logic;
        C_Flag : in std_logic;
        Reset_C : in std_logic;
        N_Flag : in std_logic;
        LoadFlags : in std_logic;
        Reset_N : in std_logic;
        V_Flag : in std_logic;
        Reset_V : in std_logic;
        Z_Flag : in std_logic;
        Reset_Z : in std_logic;
        StatusVector : out std_logic_vector(3 downto 0)
    );
end component;

--Inputs

   signal Clock_TB : std_logic := '0';
   signal C_Flag_TB : std_logic := '0';
   signal Reset_C_TB : std_logic := '0';
   signal N_Flag_TB : std_logic := '0';
   signal LoadFlags_TB : std_logic := '0';
   signal Reset_N_TB : std_logic := '0';
   signal V_Flag_TB : std_logic := '0';
   signal Reset_V_TB : std_logic := '0';
   signal Z_Flag_TB : std_logic := '0';
   signal Reset_Z_TB : std_logic := '0';

--Outputs

   signal StatusVector_TB : std_logic_vector(3 downto 0) := (others => '0');

   constant period : time := 20ns;
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
  uut: CPU_StatusRegister_21364066 port map (
        Clock => Clock_TB,
        C_Flag => C_Flag_TB,
        Reset_C => Reset_C_TB,
        N_Flag => N_Flag_TB,
        LoadFlags => LoadFlags_TB,
        Reset_N => Reset_N_TB,
        V_Flag => V_Flag_TB,
        Reset_V => Reset_V_TB,
        Z_Flag => Z_Flag_TB,
        Reset_Z => Reset_Z_TB,

        StatusVector => StatusVector_TB
    );

       Clock_TB <= not clock_TB after period/2;

        
  stim_proc: process
  begin

    --Loading C, V, N, and Z flags into the Status Register--
    wait until Clock_TB'event and Clock_TB = '1';
    LoadFlags_TB <= '1';
    C_Flag_TB <= '1';
    N_Flag_TB <= '1';
    V_Flag_TB <= '1';
    Z_Flag_TB <= '1';

    --Resetting the C-flag--
    wait until Clock_TB'event and Clock_TB = '1';
    Reset_C_TB <= '1';

    --Resetting the V-flag--
    wait until Clock_TB'event and Clock_TB = '1';
    Reset_V_TB <= '1';

    --Resetting the N-flag--
    wait until Clock_TB'event and Clock_TB = '1';
    Reset_N_TB <= '1';

    --Resetting the Z-flag--
    wait until Clock_TB'event and Clock_TB = '1';
    Reset_Z_TB <= '1';

  end process;
end Sim;
----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 01.11.2022 22:02:26
-- Design Name: 
-- Module Name: RF_Test_RegisterFile_32_15_21364066_TB - Behavioral
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

entity RF_Test_RegisterFile_32_15_21364066_TB is
--  Port ( ); We don't need ports 
end RF_Test_RegisterFile_32_15_21364066_TB;

architecture Sim of RF_Test_RegisterFile_32_15_21364066_TB is

-- Component Declaration for the Unit Under Test (UUT)

component RF_Test_RegisterFile_32_15_21364066

Port (  A_B_DataIN_Test : in std_logic_vector (1 downto 0);
        D_Test : in std_logic_vector (31 downto 0);
        Clock_Test : in std_logic;
        DR_Test : in std_logic_vector (4 downto 0);
        RW_Test : in std_logic;
        SA_Test : in std_logic_vector (4 downto 0);
        SB_Test : in std_logic_vector (4 downto 0);
        TA_Test : in std_logic_vector (3 downto 0);
        TB_Test : in std_logic_vector (3 downto 0);
        TD_Test : in std_logic_vector (3 downto 0)
);
end component;

--Inputs

    signal A_B_DataIN_Test_TB : std_logic_vector (1 downto 0) := (others => '0');
    signal D_Test_TB : std_logic_vector (31 downto 0) := (others => '0');
    signal Clock_Test_TB : std_logic := '0';
    signal DR_Test_TB : std_logic_vector (4 downto 0) := (others => '0');
    signal RW_Test_TB : std_logic := '0';
    signal SA_Test_TB : std_logic_vector (4 downto 0) := (others => '0');
    signal SB_Test_TB : std_logic_vector (4 downto 0) := (others => '0');
    signal TA_Test_TB : std_logic_vector (3 downto 0) := (others => '0');
    signal TB_Test_TB : std_logic_vector (3 downto 0) := (others => '0');
    signal TD_Test_TB : std_logic_vector (3 downto 0) := (others => '0');

--Outputs

constant period : time := 60ns;

begin

	-- Instantiate the Unit Under Test (UUT)
   uut: RF_Test_RegisterFile_32_15_21364066 port map (
        A_B_DataIN_Test => A_B_DataIN_Test_TB
        D_Test  => D_Test_TB
        Clock_Test =>  Clock_Test_TB
        DR_Test => DR_Test_TB
        RW_Test => RW_Test_TB
        SA_Test => SA_Test_TB
        SB_Test => SB_Test_TB
        TA_Test => TA_Test_TB
        TB_Test => TB_Test_TB
        TD_Test => TD_Test_TB
    );

 stim_proc: process
   begin

    D_Test_TB <= "00000001010001011111110101100010" after period/2;
    A_B_DataIN_Test <= "01" after period/2;


   

 
   end process;
end Sim;

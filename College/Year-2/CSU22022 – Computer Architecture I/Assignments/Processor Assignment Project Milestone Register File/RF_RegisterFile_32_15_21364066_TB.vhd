----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 24.10.2022 17:24:00
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

entity RF_RegisterFile_32_15_21364066_TB is
--  Port ( ); We don't need ports
end RF_RegisterFile_32_15_21364066_TB;

architecture Sim of RF_RegisterFile_32_15_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component RF_RegisterFile_32_15_21364066
port ( 
            SA : in std_logic_vector(4 downto 0);
            SB : in std_logic_vector(4 downto 0);
            DR : in std_logic_vector(4 downto 0);
            Clock : in std_logic;
            RW : in std_logic;
            D : in std_logic_vector(31 downto 0);
            TB : in std_logic_vector(3 downto 0);
            TD : in std_logic_vector(3 downto 0);
            TA : in std_logic_vector(3 downto 0);
            A : out std_logic_vector(31 downto 0);
            B : out std_logic_vector(31 downto 0)
        );
end component;

--Inputs 
signal SA_TB : std_logic_vector(4 downto 0) := (others => '0');
signal DR_TB : std_logic_vector(4 downto 0) := (others => '0');
signal SB_TB : std_logic_vector(4 downto 0) := (others => '0');

signal RW_TB : std_logic := '0';
signal Clock_TB : std_logic := '0';

signal D_TB : std_logic_vector(31 downto 0) := (others => '0');
signal TB_TB : std_logic_vector(3 downto 0) := (others => '0');
signal TD_TB : std_logic_vector(3 downto 0) := (others => '0');
signal TA_TB : std_logic_vector(3 downto 0) := (others => '0');

--Outputs
signal A_TB : std_logic_vector(31 downto 0) := (others => '0');
signal B_TB : std_logic_vector(31 downto 0) := (others => '0');

    constant period : time := 60ns;


begin
-- Instantiate the Unit Under Test (UUT)
	
   uut: RF_RegisterFile_32_15_21364066 port map (
    SA => SA_TB, 
    DR => DR_TB, 
    SB => SB_TB, 
    RW => RW_TB, 
    Clock => Clock_TB,
    D => D_TB, 
    TB => TB_TB,
    TD => TD_TB, 
    TA => TA_TB, 
    A => A_TB,
    B => B_TB
);

Clock_TB <= not Clock_TB after period/2;

stim_proc: process

begin 

-- WRITING TO & READING FROM GENERAL PURPOSE REGISTERS --

    --0 (showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101100010" after period/2;
    DR_TB <= "00000" after period/2;
    TD_TB    <= "0000" after period/2;
    --0 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --1(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101100011" after period/2;
    DR_TB <= "00001" after period/2;
    TD_TB    <= "0000" after period/2;
    --1 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "00001" after period/2;
    SB_TB <= "00001" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;
    
    --2(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101100100" after period/2;
    DR_TB <= "00010" after period/2;
    TD_TB    <= "0000" after period/2;
    --2 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "00010" after period/2;
    SB_TB <= "00010" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --3(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101100101" after period/2;
    DR_TB <= "00011" after period/2;
    TD_TB    <= "0000" after period/2;
    --3 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "00011" after period/2;
    SB_TB <= "00011" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --4(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101100110" after period/2;
    DR_TB <= "00100" after period/2;
    TD_TB    <= "0000" after period/2;
    --4 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "00100" after period/2;
    SB_TB <= "00100" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --5(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101100111" after period/2;
    DR_TB <= "00101" after period/2;
    TD_TB    <= "0000" after period/2;
    --5 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "00101" after period/2;
    SB_TB <= "00101" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --6(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101000" after period/2;
    DR_TB <= "00110" after period/2;
    TD_TB    <= "0000" after period/2;
    --6 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "00110" after period/2;
    SB_TB <= "00110" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --7(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101001" after period/2;
    DR_TB <= "00111" after period/2;
    TD_TB    <= "0000";
    --7 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "00111" after period/2;
    SB_TB <= "00111" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --8(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101010" after period/2;
    DR_TB <= "01000" after period/2;
    TD_TB    <= "0000" after period/2;
    --8 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "01000" after period/2;
    SB_TB <= "01000" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --9(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101011" after period/2;
    DR_TB <= "01001" after period/2;
    TD_TB    <= "0000" after period/2;
    --9 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "01001" after period/2;
    SB_TB <= "01001" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --10(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101100" after period/2;
    DR_TB <= "01010" after period/2;
    TD_TB    <= "0000" after period/2;
    --10 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "01010" after period/2;
    SB_TB <= "01010" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --11(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101101" after period/2;
    DR_TB <= "01011" after period/2;
    TD_TB    <= "0000" after period/2;
    --11 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "01011" after period/2;
    SB_TB <= "01011" after period/2;
    TA_TB    <= "0000";
    TB_TB    <= "0000";

    --12(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101110" after period/2;
    DR_TB <= "01100" after period/2;
    TD_TB    <= "0000" after period/2;
    --12 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "01100" after period/2;
    SB_TB <= "01100" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --13(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101111" after period/2;
    DR_TB <= "01101" after period/2;
    TD_TB    <= "0000" after period/2;
    --13 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "01101" after period/2;
    SB_TB <= "01101" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --14(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101110000" after period/2;
    DR_TB <= "01110" after period/2;
    TD_TB    <= "0000" after period/2;
    --14 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "01110" after period/2;
    SB_TB <= "01110" after period/2;
    TA_TB    <= "0000";
    TB_TB    <= "0000";

    --15(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101110001" after period/2;
    DR_TB <= "01111" after period/2;
    TD_TB    <= "0000" after period/2;
    --15 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "01111" after period/2;
    SB_TB <= "01111" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;
    

    --16(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101110010" after period/2;
    DR_TB <= "10000" after period/2;
    TD_TB    <= "0000" after period/2;
    --16 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "10000" after period/2;
    SB_TB <= "10000" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --17(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101110011" after period/2;
    DR_TB <= "10001" after period/2;
    TD_TB    <= "0000" after period/2;
    --17 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "10001" after period/2;
    SB_TB <= "10001" after period/2;
    TA_TB    <= "0000";
    TB_TB    <= "0000";

    --18(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101110100" after period/2;
    DR_TB <= "10010" after period/2;
    TD_TB    <= "0000" after period/2;
    --18 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "10010" after period/2;
    SB_TB <= "10010" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --19(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101110101" after period/2;
    DR_TB <= "10011" after period/2;
    TD_TB    <= "0000" after period/2;
    --19 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "10011" after period/2;
    SB_TB <= "10011" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --20(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101110110" after period/2;
    DR_TB <= "10100" after period/2;
    TD_TB    <= "0000" after period/2;
    --20 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "10100" after period/2;
    SB_TB <= "10100" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --21(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101110111" after period/2;
    DR_TB <= "10101" after period/2;
    TD_TB    <= "0000" after period/2;
    --21 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "10101" after period/2;
    SB_TB <= "10101" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --22(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101111000" after period/2;
    DR_TB <= "10110" after period/2;
    TD_TB    <= "0000" after period/2;
    --22 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "10110" after period/2;
    SB_TB <= "10110" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --23(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101111001" after period/2;
    DR_TB <= "10111" after period/2;
    TD_TB    <= "0000" after period/2;
    --23 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "10111" after period/2;
    SB_TB <= "10111" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --24(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101111010" after period/2;
    DR_TB <= "11000" after period/2;
    TD_TB    <= "0000" after period/2;
    --24 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "11000" after period/2;
    SB_TB <= "11000" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --25(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101111011" after period/2;
    DR_TB <= "11001" after period/2;
    TD_TB    <= "0000" after period/2;
    --25 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "11001" after period/2;
    SB_TB <= "11001" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --26(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101111100" after period/2;
    DR_TB <= "11010" after period/2;
    TD_TB    <= "0000" after period/2;
    --26 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "11010" after period/2;
    SB_TB <= "11010" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --27(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101111101" after period/2;
    DR_TB <= "11011" after period/2;
    TD_TB    <= "0000" after period/2;
    --27 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "11011" after period/2;
    SB_TB <= "11011" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --28(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101111110" after period/2;
    DR_TB <= "11100" after period/2;
    TD_TB    <= "0000" after period/2;
    --28 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "11100" after period/2;
    SB_TB <= "11100" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --29(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101111111" after period/2;
    DR_TB <= "11101" after period/2;
    TD_TB    <= "0000" after period/2;
    --29 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "11101" after period/2;
    SB_TB <= "11101" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --30(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110110000000" after period/2;
    DR_TB <= "11110" after period/2;
    TD_TB    <= "0000" after period/2;
    --30 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "11110" after period/2;
    SB_TB <= "11110" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;

    --31(showing write to register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110110000001" after period/2;
    DR_TB <= "11111" after period/2;
    TD_TB    <= "0000" after period/2;
    --31 (showing read from register)
    RW_TB    <= '1' after period/2;
    SA_TB <= "11111" after period/2;
    SB_TB <= "11111" after period/2;
    TA_TB    <= "0000" after period/2;
    TB_TB    <= "0000" after period/2;


-- WRITING TO & READING FROM TEMP REGISTERS --
 
    --0 (showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101100011" after period/2;
    TD_TB     <= "0001" after period/2;
    DR_TB <= "00000" after period/2;
    --0 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "0001" after period/2;
    TB_TB    <= "0001" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;

    --1(showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101100100" after period/2;
    TD_TB     <= "0010" after period/2;
    DR_TB <= "00000" after period/2;
    --1 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "0010" after period/2;
    TB_TB     <= "0010" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;

    --2(showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101100101" after period/2;
    TD_TB     <= "0011" after period/2;
    DR_TB <= "00000" after period/2;
    --2 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "0011" after period/2;
    TB_TB     <= "0011" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;

    --3(showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101100110" after period/2;
    TD_TB     <= "0100" after period/2;
    DR_TB <= "00000" after period/2;
    --3 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "0100" after period/2;
    TB_TB     <= "0100" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;

    --4(showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101100111" after period/2;
    TD_TB     <= "0101" after period/2;
    DR_TB <= "00000" after period/2;
    --4 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "0101" after period/2;
    TB_TB     <= "0101" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;

    --5(showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101000" after period/2;
    TD_TB     <= "0110" after period/2;
    DR_TB <= "00000" after period/2;
    --5 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "0110" after period/2;
    TB_TB     <= "0110" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;

    --6(showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101001" after period/2;
    TD_TB     <= "0111" after period/2;
    DR_TB <= "00000" after period/2;
    --6 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "0111" after period/2;
    TB_TB     <= "0111" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;

    --7(showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101010" after period/2;
    TD_TB     <= "1000" after period/2;
    DR_TB <= "00000" after period/2;
    --7 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "1000" after period/2;
    TB_TB     <= "1000" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;

    --8(showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101011" after period/2;
    TD_TB     <= "1001" after period/2;
    DR_TB <= "00000" after period/2;
    --8 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "1001" after period/2;
    TB_TB     <= "1001" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;

    --9(showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101100" after period/2;
    TD_TB     <= "1010" after period/2;
    DR_TB <= "00000" after period/2;
    --9 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "1010" after period/2;
    TB_TB     <= "1010" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;

    --10(showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101101" after period/2;
    TD_TB     <= "1011" after period/2;
    DR_TB <= "00000" after period/2;
    --10 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "1011" after period/2;
    TB_TB     <= "1011" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;

    --11(showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101110" after period/2;
    TD_TB     <= "1100" after period/2;
    DR_TB <= "00000" after period/2;
    --11 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "1100" after period/2;
    TB_TB     <= "1100" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;

    --12(showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101101111" after period/2;
    TD_TB     <= "1101" after period/2;
    DR_TB <= "00000" after period/2;
    --12 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "1101" after period/2;
    TB_TB     <= "1101" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;

    --13(showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101110000" after period/2;
    TD_TB     <= "1110" after period/2;
    DR_TB <= "00000" after period/2;
    --13 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "1110" after period/2;
    TB_TB     <= "1110" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;

    --14(showing write to temp register)
    wait until Clock_TB'event and Clock_TB = '1';
    RW_TB    <= '1' after period/2;
    D_TB  <= "00000001010001011111110101110001" after period/2;
    TD_TB     <= "1111" after period/2;
    DR_TB <= "00000" after period/2;
    --14 (showing read from temp register)
    RW_TB    <= '1' after period/2;
    TA_TB    <= "1111" after period/2;
    TB_TB     <= "1111" after period/2;
    SA_TB <= "00000" after period/2;
    SB_TB <= "00000" after period/2;
    
    end process ;
end  Sim;

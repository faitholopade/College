----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 07.11.2022 23:14:00
-- Design Name: 
-- Module Name: DP_Datapath_21364066_TB - Sim
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
use IEEE.std_logic_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity DP_Datapath_21364066_TB is
--  Port ( ); We don't need ports
end DP_Datapath_21364066_TB;

architecture Sim of DP_Datapath_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component DP_Datapath_21364066
port ( 
        IR_IN : in std_logic_vector(31 downto 0);
        MB : in std_logic;
        DATA_IN : in std_logic_vector(31 downto 0);
        MD : in std_logic;
        FS : in std_logic_vector(4 downto 0);
        Clock : in std_logic;
        DR : in std_logic_vector(4 downto 0);
        RW : in std_logic;
        SA : in std_logic_vector(4 downto 0);
        SB : in std_logic_vector(4 downto 0);
        TA : in std_logic_vector(3 downto 0);
        TB : in std_logic_vector(3 downto 0);
        TD : in std_logic_vector(3 downto 0);
        DATA_OUT : out std_logic_vector(31 downto 0);
        C : out std_logic;
        N : out std_logic;
        V : out std_logic;
        Z : out std_logic;
        ADD : out std_logic_vector(31 downto 0)
  );
end component;

--Inputs

    signal IR_IN_TB : std_logic_vector(31 downto 0) := (others => '0');
    signal MB_TB : std_logic:= '0';
    signal DATA_IN_TB : std_logic_vector(31 downto 0) := (others => '0');
    signal MD_TB : std_logic:= '0';
    signal FS_TB : std_logic_vector(4 downto 0) := (others => '0');
    signal Clock_TB : std_logic:= '0';
    signal DR_TB : std_logic_vector(4 downto 0) := (others => '0');
    signal RW_TB : std_logic:= '0';
    signal SA_TB : std_logic_vector(4 downto 0) := (others => '0');
    signal SB_TB : std_logic_vector(4 downto 0) := (others => '0');
    signal TA_TB : std_logic_vector(3 downto 0) := (others => '0');
    signal TB_TB : std_logic_vector(3 downto 0) := (others => '0');
    signal TD_TB : std_logic_vector(3 downto 0) := (others => '0');
    
--Outputs

    signal DATA_OUT_TB : std_logic_vector(31 downto 0) := (others => '0');
    signal C_TB : std_logic:= '0';
    signal N_TB : std_logic:= '0';
    signal V_TB : std_logic:= '0';
    signal Z_TB : std_logic:= '0';
    signal ADD_TB : std_logic_vector(31 downto 0) := (others => '0');

    constant period : time := 200ns; 
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: DP_Datapath_21364066 port map (
          IR_IN => IR_IN_TB,
          MB => MB_TB,
          DATA_IN => DATA_IN_TB,
          MD => MD_TB,
          FS => FS_TB,
          Clock => Clock_TB,
          DR => DR_TB,
          RW => RW_TB,
          SA => SA_TB,
          SB => SB_TB,
          TA => TA_TB,
          TB => TB_TB,
          TD => TD_TB,
          DATA_OUT => DATA_OUT_TB,
          C => C_TB,
          N => N_TB,
          V => V_TB,
          Z => Z_TB,
          ADD => ADD_TB
      );

      Clock_TB <= not Clock_TB after period/2;
        
   stim_proc: process

   begin
    --LOADING REGISTERS WITH DATA--
  --ID - 0 (General Purpose Reg 00)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101100010";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;
    
  --ID - 1 (General Purpose Reg 01)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101100001";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "00001";
      RW_TB <= '1';
      SA_TB <= "00001";
      SB_TB <= "00001";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 2 (General Purpose Reg 02)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101100000";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "00010";
      RW_TB <= '1';
      SA_TB <= "00010";
      SB_TB <= "00010";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 3 (General Purpose Reg 03)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011111";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "00011";
      RW_TB <= '1';
      SA_TB <= "00011";
      SB_TB <= "00011";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 4 (General Purpose Reg 04)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011110";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "00100";
      RW_TB <= '1';
      SA_TB <= "00100";
      SB_TB <= "00100";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 5  (General Purpose Reg 05)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011101";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "00101";
      RW_TB <= '1';
      SA_TB <= "00101";
      SB_TB <= "00101";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 6 (General Purpose Reg 06)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011100";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "00110";
      RW_TB <= '1';
      SA_TB <= "00110";
      SB_TB <= "00110";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 7 (General Purpose Reg 07)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011011";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "00111";
      RW_TB <= '1';
      SA_TB <= "00111";
      SB_TB <= "00111";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;
      
  --ID - 8 (General Purpose Reg 08)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011010";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "01000";
      RW_TB <= '1';
      SA_TB <= "01000";
      SB_TB <= "01000";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 9 (General Purpose Reg 09)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011001";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "01001";
      RW_TB <= '1';
      SA_TB <= "01001";
      SB_TB <= "01001";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 10 (General Purpose Reg 10)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011000";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "01010";
      RW_TB <= '1';
      SA_TB <= "01010";
      SB_TB <= "01010";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 11 (General Purpose Reg 11)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101010111";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "01011";
      RW_TB <= '1';
      SA_TB <= "01011";
      SB_TB <= "01011";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 12 (General Purpose Reg 12)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101010110";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "01100";
      RW_TB <= '1';
      SA_TB <= "01100";
      SB_TB <= "01100";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 13 (General Purpose Reg 13)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101010101";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "01101";
      RW_TB <= '1';
      SA_TB <= "01101";
      SB_TB <= "01101";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 14(General Purpose Reg 14)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101010100";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "01110";
      RW_TB <= '1';
      SA_TB <= "01110";
      SB_TB <= "01110";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 15 (General Purpose Reg 15)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101010011";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "01111";
      RW_TB <= '1';
      SA_TB <= "01111";
      SB_TB <= "01111";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 16 (General Purpose Reg 16)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101010010";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "10000";
      RW_TB <= '1';
      SA_TB <= "10000";
      SB_TB <= "10000";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 17 (General Purpose Reg 17)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101010001";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "10001";
      RW_TB <= '1';
      SA_TB <= "10001";
      SB_TB <= "10001";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 18 (General Purpose Reg 18)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101010000";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "10010";
      RW_TB <= '1';
      SA_TB <= "10010";
      SB_TB <= "10010";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 19 (General Purpose Reg 19)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101001111";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "10011";
      RW_TB <= '1';
      SA_TB <= "10011";
      SB_TB <= "10011";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 20 (General Purpose Reg 20)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101001110";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "10100";
      RW_TB <= '1';
      SA_TB <= "10100";
      SB_TB <= "10100";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 21 (General Purpose Reg 21)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101001101";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "10101";
      RW_TB <= '1';
      SA_TB <= "10101";
      SB_TB <= "10101";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 22 (General Purpose Reg 22)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101001100";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "10110";
      RW_TB <= '1';
      SA_TB <= "10110";
      SB_TB <= "10110";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 23 (General Purpose Reg 23)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101001011";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "10111";
      RW_TB <= '1';
      SA_TB <= "10111";
      SB_TB <= "10111";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 24 (General Purpose Reg 24)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101001010";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "11000";
      RW_TB <= '1';
      SA_TB <= "11000";
      SB_TB <= "11000";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 25 (General Purpose Reg 25)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101001001";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "11001";
      RW_TB <= '1';
      SA_TB <= "11001";
      SB_TB <= "11001";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 26 (General Purpose Reg 26)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101001000";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "11010";
      RW_TB <= '1';
      SA_TB <= "11010";
      SB_TB <= "11010";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 27 (General Purpose Reg 27)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101000111";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "11011";
      RW_TB <= '1';
      SA_TB <= "11011";
      SB_TB <= "11011";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 28 (General Purpose Reg 28)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101000110";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "11100";
      RW_TB <= '1';
      SA_TB <= "11100";
      SB_TB <= "11100";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 29 (General Purpose Reg 29)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101000101";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "11101";
      RW_TB <= '1';
      SA_TB <= "11101";
      SB_TB <= "11101";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 30 (General Purpose Reg 30)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101000100";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "11110";
      RW_TB <= '1';
      SA_TB <= "11110";
      SB_TB <= "11110";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;

  --ID - 31 (General Purpose Reg 31)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101000011";
      MD_TB <= '1';
      FS_TB <= "00000";
      DR_TB <= "11111";
      RW_TB <= '1';
      SA_TB <= "11111";
      SB_TB <= "11111";
      TA_TB <= "0000";
      TB_TB <= "0000";
      TD_TB <= "0000";
  wait for period;


  --ID - 1 (Temp Reg01)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101100001";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "0001";
      TB_TB <= "0001";
      TD_TB <= "0001";
  wait for period;

  --ID - 2 (Temp Reg02)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101100000";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "0010";
      TB_TB <= "0010";
      TD_TB <= "0010";
  wait for period;

  --ID - 3 (Temp Reg03)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011111";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "0011";
      TB_TB <= "0011";
      TD_TB <= "0011";
  wait for period;

  --ID - 4 (Temp Reg04)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011110";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "0100";
      TB_TB <= "0100";
      TD_TB <= "0100";
  wait for period;

  --ID - 5 (Temp Reg05)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011101";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "0101";
      TB_TB <= "0101";
      TD_TB <= "0101";
  wait for period;

  --ID - 6 (Temp Reg06)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011100";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "0110";
      TB_TB <= "0110";
      TD_TB <= "0110";
  wait for period;

  --ID - 7 (Temp Reg07)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011011";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "0111";
      TB_TB <= "0111";
      TD_TB <= "0111";

  wait for period;

  --ID - 8 (Temp Reg08)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011010";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "1000";
      TB_TB <= "1000";
      TD_TB <= "1000";
  wait for period;

  --ID - 9 (Temp Reg09)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011001";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "1001";
      TB_TB <= "1001";
      TD_TB <= "1001";

  wait for period;

  --ID - 10 (Temp Reg10)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101011000";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "1010";
      TB_TB <= "1010";
      TD_TB <= "1010";
  wait for period;

  --ID - 11 (Temp Reg11)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101010111";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "1011";
      TB_TB <= "1011";
      TD_TB <= "1011";
  wait for period;

  --ID - 12 (Temp Reg12)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101010110";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "1100";
      TB_TB <= "1100";
      TD_TB <= "1100";
  wait for period;

  --ID - 13 (Temp Reg13)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101010101";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "1101";
      TB_TB <= "1101";
      TD_TB <= "1101";
  wait for period;

  --ID - 14 (Temp Reg14)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101010100";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "1110";
      TB_TB <= "1110";
      TD_TB <= "1110";
  wait for period;

  --ID - 15 (Temp Reg15)
  wait until Clock_TB'event and Clock_TB='1';
      MB_TB <= '0';
      DATA_IN_TB <= "00000001010001011111110101010011";
      MD_TB <= '1';       
      FS_TB <= "00000";
      DR_TB <= "00000";
      RW_TB <= '1';
      SA_TB <= "00000";
      SB_TB <= "00000";
      TA_TB <= "1111";
      TB_TB <= "1111";
      TD_TB <= "1111";
  wait for period;

  --PERFORMING FUNCTIONAL UNIT OPERATIONS--

--00110 DR (Last Dig ID)
--01011 SA (Last Dig ID + 5)
--10101 SB (Last Dig ID + 15)

    wait for period;
    FS_TB <= "01000"; --AND 

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "01010"; --OR 

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "00110"; --A - 1 

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "10100"; --srB 

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "00101"; --A + 1s c B + 1 

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "01110"; --1s c A 

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "00100"; --A + 1s c B 

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "10000"; --B 

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "00011"; --A + B + 1 

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "11000"; --slB 

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "00010"; --A + B 

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "00111"; --A (00111) 

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "00000"; --A (00000) 

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "00001"; --A + 1 

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "01100"; --A XOR B  

    MD_TB <= '0';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;

    --PERFORMING ONLY A AND B PORT FUNCTIONAL UNIT OPERATIONS USING IR AND MUX B--
    
    IR_IN_TB <= "00000001010001011111110101100010";

    wait for period;
    FS_TB <= "01000"; --AND

    MB_TB <= '1';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "01010"; --OR 

    MB_TB <= '1';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    -- wait for period;
    -- FS_TB <= "00110"; --A - 1 

    wait for period;
    FS_TB <= "10100"; --srB 

    MB_TB <= '1';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "00101"; --A + 1s c B + 1 

    MB_TB <= '1';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    -- wait for period;
    -- FS_TB <= "01110"; --1s c A 

    wait for period;
    FS_TB <= "00100"; --A + 1s c B 

    MB_TB <= '1';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "10000"; --B 

    MB_TB <= '1';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "00011"; --A + B + 1 

    MB_TB <= '1';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "11000"; --slB 

    MB_TB <= '1';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;
    FS_TB <= "00010"; --A + B 

    MB_TB <= '1';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    -- wait for period;
    -- FS_TB <= "00111"; --A (00111) 

    -- wait for period;
    -- FS_TB <= "00000"; --A (00000) 

    -- wait for period;
    -- FS_TB <= "00001"; --A + 1 

    wait for period;
    FS_TB <= "01100"; --A XOR B  

    MB_TB <= '1';
    DR_TB <= "00110";
    SA_TB <= "01011";
    SB_TB <= "10101";
    TA_TB <= "0000";
    TB_TB <= "0000";
    TD_TB <= "0000";

    wait for period;


   end process;
end Sim;
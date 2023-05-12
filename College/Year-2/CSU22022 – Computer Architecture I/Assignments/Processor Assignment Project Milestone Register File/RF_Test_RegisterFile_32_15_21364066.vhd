----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 23.10.2022 18:46:50
-- Design Name: 
-- Module Name: RF_Test_RegisterFile_32_15_21364066 - Behavioral
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

entity RF_Test_RegisterFile_32_15_21364066 is
    Port ( A_B_DataIN_Test : in std_logic_vector (1 downto 0);
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
end RF_Test_RegisterFile_32_15_21364066;

architecture Behavioral of RF_Test_RegisterFile_32_15_21364066 is

    --RF_RegisterFile_32_15_21364066
    component RF_RegisterFile_32_15_21364066
    port(
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

    -- RF_Mux3_32Bit_21364066
    component RF_Mux3_32Bit_21364066
    port(
        In00 : in std_logic_vector(31 downto 0);
        In01 : in std_logic_vector(31 downto 0);
        In02 : in std_logic_vector(31 downto 0);
        A : in std_logic_vector(1 downto 0);
        Z : out std_logic_vector(31 downto 0)
    );
    end component;

    --RegFile outputs
    signal A_out, B_out : std_logic_vector (31 downto 0);

    --Mux3 output
    signal Z_out : std_logic_vector (31 downto 0);
begin

    RegFile: RF_RegisterFile_32_15_21364066 port map(
        Clock => Clock_Test,
        D => Z_out,
        DR => DR_Test, 
        RW => RW_Test, 
        SA => SA_Test, 
        SB => SB_Test, 
        TA => TA_Test, 
        TB => TB_Test, 
        TD => TD_Test,
        A => A_out,
        B => B_out
    );

    Mux3: RF_Mux3_32Bit_21364066 port map(
        A => A_B_DataIN_Test,
        In00 => A_out,
        In01 => B_out,
        In02 => D_Test,
        Z => Z_out
    );

end Behavioral;

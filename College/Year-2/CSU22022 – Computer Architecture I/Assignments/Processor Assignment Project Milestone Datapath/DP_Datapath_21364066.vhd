----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 07.11.2022 00:49:00
-- Design Name: 
-- Module Name: DP_Datapath_21364066 - Behavioral
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
--use UNISIM.Vcomponents.all;

entity DP_Datapath_21364066 is
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
end DP_Datapath_21364066;

architecture Behavioral of DP_Datapath_21364066 is
--RF_RegisterFile_32_15_21364066
    component RF_RegisterFile_32_15_21364066
        port(SA: in std_logic_vector(4 downto 0); 
            DR: in std_logic_vector(4 downto 0);
            SB: in std_logic_vector(4 downto 0);
            RW: in std_logic;
            Clock: in std_logic;
            D: in std_logic_vector(31 downto 0);
            TB: in std_logic_vector(3 downto 0);
            TD: in std_logic_vector(3 downto 0);
            TA: in std_logic_vector(3 downto 0);
            A: out std_logic_vector(31 downto 0);
            B: out std_logic_vector(31 downto 0)
        );
    end component;
--CPU_Mux2_32Bit_21364066
    component CPU_Mux2_32Bit_21364066
        port(
            In0 : in  std_logic_vector(31 downto 0);
            In1 : in  std_logic_vector(31 downto 0);
            Sel : in  std_logic;
            Z : out  std_logic_vector(31 downto 0)
        );
    end component;
--DP_FunctionalUnit_21364066  
    component DP_FunctionalUnit_21364066
        port(
            B : in std_logic_vector(31 DOWNTO 0);
            A : in std_logic_vector(31 DOWNTO 0);
            FS : in std_logic_vector(4 DOWNTO 0);
            C : out std_logic;
            F : out std_logic_vector(31 DOWNTO 0);
            N : out std_logic;
            Z : out std_logic;
            V : out std_logic
        );
    end component;
    
    --Internal Signals Connecting Components
    signal B_to_D : std_logic_vector(31 downto 0);
    signal D_to_R : std_logic_vector(31 downto 0);
    signal RF_to_ADD : std_logic_vector(31 downto 0);
    signal RF_to_B : std_logic_vector(31 downto 0);
    signal FU_to_D : std_logic_vector(31 downto 0);

    begin
    
    --CPU_Mux2_32Bit_21364066
    MuxB : CPU_Mux2_32Bit_21364066 port map (
            In0 => RF_to_B,
            In1 => IR_IN,
            Sel => MB,
            Z => B_to_D
    );

    --CPU_Mux2_32Bit_21364066      
    MuxD: CPU_Mux2_32Bit_21364066 port map (
            In0 => FU_to_D,
            In1 => DATA_IN,
            Sel => MD,
            Z => D_to_R
    );


    --DP_FunctionalUnit_21364066
    FunctionalUnit : DP_FunctionalUnit_21364066 port map (
          B => B_to_D,
          A => RF_to_ADD,
          FS => FS,
          C => C,
          F => FU_to_D,
          N => N,
          Z => Z,
          V => V
    );

    --RF_RegisterFile_32_15_21364066
    RegFile : RF_RegisterFile_32_15_21364066 port map (
            SA => SA,
            DR => DR,
            SB => SB,
            RW => RW,
            Clock => Clock,
            D => D_to_R,
            TB => TB,
            TD => TD,
            TA => TA,
            A => RF_to_ADD,
            B => RF_to_B
    );
       
       ADD <= RF_to_ADD;
       DATA_OUT <= B_to_D;

end Behavioral;

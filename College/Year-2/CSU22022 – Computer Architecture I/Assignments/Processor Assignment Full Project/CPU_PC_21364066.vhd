----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 22:35:00
-- Design Name: 
-- Module Name: CPU_PC_21364066 - Behavioral
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

entity CPU_PC_21364066 is
    port (  Clock : in std_logic;
            Displacement : in std_logic_vector(31 downto 0);
            PI : in std_logic;
            PL : in std_logic;
            Reset : in std_logic;

            InstAdd : out std_logic_vector(31 downto 0)
        );
end CPU_PC_21364066;

architecture Behavioral of CPU_PC_21364066 is
    component CPU_Mux2_32Bit_21364066 is
        port( In0 : in std_logic_vector(31 downto 0);
              In1 : in std_logic_vector(31 downto 0);
              Sel : in std_logic;

              Z : out std_logic_vector(31 downto 0)
        );
    end component;

    component DP_RippleCarryAdder32Bit_21364066 is
        port(  A : in std_logic_vector(31 downto 0);
               B : in std_logic_vector(31 downto 0);
               C_IN : in std_logic;

               SUM : out std_logic_vector(31 downto 0)
        );
    end component;

    component RF_Register32Bit_21364066 is 
        port(   CLK : in std_logic;
                D : in std_logic_vector(31 downto 0);
                Load : in std_logic;

                Q : out std_logic_vector(31 downto 0)
        );
    end component;

    --Internal Signals Connecting Components
    signal PL_PI_Mux_to_Adder : std_logic_vector(31 downto 0);
    signal PC_Out : std_logic_vector(31 downto 0);
    signal Adder_to_ResetMux : std_logic_vector(31 downto 0);
    signal ResetMux_to_PC : std_logic_vector(31 downto 0);

    signal PCLoad0 : std_logic;
    signal PCLoad1 : std_logic;

    begin
        PCLoad0 <= Reset OR PL after 10ns;
        PCLoad1 <= PCLoad0 OR PI after 10ns;

        PL_PI_Mux : CPU_Mux2_32Bit_21364066 port map(
            In0 => Displacement,
            In1 => X"00000001",
            Sel => PI,

            Z => PL_PI_Mux_to_Adder
        );

        Adder : DP_RippleCarryAdder32Bit_21364066 port map(
            A => PC_Out,  --InstAdd PC_Out
            B => PL_PI_Mux_to_Adder,
            C_IN => '0',

            SUM => Adder_to_ResetMux
        );

        ResetMux : CPU_Mux2_32Bit_21364066 port map(
            In0 => Adder_to_ResetMux,
            In1 => X"00000006",
            Sel => Reset,

            Z => ResetMux_to_PC
        );

        PC : RF_Register32Bit_21364066 port map(
            CLK => Clock,
            D => ResetMux_to_PC,
            Load => PCLoad1,

            Q => PC_Out
        );

    InstAdd <= PC_Out;

end Behavioral;

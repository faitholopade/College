----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 06.11.2022 09:33:00
-- Design Name: 
-- Module Name: DP_FunctionalUnit_21364066 - Behavioral
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

entity DP_FunctionalUnit_21364066 is
    port (
        B : in std_logic_vector(31 downto 0);
        A : in std_logic_vector(31 downto 0);
        FS : in std_logic_vector(4 downto 0);
        C : out std_logic;
        F : out std_logic_vector(31 downto 0);
        N : out std_logic;
        Z : out std_logic;
        V : out std_logic
    );
end DP_FunctionalUnit_21364066;

architecture Behavioral of DP_FunctionalUnit_21364066 is

    component DP_Shifter_21364066 
        port(   B : in std_logic_vector(31 downto 0);
                S1 : in std_logic;
                S2 : in std_logic;
                C : out std_logic;
                G : out std_logic_vector(31 downto 0)
        );
    end component;

    component DP_ArithmeticLogicUnit_21364066
        port(   A : in std_logic_vector(31 downto 0);
                B : in std_logic_vector(31 downto 0);
                C_IN : in std_logic;
                S0 : in std_logic;
                S1 : in std_logic;
                S2 : in std_logic;
                C : out std_logic;
                G : out std_logic_vector (31 downto 0);
                V : out std_logic
        );
    end component;

    component CPU_Mux2_32Bit_21364066 
        port(   In0 : in std_logic_vector (31 downto 0);
                In1 : in std_logic_vector (31 downto 0);
                Sel : in std_logic;
                Z : out std_logic_vector (31 downto 0)
        );
    end component;

    component DP_CFlagMux2_1Bit_21364066
        port(   C_ALU : in std_logic;
                C_Shift : in std_logic;
                FS4 : in std_logic;
                C : out std_logic
        );
    end component;

    component DP_ZeroDetection_21364066 
        port(   MuxF_to_F : in std_logic_vector (31 downto 0);
                Z : out std_logic
        );
    end component;

    signal S_to_C : std_logic;
    signal A_to_C : std_logic;
    signal S_to_M : std_logic_vector (31 downto 0) ;
    signal A_to_M : std_logic_vector(31 downto 0);
    signal M_to_Z : std_logic_vector(31 downto 0);
    signal F_to_FMux : std_logic_vector(31 downto 0);
        
    begin
        Shifter : DP_Shifter_21364066 port map (
            B => B,
            S1 => FS(2),
            S2 => FS(3),
            C => S_to_C,
            G => S_to_M
        );
          
        ALU : DP_ArithmeticLogicUnit_21364066 port map (
            A => A,
            B => B,
            C_IN => FS(0),
            S0 => FS(1),
            S1 => FS(2),
            S2 => FS(3),
            C => A_to_C,
            G => A_to_M,
            V => V
        );
          
        MuxF : CPU_Mux2_32Bit_21364066 port map (
            In0 => A_to_M,
            In1 => S_to_M,
            Sel => FS(4),
            Z => F_to_FMux 
        );
          
        C_Flag : DP_CFlagMux2_1Bit_21364066 port map (
            C_ALU => A_to_C,
            C_Shift => S_to_C,
            FS4 => FS(4),
            C => C
        );
          
        Z_Flag : DP_ZeroDetection_21364066 port map (
            MuxF_to_F => F_to_FMux ,
            Z => Z
        );
         
         N <= F_to_FMux (31);
         F <= F_to_FMux ;

end Behavioral;

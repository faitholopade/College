----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 03.11.2022 06:44:00
-- Design Name: 
-- Module Name: DP_ArithmeticLogicUnit_21364066 - Behavioral
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

entity DP_ArithmeticLogicUnit_21364066 is
    port (
        A : in std_logic_vector(31 downto 0);
        C_IN : in std_logic;
        B : in std_logic_vector(31 downto 0);
        S0 : in std_logic;
        S1 : in std_logic;
        S2: in std_logic;
        C: out std_logic;
        V : out std_logic;
        G : out std_logic_vector(31 downto 0)
    );
end DP_ArithmeticLogicUnit_21364066;

architecture Behavioral of DP_ArithmeticLogicUnit_21364066 is
    --DP_32Bit_B_Logic_21364066
    component DP_32Bit_B_Logic_21364066
        port( B : in std_logic_vector(31 downto 0);
              S0 : in std_logic;
              S1 : in std_logic;
              Y : out std_logic_vector(31 downto 0)
        );
    end component;

     --DP_32Bit_LogicCircuit_21364066
     component DP_32Bit_LogicCircuit_21364066
     port( A : in std_logic_vector (31 downto 0);
           B : in std_logic_vector (31 downto 0);
           S0 : in std_logic;
           S1 : in std_logic;
           G : out std_logic_vector (31 downto 0)
     );
    end component;

    --DP_RippleCarryAdder32Bit_21364066
    component DP_RippleCarryAdder32Bit_21364066
        port( A : in std_logic_vector(31 downto 0);
              B : in std_logic_vector(31 downto 0);
              C_IN : in std_logic;
              C_OUT : out std_logic;
              SUM : out std_logic_vector(31 downto 0);
              V : out std_logic
        );
    end component;

    --CPU_Mux2_32Bit_21364066
    component CPU_Mux2_32Bit_21364066
        port ( In0 : in std_logic_vector (31 downto 0);
               In1 : in std_logic_vector (31 downto 0);
               Sel : in std_logic;
               Z : out std_logic_vector (31 downto 0)
        );
    end component;

    signal BLogOut : std_logic_vector (31 downto 0);
    signal RCAOut : std_logic_vector (31 downto 0);
    signal LogicCircOut : std_logic_vector (31 downto 0);

    begin
        BLogic: DP_32Bit_B_Logic_21364066 port map(
            B => B,
            S0 => S0,
            S1 => S1,
            Y => BLogOut

        );

        LogicCircuit: DP_32Bit_LogicCircuit_21364066 port map(
            A => A,
            B => B,
            S0 => S0,
            S1 => S1,
            G => LogicCircOut
        );

        Adder: DP_RippleCarryAdder32Bit_21364066 port map(
            A => A,
            B => BLogOut,
            C_IN => C_IN, 
            C_OUT => C,
            SUM => RCAOut, 
            V => V
        );

        ALUMux: CPU_Mux2_32Bit_21364066 port map(
            In0 => RCAOut,
            In1 => LogicCircOut,
            Sel => S2,
            Z => G
        );

end Behavioral;

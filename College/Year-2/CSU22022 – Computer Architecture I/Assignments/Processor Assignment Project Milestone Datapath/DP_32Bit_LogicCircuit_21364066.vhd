----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 02.11.2022 13:35:00
-- Design Name: 
-- Module Name: DP_32Bit_LogicCircuit_21364066 - Behavioral
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

entity DP_32Bit_LogicCircuit_21364066 is
    port (
        A : in std_logic_vector(31 downto 0);
        B : in std_logic_vector(31 downto 0);
        S0 : in std_logic;
        S1 : in std_logic;
        G : out std_logic_vector(31 downto 0)
        );
end DP_32Bit_LogicCircuit_21364066;

architecture Behavioral of DP_32Bit_LogicCircuit_21364066 is
    --DP_SingleBit_LogicCircuit_21364066
    component DP_SingleBit_LogicCircuit_21364066
        port( A : in std_logic;
              B : in std_logic;
              S0 : in std_logic;
              S1 : in std_logic;
              G : out std_logic

        );
    end component;
    
    begin
    -- Instantiate Logic Circuit Bit 0
        BIT00: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(0),
            B => B(0),
            S0 => S0,
            S1 => S1,
            G => G(0)
        );
    -- Instantiate Logic Circuit Bit 1   
        BIT01: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(1),
            B => B(1),
            S0 => S0,
            S1 => S1,
            G => G(1)
        );
    -- Instantiate Logic Circuit Bit 2    
        BIT02: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(2),
            B => B(2),
            S0 => S0,
            S1 => S1,
            G => G(2)
        );
    -- Instantiate Logic Circuit Bit 3       
        BIT03: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(3),
            B => B(3),
            S0 => S0,
            S1 => S1,
            G => G(3)
        );
    -- Instantiate Logic Circuit Bit 4     
        BIT04: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(4),
            B => B(4),
            S0 => S0,
            S1 => S1,
            G => G(4)
        );
    -- Instantiate Logic Circuit Bit 5     
        BIT05: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(5),
            B => B(5),
            S0 => S0,
            S1 => S1,
            G => G(5)
        );
    -- Instantiate Logic Circuit Bit 6   
        BIT06: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(6),
            B => B(6),
            S0 => S0,
            S1 => S1,
            G => G(6)
        );
    -- Instantiate Logic Circuit Bit 7   
        BIT07: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(7),
            B => B(7),
            S0 => S0,
            S1 => S1,
            G => G(7)
        );
    -- Instantiate Logic Circuit Bit 8    
        BIT08: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(8),
            B => B(8),
            S0 => S0,
            S1 => S1,
            G => G(8)
        );
    -- Instantiate Logic Circuit Bit 9  
        BIT09: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(9),
            B => B(9),
            S0 => S0,
            S1 => S1,
            G => G(9)
        );
    -- Instantiate Logic Circuit Bit 10   
        BIT10: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(10),
            B => B(10),
            S0 => S0,
            S1 => S1,
            G => G(10)
        );
    -- Instantiate Logic Circuit Bit 11
        BIT11: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(11),
            B => B(11),
            S0 => S0,
            S1 => S1,
            G => G(11)
        );
    -- Instantiate Logic Circuit Bit 12
        BIT12: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(12),
            B => B(12),
            S0 => S0,
            S1 => S1,
            G => G(12)
        );
    -- Instantiate Logic Circuit Bit 13 
        BIT13: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(13),
            B => B(13),
            S0 => S0,
            S1 => S1,
            G => G(13)
        );
    -- Instantiate Logic Circuit Bit 14
        BIT14: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(14),
            B => B(14),
            S0 => S0,
            S1 => S1,
            G => G(14)
        );
    -- Instantiate Logic Circuit Bit 15  
        BIT15: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(15),
            B => B(15),
            S0 => S0,
            S1 => S1,
            G => G(15)
        );
    -- Instantiate Logic Circuit Bit 16    
         BIT16: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(16),
            B => B(16),
            S0 => S0,
            S1 => S1,
            G => G(16)
        );
    -- Instantiate Logic Circuit Bit 17   
        BIT17: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(17),
            B => B(17),
            S0 => S0,
            S1 => S1,
            G => G(17)
        );
    -- Instantiate Logic Circuit Bit 18   
        BIT18: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(18),
            B => B(18),
            S0 => S0,
            S1 => S1,
            G => G(18)
        );
    -- Instantiate Logic Circuit Bit 19
        BIT19: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(19),
            B => B(19),
            S0 => S0,
            S1 => S1,
            G => G(19)
        );
    -- Instantiate Logic Circuit Bit 20  
        BIT20: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(20),
            B => B(20),
            S0 => S0,
            S1 => S1,
            G => G(20)
        );
    -- Instantiate Logic Circuit Bit 21 
        BIT21: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(21),
            B => B(21),
            S0 => S0,
            S1 => S1,
            G => G(21)
        );
    -- Instantiate Logic Circuit Bit 22  
        BIT22: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(22),
            B => B(22),
            S0 => S0,
            S1 => S1,
            G => G(22)
        );
    -- Instantiate Logic Circuit Bit 23
        BIT23: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(23),
            B => B(23),
            S0 => S0,
            S1 => S1,
            G => G(23)
        );
    -- Instantiate Logic Circuit Bit 24    
        BIT24: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(24),
            B => B(24),
            S0 => S0,
            S1 => S1,
            G => G(24)
        );
    -- Instantiate Logic Circuit Bit 25 
        BIT25: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(25),
            B => B(25),
            S0 => S0,
            S1 => S1,
            G => G(25)
        );
    -- Instantiate Logic Circuit Bit 26 
        BIT26: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(26),
            B => B(26),
            S0 => S0,
            S1 => S1,
            G => G(26)
        );
    -- Instantiate Logic Circuit Bit 27   
        BIT27: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(27),
            B => B(27),
            S0 => S0,
            S1 => S1,
            G => G(27)
        );
    -- Instantiate Logic Circuit Bit 28
        BIT28: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(28),
            B => B(28),
            S0 => S0,
            S1 => S1,
            G => G(28)
        );
    -- Instantiate Logic Circuit Bit 29   
        BIT29: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(29),
            B => B(29),
            S0 => S0,
            S1 => S1,
            G => G(29)
        );
    -- Instantiate Logic Circuit Bit 30
        BIT30: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(30),
            B => B(30),
            S0 => S0,
            S1 => S1,
            G => G(30)
        );
    -- Instantiate Logic Circuit Bit 31    
        BIT31: DP_SingleBit_LogicCircuit_21364066 port map (
            A => A(31),
            B => B(31),
            S0 => S0,
            S1 => S1,
            G => G(31)
        );
    
end Behavioral;

----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 01.11.2022 22:35:00
-- Design Name: 
-- Module Name: DP_32Bit_B_Logic_21364066 - Behavioral
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

entity DP_32Bit_B_Logic_21364066 is
    Port( B : in std_logic_vector (31 downto 0);
          S0, S1 : in std_logic;
          Y : out std_logic_vector (31 downto 0)
    );
end DP_32Bit_B_Logic_21364066;

architecture Behavioral of DP_32Bit_B_Logic_21364066 is
    --DP_SingleBit_B_Logic_21364066
    component DP_SingleBit_B_Logic_21364066
        Port(
            B : in std_logic;
            S0, S1 : in std_logic;
            Y : out std_logic
            );
    end component;
    
begin
-- Instantiate B Logic Bit 0
    BIT00: DP_SingleBit_B_Logic_21364066 port map(
        B => B(0),
        S0 => S0,
        S1 => S1,
        Y => Y(0)
    );
-- Instantiate B Logic Bit 1    
    BIT01: DP_SingleBit_B_Logic_21364066 port map(
        B => B(1),
        S0 => S0,
        S1 => S1,
        Y => Y(1)
    );
-- Instantiate B Logic Bit 2    
    BIT02: DP_SingleBit_B_Logic_21364066 port map(
        B => B(2),
        S0 => S0,
        S1 => S1,
        Y => Y(2)
    );
-- Instantiate B Logic Bit 3
    BIT03: DP_SingleBit_B_Logic_21364066 port map(
        B => B(3),
        S0 => S0,
        S1 => S1,
        Y => Y(3)
    );
-- Instantiate B Logic Bit 4    
    BIT04: DP_SingleBit_B_Logic_21364066 port map(
        B => B(4),
        S0 => S0,
        S1 => S1,
        Y => Y(4)
    );
-- Instantiate B Logic Bit 5     
    BIT05: DP_SingleBit_B_Logic_21364066 port map(
        B => B(5),
        S0 => S0,
        S1 => S1,
        Y => Y(5)
    );
-- Instantiate B Logic Bit 6     
    BIT06: DP_SingleBit_B_Logic_21364066 port map(
        B => B(6),
        S0 => S0,
        S1 => S1,
        Y => Y(6)
    );
-- Instantiate B Logic Bit 7   
    BIT07: DP_SingleBit_B_Logic_21364066 port map(
        B => B(7),
        S0 => S0,
        S1 => S1,
        Y => Y(7)
    );
-- Instantiate B Logic Bit 8     
    BIT08: DP_SingleBit_B_Logic_21364066 port map(
        B => B(8),
        S0 => S0,
        S1 => S1,
        Y => Y(8)
    );
-- Instantiate B Logic Bit 9    
    BIT09: DP_SingleBit_B_Logic_21364066 port map(
        B => B(9),
        S0 => S0,
        S1 => S1,
        Y => Y(9)
    );
-- Instantiate B Logic Bit 10
    BIT10: DP_SingleBit_B_Logic_21364066 port map(
        B => B(10),
        S0 => S0,
        S1 => S1,
        Y => Y(10)
    );
-- Instantiate B Logic Bit 11
    BIT11: DP_SingleBit_B_Logic_21364066 port map(
        B => B(11),
        S0 => S0,
        S1 => S1,
        Y => Y(11)
    );
-- Instantiate B Logic Bit 12 
    BIT12: DP_SingleBit_B_Logic_21364066 port map(
        B => B(12),
        S0 => S0,
        S1 => S1,
        Y => Y(12)
    );
-- Instantiate B Logic Bit 13    
    BIT13: DP_SingleBit_B_Logic_21364066 port map(
        B => B(13),
        S0 => S0,
        S1 => S1,
        Y => Y(13)
    );
-- Instantiate B Logic Bit 14  
    BIT14: DP_SingleBit_B_Logic_21364066 port map(
        B => B(14),
        S0 => S0,
        S1 => S1,
        Y => Y(14)
    );
-- Instantiate B Logic Bit 15    
    BIT15: DP_SingleBit_B_Logic_21364066 port map(
        B => B(15),
        S0 => S0,
        S1 => S1,
        Y => Y(15)
    );
-- Instantiate B Logic Bit 16   
     BIT16: DP_SingleBit_B_Logic_21364066 port map(
        B => B(16),
        S0 => S0,
        S1 => S1,
        Y => Y(16)
    );
-- Instantiate B Logic Bit 17     
    BIT17: DP_SingleBit_B_Logic_21364066 port map(
        B => B(17),
        S0 => S0,
        S1 => S1,
        Y => Y(17)
    );
-- Instantiate B Logic Bit 18       
    BIT18: DP_SingleBit_B_Logic_21364066 port map(
        B => B(18),
        S0 => S0,
        S1 => S1,
        Y => Y(18)
    );
-- Instantiate B Logic Bit 19    
    BIT19: DP_SingleBit_B_Logic_21364066 port map(
        B => B(19),
        S0 => S0,
        S1 => S1,
        Y => Y(19)
    );
-- Instantiate B Logic Bit 20    
    BIT20: DP_SingleBit_B_Logic_21364066 port map(
        B => B(20),
        S0 => S0,
        S1 => S1,
        Y => Y(20)
    );
-- Instantiate B Logic Bit 21     
    BIT21: DP_SingleBit_B_Logic_21364066 port map(
        B => B(21),
        S0 => S0,
        S1 => S1,
        Y => Y(21)
    );
-- Instantiate B Logic Bit 22  
    BIT22: DP_SingleBit_B_Logic_21364066 port map(
        B => B(22),
        S0 => S0,
        S1 => S1,
        Y => Y(22)
    );
-- Instantiate B Logic Bit 23   
    BIT23: DP_SingleBit_B_Logic_21364066 port map(
        B => B(23),
        S0 => S0,
        S1 => S1,
        Y => Y(23)
    );
-- Instantiate B Logic Bit 24    
    BIT24: DP_SingleBit_B_Logic_21364066 port map(
        B => B(24),
        S0 => S0,
        S1 => S1,
        Y => Y(24)
    );
-- Instantiate B Logic Bit 25   
    BIT25: DP_SingleBit_B_Logic_21364066 port map(
        B => B(25),
        S0 => S0,
        S1 => S1,
        Y => Y(25)
    );
-- Instantiate B Logic Bit 26    
    BIT26: DP_SingleBit_B_Logic_21364066 port map(
        B => B(26),
        S0 => S0,
        S1 => S1,
        Y => Y(26)
    );
-- Instantiate B Logic Bit 27    
    BIT27: DP_SingleBit_B_Logic_21364066 port map(
        B => B(27),
        S0 => S0,
        S1 => S1,
        Y => Y(27)
    );
-- Instantiate B Logic Bit 28   
    BIT28: DP_SingleBit_B_Logic_21364066 port map(
        B => B(28),
        S0 => S0,
        S1 => S1,
        Y => Y(28)
    );
-- Instantiate B Logic Bit 29    
    BIT29: DP_SingleBit_B_Logic_21364066 port map(
        B => B(29),
        S0 => S0,
        S1 => S1,
        Y => Y(29)
    );
-- Instantiate B Logic Bit 30   
    BIT30: DP_SingleBit_B_Logic_21364066 port map(
        B => B(30),
        S0 => S0,
        S1 => S1,
        Y => Y(30)
    );
-- Instantiate B Logic Bit 31    
    BIT31: DP_SingleBit_B_Logic_21364066 port map(
        B => B(31),
        S0 => S0,
        S1 => S1,
        Y => Y(31)
    );
end Behavioral;

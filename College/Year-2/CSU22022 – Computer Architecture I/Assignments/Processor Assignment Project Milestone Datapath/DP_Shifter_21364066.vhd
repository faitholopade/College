----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 04.11.2022 15:35:00
-- Design Name: 
-- Module Name: DP_Shifter_21364066 - Behavioral
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

entity DP_Shifter_21364066 is
    port (
        B : in std_logic_vector (31 downto 0);
        S1 : in std_logic;
        S2 : in std_logic;
        G : out std_logic_vector(31 downto 0);
        C : out std_logic
    );
end DP_Shifter_21364066;

architecture Behavioral of DP_Shifter_21364066 is
    
    component DP_Mux_3_1Bit_21364066 is
        port(  Bi : in std_logic;
               S1 : in std_logic;
               S2 : in std_logic;
               slBi : in std_logic;
               srBi : in std_logic;
               Gi : out std_logic
        );
    end component;

    component DP_ShifterCFlagMux2_1Bit_21364066 is
        port(  LSB : in std_logic;
               MSB : in std_logic;
               S1 : in std_logic;
               S2 : in std_logic;
               C : out std_logic
        );
    end component;

    begin
        -- Instantiate Bit 0
        BIT00 : DP_Mux_3_1Bit_21364066 port map(
            Bi => B(0),
            S1 => S1,
            S2 => S2,
            slBi => '0',
            srBi => B(1),
            Gi => G(0)        
        );
        -- Instantiate Bit 1  
        BIT01 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(1),
                S1 => S1,
                S2 => S2,
                slBi => B(0),
                srBi => B(2),
                Gi => G(1)        
            );
        -- Instantiate Bit 2       
        BIT02 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(2),
                S1 => S1,
                S2 => S2,
                slBi => B(1),
                srBi => B(3),
                Gi => G(2)        
            );
        -- Instantiate Bit 3       
        BIT03 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(3),
                S1 => S1,
                S2 => S2,
                slBi => B(2),
                srBi => B(4),
                Gi => G(3)        
            );
        -- Instantiate Bit 4        
        BIT04 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(4),
                S1 => S1,
                S2 => S2,
                slBi => B(3),
                srBi => B(5),
                Gi => G(4)        
            );
        -- Instantiate Bit 5        
        BIT05 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(5),
                S1 => S1,
                S2 => S2,
                slBi => B(4),
                srBi => B(6),
                Gi => G(5)        
            );
        -- Instantiate Bit 6       
        BIT06 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(6),
                S1 => S1,
                S2 => S2,
                slBi => B(5),
                srBi => B(7),
                Gi => G(6)        
            );
        -- Instantiate Bit 7       
        BIT07 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(7),
                S1 => S1,
                S2 => S2,
                slBi => B(6),
                srBi => B(8),
                Gi => G(7)        
            );
        -- Instantiate Bit 8        
        BIT08 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(8),
                S1 => S1,
                S2 => S2,
                slBi => B(7),
                srBi => B(9),
                Gi => G(8)        
            );
        -- Instantiate Bit 9    
        BIT09 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(9),
                S1 => S1,
                S2 => S2,
                slBi => B(8),
                srBi => B(10),
                Gi => G(9)        
            );
        -- Instantiate Bit 10        
        BIT10 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(10),
                S1 => S1,
                S2 => S2,
                slBi => B(9),
                srBi => B(11),
                Gi => G(10)        
            );
        -- Instantiate Bit 11        
        BIT11 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(11),
                S1 => S1,
                S2 => S2,
                slBi => B(10),
                srBi => B(12),
                Gi => G(11)        
            );
        -- Instantiate Bit 12        
        BIT12 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(12),
                S1 => S1,
                S2 => S2,
                slBi => B(11),
                srBi => B(13),
                Gi => G(12)        
            );
        -- Instantiate Bit 13        
        BIT13 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(13),
                S1 => S1,
                S2 => S2,
                slBi => B(12),
                srBi => B(14),
                Gi => G(13)        
            );
        -- Instantiate Bit 14        
        BIT14 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(14),
                S1 => S1,
                S2 => S2,
                slBi => B(13),
                srBi => B(15),
                Gi => G(14)        
            );       
        -- Instantiate Bit 15        
        BIT15 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(15),
                S1 => S1,
                S2 => S2,
                slBi => B(14),
                srBi => B(16),
                Gi => G(15)        
            );  
        -- Instantiate Bit 16    
        BIT16 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(16),
                S1 => S1,
                S2 => S2,
                slBi => B(15),
                srBi => B(17),
                Gi => G(16)        
            );
        -- Instantiate Bit 17       
        BIT17 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(17),
                S1 => S1,
                S2 => S2,
                slBi => B(16),
                srBi => B(18),
                Gi => G(17)        
            );
        -- Instantiate Bit 18        
        BIT18 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(18),
                S1 => S1,
                S2 => S2,
                slBi => B(17),
                srBi => B(19),
                Gi => G(18)        
            );
        -- Instantiate Bit 19        
        BIT19 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(19),
                S1 => S1,
                S2 => S2,
                slBi => B(18),
                srBi => B(20),
                Gi => G(19)        
            );
        -- Instantiate Bit 20        
        BIT20 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(20),
                S1 => S1,
                S2 => S2,
                slBi => B(19),
                srBi => B(21),
                Gi => G(20)        
            );
        -- Instantiate Bit 21        
        BIT21 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(21),
                S1 => S1,
                S2 => S2,
                slBi => B(20),
                srBi => B(22),
                Gi => G(21)        
            );
        -- Instantiate Bit 22        
        BIT22 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(22),
                S1 => S1,
                S2 => S2,
                slBi => B(21),
                srBi => B(23),
                Gi => G(22)        
            );
        -- Instantiate Bit 23        
        BIT23 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(23),
                S1 => S1,
                S2 => S2,
                slBi => B(22),
                srBi => B(24),
                Gi => G(23)        
            );
        -- Instantiate Bit 24        
        BIT24 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(24),
                S1 => S1,
                S2 => S2,
                slBi => B(23),
                srBi => B(25),
                Gi => G(24)        
            );
        -- Instantiate Bit 25       
        BIT25 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(25),
                S1 => S1,
                S2 => S2,
                slBi => B(24),
                srBi => B(26),
                Gi => G(25)        
            );
        -- Instantiate Bit 26        
        BIT26 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(26),
                S1 => S1,
                S2 => S2,
                slBi => B(25),
                srBi => B(27),
                Gi => G(26)        
            );
        -- Instantiate Bit 27        
        BIT27 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(27),
                S1 => S1,
                S2 => S2,
                slBi => B(26),
                srBi => B(28),
                Gi => G(27)        
            );
        -- Instantiate Bit 28        
        BIT28 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(28),
                S1 => S1,
                S2 => S2,
                slBi => B(27),
                srBi => B(29),
                Gi => G(28)        
            );
        -- Instantiate Bit 29        
        BIT29 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(29),
                S1 => S1,
                S2 => S2,
                slBi => B(28),
                srBi => B(30),
                Gi => G(29)        
            );
        -- Instantiate Bit 30        
        BIT30 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(30),
                S1 => S1,
                S2 => S2,
                slBi => B(29),
                srBi => B(31),
                Gi => G(30)        
            );
        -- Instantiate Bit 31        
        BIT31 : DP_Mux_3_1Bit_21364066 port map(
                Bi => B(31),
                S1 => S1,
                S2 => S2,
                slBi => B(30),
                srBi => '0',
                Gi => G(31)        
            );
        -- Instantiate  CFlagMux
        CFlagMux : DP_ShifterCFlagMux2_1Bit_21364066 port map(
                LSB => B(0),
                MSB => B(31),
                S1 => S1,
                S2 => S2,
                C => C  
            );
end Behavioral;

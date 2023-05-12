----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 20.10.2022 22:01:00
-- Design Name: 
-- Module Name: RF_TempDestReg_Decoder_21364066 - Behavioral
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

entity RF_TempDestReg_Decoder_21364066 is
    port (
            A: in std_logic_vector (3 downto 0); 

            Q00: out std_logic;
            Q01: out std_logic;
            Q02: out std_logic;
            Q03: out std_logic;
            Q04: out std_logic;
            Q05: out std_logic;
            Q06: out std_logic;
            Q07: out std_logic;
            Q08: out std_logic;
            Q09: out std_logic;
            Q10: out std_logic;
            Q11: out std_logic;
            Q12: out std_logic;
            Q13: out std_logic;
            Q14: out std_logic;
            Q15: out std_logic

        );
end RF_TempDestReg_Decoder_21364066;

architecture Behavioral of RF_TempDestReg_Decoder_21364066 is
    begin
       Q00 <= (not A(0) and not A(1) and not A(2) and not A(3)); --0000
       Q01 <= ( A(0) and not A(1) and not A(2) and not A(3)); --1000
       Q02 <= ( not A(0) and A(1) and not A(2) and not A(3)); --0100
       Q03 <= (     A(0) and  A(1) and not A(2) and not A(3)); --1100
       Q04 <= ( not A(0) and not A(1) and  A(2) and not A(3)); --0010
       Q05 <= ( A(0) and not A(1) and  A(2) and not A(3)); --1010
       Q06 <= ( not A(0) and  A(1) and A(2) and not A(3)); --0110
       Q07 <= ( A(0) and  A(1) and  A(2) and not A(3)); --1110
       Q08 <= ( not A(0) and not A(1) and not A(2) and  A(3)); --0001
       Q09 <= (  A(0) and not A(1) and not A(2) and A(3)); --1001
       Q10 <= ( not A(0) and  A(1) and not A(2) and A(3)); --0101
       Q11 <= ( A(0) and A(1) and not A(2) and  A(3));   --1101
       Q12 <= ( not A(0) and not A(1) and  A(2) and A(3)); --0011
       Q13 <= ( A(0) and not A(1) and A(2) and A(3)); --1011
       Q14 <= ( not A(0) and A(1) and A(2) and A(3)); --0111
       Q15 <= ( A(0) and A(1) and A(2) and A(3));  --1111
        -- else
        --  '00000000000000000000000000000000';
end Behavioral;

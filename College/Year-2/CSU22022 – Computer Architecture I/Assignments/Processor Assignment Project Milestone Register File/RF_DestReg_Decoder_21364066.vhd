----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 20.10.2022 11:35:00
-- Design Name: 
-- Module Name: RF_DestReg_Decoder_21364066 - Behavioral
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

entity RF_DestReg_Decoder_21364066 is
    port (
            A: in std_logic_vector (4 downto 0); 
            

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
            Q15: out std_logic;
            Q16: out std_logic;
            Q17: out std_logic;
            Q18: out std_logic;
            Q19: out std_logic;
            Q20: out std_logic;
            Q21: out std_logic;
            Q22: out std_logic;
            Q23: out std_logic;
            Q24: out std_logic;
            Q25: out std_logic;
            Q26: out std_logic;
            Q27: out std_logic;
            Q28: out std_logic;
            Q29: out std_logic;
            Q30: out std_logic;
            Q31: out std_logic

        );
end RF_DestReg_Decoder_21364066;

architecture Behavioral of RF_DestReg_Decoder_21364066 is
    begin
        Q00<= ((not A(0)) and (not A(1)) and (not A(2)) and (not A(3)) and (not A(4))) after 1 ns;
        Q01<= (A(0) and (not A(1)) and (not A(2)) and (not A(3)) and (not A(4))) after 1 ns;
        Q02<= ((not A(0)) and A(1) and (not A(2)) and (not A(3)) and (not A(4))) after 1 ns;
        Q03<= (A(0) and A(1) and (not A(2)) and (not A(3)) and (not A(4))) after 1 ns;
        Q04<= ((not A(0)) and (not A(1)) and A(2) and (not A(3)) and (not A(4))) after 1 ns;
        Q05<= (A(0) and (not A(1)) and A(2) and (not A(3)) and (not A(4))) after 1 ns;
        Q06<= ((not A(0)) and A(1) and A(2) and (not A(3)) and (not A(4))) after 1 ns;
        Q07<= (A(0) and A(1) and A(2) and (not A(3)) and (not A(4))) after 1 ns;
        Q08<= ((not A(0)) and (not A(1)) and (not A(2)) and A(3) and (not A(4))) after 1 ns;
        Q09<= (A(0) and (not A(1)) and (not A(2)) and A(3) and (not A(4))) after 1 ns;
        Q10<= ((not A(0)) and A(1) and (not A(2)) and A(3) and (not A(4))) after 1 ns;
        Q11<= (A(0) and A(1) and (not A(2)) and A(3) and (not A(4))) after 1 ns;
        Q12<= ((not A(0)) and (not A(1)) and A(2) and A(3) and (not A(4))) after 1 ns;
        Q13<= (A(0) and (not A(1)) and A(2) and A(3) and (not A(4))) after 1 ns;
        Q14<= ((not A(0)) and A(1) and A(2) and A(3) and (not A(4))) after 1 ns;
        Q15<= (A(0) and A(1) and A(2) and A(3) and (not A(4))) after 1 ns;
        Q16<= ((not A(0)) and (not A(1)) and (not A(2)) and (not A(3)) and A(4)) after 1 ns;
        Q17<= (A(0) and (not A(1)) and (not A(2)) and (not A(3)) and A(4)) after 1 ns;
        Q18<= ((not A(0)) and A(1) and (not A(2)) and (not A(3)) and A(4)) after 1 ns;
        Q19<= (A(0) and A(1) and (not A(2)) and (not A(3)) and A(4)) after 1 ns;
        Q20<= ((not A(0)) and (not A(1)) and A(2) and (not A(3)) and A(4)) after 1 ns;
        Q21<= (A(0) and (not A(1)) and A(2) and (not A(3)) and A(4)) after 1 ns;
        Q22<= ((not A(0)) and A(1) and A(2) and (not A(3)) and A(4)) after 1 ns;
        Q23<= (A(0) and A(1) and A(2) and (not A(3)) and A(4)) after 1 ns;
        Q24<= ((not A(0)) and (not A(1)) and (not A(2)) and A(3) and A(4)) after 1 ns;
        Q25<= (A(0) and (not A(1)) and (not A(2)) and A(3) and A(4)) after 1 ns;
        Q26<= ((not A(0)) and A(1) and (not A(2)) and A(3) and A(4)) after 1 ns;
        Q27<= (A(0) and A(1) and (not A(2)) and A(3) and A(4)) after 1 ns;
        Q28<= ((not A(0)) and (not A(1)) and A(2) and A(3) and A(4)) after 1 ns;
        Q29<= (A(0) and (not A(1)) and A(2) and A(3) and A(4)) after 1 ns;
        Q30<= ((not A(0)) and A(1) and A(2) and A(3) and A(4)) after 1 ns;
        Q31<= (A(0) and A(1) and A(2) and A(3) and A(4)) after 1 ns;
end Behavioral;

--00000
--00001
--00010
--00011
--00100
--00101
--00110
--00111
--01000
--01001
--01010
--01011
--01100
--01101
--01110
--01111
--10000
--10001
--10010
--10011
--10100
--10101
--10110
--10111
--11000
--11001
--11010
--11011
--11100
--11101
--11110
--11111 
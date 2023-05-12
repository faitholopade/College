----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 02.11.2022 13:45:00
-- Design Name: 
-- Module Name: DP_SingleBit_LogicCircuit_21364066 - Behavioral
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

entity DP_SingleBit_LogicCircuit_21364066 is
    port (
        A : in std_logic;
        B : in std_logic;
        S0 : in std_logic;
        S1 : in std_logic;
        G : out std_logic
        );
end DP_SingleBit_LogicCircuit_21364066;

architecture Behavioral of DP_SingleBit_LogicCircuit_21364066 is


--Internal Signals
    -- signal AANDB std_logic;
    -- signal NOTA std_logic;
    -- signal AORB std_logic;
    -- signal AXORB std_logic;

    begin
        -- AANDB <= A AND B;
        -- NOTA <= NOT A;
        -- AORB <= A OR B;
        -- AXORB < A XOR B;

        G <=   A AND B when S0 = '0' and S1 = '0' else   
               A OR B when S0 = '1' and S1 = '0'  else   
               A XOR B when S0 = '0' and S1 = '1' else   
               NOT A when S0 = '1' and S1 = '1'   else 
               '0';

    
end Behavioral;

----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 09.11.2022 15:37:33
-- Design Name: 
-- Module Name: BM_StateDecoder_21364066 - Behavioral
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

entity BM_StateDecoder_21364066 is
    Port ( A0 : in STD_LOGIC;
           A1 : in STD_LOGIC;
           IDLE : out STD_LOGIC;
           MUL0 : out STD_LOGIC;
           MUL1 : out STD_LOGIC);
end BM_StateDecoder_21364066;

architecture Behavioral of BM_StateDecoder_21364066 is

begin

IDLE <= (NOT(A1) AND NOT(A0)) after 5ns;
MUL0 <= (NOT(A1) AND A0) after 5ns;
MUL1 <= ( A1 AND NOT(A0)) after 5ns;

end Behavioral;

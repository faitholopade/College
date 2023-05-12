----------------------------------------------------------------------------------
-- Company: Trinity College
-- Engineer: Faith Olopade
-- 
-- Create Date:    12:42:30 01/11/2022 
-- Design Name: 
-- Module Name:    DP_SingleBit_B_Logic_21364066 - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
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
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity DP_SingleBit_B_Logic_21364066 is
    Port ( B : in  std_logic;
           S0, S1 : in  std_logic;
           Y : out  std_logic
    );
end DP_SingleBit_B_Logic_21364066;

architecture Behavioral of DP_SingleBit_B_Logic_21364066 is

begin
    Y <= (B and S0) or (not B and S1); --Yi = S0Bi + S1!Bi
end Behavioral;

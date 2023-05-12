----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 03.11.2022 16:45:00
-- Design Name: 
-- Module Name: DP_Mux3_1Bit_21364066 - Behavioral
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

entity DP_Mux_3_1Bit_21364066 is
    Port(
            Bi, slBi, srBi : in std_logic;
            S1, S2 : in std_logic;
            Gi : out std_logic
    );
end DP_Mux_3_1Bit_21364066;

architecture Behavioral of DP_Mux_3_1Bit_21364066 is
begin
		Gi <= Bi when S1 = '0' and S2 = '0' else
		      srBi when S1 = '1' and S2 = '0' else
		      slBi when S1 = '0' and S2 = '1' else
		      '0'; --Bi ;
end Behavioral;

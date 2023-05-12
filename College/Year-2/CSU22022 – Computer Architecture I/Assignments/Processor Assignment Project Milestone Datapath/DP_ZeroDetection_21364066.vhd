----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 29.10.2022 20:32:08
-- Design Name: 
-- Module Name: DP_ZeroDetection_21364066 - Behavioral
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

entity DP_ZeroDetection_21364066 is
	port (
		MuxF_to_F : in std_logic_vector(31 downto 0);
		Z : out std_logic
	);
end DP_ZeroDetection_21364066;
 
architecture Behavioral of DP_ZeroDetection_21364066 is
 
begin
	Z <= '1' when MuxF_to_F = "00000000000000000000000000000000" else '0';
 
end Behavioral;

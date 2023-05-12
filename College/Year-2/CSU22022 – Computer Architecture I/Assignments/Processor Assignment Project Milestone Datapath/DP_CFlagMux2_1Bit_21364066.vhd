----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 05.11.2022 10:42:00
-- Design Name: 
-- Module Name: DP_CFlagMux2_1Bit_21364066 - Behavioral
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

entity DP_CFlagMux2_1Bit_21364066 is
    port (
        C_ALU : in std_logic;
        C_Shift : in std_logic;
        FS4 : in std_logic;
        C : out std_logic
        );
end DP_CFlagMux2_1Bit_21364066;

architecture Behavioral of DP_CFlagMux2_1Bit_21364066 is
    begin
    process(FS4, C_ALU, C_Shift)
        begin 
        case FS4 is
            when '0' => C <= C_ALU;
            when '1' => C <= C_Shift;
            when others => C <='0';
        end case;
    end process;
end Behavioral;

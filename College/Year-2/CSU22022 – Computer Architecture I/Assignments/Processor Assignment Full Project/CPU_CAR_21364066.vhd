----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 22:35:00
-- Design Name: 
-- Module Name: CPU_CAR_21364066 - Behavioral
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
use IEEE.NUMERIC_STD.ALL;

use IEEE.std_logic_unsigned.all;


-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity CPU_CAR_21364066 is
    port (
                Address : in std_logic_vector(16 downto 0);
                Clock : in std_logic;
                LoadAdd : in std_logic;
                Reset : in std_logic;

                CMAdd : out std_logic_vector(16 downto 0)
        );
end CPU_CAR_21364066;

architecture Behavioral of CPU_CAR_21364066 is

    signal CAR_Out : std_logic_vector(16 downto 0);
    begin
    process(Reset, Clock)
    begin
        if rising_edge(Clock) then
            if Reset = '1' then
                CAR_Out <= "00000000000000110";
            elsif LoadAdd = '1' then
                CAR_Out <= Address;
            else
                CAR_Out <= CAR_Out + "00000000000000001";
            end if;           
        end if;
        CMAdd <= CAR_Out;
    end process;       
end Behavioral;

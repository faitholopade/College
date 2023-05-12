----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 22:35:00
-- Design Name: 
-- Module Name: CPU_SMux_21364066 - Behavioral
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

entity CPU_SMux_21364066 is
    port (  C_Flag : in std_logic;
            MS : in std_logic_vector(2 downto 0);
            N : in std_logic;
            Not_C_Flag : in std_logic;
            Not_Z_Flag : in std_logic;
            One : in std_logic;
            V_Flag : in std_logic;
            Z_Flag : in std_logic;
            Zero : in std_logic;

            CAR : out std_logic
        );
end CPU_SMux_21364066;

architecture Behavioral of CPU_SMux_21364066 is
    begin
        CAR <=  Zero       after  10ns  when  MS = "000" else
                One        after  10ns  when  MS = "001" else
                C_Flag     after  10ns  when  MS = "010" else
                V_Flag     after  10ns  when  MS = "011" else
                Z_Flag     after  10ns  when  MS = "100" else
                N          after  10ns  when  MS = "101" else
                Not_C_Flag after  10ns  when  MS = "110" else
                Not_Z_Flag after  10ns  when  MS = "111" else
                '0' after 10ns;
end Behavioral;

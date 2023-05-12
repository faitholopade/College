----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 22:35:00
-- Design Name: 
-- Module Name: CPU_StatusRegister_21364066 - Behavioral
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

entity CPU_StatusRegister_21364066 is
    port (  Clock : in std_logic;
            C_Flag : in std_logic;
            Reset_C : in std_logic;
            N_Flag : in std_logic;
            LoadFlags : in std_logic;
            Reset_N : in std_logic;
            V_Flag : in std_logic;
            Reset_V : in std_logic;
            Z_Flag : in std_logic;
            Reset_Z : in std_logic;

            StatusVector : out std_logic_vector(3 downto 0)
        );
end CPU_StatusRegister_21364066;

architecture Behavioral of CPU_StatusRegister_21364066 is
    component CPU_DFlipFlop_21364066 is
        port(
                Clock : in std_logic;
                D : in std_logic;
                Enable : in std_logic;
                Reset : in std_logic;

                Q: out std_logic
        );
    end component;

    begin
        CFlag : CPU_DFlipFlop_21364066 port map(
            Clock => Clock,
            D => C_Flag,
            Enable => LoadFlags,
            Reset => Reset_C,

            Q => StatusVector(0)
        );

        VFlag : CPU_DFlipFlop_21364066 port map(
            Clock => Clock,
            D => V_Flag,
            Enable => LoadFlags,
            Reset => Reset_V,

            Q => StatusVector(1)
        );

        NFlag : CPU_DFlipFlop_21364066 port map(
            Clock => Clock,
            D => N_Flag,
            Enable => LoadFlags,
            Reset => Reset_N,

            Q => StatusVector(2)
        );

        ZFlag : CPU_DFlipFlop_21364066 port map(
            Clock => Clock,
            D => Z_Flag,
            Enable => LoadFlags,
            Reset => Reset_Z,

            Q => StatusVector(3)
        );

        -- process(Clock)
        -- begin
        --     if rising_edge(Clock) and LoadFlags = '1' then
        --         StatusVector(1) <= V_Flag after 10ns;
        --         StatusVector(0) <= C_Flag after 10ns;
        --         StatusVector(2) <= N_Flag after 10ns;
        --         StatusVector(3) <= Z_Flag after 10ns;
        --         if Reset_V = '1' then
        --             StatusVector(1) <= '0' after 10ns;
        --         end if;
        --         if Reset_C = '1' then
        --             StatusVector(0) <= '0' after 10ns;
        --         end if;
        --         if Reset_N = '1' then
        --             StatusVector(2) <= '0' after 10ns;
        --         end if;
        --         if Reset_Z = '1' then
        --             StatusVector(3) <= '0' after 10ns;
        --         end if;
        --     end if;
        -- end process;

end Behavioral;

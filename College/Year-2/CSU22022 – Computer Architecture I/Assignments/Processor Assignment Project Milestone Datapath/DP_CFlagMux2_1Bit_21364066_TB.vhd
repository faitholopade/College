----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 05.11.2022 16:27:00
-- Design Name: 
-- Module Name: DP_CFlagMux2_1Bit_21364066_TB - Sim
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

entity DP_CFlagMux2_1Bit_21364066_TB is
--  Port ( ); We don't need ports
end DP_CFlagMux2_1Bit_21364066_TB;

architecture Sim of DP_CFlagMux2_1Bit_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component DP_CFlagMux2_1Bit_21364066
port ( 
        C_ALU : in std_logic;
        C_Shift : in std_logic;
        FS4 : in std_logic;
        C : out std_logic
      );
end component;

--Inputs

    signal C_ALU_TB : std_logic:= '0';
    signal C_Shift_TB : std_logic:= '0';
    signal FS4_TB : std_logic:= '0';
    
--Outputs

    signal C_TB : std_logic:= '0';
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: DP_CFlagMux2_1Bit_21364066 port map (
          C_ALU => C_ALU_TB,
          C_Shift => C_Shift_TB,
          FS4 => FS4_TB,
          C => C_TB
        );

        
   stim_proc: process

   begin
   
    -- --00
    -- C_ALU_TB <= '0';
    -- C_Shift_TB <= '0';

    -- wait for 5ns;
    -- FS4_TB <= '0';

    -- wait for 5ns;
    -- FS4_TB <= '1';
    
    --01
    C_ALU_TB <= '0';
    C_Shift_TB <= '1';
    
    wait for 5ns;
    FS4_TB <= '0';

    wait for 5ns;
    FS4_TB <= '1';
    
    -- --10
    -- C_ALU_TB <= '1';
    -- C_Shift_TB <= '0';
    
    -- wait for 5ns;
    -- FS4_TB <= '0';

    -- wait for 5ns;
    -- FS4_TB <= '1';
    
    -- --11
    -- C_ALU_TB <= '1';
    -- C_Shift_TB <= '1';
    
    -- wait for 5ns;
    -- FS4_TB <= '0';

    -- wait for 5ns;
    -- FS4_TB <= '1';
    
   end process;
end Sim;
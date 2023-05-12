----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 23:12:00
-- Design Name: 
-- Module Name: CPU_IR_21364066_TB - Sim
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

entity CPU_IR_21364066_TB is
--  Port ( ); We don't need ports
end CPU_IR_21364066_TB;

architecture Sim of CPU_IR_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component CPU_IR_21364066
port (  Clock: in std_logic;
        IL : in std_logic;
        Instruction : in std_logic_vector(31 downto 0); 
        DR : out std_logic_vector(4 downto 0);
        Opcode : out std_logic_vector(16 downto 0);
        SA : out std_logic_vector(4 downto 0);
        SB : out std_logic_vector(4 downto 0)
      );
end component;

--Inputs

    signal Clock_TB : std_logic := '0';
    signal IL_TB : std_logic := '0';
    signal Instruction_TB : std_logic_vector(31 downto 0) := (others => '0');
    
--Outputs

    signal  DR_TB : std_logic_vector(4 downto 0) := (others => '0');
    signal Opcode_TB : std_logic_vector(16 downto 0) := (others => '0');
    signal  SA_TB : std_logic_vector(4 downto 0) := (others => '0');
    signal  SB_TB : std_logic_vector(4 downto 0) := (others => '0');

    constant period : time := 100ns;
    
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: CPU_IR_21364066 port map (
        Clock => Clock_TB,
        IL => IL_TB,
        Instruction => Instruction_TB,
        DR => DR_TB,
        Opcode => Opcode_TB,
        SA => SA_TB,
        SB => SB_TB
    );
    
    Clock_TB <= not Clock_TB after period/2;

   stim_proc: process
   begin

    --OUTPUT ALL 0'S'
    wait until Clock_TB'event and Clock_TB = '1';
    IL_TB <= '0';
    Instruction_TB <= "00000000001000000001100011000000";
    -- Opcode          |DR   |SA  | SB  
    -- 00000000001000000001100011000000
    wait for 100ns;

    --OUTPUT INSTRUCTION--
    wait until Clock_TB'event and Clock_TB = '1';
    IL_TB <= '1';
    Instruction_TB <= "00000000001000000001100011000000";
    -- Opcode          |DR   |SA  | SB  
    -- 00000000001000000001100011000000
    wait for 100ns;

   end process;
end Sim;
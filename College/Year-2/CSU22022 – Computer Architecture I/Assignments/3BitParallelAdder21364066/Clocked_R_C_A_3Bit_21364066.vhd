----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 07.10.2022 09:44:05
-- Design Name: 
-- Module Name: Clocked_R_C_A_3Bit_21364066 - Behavioral
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

entity Clocked_R_C_A_3Bit_21364066 is
    PORT(Source_A : in STD_LOGIC_VECTOR (2 downto 0);
         EN_Source_A : in STD_LOGIC;
         Source_B : in STD_LOGIC_VECTOR (2 downto 0);
         EN_Source_B : in STD_LOGIC;
         C_IN : in STD_LOGIC;
         CLOCK : in STD_LOGIC;
         EN_SUM : in STD_LOGIC;
         SUM : out STD_LOGIC_VECTOR (2 downto 0);
         C : out STD_LOGIC;
         V : out STD_LOGIC);   
end Clocked_R_C_A_3Bit_21364066;

architecture Behavioral of Clocked_R_C_A_3Bit_21364066 is

component Register3Bit_21364066
port ( 
    D : in STD_LOGIC_VECTOR (2 downto 0);
    CLK : in STD_LOGIC;
    Load : in STD_LOGIC;
    Q : out STD_LOGIC_VECTOR (2 downto 0)
      );
end component;

component FlipFlop_21364066
port ( 
    D : in STD_LOGIC;
    CLK : in STD_LOGIC;
    Q : out STD_LOGIC
      );
end component;

component RippleCarryAdder3Bit_21364066

Port ( A : in STD_LOGIC_VECTOR (2 downto 0);
       B : in STD_LOGIC_VECTOR (2 downto 0);
       C_IN : in STD_LOGIC;
       SUM : out STD_LOGIC_VECTOR (2 downto 0);
       C_OUT : out STD_LOGIC;
       V : out STD_LOGIC);
end component;

    signal REG_A_to_Adder : STD_LOGIC_VECTOR (2 downto 0);
    signal REG_B_to_Adder : STD_LOGIC_VECTOR (2 downto 0);
    signal Adder_to_REG_SUM : STD_LOGIC_VECTOR (2 downto 0);
    
    signal AdderC_to_CFlag, AdderV_to_VFlag, FlipFlop_C_IN_to_Adder : STD_LOGIC;

begin

   RegisterA: Register3Bit_21364066 port map (
          D => Source_A,
          CLK => CLOCK,
          Load => EN_Source_A,
          Q => REG_A_to_Adder
        );

   RegisterB: Register3Bit_21364066 port map (
          D => Source_B,
          CLK => CLOCK,
          Load => EN_Source_B,
          Q => REG_B_to_Adder
        );
        
    FlipFlop_C_IN: FlipFlop_21364066 port map (
          D => C_IN,
          CLK => CLOCK,
          Q => FlipFlop_C_IN_to_Adder
        );
      
    Adder: RippleCarryAdder3Bit_21364066 PORT MAP (
       A => REG_A_to_Adder,
       B => REG_B_to_Adder,
       C_IN => FlipFlop_C_IN_to_Adder,
       SUM => Adder_to_REG_SUM,
       C_OUT => AdderC_to_CFlag,
       V => AdderV_to_VFlag
       );    
        
   RegisterSUM: Register3Bit_21364066 port map (
          D => Adder_to_REG_SUM,
          CLK => CLOCK,
          Load => EN_SUM,
          Q => SUM
        );
        
   C_Flag: FlipFlop_21364066 port map (
          D => AdderC_to_CFlag,
          CLK => CLOCK,
          Q => C
        );

   V_Flag: FlipFlop_21364066 port map (
          D => AdderV_to_VFlag,
          CLK => CLOCK,
          Q => V
        );
        
end Behavioral;

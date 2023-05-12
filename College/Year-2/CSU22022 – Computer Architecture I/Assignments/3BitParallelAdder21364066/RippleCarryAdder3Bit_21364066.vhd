----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 06.10.2022 23:37:03
-- Design Name: 
-- Module Name: RippleCarryAdder3Bit_21364066 - Behavioral
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

entity RippleCarryAdder3Bit_21364066 is

    Port ( A : in STD_LOGIC_VECTOR (2 downto 0);
           B : in STD_LOGIC_VECTOR (2 downto 0);
           C_IN : in STD_LOGIC;
           SUM : out STD_LOGIC_VECTOR (2 downto 0);
           C_OUT : out STD_LOGIC;
           V : out STD_LOGIC);
           
end RippleCarryAdder3Bit_21364066;

architecture Behavioral of RippleCarryAdder3Bit_21364066 is

COMPONENT FullAdder_21364066    
    PORT(
         A : in STD_LOGIC;
         B : in STD_LOGIC;
         C_IN : in STD_LOGIC;
         SUM : out STD_LOGIC;
         C_OUT : out STD_LOGIC
        );
    END COMPONENT;
    
Signal C0_to_C1, C1_to_C2, C2_to_COut : STD_LOGIC;    

begin

   -- Instantiate Full Adder Bit 0
   BIT0: FullAdder_21364066 PORT MAP (
          A => A(0),
          B => B(0),
          C_IN => C_IN,
          SUM => SUM(0),
          C_OUT => C0_to_C1
        );

   -- Instantiate Full Adder Bit 1
   BIT1: FullAdder_21364066 PORT MAP (
          A => A(1),
          B => B(1),
          C_IN => C0_to_C1,
          SUM => SUM(1),
          C_OUT => C1_to_C2
        );
        
   -- Instantiate Full Adder Bit 2
   BIT2: FullAdder_21364066 PORT MAP (
          A => A(2),
          B => B(2),
          C_IN => C1_to_C2,
          SUM => SUM(2),
          C_OUT => C2_to_COut
        );
        
    -- Carry and Overflow 
    
    C_OUT <= C2_to_COut;
    
    V <= C2_to_COut XOR C1_to_C2 after 3ns;  

end Behavioral;

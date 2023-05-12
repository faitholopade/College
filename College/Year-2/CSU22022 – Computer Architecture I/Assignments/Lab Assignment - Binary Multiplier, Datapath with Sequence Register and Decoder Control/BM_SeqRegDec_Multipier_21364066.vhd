----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 10.11.2022 17:17:20
-- Design Name: 
-- Module Name: BM_SeqRegDec_Multipier_21364066- Behavioral
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

entity BM_SeqRegDec_Multipier_21364066 is
    Port ( Clock : in STD_LOGIC;
           FlipFlopReset : in STD_LOGIC;
           RegB : in STD_LOGIC_VECTOR (4 downto 0);
           RegQ : in STD_LOGIC_VECTOR (4 downto 0);
           LoadB : in STD_LOGIC;
           LoadQ : in STD_LOGIC;
           G : in STD_LOGIC;
           Product : out STD_LOGIC_VECTOR (9 downto 0));
end BM_SeqRegDec_Multipier_21364066;

architecture Behavioral of BM_SeqRegDec_Multipier_21364066 is

component BM_Counter_21364066
port ( 
      Clock : in STD_LOGIC;
      Reset : in STD_LOGIC;
      Decrement : in STD_LOGIC;
      Counter_Out : Out STD_LOGIC_VECTOR (2 downto 0)
      );
end component;

component BM_Control_SeqRegDec_21364066
port ( 
      Clock : in STD_LOGIC;
      FlipFlopReset : in STD_LOGIC;
      Z : in STD_LOGIC;
      G : in STD_LOGIC;
      Q0 : in STD_LOGIC;
      Init : out STD_LOGIC;
      Shift : out STD_LOGIC;
      Add : out STD_LOGIC
      );
end component;

component BM_Datapath_21364066
Port ( 
      RegB : in STD_LOGIC_VECTOR (4 downto 0);
      RegQ : in STD_LOGIC_VECTOR (4 downto 0);
      LoadB : in STD_LOGIC;
      LoadQ : in STD_LOGIC;
      Clr : in STD_LOGIC;
      Clock : in STD_LOGIC;
      Shift : in STD_LOGIC;
      Add : in STD_LOGIC;
      Product : out STD_LOGIC_VECTOR (9 downto 0);
      Q0 : out STD_LOGIC
      );
end component;

    signal Count : STD_LOGIC_VECTOR (2 downto 0);
    signal Q0, Shift, Add, Init, Z : STD_LOGIC;

begin

CounterP: BM_Counter_21364066 port map (     
      Clock => Clock,
      Reset => Init,
      Decrement => Shift,
      Counter_Out => Count      
       );

Z <= NOT(Count(2)) AND NOT(Count(1)) AND NOT(Count(0)) after 2ns;

ControlUnit: BM_Control_SeqRegDec_21364066 port map (     
      Clock => Clock,
      FlipFlopReset => FlipFlopReset,
      Z => Z,
      G => G,
      Q0 => Q0,
      Init => Init,
      Shift => Shift,
      Add => Add    
       );
       
Datapath : BM_Datapath_21364066 port map (     
      RegB => RegB,
      RegQ => RegQ,
      LoadB => LoadB,
      LoadQ => LoadQ,
      Clr => Init,
      Clock => Clock,
      Shift => Shift,
      Add => Add,
      Product => Product,
      Q0 => Q0 
      );       

end Behavioral;

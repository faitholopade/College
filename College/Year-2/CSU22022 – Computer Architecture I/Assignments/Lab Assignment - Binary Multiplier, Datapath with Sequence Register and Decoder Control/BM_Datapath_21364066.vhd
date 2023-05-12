----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 30.10.2022 23:15:58
-- Design Name: 
-- Module Name: BM_Datapath_21364066 - Behavioral
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

entity BM_Datapath_21364066 is
    Port ( RegB : in STD_LOGIC_VECTOR (4 downto 0);
           RegQ : in STD_LOGIC_VECTOR (4 downto 0);
           LoadB : in STD_LOGIC;
           LoadQ : in STD_LOGIC;
           Clr : in STD_LOGIC;
           Clock : in STD_LOGIC;
           Shift : in STD_LOGIC;
           Add : in STD_LOGIC;
           Product : out STD_LOGIC_VECTOR (9 downto 0);
           Q0 : out STD_LOGIC);
end BM_Datapath_21364066;

architecture Behavioral of BM_Datapath_21364066 is

component BM_Register5Bit_21364066
port ( 
      D : in STD_LOGIC_VECTOR (4 downto 0);
      Clr  : in STD_LOGIC;
      Load : in STD_LOGIC;
      Clock : in STD_LOGIC;
      Q : out STD_LOGIC_VECTOR (4 downto 0) 
      );
end component;

component BM_Shift_C_RegA_RegQ_21364066
port ( 
      C : in STD_LOGIC;
      A_In : in STD_LOGIC_VECTOR (4 downto 0);
      Q_In : in STD_LOGIC_VECTOR (4 downto 0);
      Clock : in STD_LOGIC;
      LoadCRegA : in STD_LOGIC;
      LoadRegQ : in STD_LOGIC;
      ClrCRegA : in STD_LOGIC;
      Shift : in STD_LOGIC;
      A_Out : out STD_LOGIC_VECTOR (4 downto 0);
      Product : out STD_LOGIC_VECTOR (9 downto 0)
      );
end component;

component BM_RippleCarryAdder5Bit_21364066
Port ( 
      A : in STD_LOGIC_VECTOR (4 downto 0);
      B : in STD_LOGIC_VECTOR (4 downto 0);
      C_IN : in STD_LOGIC;
      SUM : out STD_LOGIC_VECTOR (4 downto 0);
      C_OUT : out STD_LOGIC
      );
end component;

signal RegB_to_AdderB, ShiftRegA_To_AdderA, AdderSUM_To_ShiftRegA : std_logic_vector(4 downto 0);
signal Pro : std_logic_vector(9 downto 0);
signal AdderC_To_ShiftC : std_logic;
    
begin

RegisterB: BM_Register5Bit_21364066 port map (
          D => RegB,
          Clr => '0',
          Load => LoadB,
          Clock => Clock,
          Q => RegB_to_AdderB           
          );
          
Adder: BM_RippleCarryAdder5Bit_21364066 PORT MAP (
          A => ShiftRegA_To_AdderA,
          B => RegB_to_AdderB,
          C_IN => '0',
          SUM => AdderSUM_To_ShiftRegA,
          C_OUT => AdderC_To_ShiftC
          );

ShiftCRegARegQ: BM_Shift_C_RegA_RegQ_21364066 port map (     
          C => AdderC_To_ShiftC,
          A_In => AdderSUM_To_ShiftRegA,
          Q_In => RegQ,
          Clock => Clock,
          LoadCRegA => Add,
          LoadRegQ => LoadQ,
          ClrCRegA => Clr,
          Shift => Shift,
          A_Out => ShiftRegA_To_AdderA,
          Product => Pro     
          );
          
 Product <= Pro;
 Q0 <= Pro(0);       

end Behavioral;

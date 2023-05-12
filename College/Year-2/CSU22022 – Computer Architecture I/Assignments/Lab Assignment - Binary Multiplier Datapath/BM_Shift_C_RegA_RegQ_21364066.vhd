----------------------------------------------------------------------------------
-- Company: Trinity College Dublin 
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 30.10.2022 15:05:30
-- Design Name: 
-- Module Name: BM_Shift_C_RegA_RegQ_21364066 - Behavioral
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

entity BM_Shift_C_RegA_RegQ_21364066 is
    Port ( C : in STD_LOGIC;
           A_In : in STD_LOGIC_VECTOR (4 downto 0);
           Q_In : in STD_LOGIC_VECTOR (4 downto 0);
           Clock : in STD_LOGIC;
           LoadCRegA : in STD_LOGIC;
           LoadRegQ : in STD_LOGIC;
           ClrCRegARegQ : in STD_LOGIC;
           Shift : in STD_LOGIC;
           A_Out : out STD_LOGIC_VECTOR (4 downto 0);
           Product : out STD_LOGIC_VECTOR (9 downto 0));
end BM_Shift_C_RegA_RegQ_21364066;

architecture Behavioral of BM_Shift_C_RegA_RegQ_21364066 is

component BM_ShiftFlipFlop_21364066
port ( 
      Clock : in STD_LOGIC;
      Load : in STD_LOGIC;
      Clr : in STD_LOGIC;
      Shift : in STD_LOGIC;
      P : in STD_LOGIC;
      Q : inout STD_LOGIC
      );
end component;

component BM_ShiftRegister_5Bit_21364066
port ( 
      D : in STD_LOGIC;
      Clock : in STD_LOGIC;
      Load : in STD_LOGIC;
      Clr : in STD_LOGIC;
      Shift : in STD_LOGIC;
      P : in STD_LOGIC_VECTOR (4 downto 0);
      Q : inout STD_LOGIC_VECTOR (4 downto 0)
      );
end component;

    signal ShiftRegA_Out, ShiftRegQ_Out : std_logic_vector(4 downto 0);
    signal C_To_RegA : std_logic;

begin

ShiftFlipFlopC: BM_ShiftFlipFlop_21364066 port map (     
      Clock => Clock,
      Load => LoadCRegA,
      Clr => ClrCRegARegQ,
      Shift => Shift,
      P => C,
      Q => C_To_RegA       
      );

ShiftRegA: BM_ShiftRegister_5Bit_21364066 port map (     
      D => C_To_RegA,
      Clock => Clock,
      Load => LoadCRegA,
      Clr => ClrCRegARegQ,
      Shift => Shift,
      P => A_In,
      Q => ShiftRegA_Out      
      );
       
ShiftRegQ: BM_ShiftRegister_5Bit_21364066 port map (     
      D => ShiftRegA_Out(0),
      Clock => Clock,
      Load => LoadRegQ,
      Clr => ClrCRegARegQ,
      Shift => Shift,
      P => Q_In,
      Q => ShiftRegQ_Out      
      );
      
A_Out <= ShiftRegA_Out;
Product <= ShiftRegA_Out & ShiftRegQ_Out;
      
end Behavioral;

----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 09.11.2022 18:55:32
-- Design Name: 
-- Module Name: BM_Control_SeqRegDec_21364066 - Behavioral
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

entity BM_Control_SeqRegDec_21364066 is
    Port ( Clock : in STD_LOGIC;
           FlipFlopReset : in STD_LOGIC;
           Z : in STD_LOGIC;
           G : in STD_LOGIC;
           Q0 : in STD_LOGIC;
           Init : out STD_LOGIC;
           Shift : out STD_LOGIC;
           Add : out STD_LOGIC);
end BM_Control_SeqRegDec_21364066;

architecture Behavioral of BM_Control_SeqRegDec_21364066 is

component BM_DFlipFlop_21364066
port ( 
      D : in STD_LOGIC;
      Reset : in STD_LOGIC;
      Clock : in STD_LOGIC;
      Q : out STD_LOGIC
      );
end component;

component BM_StateDecoder_21364066
     Port ( 
           A0 : in STD_LOGIC;
           A1 : in STD_LOGIC;
           IDLE : out STD_LOGIC;
           MUL0 : out STD_LOGIC;
           MUL1 : out STD_LOGIC           
           );                      
end component;

    signal M0_to_StateDecoder, M1_to_StateDecoder : STD_LOGIC;
    signal IDLE_State, MUL0_State, MUL1_State : STD_LOGIC;
    signal GAnd_to_Or, ZAnd_to_Or, NOTZero, Or_to_M0 : STD_LOGIC;

begin

GAnd_to_Or <= IDLE_State AND G;
NOTZero <= NOT(Z);
ZAnd_to_Or <= MUL1_State AND NOTZero;
Or_to_M0 <= GAnd_to_Or OR ZAnd_to_Or;

M0: BM_DFlipFlop_21364066 port map (     
       D => Or_to_M0,
       Reset => FlipFlopReset,
       Clock => Clock,
       Q => M0_to_StateDecoder      
       );

M1: BM_DFlipFlop_21364066 port map (     
       D => MUL0_State,
       Reset => FlipFlopReset,
       Clock => Clock,
       Q => M1_to_StateDecoder      
       );

Decoder: BM_StateDecoder_21364066 PORT MAP (
       A0 => M0_to_StateDecoder,
       A1 => M1_to_StateDecoder,
       IDLE => IDLE_State,
       MUL0 => MUL0_State,
       MUL1 => MUL1_State  
       );

Init <= GAnd_to_Or;
Shift <= MUL1_State;
Add <= MUL0_State AND Q0;

end Behavioral;

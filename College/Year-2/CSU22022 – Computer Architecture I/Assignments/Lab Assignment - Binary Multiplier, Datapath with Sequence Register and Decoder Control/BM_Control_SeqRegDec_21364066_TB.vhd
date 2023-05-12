----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Prof. Michael Manzke
-- 
-- Create Date: 09.11.2022 20:15:12
-- Design Name: 
-- Module Name: BM_Control_SeqRegDec_21364066_TB - Simulation
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

entity BM_Control_SeqRegDec_21364066_TB is
--  Port ( ); we don't need ports
end BM_Control_SeqRegDec_21364066_TB;

architecture Simulation of BM_Control_SeqRegDec_21364066_TB is

-- Component Declaration for the Unit Under Test (UUT)
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

--Inputs
    
    signal Clock_TB : STD_LOGIC := '0';
    signal FlipFlopReset_TB : STD_LOGIC := '0';
    signal Z_TB : STD_LOGIC := '0';
    signal G_TB : STD_LOGIC := '0';
    signal Q0_TB : STD_LOGIC := '0';    
    
    
--Outputs

    signal Init_TB : STD_LOGIC := '0';
    signal Shift_TB : STD_LOGIC := '0';
    signal Add_TB : STD_LOGIC := '0';
    
    constant PERIOD : time := 100ns;

begin

	-- Instantiate the Unit Under Test (UUT)
	
   uut: BM_Control_SeqRegDec_21364066 port map (     
      Clock => Clock_TB,
      FlipFlopReset => FlipFlopReset_TB,
      Z => Z_TB,
      G => G_TB,
      Q0 => Q0_TB,
      Init => Init_TB,
      Shift => Shift_TB,
      Add => Add_TB    
       );
        
   Clock_TB <= not Clock_TB after PERIOD/2;
   
     stim_proc: process

   begin
         -- Test vector 00 - Reset Flip Flops
   
      wait until Clock_TB'event and Clock_TB='1';
        
         FlipFlopReset_TB <= '1' after PERIOD/4;
         Z_TB <= '0' after PERIOD/4;
         G_TB <= '0' after PERIOD/4;
         Q0_TB <= '0' after PERIOD/4;
         
      -- Test vector 01 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         FlipFlopReset_TB <= '0' after PERIOD/4;
         Z_TB <= '0' after PERIOD/4;
         G_TB <= '0' after PERIOD/4;
         Q0_TB <= '0' after PERIOD/4;
         
      -- Test vector 02 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         FlipFlopReset_TB <= '0' after PERIOD/4;        
         Z_TB <= '0' after PERIOD/4;
         G_TB <= '0' after PERIOD/4;
         Q0_TB <= '0' after PERIOD/4;
         
      -- Test vector 03 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         FlipFlopReset_TB <= '0' after PERIOD/4;
         Z_TB <= '0' after PERIOD/4;
         G_TB <= '1' after PERIOD/4;
         Q0_TB <= '0' after PERIOD/4;
         
      -- Test vector 04 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         FlipFlopReset_TB <= '0' after PERIOD/4;
         Z_TB <= '0' after PERIOD/4;
         G_TB <= '0' after PERIOD/4;
         Q0_TB <= '0' after PERIOD/4;         
                     
       -- Test vector 05 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         FlipFlopReset_TB <= '0' after PERIOD/4;
         Z_TB <= '0' after PERIOD/4;
         G_TB <= '0' after PERIOD/4;
         Q0_TB <= '0' after PERIOD/4;  
         
       -- Test vector 06 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         FlipFlopReset_TB <= '0' after PERIOD/4;
         Z_TB <= '0' after PERIOD/4;
         G_TB <= '0' after PERIOD/4;
         Q0_TB <= '0' after PERIOD/4; 
         
        -- Test vector 07 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         FlipFlopReset_TB <= '0' after PERIOD/4;
         Z_TB <= '0' after PERIOD/4;
         G_TB <= '0' after PERIOD/4;
         Q0_TB <= '1' after PERIOD/4; 
         
         -- Test vector 08 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         FlipFlopReset_TB <= '0' after PERIOD/4;
         Z_TB <= '0' after PERIOD/4;
         G_TB <= '0' after PERIOD/4;
         Q0_TB <= '0' after PERIOD/4; 
         
        -- Test vector 09 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         FlipFlopReset_TB <= '0' after PERIOD/4;
         Z_TB <= '0' after PERIOD/4;
         G_TB <= '0' after PERIOD/4;
         Q0_TB <= '1' after PERIOD/4;        
                 
         -- Test vector 10 
   
      wait until Clock_TB'event and Clock_TB='1';
        
         FlipFlopReset_TB <= '0' after PERIOD/4;
         Z_TB <= '1' after PERIOD/4;
         G_TB <= '0' after PERIOD/4;
         Q0_TB <= '0' after PERIOD/4;                 
                                                                        
   end process;

end Simulation;

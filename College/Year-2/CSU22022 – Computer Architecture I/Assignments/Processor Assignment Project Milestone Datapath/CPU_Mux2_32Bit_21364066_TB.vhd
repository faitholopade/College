library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 28.10.2022 18:12:00
-- Design Name: 
-- Module Name: CPU_Mux2_32Bit_21364066_TB - Sim
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
entity CPU_Mux2_32Bit_21364066_TB is
    --  Port ( ); We don't need ports
    end CPU_Mux2_32Bit_21364066_TB;
    
    architecture Sim of CPU_Mux2_32Bit_21364066_TB is
    -- Component Declaration for the Unit Under Test (UUT)
    
    component CPU_Mux2_32Bit_21364066
    port (  In0 : in  std_logic_vector(31 downto 0);
            In1 : in  std_logic_vector(31 downto 0);
            Sel : in  std_logic;
            Z : out  std_logic_vector(31 downto 0)
      );
    end component;
    
    --Inputs
    
        signal In0_TB : std_logic_vector(31 downto 0):= (others => '0');
        signal In1_TB : std_logic_vector(31 downto 0):= (others => '0');
        signal Sel_TB : std_logic := '0' ;
        
    --Outputs
    
        signal Z_TB : std_logic_vector(31 downto 0):= (others => '0');
        
    begin
        -- Instantiate the Unit Under Test (UUT)
        
       uut: CPU_Mux2_32Bit_21364066 port map (
              In0 => In0_TB,
              In1 => In1_TB,
              Sel => Sel_TB,
              Z => Z_TB
            );
    
            
       stim_proc: process
    
       begin
        In0_TB <= "00000001010001011111110101100010"; --ID
        In1_TB <= "00000001010001011111110101100011"; --ID + 1
    
        wait for 5ns;
        Sel_TB <= '0';
    
        wait for 5ns;
        Sel_TB <= '1';
       end process;
    end Sim;
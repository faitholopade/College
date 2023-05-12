----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 21.10.2022 00:26:54
-- Design Name: 
-- Module Name: RF_RegisterFile_32_15_21364066 - Behavioral
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

entity RF_RegisterFile_32_15_21364066 is
   Port (   SA : in std_logic_vector(4 downto 0);
            SB : in std_logic_vector(4 downto 0);
            DR : in std_logic_vector(4 downto 0);
            Clock : in std_logic;
            RW : in std_logic;
            D : in std_logic_vector(31 downto 0);
            TB : in std_logic_vector(3 downto 0);
            TD : in std_logic_vector(3 downto 0);
            TA : in std_logic_vector(3 downto 0);
            A : out std_logic_vector(31 downto 0);
            B : out std_logic_vector(31 downto 0)
        );
end RF_RegisterFile_32_15_21364066;

architecture Behavioral of RF_RegisterFile_32_15_21364066 is

    --RF_DestReg_Decoder_21364066
    component RF_DestReg_Decoder_21364066
    port(
            A : in std_logic_vector(4 downto 0);
            Q00 : out std_logic;
            Q01 : out std_logic;
            Q02 : out std_logic;
            Q03 : out std_logic;
            Q04 : out std_logic;
            Q05 : out std_logic;
            Q06 : out std_logic;
            Q07 : out std_logic;
            Q08 : out std_logic;
            Q09 : out std_logic;
            Q10 : out std_logic;
            Q11 : out std_logic;
            Q12 : out std_logic;
            Q13 : out std_logic;
            Q14 : out std_logic;
            Q15 : out std_logic;
            Q16 : out std_logic;
            Q17 : out std_logic;
            Q18 : out std_logic;
            Q19 : out std_logic;
            Q20 : out std_logic;
            Q21 : out std_logic;
            Q22 : out std_logic;
            Q23 : out std_logic;
            Q24 : out std_logic;
            Q25 : out std_logic;
            Q26 : out std_logic;
            Q27 : out std_logic;
            Q28 : out std_logic;
            Q29 : out std_logic;
            Q30 : out std_logic;
            Q31 : out std_logic
        );
    end component;

    --RF_TempDestReg_Decoder_21364066
    component RF_TempDestReg_Decoder_21364066
    port(
            A: in std_logic_vector (3 downto 0); 

            Q00: out std_logic;
            Q01: out std_logic;
            Q02: out std_logic;
            Q03: out std_logic;
            Q04: out std_logic;
            Q05: out std_logic;
            Q06: out std_logic;
            Q07: out std_logic;
            Q08: out std_logic;
            Q09: out std_logic;
            Q10: out std_logic;
            Q11: out std_logic;
            Q12: out std_logic;
            Q13: out std_logic;
            Q14: out std_logic;
            Q15: out std_logic
        );
    end component;

    --RF_Register32Bit_21364066
    component RF_Register32Bit_21364066
    port(
           D : in STD_LOGIC_VECTOR (31 downto 0);
           Load : in STD_LOGIC;
           CLK : in STD_LOGIC;
           Q : out STD_LOGIC_VECTOR (31 downto 0)
        );
    end component;

    --RF_Mux32_32Bit_21364066
    component RF_Mux32_32Bit_21364066
    port(
            In00 : in std_logic_vector(31 downto 0);
            In01 : in std_logic_vector(31 downto 0);
            In02 : in std_logic_vector(31 downto 0);
            In03 : in std_logic_vector(31 downto 0);
            In04 : in std_logic_vector(31 downto 0);
            In05 : in std_logic_vector(31 downto 0);
            In06 : in std_logic_vector(31 downto 0);
            In07 : in std_logic_vector(31 downto 0);
            In08 : in std_logic_vector(31 downto 0);
            In09 : in std_logic_vector(31 downto 0);
            In10 : in std_logic_vector(31 downto 0);
            In11 : in std_logic_vector(31 downto 0);
            In12 : in std_logic_vector(31 downto 0);
            In13 : in std_logic_vector(31 downto 0);
            In14 : in std_logic_vector(31 downto 0);
            In15 : in std_logic_vector(31 downto 0);
            In16 : in std_logic_vector(31 downto 0);
            In17 : in std_logic_vector(31 downto 0);
            In18 : in std_logic_vector(31 downto 0);
            In19 : in std_logic_vector(31 downto 0);
            In20 : in std_logic_vector(31 downto 0);
            In21 : in std_logic_vector(31 downto 0);
            In22 : in std_logic_vector(31 downto 0);
            In23 : in std_logic_vector(31 downto 0);
            In24 : in std_logic_vector(31 downto 0);
            In25 : in std_logic_vector(31 downto 0);
            In26 : in std_logic_vector(31 downto 0);
            In27 : in std_logic_vector(31 downto 0);
            In28 : in std_logic_vector(31 downto 0);
            In29 : in std_logic_vector(31 downto 0);
            In30 : in std_logic_vector(31 downto 0);
            In31 : in std_logic_vector(31 downto 0);

            A : in std_logic_vector(4 downto 0);

            Z : out std_logic_vector(31 downto 0)
        );
    end component;

    --RF_Mux16_32Bit_21364066
    component RF_Mux16_32Bit_21364066
    port(
            In00 : in std_logic_vector(31 downto 0);
            In01 : in std_logic_vector(31 downto 0);
            In02 : in std_logic_vector(31 downto 0);
            In03 : in std_logic_vector(31 downto 0);
            In04 : in std_logic_vector(31 downto 0);
            In05 : in std_logic_vector(31 downto 0);
            In06 : in std_logic_vector(31 downto 0);
            In07 : in std_logic_vector(31 downto 0);
            In08 : in std_logic_vector(31 downto 0);
            In09 : in std_logic_vector(31 downto 0);
            In10 : in std_logic_vector(31 downto 0);
            In11 : in std_logic_vector(31 downto 0);
            In12 : in std_logic_vector(31 downto 0);
            In13 : in std_logic_vector(31 downto 0);
            In14 : in std_logic_vector(31 downto 0);
            In15 : in std_logic_vector(31 downto 0);

            A : in std_logic_vector(3 downto 0);

            Z : out std_logic_vector(31 downto 0)
        );
    end component;

    -- register 00 to 31 load signals
    signal Load_Reg00 : std_logic;
    signal Load_Reg01 : std_logic;
    signal Load_Reg02 : std_logic;
    signal Load_Reg03 : std_logic;
    signal Load_Reg04 : std_logic;
    signal Load_Reg05 : std_logic;
    signal Load_Reg06 : std_logic;
    signal Load_Reg07 : std_logic;
    signal Load_Reg08 : std_logic;
    signal Load_Reg09 : std_logic;
    signal Load_Reg10 : std_logic;
    signal Load_Reg11 : std_logic;
    signal Load_Reg12 : std_logic;
    signal Load_Reg13 : std_logic;
    signal Load_Reg14 : std_logic;
    signal Load_Reg15 : std_logic;
    signal Load_Reg16 : std_logic;
    signal Load_Reg17 : std_logic;
    signal Load_Reg18 : std_logic;
    signal Load_Reg19 : std_logic;
    signal Load_Reg20 : std_logic;
    signal Load_Reg21 : std_logic;
    signal Load_Reg22 : std_logic;
    signal Load_Reg23 : std_logic;
    signal Load_Reg24 : std_logic;
    signal Load_Reg25 : std_logic;
    signal Load_Reg26 : std_logic;
    signal Load_Reg27 : std_logic;
    signal Load_Reg28 : std_logic;
    signal Load_Reg29 : std_logic;
    signal Load_Reg30 : std_logic;
    signal Load_Reg31 : std_logic;

    --temp register 00 to 15 load signal
    signal Load_TempReg00 : std_logic;
    signal Load_TempReg01 : std_logic;
    signal Load_TempReg02 : std_logic;
    signal Load_TempReg03 : std_logic;
    signal Load_TempReg04 : std_logic;
    signal Load_TempReg05 : std_logic;
    signal Load_TempReg06 : std_logic;
    signal Load_TempReg07 : std_logic;
    signal Load_TempReg08 : std_logic;
    signal Load_TempReg09 : std_logic;
    signal Load_TempReg10 : std_logic;
    signal Load_TempReg11 : std_logic;
    signal Load_TempReg12 : std_logic;
    signal Load_TempReg13 : std_logic;
    signal Load_TempReg14 : std_logic;
    signal Load_TempReg15 : std_logic;

    -- register 00 to 31 output
    signal Reg00_out : std_logic_vector(31 downto 0); 
    signal Reg01_out : std_logic_vector(31 downto 0); 
    signal Reg02_out : std_logic_vector(31 downto 0); 
    signal Reg03_out : std_logic_vector(31 downto 0); 
    signal Reg04_out : std_logic_vector(31 downto 0); 
    signal Reg05_out : std_logic_vector(31 downto 0); 
    signal Reg06_out : std_logic_vector(31 downto 0); 
    signal Reg07_out : std_logic_vector(31 downto 0); 
    signal Reg08_out : std_logic_vector(31 downto 0); 
    signal Reg09_out : std_logic_vector(31 downto 0); 
    signal Reg10_out : std_logic_vector(31 downto 0); 
    signal Reg11_out : std_logic_vector(31 downto 0); 
    signal Reg12_out : std_logic_vector(31 downto 0); 
    signal Reg13_out : std_logic_vector(31 downto 0); 
    signal Reg14_out : std_logic_vector(31 downto 0); 
    signal Reg15_out : std_logic_vector(31 downto 0); 
    signal Reg16_out : std_logic_vector(31 downto 0); 
    signal Reg17_out : std_logic_vector(31 downto 0); 
    signal Reg18_out : std_logic_vector(31 downto 0); 
    signal Reg19_out : std_logic_vector(31 downto 0); 
    signal Reg20_out : std_logic_vector(31 downto 0); 
    signal Reg21_out : std_logic_vector(31 downto 0); 
    signal Reg22_out : std_logic_vector(31 downto 0); 
    signal Reg23_out : std_logic_vector(31 downto 0); 
    signal Reg24_out : std_logic_vector(31 downto 0); 
    signal Reg25_out : std_logic_vector(31 downto 0); 
    signal Reg26_out : std_logic_vector(31 downto 0); 
    signal Reg27_out : std_logic_vector(31 downto 0); 
    signal Reg28_out : std_logic_vector(31 downto 0); 
    signal Reg29_out : std_logic_vector(31 downto 0); 
    signal Reg30_out : std_logic_vector(31 downto 0); 
    signal Reg31_out : std_logic_vector(31 downto 0); 

    --temp register 01 to 15 output
   -- signal TempReg00_out : std_logic_vector(31 downto 0); 
    signal TempReg01_out : std_logic_vector(31 downto 0); 
    signal TempReg02_out : std_logic_vector(31 downto 0); 
    signal TempReg03_out : std_logic_vector(31 downto 0); 
    signal TempReg04_out : std_logic_vector(31 downto 0); 
    signal TempReg05_out : std_logic_vector(31 downto 0); 
    signal TempReg06_out : std_logic_vector(31 downto 0); 
    signal TempReg07_out : std_logic_vector(31 downto 0); 
    signal TempReg08_out : std_logic_vector(31 downto 0); 
    signal TempReg09_out : std_logic_vector(31 downto 0); 
    signal TempReg10_out : std_logic_vector(31 downto 0); 
    signal TempReg11_out : std_logic_vector(31 downto 0); 
    signal TempReg12_out : std_logic_vector(31 downto 0); 
    signal TempReg13_out : std_logic_vector(31 downto 0); 
    signal TempReg14_out : std_logic_vector(31 downto 0); 
    signal TempReg15_out : std_logic_vector(31 downto 0); 

    -- mux 32 bit outputs
    signal MUX32_A_out, MUX32_B_out : std_logic_vector(31 downto 0);

    --enable signals register 00 to 31
    signal Reg00_AND : std_logic;  
    signal Reg01_AND : std_logic;
    signal Reg02_AND : std_logic;
    signal Reg03_AND : std_logic;
    signal Reg04_AND : std_logic;
    signal Reg05_AND : std_logic;
    signal Reg06_AND : std_logic;
    signal Reg07_AND : std_logic;
    signal Reg08_AND : std_logic;
    signal Reg09_AND : std_logic;
    signal Reg10_AND : std_logic;
    signal Reg11_AND : std_logic;
    signal Reg12_AND : std_logic;
    signal Reg13_AND : std_logic;
    signal Reg14_AND : std_logic;
    signal Reg15_AND : std_logic;
    signal Reg16_AND : std_logic;
    signal Reg17_AND : std_logic;
    signal Reg18_AND : std_logic;
    signal Reg19_AND : std_logic;
    signal Reg20_AND : std_logic;
    signal Reg21_AND : std_logic;
    signal Reg22_AND : std_logic;
    signal Reg23_AND : std_logic;
    signal Reg24_AND : std_logic;
    signal Reg25_AND : std_logic;
    signal Reg26_AND : std_logic;
    signal Reg27_AND : std_logic;
    signal Reg28_AND : std_logic;
    signal Reg29_AND : std_logic;
    signal Reg30_AND : std_logic;
    signal Reg31_AND : std_logic;
 

    --enable signals temp registers 01 to 15
    signal TempReg00_AND : std_logic;
    signal TempReg01_AND : std_logic;
    signal TempReg02_AND : std_logic;
    signal TempReg03_AND : std_logic;
    signal TempReg04_AND : std_logic;
    signal TempReg05_AND : std_logic;
    signal TempReg06_AND : std_logic;
    signal TempReg07_AND : std_logic;
    signal TempReg08_AND : std_logic;
    signal TempReg09_AND : std_logic;
    signal TempReg10_AND : std_logic;
    signal TempReg11_AND : std_logic;
    signal TempReg12_AND : std_logic;
    signal TempReg13_AND : std_logic;
    signal TempReg14_AND : std_logic;
    signal TempReg15_AND : std_logic;
    
begin
    DestReg_Decoder: RF_DestReg_Decoder_21364066 port map(
        A => DR,
        Q00 => Reg00_AND,
        Q01 => Reg01_AND,
        Q02 => Reg02_AND,
        Q03 => Reg03_AND,
        Q04 => Reg04_AND,
        Q05 => Reg05_AND,
        Q06 => Reg06_AND,
        Q07 => Reg07_AND,
        Q08 => Reg08_AND,
        Q09 => Reg09_AND,
        Q10 => Reg10_AND,
        Q11 => Reg11_AND,
        Q12 => Reg12_AND,
        Q13 => Reg13_AND,
        Q14 => Reg14_AND,
        Q15 => Reg15_AND,
        Q16 => Reg16_AND,
        Q17 => Reg17_AND,
        Q18 => Reg18_AND,
        Q19 => Reg19_AND,
        Q20 => Reg20_AND,
        Q21 => Reg21_AND,
        Q22 => Reg22_AND,
        Q23 => Reg23_AND,
        Q24 => Reg24_AND,
        Q25 => Reg25_AND,
        Q26 => Reg26_AND,
        Q27 => Reg27_AND,
        Q28 => Reg28_AND,
        Q29 => Reg29_AND,
        Q30 => Reg30_AND,
        Q31 => Reg31_AND
    );
    DestTempReg_Decoder: RF_TempDestReg_Decoder_21364066 port map (
        A => TD,
        Q00 => TempReg00_AND,
        Q01 => TempReg01_AND,
        Q02 => TempReg02_AND,
        Q03 => TempReg03_AND,
        Q04 => TempReg04_AND,
        Q05 => TempReg05_AND,
        Q06 => TempReg06_AND,
        Q07 => TempReg07_AND,
        Q08 => TempReg08_AND,
        Q09 => TempReg09_AND,
        Q10 => TempReg10_AND,
        Q11 => TempReg11_AND,
        Q12 => TempReg12_AND,
        Q13 => TempReg13_AND,
        Q14 => TempReg14_AND,
        Q15 => TempReg15_AND
    );

    -- enable logic registers 00 to 31
    Load_Reg00 <= ((RW AND Reg00_AND) AND TempReg00_AND) after 4ns;
    Load_Reg01 <= ((RW AND Reg01_AND) AND TempReg00_AND) after 4ns;
    Load_Reg02 <= ((RW AND Reg02_AND) AND TempReg00_AND) after 4ns;
    Load_Reg03 <= ((RW AND Reg03_AND) AND TempReg00_AND) after 4ns;
    Load_Reg04 <= ((RW AND Reg04_AND) AND TempReg00_AND) after 4ns;
    Load_Reg05 <= ((RW AND Reg05_AND) AND TempReg00_AND) after 4ns;
    Load_Reg06 <= ((RW AND Reg06_AND) AND TempReg00_AND) after 4ns;
    Load_Reg07 <= ((RW AND Reg07_AND) AND TempReg00_AND) after 4ns;
    Load_Reg08 <= ((RW AND Reg08_AND) AND TempReg00_AND) after 4ns;
    Load_Reg09 <= ((RW AND Reg09_AND) AND TempReg00_AND) after 4ns;
    Load_Reg10 <= ((RW AND Reg10_AND) AND TempReg00_AND) after 4ns;
    Load_Reg11 <= ((RW AND Reg11_AND) AND TempReg00_AND) after 4ns;
    Load_Reg12 <= ((RW AND Reg12_AND) AND TempReg00_AND) after 4ns;
    Load_Reg13 <= ((RW AND Reg13_AND) AND TempReg00_AND) after 4ns;
    Load_Reg14 <= ((RW AND Reg14_AND) AND TempReg00_AND) after 4ns;
    Load_Reg15 <= ((RW AND Reg15_AND) AND TempReg00_AND) after 4ns;
    Load_Reg16 <= ((RW AND Reg16_AND) AND TempReg00_AND) after 4ns;
    Load_Reg17 <= ((RW AND Reg17_AND) AND TempReg00_AND) after 4ns;
    Load_Reg18 <= ((RW AND Reg18_AND) AND TempReg00_AND) after 4ns;
    Load_Reg19 <= ((RW AND Reg19_AND) AND TempReg00_AND) after 4ns;
    Load_Reg20 <= ((RW AND Reg20_AND) AND TempReg00_AND) after 4ns;
    Load_Reg21 <= ((RW AND Reg21_AND) AND TempReg00_AND) after 4ns;
    Load_Reg22 <= ((RW AND Reg22_AND) AND TempReg00_AND) after 4ns;
    Load_Reg23 <= ((RW AND Reg23_AND) AND TempReg00_AND) after 4ns;
    Load_Reg24 <= ((RW AND Reg24_AND) AND TempReg00_AND) after 4ns;
    Load_Reg25 <= ((RW AND Reg25_AND) AND TempReg00_AND) after 4ns;
    Load_Reg26 <= ((RW AND Reg26_AND) AND TempReg00_AND) after 4ns;
    Load_Reg27 <= ((RW AND Reg27_AND) AND TempReg00_AND) after 4ns;
    Load_Reg28 <= ((RW AND Reg28_AND) AND TempReg00_AND) after 4ns;
    Load_Reg29 <= ((RW AND Reg29_AND) AND TempReg00_AND) after 4ns;
    Load_Reg30 <= ((RW AND Reg30_AND) AND TempReg00_AND) after 4ns;
    Load_Reg31 <= ((RW AND Reg31_AND) AND TempReg00_AND) after 4ns;


    --enable logic for temp register 00 to 15
    Load_TempReg01 <= (RW AND TempReg01_AND);
    Load_TempReg02 <= (RW AND TempReg02_AND);
    Load_TempReg03 <= (RW AND TempReg03_AND);
    Load_TempReg04 <= (RW AND TempReg04_AND);
    Load_TempReg05 <= (RW AND TempReg05_AND);
    Load_TempReg06 <= (RW AND TempReg06_AND);
    Load_TempReg07 <= (RW AND TempReg07_AND);
    Load_TempReg08 <= (RW AND TempReg08_AND);
    Load_TempReg09 <= (RW AND TempReg09_AND);
    Load_TempReg10 <= (RW AND TempReg10_AND);
    Load_TempReg11 <= (RW AND TempReg11_AND);
    Load_TempReg12 <= (RW AND TempReg12_AND);
    Load_TempReg13 <= (RW AND TempReg13_AND);
    Load_TempReg14 <= (RW AND TempReg14_AND);
    Load_TempReg15 <= (RW AND TempReg15_AND);

    --temp registers 01 to 15
    TempReg01: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg01,
        Clk => Clock,
        Q => TempReg01_out
       );
       
       TempReg02: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg02,
        Clk => Clock,
        Q => TempReg02_out
       );
       
       TempReg03: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg03,
        Clk => Clock,
        Q => TempReg03_out
       );
       
       TempReg04: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg04,
        Clk => Clock,
        Q => TempReg04_out
       );
       
       TempReg05: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg05,
        Clk => Clock,
        Q => TempReg05_out
       );
       
       TempReg06: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg06,
        Clk => Clock,
        Q => TempReg06_out
       );
       
       TempReg07: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg07,
        Clk => Clock,
        Q => TempReg07_out
       );
       
       TempReg08: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg08,
        Clk => Clock,
        Q => TempReg08_out
       );
       
       TempReg09: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg09,
        Clk => Clock,
        Q => TempReg09_out
       );
       
       TempReg10: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg10,
        Clk => Clock,
        Q => TempReg10_out
       );
       
       TempReg11: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg11,
        Clk => Clock,
        Q => TempReg11_out
       );
       
       TempReg12: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg12,
        Clk => Clock,
        Q => TempReg12_out
       );
       
       TempReg13: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg13,
        Clk => Clock,
        Q => TempReg13_out
       );
       
       TempReg14: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg14,
        Clk => Clock,
        Q => TempReg14_out
       );

       TempReg15: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_TempReg15,
        Clk => Clock,
        Q => TempReg15_out
       );
       
       
       
       
       
    --registers 00 to 31
       Register00: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg00,
        Clk => Clock,
        Q => Reg00_out
       );

       Register01: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg01,
        Clk => Clock,
        Q => Reg01_out
       );
       
       Register02: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg02,
        Clk => Clock,
        Q => Reg02_out
       );
       
       Register03: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg03,
        Clk => Clock,
        Q => Reg03_out
       );
       
       Register04: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg04,
        Clk => Clock,
        Q => Reg04_out
       );
       
       Register05: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg05,
        Clk => Clock,
        Q => Reg05_out
       );
       
       Register06: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg06,
        Clk => Clock,
        Q => Reg06_out
       );
       
       Register07: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg07,
        Clk => Clock,
        Q => Reg07_out
       );
       
       Register08: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg08,
        Clk => Clock,
        Q => Reg08_out
       );
       
       Register09: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg09,
        Clk => Clock,
        Q => Reg09_out
       );
       
       
       Register10: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg10,
        Clk => Clock,
        Q => Reg10_out
       );
       
       Register11: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg11,
        Clk => Clock,
        Q => Reg11_out
       );
       
       Register12: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg12,
        Clk => Clock,
        Q => Reg12_out
       );
       
       Register13: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg13,
        Clk => Clock,
        Q => Reg13_out
       );
       
       Register14: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg14,
        Clk => Clock,
        Q => Reg14_out
       );
       
       Register15: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg15,
        Clk => Clock,
        Q => Reg15_out
       );
       
       Register16: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg16,
        Clk => Clock,
        Q => Reg16_out
       );
       
       Register17: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg17,
        Clk => Clock,
        Q => Reg17_out
       );
       
       Register18: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg18,
        Clk => Clock,
        Q => Reg18_out
       );
       
       Register19: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg19,
        Clk => Clock,
        Q => Reg19_out
       );
       
       Register20: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg20,
        Clk => Clock,
        Q => Reg20_out
       );
       
       Register21: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg21,
        Clk => Clock,
        Q => Reg21_out
       );
       
       Register22: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg22,
        Clk => Clock,
        Q => Reg22_out
       );
       
       Register23: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg23,
        Clk => Clock,
        Q => Reg23_out
       );
       
       Register24: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg24,
        Clk => Clock,
        Q => Reg24_out
       );
       
       Register25: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg25,
        Clk => Clock,
        Q => Reg25_out
       );
       
       Register26: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg26,
        Clk => Clock,
        Q => Reg26_out
       );
       
       Register27: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg27,
        Clk => Clock,
        Q => Reg27_out
       );
       
       Register28: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg28,
        Clk => Clock,
        Q => Reg28_out
       );
       
       Register29: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg29,
        Clk => Clock,
        Q => Reg29_out
       );
       
       Register30: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg30,
        Clk => Clock,
        Q => Reg30_out
       );
       
       Register31: RF_Register32Bit_21364066 port map(
        D => D,
        Load => Load_Reg31,
        Clk => Clock,
        Q => Reg31_out
       );
              
    -- multiplexers
    MUX32_A: RF_Mux32_32Bit_21364066 port map(
        In00 => Reg00_out,
        In01 => Reg01_out,
        In02 => Reg02_out,
        In03 => Reg03_out,
        In04 => Reg04_out,
        In05 => Reg05_out,
        In06 => Reg06_out,
        In07 => Reg07_out,
        In08 => Reg08_out,
        In09 => Reg09_out,
        In10 => Reg10_out,
        In11 => Reg11_out,
        In12 => Reg12_out,
        In13 => Reg13_out,
        In14 => Reg14_out,
        In15 => Reg15_out,
        In16 => Reg16_out,
        In17 => Reg17_out,
        In18 => Reg18_out,
        In19 => Reg19_out,
        In20 => Reg20_out,
        In21 => Reg21_out,
        In22 => Reg22_out,
        In23 => Reg23_out,
        In24 => Reg24_out,
        In25 => Reg25_out,
        In26 => Reg26_out,
        In27 => Reg27_out,
        In28 => Reg28_out,
        In29 => Reg29_out,
        In30 => Reg30_out,
        In31 => Reg31_out,
        
        A => SA, 

        Z => MUX32_A_out
    );

    MUX32_B: RF_Mux32_32Bit_21364066 port map(
        In00 => Reg00_out,
        In01 => Reg01_out,
        In02 => Reg02_out,
        In03 => Reg03_out,
        In04 => Reg04_out,
        In05 => Reg05_out,
        In06 => Reg06_out,
        In07 => Reg07_out,
        In08 => Reg08_out,
        In09 => Reg09_out,
        In10 => Reg10_out,
        In11 => Reg11_out,
        In12 => Reg12_out,
        In13 => Reg13_out,
        In14 => Reg14_out,
        In15 => Reg15_out,
        In16 => Reg16_out,
        In17 => Reg17_out,
        In18 => Reg18_out,
        In19 => Reg19_out,
        In20 => Reg20_out,
        In21 => Reg21_out,
        In22 => Reg22_out,
        In23 => Reg23_out,
        In24 => Reg24_out,
        In25 => Reg25_out,
        In26 => Reg26_out,
        In27 => Reg27_out,
        In28 => Reg28_out,
        In29 => Reg29_out,
        In30 => Reg30_out,
        In31 => Reg31_out,
        
        A => SB, 

        Z => MUX32_B_out
    );

    MUX16_A: RF_Mux16_32Bit_21364066 port map(
        In00 => MUX32_A_out,
        In01 => TempReg01_out,
        In02 => TempReg02_out,
        In03 => TempReg03_out,
        In04 => TempReg04_out,
        In05 => TempReg05_out,
        In06 => TempReg06_out,
        In07 => TempReg07_out,
        In08 => TempReg08_out,
        In09 => TempReg09_out,
        In10 => TempReg10_out,
        In11 => TempReg11_out,
        In12 => TempReg12_out,
        In13 => TempReg13_out,
        In14 => TempReg14_out,
        In15 => TempReg15_out,

        A => TA, 

        Z => A
    );

    MUX16_B: RF_Mux16_32Bit_21364066 port map(
        In00 => MUX32_B_out,
        In01 => TempReg01_out,
        In02 => TempReg02_out,
        In03 => TempReg03_out,
        In04 => TempReg04_out,
        In05 => TempReg05_out,
        In06 => TempReg06_out,
        In07 => TempReg07_out,
        In08 => TempReg08_out,
        In09 => TempReg09_out,
        In10 => TempReg10_out,
        In11 => TempReg11_out,
        In12 => TempReg12_out,
        In13 => TempReg13_out,
        In14 => TempReg14_out,
        In15 => TempReg15_out,

        A => TB, 

        Z => B
    );
end Behavioral; 
----------------------------------------------------------------------------------
-- Company: Trinity College architecture
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 23:12:00
-- Design Name: 
-- Module Name: CPU_RAM_21364066_TB - Sim
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity CPU_RAM_21364066_TB is
--  Port ( ); We don't need ports
end CPU_RAM_21364066_TB;

architecture Sim of CPU_RAM_21364066_TB is
-- Component Declaration for the Unit Under Test (UUT)

component CPU_RAM_21364066
port ( 
        Clock : in std_logic;
        Address : in std_logic_vector (31 downto 0);
        DataIn : in std_logic_vector (31 downto 0);
        WriteEnable : in std_logic;
        DataOut : out std_logic_vector (31 downto 0)
    );
end component;

--Inputs

    signal Clock_TB : std_logic := '0';
    signal Address_TB : std_logic_vector(31 downto 0) := (others => '0');
    signal DataIn_TB : std_logic_vector(31 downto 0) := (others => '0');
    signal WriteEnable_TB : std_logic := '0';
    
--Outputs

    signal DataOut_TB : std_logic_vector(31 downto 0) := (others => '0');
    
    constant period : time := 100ns;
begin
	-- Instantiate the Unit Under Test (UUT)
	
   uut: CPU_RAM_21364066 port map (
        Clock => Clock_TB,
        Address => Address_TB,
        DataIn => DataIn_TB,
        WriteEnable => WriteEnable_TB,
        DataOut => DataOut_TB
    );

    Clock_TB <= not Clock_TB after period/2;
   
   stim_proc: process
   begin
    
    --READ--
    --Address 0--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000000";

    --Address 1--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000001";

    --Address 2--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000002";

    --Address 3--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000003";

    --Address 4--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000004";

    --Address 5--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000005";

    --Address 6--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000006";

    --Address 7--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000007";

    --Address 8--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000008";

    --Address 9--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000009";

    --Address 10--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000000A";

    --Address 11--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000000B";

    --Address 12--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000000C";

    --Address 13--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000000D";

    --Address 14--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000000E";

    --Address 15--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000000F";

    --Address 16--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000010";

    --Address 17--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000011";

    --Address 18--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000012";

    --Address 19--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000013";

    --Address 20--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000014";

    --Address 21--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000015";

    --Address 22--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000016";

    --Address 23--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000017";

    --Address 24--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000018";

    --Address 25--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000019";

    --Address 26--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000001A";

    --Address 27--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000001B";

    --Address 28--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000001C";

    --Address 29--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000001D";

    --Address 30--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000001E";

    --Address 31--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000001F";

    --Address 32--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000020";

    --Address 33--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000021";

    --Address 34--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000022";

    --Address 35--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000023";

    --Address 36--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000024";

    --Address 37--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000025";

    --Address 38--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000026";

    --Address 39--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000027";

    --Address 40--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000028";

    --Address 41--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000029";

    --Address 42--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000002A";

    --Address 43--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000002B";

    --Address 44--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000002C";

    --Address 45--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000002D";

    --Address 46--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000002E";

    --Address 47--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000002F";

    --Address 48--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000030";

    --Address 49--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000031";

    --Address 50--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000032";

    --Address 51--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000033";

    --Address 52--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000034";

    --Address 53--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000035";

    --Address 54--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000036";

    --Address 55--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000037";

    --Address 56--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000038";

    --Address 57--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000039";

    --Address 58--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000003A";

    --Address 59--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000003B";

    --Address 60--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000003C";

    --Address 61--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000003D";

    --Address 62--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000003E";

    --Address 63--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000003F";

    --Address 64--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000040";

    --Address 65--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000041";

    --Address 66--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000042";

    --Address 67--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000043";

    --Address 68--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000044";

    --Address 69--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000045";

    --Address 70--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000046";

    --Address 71--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000047";

    --Address 72--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000048";

    --Address 73--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000049";

    --Address 74--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000004A";

    --Address 75--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000004B";

    --Address 76--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000004C";

    --Address 77--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000004D";

    --Address 78--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000004E";

    --Address 79--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000004F";

    --Address 80--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000050";

    --Address 81--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000051";

    --Address 82--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000052";

    --Address 83--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000053";

    --Address 84--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000054";

    --Address 85--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000055";

    --Address 86--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000056";

    --Address 87--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000057";

    --Address 88--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000058";

    --Address 89--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000059";

    --Address 90--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000005A";

    --Address 91--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000005B";

    --Address 92--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000005C";

    --Address 93--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000005D";

    --Address 94--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000005E";

    --Address 95--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000005F";

    --Address 96--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000060";

    --Address 97--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000061";

    --Address 98--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000062";

    --Address 99--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000063";

    --Address 100--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000064";

    --Address 101--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000065";

    --Address 102--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000066";

    --Address 103--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000067";

    --Address 104--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000068";

    --Address 105--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000069";

    --Address 106--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000006A";

    --Address 107--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000006B";

    --Address 108--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000006C";

    --Address 109--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000006D";

    --Address 110--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000006E";

    --Address 111--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000006F";

    --Address 112--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000070";

    --Address 113--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000071";

    --Address 114--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000072";

    --Address 115--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000073";

    --Address 116--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000074";

    --Address 117--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000075";

    --Address 118--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000076";

    --Address 119--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000077";

    --Address 120--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000078";

    --Address 121--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"00000079";

    --Address 122--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000007A";

    --Address 123--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000007B";

    --Address 124--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000007C";

    --Address 125--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000007D";

    --Address 126--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000007E";

    --Address 127--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    Address_TB <= X"0000007F";


    --OVERWRITE WITH 0--
    --Address 0--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000000";

    --Address 1--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000001";

    --Address 2--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000002";

    --Address 3--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000003";

    --Address 4--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000004";

    --Address 5--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000005";

    --Address 6--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000006";

    --Address 7--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000007";

    --Address 8--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000008";

    --Address 9--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000009";

    --Address 10--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"0000000A";

    --Address 11--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"0000000B";

    --Address 12--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"0000000C";

    --Address 13--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"0000000D";

    --Address 14--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"0000000E";

    --Address 15--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"0000000F";

    --Address 16--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000010";

    --Address 17--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000011";

    --Address 18--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000012";

    --Address 19--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000013";

    --Address 20--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000014";

    --Address 21--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000015";

    --Address 22--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000016";

    --Address 23--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000017";

    --Address 24--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000018";

    --Address 25--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000019";

    --Address 26--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"0000001A";

    --Address 27--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"0000001B";

    --Address 28--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"0000001C";

    --Address 29--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"0000001D";

    --Address 30--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"0000001E";

    --Address 31--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '1';
    DataIn_TB <= X"00000000";
    Address_TB <= X"0000001F";

    --NO OVERWRITE WITHOUT WriteEnable_TB(MW) = 1--
    --Address 32--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000020";

    --Address 33--
    wait until Clock_TB'event and Clock_TB = '1';
    WriteEnable_TB <= '0';
    DataIn_TB <= X"00000000";
    Address_TB <= X"00000021";

   end process;
end Sim;
----------------------------------------------------------------------------------
-- Company: Trinity College Dublin
-- Engineer: Faith Olopade
-- 
-- Create Date: 17.10.2022 22:35:00
-- Design Name: 
-- Module Name: CPU_RAM_21364066 - Behavioral
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

entity CPU_RAM_21364066 is
    port (  Clock : in std_logic;
            Address : in std_logic_vector (31 downto 0);
            DataIn : in std_logic_vector (31 downto 0);
            WriteEnable : in std_logic;
            DataOut : out std_logic_vector (31 downto 0)
        );
end CPU_RAM_21364066;

architecture Behavioral of CPU_RAM_21364066 is
    -- we use the least significant 7 bits of the address
type RAM_array is array(0 to 127) of std_logic_vector (31 downto 0);
signal RAM : RAM_array:=( --INITIALIZE--
                X"00000042", -- 66   DataOut = Last 2 ID + 0  
                X"00000043", -- 67   DataOut = Last 2 ID + 1  
                X"00000044", -- 68   DataOut = Last 2 ID + 2  
                X"00000045", -- 69   DataOut = Last 2 ID + 3  
                X"00000046", -- 70   DataOut = Last 2 ID + 4  
                X"00000047", -- 71   DataOut = Last 2 ID + 5  
                X"00000048", -- 72   DataOut = Last 2 ID + 6  
                X"00000049", -- 73   DataOut = Last 2 ID + 7  
                X"0000004A", -- 74   DataOut = Last 2 ID + 8  
                X"0000004B", -- 75   DataOut = Last 2 ID + 9  
                X"0000004C", -- 76   DataOut = Last 2 ID + 10  
                X"0000004D", -- 77   DataOut = Last 2 ID + 11  
                X"0000004E", -- 78   DataOut = Last 2 ID + 12  
                X"0000004F", -- 79   DataOut = Last 2 ID + 13  
                X"00000050", -- 80   DataOut = Last 2 ID + 14  
                X"00000051", -- 81   DataOut = Last 2 ID + 15

                X"00000052", -- 82   DataOut = Last 2 ID + 16
                X"00000053", -- 83   DataOut = Last 2 ID + 17
                X"00000054", -- 84   DataOut = Last 2 ID + 18
                X"00000055", -- 85   DataOut = Last 2 ID + 19
                X"00000056", -- 86   DataOut = Last 2 ID + 20
                X"00000057", -- 87   DataOut = Last 2 ID + 21
                X"00000058", -- 88   DataOut = Last 2 ID + 22
                X"00000059", -- 89   DataOut = Last 2 ID + 23
                X"0000005A", -- 90   DataOut = Last 2 ID + 24
                X"0000005B", -- 91   DataOut = Last 2 ID + 25
                X"0000005C", -- 92   DataOut = Last 2 ID + 26
                X"0000005D", -- 93   DataOut = Last 2 ID + 27
                X"0000005E", -- 94   DataOut = Last 2 ID + 28
                X"0000005F", -- 95   DataOut = Last 2 ID + 29
                X"00000060", -- 96   DataOut = Last 2 ID + 30
                X"00000061", -- 97   DataOut = Last 2 ID + 31

                X"00000062", -- 98   DataOut = Last 2 ID + 32
                X"00000063", -- 99   DataOut = Last 2 ID + 33
                X"00000064", -- 100  DataOut = Last 2 ID + 34
                X"00000065", -- 101  DataOut = Last 2 ID + 35
                X"00000066", -- 102  DataOut = Last 2 ID + 36
                X"00000067", -- 103  DataOut = Last 2 ID + 37
                X"00000068", -- 104  DataOut = Last 2 ID + 38
                X"00000069", -- 105  DataOut = Last 2 ID + 39
                X"0000006A", -- 106  DataOut = Last 2 ID + 40
                X"0000006B", -- 107  DataOut = Last 2 ID + 41
                X"0000006C", -- 108  DataOut = Last 2 ID + 42
                X"0000006D", -- 109  DataOut = Last 2 ID + 43
                X"0000006E", -- 110  DataOut = Last 2 ID + 44
                X"0000006F", -- 111  DataOut = Last 2 ID + 45
                X"00000070", -- 112  DataOut = Last 2 ID + 46
                X"00000071", -- 113  DataOut = Last 2 ID + 47

                X"00000072", -- 114   DataOut = Last 2 ID + 48
                X"00000073", -- 115   DataOut = Last 2 ID + 49
                X"00000074", -- 116   DataOut = Last 2 ID + 50
                X"00000075", -- 117   DataOut = Last 2 ID + 51
                X"00000076", -- 118   DataOut = Last 2 ID + 52
                X"00000077", -- 119   DataOut = Last 2 ID + 53
                X"00000078", -- 120   DataOut = Last 2 ID + 54
                X"00000079", -- 121   DataOut = Last 2 ID + 55
                X"0000007A", -- 122   DataOut = Last 2 ID + 56
                X"0000007B", -- 123   DataOut = Last 2 ID + 57
                X"0000007C", -- 124   DataOut = Last 2 ID + 58
                X"0000007D", -- 125   DataOut = Last 2 ID + 59
                X"0000007E", -- 126   DataOut = Last 2 ID + 60
                X"0000007F", -- 127   DataOut = Last 2 ID + 61
                X"00000080", -- 128   DataOut = Last 2 ID + 62
                X"00000081", -- 129   DataOut = Last 2 ID + 63

                X"00000082", -- 130   DataOut = Last 2 ID + 64
                X"00000083", -- 131   DataOut = Last 2 ID + 65
                X"00000084", -- 132   DataOut = Last 2 ID + 66
                X"00000085", -- 133   DataOut = Last 2 ID + 67
                X"00000086", -- 134   DataOut = Last 2 ID + 68
                X"00000087", -- 135   DataOut = Last 2 ID + 69
                X"00000088", -- 136   DataOut = Last 2 ID + 70
                X"00000089", -- 137   DataOut = Last 2 ID + 71
                X"0000008A", -- 138   DataOut = Last 2 ID + 72
                X"0000008B", -- 139   DataOut = Last 2 ID + 73
                X"0000008C", -- 140   DataOut = Last 2 ID + 74
                X"0000008D", -- 141   DataOut = Last 2 ID + 75
                X"0000008E", -- 142   DataOut = Last 2 ID + 76
                X"0000008F", -- 143   DataOut = Last 2 ID + 77
                X"00000090", -- 144   DataOut = Last 2 ID + 78
                X"00000091", -- 145   DataOut = Last 2 ID + 79

                X"00000092", -- 146   DataOut = Last 2 ID + 80
                X"00000093", -- 147   DataOut = Last 2 ID + 81
                X"00000094", -- 148   DataOut = Last 2 ID + 82
                X"00000095", -- 149   DataOut = Last 2 ID + 83
                X"00000096", -- 150   DataOut = Last 2 ID + 84
                X"00000097", -- 151   DataOut = Last 2 ID + 85
                X"00000098", -- 152   DataOut = Last 2 ID + 86
                X"00000099", -- 153   DataOut = Last 2 ID + 87
                X"0000009A", -- 154   DataOut = Last 2 ID + 88
                X"0000009B", -- 155   DataOut = Last 2 ID + 89
                X"0000009C", -- 156   DataOut = Last 2 ID + 90
                X"0000009D", -- 157   DataOut = Last 2 ID + 91
                X"0000009E", -- 158   DataOut = Last 2 ID + 92
                X"0000009F", -- 159   DataOut = Last 2 ID + 93
                X"000000A0", -- 160   DataOut = Last 2 ID + 94
                X"000000A1", -- 161   DataOut = Last 2 ID + 95

                X"000000A2", -- 162   DataOut = Last 2 ID + 96
                X"000000A3", -- 163   DataOut = Last 2 ID + 97
                X"000000A4", -- 164   DataOut = Last 2 ID + 98
                X"000000A5", -- 165   DataOut = Last 2 ID + 99
                X"000000A6", -- 166   DataOut = Last 2 ID + 100
                X"000000A7", -- 167   DataOut = Last 2 ID + 101
                X"000000A8", -- 168   DataOut = Last 2 ID + 102
                X"000000A9", -- 169   DataOut = Last 2 ID + 103
                X"000000AA", -- 170   DataOut = Last 2 ID + 104
                X"000000AB", -- 171   DataOut = Last 2 ID + 105
                X"000000AC", -- 172   DataOut = Last 2 ID + 106
                X"000000AD", -- 173   DataOut = Last 2 ID + 107
                X"000000AE", -- 174   DataOut = Last 2 ID + 108
                X"000000AF", -- 175   DataOut = Last 2 ID + 109
                X"000000B0", -- 176   DataOut = Last 2 ID + 110
                X"000000B1", -- 177   DataOut = Last 2 ID + 111

                X"000000B2", -- 178   DataOut = Last 2 ID + 112
                X"000000B3", -- 179   DataOut = Last 2 ID + 113
                X"000000B4", -- 180   DataOut = Last 2 ID + 114
                X"000000B5", -- 181   DataOut = Last 2 ID + 115
                X"000000B6", -- 182   DataOut = Last 2 ID + 116
                X"000000B7", -- 183   DataOut = Last 2 ID + 117
                X"000000B8", -- 184   DataOut = Last 2 ID + 118
                X"000000B9", -- 185   DataOut = Last 2 ID + 119
                X"000000BA", -- 186   DataOut = Last 2 ID + 120
                X"000000BB", -- 187   DataOut = Last 2 ID + 121
                X"000000BC", -- 188   DataOut = Last 2 ID + 122
                X"000000BD", -- 189   DataOut = Last 2 ID + 123
                X"000000BE", -- 190   DataOut = Last 2 ID + 124
                X"000000BF", -- 191   DataOut = Last 2 ID + 125
                X"000000C0", -- 192   DataOut = Last 2 ID + 126
                X"000000C1"  -- 193   DataOut = Last 2 ID + 127         
    );
begin
    process (Clock)
    
    begin
        if Clock'event and Clock='1' then
            if WriteEnable='1' then
            RAM(to_integer(unsigned(Address(6 downto 0)))) <= DataIn after 10ns;
            end if;
        end if;
    end process;
    
    DataOut <= RAM(to_integer(unsigned(Address(6 downto 0)))) after 10ns;
    
end Behavioral;
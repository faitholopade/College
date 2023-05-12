RW_TB    <= '1';

TD_TB    <= "0000";
-- DR_TB    <= "00000";
-- SA_TB_TB <= "00000";
-- SB_TB_TB <= "00000";

-- REGISTERS --

--0
wait until Clock_TB'event and Clock_TB = '1';
D_TB  <= "00000001010001011111110101100010";
TA_TB    <= "0000";
TB_TB    <= "0000";
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;

--1
wait until Clock_TB'event and Clock_TB = '1';
D_TB  <= "00000001010001011111110101100011" after period/2;
TA_TB    <= "0001";
TB_TB    <= "0001";
DR_TB <= "00001" after period/2;
SA_TB <= "00001" after period/2;
SB_TB <= "00001" after period/2;
 
--2
wait until Clock_TB'event and Clock_TB = '1';
D_TB  <= "00000001010001011111110101100100" after period/2;
TA_TB    <= "0010";
TB_TB    <= "0010";
DR_TB <= "00010" after period/2;
SA_TB <= "00010" after period/2;
SB_TB <= "00010" after period/2;
 
--3
wait until Clock_TB'event and Clock_TB = '1';
D_TB  <= "00000001010001011111110101100101" after period/2;
TA_TB    <= "0011";
TB_TB    <= "0011";
DR_TB <= "00011" after period/2;
SA_TB <= "00011" after period/2;
SB_TB <= "00011" after period/2;
 
--4
wait until Clock_TB'event and Clock_TB = '1';
D_TB  <= "00000001010001011111110101100110" after period/2;
TA_TB    <= "0100";
TB_TB    <= "0100";
DR_TB <= "00100" after period/2;
SA_TB <= "00100" after period/2;
SB_TB <= "00100" after period/2;
 
--5
wait until Clock_TB'event and Clock_TB = '1';
D_TB  <= "00000001010001011111110101100111" after period/2;
TA_TB    <= "0101";
TB_TB    <= "0101";
DR_TB <= "00101" after period/2;
SA_TB <= "00101" after period/2;
SB_TB <= "00101" after period/2;
 
--6
wait until Clock_TB'event and Clock_TB = '1';
D_TB  <= "00000001010001011111110101101000" after period/2;
TA_TB    <= "0110";
TB_TB    <= "0110";
DR_TB <= "00110" after period/2;
SA_TB <= "00110" after period/2;
SB_TB <= "00110" after period/2;
 
--7
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101101001" after period/2;
TA_TB    <= "0111";
TB_TB    <= "0111";
DR_TB <= "00111" after period/2;
SA_TB <= "00111" after period/2;
SB_TB <= "00111" after period/2;
 
--8
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101101010" after period/2;
TA_TB    <= "1000";
TB_TB    <= "1000";
DR_TB <= "01000" after period/2;
SA_TB <= "01000" after period/2;
SB_TB <= "01000" after period/2;
 
--9
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101101011" after period/2;
TA_TB    <= "1001";
TB_TB    <= "1001";
DR_TB <= "01001" after period/2;
SA_TB <= "01001" after period/2;
SB_TB <= "01001" after period/2;
 
--10
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101101100" after period/2;
TA_TB    <= "1010";
TB_TB    <= "1010";
DR_TB <= "01010" after period/2;
SA_TB <= "01010" after period/2;
SB_TB <= "01010" after period/2;
 
--11
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101101101" after period/2;
TA_TB    <= "1011";
TB_TB    <= "1011";
DR_TB <= "01011" after period/2;
SA_TB <= "01011" after period/2;
SB_TB <= "01011" after period/2;
 
--12
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101101110" after period/2;
TA_TB    <= "1100";
TB_TB    <= "1100";
DR_TB <= "01100" after period/2;
SA_TB <= "01100" after period/2;
SB_TB <= "01100" after period/2;
 
--13
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101101111" after period/2;
TA_TB    <= "1101";
TB_TB    <= "1101";
DR_TB <= "01101" after period/2;
SA_TB <= "01101" after period/2;
SB_TB <= "01101" after period/2;
 
--14
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101110000" after period/2;
TA_TB    <= "1110";
TB_TB    <= "1110";
DR_TB <= "01110" after period/2;
SA_TB <= "01110" after period/2;
SB_TB <= "01110" after period/2;
 
--15
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101110001" after period/2;
TA_TB    <= "1111";
TB_TB    <= "1111";
DR_TB <= "01111" after period/2;
SA_TB <= "01111" after period/2;
SB_TB <= "01111" after period/2;
 
--16
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101110010" after period/2;
TA_TB    <= "0000";
TB_TB    <= "0000";
DR_TB <= "10000" after period/2;
SA_TB <= "10000" after period/2;
SB_TB <= "10000" after period/2;
 
--17
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101110011" after period/2;
TA_TB    <= "0001";
TB_TB    <= "0001";
DR_TB <= "10001" after period/2;
SA_TB <= "10001" after period/2;
SB_TB <= "10001" after period/2;
 
--18
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101110100" after period/2;
TA_TB    <= "0010";
TB_TB    <= "0010";
DR_TB <= "10010" after period/2;
SA_TB <= "10010" after period/2;
SB_TB <= "10010" after period/2;
 
--19
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101110101" after period/2;
TA_TB    <= "0011";
TB_TB    <= "0011";
DR_TB <= "10011" after period/2;
SA_TB <= "10011" after period/2;
SB_TB <= "10011" after period/2;
 
--20
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101110110" after period/2;
TA_TB    <= "0100";
TB_TB    <= "0100";
DR_TB <= "10100" after period/2;
SA_TB <= "10100" after period/2;
SB_TB <= "10100" after period/2;
 
--21
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101110111" after period/2;
TA_TB    <= "0101";
TB_TB    <= "0101";
DR_TB <= "10101" after period/2;
SA_TB <= "10101" after period/2;
SB_TB <= "10101" after period/2;
 
--22
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101111000" after period/2;
TA_TB    <= "0110";
TB_TB    <= "0110";
DR_TB <= "10110" after period/2;
SA_TB <= "10110" after period/2;
SB_TB <= "10110" after period/2;
 
--23
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101111001" after period/2;
TA_TB    <= "0111";
TB_TB    <= "0111";
DR_TB <= "10111" after period/2;
SA_TB <= "10111" after period/2;
SB_TB <= "10111" after period/2;
 
--24
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101111010" after period/2;
TA_TB    <= "1000";
TB_TB    <= "1000";
DR_TB <= "11000" after period/2;
SA_TB <= "11000" after period/2;
SB_TB <= "11000" after period/2;
 
--25
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101111011" after period/2;
TA_TB    <= "1001";
TB_TB    <= "1001";
DR_TB <= "11001" after period/2;
SA_TB <= "11001" after period/2;
SB_TB <= "11001" after period/2;
 
--26
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101111100" after period/2;
TA_TB    <= "1010";
TB_TB    <= "1010";
DR_TB <= "11010" after period/2;
SA_TB <= "11010" after period/2;
SB_TB <= "11010" after period/2;
 
--27
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101111101" after period/2;
TA_TB    <= "1011";
TB_TB    <= "1011";
DR_TB <= "11011" after period/2;
SA_TB <= "11011" after period/2;
SB_TB <= "11011" after period/2;
 
--28
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101111110" after period/2;
TA_TB    <= "1100";
TB_TB    <= "1100";
DR_TB <= "11100" after period/2;
SA_TB <= "11100" after period/2;
SB_TB <= "11100" after period/2;
 
--29
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110101111111" after period/2;
TA_TB    <= "1101";
TB_TB    <= "1101";
DR_TB <= "11101" after period/2;
SA_TB <= "11101" after period/2;
SB_TB <= "11101" after period/2;
 
--30
wait until Clock_TB'event and Clock_TB = '1';
-- RW    <= '1' after period/2;
D_TB  <= "00000001010001011111110110000000" after period/2;
TA_TB    <= "1110";
TB_TB    <= "1110";
DR_TB <= "11110" after period/2;
SA_TB <= "11110" after period/2;
SB_TB <= "11110" after period/2;
 
--31
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110110000001" after period/2;
TA_TB    <= "1111";
TB_TB    <= "1111";
DR_TB <= "11111" after period/2;
SA_TB <= "11111" after period/2;
SB_TB <= "11111" after period/2;

--TEMP REGISTERS --
 
--0
wait until Clock_TB'event and Clock_TB = '1';
RW_TB    <= '1' after period/2;
D_TB  <= "00000001010001011111110101100010" after period/2;
TA_TB    <= "0001" after period/2;
TB_TB    <= "0001" after period/2;
TD_TB     <= "0001" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
--1
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110101100011" after period/2;
TA_TB    <= "0010" after period/2;
TB_TB     <= "0010" after period/2;
TD_TB     <= "0010" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
--2
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110101100100" after period/2;
TA_TB    <= "0010" after period/2;
TB_TB     <= "0010" after period/2;
TD_TB     <= "0010" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
--3
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110101100101" after period/2;
TA_TB    <= "0011" after period/2;
TB_TB     <= "0011" after period/2;
TD_TB     <= "0011" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
--4
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110101100110" after period/2;
TA_TB    <= "0100" after period/2;
TB_TB     <= "0100" after period/2;
TD_TB     <= "0100" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
--5
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110101100111" after period/2;
TA_TB    <= "0101" after period/2;
TB_TB     <= "0101" after period/2;
TD_TB     <= "0101" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
--6
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110101101000" after period/2;
TA_TB    <= "0110" after period/2;
TB_TB     <= "0110" after period/2;
TD_TB     <= "0110" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
--7
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110101101001" after period/2;
TA_TB    <= "0111" after period/2;
TB_TB     <= "0111" after period/2;
TD_TB     <= "0111" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
--8
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110101101010" after period/2;
TA_TB    <= "1000" after period/2;
TB_TB     <= "1000" after period/2;
TD_TB     <= "1000" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
--9
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110101101011" after period/2;
TA_TB    <= "1001" after period/2;
TB_TB     <= "1001" after period/2;
TD_TB     <= "1001" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
--10
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110101101100" after period/2;
TA_TB    <= "1010" after period/2;
TB_TB     <= "1010" after period/2;
TD_TB     <= "1010" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
--11
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110101101101" after period/2;
TA_TB    <= "1011" after period/2;
TB_TB     <= "1011" after period/2;
TD_TB     <= "1011" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
--12
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110101101110" after period/2;
TA_TB    <= "1100" after period/2;
TB_TB     <= "1100" after period/2;
TD_TB     <= "1100" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
--13
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110101101111" after period/2;
TA_TB    <= "1101" after period/2;
TB_TB     <= "1101" after period/2;
TD_TB     <= "1101" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
--14
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "00000001010001011111110101110000" after period/2;
TA_TB    <= "1110" after period/2;
TB_TB     <= "1110" after period/2;
TD_TB    <= "1110" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;

--15
wait until Clock_TB'event and Clock_TB = '1';
-- RW_TB     <= '1' after period/2;
D_TB  <= "" after period/2;
TA_TB    <= "1111" after period/2;
TB_TB     <= "1111" after period/2;
TD_TB     <= "1111" after period/2;
DR_TB <= "00000" after period/2;
SA_TB <= "00000" after period/2;
SB_TB <= "00000" after period/2;
 
END PROCESS;

END;
--------------------------------------------------------------------------------
--                  FixRealKCM_Freq100_uid8_T0_Freq100_uid11
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity FixRealKCM_Freq100_uid8_T0_Freq100_uid11 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(11 downto 0)   );
end entity;

architecture arch of FixRealKCM_Freq100_uid8_T0_Freq100_uid11 is
signal Y0 :  std_logic_vector(11 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(11 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000001000" when "000000",
      "000000110110" when "000001",
      "000001100100" when "000010",
      "000010010010" when "000011",
      "000011000001" when "000100",
      "000011101111" when "000101",
      "000100011101" when "000110",
      "000101001011" when "000111",
      "000101111001" when "001000",
      "000110100111" when "001001",
      "000111010110" when "001010",
      "001000000100" when "001011",
      "001000110010" when "001100",
      "001001100000" when "001101",
      "001010001110" when "001110",
      "001010111100" when "001111",
      "001011101011" when "010000",
      "001100011001" when "010001",
      "001101000111" when "010010",
      "001101110101" when "010011",
      "001110100011" when "010100",
      "001111010001" when "010101",
      "010000000000" when "010110",
      "010000101110" when "010111",
      "010001011100" when "011000",
      "010010001010" when "011001",
      "010010111000" when "011010",
      "010011100110" when "011011",
      "010100010101" when "011100",
      "010101000011" when "011101",
      "010101110001" when "011110",
      "010110011111" when "011111",
      "010111001101" when "100000",
      "010111111011" when "100001",
      "011000101010" when "100010",
      "011001011000" when "100011",
      "011010000110" when "100100",
      "011010110100" when "100101",
      "011011100010" when "100110",
      "011100010000" when "100111",
      "011100111111" when "101000",
      "011101101101" when "101001",
      "011110011011" when "101010",
      "011111001001" when "101011",
      "011111110111" when "101100",
      "100000100101" when "101101",
      "100001010100" when "101110",
      "100010000010" when "101111",
      "100010110000" when "110000",
      "100011011110" when "110001",
      "100100001100" when "110010",
      "100100111010" when "110011",
      "100101101001" when "110100",
      "100110010111" when "110101",
      "100111000101" when "110110",
      "100111110011" when "110111",
      "101000100001" when "111000",
      "101001001111" when "111001",
      "101001111110" when "111010",
      "101010101100" when "111011",
      "101011011010" when "111100",
      "101100001000" when "111101",
      "101100110110" when "111110",
      "101101100100" when "111111",
      "------------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                  FixRealKCM_Freq100_uid8_T1_Freq100_uid14
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity FixRealKCM_Freq100_uid8_T1_Freq100_uid14 is
    port (X : in  std_logic_vector(3 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of FixRealKCM_Freq100_uid8_T1_Freq100_uid14 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "0000",
      "000011" when "0001",
      "000110" when "0010",
      "001001" when "0011",
      "001100" when "0100",
      "001110" when "0101",
      "010001" when "0110",
      "010100" when "0111",
      "010111" when "1000",
      "011010" when "1001",
      "011101" when "1010",
      "100000" when "1011",
      "100011" when "1100",
      "100110" when "1101",
      "101000" when "1110",
      "101011" when "1111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                 FixRealKCM_Freq100_uid20_T0_Freq100_uid23
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity FixRealKCM_Freq100_uid20_T0_Freq100_uid23 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(34 downto 0)   );
end entity;

architecture arch of FixRealKCM_Freq100_uid20_T0_Freq100_uid23 is
signal Y0 :  std_logic_vector(34 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(34 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "00000000000000000000000000000000000" when "000000",
      "00000010110001011100100001011111111" when "000001",
      "00000101100010111001000010111111110" when "000010",
      "00001000010100010101100100011111101" when "000011",
      "00001011000101110010000101111111100" when "000100",
      "00001101110111001110100111011111011" when "000101",
      "00010000101000101011001000111111010" when "000110",
      "00010011011010000111101010011111001" when "000111",
      "00010110001011100100001011111111000" when "001000",
      "00011000111101000000101101011110111" when "001001",
      "00011011101110011101001110111110110" when "001010",
      "00011110011111111001110000011110101" when "001011",
      "00100001010001010110010001111110100" when "001100",
      "00100100000010110010110011011110011" when "001101",
      "00100110110100001111010100111110010" when "001110",
      "00101001100101101011110110011110001" when "001111",
      "00101100010111001000010111111110000" when "010000",
      "00101111001000100100111001011101111" when "010001",
      "00110001111010000001011010111101110" when "010010",
      "00110100101011011101111100011101101" when "010011",
      "00110111011100111010011101111101100" when "010100",
      "00111010001110010110111111011101011" when "010101",
      "00111100111111110011100000111101010" when "010110",
      "00111111110001010000000010011101000" when "010111",
      "01000010100010101100100011111100111" when "011000",
      "01000101010100001001000101011100110" when "011001",
      "01001000000101100101100110111100101" when "011010",
      "01001010110111000010001000011100100" when "011011",
      "01001101101000011110101001111100011" when "011100",
      "01010000011001111011001011011100010" when "011101",
      "01010011001011010111101100111100001" when "011110",
      "01010101111100110100001110011100000" when "011111",
      "01011000101110010000101111111011111" when "100000",
      "01011011011111101101010001011011110" when "100001",
      "01011110010001001001110010111011101" when "100010",
      "01100001000010100110010100011011100" when "100011",
      "01100011110100000010110101111011011" when "100100",
      "01100110100101011111010111011011010" when "100101",
      "01101001010110111011111000111011001" when "100110",
      "01101100001000011000011010011011000" when "100111",
      "01101110111001110100111011111010111" when "101000",
      "01110001101011010001011101011010110" when "101001",
      "01110100011100101101111110111010101" when "101010",
      "01110111001110001010100000011010100" when "101011",
      "01111001111111100111000001111010011" when "101100",
      "01111100110001000011100011011010010" when "101101",
      "01111111100010100000000100111010001" when "101110",
      "10000010010011111100100110011010000" when "101111",
      "10000101000101011001000111111001111" when "110000",
      "10000111110110110101101001011001110" when "110001",
      "10001010101000010010001010111001101" when "110010",
      "10001101011001101110101100011001100" when "110011",
      "10010000001011001011001101111001011" when "110100",
      "10010010111100100111101111011001010" when "110101",
      "10010101101110000100010000111001001" when "110110",
      "10011000011111100000110010011001000" when "110111",
      "10011011010000111101010011111000111" when "111000",
      "10011110000010011001110101011000110" when "111001",
      "10100000110011110110010110111000101" when "111010",
      "10100011100101010010111000011000100" when "111011",
      "10100110010110101111011001111000011" when "111100",
      "10101001001000001011111011011000010" when "111101",
      "10101011111001101000011100111000001" when "111110",
      "10101110101011000100111110011000000" when "111111",
      "-----------------------------------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                 FixRealKCM_Freq100_uid20_T1_Freq100_uid26
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity FixRealKCM_Freq100_uid20_T1_Freq100_uid26 is
    port (X : in  std_logic_vector(1 downto 0);
          Y : out  std_logic_vector(28 downto 0)   );
end entity;

architecture arch of FixRealKCM_Freq100_uid20_T1_Freq100_uid26 is
signal Y0 :  std_logic_vector(28 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(28 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "00000000000000000000000000000" when "00",
      "00101100010111001000011000000" when "01",
      "01011000101110010000101111111" when "10",
      "10000101000101011001000111111" when "11",
      "-----------------------------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--          compressedTable_Freq100_uid37_subsampling_Freq100_uid39
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 1.248000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity compressedTable_Freq100_uid37_subsampling_Freq100_uid39 is
    port (X : in  std_logic_vector(6 downto 0);
          Y : out  std_logic_vector(8 downto 0)   );
end entity;

architecture arch of compressedTable_Freq100_uid37_subsampling_Freq100_uid39 is
signal Y0 :  std_logic_vector(8 downto 0);
   -- timing of Y0: (c0, 1.248000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(8 downto 0);
   -- timing of Y1: (c0, 1.248000ns)
begin
   with X  select  Y0 <= 
      "100000000" when "0000000",
      "100000010" when "0000001",
      "100000100" when "0000010",
      "100000110" when "0000011",
      "100001000" when "0000100",
      "100001010" when "0000101",
      "100001100" when "0000110",
      "100001110" when "0000111",
      "100010000" when "0001000",
      "100010010" when "0001001",
      "100010100" when "0001010",
      "100010110" when "0001011",
      "100011001" when "0001100",
      "100011011" when "0001101",
      "100011101" when "0001110",
      "100011111" when "0001111",
      "100100010" when "0010000",
      "100100100" when "0010001",
      "100100110" when "0010010",
      "100101000" when "0010011",
      "100101011" when "0010100",
      "100101101" when "0010101",
      "100110000" when "0010110",
      "100110010" when "0010111",
      "100110100" when "0011000",
      "100110111" when "0011001",
      "100111001" when "0011010",
      "100111100" when "0011011",
      "100111110" when "0011100",
      "101000001" when "0011101",
      "101000011" when "0011110",
      "101000110" when "0011111",
      "101001000" when "0100000",
      "101001011" when "0100001",
      "101001101" when "0100010",
      "101010000" when "0100011",
      "101010011" when "0100100",
      "101010101" when "0100101",
      "101011000" when "0100110",
      "101011011" when "0100111",
      "101011101" when "0101000",
      "101100000" when "0101001",
      "101100011" when "0101010",
      "101100110" when "0101011",
      "101101001" when "0101100",
      "101101011" when "0101101",
      "101101110" when "0101110",
      "101110001" when "0101111",
      "101110100" when "0110000",
      "101110111" when "0110001",
      "101111010" when "0110010",
      "101111101" when "0110011",
      "110000000" when "0110100",
      "110000011" when "0110101",
      "110000110" when "0110110",
      "110001001" when "0110111",
      "110001100" when "0111000",
      "110001111" when "0111001",
      "110010010" when "0111010",
      "110010101" when "0111011",
      "110011001" when "0111100",
      "110011100" when "0111101",
      "110011111" when "0111110",
      "110100010" when "0111111",
      "010011011" when "1000000",
      "010011100" when "1000001",
      "010011101" when "1000010",
      "010011110" when "1000011",
      "010100000" when "1000100",
      "010100001" when "1000101",
      "010100010" when "1000110",
      "010100011" when "1000111",
      "010100101" when "1001000",
      "010100110" when "1001001",
      "010100111" when "1001010",
      "010101001" when "1001011",
      "010101010" when "1001100",
      "010101011" when "1001101",
      "010101101" when "1001110",
      "010101110" when "1001111",
      "010101111" when "1010000",
      "010110001" when "1010001",
      "010110010" when "1010010",
      "010110100" when "1010011",
      "010110101" when "1010100",
      "010110110" when "1010101",
      "010111000" when "1010110",
      "010111001" when "1010111",
      "010111011" when "1011000",
      "010111100" when "1011001",
      "010111110" when "1011010",
      "010111111" when "1011011",
      "011000001" when "1011100",
      "011000010" when "1011101",
      "011000100" when "1011110",
      "011000101" when "1011111",
      "011000111" when "1100000",
      "011001000" when "1100001",
      "011001010" when "1100010",
      "011001100" when "1100011",
      "011001101" when "1100100",
      "011001111" when "1100101",
      "011010000" when "1100110",
      "011010010" when "1100111",
      "011010100" when "1101000",
      "011010101" when "1101001",
      "011010111" when "1101010",
      "011011001" when "1101011",
      "011011010" when "1101100",
      "011011100" when "1101101",
      "011011110" when "1101110",
      "011100000" when "1101111",
      "011100001" when "1110000",
      "011100011" when "1110001",
      "011100101" when "1110010",
      "011100111" when "1110011",
      "011101001" when "1110100",
      "011101010" when "1110101",
      "011101100" when "1110110",
      "011101110" when "1110111",
      "011110000" when "1111000",
      "011110010" when "1111001",
      "011110100" when "1111010",
      "011110110" when "1111011",
      "011111000" when "1111100",
      "011111010" when "1111101",
      "011111100" when "1111110",
      "011111110" when "1111111",
      "---------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                      FixFunctionByTable_Freq100_uid44
-- Evaluator for exp(x*1b-10)-1-x*1b-10 on [0,1) for lsbIn=-7 (wIn=7), msbout=-22, lsbOut=-27 (wOut=6). Out interval: [0; 4.69567e-07]. Output is unsigned

-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2010-2018)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 1.248000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity FixFunctionByTable_Freq100_uid44 is
    port (X : in  std_logic_vector(6 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of FixFunctionByTable_Freq100_uid44 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 1.248000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 1.248000ns)
begin
   with X  select  Y0 <= 
      "000000" when "0000000",
      "000000" when "0000001",
      "000000" when "0000010",
      "000000" when "0000011",
      "000000" when "0000100",
      "000000" when "0000101",
      "000000" when "0000110",
      "000000" when "0000111",
      "000000" when "0001000",
      "000000" when "0001001",
      "000000" when "0001010",
      "000000" when "0001011",
      "000001" when "0001100",
      "000001" when "0001101",
      "000001" when "0001110",
      "000001" when "0001111",
      "000001" when "0010000",
      "000001" when "0010001",
      "000001" when "0010010",
      "000001" when "0010011",
      "000010" when "0010100",
      "000010" when "0010101",
      "000010" when "0010110",
      "000010" when "0010111",
      "000010" when "0011000",
      "000010" when "0011001",
      "000011" when "0011010",
      "000011" when "0011011",
      "000011" when "0011100",
      "000011" when "0011101",
      "000100" when "0011110",
      "000100" when "0011111",
      "000100" when "0100000",
      "000100" when "0100001",
      "000101" when "0100010",
      "000101" when "0100011",
      "000101" when "0100100",
      "000101" when "0100101",
      "000110" when "0100110",
      "000110" when "0100111",
      "000110" when "0101000",
      "000111" when "0101001",
      "000111" when "0101010",
      "000111" when "0101011",
      "001000" when "0101100",
      "001000" when "0101101",
      "001000" when "0101110",
      "001001" when "0101111",
      "001001" when "0110000",
      "001001" when "0110001",
      "001010" when "0110010",
      "001010" when "0110011",
      "001011" when "0110100",
      "001011" when "0110101",
      "001011" when "0110110",
      "001100" when "0110111",
      "001100" when "0111000",
      "001101" when "0111001",
      "001101" when "0111010",
      "001110" when "0111011",
      "001110" when "0111100",
      "001111" when "0111101",
      "001111" when "0111110",
      "010000" when "0111111",
      "010000" when "1000000",
      "010001" when "1000001",
      "010001" when "1000010",
      "010010" when "1000011",
      "010010" when "1000100",
      "010011" when "1000101",
      "010011" when "1000110",
      "010100" when "1000111",
      "010100" when "1001000",
      "010101" when "1001001",
      "010101" when "1001010",
      "010110" when "1001011",
      "010111" when "1001100",
      "010111" when "1001101",
      "011000" when "1001110",
      "011000" when "1001111",
      "011001" when "1010000",
      "011010" when "1010001",
      "011010" when "1010010",
      "011011" when "1010011",
      "011100" when "1010100",
      "011100" when "1010101",
      "011101" when "1010110",
      "011110" when "1010111",
      "011110" when "1011000",
      "011111" when "1011001",
      "100000" when "1011010",
      "100000" when "1011011",
      "100001" when "1011100",
      "100010" when "1011101",
      "100011" when "1011110",
      "100011" when "1011111",
      "100100" when "1100000",
      "100101" when "1100001",
      "100110" when "1100010",
      "100110" when "1100011",
      "100111" when "1100100",
      "101000" when "1100101",
      "101001" when "1100110",
      "101001" when "1100111",
      "101010" when "1101000",
      "101011" when "1101001",
      "101100" when "1101010",
      "101101" when "1101011",
      "101110" when "1101100",
      "101110" when "1101101",
      "101111" when "1101110",
      "110000" when "1101111",
      "110001" when "1110000",
      "110010" when "1110001",
      "110011" when "1110010",
      "110100" when "1110011",
      "110101" when "1110100",
      "110101" when "1110101",
      "110110" when "1110110",
      "110111" when "1110111",
      "111000" when "1111000",
      "111001" when "1111001",
      "111010" when "1111010",
      "111011" when "1111011",
      "111100" when "1111100",
      "111101" when "1111101",
      "111110" when "1111110",
      "111111" when "1111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid60
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid60 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid60 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid65
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid65 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid65 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid70
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid70 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid70 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid75
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid75 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid75 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid80
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid80 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid80 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid85
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid85 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid85 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid90
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid90 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid90 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid95
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid95 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid95 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid100
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid100 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid100 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid105
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid105 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid105 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid110
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid110 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid110 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid115
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid115 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid115 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid120
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid120 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid120 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid125
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid125 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid125 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid130
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid130 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid130 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid135
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid135 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid135 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid140
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid140 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid140 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid145
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid145 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid145 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid150
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid150 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid150 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid155
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid155 is
    port (X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid155 is
signal Y0 :  std_logic_vector(5 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "000000" when "000000",
      "000000" when "000001",
      "000000" when "000010",
      "000000" when "000011",
      "000000" when "000100",
      "000000" when "000101",
      "000000" when "000110",
      "000000" when "000111",
      "000000" when "001000",
      "000001" when "001001",
      "000010" when "001010",
      "000011" when "001011",
      "000100" when "001100",
      "000101" when "001101",
      "000110" when "001110",
      "000111" when "001111",
      "000000" when "010000",
      "000010" when "010001",
      "000100" when "010010",
      "000110" when "010011",
      "001000" when "010100",
      "001010" when "010101",
      "001100" when "010110",
      "001110" when "010111",
      "000000" when "011000",
      "000011" when "011001",
      "000110" when "011010",
      "001001" when "011011",
      "001100" when "011100",
      "001111" when "011101",
      "010010" when "011110",
      "010101" when "011111",
      "000000" when "100000",
      "000100" when "100001",
      "001000" when "100010",
      "001100" when "100011",
      "010000" when "100100",
      "010100" when "100101",
      "011000" when "100110",
      "011100" when "100111",
      "000000" when "101000",
      "000101" when "101001",
      "001010" when "101010",
      "001111" when "101011",
      "010100" when "101100",
      "011001" when "101101",
      "011110" when "101110",
      "100011" when "101111",
      "000000" when "110000",
      "000110" when "110001",
      "001100" when "110010",
      "010010" when "110011",
      "011000" when "110100",
      "011110" when "110101",
      "100100" when "110110",
      "101010" when "110111",
      "000000" when "111000",
      "000111" when "111001",
      "001110" when "111010",
      "010101" when "111011",
      "011100" when "111100",
      "100011" when "111101",
      "101010" when "111110",
      "110001" when "111111",
      "------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid160
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid160 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid160 is
signal Y0 :  std_logic_vector(4 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00001" when "00101",
      "00010" when "00110",
      "00011" when "00111",
      "00000" when "01000",
      "00010" when "01001",
      "00100" when "01010",
      "00110" when "01011",
      "00000" when "01100",
      "00011" when "01101",
      "00110" when "01110",
      "01001" when "01111",
      "00000" when "10000",
      "00100" when "10001",
      "01000" when "10010",
      "01100" when "10011",
      "00000" when "10100",
      "00101" when "10101",
      "01010" when "10110",
      "01111" when "10111",
      "00000" when "11000",
      "00110" when "11001",
      "01100" when "11010",
      "10010" when "11011",
      "00000" when "11100",
      "00111" when "11101",
      "01110" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid167
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid167 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid167 is
signal Y0 :  std_logic_vector(4 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00001" when "00101",
      "00010" when "00110",
      "00011" when "00111",
      "00000" when "01000",
      "00010" when "01001",
      "00100" when "01010",
      "00110" when "01011",
      "00000" when "01100",
      "00011" when "01101",
      "00110" when "01110",
      "01001" when "01111",
      "00000" when "10000",
      "00100" when "10001",
      "01000" when "10010",
      "01100" when "10011",
      "00000" when "10100",
      "00101" when "10101",
      "01010" when "10110",
      "01111" when "10111",
      "00000" when "11000",
      "00110" when "11001",
      "01100" when "11010",
      "10010" when "11011",
      "00000" when "11100",
      "00111" when "11101",
      "01110" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid172
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid172 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid172 is
signal Y0 :  std_logic_vector(4 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00001" when "00101",
      "00010" when "00110",
      "00011" when "00111",
      "00000" when "01000",
      "00010" when "01001",
      "00100" when "01010",
      "00110" when "01011",
      "00000" when "01100",
      "00011" when "01101",
      "00110" when "01110",
      "01001" when "01111",
      "00000" when "10000",
      "00100" when "10001",
      "01000" when "10010",
      "01100" when "10011",
      "00000" when "10100",
      "00101" when "10101",
      "01010" when "10110",
      "01111" when "10111",
      "00000" when "11000",
      "00110" when "11001",
      "01100" when "11010",
      "10010" when "11011",
      "00000" when "11100",
      "00111" when "11101",
      "01110" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid177
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid177 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid177 is
signal Y0 :  std_logic_vector(4 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00001" when "00101",
      "00010" when "00110",
      "00011" when "00111",
      "00000" when "01000",
      "00010" when "01001",
      "00100" when "01010",
      "00110" when "01011",
      "00000" when "01100",
      "00011" when "01101",
      "00110" when "01110",
      "01001" when "01111",
      "00000" when "10000",
      "00100" when "10001",
      "01000" when "10010",
      "01100" when "10011",
      "00000" when "10100",
      "00101" when "10101",
      "01010" when "10110",
      "01111" when "10111",
      "00000" when "11000",
      "00110" when "11001",
      "01100" when "11010",
      "10010" when "11011",
      "00000" when "11100",
      "00111" when "11101",
      "01110" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq100_uid182
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: Y: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq100_uid182 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq100_uid182 is
signal Y0 :  std_logic_vector(4 downto 0);
   -- timing of Y0: (c0, 0.624000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
   -- timing of Y1: (c0, 0.624000ns)
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00001" when "00101",
      "00010" when "00110",
      "00011" when "00111",
      "00000" when "01000",
      "00010" when "01001",
      "00100" when "01010",
      "00110" when "01011",
      "00000" when "01100",
      "00011" when "01101",
      "00110" when "01110",
      "01001" when "01111",
      "00000" when "10000",
      "00100" when "10001",
      "01000" when "10010",
      "01100" when "10011",
      "00000" when "10100",
      "00101" when "10101",
      "01010" when "10110",
      "01111" when "10111",
      "00000" when "11000",
      "00110" when "11001",
      "01100" when "11010",
      "10010" when "11011",
      "00000" when "11100",
      "00111" when "11101",
      "01110" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                       Compressor_6_3_Freq100_uid204
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X0
-- Output signals: R
--  approx. input signal timings: X0: (c0, 0.000000ns)
--  approx. output signal timings: R: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_6_3_Freq100_uid204 is
    port (X0 : in  std_logic_vector(5 downto 0);
          R : out  std_logic_vector(2 downto 0)   );
end entity;

architecture arch of Compressor_6_3_Freq100_uid204 is
signal X :  std_logic_vector(5 downto 0);
   -- timing of X: (c0, 0.000000ns)
signal R0 :  std_logic_vector(2 downto 0);
   -- timing of R0: (c0, 0.624000ns)
begin
   X <= X0 ;

   with X  select  R0 <= 
      "000" when "000000",
      "001" when "000001" | "000010" | "000100" | "001000" | "010000" | "100000",
      "010" when "000011" | "000101" | "000110" | "001001" | "001010" | "001100" | "010001" | "010010" | "010100" | "011000" | "100001" | "100010" | "100100" | "101000" | "110000",
      "011" when "000111" | "001011" | "001101" | "001110" | "010011" | "010101" | "010110" | "011001" | "011010" | "011100" | "100011" | "100101" | "100110" | "101001" | "101010" | "101100" | "110001" | "110010" | "110100" | "111000",
      "100" when "001111" | "010111" | "011011" | "011101" | "011110" | "100111" | "101011" | "101101" | "101110" | "110011" | "110101" | "110110" | "111001" | "111010" | "111100",
      "101" when "011111" | "101111" | "110111" | "111011" | "111101" | "111110",
      "110" when "111111",
      "---" when others;
   R <= R0;
end architecture;

--------------------------------------------------------------------------------
--                       Compressor_14_3_Freq100_uid208
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X1 X0
-- Output signals: R
--  approx. input signal timings: X1: (c0, 0.000000ns)X0: (c0, 0.000000ns)
--  approx. output signal timings: R: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_14_3_Freq100_uid208 is
    port (X1 : in  std_logic_vector(0 downto 0);
          X0 : in  std_logic_vector(3 downto 0);
          R : out  std_logic_vector(2 downto 0)   );
end entity;

architecture arch of Compressor_14_3_Freq100_uid208 is
signal X :  std_logic_vector(4 downto 0);
   -- timing of X: (c0, 0.000000ns)
signal R0 :  std_logic_vector(2 downto 0);
   -- timing of R0: (c0, 0.624000ns)
begin
   X <= X1 & X0 ;

   with X  select  R0 <= 
      "000" when "00000",
      "001" when "00001" | "00010" | "00100" | "01000",
      "010" when "00011" | "00101" | "00110" | "01001" | "01010" | "01100" | "10000",
      "011" when "00111" | "01011" | "01101" | "01110" | "10001" | "10010" | "10100" | "11000",
      "100" when "01111" | "10011" | "10101" | "10110" | "11001" | "11010" | "11100",
      "101" when "10111" | "11011" | "11101" | "11110",
      "110" when "11111",
      "---" when others;
   R <= R0;
end architecture;

--------------------------------------------------------------------------------
--                       Compressor_3_2_Freq100_uid214
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X0
-- Output signals: R
--  approx. input signal timings: X0: (c0, 0.000000ns)
--  approx. output signal timings: R: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_3_2_Freq100_uid214 is
    port (X0 : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of Compressor_3_2_Freq100_uid214 is
signal X :  std_logic_vector(2 downto 0);
   -- timing of X: (c0, 0.000000ns)
signal R0 :  std_logic_vector(1 downto 0);
   -- timing of R0: (c0, 0.624000ns)
begin
   X <= X0 ;

   with X  select  R0 <= 
      "00" when "000",
      "01" when "001" | "010" | "100",
      "10" when "011" | "101" | "110",
      "11" when "111",
      "--" when others;
   R <= R0;
end architecture;

--------------------------------------------------------------------------------
--                       Compressor_23_3_Freq100_uid248
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X1 X0
-- Output signals: R
--  approx. input signal timings: X1: (c0, 0.000000ns)X0: (c0, 0.000000ns)
--  approx. output signal timings: R: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_23_3_Freq100_uid248 is
    port (X1 : in  std_logic_vector(1 downto 0);
          X0 : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(2 downto 0)   );
end entity;

architecture arch of Compressor_23_3_Freq100_uid248 is
signal X :  std_logic_vector(4 downto 0);
   -- timing of X: (c0, 0.000000ns)
signal R0 :  std_logic_vector(2 downto 0);
   -- timing of R0: (c0, 0.624000ns)
begin
   X <= X1 & X0 ;

   with X  select  R0 <= 
      "000" when "00000",
      "001" when "00001" | "00010" | "00100",
      "010" when "00011" | "00101" | "00110" | "01000" | "10000",
      "011" when "00111" | "01001" | "01010" | "01100" | "10001" | "10010" | "10100",
      "100" when "01011" | "01101" | "01110" | "10011" | "10101" | "10110" | "11000",
      "101" when "01111" | "10111" | "11001" | "11010" | "11100",
      "110" when "11011" | "11101" | "11110",
      "111" when "11111",
      "---" when others;
   R <= R0;
end architecture;

--------------------------------------------------------------------------------
--                       Compressor_5_3_Freq100_uid272
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X0
-- Output signals: R
--  approx. input signal timings: X0: (c0, 0.000000ns)
--  approx. output signal timings: R: (c0, 0.624000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_5_3_Freq100_uid272 is
    port (X0 : in  std_logic_vector(4 downto 0);
          R : out  std_logic_vector(2 downto 0)   );
end entity;

architecture arch of Compressor_5_3_Freq100_uid272 is
signal X :  std_logic_vector(4 downto 0);
   -- timing of X: (c0, 0.000000ns)
signal R0 :  std_logic_vector(2 downto 0);
   -- timing of R0: (c0, 0.624000ns)
begin
   X <= X0 ;

   with X  select  R0 <= 
      "000" when "00000",
      "001" when "00001" | "00010" | "00100" | "01000" | "10000",
      "010" when "00011" | "00101" | "00110" | "01001" | "01010" | "01100" | "10001" | "10010" | "10100" | "11000",
      "011" when "00111" | "01011" | "01101" | "01110" | "10011" | "10101" | "10110" | "11001" | "11010" | "11100",
      "100" when "01111" | "10111" | "11011" | "11101" | "11110",
      "101" when "11111",
      "---" when others;
   R <= R0;
end architecture;

--------------------------------------------------------------------------------
--                    LeftShifter24_by_max_33_Freq100_uid4
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca (2008-2011), Florent de Dinechin (2008-2019)
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X S
-- Output signals: R
--  approx. input signal timings: X: (c0, 1.368000ns)S: (c0, 2.736000ns)
--  approx. output signal timings: R: (c0, 6.138000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity LeftShifter24_by_max_33_Freq100_uid4 is
    port (clk : in std_logic;
          X : in  std_logic_vector(23 downto 0);
          S : in  std_logic_vector(5 downto 0);
          R : out  std_logic_vector(56 downto 0)   );
end entity;

architecture arch of LeftShifter24_by_max_33_Freq100_uid4 is
signal ps :  std_logic_vector(5 downto 0);
   -- timing of ps: (c0, 2.736000ns)
signal level0 :  std_logic_vector(23 downto 0);
   -- timing of level0: (c0, 1.368000ns)
signal level1 :  std_logic_vector(24 downto 0);
   -- timing of level1: (c0, 2.736000ns)
signal level2 :  std_logic_vector(26 downto 0);
   -- timing of level2: (c0, 3.630000ns)
signal level3 :  std_logic_vector(30 downto 0);
   -- timing of level3: (c0, 3.630000ns)
signal level4 :  std_logic_vector(38 downto 0);
   -- timing of level4: (c0, 4.644000ns)
signal level5 :  std_logic_vector(54 downto 0);
   -- timing of level5: (c0, 4.644000ns)
signal level6 :  std_logic_vector(86 downto 0);
   -- timing of level6: (c0, 6.138000ns)
begin
   ps<= S;
   level0<= X;
   level1<= level0 & (0 downto 0 => '0') when ps(0)= '1' else     (0 downto 0 => '0') & level0;
   level2<= level1 & (1 downto 0 => '0') when ps(1)= '1' else     (1 downto 0 => '0') & level1;
   level3<= level2 & (3 downto 0 => '0') when ps(2)= '1' else     (3 downto 0 => '0') & level2;
   level4<= level3 & (7 downto 0 => '0') when ps(3)= '1' else     (7 downto 0 => '0') & level3;
   level5<= level4 & (15 downto 0 => '0') when ps(4)= '1' else     (15 downto 0 => '0') & level4;
   level6<= level5 & (31 downto 0 => '0') when ps(5)= '1' else     (31 downto 0 => '0') & level5;
   R <= level6(56 downto 0);
end architecture;

--------------------------------------------------------------------------------
--                         IntAdder_13_Freq100_uid18
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y Cin
-- Output signals: R
--  approx. input signal timings: X: (c0, 6.762000ns)Y: (c0, 6.762000ns)Cin: (c0, 0.000000ns)
--  approx. output signal timings: R: (c0, 8.244000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_13_Freq100_uid18 is
    port (clk : in std_logic;
          X : in  std_logic_vector(12 downto 0);
          Y : in  std_logic_vector(12 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(12 downto 0)   );
end entity;

architecture arch of IntAdder_13_Freq100_uid18 is
signal Rtmp :  std_logic_vector(12 downto 0);
   -- timing of Rtmp: (c0, 8.244000ns)
begin
   Rtmp <= X + Y + Cin;
   R <= Rtmp;
end architecture;

--------------------------------------------------------------------------------
--                          FixRealKCM_Freq100_uid8
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: R
--  approx. input signal timings: X: (c0, 6.138000ns)
--  approx. output signal timings: R: (c0, 8.244000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity FixRealKCM_Freq100_uid8 is
    port (clk : in std_logic;
          X : in  std_logic_vector(9 downto 0);
          R : out  std_logic_vector(7 downto 0)   );
end entity;

architecture arch of FixRealKCM_Freq100_uid8 is
   component FixRealKCM_Freq100_uid8_T0_Freq100_uid11 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(11 downto 0)   );
   end component;

   component FixRealKCM_Freq100_uid8_T1_Freq100_uid14 is
      port ( X : in  std_logic_vector(3 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntAdder_13_Freq100_uid18 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(12 downto 0);
             Y : in  std_logic_vector(12 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(12 downto 0)   );
   end component;

signal FixRealKCM_Freq100_uid8_A0 :  std_logic_vector(5 downto 0);
   -- timing of FixRealKCM_Freq100_uid8_A0: (c0, 6.138000ns)
signal FixRealKCM_Freq100_uid8_T0 :  std_logic_vector(11 downto 0);
   -- timing of FixRealKCM_Freq100_uid8_T0: (c0, 6.762000ns)
signal FixRealKCM_Freq100_uid8_T0_copy12 :  std_logic_vector(11 downto 0);
   -- timing of FixRealKCM_Freq100_uid8_T0_copy12: (c0, 6.138000ns)
signal bh9_w0_0 :  std_logic;
   -- timing of bh9_w0_0: (c0, 6.762000ns)
signal bh9_w1_0 :  std_logic;
   -- timing of bh9_w1_0: (c0, 6.762000ns)
signal bh9_w2_0 :  std_logic;
   -- timing of bh9_w2_0: (c0, 6.762000ns)
signal bh9_w3_0 :  std_logic;
   -- timing of bh9_w3_0: (c0, 6.762000ns)
signal bh9_w4_0 :  std_logic;
   -- timing of bh9_w4_0: (c0, 6.762000ns)
signal bh9_w5_0 :  std_logic;
   -- timing of bh9_w5_0: (c0, 6.762000ns)
signal bh9_w6_0 :  std_logic;
   -- timing of bh9_w6_0: (c0, 6.762000ns)
signal bh9_w7_0 :  std_logic;
   -- timing of bh9_w7_0: (c0, 6.762000ns)
signal bh9_w8_0 :  std_logic;
   -- timing of bh9_w8_0: (c0, 6.762000ns)
signal bh9_w9_0 :  std_logic;
   -- timing of bh9_w9_0: (c0, 6.762000ns)
signal bh9_w10_0 :  std_logic;
   -- timing of bh9_w10_0: (c0, 6.762000ns)
signal bh9_w11_0 :  std_logic;
   -- timing of bh9_w11_0: (c0, 6.762000ns)
signal FixRealKCM_Freq100_uid8_A1 :  std_logic_vector(3 downto 0);
   -- timing of FixRealKCM_Freq100_uid8_A1: (c0, 6.138000ns)
signal FixRealKCM_Freq100_uid8_T1 :  std_logic_vector(5 downto 0);
   -- timing of FixRealKCM_Freq100_uid8_T1: (c0, 6.762000ns)
signal FixRealKCM_Freq100_uid8_T1_copy15 :  std_logic_vector(5 downto 0);
   -- timing of FixRealKCM_Freq100_uid8_T1_copy15: (c0, 6.138000ns)
signal bh9_w0_1 :  std_logic;
   -- timing of bh9_w0_1: (c0, 6.762000ns)
signal bh9_w1_1 :  std_logic;
   -- timing of bh9_w1_1: (c0, 6.762000ns)
signal bh9_w2_1 :  std_logic;
   -- timing of bh9_w2_1: (c0, 6.762000ns)
signal bh9_w3_1 :  std_logic;
   -- timing of bh9_w3_1: (c0, 6.762000ns)
signal bh9_w4_1 :  std_logic;
   -- timing of bh9_w4_1: (c0, 6.762000ns)
signal bh9_w5_1 :  std_logic;
   -- timing of bh9_w5_1: (c0, 6.762000ns)
signal bitheapFinalAdd_bh9_In0 :  std_logic_vector(12 downto 0);
   -- timing of bitheapFinalAdd_bh9_In0: (c0, 6.762000ns)
signal bitheapFinalAdd_bh9_In1 :  std_logic_vector(12 downto 0);
   -- timing of bitheapFinalAdd_bh9_In1: (c0, 6.762000ns)
signal bitheapFinalAdd_bh9_Cin :  std_logic;
   -- timing of bitheapFinalAdd_bh9_Cin: (c0, 0.000000ns)
signal bitheapFinalAdd_bh9_Out :  std_logic_vector(12 downto 0);
   -- timing of bitheapFinalAdd_bh9_Out: (c0, 8.244000ns)
signal bitheapResult_bh9 :  std_logic_vector(11 downto 0);
   -- timing of bitheapResult_bh9: (c0, 8.244000ns)
signal OutRes :  std_logic_vector(11 downto 0);
   -- timing of OutRes: (c0, 8.244000ns)
begin
-- This operator multiplies by 1/log(2)
   FixRealKCM_Freq100_uid8_A0 <= X(9 downto 4);-- input address  m=6  l=1
   FixRealKCM_Freq100_uid8_Table0: FixRealKCM_Freq100_uid8_T0_Freq100_uid11
      port map ( X => FixRealKCM_Freq100_uid8_A0,
                 Y => FixRealKCM_Freq100_uid8_T0_copy12);
   FixRealKCM_Freq100_uid8_T0 <= FixRealKCM_Freq100_uid8_T0_copy12; -- output copy to hold a pipeline register if needed
   bh9_w0_0 <= FixRealKCM_Freq100_uid8_T0(0);
   bh9_w1_0 <= FixRealKCM_Freq100_uid8_T0(1);
   bh9_w2_0 <= FixRealKCM_Freq100_uid8_T0(2);
   bh9_w3_0 <= FixRealKCM_Freq100_uid8_T0(3);
   bh9_w4_0 <= FixRealKCM_Freq100_uid8_T0(4);
   bh9_w5_0 <= FixRealKCM_Freq100_uid8_T0(5);
   bh9_w6_0 <= FixRealKCM_Freq100_uid8_T0(6);
   bh9_w7_0 <= FixRealKCM_Freq100_uid8_T0(7);
   bh9_w8_0 <= FixRealKCM_Freq100_uid8_T0(8);
   bh9_w9_0 <= FixRealKCM_Freq100_uid8_T0(9);
   bh9_w10_0 <= FixRealKCM_Freq100_uid8_T0(10);
   bh9_w11_0 <= FixRealKCM_Freq100_uid8_T0(11);
   FixRealKCM_Freq100_uid8_A1 <= X(3 downto 0);-- input address  m=0  l=-3
   FixRealKCM_Freq100_uid8_Table1: FixRealKCM_Freq100_uid8_T1_Freq100_uid14
      port map ( X => FixRealKCM_Freq100_uid8_A1,
                 Y => FixRealKCM_Freq100_uid8_T1_copy15);
   FixRealKCM_Freq100_uid8_T1 <= FixRealKCM_Freq100_uid8_T1_copy15; -- output copy to hold a pipeline register if needed
   bh9_w0_1 <= FixRealKCM_Freq100_uid8_T1(0);
   bh9_w1_1 <= FixRealKCM_Freq100_uid8_T1(1);
   bh9_w2_1 <= FixRealKCM_Freq100_uid8_T1(2);
   bh9_w3_1 <= FixRealKCM_Freq100_uid8_T1(3);
   bh9_w4_1 <= FixRealKCM_Freq100_uid8_T1(4);
   bh9_w5_1 <= FixRealKCM_Freq100_uid8_T1(5);

   -- Adding the constant bits 
      -- All the constant bits are zero, nothing to add


   bitheapFinalAdd_bh9_In0 <= "0" & bh9_w11_0 & bh9_w10_0 & bh9_w9_0 & bh9_w8_0 & bh9_w7_0 & bh9_w6_0 & bh9_w5_1 & bh9_w4_1 & bh9_w3_1 & bh9_w2_1 & bh9_w1_1 & bh9_w0_1;
   bitheapFinalAdd_bh9_In1 <= "0" & "0" & "0" & "0" & "0" & "0" & "0" & bh9_w5_0 & bh9_w4_0 & bh9_w3_0 & bh9_w2_0 & bh9_w1_0 & bh9_w0_0;
   bitheapFinalAdd_bh9_Cin <= '0';

   bitheapFinalAdd_bh9: IntAdder_13_Freq100_uid18
      port map ( clk  => clk,
                 Cin => bitheapFinalAdd_bh9_Cin,
                 X => bitheapFinalAdd_bh9_In0,
                 Y => bitheapFinalAdd_bh9_In1,
                 R => bitheapFinalAdd_bh9_Out);
   bitheapResult_bh9 <= bitheapFinalAdd_bh9_Out(11 downto 0);
   OutRes <= bitheapResult_bh9(11 downto 0);
   R <= OutRes(11 downto 4);
end architecture;

--------------------------------------------------------------------------------
--                         IntAdder_36_Freq100_uid30
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 1 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y Cin
-- Output signals: R
--  approx. input signal timings: X: (c0, 8.868000ns)Y: (c0, 8.868000ns)Cin: (c0, 0.000000ns)
--  approx. output signal timings: R: (c1, 1.552000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_36_Freq100_uid30 is
    port (clk : in std_logic;
          X : in  std_logic_vector(35 downto 0);
          Y : in  std_logic_vector(35 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(35 downto 0)   );
end entity;

architecture arch of IntAdder_36_Freq100_uid30 is
signal Rtmp :  std_logic_vector(35 downto 0);
   -- timing of Rtmp: (c1, 1.552000ns)
signal X_d1 :  std_logic_vector(35 downto 0);
   -- timing of X: (c0, 8.868000ns)
signal Y_d1 :  std_logic_vector(35 downto 0);
   -- timing of Y: (c0, 8.868000ns)
signal Cin_d1 :  std_logic;
   -- timing of Cin: (c0, 0.000000ns)
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            X_d1 <=  X;
            Y_d1 <=  Y;
            Cin_d1 <=  Cin;
         end if;
      end process;
   Rtmp <= X_d1 + Y_d1 + Cin_d1;
   R <= Rtmp;
end architecture;

--------------------------------------------------------------------------------
--                          FixRealKCM_Freq100_uid20
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 1 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: R
--  approx. input signal timings: X: (c0, 8.244000ns)
--  approx. output signal timings: R: (c1, 1.552000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity FixRealKCM_Freq100_uid20 is
    port (clk : in std_logic;
          X : in  std_logic_vector(7 downto 0);
          R : out  std_logic_vector(34 downto 0)   );
end entity;

architecture arch of FixRealKCM_Freq100_uid20 is
   component FixRealKCM_Freq100_uid20_T0_Freq100_uid23 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(34 downto 0)   );
   end component;

   component FixRealKCM_Freq100_uid20_T1_Freq100_uid26 is
      port ( X : in  std_logic_vector(1 downto 0);
             Y : out  std_logic_vector(28 downto 0)   );
   end component;

   component IntAdder_36_Freq100_uid30 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(35 downto 0);
             Y : in  std_logic_vector(35 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(35 downto 0)   );
   end component;

signal FixRealKCM_Freq100_uid20_A0 :  std_logic_vector(5 downto 0);
   -- timing of FixRealKCM_Freq100_uid20_A0: (c0, 8.244000ns)
signal FixRealKCM_Freq100_uid20_T0 :  std_logic_vector(34 downto 0);
   -- timing of FixRealKCM_Freq100_uid20_T0: (c0, 8.868000ns)
signal FixRealKCM_Freq100_uid20_T0_copy24 :  std_logic_vector(34 downto 0);
   -- timing of FixRealKCM_Freq100_uid20_T0_copy24: (c0, 8.244000ns)
signal bh21_w0_0 :  std_logic;
   -- timing of bh21_w0_0: (c0, 8.868000ns)
signal bh21_w1_0 :  std_logic;
   -- timing of bh21_w1_0: (c0, 8.868000ns)
signal bh21_w2_0 :  std_logic;
   -- timing of bh21_w2_0: (c0, 8.868000ns)
signal bh21_w3_0 :  std_logic;
   -- timing of bh21_w3_0: (c0, 8.868000ns)
signal bh21_w4_0 :  std_logic;
   -- timing of bh21_w4_0: (c0, 8.868000ns)
signal bh21_w5_0 :  std_logic;
   -- timing of bh21_w5_0: (c0, 8.868000ns)
signal bh21_w6_0 :  std_logic;
   -- timing of bh21_w6_0: (c0, 8.868000ns)
signal bh21_w7_0 :  std_logic;
   -- timing of bh21_w7_0: (c0, 8.868000ns)
signal bh21_w8_0 :  std_logic;
   -- timing of bh21_w8_0: (c0, 8.868000ns)
signal bh21_w9_0 :  std_logic;
   -- timing of bh21_w9_0: (c0, 8.868000ns)
signal bh21_w10_0 :  std_logic;
   -- timing of bh21_w10_0: (c0, 8.868000ns)
signal bh21_w11_0 :  std_logic;
   -- timing of bh21_w11_0: (c0, 8.868000ns)
signal bh21_w12_0 :  std_logic;
   -- timing of bh21_w12_0: (c0, 8.868000ns)
signal bh21_w13_0 :  std_logic;
   -- timing of bh21_w13_0: (c0, 8.868000ns)
signal bh21_w14_0 :  std_logic;
   -- timing of bh21_w14_0: (c0, 8.868000ns)
signal bh21_w15_0 :  std_logic;
   -- timing of bh21_w15_0: (c0, 8.868000ns)
signal bh21_w16_0 :  std_logic;
   -- timing of bh21_w16_0: (c0, 8.868000ns)
signal bh21_w17_0 :  std_logic;
   -- timing of bh21_w17_0: (c0, 8.868000ns)
signal bh21_w18_0 :  std_logic;
   -- timing of bh21_w18_0: (c0, 8.868000ns)
signal bh21_w19_0 :  std_logic;
   -- timing of bh21_w19_0: (c0, 8.868000ns)
signal bh21_w20_0 :  std_logic;
   -- timing of bh21_w20_0: (c0, 8.868000ns)
signal bh21_w21_0 :  std_logic;
   -- timing of bh21_w21_0: (c0, 8.868000ns)
signal bh21_w22_0 :  std_logic;
   -- timing of bh21_w22_0: (c0, 8.868000ns)
signal bh21_w23_0 :  std_logic;
   -- timing of bh21_w23_0: (c0, 8.868000ns)
signal bh21_w24_0 :  std_logic;
   -- timing of bh21_w24_0: (c0, 8.868000ns)
signal bh21_w25_0 :  std_logic;
   -- timing of bh21_w25_0: (c0, 8.868000ns)
signal bh21_w26_0 :  std_logic;
   -- timing of bh21_w26_0: (c0, 8.868000ns)
signal bh21_w27_0 :  std_logic;
   -- timing of bh21_w27_0: (c0, 8.868000ns)
signal bh21_w28_0 :  std_logic;
   -- timing of bh21_w28_0: (c0, 8.868000ns)
signal bh21_w29_0 :  std_logic;
   -- timing of bh21_w29_0: (c0, 8.868000ns)
signal bh21_w30_0 :  std_logic;
   -- timing of bh21_w30_0: (c0, 8.868000ns)
signal bh21_w31_0 :  std_logic;
   -- timing of bh21_w31_0: (c0, 8.868000ns)
signal bh21_w32_0 :  std_logic;
   -- timing of bh21_w32_0: (c0, 8.868000ns)
signal bh21_w33_0 :  std_logic;
   -- timing of bh21_w33_0: (c0, 8.868000ns)
signal bh21_w34_0 :  std_logic;
   -- timing of bh21_w34_0: (c0, 8.868000ns)
signal FixRealKCM_Freq100_uid20_A1 :  std_logic_vector(1 downto 0);
   -- timing of FixRealKCM_Freq100_uid20_A1: (c0, 8.244000ns)
signal FixRealKCM_Freq100_uid20_T1 :  std_logic_vector(28 downto 0);
   -- timing of FixRealKCM_Freq100_uid20_T1: (c0, 8.868000ns)
signal FixRealKCM_Freq100_uid20_T1_copy27 :  std_logic_vector(28 downto 0);
   -- timing of FixRealKCM_Freq100_uid20_T1_copy27: (c0, 8.244000ns)
signal bh21_w0_1 :  std_logic;
   -- timing of bh21_w0_1: (c0, 8.868000ns)
signal bh21_w1_1 :  std_logic;
   -- timing of bh21_w1_1: (c0, 8.868000ns)
signal bh21_w2_1 :  std_logic;
   -- timing of bh21_w2_1: (c0, 8.868000ns)
signal bh21_w3_1 :  std_logic;
   -- timing of bh21_w3_1: (c0, 8.868000ns)
signal bh21_w4_1 :  std_logic;
   -- timing of bh21_w4_1: (c0, 8.868000ns)
signal bh21_w5_1 :  std_logic;
   -- timing of bh21_w5_1: (c0, 8.868000ns)
signal bh21_w6_1 :  std_logic;
   -- timing of bh21_w6_1: (c0, 8.868000ns)
signal bh21_w7_1 :  std_logic;
   -- timing of bh21_w7_1: (c0, 8.868000ns)
signal bh21_w8_1 :  std_logic;
   -- timing of bh21_w8_1: (c0, 8.868000ns)
signal bh21_w9_1 :  std_logic;
   -- timing of bh21_w9_1: (c0, 8.868000ns)
signal bh21_w10_1 :  std_logic;
   -- timing of bh21_w10_1: (c0, 8.868000ns)
signal bh21_w11_1 :  std_logic;
   -- timing of bh21_w11_1: (c0, 8.868000ns)
signal bh21_w12_1 :  std_logic;
   -- timing of bh21_w12_1: (c0, 8.868000ns)
signal bh21_w13_1 :  std_logic;
   -- timing of bh21_w13_1: (c0, 8.868000ns)
signal bh21_w14_1 :  std_logic;
   -- timing of bh21_w14_1: (c0, 8.868000ns)
signal bh21_w15_1 :  std_logic;
   -- timing of bh21_w15_1: (c0, 8.868000ns)
signal bh21_w16_1 :  std_logic;
   -- timing of bh21_w16_1: (c0, 8.868000ns)
signal bh21_w17_1 :  std_logic;
   -- timing of bh21_w17_1: (c0, 8.868000ns)
signal bh21_w18_1 :  std_logic;
   -- timing of bh21_w18_1: (c0, 8.868000ns)
signal bh21_w19_1 :  std_logic;
   -- timing of bh21_w19_1: (c0, 8.868000ns)
signal bh21_w20_1 :  std_logic;
   -- timing of bh21_w20_1: (c0, 8.868000ns)
signal bh21_w21_1 :  std_logic;
   -- timing of bh21_w21_1: (c0, 8.868000ns)
signal bh21_w22_1 :  std_logic;
   -- timing of bh21_w22_1: (c0, 8.868000ns)
signal bh21_w23_1 :  std_logic;
   -- timing of bh21_w23_1: (c0, 8.868000ns)
signal bh21_w24_1 :  std_logic;
   -- timing of bh21_w24_1: (c0, 8.868000ns)
signal bh21_w25_1 :  std_logic;
   -- timing of bh21_w25_1: (c0, 8.868000ns)
signal bh21_w26_1 :  std_logic;
   -- timing of bh21_w26_1: (c0, 8.868000ns)
signal bh21_w27_1 :  std_logic;
   -- timing of bh21_w27_1: (c0, 8.868000ns)
signal bh21_w28_1 :  std_logic;
   -- timing of bh21_w28_1: (c0, 8.868000ns)
signal bitheapFinalAdd_bh21_In0 :  std_logic_vector(35 downto 0);
   -- timing of bitheapFinalAdd_bh21_In0: (c0, 8.868000ns)
signal bitheapFinalAdd_bh21_In1 :  std_logic_vector(35 downto 0);
   -- timing of bitheapFinalAdd_bh21_In1: (c0, 8.868000ns)
signal bitheapFinalAdd_bh21_Cin :  std_logic;
   -- timing of bitheapFinalAdd_bh21_Cin: (c0, 0.000000ns)
signal bitheapFinalAdd_bh21_Out :  std_logic_vector(35 downto 0);
   -- timing of bitheapFinalAdd_bh21_Out: (c1, 1.552000ns)
signal bitheapResult_bh21 :  std_logic_vector(34 downto 0);
   -- timing of bitheapResult_bh21: (c1, 1.552000ns)
signal OutRes :  std_logic_vector(34 downto 0);
   -- timing of OutRes: (c1, 1.552000ns)
begin
-- This operator multiplies by log(2)
   FixRealKCM_Freq100_uid20_A0 <= X(7 downto 2);-- input address  m=7  l=2
   FixRealKCM_Freq100_uid20_Table0: FixRealKCM_Freq100_uid20_T0_Freq100_uid23
      port map ( X => FixRealKCM_Freq100_uid20_A0,
                 Y => FixRealKCM_Freq100_uid20_T0_copy24);
   FixRealKCM_Freq100_uid20_T0 <= FixRealKCM_Freq100_uid20_T0_copy24; -- output copy to hold a pipeline register if needed
   bh21_w0_0 <= FixRealKCM_Freq100_uid20_T0(0);
   bh21_w1_0 <= FixRealKCM_Freq100_uid20_T0(1);
   bh21_w2_0 <= FixRealKCM_Freq100_uid20_T0(2);
   bh21_w3_0 <= FixRealKCM_Freq100_uid20_T0(3);
   bh21_w4_0 <= FixRealKCM_Freq100_uid20_T0(4);
   bh21_w5_0 <= FixRealKCM_Freq100_uid20_T0(5);
   bh21_w6_0 <= FixRealKCM_Freq100_uid20_T0(6);
   bh21_w7_0 <= FixRealKCM_Freq100_uid20_T0(7);
   bh21_w8_0 <= FixRealKCM_Freq100_uid20_T0(8);
   bh21_w9_0 <= FixRealKCM_Freq100_uid20_T0(9);
   bh21_w10_0 <= FixRealKCM_Freq100_uid20_T0(10);
   bh21_w11_0 <= FixRealKCM_Freq100_uid20_T0(11);
   bh21_w12_0 <= FixRealKCM_Freq100_uid20_T0(12);
   bh21_w13_0 <= FixRealKCM_Freq100_uid20_T0(13);
   bh21_w14_0 <= FixRealKCM_Freq100_uid20_T0(14);
   bh21_w15_0 <= FixRealKCM_Freq100_uid20_T0(15);
   bh21_w16_0 <= FixRealKCM_Freq100_uid20_T0(16);
   bh21_w17_0 <= FixRealKCM_Freq100_uid20_T0(17);
   bh21_w18_0 <= FixRealKCM_Freq100_uid20_T0(18);
   bh21_w19_0 <= FixRealKCM_Freq100_uid20_T0(19);
   bh21_w20_0 <= FixRealKCM_Freq100_uid20_T0(20);
   bh21_w21_0 <= FixRealKCM_Freq100_uid20_T0(21);
   bh21_w22_0 <= FixRealKCM_Freq100_uid20_T0(22);
   bh21_w23_0 <= FixRealKCM_Freq100_uid20_T0(23);
   bh21_w24_0 <= FixRealKCM_Freq100_uid20_T0(24);
   bh21_w25_0 <= FixRealKCM_Freq100_uid20_T0(25);
   bh21_w26_0 <= FixRealKCM_Freq100_uid20_T0(26);
   bh21_w27_0 <= FixRealKCM_Freq100_uid20_T0(27);
   bh21_w28_0 <= FixRealKCM_Freq100_uid20_T0(28);
   bh21_w29_0 <= FixRealKCM_Freq100_uid20_T0(29);
   bh21_w30_0 <= FixRealKCM_Freq100_uid20_T0(30);
   bh21_w31_0 <= FixRealKCM_Freq100_uid20_T0(31);
   bh21_w32_0 <= FixRealKCM_Freq100_uid20_T0(32);
   bh21_w33_0 <= FixRealKCM_Freq100_uid20_T0(33);
   bh21_w34_0 <= FixRealKCM_Freq100_uid20_T0(34);
   FixRealKCM_Freq100_uid20_A1 <= X(1 downto 0);-- input address  m=1  l=0
   FixRealKCM_Freq100_uid20_Table1: FixRealKCM_Freq100_uid20_T1_Freq100_uid26
      port map ( X => FixRealKCM_Freq100_uid20_A1,
                 Y => FixRealKCM_Freq100_uid20_T1_copy27);
   FixRealKCM_Freq100_uid20_T1 <= FixRealKCM_Freq100_uid20_T1_copy27; -- output copy to hold a pipeline register if needed
   bh21_w0_1 <= FixRealKCM_Freq100_uid20_T1(0);
   bh21_w1_1 <= FixRealKCM_Freq100_uid20_T1(1);
   bh21_w2_1 <= FixRealKCM_Freq100_uid20_T1(2);
   bh21_w3_1 <= FixRealKCM_Freq100_uid20_T1(3);
   bh21_w4_1 <= FixRealKCM_Freq100_uid20_T1(4);
   bh21_w5_1 <= FixRealKCM_Freq100_uid20_T1(5);
   bh21_w6_1 <= FixRealKCM_Freq100_uid20_T1(6);
   bh21_w7_1 <= FixRealKCM_Freq100_uid20_T1(7);
   bh21_w8_1 <= FixRealKCM_Freq100_uid20_T1(8);
   bh21_w9_1 <= FixRealKCM_Freq100_uid20_T1(9);
   bh21_w10_1 <= FixRealKCM_Freq100_uid20_T1(10);
   bh21_w11_1 <= FixRealKCM_Freq100_uid20_T1(11);
   bh21_w12_1 <= FixRealKCM_Freq100_uid20_T1(12);
   bh21_w13_1 <= FixRealKCM_Freq100_uid20_T1(13);
   bh21_w14_1 <= FixRealKCM_Freq100_uid20_T1(14);
   bh21_w15_1 <= FixRealKCM_Freq100_uid20_T1(15);
   bh21_w16_1 <= FixRealKCM_Freq100_uid20_T1(16);
   bh21_w17_1 <= FixRealKCM_Freq100_uid20_T1(17);
   bh21_w18_1 <= FixRealKCM_Freq100_uid20_T1(18);
   bh21_w19_1 <= FixRealKCM_Freq100_uid20_T1(19);
   bh21_w20_1 <= FixRealKCM_Freq100_uid20_T1(20);
   bh21_w21_1 <= FixRealKCM_Freq100_uid20_T1(21);
   bh21_w22_1 <= FixRealKCM_Freq100_uid20_T1(22);
   bh21_w23_1 <= FixRealKCM_Freq100_uid20_T1(23);
   bh21_w24_1 <= FixRealKCM_Freq100_uid20_T1(24);
   bh21_w25_1 <= FixRealKCM_Freq100_uid20_T1(25);
   bh21_w26_1 <= FixRealKCM_Freq100_uid20_T1(26);
   bh21_w27_1 <= FixRealKCM_Freq100_uid20_T1(27);
   bh21_w28_1 <= FixRealKCM_Freq100_uid20_T1(28);

   -- Adding the constant bits 
      -- All the constant bits are zero, nothing to add


   bitheapFinalAdd_bh21_In0 <= "0" & bh21_w34_0 & bh21_w33_0 & bh21_w32_0 & bh21_w31_0 & bh21_w30_0 & bh21_w29_0 & bh21_w28_0 & bh21_w27_0 & bh21_w26_0 & bh21_w25_0 & bh21_w24_0 & bh21_w23_0 & bh21_w22_0 & bh21_w21_0 & bh21_w20_0 & bh21_w19_0 & bh21_w18_0 & bh21_w17_0 & bh21_w16_0 & bh21_w15_0 & bh21_w14_0 & bh21_w13_0 & bh21_w12_0 & bh21_w11_0 & bh21_w10_0 & bh21_w9_0 & bh21_w8_0 & bh21_w7_0 & bh21_w6_0 & bh21_w5_0 & bh21_w4_0 & bh21_w3_0 & bh21_w2_0 & bh21_w1_0 & bh21_w0_0;
   bitheapFinalAdd_bh21_In1 <= "0" & "0" & "0" & "0" & "0" & "0" & "0" & bh21_w28_1 & bh21_w27_1 & bh21_w26_1 & bh21_w25_1 & bh21_w24_1 & bh21_w23_1 & bh21_w22_1 & bh21_w21_1 & bh21_w20_1 & bh21_w19_1 & bh21_w18_1 & bh21_w17_1 & bh21_w16_1 & bh21_w15_1 & bh21_w14_1 & bh21_w13_1 & bh21_w12_1 & bh21_w11_1 & bh21_w10_1 & bh21_w9_1 & bh21_w8_1 & bh21_w7_1 & bh21_w6_1 & bh21_w5_1 & bh21_w4_1 & bh21_w3_1 & bh21_w2_1 & bh21_w1_1 & bh21_w0_1;
   bitheapFinalAdd_bh21_Cin <= '0';

   bitheapFinalAdd_bh21: IntAdder_36_Freq100_uid30
      port map ( clk  => clk,
                 Cin => bitheapFinalAdd_bh21_Cin,
                 X => bitheapFinalAdd_bh21_In0,
                 Y => bitheapFinalAdd_bh21_In1,
                 R => bitheapFinalAdd_bh21_Out);
   bitheapResult_bh21 <= bitheapFinalAdd_bh21_Out(34 downto 0);
   OutRes <= bitheapResult_bh21(34 downto 0);
   R <= OutRes(34 downto 0);
end architecture;

--------------------------------------------------------------------------------
--                         IntAdder_27_Freq100_uid33
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 1 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y Cin
-- Output signals: R
--  approx. input signal timings: X: (c0, 6.762000ns)Y: (c1, 1.552000ns)Cin: (c0, 0.000000ns)
--  approx. output signal timings: R: (c1, 3.376000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_27_Freq100_uid33 is
    port (clk : in std_logic;
          X : in  std_logic_vector(26 downto 0);
          Y : in  std_logic_vector(26 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(26 downto 0)   );
end entity;

architecture arch of IntAdder_27_Freq100_uid33 is
signal Rtmp :  std_logic_vector(26 downto 0);
   -- timing of Rtmp: (c1, 3.376000ns)
signal X_d1 :  std_logic_vector(26 downto 0);
   -- timing of X: (c0, 6.762000ns)
signal Cin_d1 :  std_logic;
   -- timing of Cin: (c0, 0.000000ns)
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            X_d1 <=  X;
            Cin_d1 <=  Cin;
         end if;
      end process;
   Rtmp <= X_d1 + Y + Cin_d1;
   R <= Rtmp;
end architecture;

--------------------------------------------------------------------------------
--              compressedTable_Freq100_uid37_diff_Freq100_uid42
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c1, 3.376000ns)
--  approx. output signal timings: Y: (c1, 0.000000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity compressedTable_Freq100_uid37_diff_Freq100_uid42 is
    port (clk : in std_logic;
          X : in  std_logic_vector(9 downto 0);
          Y : out  std_logic_vector(20 downto 0)   );
end entity;

architecture arch of compressedTable_Freq100_uid37_diff_Freq100_uid42 is
signal Y0, Y0_d1 :  std_logic_vector(20 downto 0);
   -- timing of Y0: (c1, 3.376000ns)
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "block";
signal Y1 :  std_logic_vector(20 downto 0);
   -- timing of Y1: (c1, 0.000000ns)
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            Y0_d1 <=  Y0;
         end if;
      end process;
   with X  select  Y0 <= 
      "000000000000000000000" when "0000000000",
      "000100000000001000000" when "0000000001",
      "001000000000100000000" when "0000000010",
      "001100000001001000001" when "0000000011",
      "010000000010000000001" when "0000000100",
      "010100000011001000011" when "0000000101",
      "011000000100100000101" when "0000000110",
      "011100000110001000111" when "0000000111",
      "000000001000000001011" when "0000001000",
      "000100001010001001111" when "0000001001",
      "001000001100100010101" when "0000001010",
      "001100001111001011100" when "0000001011",
      "010000010010000100100" when "0000001100",
      "010100010101001101110" when "0000001101",
      "011000011000100111001" when "0000001110",
      "011100011100010000111" when "0000001111",
      "000000100000001010110" when "0000010000",
      "000100100100010100111" when "0000010001",
      "001000101000101111010" when "0000010010",
      "001100101101011010000" when "0000010011",
      "010000110010010100111" when "0000010100",
      "010100110111100000010" when "0000010101",
      "011000111100111011111" when "0000010110",
      "011101000010100111111" when "0000010111",
      "000001001000100100010" when "0000011000",
      "000101001110110001000" when "0000011001",
      "001001010101001110001" when "0000011010",
      "001101011011111011101" when "0000011011",
      "010001100010111001100" when "0000011100",
      "010101101010001000000" when "0000011101",
      "011001110001100110111" when "0000011110",
      "011101111001010110001" when "0000011111",
      "000010000001010110000" when "0000100000",
      "000110001001100110011" when "0000100001",
      "001010010010000111010" when "0000100010",
      "001110011010111000101" when "0000100011",
      "010010100011111010101" when "0000100100",
      "010110101101001101001" when "0000100101",
      "011010110110110000010" when "0000100110",
      "011111000000100100000" when "0000100111",
      "000011001010101000010" when "0000101000",
      "000111010100111101010" when "0000101001",
      "001011011111100010111" when "0000101010",
      "001111101010011001010" when "0000101011",
      "010011110101100000010" when "0000101100",
      "011000000000110111111" when "0000101101",
      "011100001100100000011" when "0000101110",
      "100000011000011001100" when "0000101111",
      "000100100100100011011" when "0000110000",
      "001000110000111110001" when "0000110001",
      "001100111101101001100" when "0000110010",
      "010001001010100101110" when "0000110011",
      "010101010111110010111" when "0000110100",
      "011001100101010000110" when "0000110101",
      "011101110010111111100" when "0000110110",
      "100010000000111111001" when "0000110111",
      "000110001111001111101" when "0000111000",
      "001010011101110001000" when "0000111001",
      "001110101100100011011" when "0000111010",
      "010010111011100110101" when "0000111011",
      "010111001010111010111" when "0000111100",
      "011011011010100000000" when "0000111101",
      "011111101010010110001" when "0000111110",
      "100011111010011101010" when "0000111111",
      "001000001010110101100" when "0001000000",
      "001100011011011110101" when "0001000001",
      "010000101100011000111" when "0001000010",
      "010100111101100100010" when "0001000011",
      "011001001111000000101" when "0001000100",
      "011101100000101110001" when "0001000101",
      "100001110010101100110" when "0001000110",
      "100110000100111100100" when "0001000111",
      "001010010111011101011" when "0001001000",
      "001110101010001111011" when "0001001001",
      "010010111101010010101" when "0001001010",
      "010111010000100111000" when "0001001011",
      "011011100100001100110" when "0001001100",
      "011111111000000011101" when "0001001101",
      "100100001100001011110" when "0001001110",
      "101000100000100101001" when "0001001111",
      "001100110101001111110" when "0001010000",
      "010001001010001011110" when "0001010001",
      "010101011111011001001" when "0001010010",
      "011001110100110111110" when "0001010011",
      "011110001010100111101" when "0001010100",
      "100010100000101001000" when "0001010101",
      "100110110110111011110" when "0001010110",
      "101011001101011111111" when "0001010111",
      "001111100100010101100" when "0001011000",
      "010011111011011100100" when "0001011001",
      "011000010010110100111" when "0001011010",
      "011100101010011110110" when "0001011011",
      "100001000010011010010" when "0001011100",
      "100101011010100111001" when "0001011101",
      "101001110011000101100" when "0001011110",
      "101110001011110101100" when "0001011111",
      "000010100100110111000" when "0001100000",
      "000110111110001010001" when "0001100001",
      "001011010111101110110" when "0001100010",
      "001111110001100101001" when "0001100011",
      "010100001011101101000" when "0001100100",
      "011000100110000110100" when "0001100101",
      "011101000000110001110" when "0001100110",
      "100001011011101110101" when "0001100111",
      "000101110110111101010" when "0001101000",
      "001010010010011101100" when "0001101001",
      "001110101110001111100" when "0001101010",
      "010011001010010011011" when "0001101011",
      "010111100110101000111" when "0001101100",
      "011100000011010000001" when "0001101101",
      "100000100000001001010" when "0001101110",
      "100100111101010100001" when "0001101111",
      "001001011010110000111" when "0001110000",
      "001101111000011111100" when "0001110001",
      "010010010110100000000" when "0001110010",
      "010110110100110010011" when "0001110011",
      "011011010011010110101" when "0001110100",
      "011111110010001100110" when "0001110101",
      "100100010001010100111" when "0001110110",
      "101000110000101111000" when "0001110111",
      "001101010000011011000" when "0001111000",
      "010001110000011001000" when "0001111001",
      "010110010000101001000" when "0001111010",
      "011010110001001011001" when "0001111011",
      "011111010001111111010" when "0001111100",
      "100011110011000101011" when "0001111101",
      "101000010100011101101" when "0001111110",
      "101100110110000111111" when "0001111111",
      "000001011000000100011" when "0010000000",
      "000101111010010010111" when "0010000001",
      "001010011100110011101" when "0010000010",
      "001110111111100110100" when "0010000011",
      "010011100010101011101" when "0010000100",
      "011000000110000010111" when "0010000101",
      "011100101001101100011" when "0010000110",
      "100001001101101000001" when "0010000111",
      "000101110001110110001" when "0010001000",
      "001010010110010110011" when "0010001001",
      "001110111011001000111" when "0010001010",
      "010011100000001101110" when "0010001011",
      "011000000101100100111" when "0010001100",
      "011100101011001110011" when "0010001101",
      "100001010001001010011" when "0010001110",
      "100101110111011000101" when "0010001111",
      "001010011101111001010" when "0010010000",
      "001111000100101100011" when "0010010001",
      "010011101011110001111" when "0010010010",
      "011000010011001001110" when "0010010011",
      "011100111010110100010" when "0010010100",
      "100001100010110001001" when "0010010101",
      "100110001011000000101" when "0010010110",
      "101010110011100010100" when "0010010111",
      "001111011100010111000" when "0010011000",
      "010100000101011110001" when "0010011001",
      "011000101110110111110" when "0010011010",
      "011101011000100100000" when "0010011011",
      "100010000010100010110" when "0010011100",
      "100110101100110100010" when "0010011101",
      "101011010111011000011" when "0010011110",
      "110000000010001111010" when "0010011111",
      "000100101101011000110" when "0010100000",
      "001001011000110100111" when "0010100001",
      "001110000100100011110" when "0010100010",
      "010010110000100101100" when "0010100011",
      "010111011100111001111" when "0010100100",
      "011100001001100001001" when "0010100101",
      "100000110110011011001" when "0010100110",
      "100101100011100111111" when "0010100111",
      "001010010001000111100" when "0010101000",
      "001110111110111010000" when "0010101001",
      "010011101100111111011" when "0010101010",
      "011000011011010111101" when "0010101011",
      "011101001010000010111" when "0010101100",
      "100001111001000000111" when "0010101101",
      "100110101000010010000" when "0010101110",
      "101011010111110110000" when "0010101111",
      "000000000111101101000" when "0010110000",
      "000100110111110111000" when "0010110001",
      "001001101000010100000" when "0010110010",
      "001110011001000100000" when "0010110011",
      "010011001010000111001" when "0010110100",
      "010111111011011101010" when "0010110101",
      "011100101101000110100" when "0010110110",
      "100001011111000010111" when "0010110111",
      "000110010001010010011" when "0010111000",
      "001011000011110101001" when "0010111001",
      "001111110110101010111" when "0010111010",
      "010100101001110011111" when "0010111011",
      "011001011101010000001" when "0010111100",
      "011110010000111111101" when "0010111101",
      "100011000101000010010" when "0010111110",
      "100111111001011000010" when "0010111111",
      "001100101110000001100" when "0011000000",
      "010001100010111110000" when "0011000001",
      "010110011000001101111" when "0011000010",
      "011011001101110001000" when "0011000011",
      "100000000011100111100" when "0011000100",
      "100100111001110001100" when "0011000101",
      "101001110000001110110" when "0011000110",
      "101110100110111111100" when "0011000111",
      "000011011110000011101" when "0011001000",
      "001000010101011011010" when "0011001001",
      "001101001101000110011" when "0011001010",
      "010010000101000100111" when "0011001011",
      "010110111101010111000" when "0011001100",
      "011011110101111100101" when "0011001101",
      "100000101110110101110" when "0011001110",
      "100101101000000010100" when "0011001111",
      "001010100001100010110" when "0011010000",
      "001111011011010110101" when "0011010001",
      "010100010101011110001" when "0011010010",
      "011001001111111001011" when "0011010011",
      "011110001010101000001" when "0011010100",
      "100011000101101010101" when "0011010101",
      "101000000001000000111" when "0011010110",
      "101100111100101010110" when "0011010111",
      "000001111000101000100" when "0011011000",
      "000110110100111001111" when "0011011001",
      "001011110001011111001" when "0011011010",
      "010000101110011000001" when "0011011011",
      "010101101011100100111" when "0011011100",
      "011010101001000101100" when "0011011101",
      "011111100110111010000" when "0011011110",
      "100100100101000010011" when "0011011111",
      "001001100011011110101" when "0011100000",
      "001110100010001110111" when "0011100001",
      "010011100001010011000" when "0011100010",
      "011000100000101011000" when "0011100011",
      "011101100000010111000" when "0011100100",
      "100010100000010111000" when "0011100101",
      "100111100000101011001" when "0011100110",
      "101100100001010011001" when "0011100111",
      "000001100010001111010" when "0011101000",
      "000110100011011111100" when "0011101001",
      "001011100101000011110" when "0011101010",
      "010000100110111100001" when "0011101011",
      "010101101001001000101" when "0011101100",
      "011010101011101001010" when "0011101101",
      "011111101110011110000" when "0011101110",
      "100100110001100111000" when "0011101111",
      "001001110101000100010" when "0011110000",
      "001110111000110101101" when "0011110001",
      "010011111100111011011" when "0011110010",
      "011001000001010101010" when "0011110011",
      "011110000110000011100" when "0011110100",
      "100011001011000110001" when "0011110101",
      "101000010000011100111" when "0011110110",
      "101101010110001000001" when "0011110111",
      "000010011100000111110" when "0011111000",
      "000111100010011011101" when "0011111001",
      "001100101001000100000" when "0011111010",
      "010001110000000000110" when "0011111011",
      "010110110111010010000" when "0011111100",
      "011011111110110111110" when "0011111101",
      "100001000110110001111" when "0011111110",
      "100110001111000000100" when "0011111111",
      "001011010111100011110" when "0100000000",
      "010000100000011011100" when "0100000001",
      "010101101001100111111" when "0100000010",
      "011010110011001000110" when "0100000011",
      "011111111100111110010" when "0100000100",
      "100101000111001000011" when "0100000101",
      "101010010001100111001" when "0100000110",
      "101111011100011010100" when "0100000111",
      "000100100111100010101" when "0100001000",
      "001001110010111111100" when "0100001001",
      "001110111110110001000" when "0100001010",
      "010100001010110111011" when "0100001011",
      "011001010111010010011" when "0100001100",
      "011110100100000010010" when "0100001101",
      "100011110001000111000" when "0100001110",
      "101000111110100000011" when "0100001111",
      "001110001100001110110" when "0100010000",
      "010011011010010010000" when "0100010001",
      "011000101000101010000" when "0100010010",
      "011101110111010111000" when "0100010011",
      "100011000110011001000" when "0100010100",
      "101000010101101111111" when "0100010101",
      "101101100101011011110" when "0100010110",
      "110010110101011100100" when "0100010111",
      "001000000101110010011" when "0100011000",
      "001101010110011101010" when "0100011001",
      "010010100111011101010" when "0100011010",
      "010111111000110010010" when "0100011011",
      "011101001010011100010" when "0100011100",
      "100010011100011011100" when "0100011101",
      "100111101110101111111" when "0100011110",
      "101101000001011001011" when "0100011111",
      "000010010100011000000" when "0100100000",
      "000111100111101011111" when "0100100001",
      "001100111011010101000" when "0100100010",
      "010010001111010011011" when "0100100011",
      "010111100011100111000" when "0100100100",
      "011100111000001111110" when "0100100101",
      "100010001101001110000" when "0100100110",
      "100111100010100001100" when "0100100111",
      "001100111000001010010" when "0100101000",
      "010010001110001000100" when "0100101001",
      "010111100100011100001" when "0100101010",
      "011100111011000101000" when "0100101011",
      "100010010010000011100" when "0100101100",
      "100111101001010111011" when "0100101101",
      "101101000001000000101" when "0100101110",
      "110010011000111111100" when "0100101111",
      "000111110001010011110" when "0100110000",
      "001101001001111101101" when "0100110001",
      "010010100010111101000" when "0100110010",
      "010111111100010010000" when "0100110011",
      "011101010101111100101" when "0100110100",
      "100010101111111100110" when "0100110101",
      "101000001010010010101" when "0100110110",
      "101101100100111110000" when "0100110111",
      "000010111111111111010" when "0100111000",
      "001000011011010110000" when "0100111001",
      "001101110111000010101" when "0100111010",
      "010011010011000100111" when "0100111011",
      "011000101111011101000" when "0100111100",
      "011110001100001010111" when "0100111101",
      "100011101001001110100" when "0100111110",
      "101001000110101000000" when "0100111111",
      "001110100100010111011" when "0101000000",
      "010100000010011100101" when "0101000001",
      "011001100000110111110" when "0101000010",
      "011110111111101000110" when "0101000011",
      "100100011110101111101" when "0101000100",
      "101001111110001100100" when "0101000101",
      "101111011101111111011" when "0101000110",
      "110100111110001000010" when "0101000111",
      "001010011110100111010" when "0101001000",
      "001111111111011100001" when "0101001001",
      "010101100000100111001" when "0101001010",
      "011011000010001000010" when "0101001011",
      "100000100011111111011" when "0101001100",
      "100110000110001100110" when "0101001101",
      "101011101000110000010" when "0101001110",
      "110001001011101001111" when "0101001111",
      "000110101110111001101" when "0101010000",
      "001100010010011111110" when "0101010001",
      "010001110110011100000" when "0101010010",
      "010111011010101110100" when "0101010011",
      "011100111111010111011" when "0101010100",
      "100010100100010110100" when "0101010101",
      "101000001001101011111" when "0101010110",
      "101101101111010111101" when "0101010111",
      "000011010101011001110" when "0101011000",
      "001000111011110010011" when "0101011001",
      "001110100010100001010" when "0101011010",
      "010100001001100110101" when "0101011011",
      "011001110001000010100" when "0101011100",
      "011111011000110100110" when "0101011101",
      "100101000000111101101" when "0101011110",
      "101010101001011100111" when "0101011111",
      "000000010010010010110" when "0101100000",
      "000101111011011111010" when "0101100001",
      "001011100101000010010" when "0101100010",
      "010001001110111011111" when "0101100011",
      "010110111001001100001" when "0101100100",
      "011100100011110011000" when "0101100101",
      "100010001110110000101" when "0101100110",
      "100111111010000100111" when "0101100111",
      "001101100101101111111" when "0101101000",
      "010011010001110001101" when "0101101001",
      "011000111110001010001" when "0101101010",
      "011110101010111001011" when "0101101011",
      "100100010111111111100" when "0101101100",
      "101010000101011100011" when "0101101101",
      "101111110011010000001" when "0101101110",
      "110101100001011010110" when "0101101111",
      "001011001111111100011" when "0101110000",
      "010000111110110100110" when "0101110001",
      "010110101110000100010" when "0101110010",
      "011100011101101010100" when "0101110011",
      "100010001101100111111" when "0101110100",
      "100111111101111100010" when "0101110101",
      "101101101110100111101" when "0101110110",
      "110011011111101010001" when "0101110111",
      "001001010001000011101" when "0101111000",
      "001111000010110100010" when "0101111001",
      "010100110100111100000" when "0101111010",
      "011010100111011010111" when "0101111011",
      "100000011010010000111" when "0101111100",
      "100110001101011110001" when "0101111101",
      "101100000001000010101" when "0101111110",
      "110001110100111110010" when "0101111111",
      "000111101001010001010" when "0110000000",
      "001101011101111011100" when "0110000001",
      "010011010010111101000" when "0110000010",
      "011001001000010101111" when "0110000011",
      "011110111110000110000" when "0110000100",
      "100100110100001101101" when "0110000101",
      "101010101010101100101" when "0110000110",
      "110000100001100011000" when "0110000111",
      "000110011000110000110" when "0110001000",
      "001100010000010110000" when "0110001001",
      "010010001000010010111" when "0110001010",
      "011000000000100111001" when "0110001011",
      "011101111001010010111" when "0110001100",
      "100011110010010110010" when "0110001101",
      "101001101011110001010" when "0110001110",
      "101111100101100011110" when "0110001111",
      "000101011111101101111" when "0110010000",
      "001011011010001111110" when "0110010001",
      "010001010101001001010" when "0110010010",
      "010111010000011010011" when "0110010011",
      "011101001100000011010" when "0110010100",
      "100011001000000011111" when "0110010101",
      "101001000100011100010" when "0110010110",
      "101111000001001100100" when "0110010111",
      "000100111110010100011" when "0110011000",
      "001010111011110100010" when "0110011001",
      "010000111001101011111" when "0110011010",
      "010110110111111011100" when "0110011011",
      "011100110110100010111" when "0110011100",
      "100010110101100010010" when "0110011101",
      "101000110100111001101" when "0110011110",
      "101110110100101000111" when "0110011111",
      "000100110100110000010" when "0110100000",
      "001010110101001111100" when "0110100001",
      "010000110110000110111" when "0110100010",
      "010110110111010110010" when "0110100011",
      "011100111000111101110" when "0110100100",
      "100010111010111101011" when "0110100101",
      "101000111101010101001" when "0110100110",
      "101111000000000101001" when "0110100111",
      "000101000011001101010" when "0110101000",
      "001011000110101101100" when "0110101001",
      "010001001010100110000" when "0110101010",
      "010111001110110110111" when "0110101011",
      "011101010011011111111" when "0110101100",
      "100011011000100001010" when "0110101101",
      "101001011101111011000" when "0110101110",
      "101111100011101101000" when "0110101111",
      "000101101001110111100" when "0110110000",
      "001011110000011010010" when "0110110001",
      "010001110111010101100" when "0110110010",
      "010111111110101001010" when "0110110011",
      "011110000110010101011" when "0110110100",
      "100100001110011010000" when "0110110101",
      "101010010110110111001" when "0110110110",
      "110000011111101100111" when "0110110111",
      "000110101000111011001" when "0110111000",
      "001100110010100010000" when "0110111001",
      "010010111100100001100" when "0110111010",
      "011001000110111001100" when "0110111011",
      "011111010001101010011" when "0110111100",
      "100101011100110011110" when "0110111101",
      "101011101000010101111" when "0110111110",
      "110001110100010000111" when "0110111111",
      "001000000000100100100" when "0111000000",
      "001110001101010000111" when "0111000001",
      "010100011010010110001" when "0111000010",
      "011010100111110100010" when "0111000011",
      "100000110101101011001" when "0111000100",
      "100111000011111010111" when "0111000101",
      "101101010010100011101" when "0111000110",
      "110011100001100101010" when "0111000111",
      "001001110000111111111" when "0111001000",
      "010000000000110011011" when "0111001001",
      "010110010000111111111" when "0111001010",
      "011100100001100101100" when "0111001011",
      "100010110010100100001" when "0111001100",
      "101001000011111011111" when "0111001101",
      "101111010101101100101" when "0111001110",
      "110101100111110110100" when "0111001111",
      "001011111010011001101" when "0111010000",
      "010010001101010101111" when "0111010001",
      "011000100000101011010" when "0111010010",
      "011110110100011010000" when "0111010011",
      "100101001000100001111" when "0111010100",
      "101011011101000011000" when "0111010101",
      "110001110001111101100" when "0111010110",
      "111000000111010001010" when "0111010111",
      "001110011100111110011" when "0111011000",
      "010100110011000100111" when "0111011001",
      "011011001001100100110" when "0111011010",
      "100001100000011110001" when "0111011011",
      "100111110111110000111" when "0111011100",
      "101110001111011101001" when "0111011101",
      "110100100111100010111" when "0111011110",
      "111011000000000010000" when "0111011111",
      "000001011000111010111" when "0111100000",
      "000111110010001101001" when "0111100001",
      "001110001011111001001" when "0111100010",
      "010100100101111110101" when "0111100011",
      "011011000000011101111" when "0111100100",
      "100001011011010110110" when "0111100101",
      "100111110110101001010" when "0111100110",
      "101110010010010101101" when "0111100111",
      "000100101110011011101" when "0111101000",
      "001011001010111011011" when "0111101001",
      "010001100111110101000" when "0111101010",
      "011000000101001000011" when "0111101011",
      "011110100010110101101" when "0111101100",
      "100101000000111100110" when "0111101101",
      "101011011111011101110" when "0111101110",
      "110001111110011000101" when "0111101111",
      "001000011101101101100" when "0111110000",
      "001110111101011100011" when "0111110001",
      "010101011101100101010" when "0111110010",
      "011011111110001000001" when "0111110011",
      "100010011111000101000" when "0111110100",
      "101001000000011100000" when "0111110101",
      "101111100010001101001" when "0111110110",
      "110110000100011000010" when "0111110111",
      "001100100110111101101" when "0111111000",
      "010011001001111101001" when "0111111001",
      "011001101101010110111" when "0111111010",
      "100000010001001010111" when "0111111011",
      "100110110101011001000" when "0111111100",
      "101101011010000001100" when "0111111101",
      "110011111111000100010" when "0111111110",
      "111010100100100001011" when "0111111111",
      "000100010110010111111" when "1000000000",
      "000110110001101110001" when "1000000001",
      "001001001101001110001" when "1000000010",
      "001011101000110111110" when "1000000011",
      "001110000100101011010" when "1000000100",
      "010000100000101000011" when "1000000101",
      "010010111100101111010" when "1000000110",
      "010101011001000000000" when "1000000111",
      "000111110101011010011" when "1000001000",
      "001010010001111110101" when "1000001001",
      "001100101110101100101" when "1000001010",
      "001111001011100100100" when "1000001011",
      "010001101000100110001" when "1000001100",
      "010100000101110001101" when "1000001101",
      "010110100011000110111" when "1000001110",
      "011001000000100110000" when "1000001111",
      "001011011110001110111" when "1000010000",
      "001101111100000001110" when "1000010001",
      "010000011001111110100" when "1000010010",
      "010010111000000101000" when "1000010011",
      "010101010110010101100" when "1000010100",
      "010111110100101111110" when "1000010101",
      "011010010011010100000" when "1000010110",
      "011100110010000010010" when "1000010111",
      "001111010000111010010" when "1000011000",
      "010001101111111100011" when "1000011001",
      "010100001111001000010" when "1000011010",
      "010110101110011110010" when "1000011011",
      "011001001101111110001" when "1000011100",
      "011011101101101000000" when "1000011101",
      "011110001101011011111" when "1000011110",
      "100000101101011001101" when "1000011111",
      "000011001101100001100" when "1000100000",
      "000101101101110011011" when "1000100001",
      "001000001110001111010" when "1000100010",
      "001010101110110101001" when "1000100011",
      "001101001111100101001" when "1000100100",
      "001111110000011111001" when "1000100101",
      "010010010001100011001" when "1000100110",
      "010100110010110001010" when "1000100111",
      "000111010100001001100" when "1000101000",
      "001001110101101011110" when "1000101001",
      "001100010111011000010" when "1000101010",
      "001110111001001110110" when "1000101011",
      "010001011011001111011" when "1000101100",
      "010011111101011010001" when "1000101101",
      "010110011111101111000" when "1000101110",
      "011001000010001110001" when "1000101111",
      "001011100100110111011" when "1000110000",
      "001110000111101010110" when "1000110001",
      "010000101010101000010" when "1000110010",
      "010011001101110000000" when "1000110011",
      "010101110001000010000" when "1000110100",
      "011000010100011110001" when "1000110101",
      "011010111000000100101" when "1000110110",
      "011101011011110101010" when "1000110111",
      "001111111111110000000" when "1000111000",
      "010010100011110101001" when "1000111001",
      "010101001000000100100" when "1000111010",
      "010111101100011110001" when "1000111011",
      "011010010001000010001" when "1000111100",
      "011100110101110000011" when "1000111101",
      "011111011010101000111" when "1000111110",
      "100001111111101011101" when "1000111111",
      "000100100100111000110" when "1001000000",
      "000111001010010000010" when "1001000001",
      "001001101111110010001" when "1001000010",
      "001100010101011110010" when "1001000011",
      "001110111011010100110" when "1001000100",
      "010001100001010101101" when "1001000101",
      "010100000111100000111" when "1001000110",
      "010110101101110110101" when "1001000111",
      "001001010100010110101" when "1001001000",
      "001011111011000001001" when "1001001001",
      "001110100001110110000" when "1001001010",
      "010001001000110101011" when "1001001011",
      "010011101111111111001" when "1001001100",
      "010110010111010011011" when "1001001101",
      "011000111110110010001" when "1001001110",
      "011011100110011011010" when "1001001111",
      "001110001110001110111" when "1001010000",
      "010000110110001101000" when "1001010001",
      "010011011110010101101" when "1001010010",
      "010110000110101000110" when "1001010011",
      "011000101111000110100" when "1001010100",
      "011011010111101110110" when "1001010101",
      "011110000000100001100" when "1001010110",
      "100000101001011110110" when "1001010111",
      "000011010010100110101" when "1001011000",
      "000101111011111001001" when "1001011001",
      "001000100101010110001" when "1001011010",
      "001011001110111101110" when "1001011011",
      "001101111000110000000" when "1001011100",
      "010000100010101100111" when "1001011101",
      "010011001100110100011" when "1001011110",
      "010101110111000110100" when "1001011111",
      "001000100001100011010" when "1001100000",
      "001011001100001010101" when "1001100001",
      "001101110110111100110" when "1001100010",
      "010000100001111001100" when "1001100011",
      "010011001101000001000" when "1001100100",
      "010101111000010011001" when "1001100101",
      "011000100011110000000" when "1001100110",
      "011011001111010111101" when "1001100111",
      "001101111011001010000" when "1001101000",
      "010000100111000111000" when "1001101001",
      "010011010011001110111" when "1001101010",
      "010101111111100001100" when "1001101011",
      "011000101011111110110" when "1001101100",
      "011011011000100111000" when "1001101101",
      "011110000101011001111" when "1001101110",
      "100000110010010111101" when "1001101111",
      "000011011111100000010" when "1001110000",
      "000110001100110011101" when "1001110001",
      "001000111010010001110" when "1001110010",
      "001011100111111010111" when "1001110011",
      "001110010101101110110" when "1001110100",
      "010001000011101101101" when "1001110101",
      "010011110001110111010" when "1001110110",
      "010110100000001011111" when "1001110111",
      "001001001110101011010" when "1001111000",
      "001011111101010101101" when "1001111001",
      "001110101100001011000" when "1001111010",
      "010001011011001011001" when "1001111011",
      "010100001010010110011" when "1001111100",
      "010110111001101100100" when "1001111101",
      "011001101001001101101" when "1001111110",
      "011100011000111001101" when "1001111111",
      "001111001000110000101" when "1010000000",
      "010001111000110010110" when "1010000001",
      "010100101000111111110" when "1010000010",
      "010111011001010111111" when "1010000011",
      "011010001001111011000" when "1010000100",
      "011100111010101001001" when "1010000101",
      "011111101011100010010" when "1010000110",
      "100010011100100110100" when "1010000111",
      "000101001101110101111" when "1010001000",
      "000111111111010000010" when "1010001001",
      "001010110000110101110" when "1010001010",
      "001101100010100110011" when "1010001011",
      "010000010100100010001" when "1010001100",
      "010011000110101001000" when "1010001101",
      "010101111000111011000" when "1010001110",
      "011000101011011000001" when "1010001111",
      "001011011110000000011" when "1010010000",
      "001110010000110011111" when "1010010001",
      "010001000011110010100" when "1010010010",
      "010011110110111100011" when "1010010011",
      "010110101010010001011" when "1010010100",
      "011001011101110001101" when "1010010101",
      "011100010001011101001" when "1010010110",
      "011111000101010011110" when "1010010111",
      "000001111001010101110" when "1010011000",
      "000100101101100011000" when "1010011001",
      "000111100001111011100" when "1010011010",
      "001010010110011111010" when "1010011011",
      "001101001011001110010" when "1010011100",
      "010000000000001000101" when "1010011101",
      "010010110101001110010" when "1010011110",
      "010101101010011111010" when "1010011111",
      "001000011111111011101" when "1010100000",
      "001011010101100011010" when "1010100001",
      "001110001011010110010" when "1010100010",
      "010001000001010100101" when "1010100011",
      "010011110111011110100" when "1010100100",
      "010110101101110011101" when "1010100101",
      "011001100100010100001" when "1010100110",
      "011100011011000000001" when "1010100111",
      "001111010001110111101" when "1010101000",
      "010010001000111010011" when "1010101001",
      "010101000000001000101" when "1010101010",
      "010111110111100010011" when "1010101011",
      "011010101111000111101" when "1010101100",
      "011101100110111000011" when "1010101101",
      "100000011110110100100" when "1010101110",
      "100011010110111100001" when "1010101111",
      "000110001111001111011" when "1010110000",
      "001001000111101110001" when "1010110001",
      "001100000000011000011" when "1010110010",
      "001110111001001110001" when "1010110011",
      "010001110010001111100" when "1010110100",
      "010100101011011100011" when "1010110101",
      "010111100100110100111" when "1010110110",
      "011010011110011001000" when "1010110111",
      "001101011000001000110" when "1010111000",
      "010000010010000100000" when "1010111001",
      "010011001100001011000" when "1010111010",
      "010110000110011101101" when "1010111011",
      "011001000000111011110" when "1010111100",
      "011011111011100101110" when "1010111101",
      "011110110110011011010" when "1010111110",
      "100001110001011100100" when "1010111111",
      "000100101100101001011" when "1011000000",
      "000111101000000010001" when "1011000001",
      "001010100011100110100" when "1011000010",
      "001101011111010110100" when "1011000011",
      "010000011011010010011" when "1011000100",
      "010011010111011010000" when "1011000101",
      "010110010011101101010" when "1011000110",
      "011001010000001100011" when "1011000111",
      "001100001100110111011" when "1011001000",
      "001111001001101110000" when "1011001001",
      "010010000110110000100" when "1011001010",
      "010101000011111110111" when "1011001011",
      "011000000001011001000" when "1011001100",
      "011010111110111111000" when "1011001101",
      "011101111100110000111" when "1011001110",
      "100000111010101110101" when "1011001111",
      "000011111000111000010" when "1011010000",
      "000110110111001101110" when "1011010001",
      "001001110101101111001" when "1011010010",
      "001100110100011100100" when "1011010011",
      "001111110011010101110" when "1011010100",
      "010010110010011010111" when "1011010101",
      "010101110001101100000" when "1011010110",
      "011000110001001001001" when "1011010111",
      "001011110000110010001" when "1011011000",
      "001110110000100111010" when "1011011001",
      "010001110000101000010" when "1011011010",
      "010100110000110101010" when "1011011011",
      "010111110001001110011" when "1011011100",
      "011010110001110011100" when "1011011101",
      "011101110010100100101" when "1011011110",
      "100000110011100001110" when "1011011111",
      "000011110100101011000" when "1011100000",
      "000110110110000000011" when "1011100001",
      "001001110111100001110" when "1011100010",
      "001100111001001111011" when "1011100011",
      "001111111011001001000" when "1011100100",
      "010010111101001110110" when "1011100101",
      "010101111111100000101" when "1011100110",
      "011001000001111110101" when "1011100111",
      "001100000100101000111" when "1011101000",
      "001111000111011111010" when "1011101001",
      "010010001010100001110" when "1011101010",
      "010101001101110000101" when "1011101011",
      "011000010001001011100" when "1011101100",
      "011011010100110010110" when "1011101101",
      "011110011000100110001" when "1011101110",
      "100001011100100101110" when "1011101111",
      "000100100000110001110" when "1011110000",
      "000111100101001001111" when "1011110001",
      "001010101001101110011" when "1011110010",
      "001101101110011111001" when "1011110011",
      "010000110011011100001" when "1011110100",
      "010011111000100101100" when "1011110101",
      "010110111101111011010" when "1011110110",
      "011010000011011101010" when "1011110111",
      "001101001001001011110" when "1011111000",
      "010000001111000110100" when "1011111001",
      "010011010101001101101" when "1011111010",
      "010110011011100001001" when "1011111011",
      "011001100010000001000" when "1011111100",
      "011100101000101101011" when "1011111101",
      "011111101111100110001" when "1011111110",
      "100010110110101011011" when "1011111111",
      "000101111101111101000" when "1100000000",
      "001001000101011011001" when "1100000001",
      "001100001101000101101" when "1100000010",
      "001111010100111100110" when "1100000011",
      "010010011101000000010" when "1100000100",
      "010101100101010000011" when "1100000101",
      "011000101101101100111" when "1100000110",
      "011011110110010110000" when "1100000111",
      "001110111111001011110" when "1100001000",
      "010010001000001110000" when "1100001001",
      "010101010001011100110" when "1100001010",
      "011000011010111000001" when "1100001011",
      "011011100100100000001" when "1100001100",
      "011110101110010100110" when "1100001101",
      "100001111000010101111" when "1100001110",
      "100101000010100011110" when "1100001111",
      "001000001100111110010" when "1100010000",
      "001011010111100101011" when "1100010001",
      "001110100010011001001" when "1100010010",
      "010001101101011001101" when "1100010011",
      "010100111000100110111" when "1100010100",
      "011000000100000000110" when "1100010101",
      "011011001111100111011" when "1100010110",
      "011110011011011010110" when "1100010111",
      "000001100111011010110" when "1100011000",
      "000100110011100111101" when "1100011001",
      "001000000000000001010" when "1100011010",
      "001011001100100111101" when "1100011011",
      "001110011001011010111" when "1100011100",
      "010001100110011010111" when "1100011101",
      "010100110011100111101" when "1100011110",
      "011000000001000001010" when "1100011111",
      "001011001110100111110" when "1100100000",
      "001110011100011011001" when "1100100001",
      "010001101010011011011" when "1100100010",
      "010100111000101000011" when "1100100011",
      "011000000111000010011" when "1100100100",
      "011011010101101001011" when "1100100101",
      "011110100100011101001" when "1100100110",
      "100001110011011101111" when "1100100111",
      "000101000010101011101" when "1100101000",
      "001000010010000110010" when "1100101001",
      "001011100001101101111" when "1100101010",
      "001110110001100010011" when "1100101011",
      "010010000001100100000" when "1100101100",
      "010101010001110010101" when "1100101101",
      "011000100010001110010" when "1100101110",
      "011011110010110110111" when "1100101111",
      "001111000011101100101" when "1100110000",
      "010010010100101111011" when "1100110001",
      "010101100101111111010" when "1100110010",
      "011000110111011100001" when "1100110011",
      "011100001001000110001" when "1100110100",
      "011111011010111101010" when "1100110101",
      "100010101101000001100" when "1100110110",
      "100101111111010010111" when "1100110111",
      "001001010001110001100" when "1100111000",
      "001100100100011101001" when "1100111001",
      "001111110111010110000" when "1100111010",
      "010011001010011100000" when "1100111011",
      "010110011101101111010" when "1100111100",
      "011001110001001111110" when "1100111101",
      "011101000100111101100" when "1100111110",
      "100000011000111000011" when "1100111111",
      "000011101101000000101" when "1101000000",
      "000111000001010110000" when "1101000001",
      "001010010101111000110" when "1101000010",
      "001101101010101000110" when "1101000011",
      "010000111111100110001" when "1101000100",
      "010100010100110000110" when "1101000101",
      "010111101010001000110" when "1101000110",
      "011010111111101110000" when "1101000111",
      "001110010101100000101" when "1101001000",
      "010001101011100000110" when "1101001001",
      "010101000001101110001" when "1101001010",
      "011000011000001000111" when "1101001011",
      "011011101110110001001" when "1101001100",
      "011111000101100110110" when "1101001101",
      "100010011100101001111" when "1101001110",
      "100101110011111010011" when "1101001111",
      "001001001011011000011" when "1101010000",
      "001100100011000011110" when "1101010001",
      "001111111010111100110" when "1101010010",
      "010011010011000011001" when "1101010011",
      "010110101011010111001" when "1101010100",
      "011010000011111000101" when "1101010101",
      "011101011100100111101" when "1101010110",
      "100000110101100100001" when "1101010111",
      "000100001110101110010" when "1101011000",
      "000111101000000110000" when "1101011001",
      "001011000001101011010" when "1101011010",
      "001110011011011110010" when "1101011011",
      "010001110101011110110" when "1101011100",
      "010101001111101100111" when "1101011101",
      "011000101010001000110" when "1101011110",
      "011100000100110010001" when "1101011111",
      "001111011111101001010" when "1101100000",
      "010010111010101110001" when "1101100001",
      "010110010110000000101" when "1101100010",
      "011001110001100000111" when "1101100011",
      "011101001101001110111" when "1101100100",
      "100000101001001010100" when "1101100101",
      "100100000101010100000" when "1101100110",
      "100111100001101011010" when "1101100111",
      "001010111110010000010" when "1101101000",
      "001110011011000011000" when "1101101001",
      "010001111000000011101" when "1101101010",
      "010101010101010010000" when "1101101011",
      "011000110010101110010" when "1101101100",
      "011100010000011000011" when "1101101101",
      "011111101110010000011" when "1101101110",
      "100011001100010110001" when "1101101111",
      "000110101010101001111" when "1101110000",
      "001010001001001011100" when "1101110001",
      "001101100111111011000" when "1101110010",
      "010001000110111000100" when "1101110011",
      "010100100110000011111" when "1101110100",
      "011000000101011101010" when "1101110101",
      "011011100101000100101" when "1101110110",
      "011111000100111001111" when "1101110111",
      "000010100100111101001" when "1101111000",
      "000110000101001110100" when "1101111001",
      "001001100101101101111" when "1101111010",
      "001101000110011011010" when "1101111011",
      "010000100111010110101" when "1101111100",
      "010100001000100000001" when "1101111101",
      "010111101001110111110" when "1101111110",
      "011011001011011101011" when "1101111111",
      "001110101101010001001" when "1110000000",
      "010010001111010011000" when "1110000001",
      "010101110001100011001" when "1110000010",
      "011001010100000001010" when "1110000011",
      "011100110110101101101" when "1110000100",
      "100000011001101000001" when "1110000101",
      "100011111100110000110" when "1110000110",
      "100111100000000111101" when "1110000111",
      "001011000011101100110" when "1110001000",
      "001110100111100000001" when "1110001001",
      "010010001011100001110" when "1110001010",
      "010101101111110001101" when "1110001011",
      "011001010100001111110" when "1110001100",
      "011100111000111100001" when "1110001101",
      "100000011101110110111" when "1110001110",
      "100100000010111111111" when "1110001111",
      "000111101000010111010" when "1110010000",
      "001011001101111100111" when "1110010001",
      "001110110011110001000" when "1110010010",
      "010010011001110011011" when "1110010011",
      "010110000000000100010" when "1110010100",
      "011001100110100011011" when "1110010101",
      "011101001101010001000" when "1110010110",
      "100000110100001101001" when "1110010111",
      "000100011011010111100" when "1110011000",
      "001000000010110000100" when "1110011001",
      "001011101010010111111" when "1110011010",
      "001111010010001101110" when "1110011011",
      "010010111010010010010" when "1110011100",
      "010110100010100101001" when "1110011101",
      "011010001011000110100" when "1110011110",
      "011101110011110110100" when "1110011111",
      "000001011100110101000" when "1110100000",
      "000101000110000010001" when "1110100001",
      "001000101111011101110" when "1110100010",
      "001100011001001000000" when "1110100011",
      "010000000011000000111" when "1110100100",
      "010011101101001000011" when "1110100101",
      "010111010111011110100" when "1110100110",
      "011011000010000011011" when "1110100111",
      "001110101100110110111" when "1110101000",
      "010010010111111001000" when "1110101001",
      "010110000011001001110" when "1110101010",
      "011001101110101001011" when "1110101011",
      "011101011010010111101" when "1110101100",
      "100001000110010100101" when "1110101101",
      "100100110010100000011" when "1110101110",
      "101000011110111011000" when "1110101111",
      "001100001011100100010" when "1110110000",
      "001111111000011100011" when "1110110001",
      "010011100101100011011" when "1110110010",
      "010111010010111001001" when "1110110011",
      "011011000000011101110" when "1110110100",
      "011110101110010001010" when "1110110101",
      "100010011100010011100" when "1110110110",
      "100110001010100100110" when "1110110111",
      "001001111001000100111" when "1110111000",
      "001101100111110011111" when "1110111001",
      "010001010110110001111" when "1110111010",
      "010101000101111110110" when "1110111011",
      "011000110101011010101" when "1110111100",
      "011100100101000101011" when "1110111101",
      "100000010100111111010" when "1110111110",
      "100100000101001000000" when "1110111111",
      "000111110101011111111" when "1111000000",
      "001011100110000110110" when "1111000001",
      "001111010110111100101" when "1111000010",
      "010011001000000001101" when "1111000011",
      "010110111001010101101" when "1111000100",
      "011010101010111000110" when "1111000101",
      "011110011100101011000" when "1111000110",
      "100010001110101100011" when "1111000111",
      "000110000000111100111" when "1111001000",
      "001001110011011100100" when "1111001001",
      "001101100110001011010" when "1111001010",
      "010001011001001001010" when "1111001011",
      "010101001100010110011" when "1111001100",
      "011000111111110010111" when "1111001101",
      "011100110011011110011" when "1111001110",
      "100000100111011001010" when "1111001111",
      "000100011011100011011" when "1111010000",
      "001000001111111100110" when "1111010001",
      "001100000100100101011" when "1111010010",
      "001111111001011101010" when "1111010011",
      "010011101110100100100" when "1111010100",
      "010111100011111011001" when "1111010101",
      "011011011001100001000" when "1111010110",
      "011111001111010110010" when "1111010111",
      "000011000101011011000" when "1111011000",
      "000110111011101111000" when "1111011001",
      "001010110010010010011" when "1111011010",
      "001110101001000101010" when "1111011011",
      "010010100000000111100" when "1111011100",
      "010110010111011001010" when "1111011101",
      "011010001110111010100" when "1111011110",
      "011110000110101011001" when "1111011111",
      "000001111110101011011" when "1111100000",
      "000101110110111011000" when "1111100001",
      "001001101111011010010" when "1111100010",
      "001101101000001000111" when "1111100011",
      "010001100001000111010" when "1111100100",
      "010101011010010101001" when "1111100101",
      "011001010011110010100" when "1111100110",
      "011101001101011111100" when "1111100111",
      "000001000111011100010" when "1111101000",
      "000101000001101000100" when "1111101001",
      "001000111100000100011" when "1111101010",
      "001100110110110000000" when "1111101011",
      "010000110001101011010" when "1111101100",
      "010100101100110110010" when "1111101101",
      "011000101000010000111" when "1111101110",
      "011100100011111011010" when "1111101111",
      "000000011111110101011" when "1111110000",
      "000100011011111111010" when "1111110001",
      "001000011000011000111" when "1111110010",
      "001100010101000010010" when "1111110011",
      "010000010001111011100" when "1111110100",
      "010100001111000100100" when "1111110101",
      "011000001100011101011" when "1111110110",
      "011100001010000110001" when "1111110111",
      "000000000111111110101" when "1111111000",
      "000100000110000111001" when "1111111001",
      "001000000100011111100" when "1111111010",
      "001100000011000111101" when "1111111011",
      "010000000001111111111" when "1111111100",
      "010100000001000111111" when "1111111101",
      "011000000000100000000" when "1111111110",
      "011100000000001000000" when "1111111111",
      "---------------------" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                       compressedTable_Freq100_uid37
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Luc Forget, Maxime Christ (2020)
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c1, 3.376000ns)
--  approx. output signal timings: Y: (c1, 5.992000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity compressedTable_Freq100_uid37 is
    port (clk : in std_logic;
          X : in  std_logic_vector(9 downto 0);
          Y : out  std_logic_vector(27 downto 0)   );
end entity;

architecture arch of compressedTable_Freq100_uid37 is
   component compressedTable_Freq100_uid37_subsampling_Freq100_uid39 is
      port ( X : in  std_logic_vector(6 downto 0);
             Y : out  std_logic_vector(8 downto 0)   );
   end component;

   component compressedTable_Freq100_uid37_diff_Freq100_uid42 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(9 downto 0);
             Y : out  std_logic_vector(20 downto 0)   );
   end component;

signal X_subsampling :  std_logic_vector(6 downto 0);
   -- timing of X_subsampling: (c1, 3.376000ns)
signal Y_subsampling :  std_logic_vector(8 downto 0);
   -- timing of Y_subsampling: (c1, 4.624000ns)
signal Y_subsampling_copy40 :  std_logic_vector(8 downto 0);
   -- timing of Y_subsampling_copy40: (c1, 3.376000ns)
signal Y_diff :  std_logic_vector(20 downto 0);
   -- timing of Y_diff: (c1, 0.000000ns)
signal fullOut_topbits :  std_logic_vector(8 downto 0);
   -- timing of fullOut_topbits: (c1, 5.992000ns)
signal fullOut :  std_logic_vector(27 downto 0);
   -- timing of fullOut: (c1, 5.992000ns)
begin
   X_subsampling <= X(9 downto 3);
   compressedTable_Freq100_uid37_subsampling: compressedTable_Freq100_uid37_subsampling_Freq100_uid39
      port map ( X => X_subsampling,
                 Y => Y_subsampling_copy40);
   Y_subsampling <= Y_subsampling_copy40; -- output copy to hold a pipeline register if needed
   compressedTable_Freq100_uid37_diff: compressedTable_Freq100_uid37_diff_Freq100_uid42
      port map ( clk  => clk,
                 X => X,
                 Y => Y_diff);
   fullOut_topbits <= Y_subsampling + ("0000000"& (Y_diff(20 downto 19)));
   fullOut <= fullOut_topbits & (Y_diff(18 downto 0));
   Y <= fullOut;
end architecture;

--------------------------------------------------------------------------------
--                      FixFunctionByTable_Freq100_uid35
-- Evaluator for exp(x*1b-1) on [-1,1) for lsbIn=-9 (wIn=10), msbout=0, lsbOut=-27 (wOut=28). Out interval: [0.606531; 1.64711]. Output is unsigned

-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2010-2018)
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: Y
--  approx. input signal timings: X: (c1, 3.376000ns)
--  approx. output signal timings: Y: (c1, 5.992000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity FixFunctionByTable_Freq100_uid35 is
    port (clk : in std_logic;
          X : in  std_logic_vector(9 downto 0);
          Y : out  std_logic_vector(27 downto 0)   );
end entity;

architecture arch of FixFunctionByTable_Freq100_uid35 is
   component compressedTable_Freq100_uid37 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(9 downto 0);
             Y : out  std_logic_vector(27 downto 0)   );
   end component;

begin
   compressedTable: compressedTable_Freq100_uid37
      port map ( clk  => clk,
                 X => X,
                 Y => Y);
end architecture;

--------------------------------------------------------------------------------
--                         IntAdder_18_Freq100_uid48
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 1 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y Cin
-- Output signals: R
--  approx. input signal timings: X: (c1, 3.376000ns)Y: (c1, 4.624000ns)Cin: (c0, 0.000000ns)
--  approx. output signal timings: R: (c1, 6.220000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_18_Freq100_uid48 is
    port (clk : in std_logic;
          X : in  std_logic_vector(17 downto 0);
          Y : in  std_logic_vector(17 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(17 downto 0)   );
end entity;

architecture arch of IntAdder_18_Freq100_uid48 is
signal Rtmp :  std_logic_vector(17 downto 0);
   -- timing of Rtmp: (c1, 6.220000ns)
signal Cin_d1 :  std_logic;
   -- timing of Cin: (c0, 0.000000ns)
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            Cin_d1 <=  Cin;
         end if;
      end process;
   Rtmp <= X + Y + Cin_d1;
   R <= Rtmp;
end architecture;

--------------------------------------------------------------------------------
--                         IntAdder_18_Freq100_uid52
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 1 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y Cin
-- Output signals: R
--  approx. input signal timings: X: (c1, 5.992000ns)Y: (c0, 0.000000ns)Cin: (c0, 0.000000ns)
--  approx. output signal timings: R: (c1, 7.588000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_18_Freq100_uid52 is
    port (clk : in std_logic;
          X : in  std_logic_vector(17 downto 0);
          Y : in  std_logic_vector(17 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(17 downto 0)   );
end entity;

architecture arch of IntAdder_18_Freq100_uid52 is
signal Rtmp :  std_logic_vector(17 downto 0);
   -- timing of Rtmp: (c1, 7.588000ns)
signal Y_d1 :  std_logic_vector(17 downto 0);
   -- timing of Y: (c0, 0.000000ns)
signal Cin_d1 :  std_logic;
   -- timing of Cin: (c0, 0.000000ns)
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            Y_d1 <=  Y;
            Cin_d1 <=  Cin;
         end if;
      end process;
   Rtmp <= X + Y_d1 + Cin_d1;
   R <= Rtmp;
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x3_Freq100_uid58
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid58 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid58 is
   component MultTable_Freq100_uid60 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy61 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy61: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid60
      port map ( X => Xtable,
                 Y => Y1_copy61);
   Y1 <= Y1_copy61; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x3_Freq100_uid63
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid63 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid63 is
   component MultTable_Freq100_uid65 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy66 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy66: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid65
      port map ( X => Xtable,
                 Y => Y1_copy66);
   Y1 <= Y1_copy66; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x3_Freq100_uid68
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid68 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid68 is
   component MultTable_Freq100_uid70 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy71 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy71: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid70
      port map ( X => Xtable,
                 Y => Y1_copy71);
   Y1 <= Y1_copy71; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x3_Freq100_uid73
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid73 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid73 is
   component MultTable_Freq100_uid75 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy76 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy76: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid75
      port map ( X => Xtable,
                 Y => Y1_copy76);
   Y1 <= Y1_copy76; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x3_Freq100_uid78
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid78 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid78 is
   component MultTable_Freq100_uid80 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy81 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy81: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid80
      port map ( X => Xtable,
                 Y => Y1_copy81);
   Y1 <= Y1_copy81; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x3_Freq100_uid83
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid83 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid83 is
   component MultTable_Freq100_uid85 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy86 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy86: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid85
      port map ( X => Xtable,
                 Y => Y1_copy86);
   Y1 <= Y1_copy86; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x3_Freq100_uid88
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid88 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid88 is
   component MultTable_Freq100_uid90 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy91 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy91: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid90
      port map ( X => Xtable,
                 Y => Y1_copy91);
   Y1 <= Y1_copy91; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x3_Freq100_uid93
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid93 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid93 is
   component MultTable_Freq100_uid95 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy96 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy96: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid95
      port map ( X => Xtable,
                 Y => Y1_copy96);
   Y1 <= Y1_copy96; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x3_Freq100_uid98
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid98 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid98 is
   component MultTable_Freq100_uid100 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy101 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy101: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid100
      port map ( X => Xtable,
                 Y => Y1_copy101);
   Y1 <= Y1_copy101; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x3_Freq100_uid103
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid103 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid103 is
   component MultTable_Freq100_uid105 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy106 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy106: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid105
      port map ( X => Xtable,
                 Y => Y1_copy106);
   Y1 <= Y1_copy106; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x3_Freq100_uid108
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid108 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid108 is
   component MultTable_Freq100_uid110 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy111 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy111: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid110
      port map ( X => Xtable,
                 Y => Y1_copy111);
   Y1 <= Y1_copy111; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x3_Freq100_uid113
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid113 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid113 is
   component MultTable_Freq100_uid115 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy116 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy116: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid115
      port map ( X => Xtable,
                 Y => Y1_copy116);
   Y1 <= Y1_copy116; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x3_Freq100_uid118
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid118 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid118 is
   component MultTable_Freq100_uid120 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy121 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy121: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid120
      port map ( X => Xtable,
                 Y => Y1_copy121);
   Y1 <= Y1_copy121; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x3_Freq100_uid123
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid123 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid123 is
   component MultTable_Freq100_uid125 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy126 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy126: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid125
      port map ( X => Xtable,
                 Y => Y1_copy126);
   Y1 <= Y1_copy126; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x3_Freq100_uid128
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid128 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid128 is
   component MultTable_Freq100_uid130 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy131 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy131: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid130
      port map ( X => Xtable,
                 Y => Y1_copy131);
   Y1 <= Y1_copy131; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x3_Freq100_uid133
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid133 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid133 is
   component MultTable_Freq100_uid135 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy136 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy136: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid135
      port map ( X => Xtable,
                 Y => Y1_copy136);
   Y1 <= Y1_copy136; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x3_Freq100_uid138
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid138 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid138 is
   component MultTable_Freq100_uid140 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy141 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy141: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid140
      port map ( X => Xtable,
                 Y => Y1_copy141);
   Y1 <= Y1_copy141; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x3_Freq100_uid143
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid143 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid143 is
   component MultTable_Freq100_uid145 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy146 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy146: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid145
      port map ( X => Xtable,
                 Y => Y1_copy146);
   Y1 <= Y1_copy146; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x3_Freq100_uid148
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid148 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid148 is
   component MultTable_Freq100_uid150 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy151 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy151: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid150
      port map ( X => Xtable,
                 Y => Y1_copy151);
   Y1 <= Y1_copy151; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x3_Freq100_uid153
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x3_Freq100_uid153 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x3_Freq100_uid153 is
   component MultTable_Freq100_uid155 is
      port ( X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(5 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(5 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy156 :  std_logic_vector(5 downto 0);
   -- timing of Y1_copy156: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid155
      port map ( X => Xtable,
                 Y => Y1_copy156);
   Y1 <= Y1_copy156; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_2x3_Freq100_uid158
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_2x3_Freq100_uid158 is
    port (clk : in std_logic;
          X : in  std_logic_vector(1 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_2x3_Freq100_uid158 is
   component MultTable_Freq100_uid160 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(4 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy161 :  std_logic_vector(4 downto 0);
   -- timing of Y1_copy161: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid160
      port map ( X => Xtable,
                 Y => Y1_copy161);
   Y1 <= Y1_copy161; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_1x2_Freq100_uid163
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq100_uid163 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq100_uid163 is
signal replicated :  std_logic_vector(1 downto 0);
   -- timing of replicated: (c1, 7.588000ns)
signal prod :  std_logic_vector(1 downto 0);
   -- timing of prod: (c1, 8.212000ns)
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_2x3_Freq100_uid165
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_2x3_Freq100_uid165 is
    port (clk : in std_logic;
          X : in  std_logic_vector(1 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_2x3_Freq100_uid165 is
   component MultTable_Freq100_uid167 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(4 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy168 :  std_logic_vector(4 downto 0);
   -- timing of Y1_copy168: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid167
      port map ( X => Xtable,
                 Y => Y1_copy168);
   Y1 <= Y1_copy168; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_2x3_Freq100_uid170
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_2x3_Freq100_uid170 is
    port (clk : in std_logic;
          X : in  std_logic_vector(1 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_2x3_Freq100_uid170 is
   component MultTable_Freq100_uid172 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(4 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy173 :  std_logic_vector(4 downto 0);
   -- timing of Y1_copy173: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid172
      port map ( X => Xtable,
                 Y => Y1_copy173);
   Y1 <= Y1_copy173; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_2x3_Freq100_uid175
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_2x3_Freq100_uid175 is
    port (clk : in std_logic;
          X : in  std_logic_vector(1 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_2x3_Freq100_uid175 is
   component MultTable_Freq100_uid177 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(4 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy178 :  std_logic_vector(4 downto 0);
   -- timing of Y1_copy178: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid177
      port map ( X => Xtable,
                 Y => Y1_copy178);
   Y1 <= Y1_copy178; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_2x3_Freq100_uid180
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_2x3_Freq100_uid180 is
    port (clk : in std_logic;
          X : in  std_logic_vector(1 downto 0);
          Y : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_2x3_Freq100_uid180 is
   component MultTable_Freq100_uid182 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
   -- timing of Xtable: (c1, 7.588000ns)
signal Y1 :  std_logic_vector(4 downto 0);
   -- timing of Y1: (c1, 8.212000ns)
signal Y1_copy183 :  std_logic_vector(4 downto 0);
   -- timing of Y1_copy183: (c1, 7.588000ns)
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq100_uid182
      port map ( X => Xtable,
                 Y => Y1_copy183);
   Y1 <= Y1_copy183; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_1x2_Freq100_uid185
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq100_uid185 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq100_uid185 is
signal replicated :  std_logic_vector(1 downto 0);
   -- timing of replicated: (c1, 7.588000ns)
signal prod :  std_logic_vector(1 downto 0);
   -- timing of prod: (c1, 8.212000ns)
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_1x1_Freq100_uid187
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x1_Freq100_uid187 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(0 downto 0);
          R : out  std_logic_vector(0 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x1_Freq100_uid187 is
signal replicated :  std_logic_vector(0 downto 0);
   -- timing of replicated: (c1, 7.588000ns)
signal prod :  std_logic_vector(0 downto 0);
   -- timing of prod: (c1, 8.212000ns)
begin
   replicated <= (0 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_1x1_Freq100_uid189
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x1_Freq100_uid189 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(0 downto 0);
          R : out  std_logic_vector(0 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x1_Freq100_uid189 is
signal replicated :  std_logic_vector(0 downto 0);
   -- timing of replicated: (c1, 7.588000ns)
signal prod :  std_logic_vector(0 downto 0);
   -- timing of prod: (c1, 8.212000ns)
begin
   replicated <= (0 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_1x2_Freq100_uid191
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq100_uid191 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq100_uid191 is
signal replicated :  std_logic_vector(1 downto 0);
   -- timing of replicated: (c1, 7.588000ns)
signal prod :  std_logic_vector(1 downto 0);
   -- timing of prod: (c1, 8.212000ns)
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_1x2_Freq100_uid193
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq100_uid193 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq100_uid193 is
signal replicated :  std_logic_vector(1 downto 0);
   -- timing of replicated: (c1, 7.588000ns)
signal prod :  std_logic_vector(1 downto 0);
   -- timing of prod: (c1, 8.212000ns)
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_1x2_Freq100_uid195
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq100_uid195 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq100_uid195 is
signal replicated :  std_logic_vector(1 downto 0);
   -- timing of replicated: (c1, 7.588000ns)
signal prod :  std_logic_vector(1 downto 0);
   -- timing of prod: (c1, 8.212000ns)
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_1x1_Freq100_uid197
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x1_Freq100_uid197 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(0 downto 0);
          R : out  std_logic_vector(0 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x1_Freq100_uid197 is
signal replicated :  std_logic_vector(0 downto 0);
   -- timing of replicated: (c1, 7.588000ns)
signal prod :  std_logic_vector(0 downto 0);
   -- timing of prod: (c1, 8.212000ns)
begin
   replicated <= (0 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_1x1_Freq100_uid199
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x1_Freq100_uid199 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(0 downto 0);
          R : out  std_logic_vector(0 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x1_Freq100_uid199 is
signal replicated :  std_logic_vector(0 downto 0);
   -- timing of replicated: (c1, 7.588000ns)
signal prod :  std_logic_vector(0 downto 0);
   -- timing of prod: (c1, 8.212000ns)
begin
   replicated <= (0 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_1x1_Freq100_uid201
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c1, 8.212000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x1_Freq100_uid201 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(0 downto 0);
          R : out  std_logic_vector(0 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x1_Freq100_uid201 is
signal replicated :  std_logic_vector(0 downto 0);
   -- timing of replicated: (c1, 7.588000ns)
signal prod :  std_logic_vector(0 downto 0);
   -- timing of prod: (c1, 8.212000ns)
begin
   replicated <= (0 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                         IntAdder_19_Freq100_uid354
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 2 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y Cin
-- Output signals: R
--  approx. input signal timings: X: (c2, 1.226000ns)Y: (c2, 1.226000ns)Cin: (c0, 0.000000ns)
--  approx. output signal timings: R: (c2, 2.822000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_19_Freq100_uid354 is
    port (clk : in std_logic;
          X : in  std_logic_vector(18 downto 0);
          Y : in  std_logic_vector(18 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(18 downto 0)   );
end entity;

architecture arch of IntAdder_19_Freq100_uid354 is
signal Rtmp :  std_logic_vector(18 downto 0);
   -- timing of Rtmp: (c2, 2.822000ns)
signal Cin_d1, Cin_d2 :  std_logic;
   -- timing of Cin: (c0, 0.000000ns)
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            Cin_d1 <=  Cin;
            Cin_d2 <=  Cin_d1;
         end if;
      end process;
   Rtmp <= X + Y + Cin_d2;
   R <= Rtmp;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplier_17x18_19_Freq100_uid54
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Martin Kumm, Florent de Dinechin, Andreas Böttcher, Kinga Illyes, Bogdan Popa, Bogdan Pasca, 2012-
--------------------------------------------------------------------------------
-- Pipeline depth: 1 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: (c1, 7.588000ns)Y: (c1, 6.220000ns)
--  approx. output signal timings: R: (c2, 2.822000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity IntMultiplier_17x18_19_Freq100_uid54 is
    port (clk : in std_logic;
          X : in  std_logic_vector(16 downto 0);
          Y : in  std_logic_vector(17 downto 0);
          R : out  std_logic_vector(18 downto 0)   );
end entity;

architecture arch of IntMultiplier_17x18_19_Freq100_uid54 is
   component IntMultiplierLUT_3x3_Freq100_uid58 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid63 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid68 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid73 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid78 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid83 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid88 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid93 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid98 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid103 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid108 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid113 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid118 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid123 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid128 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid133 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid138 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid143 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid148 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_3x3_Freq100_uid153 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntMultiplierLUT_2x3_Freq100_uid158 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(1 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq100_uid163 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_2x3_Freq100_uid165 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(1 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_2x3_Freq100_uid170 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(1 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_2x3_Freq100_uid175 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(1 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_2x3_Freq100_uid180 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(1 downto 0);
             Y : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq100_uid185 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_1x1_Freq100_uid187 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(0 downto 0);
             R : out  std_logic_vector(0 downto 0)   );
   end component;

   component IntMultiplierLUT_1x1_Freq100_uid189 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(0 downto 0);
             R : out  std_logic_vector(0 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq100_uid191 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq100_uid193 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq100_uid195 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_1x1_Freq100_uid197 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(0 downto 0);
             R : out  std_logic_vector(0 downto 0)   );
   end component;

   component IntMultiplierLUT_1x1_Freq100_uid199 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(0 downto 0);
             R : out  std_logic_vector(0 downto 0)   );
   end component;

   component IntMultiplierLUT_1x1_Freq100_uid201 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(0 downto 0);
             R : out  std_logic_vector(0 downto 0)   );
   end component;

   component Compressor_6_3_Freq100_uid204 is
      port ( X0 : in  std_logic_vector(5 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_14_3_Freq100_uid208 is
      port ( X1 : in  std_logic_vector(0 downto 0);
             X0 : in  std_logic_vector(3 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_3_2_Freq100_uid214 is
      port ( X0 : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component Compressor_23_3_Freq100_uid248 is
      port ( X1 : in  std_logic_vector(1 downto 0);
             X0 : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_5_3_Freq100_uid272 is
      port ( X0 : in  std_logic_vector(4 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component IntAdder_19_Freq100_uid354 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(18 downto 0);
             Y : in  std_logic_vector(18 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(18 downto 0)   );
   end component;

signal XX_m55 :  std_logic_vector(16 downto 0);
   -- timing of XX_m55: (c1, 7.588000ns)
signal YY_m55 :  std_logic_vector(17 downto 0);
   -- timing of YY_m55: (c1, 6.220000ns)
signal tile_0_X :  std_logic_vector(2 downto 0);
   -- timing of tile_0_X: (c1, 7.588000ns)
signal tile_0_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_0_Y: (c1, 6.220000ns)
signal tile_0_output :  std_logic_vector(5 downto 0);
   -- timing of tile_0_output: (c1, 8.212000ns)
signal tile_0_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_0_filtered_output: (c1, 8.212000ns)
signal bh56_w29_0 :  std_logic;
   -- timing of bh56_w29_0: (c1, 8.212000ns)
signal bh56_w30_0 :  std_logic;
   -- timing of bh56_w30_0: (c1, 8.212000ns)
signal bh56_w31_0 :  std_logic;
   -- timing of bh56_w31_0: (c1, 8.212000ns)
signal bh56_w32_0 :  std_logic;
   -- timing of bh56_w32_0: (c1, 8.212000ns)
signal bh56_w33_0 :  std_logic;
   -- timing of bh56_w33_0: (c1, 8.212000ns)
signal bh56_w34_0 :  std_logic;
   -- timing of bh56_w34_0: (c1, 8.212000ns)
signal tile_1_X :  std_logic_vector(2 downto 0);
   -- timing of tile_1_X: (c1, 7.588000ns)
signal tile_1_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_1_Y: (c1, 6.220000ns)
signal tile_1_output :  std_logic_vector(5 downto 0);
   -- timing of tile_1_output: (c1, 8.212000ns)
signal tile_1_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_1_filtered_output: (c1, 8.212000ns)
signal bh56_w26_0 :  std_logic;
   -- timing of bh56_w26_0: (c1, 8.212000ns)
signal bh56_w27_0 :  std_logic;
   -- timing of bh56_w27_0: (c1, 8.212000ns)
signal bh56_w28_0 :  std_logic;
   -- timing of bh56_w28_0: (c1, 8.212000ns)
signal bh56_w29_1 :  std_logic;
   -- timing of bh56_w29_1: (c1, 8.212000ns)
signal bh56_w30_1 :  std_logic;
   -- timing of bh56_w30_1: (c1, 8.212000ns)
signal bh56_w31_1 :  std_logic;
   -- timing of bh56_w31_1: (c1, 8.212000ns)
signal tile_2_X :  std_logic_vector(2 downto 0);
   -- timing of tile_2_X: (c1, 7.588000ns)
signal tile_2_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_2_Y: (c1, 6.220000ns)
signal tile_2_output :  std_logic_vector(5 downto 0);
   -- timing of tile_2_output: (c1, 8.212000ns)
signal tile_2_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_2_filtered_output: (c1, 8.212000ns)
signal bh56_w26_1 :  std_logic;
   -- timing of bh56_w26_1: (c1, 8.212000ns)
signal bh56_w27_1 :  std_logic;
   -- timing of bh56_w27_1: (c1, 8.212000ns)
signal bh56_w28_1 :  std_logic;
   -- timing of bh56_w28_1: (c1, 8.212000ns)
signal bh56_w29_2 :  std_logic;
   -- timing of bh56_w29_2: (c1, 8.212000ns)
signal bh56_w30_2 :  std_logic;
   -- timing of bh56_w30_2: (c1, 8.212000ns)
signal bh56_w31_2 :  std_logic;
   -- timing of bh56_w31_2: (c1, 8.212000ns)
signal tile_3_X :  std_logic_vector(2 downto 0);
   -- timing of tile_3_X: (c1, 7.588000ns)
signal tile_3_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_3_Y: (c1, 6.220000ns)
signal tile_3_output :  std_logic_vector(5 downto 0);
   -- timing of tile_3_output: (c1, 8.212000ns)
signal tile_3_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_3_filtered_output: (c1, 8.212000ns)
signal bh56_w23_0 :  std_logic;
   -- timing of bh56_w23_0: (c1, 8.212000ns)
signal bh56_w24_0 :  std_logic;
   -- timing of bh56_w24_0: (c1, 8.212000ns)
signal bh56_w25_0 :  std_logic;
   -- timing of bh56_w25_0: (c1, 8.212000ns)
signal bh56_w26_2 :  std_logic;
   -- timing of bh56_w26_2: (c1, 8.212000ns)
signal bh56_w27_2 :  std_logic;
   -- timing of bh56_w27_2: (c1, 8.212000ns)
signal bh56_w28_2 :  std_logic;
   -- timing of bh56_w28_2: (c1, 8.212000ns)
signal tile_4_X :  std_logic_vector(2 downto 0);
   -- timing of tile_4_X: (c1, 7.588000ns)
signal tile_4_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_4_Y: (c1, 6.220000ns)
signal tile_4_output :  std_logic_vector(5 downto 0);
   -- timing of tile_4_output: (c1, 8.212000ns)
signal tile_4_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_4_filtered_output: (c1, 8.212000ns)
signal bh56_w23_1 :  std_logic;
   -- timing of bh56_w23_1: (c1, 8.212000ns)
signal bh56_w24_1 :  std_logic;
   -- timing of bh56_w24_1: (c1, 8.212000ns)
signal bh56_w25_1 :  std_logic;
   -- timing of bh56_w25_1: (c1, 8.212000ns)
signal bh56_w26_3 :  std_logic;
   -- timing of bh56_w26_3: (c1, 8.212000ns)
signal bh56_w27_3 :  std_logic;
   -- timing of bh56_w27_3: (c1, 8.212000ns)
signal bh56_w28_3 :  std_logic;
   -- timing of bh56_w28_3: (c1, 8.212000ns)
signal tile_5_X :  std_logic_vector(2 downto 0);
   -- timing of tile_5_X: (c1, 7.588000ns)
signal tile_5_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_5_Y: (c1, 6.220000ns)
signal tile_5_output :  std_logic_vector(5 downto 0);
   -- timing of tile_5_output: (c1, 8.212000ns)
signal tile_5_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_5_filtered_output: (c1, 8.212000ns)
signal bh56_w23_2 :  std_logic;
   -- timing of bh56_w23_2: (c1, 8.212000ns)
signal bh56_w24_2 :  std_logic;
   -- timing of bh56_w24_2: (c1, 8.212000ns)
signal bh56_w25_2 :  std_logic;
   -- timing of bh56_w25_2: (c1, 8.212000ns)
signal bh56_w26_4 :  std_logic;
   -- timing of bh56_w26_4: (c1, 8.212000ns)
signal bh56_w27_4 :  std_logic;
   -- timing of bh56_w27_4: (c1, 8.212000ns)
signal bh56_w28_4 :  std_logic;
   -- timing of bh56_w28_4: (c1, 8.212000ns)
signal tile_6_X :  std_logic_vector(2 downto 0);
   -- timing of tile_6_X: (c1, 7.588000ns)
signal tile_6_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_6_Y: (c1, 6.220000ns)
signal tile_6_output :  std_logic_vector(5 downto 0);
   -- timing of tile_6_output: (c1, 8.212000ns)
signal tile_6_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_6_filtered_output: (c1, 8.212000ns)
signal bh56_w20_0 :  std_logic;
   -- timing of bh56_w20_0: (c1, 8.212000ns)
signal bh56_w21_0 :  std_logic;
   -- timing of bh56_w21_0: (c1, 8.212000ns)
signal bh56_w22_0 :  std_logic;
   -- timing of bh56_w22_0: (c1, 8.212000ns)
signal bh56_w23_3 :  std_logic;
   -- timing of bh56_w23_3: (c1, 8.212000ns)
signal bh56_w24_3 :  std_logic;
   -- timing of bh56_w24_3: (c1, 8.212000ns)
signal bh56_w25_3 :  std_logic;
   -- timing of bh56_w25_3: (c1, 8.212000ns)
signal tile_7_X :  std_logic_vector(2 downto 0);
   -- timing of tile_7_X: (c1, 7.588000ns)
signal tile_7_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_7_Y: (c1, 6.220000ns)
signal tile_7_output :  std_logic_vector(5 downto 0);
   -- timing of tile_7_output: (c1, 8.212000ns)
signal tile_7_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_7_filtered_output: (c1, 8.212000ns)
signal bh56_w20_1 :  std_logic;
   -- timing of bh56_w20_1: (c1, 8.212000ns)
signal bh56_w21_1 :  std_logic;
   -- timing of bh56_w21_1: (c1, 8.212000ns)
signal bh56_w22_1 :  std_logic;
   -- timing of bh56_w22_1: (c1, 8.212000ns)
signal bh56_w23_4 :  std_logic;
   -- timing of bh56_w23_4: (c1, 8.212000ns)
signal bh56_w24_4 :  std_logic;
   -- timing of bh56_w24_4: (c1, 8.212000ns)
signal bh56_w25_4 :  std_logic;
   -- timing of bh56_w25_4: (c1, 8.212000ns)
signal tile_8_X :  std_logic_vector(2 downto 0);
   -- timing of tile_8_X: (c1, 7.588000ns)
signal tile_8_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_8_Y: (c1, 6.220000ns)
signal tile_8_output :  std_logic_vector(5 downto 0);
   -- timing of tile_8_output: (c1, 8.212000ns)
signal tile_8_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_8_filtered_output: (c1, 8.212000ns)
signal bh56_w20_2 :  std_logic;
   -- timing of bh56_w20_2: (c1, 8.212000ns)
signal bh56_w21_2 :  std_logic;
   -- timing of bh56_w21_2: (c1, 8.212000ns)
signal bh56_w22_2 :  std_logic;
   -- timing of bh56_w22_2: (c1, 8.212000ns)
signal bh56_w23_5 :  std_logic;
   -- timing of bh56_w23_5: (c1, 8.212000ns)
signal bh56_w24_5 :  std_logic;
   -- timing of bh56_w24_5: (c1, 8.212000ns)
signal bh56_w25_5 :  std_logic;
   -- timing of bh56_w25_5: (c1, 8.212000ns)
signal tile_9_X :  std_logic_vector(2 downto 0);
   -- timing of tile_9_X: (c1, 7.588000ns)
signal tile_9_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_9_Y: (c1, 6.220000ns)
signal tile_9_output :  std_logic_vector(5 downto 0);
   -- timing of tile_9_output: (c1, 8.212000ns)
signal tile_9_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_9_filtered_output: (c1, 8.212000ns)
signal bh56_w20_3 :  std_logic;
   -- timing of bh56_w20_3: (c1, 8.212000ns)
signal bh56_w21_3 :  std_logic;
   -- timing of bh56_w21_3: (c1, 8.212000ns)
signal bh56_w22_3 :  std_logic;
   -- timing of bh56_w22_3: (c1, 8.212000ns)
signal bh56_w23_6 :  std_logic;
   -- timing of bh56_w23_6: (c1, 8.212000ns)
signal bh56_w24_6 :  std_logic;
   -- timing of bh56_w24_6: (c1, 8.212000ns)
signal bh56_w25_6 :  std_logic;
   -- timing of bh56_w25_6: (c1, 8.212000ns)
signal tile_10_X :  std_logic_vector(2 downto 0);
   -- timing of tile_10_X: (c1, 7.588000ns)
signal tile_10_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_10_Y: (c1, 6.220000ns)
signal tile_10_output :  std_logic_vector(5 downto 0);
   -- timing of tile_10_output: (c1, 8.212000ns)
signal tile_10_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_10_filtered_output: (c1, 8.212000ns)
signal bh56_w17_0 :  std_logic;
   -- timing of bh56_w17_0: (c1, 8.212000ns)
signal bh56_w18_0 :  std_logic;
   -- timing of bh56_w18_0: (c1, 8.212000ns)
signal bh56_w19_0 :  std_logic;
   -- timing of bh56_w19_0: (c1, 8.212000ns)
signal bh56_w20_4 :  std_logic;
   -- timing of bh56_w20_4: (c1, 8.212000ns)
signal bh56_w21_4 :  std_logic;
   -- timing of bh56_w21_4: (c1, 8.212000ns)
signal bh56_w22_4 :  std_logic;
   -- timing of bh56_w22_4: (c1, 8.212000ns)
signal tile_11_X :  std_logic_vector(2 downto 0);
   -- timing of tile_11_X: (c1, 7.588000ns)
signal tile_11_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_11_Y: (c1, 6.220000ns)
signal tile_11_output :  std_logic_vector(5 downto 0);
   -- timing of tile_11_output: (c1, 8.212000ns)
signal tile_11_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_11_filtered_output: (c1, 8.212000ns)
signal bh56_w17_1 :  std_logic;
   -- timing of bh56_w17_1: (c1, 8.212000ns)
signal bh56_w18_1 :  std_logic;
   -- timing of bh56_w18_1: (c1, 8.212000ns)
signal bh56_w19_1 :  std_logic;
   -- timing of bh56_w19_1: (c1, 8.212000ns)
signal bh56_w20_5 :  std_logic;
   -- timing of bh56_w20_5: (c1, 8.212000ns)
signal bh56_w21_5 :  std_logic;
   -- timing of bh56_w21_5: (c1, 8.212000ns)
signal bh56_w22_5 :  std_logic;
   -- timing of bh56_w22_5: (c1, 8.212000ns)
signal tile_12_X :  std_logic_vector(2 downto 0);
   -- timing of tile_12_X: (c1, 7.588000ns)
signal tile_12_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_12_Y: (c1, 6.220000ns)
signal tile_12_output :  std_logic_vector(5 downto 0);
   -- timing of tile_12_output: (c1, 8.212000ns)
signal tile_12_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_12_filtered_output: (c1, 8.212000ns)
signal bh56_w17_2 :  std_logic;
   -- timing of bh56_w17_2: (c1, 8.212000ns)
signal bh56_w18_2 :  std_logic;
   -- timing of bh56_w18_2: (c1, 8.212000ns)
signal bh56_w19_2 :  std_logic;
   -- timing of bh56_w19_2: (c1, 8.212000ns)
signal bh56_w20_6 :  std_logic;
   -- timing of bh56_w20_6: (c1, 8.212000ns)
signal bh56_w21_6 :  std_logic;
   -- timing of bh56_w21_6: (c1, 8.212000ns)
signal bh56_w22_6 :  std_logic;
   -- timing of bh56_w22_6: (c1, 8.212000ns)
signal tile_13_X :  std_logic_vector(2 downto 0);
   -- timing of tile_13_X: (c1, 7.588000ns)
signal tile_13_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_13_Y: (c1, 6.220000ns)
signal tile_13_output :  std_logic_vector(5 downto 0);
   -- timing of tile_13_output: (c1, 8.212000ns)
signal tile_13_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_13_filtered_output: (c1, 8.212000ns)
signal bh56_w17_3 :  std_logic;
   -- timing of bh56_w17_3: (c1, 8.212000ns)
signal bh56_w18_3 :  std_logic;
   -- timing of bh56_w18_3: (c1, 8.212000ns)
signal bh56_w19_3 :  std_logic;
   -- timing of bh56_w19_3: (c1, 8.212000ns)
signal bh56_w20_7 :  std_logic;
   -- timing of bh56_w20_7: (c1, 8.212000ns)
signal bh56_w21_7 :  std_logic;
   -- timing of bh56_w21_7: (c1, 8.212000ns)
signal bh56_w22_7 :  std_logic;
   -- timing of bh56_w22_7: (c1, 8.212000ns)
signal tile_14_X :  std_logic_vector(2 downto 0);
   -- timing of tile_14_X: (c1, 7.588000ns)
signal tile_14_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_14_Y: (c1, 6.220000ns)
signal tile_14_output :  std_logic_vector(5 downto 0);
   -- timing of tile_14_output: (c1, 8.212000ns)
signal tile_14_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_14_filtered_output: (c1, 8.212000ns)
signal bh56_w17_4 :  std_logic;
   -- timing of bh56_w17_4: (c1, 8.212000ns)
signal bh56_w18_4 :  std_logic;
   -- timing of bh56_w18_4: (c1, 8.212000ns)
signal bh56_w19_4 :  std_logic;
   -- timing of bh56_w19_4: (c1, 8.212000ns)
signal bh56_w20_8 :  std_logic;
   -- timing of bh56_w20_8: (c1, 8.212000ns)
signal bh56_w21_8 :  std_logic;
   -- timing of bh56_w21_8: (c1, 8.212000ns)
signal bh56_w22_8 :  std_logic;
   -- timing of bh56_w22_8: (c1, 8.212000ns)
signal tile_15_X :  std_logic_vector(2 downto 0);
   -- timing of tile_15_X: (c1, 7.588000ns)
signal tile_15_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_15_Y: (c1, 6.220000ns)
signal tile_15_output :  std_logic_vector(5 downto 0);
   -- timing of tile_15_output: (c1, 8.212000ns)
signal tile_15_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_15_filtered_output: (c1, 8.212000ns)
signal bh56_w14_0 :  std_logic;
   -- timing of bh56_w14_0: (c1, 8.212000ns)
signal bh56_w15_0 :  std_logic;
   -- timing of bh56_w15_0: (c1, 8.212000ns)
signal bh56_w16_0 :  std_logic;
   -- timing of bh56_w16_0: (c1, 8.212000ns)
signal bh56_w17_5 :  std_logic;
   -- timing of bh56_w17_5: (c1, 8.212000ns)
signal bh56_w18_5 :  std_logic;
   -- timing of bh56_w18_5: (c1, 8.212000ns)
signal bh56_w19_5 :  std_logic;
   -- timing of bh56_w19_5: (c1, 8.212000ns)
signal tile_16_X :  std_logic_vector(2 downto 0);
   -- timing of tile_16_X: (c1, 7.588000ns)
signal tile_16_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_16_Y: (c1, 6.220000ns)
signal tile_16_output :  std_logic_vector(5 downto 0);
   -- timing of tile_16_output: (c1, 8.212000ns)
signal tile_16_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_16_filtered_output: (c1, 8.212000ns)
signal bh56_w14_1 :  std_logic;
   -- timing of bh56_w14_1: (c1, 8.212000ns)
signal bh56_w15_1 :  std_logic;
   -- timing of bh56_w15_1: (c1, 8.212000ns)
signal bh56_w16_1 :  std_logic;
   -- timing of bh56_w16_1: (c1, 8.212000ns)
signal bh56_w17_6 :  std_logic;
   -- timing of bh56_w17_6: (c1, 8.212000ns)
signal bh56_w18_6 :  std_logic;
   -- timing of bh56_w18_6: (c1, 8.212000ns)
signal bh56_w19_6 :  std_logic;
   -- timing of bh56_w19_6: (c1, 8.212000ns)
signal tile_17_X :  std_logic_vector(2 downto 0);
   -- timing of tile_17_X: (c1, 7.588000ns)
signal tile_17_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_17_Y: (c1, 6.220000ns)
signal tile_17_output :  std_logic_vector(5 downto 0);
   -- timing of tile_17_output: (c1, 8.212000ns)
signal tile_17_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_17_filtered_output: (c1, 8.212000ns)
signal bh56_w14_2 :  std_logic;
   -- timing of bh56_w14_2: (c1, 8.212000ns)
signal bh56_w15_2 :  std_logic;
   -- timing of bh56_w15_2: (c1, 8.212000ns)
signal bh56_w16_2 :  std_logic;
   -- timing of bh56_w16_2: (c1, 8.212000ns)
signal bh56_w17_7 :  std_logic;
   -- timing of bh56_w17_7: (c1, 8.212000ns)
signal bh56_w18_7 :  std_logic;
   -- timing of bh56_w18_7: (c1, 8.212000ns)
signal bh56_w19_7 :  std_logic;
   -- timing of bh56_w19_7: (c1, 8.212000ns)
signal tile_18_X :  std_logic_vector(2 downto 0);
   -- timing of tile_18_X: (c1, 7.588000ns)
signal tile_18_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_18_Y: (c1, 6.220000ns)
signal tile_18_output :  std_logic_vector(5 downto 0);
   -- timing of tile_18_output: (c1, 8.212000ns)
signal tile_18_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_18_filtered_output: (c1, 8.212000ns)
signal bh56_w14_3 :  std_logic;
   -- timing of bh56_w14_3: (c1, 8.212000ns)
signal bh56_w15_3 :  std_logic;
   -- timing of bh56_w15_3: (c1, 8.212000ns)
signal bh56_w16_3 :  std_logic;
   -- timing of bh56_w16_3: (c1, 8.212000ns)
signal bh56_w17_8 :  std_logic;
   -- timing of bh56_w17_8: (c1, 8.212000ns)
signal bh56_w18_8 :  std_logic;
   -- timing of bh56_w18_8: (c1, 8.212000ns)
signal bh56_w19_8 :  std_logic;
   -- timing of bh56_w19_8: (c1, 8.212000ns)
signal tile_19_X :  std_logic_vector(2 downto 0);
   -- timing of tile_19_X: (c1, 7.588000ns)
signal tile_19_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_19_Y: (c1, 6.220000ns)
signal tile_19_output :  std_logic_vector(5 downto 0);
   -- timing of tile_19_output: (c1, 8.212000ns)
signal tile_19_filtered_output :  unsigned(5-0 downto 0);
   -- timing of tile_19_filtered_output: (c1, 8.212000ns)
signal bh56_w14_4 :  std_logic;
   -- timing of bh56_w14_4: (c1, 8.212000ns)
signal bh56_w15_4 :  std_logic;
   -- timing of bh56_w15_4: (c1, 8.212000ns)
signal bh56_w16_4 :  std_logic;
   -- timing of bh56_w16_4: (c1, 8.212000ns)
signal bh56_w17_9 :  std_logic;
   -- timing of bh56_w17_9: (c1, 8.212000ns)
signal bh56_w18_9 :  std_logic;
   -- timing of bh56_w18_9: (c1, 8.212000ns)
signal bh56_w19_9 :  std_logic;
   -- timing of bh56_w19_9: (c1, 8.212000ns)
signal tile_20_X :  std_logic_vector(1 downto 0);
   -- timing of tile_20_X: (c1, 7.588000ns)
signal tile_20_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_20_Y: (c1, 6.220000ns)
signal tile_20_output :  std_logic_vector(4 downto 0);
   -- timing of tile_20_output: (c1, 8.212000ns)
signal tile_20_filtered_output :  unsigned(4-0 downto 0);
   -- timing of tile_20_filtered_output: (c1, 8.212000ns)
signal bh56_w15_5 :  std_logic;
   -- timing of bh56_w15_5: (c1, 8.212000ns)
signal bh56_w16_5 :  std_logic;
   -- timing of bh56_w16_5: (c1, 8.212000ns)
signal bh56_w17_10 :  std_logic;
   -- timing of bh56_w17_10: (c1, 8.212000ns)
signal bh56_w18_10 :  std_logic;
   -- timing of bh56_w18_10: (c1, 8.212000ns)
signal bh56_w19_10 :  std_logic;
   -- timing of bh56_w19_10: (c1, 8.212000ns)
signal tile_21_X :  std_logic_vector(0 downto 0);
   -- timing of tile_21_X: (c1, 7.588000ns)
signal tile_21_Y :  std_logic_vector(1 downto 0);
   -- timing of tile_21_Y: (c1, 6.220000ns)
signal tile_21_output :  std_logic_vector(1 downto 0);
   -- timing of tile_21_output: (c1, 8.212000ns)
signal tile_21_filtered_output :  unsigned(1-0 downto 0);
   -- timing of tile_21_filtered_output: (c1, 8.212000ns)
signal bh56_w14_5 :  std_logic;
   -- timing of bh56_w14_5: (c1, 8.212000ns)
signal bh56_w15_6 :  std_logic;
   -- timing of bh56_w15_6: (c1, 8.212000ns)
signal tile_22_X :  std_logic_vector(1 downto 0);
   -- timing of tile_22_X: (c1, 7.588000ns)
signal tile_22_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_22_Y: (c1, 6.220000ns)
signal tile_22_output :  std_logic_vector(4 downto 0);
   -- timing of tile_22_output: (c1, 8.212000ns)
signal tile_22_filtered_output :  unsigned(4-0 downto 0);
   -- timing of tile_22_filtered_output: (c1, 8.212000ns)
signal bh56_w12_0 :  std_logic;
   -- timing of bh56_w12_0: (c1, 8.212000ns)
signal bh56_w13_0 :  std_logic;
   -- timing of bh56_w13_0: (c1, 8.212000ns)
signal bh56_w14_6 :  std_logic;
   -- timing of bh56_w14_6: (c1, 8.212000ns)
signal bh56_w15_7 :  std_logic;
   -- timing of bh56_w15_7: (c1, 8.212000ns)
signal bh56_w16_6 :  std_logic;
   -- timing of bh56_w16_6: (c1, 8.212000ns)
signal tile_23_X :  std_logic_vector(1 downto 0);
   -- timing of tile_23_X: (c1, 7.588000ns)
signal tile_23_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_23_Y: (c1, 6.220000ns)
signal tile_23_output :  std_logic_vector(4 downto 0);
   -- timing of tile_23_output: (c1, 8.212000ns)
signal tile_23_filtered_output :  unsigned(4-0 downto 0);
   -- timing of tile_23_filtered_output: (c1, 8.212000ns)
signal bh56_w12_1 :  std_logic;
   -- timing of bh56_w12_1: (c1, 8.212000ns)
signal bh56_w13_1 :  std_logic;
   -- timing of bh56_w13_1: (c1, 8.212000ns)
signal bh56_w14_7 :  std_logic;
   -- timing of bh56_w14_7: (c1, 8.212000ns)
signal bh56_w15_8 :  std_logic;
   -- timing of bh56_w15_8: (c1, 8.212000ns)
signal bh56_w16_7 :  std_logic;
   -- timing of bh56_w16_7: (c1, 8.212000ns)
signal tile_24_X :  std_logic_vector(1 downto 0);
   -- timing of tile_24_X: (c1, 7.588000ns)
signal tile_24_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_24_Y: (c1, 6.220000ns)
signal tile_24_output :  std_logic_vector(4 downto 0);
   -- timing of tile_24_output: (c1, 8.212000ns)
signal tile_24_filtered_output :  unsigned(4-0 downto 0);
   -- timing of tile_24_filtered_output: (c1, 8.212000ns)
signal bh56_w12_2 :  std_logic;
   -- timing of bh56_w12_2: (c1, 8.212000ns)
signal bh56_w13_2 :  std_logic;
   -- timing of bh56_w13_2: (c1, 8.212000ns)
signal bh56_w14_8 :  std_logic;
   -- timing of bh56_w14_8: (c1, 8.212000ns)
signal bh56_w15_9 :  std_logic;
   -- timing of bh56_w15_9: (c1, 8.212000ns)
signal bh56_w16_8 :  std_logic;
   -- timing of bh56_w16_8: (c1, 8.212000ns)
signal tile_25_X :  std_logic_vector(1 downto 0);
   -- timing of tile_25_X: (c1, 7.588000ns)
signal tile_25_Y :  std_logic_vector(2 downto 0);
   -- timing of tile_25_Y: (c1, 6.220000ns)
signal tile_25_output :  std_logic_vector(4 downto 0);
   -- timing of tile_25_output: (c1, 8.212000ns)
signal tile_25_filtered_output :  unsigned(4-0 downto 0);
   -- timing of tile_25_filtered_output: (c1, 8.212000ns)
signal bh56_w12_3 :  std_logic;
   -- timing of bh56_w12_3: (c1, 8.212000ns)
signal bh56_w13_3 :  std_logic;
   -- timing of bh56_w13_3: (c1, 8.212000ns)
signal bh56_w14_9 :  std_logic;
   -- timing of bh56_w14_9: (c1, 8.212000ns)
signal bh56_w15_10 :  std_logic;
   -- timing of bh56_w15_10: (c1, 8.212000ns)
signal bh56_w16_9 :  std_logic;
   -- timing of bh56_w16_9: (c1, 8.212000ns)
signal tile_26_X :  std_logic_vector(0 downto 0);
   -- timing of tile_26_X: (c1, 7.588000ns)
signal tile_26_Y :  std_logic_vector(1 downto 0);
   -- timing of tile_26_Y: (c1, 6.220000ns)
signal tile_26_output :  std_logic_vector(1 downto 0);
   -- timing of tile_26_output: (c1, 8.212000ns)
signal tile_26_filtered_output :  unsigned(1-0 downto 0);
   -- timing of tile_26_filtered_output: (c1, 8.212000ns)
signal bh56_w13_4 :  std_logic;
   -- timing of bh56_w13_4: (c1, 8.212000ns)
signal bh56_w14_10 :  std_logic;
   -- timing of bh56_w14_10: (c1, 8.212000ns)
signal tile_27_X :  std_logic_vector(0 downto 0);
   -- timing of tile_27_X: (c1, 7.588000ns)
signal tile_27_Y :  std_logic_vector(0 downto 0);
   -- timing of tile_27_Y: (c1, 6.220000ns)
signal tile_27_output :  std_logic_vector(0 downto 0);
   -- timing of tile_27_output: (c1, 8.212000ns)
signal tile_27_filtered_output :  unsigned(0-0 downto 0);
   -- timing of tile_27_filtered_output: (c1, 8.212000ns)
signal bh56_w13_5 :  std_logic;
   -- timing of bh56_w13_5: (c1, 8.212000ns)
signal tile_28_X :  std_logic_vector(0 downto 0);
   -- timing of tile_28_X: (c1, 7.588000ns)
signal tile_28_Y :  std_logic_vector(0 downto 0);
   -- timing of tile_28_Y: (c1, 6.220000ns)
signal tile_28_output :  std_logic_vector(0 downto 0);
   -- timing of tile_28_output: (c1, 8.212000ns)
signal tile_28_filtered_output :  unsigned(0-0 downto 0);
   -- timing of tile_28_filtered_output: (c1, 8.212000ns)
signal bh56_w13_6 :  std_logic;
   -- timing of bh56_w13_6: (c1, 8.212000ns)
signal tile_29_X :  std_logic_vector(0 downto 0);
   -- timing of tile_29_X: (c1, 7.588000ns)
signal tile_29_Y :  std_logic_vector(1 downto 0);
   -- timing of tile_29_Y: (c1, 6.220000ns)
signal tile_29_output :  std_logic_vector(1 downto 0);
   -- timing of tile_29_output: (c1, 8.212000ns)
signal tile_29_filtered_output :  unsigned(1-0 downto 0);
   -- timing of tile_29_filtered_output: (c1, 8.212000ns)
signal bh56_w12_4 :  std_logic;
   -- timing of bh56_w12_4: (c1, 8.212000ns)
signal bh56_w13_7 :  std_logic;
   -- timing of bh56_w13_7: (c1, 8.212000ns)
signal tile_30_X :  std_logic_vector(0 downto 0);
   -- timing of tile_30_X: (c1, 7.588000ns)
signal tile_30_Y :  std_logic_vector(1 downto 0);
   -- timing of tile_30_Y: (c1, 6.220000ns)
signal tile_30_output :  std_logic_vector(1 downto 0);
   -- timing of tile_30_output: (c1, 8.212000ns)
signal tile_30_filtered_output :  unsigned(1-0 downto 0);
   -- timing of tile_30_filtered_output: (c1, 8.212000ns)
signal bh56_w12_5 :  std_logic;
   -- timing of bh56_w12_5: (c1, 8.212000ns)
signal bh56_w13_8 :  std_logic;
   -- timing of bh56_w13_8: (c1, 8.212000ns)
signal tile_31_X :  std_logic_vector(0 downto 0);
   -- timing of tile_31_X: (c1, 7.588000ns)
signal tile_31_Y :  std_logic_vector(1 downto 0);
   -- timing of tile_31_Y: (c1, 6.220000ns)
signal tile_31_output :  std_logic_vector(1 downto 0);
   -- timing of tile_31_output: (c1, 8.212000ns)
signal tile_31_filtered_output :  unsigned(1-0 downto 0);
   -- timing of tile_31_filtered_output: (c1, 8.212000ns)
signal bh56_w12_6 :  std_logic;
   -- timing of bh56_w12_6: (c1, 8.212000ns)
signal bh56_w13_9 :  std_logic;
   -- timing of bh56_w13_9: (c1, 8.212000ns)
signal tile_32_X :  std_logic_vector(0 downto 0);
   -- timing of tile_32_X: (c1, 7.588000ns)
signal tile_32_Y :  std_logic_vector(0 downto 0);
   -- timing of tile_32_Y: (c1, 6.220000ns)
signal tile_32_output :  std_logic_vector(0 downto 0);
   -- timing of tile_32_output: (c1, 8.212000ns)
signal tile_32_filtered_output :  unsigned(0-0 downto 0);
   -- timing of tile_32_filtered_output: (c1, 8.212000ns)
signal bh56_w12_7 :  std_logic;
   -- timing of bh56_w12_7: (c1, 8.212000ns)
signal tile_33_X :  std_logic_vector(0 downto 0);
   -- timing of tile_33_X: (c1, 7.588000ns)
signal tile_33_Y :  std_logic_vector(0 downto 0);
   -- timing of tile_33_Y: (c1, 6.220000ns)
signal tile_33_output :  std_logic_vector(0 downto 0);
   -- timing of tile_33_output: (c1, 8.212000ns)
signal tile_33_filtered_output :  unsigned(0-0 downto 0);
   -- timing of tile_33_filtered_output: (c1, 8.212000ns)
signal bh56_w12_8 :  std_logic;
   -- timing of bh56_w12_8: (c1, 8.212000ns)
signal tile_34_X :  std_logic_vector(0 downto 0);
   -- timing of tile_34_X: (c1, 7.588000ns)
signal tile_34_Y :  std_logic_vector(0 downto 0);
   -- timing of tile_34_Y: (c1, 6.220000ns)
signal tile_34_output :  std_logic_vector(0 downto 0);
   -- timing of tile_34_output: (c1, 8.212000ns)
signal tile_34_filtered_output :  unsigned(0-0 downto 0);
   -- timing of tile_34_filtered_output: (c1, 8.212000ns)
signal bh56_w12_9 :  std_logic;
   -- timing of bh56_w12_9: (c1, 8.212000ns)
signal bh56_w12_10, bh56_w12_10_d1 :  std_logic;
   -- timing of bh56_w12_10: (c0, 0.000000ns)
signal bh56_w13_10, bh56_w13_10_d1 :  std_logic;
   -- timing of bh56_w13_10: (c0, 0.000000ns)
signal bh56_w14_11, bh56_w14_11_d1 :  std_logic;
   -- timing of bh56_w14_11: (c0, 0.000000ns)
signal bh56_w15_11, bh56_w15_11_d1 :  std_logic;
   -- timing of bh56_w15_11: (c0, 0.000000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid205_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid205_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid205_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid205_Out0: (c1, 8.836000ns)
signal bh56_w12_11 :  std_logic;
   -- timing of bh56_w12_11: (c1, 8.836000ns)
signal bh56_w13_11 :  std_logic;
   -- timing of bh56_w13_11: (c1, 8.836000ns)
signal bh56_w14_12 :  std_logic;
   -- timing of bh56_w14_12: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid205_Out0_copy206 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid205_Out0_copy206: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid209_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid209_In0: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid209_In1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid209_In1: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid209_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid209_Out0: (c1, 8.836000ns)
signal bh56_w12_12 :  std_logic;
   -- timing of bh56_w12_12: (c1, 8.836000ns)
signal bh56_w13_12 :  std_logic;
   -- timing of bh56_w13_12: (c1, 8.836000ns)
signal bh56_w14_13 :  std_logic;
   -- timing of bh56_w14_13: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid209_Out0_copy210 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid209_Out0_copy210: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid211_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid211_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid211_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid211_Out0: (c1, 8.836000ns)
signal bh56_w13_13 :  std_logic;
   -- timing of bh56_w13_13: (c1, 8.836000ns)
signal bh56_w14_14 :  std_logic;
   -- timing of bh56_w14_14: (c1, 8.836000ns)
signal bh56_w15_12 :  std_logic;
   -- timing of bh56_w15_12: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid211_Out0_copy212 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid211_Out0_copy212: (c1, 8.212000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid215_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid215_In0: (c1, 8.212000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid215_Out0 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid215_Out0: (c1, 8.836000ns)
signal bh56_w13_14 :  std_logic;
   -- timing of bh56_w13_14: (c1, 8.836000ns)
signal bh56_w14_15 :  std_logic;
   -- timing of bh56_w14_15: (c1, 8.836000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid215_Out0_copy216 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid215_Out0_copy216: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid217_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid217_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid217_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid217_Out0: (c1, 8.836000ns)
signal bh56_w14_16 :  std_logic;
   -- timing of bh56_w14_16: (c1, 8.836000ns)
signal bh56_w15_13 :  std_logic;
   -- timing of bh56_w15_13: (c1, 8.836000ns)
signal bh56_w16_10 :  std_logic;
   -- timing of bh56_w16_10: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid217_Out0_copy218 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid217_Out0_copy218: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid219_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid219_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid219_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid219_Out0: (c1, 8.836000ns)
signal bh56_w14_17 :  std_logic;
   -- timing of bh56_w14_17: (c1, 8.836000ns)
signal bh56_w15_14 :  std_logic;
   -- timing of bh56_w15_14: (c1, 8.836000ns)
signal bh56_w16_11 :  std_logic;
   -- timing of bh56_w16_11: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid219_Out0_copy220 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid219_Out0_copy220: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid221_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid221_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid221_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid221_Out0: (c1, 8.836000ns)
signal bh56_w15_15 :  std_logic;
   -- timing of bh56_w15_15: (c1, 8.836000ns)
signal bh56_w16_12 :  std_logic;
   -- timing of bh56_w16_12: (c1, 8.836000ns)
signal bh56_w17_11 :  std_logic;
   -- timing of bh56_w17_11: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid221_Out0_copy222 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid221_Out0_copy222: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid223_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid223_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid223_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid223_Out0: (c1, 8.836000ns)
signal bh56_w15_16 :  std_logic;
   -- timing of bh56_w15_16: (c1, 8.836000ns)
signal bh56_w16_13 :  std_logic;
   -- timing of bh56_w16_13: (c1, 8.836000ns)
signal bh56_w17_12 :  std_logic;
   -- timing of bh56_w17_12: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid223_Out0_copy224 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid223_Out0_copy224: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid225_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid225_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid225_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid225_Out0: (c1, 8.836000ns)
signal bh56_w16_14 :  std_logic;
   -- timing of bh56_w16_14: (c1, 8.836000ns)
signal bh56_w17_13 :  std_logic;
   -- timing of bh56_w17_13: (c1, 8.836000ns)
signal bh56_w18_11 :  std_logic;
   -- timing of bh56_w18_11: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid225_Out0_copy226 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid225_Out0_copy226: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid227_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid227_In0: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid227_In1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid227_In1: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid227_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid227_Out0: (c1, 8.836000ns)
signal bh56_w16_15 :  std_logic;
   -- timing of bh56_w16_15: (c1, 8.836000ns)
signal bh56_w17_14 :  std_logic;
   -- timing of bh56_w17_14: (c1, 8.836000ns)
signal bh56_w18_12 :  std_logic;
   -- timing of bh56_w18_12: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid227_Out0_copy228 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid227_Out0_copy228: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid229_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid229_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid229_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid229_Out0: (c1, 8.836000ns)
signal bh56_w17_15 :  std_logic;
   -- timing of bh56_w17_15: (c1, 8.836000ns)
signal bh56_w18_13 :  std_logic;
   -- timing of bh56_w18_13: (c1, 8.836000ns)
signal bh56_w19_11 :  std_logic;
   -- timing of bh56_w19_11: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid229_Out0_copy230 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid229_Out0_copy230: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid231_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid231_In0: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid231_In1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid231_In1: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid231_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid231_Out0: (c1, 8.836000ns)
signal bh56_w17_16 :  std_logic;
   -- timing of bh56_w17_16: (c1, 8.836000ns)
signal bh56_w18_14 :  std_logic;
   -- timing of bh56_w18_14: (c1, 8.836000ns)
signal bh56_w19_12 :  std_logic;
   -- timing of bh56_w19_12: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid231_Out0_copy232 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid231_Out0_copy232: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid233_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid233_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid233_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid233_Out0: (c1, 8.836000ns)
signal bh56_w18_15 :  std_logic;
   -- timing of bh56_w18_15: (c1, 8.836000ns)
signal bh56_w19_13 :  std_logic;
   -- timing of bh56_w19_13: (c1, 8.836000ns)
signal bh56_w20_9 :  std_logic;
   -- timing of bh56_w20_9: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid233_Out0_copy234 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid233_Out0_copy234: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid235_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid235_In0: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid235_In1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid235_In1: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid235_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid235_Out0: (c1, 8.836000ns)
signal bh56_w18_16 :  std_logic;
   -- timing of bh56_w18_16: (c1, 8.836000ns)
signal bh56_w19_14 :  std_logic;
   -- timing of bh56_w19_14: (c1, 8.836000ns)
signal bh56_w20_10 :  std_logic;
   -- timing of bh56_w20_10: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid235_Out0_copy236 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid235_Out0_copy236: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid237_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid237_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid237_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid237_Out0: (c1, 8.836000ns)
signal bh56_w19_15 :  std_logic;
   -- timing of bh56_w19_15: (c1, 8.836000ns)
signal bh56_w20_11 :  std_logic;
   -- timing of bh56_w20_11: (c1, 8.836000ns)
signal bh56_w21_9 :  std_logic;
   -- timing of bh56_w21_9: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid237_Out0_copy238 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid237_Out0_copy238: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid239_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid239_In0: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid239_In1, Compressor_14_3_Freq100_uid208_bh56_uid239_In1_d1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid239_In1: (c0, 0.000000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid239_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid239_Out0: (c1, 8.836000ns)
signal bh56_w19_16 :  std_logic;
   -- timing of bh56_w19_16: (c1, 8.836000ns)
signal bh56_w20_12 :  std_logic;
   -- timing of bh56_w20_12: (c1, 8.836000ns)
signal bh56_w21_10 :  std_logic;
   -- timing of bh56_w21_10: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid239_Out0_copy240 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid239_Out0_copy240: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid241_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid241_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid241_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid241_Out0: (c1, 8.836000ns)
signal bh56_w20_13 :  std_logic;
   -- timing of bh56_w20_13: (c1, 8.836000ns)
signal bh56_w21_11 :  std_logic;
   -- timing of bh56_w21_11: (c1, 8.836000ns)
signal bh56_w22_9 :  std_logic;
   -- timing of bh56_w22_9: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid241_Out0_copy242 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid241_Out0_copy242: (c1, 8.212000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid243_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid243_In0: (c1, 8.212000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid243_Out0 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid243_Out0: (c1, 8.836000ns)
signal bh56_w20_14 :  std_logic;
   -- timing of bh56_w20_14: (c1, 8.836000ns)
signal bh56_w21_12 :  std_logic;
   -- timing of bh56_w21_12: (c1, 8.836000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid243_Out0_copy244 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid243_Out0_copy244: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid245_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid245_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid245_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid245_Out0: (c1, 8.836000ns)
signal bh56_w21_13 :  std_logic;
   -- timing of bh56_w21_13: (c1, 8.836000ns)
signal bh56_w22_10 :  std_logic;
   -- timing of bh56_w22_10: (c1, 8.836000ns)
signal bh56_w23_7 :  std_logic;
   -- timing of bh56_w23_7: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid245_Out0_copy246 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid245_Out0_copy246: (c1, 8.212000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid249_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid249_In0: (c1, 8.212000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid249_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid249_In1: (c1, 8.212000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid249_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid249_Out0: (c1, 8.836000ns)
signal bh56_w21_14 :  std_logic;
   -- timing of bh56_w21_14: (c1, 8.836000ns)
signal bh56_w22_11 :  std_logic;
   -- timing of bh56_w22_11: (c1, 8.836000ns)
signal bh56_w23_8 :  std_logic;
   -- timing of bh56_w23_8: (c1, 8.836000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid249_Out0_copy250 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid249_Out0_copy250: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid251_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid251_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid251_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid251_Out0: (c1, 8.836000ns)
signal bh56_w22_12 :  std_logic;
   -- timing of bh56_w22_12: (c1, 8.836000ns)
signal bh56_w23_9 :  std_logic;
   -- timing of bh56_w23_9: (c1, 8.836000ns)
signal bh56_w24_7 :  std_logic;
   -- timing of bh56_w24_7: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid251_Out0_copy252 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid251_Out0_copy252: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid253_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid253_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid253_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid253_Out0: (c1, 8.836000ns)
signal bh56_w23_10 :  std_logic;
   -- timing of bh56_w23_10: (c1, 8.836000ns)
signal bh56_w24_8 :  std_logic;
   -- timing of bh56_w24_8: (c1, 8.836000ns)
signal bh56_w25_7 :  std_logic;
   -- timing of bh56_w25_7: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid253_Out0_copy254 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid253_Out0_copy254: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid255_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid255_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid255_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid255_Out0: (c1, 8.836000ns)
signal bh56_w24_9 :  std_logic;
   -- timing of bh56_w24_9: (c1, 8.836000ns)
signal bh56_w25_8 :  std_logic;
   -- timing of bh56_w25_8: (c1, 8.836000ns)
signal bh56_w26_5 :  std_logic;
   -- timing of bh56_w26_5: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid255_Out0_copy256 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid255_Out0_copy256: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid257_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid257_In0: (c1, 8.212000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid257_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid257_Out0: (c1, 8.836000ns)
signal bh56_w25_9 :  std_logic;
   -- timing of bh56_w25_9: (c1, 8.836000ns)
signal bh56_w26_6 :  std_logic;
   -- timing of bh56_w26_6: (c1, 8.836000ns)
signal bh56_w27_5 :  std_logic;
   -- timing of bh56_w27_5: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid257_Out0_copy258 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid257_Out0_copy258: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid259_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid259_In0: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid259_In1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid259_In1: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid259_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid259_Out0: (c1, 8.836000ns)
signal bh56_w26_7 :  std_logic;
   -- timing of bh56_w26_7: (c1, 8.836000ns)
signal bh56_w27_6 :  std_logic;
   -- timing of bh56_w27_6: (c1, 8.836000ns)
signal bh56_w28_5 :  std_logic;
   -- timing of bh56_w28_5: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid259_Out0_copy260 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid259_Out0_copy260: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid261_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid261_In0: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid261_In1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid261_In1: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid261_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid261_Out0: (c1, 8.836000ns)
signal bh56_w27_7 :  std_logic;
   -- timing of bh56_w27_7: (c1, 8.836000ns)
signal bh56_w28_6 :  std_logic;
   -- timing of bh56_w28_6: (c1, 8.836000ns)
signal bh56_w29_3 :  std_logic;
   -- timing of bh56_w29_3: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid261_Out0_copy262 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid261_Out0_copy262: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid263_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid263_In0: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid263_In1, Compressor_14_3_Freq100_uid208_bh56_uid263_In1_d1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid263_In1: (c0, 0.000000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid263_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid263_Out0: (c1, 8.836000ns)
signal bh56_w28_7 :  std_logic;
   -- timing of bh56_w28_7: (c1, 8.836000ns)
signal bh56_w29_4 :  std_logic;
   -- timing of bh56_w29_4: (c1, 8.836000ns)
signal bh56_w30_3 :  std_logic;
   -- timing of bh56_w30_3: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid263_Out0_copy264 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid263_Out0_copy264: (c1, 8.212000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid265_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid265_In0: (c1, 8.212000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid265_Out0 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid265_Out0: (c1, 8.836000ns)
signal bh56_w29_5 :  std_logic;
   -- timing of bh56_w29_5: (c1, 8.836000ns)
signal bh56_w30_4 :  std_logic;
   -- timing of bh56_w30_4: (c1, 8.836000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid265_Out0_copy266 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid265_Out0_copy266: (c1, 8.212000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid267_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid267_In0: (c1, 8.212000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid267_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid267_In1: (c1, 8.212000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid267_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid267_Out0: (c1, 8.836000ns)
signal bh56_w30_5 :  std_logic;
   -- timing of bh56_w30_5: (c1, 8.836000ns)
signal bh56_w31_3 :  std_logic;
   -- timing of bh56_w31_3: (c1, 8.836000ns)
signal bh56_w32_1 :  std_logic;
   -- timing of bh56_w32_1: (c1, 8.836000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid267_Out0_copy268 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid267_Out0_copy268: (c1, 8.212000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid269_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid269_In0: (c1, 8.836000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid269_Out0 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid269_Out0: (c1, 9.460000ns)
signal bh56_w12_13, bh56_w12_13_d1 :  std_logic;
   -- timing of bh56_w12_13: (c1, 9.460000ns)
signal bh56_w13_15 :  std_logic;
   -- timing of bh56_w13_15: (c1, 9.460000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid269_Out0_copy270 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid269_Out0_copy270: (c1, 8.836000ns)
signal Compressor_5_3_Freq100_uid272_bh56_uid273_In0 :  std_logic_vector(4 downto 0);
   -- timing of Compressor_5_3_Freq100_uid272_bh56_uid273_In0: (c1, 8.836000ns)
signal Compressor_5_3_Freq100_uid272_bh56_uid273_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_5_3_Freq100_uid272_bh56_uid273_Out0: (c1, 9.460000ns)
signal bh56_w13_16 :  std_logic;
   -- timing of bh56_w13_16: (c1, 9.460000ns)
signal bh56_w14_18 :  std_logic;
   -- timing of bh56_w14_18: (c1, 9.460000ns)
signal bh56_w15_17 :  std_logic;
   -- timing of bh56_w15_17: (c1, 9.460000ns)
signal Compressor_5_3_Freq100_uid272_bh56_uid273_Out0_copy274 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_5_3_Freq100_uid272_bh56_uid273_Out0_copy274: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid275_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid275_In0: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid275_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid275_Out0: (c1, 9.460000ns)
signal bh56_w14_19 :  std_logic;
   -- timing of bh56_w14_19: (c1, 9.460000ns)
signal bh56_w15_18 :  std_logic;
   -- timing of bh56_w15_18: (c1, 9.460000ns)
signal bh56_w16_16 :  std_logic;
   -- timing of bh56_w16_16: (c1, 9.460000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid275_Out0_copy276 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid275_Out0_copy276: (c1, 8.836000ns)
signal Compressor_5_3_Freq100_uid272_bh56_uid277_In0 :  std_logic_vector(4 downto 0);
   -- timing of Compressor_5_3_Freq100_uid272_bh56_uid277_In0: (c1, 8.836000ns)
signal Compressor_5_3_Freq100_uid272_bh56_uid277_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_5_3_Freq100_uid272_bh56_uid277_Out0: (c1, 9.460000ns)
signal bh56_w15_19 :  std_logic;
   -- timing of bh56_w15_19: (c1, 9.460000ns)
signal bh56_w16_17 :  std_logic;
   -- timing of bh56_w16_17: (c1, 9.460000ns)
signal bh56_w17_17 :  std_logic;
   -- timing of bh56_w17_17: (c1, 9.460000ns)
signal Compressor_5_3_Freq100_uid272_bh56_uid277_Out0_copy278 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_5_3_Freq100_uid272_bh56_uid277_Out0_copy278: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid279_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid279_In0: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid279_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid279_Out0: (c1, 9.460000ns)
signal bh56_w16_18, bh56_w16_18_d1 :  std_logic;
   -- timing of bh56_w16_18: (c1, 9.460000ns)
signal bh56_w17_18 :  std_logic;
   -- timing of bh56_w17_18: (c1, 9.460000ns)
signal bh56_w18_17 :  std_logic;
   -- timing of bh56_w18_17: (c1, 9.460000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid279_Out0_copy280 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid279_Out0_copy280: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid281_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid281_In0: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid281_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid281_Out0: (c1, 9.460000ns)
signal bh56_w17_19 :  std_logic;
   -- timing of bh56_w17_19: (c1, 9.460000ns)
signal bh56_w18_18 :  std_logic;
   -- timing of bh56_w18_18: (c1, 9.460000ns)
signal bh56_w19_17 :  std_logic;
   -- timing of bh56_w19_17: (c1, 9.460000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid281_Out0_copy282 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid281_Out0_copy282: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid283_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid283_In0: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid283_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid283_Out0: (c1, 9.460000ns)
signal bh56_w18_19, bh56_w18_19_d1 :  std_logic;
   -- timing of bh56_w18_19: (c1, 9.460000ns)
signal bh56_w19_18 :  std_logic;
   -- timing of bh56_w19_18: (c1, 9.460000ns)
signal bh56_w20_15 :  std_logic;
   -- timing of bh56_w20_15: (c1, 9.460000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid283_Out0_copy284 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid283_Out0_copy284: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid285_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid285_In0: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid285_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid285_Out0: (c1, 9.460000ns)
signal bh56_w19_19 :  std_logic;
   -- timing of bh56_w19_19: (c1, 9.460000ns)
signal bh56_w20_16 :  std_logic;
   -- timing of bh56_w20_16: (c1, 9.460000ns)
signal bh56_w21_15 :  std_logic;
   -- timing of bh56_w21_15: (c1, 9.460000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid285_Out0_copy286 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid285_Out0_copy286: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid287_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid287_In0: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid287_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid287_Out0: (c1, 9.460000ns)
signal bh56_w20_17, bh56_w20_17_d1 :  std_logic;
   -- timing of bh56_w20_17: (c1, 9.460000ns)
signal bh56_w21_16 :  std_logic;
   -- timing of bh56_w21_16: (c1, 9.460000ns)
signal bh56_w22_13 :  std_logic;
   -- timing of bh56_w22_13: (c1, 9.460000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid287_Out0_copy288 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid287_Out0_copy288: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid289_In0 :  std_logic_vector(5 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid289_In0: (c1, 8.836000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid289_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid289_Out0: (c1, 9.460000ns)
signal bh56_w21_17 :  std_logic;
   -- timing of bh56_w21_17: (c1, 9.460000ns)
signal bh56_w22_14 :  std_logic;
   -- timing of bh56_w22_14: (c1, 9.460000ns)
signal bh56_w23_11 :  std_logic;
   -- timing of bh56_w23_11: (c1, 9.460000ns)
signal Compressor_6_3_Freq100_uid204_bh56_uid289_Out0_copy290 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_6_3_Freq100_uid204_bh56_uid289_Out0_copy290: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid291_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid291_In0: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid291_In1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid291_In1: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid291_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid291_Out0: (c1, 9.460000ns)
signal bh56_w22_15 :  std_logic;
   -- timing of bh56_w22_15: (c1, 9.460000ns)
signal bh56_w23_12 :  std_logic;
   -- timing of bh56_w23_12: (c1, 9.460000ns)
signal bh56_w24_10 :  std_logic;
   -- timing of bh56_w24_10: (c1, 9.460000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid291_Out0_copy292 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid291_Out0_copy292: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid293_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid293_In0: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid293_In1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid293_In1: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid293_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid293_Out0: (c1, 9.460000ns)
signal bh56_w23_13 :  std_logic;
   -- timing of bh56_w23_13: (c1, 9.460000ns)
signal bh56_w24_11 :  std_logic;
   -- timing of bh56_w24_11: (c1, 9.460000ns)
signal bh56_w25_10 :  std_logic;
   -- timing of bh56_w25_10: (c1, 9.460000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid293_Out0_copy294 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid293_Out0_copy294: (c1, 8.836000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid295_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid295_In0: (c1, 8.836000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid295_Out0 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid295_Out0: (c1, 9.460000ns)
signal bh56_w24_12 :  std_logic;
   -- timing of bh56_w24_12: (c1, 9.460000ns)
signal bh56_w25_11 :  std_logic;
   -- timing of bh56_w25_11: (c1, 9.460000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid295_Out0_copy296 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid295_Out0_copy296: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid297_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid297_In0: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid297_In1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid297_In1: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid297_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid297_Out0: (c1, 9.460000ns)
signal bh56_w25_12 :  std_logic;
   -- timing of bh56_w25_12: (c1, 9.460000ns)
signal bh56_w26_8 :  std_logic;
   -- timing of bh56_w26_8: (c1, 9.460000ns)
signal bh56_w27_8 :  std_logic;
   -- timing of bh56_w27_8: (c1, 9.460000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid297_Out0_copy298 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid297_Out0_copy298: (c1, 8.836000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid299_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid299_In0: (c1, 8.836000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid299_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid299_In1: (c1, 8.836000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid299_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid299_Out0: (c1, 9.460000ns)
signal bh56_w26_9 :  std_logic;
   -- timing of bh56_w26_9: (c1, 9.460000ns)
signal bh56_w27_9 :  std_logic;
   -- timing of bh56_w27_9: (c1, 9.460000ns)
signal bh56_w28_8 :  std_logic;
   -- timing of bh56_w28_8: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid299_Out0_copy300 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid299_Out0_copy300: (c1, 8.836000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid301_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid301_In0: (c1, 8.836000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid301_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid301_In1: (c1, 8.836000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid301_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid301_Out0: (c1, 9.460000ns)
signal bh56_w28_9 :  std_logic;
   -- timing of bh56_w28_9: (c1, 9.460000ns)
signal bh56_w29_6 :  std_logic;
   -- timing of bh56_w29_6: (c1, 9.460000ns)
signal bh56_w30_6 :  std_logic;
   -- timing of bh56_w30_6: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid301_Out0_copy302 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid301_Out0_copy302: (c1, 8.836000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid303_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid303_In0: (c1, 8.836000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid303_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid303_In1: (c1, 8.836000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid303_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid303_Out0: (c1, 9.460000ns)
signal bh56_w30_7 :  std_logic;
   -- timing of bh56_w30_7: (c1, 9.460000ns)
signal bh56_w31_4, bh56_w31_4_d1 :  std_logic;
   -- timing of bh56_w31_4: (c1, 9.460000ns)
signal bh56_w32_2 :  std_logic;
   -- timing of bh56_w32_2: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid303_Out0_copy304 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid303_Out0_copy304: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid305_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid305_In0: (c1, 8.836000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid305_In1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid305_In1: (c1, 8.212000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid305_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid305_Out0: (c1, 9.460000ns)
signal bh56_w32_3 :  std_logic;
   -- timing of bh56_w32_3: (c1, 9.460000ns)
signal bh56_w33_1 :  std_logic;
   -- timing of bh56_w33_1: (c1, 9.460000ns)
signal bh56_w34_1 :  std_logic;
   -- timing of bh56_w34_1: (c1, 9.460000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid305_Out0_copy306 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid305_Out0_copy306: (c1, 8.836000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid307_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid307_In0: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid307_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid307_In1: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid307_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid307_Out0: (c2, 0.602000ns)
signal bh56_w13_17 :  std_logic;
   -- timing of bh56_w13_17: (c2, 0.602000ns)
signal bh56_w14_20 :  std_logic;
   -- timing of bh56_w14_20: (c2, 0.602000ns)
signal bh56_w15_20 :  std_logic;
   -- timing of bh56_w15_20: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid307_Out0_copy308, Compressor_23_3_Freq100_uid248_bh56_uid307_Out0_copy308_d1 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid307_Out0_copy308: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid309_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid309_In0: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid309_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid309_In1: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid309_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid309_Out0: (c2, 0.602000ns)
signal bh56_w15_21 :  std_logic;
   -- timing of bh56_w15_21: (c2, 0.602000ns)
signal bh56_w16_19 :  std_logic;
   -- timing of bh56_w16_19: (c2, 0.602000ns)
signal bh56_w17_20 :  std_logic;
   -- timing of bh56_w17_20: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid309_Out0_copy310, Compressor_23_3_Freq100_uid248_bh56_uid309_Out0_copy310_d1 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid309_Out0_copy310: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid311_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid311_In0: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid311_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid311_In1: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid311_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid311_Out0: (c2, 0.602000ns)
signal bh56_w17_21 :  std_logic;
   -- timing of bh56_w17_21: (c2, 0.602000ns)
signal bh56_w18_20 :  std_logic;
   -- timing of bh56_w18_20: (c2, 0.602000ns)
signal bh56_w19_20 :  std_logic;
   -- timing of bh56_w19_20: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid311_Out0_copy312, Compressor_23_3_Freq100_uid248_bh56_uid311_Out0_copy312_d1 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid311_Out0_copy312: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid313_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid313_In0: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid313_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid313_In1: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid313_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid313_Out0: (c2, 0.602000ns)
signal bh56_w19_21 :  std_logic;
   -- timing of bh56_w19_21: (c2, 0.602000ns)
signal bh56_w20_18 :  std_logic;
   -- timing of bh56_w20_18: (c2, 0.602000ns)
signal bh56_w21_18 :  std_logic;
   -- timing of bh56_w21_18: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid313_Out0_copy314, Compressor_23_3_Freq100_uid248_bh56_uid313_Out0_copy314_d1 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid313_Out0_copy314: (c1, 9.460000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid315_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid315_In0: (c1, 9.460000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid315_Out0 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid315_Out0: (c2, 0.602000ns)
signal bh56_w21_19 :  std_logic;
   -- timing of bh56_w21_19: (c2, 0.602000ns)
signal bh56_w22_16 :  std_logic;
   -- timing of bh56_w22_16: (c2, 0.602000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid315_Out0_copy316, Compressor_3_2_Freq100_uid214_bh56_uid315_Out0_copy316_d1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid315_Out0_copy316: (c1, 9.460000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid317_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid317_In0: (c1, 9.460000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid317_In1, Compressor_14_3_Freq100_uid208_bh56_uid317_In1_d1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid317_In1: (c0, 0.000000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid317_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid317_Out0: (c2, 0.602000ns)
signal bh56_w22_17 :  std_logic;
   -- timing of bh56_w22_17: (c2, 0.602000ns)
signal bh56_w23_14 :  std_logic;
   -- timing of bh56_w23_14: (c2, 0.602000ns)
signal bh56_w24_13 :  std_logic;
   -- timing of bh56_w24_13: (c2, 0.602000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid317_Out0_copy318, Compressor_14_3_Freq100_uid208_bh56_uid317_Out0_copy318_d1 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid317_Out0_copy318: (c1, 9.460000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid319_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid319_In0: (c1, 9.460000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid319_Out0 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid319_Out0: (c2, 0.602000ns)
signal bh56_w23_15 :  std_logic;
   -- timing of bh56_w23_15: (c2, 0.602000ns)
signal bh56_w24_14 :  std_logic;
   -- timing of bh56_w24_14: (c2, 0.602000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid319_Out0_copy320, Compressor_3_2_Freq100_uid214_bh56_uid319_Out0_copy320_d1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid319_Out0_copy320: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid321_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid321_In0: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid321_In1, Compressor_23_3_Freq100_uid248_bh56_uid321_In1_d1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid321_In1: (c0, 0.000000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid321_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid321_Out0: (c2, 0.602000ns)
signal bh56_w24_15 :  std_logic;
   -- timing of bh56_w24_15: (c2, 0.602000ns)
signal bh56_w25_13 :  std_logic;
   -- timing of bh56_w25_13: (c2, 0.602000ns)
signal bh56_w26_10 :  std_logic;
   -- timing of bh56_w26_10: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid321_Out0_copy322, Compressor_23_3_Freq100_uid248_bh56_uid321_Out0_copy322_d1 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid321_Out0_copy322: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid323_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid323_In0: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid323_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid323_In1: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid323_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid323_Out0: (c2, 0.602000ns)
signal bh56_w25_14 :  std_logic;
   -- timing of bh56_w25_14: (c2, 0.602000ns)
signal bh56_w26_11 :  std_logic;
   -- timing of bh56_w26_11: (c2, 0.602000ns)
signal bh56_w27_10 :  std_logic;
   -- timing of bh56_w27_10: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid323_Out0_copy324, Compressor_23_3_Freq100_uid248_bh56_uid323_Out0_copy324_d1 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid323_Out0_copy324: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid325_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid325_In0: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid325_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid325_In1: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid325_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid325_Out0: (c2, 0.602000ns)
signal bh56_w27_11 :  std_logic;
   -- timing of bh56_w27_11: (c2, 0.602000ns)
signal bh56_w28_10 :  std_logic;
   -- timing of bh56_w28_10: (c2, 0.602000ns)
signal bh56_w29_7 :  std_logic;
   -- timing of bh56_w29_7: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid325_Out0_copy326, Compressor_23_3_Freq100_uid248_bh56_uid325_Out0_copy326_d1 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid325_Out0_copy326: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid327_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid327_In0: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid327_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid327_In1: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid327_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid327_Out0: (c2, 0.602000ns)
signal bh56_w29_8 :  std_logic;
   -- timing of bh56_w29_8: (c2, 0.602000ns)
signal bh56_w30_8 :  std_logic;
   -- timing of bh56_w30_8: (c2, 0.602000ns)
signal bh56_w31_5 :  std_logic;
   -- timing of bh56_w31_5: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid327_Out0_copy328, Compressor_23_3_Freq100_uid248_bh56_uid327_Out0_copy328_d1 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid327_Out0_copy328: (c1, 9.460000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid329_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid329_In0: (c1, 9.460000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid329_In1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid329_In1: (c1, 9.460000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid329_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid329_Out0: (c2, 0.602000ns)
signal bh56_w32_4 :  std_logic;
   -- timing of bh56_w32_4: (c2, 0.602000ns)
signal bh56_w33_2 :  std_logic;
   -- timing of bh56_w33_2: (c2, 0.602000ns)
signal bh56_w34_2 :  std_logic;
   -- timing of bh56_w34_2: (c2, 0.602000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid329_Out0_copy330, Compressor_14_3_Freq100_uid208_bh56_uid329_Out0_copy330_d1 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid329_Out0_copy330: (c1, 9.460000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid331_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid331_In0: (c1, 9.460000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid331_Out0 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid331_Out0: (c2, 0.602000ns)
signal bh56_w34_3 :  std_logic;
   -- timing of bh56_w34_3: (c2, 0.602000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid331_Out0_copy332, Compressor_3_2_Freq100_uid214_bh56_uid331_Out0_copy332_d1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid331_Out0_copy332: (c1, 9.460000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid333_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid333_In0: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid333_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid333_In1: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid333_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid333_Out0: (c2, 1.226000ns)
signal bh56_w15_22 :  std_logic;
   -- timing of bh56_w15_22: (c2, 1.226000ns)
signal bh56_w16_20 :  std_logic;
   -- timing of bh56_w16_20: (c2, 1.226000ns)
signal bh56_w17_22 :  std_logic;
   -- timing of bh56_w17_22: (c2, 1.226000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid333_Out0_copy334 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid333_Out0_copy334: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid335_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid335_In0: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid335_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid335_In1: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid335_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid335_Out0: (c2, 1.226000ns)
signal bh56_w17_23 :  std_logic;
   -- timing of bh56_w17_23: (c2, 1.226000ns)
signal bh56_w18_21 :  std_logic;
   -- timing of bh56_w18_21: (c2, 1.226000ns)
signal bh56_w19_22 :  std_logic;
   -- timing of bh56_w19_22: (c2, 1.226000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid335_Out0_copy336 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid335_Out0_copy336: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid337_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid337_In0: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid337_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid337_In1: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid337_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid337_Out0: (c2, 1.226000ns)
signal bh56_w19_23 :  std_logic;
   -- timing of bh56_w19_23: (c2, 1.226000ns)
signal bh56_w20_19 :  std_logic;
   -- timing of bh56_w20_19: (c2, 1.226000ns)
signal bh56_w21_20 :  std_logic;
   -- timing of bh56_w21_20: (c2, 1.226000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid337_Out0_copy338 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid337_Out0_copy338: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid339_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid339_In0: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid339_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid339_In1: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid339_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid339_Out0: (c2, 1.226000ns)
signal bh56_w21_21 :  std_logic;
   -- timing of bh56_w21_21: (c2, 1.226000ns)
signal bh56_w22_18 :  std_logic;
   -- timing of bh56_w22_18: (c2, 1.226000ns)
signal bh56_w23_16 :  std_logic;
   -- timing of bh56_w23_16: (c2, 1.226000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid339_Out0_copy340 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid339_Out0_copy340: (c2, 0.602000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid341_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid341_In0: (c2, 0.602000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid341_Out0 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid341_Out0: (c2, 1.226000ns)
signal bh56_w23_17 :  std_logic;
   -- timing of bh56_w23_17: (c2, 1.226000ns)
signal bh56_w24_16 :  std_logic;
   -- timing of bh56_w24_16: (c2, 1.226000ns)
signal Compressor_3_2_Freq100_uid214_bh56_uid341_Out0_copy342 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_3_2_Freq100_uid214_bh56_uid341_Out0_copy342: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid343_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid343_In0: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid343_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid343_In1: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid343_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid343_Out0: (c2, 1.226000ns)
signal bh56_w24_17 :  std_logic;
   -- timing of bh56_w24_17: (c2, 1.226000ns)
signal bh56_w25_15 :  std_logic;
   -- timing of bh56_w25_15: (c2, 1.226000ns)
signal bh56_w26_12 :  std_logic;
   -- timing of bh56_w26_12: (c2, 1.226000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid343_Out0_copy344 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid343_Out0_copy344: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid345_In0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid345_In0: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid345_In1 :  std_logic_vector(1 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid345_In1: (c2, 0.602000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid345_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid345_Out0: (c2, 1.226000ns)
signal bh56_w26_13 :  std_logic;
   -- timing of bh56_w26_13: (c2, 1.226000ns)
signal bh56_w27_12 :  std_logic;
   -- timing of bh56_w27_12: (c2, 1.226000ns)
signal bh56_w28_11 :  std_logic;
   -- timing of bh56_w28_11: (c2, 1.226000ns)
signal Compressor_23_3_Freq100_uid248_bh56_uid345_Out0_copy346 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_23_3_Freq100_uid248_bh56_uid345_Out0_copy346: (c2, 0.602000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid347_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid347_In0: (c2, 0.602000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid347_In1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid347_In1: (c2, 0.602000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid347_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid347_Out0: (c2, 1.226000ns)
signal bh56_w29_9 :  std_logic;
   -- timing of bh56_w29_9: (c2, 1.226000ns)
signal bh56_w30_9 :  std_logic;
   -- timing of bh56_w30_9: (c2, 1.226000ns)
signal bh56_w31_6 :  std_logic;
   -- timing of bh56_w31_6: (c2, 1.226000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid347_Out0_copy348 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid347_Out0_copy348: (c2, 0.602000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid349_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid349_In0: (c2, 0.602000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid349_In1 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid349_In1: (c2, 0.602000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid349_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid349_Out0: (c2, 1.226000ns)
signal bh56_w31_7 :  std_logic;
   -- timing of bh56_w31_7: (c2, 1.226000ns)
signal bh56_w32_5 :  std_logic;
   -- timing of bh56_w32_5: (c2, 1.226000ns)
signal bh56_w33_3 :  std_logic;
   -- timing of bh56_w33_3: (c2, 1.226000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid349_Out0_copy350 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid349_Out0_copy350: (c2, 0.602000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid351_In0 :  std_logic_vector(3 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid351_In0: (c2, 0.602000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid351_In1, Compressor_14_3_Freq100_uid208_bh56_uid351_In1_d1, Compressor_14_3_Freq100_uid208_bh56_uid351_In1_d2 :  std_logic_vector(0 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid351_In1: (c0, 0.000000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid351_Out0 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid351_Out0: (c2, 1.226000ns)
signal bh56_w34_4 :  std_logic;
   -- timing of bh56_w34_4: (c2, 1.226000ns)
signal Compressor_14_3_Freq100_uid208_bh56_uid351_Out0_copy352 :  std_logic_vector(2 downto 0);
   -- timing of Compressor_14_3_Freq100_uid208_bh56_uid351_Out0_copy352: (c2, 0.602000ns)
signal tmp_bitheapResult_bh56_16 :  std_logic_vector(16 downto 0);
   -- timing of tmp_bitheapResult_bh56_16: (c2, 1.226000ns)
signal bitheapFinalAdd_bh56_In0 :  std_logic_vector(18 downto 0);
   -- timing of bitheapFinalAdd_bh56_In0: (c2, 1.226000ns)
signal bitheapFinalAdd_bh56_In1 :  std_logic_vector(18 downto 0);
   -- timing of bitheapFinalAdd_bh56_In1: (c2, 1.226000ns)
signal bitheapFinalAdd_bh56_Cin :  std_logic;
   -- timing of bitheapFinalAdd_bh56_Cin: (c0, 0.000000ns)
signal bitheapFinalAdd_bh56_Out :  std_logic_vector(18 downto 0);
   -- timing of bitheapFinalAdd_bh56_Out: (c2, 2.822000ns)
signal bitheapResult_bh56 :  std_logic_vector(34 downto 0);
   -- timing of bitheapResult_bh56: (c2, 2.822000ns)
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            bh56_w12_10_d1 <=  bh56_w12_10;
            bh56_w13_10_d1 <=  bh56_w13_10;
            bh56_w14_11_d1 <=  bh56_w14_11;
            bh56_w15_11_d1 <=  bh56_w15_11;
            Compressor_14_3_Freq100_uid208_bh56_uid239_In1_d1 <=  Compressor_14_3_Freq100_uid208_bh56_uid239_In1;
            Compressor_14_3_Freq100_uid208_bh56_uid263_In1_d1 <=  Compressor_14_3_Freq100_uid208_bh56_uid263_In1;
            bh56_w12_13_d1 <=  bh56_w12_13;
            bh56_w16_18_d1 <=  bh56_w16_18;
            bh56_w18_19_d1 <=  bh56_w18_19;
            bh56_w20_17_d1 <=  bh56_w20_17;
            bh56_w31_4_d1 <=  bh56_w31_4;
            Compressor_23_3_Freq100_uid248_bh56_uid307_Out0_copy308_d1 <=  Compressor_23_3_Freq100_uid248_bh56_uid307_Out0_copy308;
            Compressor_23_3_Freq100_uid248_bh56_uid309_Out0_copy310_d1 <=  Compressor_23_3_Freq100_uid248_bh56_uid309_Out0_copy310;
            Compressor_23_3_Freq100_uid248_bh56_uid311_Out0_copy312_d1 <=  Compressor_23_3_Freq100_uid248_bh56_uid311_Out0_copy312;
            Compressor_23_3_Freq100_uid248_bh56_uid313_Out0_copy314_d1 <=  Compressor_23_3_Freq100_uid248_bh56_uid313_Out0_copy314;
            Compressor_3_2_Freq100_uid214_bh56_uid315_Out0_copy316_d1 <=  Compressor_3_2_Freq100_uid214_bh56_uid315_Out0_copy316;
            Compressor_14_3_Freq100_uid208_bh56_uid317_In1_d1 <=  Compressor_14_3_Freq100_uid208_bh56_uid317_In1;
            Compressor_14_3_Freq100_uid208_bh56_uid317_Out0_copy318_d1 <=  Compressor_14_3_Freq100_uid208_bh56_uid317_Out0_copy318;
            Compressor_3_2_Freq100_uid214_bh56_uid319_Out0_copy320_d1 <=  Compressor_3_2_Freq100_uid214_bh56_uid319_Out0_copy320;
            Compressor_23_3_Freq100_uid248_bh56_uid321_In1_d1 <=  Compressor_23_3_Freq100_uid248_bh56_uid321_In1;
            Compressor_23_3_Freq100_uid248_bh56_uid321_Out0_copy322_d1 <=  Compressor_23_3_Freq100_uid248_bh56_uid321_Out0_copy322;
            Compressor_23_3_Freq100_uid248_bh56_uid323_Out0_copy324_d1 <=  Compressor_23_3_Freq100_uid248_bh56_uid323_Out0_copy324;
            Compressor_23_3_Freq100_uid248_bh56_uid325_Out0_copy326_d1 <=  Compressor_23_3_Freq100_uid248_bh56_uid325_Out0_copy326;
            Compressor_23_3_Freq100_uid248_bh56_uid327_Out0_copy328_d1 <=  Compressor_23_3_Freq100_uid248_bh56_uid327_Out0_copy328;
            Compressor_14_3_Freq100_uid208_bh56_uid329_Out0_copy330_d1 <=  Compressor_14_3_Freq100_uid208_bh56_uid329_Out0_copy330;
            Compressor_3_2_Freq100_uid214_bh56_uid331_Out0_copy332_d1 <=  Compressor_3_2_Freq100_uid214_bh56_uid331_Out0_copy332;
            Compressor_14_3_Freq100_uid208_bh56_uid351_In1_d1 <=  Compressor_14_3_Freq100_uid208_bh56_uid351_In1;
            Compressor_14_3_Freq100_uid208_bh56_uid351_In1_d2 <=  Compressor_14_3_Freq100_uid208_bh56_uid351_In1_d1;
         end if;
      end process;
   XX_m55 <= X ;
   YY_m55 <= Y ;
   tile_0_X <= X(16 downto 14);
   tile_0_Y <= Y(17 downto 15);
   tile_0_mult: IntMultiplierLUT_3x3_Freq100_uid58
      port map ( clk  => clk,
                 X => tile_0_X,
                 Y => tile_0_Y,
                 R => tile_0_output);

   tile_0_filtered_output <= unsigned(tile_0_output(5 downto 0));
   bh56_w29_0 <= tile_0_filtered_output(0);
   bh56_w30_0 <= tile_0_filtered_output(1);
   bh56_w31_0 <= tile_0_filtered_output(2);
   bh56_w32_0 <= tile_0_filtered_output(3);
   bh56_w33_0 <= tile_0_filtered_output(4);
   bh56_w34_0 <= tile_0_filtered_output(5);
   tile_1_X <= X(16 downto 14);
   tile_1_Y <= Y(14 downto 12);
   tile_1_mult: IntMultiplierLUT_3x3_Freq100_uid63
      port map ( clk  => clk,
                 X => tile_1_X,
                 Y => tile_1_Y,
                 R => tile_1_output);

   tile_1_filtered_output <= unsigned(tile_1_output(5 downto 0));
   bh56_w26_0 <= tile_1_filtered_output(0);
   bh56_w27_0 <= tile_1_filtered_output(1);
   bh56_w28_0 <= tile_1_filtered_output(2);
   bh56_w29_1 <= tile_1_filtered_output(3);
   bh56_w30_1 <= tile_1_filtered_output(4);
   bh56_w31_1 <= tile_1_filtered_output(5);
   tile_2_X <= X(13 downto 11);
   tile_2_Y <= Y(17 downto 15);
   tile_2_mult: IntMultiplierLUT_3x3_Freq100_uid68
      port map ( clk  => clk,
                 X => tile_2_X,
                 Y => tile_2_Y,
                 R => tile_2_output);

   tile_2_filtered_output <= unsigned(tile_2_output(5 downto 0));
   bh56_w26_1 <= tile_2_filtered_output(0);
   bh56_w27_1 <= tile_2_filtered_output(1);
   bh56_w28_1 <= tile_2_filtered_output(2);
   bh56_w29_2 <= tile_2_filtered_output(3);
   bh56_w30_2 <= tile_2_filtered_output(4);
   bh56_w31_2 <= tile_2_filtered_output(5);
   tile_3_X <= X(16 downto 14);
   tile_3_Y <= Y(11 downto 9);
   tile_3_mult: IntMultiplierLUT_3x3_Freq100_uid73
      port map ( clk  => clk,
                 X => tile_3_X,
                 Y => tile_3_Y,
                 R => tile_3_output);

   tile_3_filtered_output <= unsigned(tile_3_output(5 downto 0));
   bh56_w23_0 <= tile_3_filtered_output(0);
   bh56_w24_0 <= tile_3_filtered_output(1);
   bh56_w25_0 <= tile_3_filtered_output(2);
   bh56_w26_2 <= tile_3_filtered_output(3);
   bh56_w27_2 <= tile_3_filtered_output(4);
   bh56_w28_2 <= tile_3_filtered_output(5);
   tile_4_X <= X(13 downto 11);
   tile_4_Y <= Y(14 downto 12);
   tile_4_mult: IntMultiplierLUT_3x3_Freq100_uid78
      port map ( clk  => clk,
                 X => tile_4_X,
                 Y => tile_4_Y,
                 R => tile_4_output);

   tile_4_filtered_output <= unsigned(tile_4_output(5 downto 0));
   bh56_w23_1 <= tile_4_filtered_output(0);
   bh56_w24_1 <= tile_4_filtered_output(1);
   bh56_w25_1 <= tile_4_filtered_output(2);
   bh56_w26_3 <= tile_4_filtered_output(3);
   bh56_w27_3 <= tile_4_filtered_output(4);
   bh56_w28_3 <= tile_4_filtered_output(5);
   tile_5_X <= X(10 downto 8);
   tile_5_Y <= Y(17 downto 15);
   tile_5_mult: IntMultiplierLUT_3x3_Freq100_uid83
      port map ( clk  => clk,
                 X => tile_5_X,
                 Y => tile_5_Y,
                 R => tile_5_output);

   tile_5_filtered_output <= unsigned(tile_5_output(5 downto 0));
   bh56_w23_2 <= tile_5_filtered_output(0);
   bh56_w24_2 <= tile_5_filtered_output(1);
   bh56_w25_2 <= tile_5_filtered_output(2);
   bh56_w26_4 <= tile_5_filtered_output(3);
   bh56_w27_4 <= tile_5_filtered_output(4);
   bh56_w28_4 <= tile_5_filtered_output(5);
   tile_6_X <= X(16 downto 14);
   tile_6_Y <= Y(8 downto 6);
   tile_6_mult: IntMultiplierLUT_3x3_Freq100_uid88
      port map ( clk  => clk,
                 X => tile_6_X,
                 Y => tile_6_Y,
                 R => tile_6_output);

   tile_6_filtered_output <= unsigned(tile_6_output(5 downto 0));
   bh56_w20_0 <= tile_6_filtered_output(0);
   bh56_w21_0 <= tile_6_filtered_output(1);
   bh56_w22_0 <= tile_6_filtered_output(2);
   bh56_w23_3 <= tile_6_filtered_output(3);
   bh56_w24_3 <= tile_6_filtered_output(4);
   bh56_w25_3 <= tile_6_filtered_output(5);
   tile_7_X <= X(13 downto 11);
   tile_7_Y <= Y(11 downto 9);
   tile_7_mult: IntMultiplierLUT_3x3_Freq100_uid93
      port map ( clk  => clk,
                 X => tile_7_X,
                 Y => tile_7_Y,
                 R => tile_7_output);

   tile_7_filtered_output <= unsigned(tile_7_output(5 downto 0));
   bh56_w20_1 <= tile_7_filtered_output(0);
   bh56_w21_1 <= tile_7_filtered_output(1);
   bh56_w22_1 <= tile_7_filtered_output(2);
   bh56_w23_4 <= tile_7_filtered_output(3);
   bh56_w24_4 <= tile_7_filtered_output(4);
   bh56_w25_4 <= tile_7_filtered_output(5);
   tile_8_X <= X(10 downto 8);
   tile_8_Y <= Y(14 downto 12);
   tile_8_mult: IntMultiplierLUT_3x3_Freq100_uid98
      port map ( clk  => clk,
                 X => tile_8_X,
                 Y => tile_8_Y,
                 R => tile_8_output);

   tile_8_filtered_output <= unsigned(tile_8_output(5 downto 0));
   bh56_w20_2 <= tile_8_filtered_output(0);
   bh56_w21_2 <= tile_8_filtered_output(1);
   bh56_w22_2 <= tile_8_filtered_output(2);
   bh56_w23_5 <= tile_8_filtered_output(3);
   bh56_w24_5 <= tile_8_filtered_output(4);
   bh56_w25_5 <= tile_8_filtered_output(5);
   tile_9_X <= X(7 downto 5);
   tile_9_Y <= Y(17 downto 15);
   tile_9_mult: IntMultiplierLUT_3x3_Freq100_uid103
      port map ( clk  => clk,
                 X => tile_9_X,
                 Y => tile_9_Y,
                 R => tile_9_output);

   tile_9_filtered_output <= unsigned(tile_9_output(5 downto 0));
   bh56_w20_3 <= tile_9_filtered_output(0);
   bh56_w21_3 <= tile_9_filtered_output(1);
   bh56_w22_3 <= tile_9_filtered_output(2);
   bh56_w23_6 <= tile_9_filtered_output(3);
   bh56_w24_6 <= tile_9_filtered_output(4);
   bh56_w25_6 <= tile_9_filtered_output(5);
   tile_10_X <= X(16 downto 14);
   tile_10_Y <= Y(5 downto 3);
   tile_10_mult: IntMultiplierLUT_3x3_Freq100_uid108
      port map ( clk  => clk,
                 X => tile_10_X,
                 Y => tile_10_Y,
                 R => tile_10_output);

   tile_10_filtered_output <= unsigned(tile_10_output(5 downto 0));
   bh56_w17_0 <= tile_10_filtered_output(0);
   bh56_w18_0 <= tile_10_filtered_output(1);
   bh56_w19_0 <= tile_10_filtered_output(2);
   bh56_w20_4 <= tile_10_filtered_output(3);
   bh56_w21_4 <= tile_10_filtered_output(4);
   bh56_w22_4 <= tile_10_filtered_output(5);
   tile_11_X <= X(13 downto 11);
   tile_11_Y <= Y(8 downto 6);
   tile_11_mult: IntMultiplierLUT_3x3_Freq100_uid113
      port map ( clk  => clk,
                 X => tile_11_X,
                 Y => tile_11_Y,
                 R => tile_11_output);

   tile_11_filtered_output <= unsigned(tile_11_output(5 downto 0));
   bh56_w17_1 <= tile_11_filtered_output(0);
   bh56_w18_1 <= tile_11_filtered_output(1);
   bh56_w19_1 <= tile_11_filtered_output(2);
   bh56_w20_5 <= tile_11_filtered_output(3);
   bh56_w21_5 <= tile_11_filtered_output(4);
   bh56_w22_5 <= tile_11_filtered_output(5);
   tile_12_X <= X(10 downto 8);
   tile_12_Y <= Y(11 downto 9);
   tile_12_mult: IntMultiplierLUT_3x3_Freq100_uid118
      port map ( clk  => clk,
                 X => tile_12_X,
                 Y => tile_12_Y,
                 R => tile_12_output);

   tile_12_filtered_output <= unsigned(tile_12_output(5 downto 0));
   bh56_w17_2 <= tile_12_filtered_output(0);
   bh56_w18_2 <= tile_12_filtered_output(1);
   bh56_w19_2 <= tile_12_filtered_output(2);
   bh56_w20_6 <= tile_12_filtered_output(3);
   bh56_w21_6 <= tile_12_filtered_output(4);
   bh56_w22_6 <= tile_12_filtered_output(5);
   tile_13_X <= X(7 downto 5);
   tile_13_Y <= Y(14 downto 12);
   tile_13_mult: IntMultiplierLUT_3x3_Freq100_uid123
      port map ( clk  => clk,
                 X => tile_13_X,
                 Y => tile_13_Y,
                 R => tile_13_output);

   tile_13_filtered_output <= unsigned(tile_13_output(5 downto 0));
   bh56_w17_3 <= tile_13_filtered_output(0);
   bh56_w18_3 <= tile_13_filtered_output(1);
   bh56_w19_3 <= tile_13_filtered_output(2);
   bh56_w20_7 <= tile_13_filtered_output(3);
   bh56_w21_7 <= tile_13_filtered_output(4);
   bh56_w22_7 <= tile_13_filtered_output(5);
   tile_14_X <= X(4 downto 2);
   tile_14_Y <= Y(17 downto 15);
   tile_14_mult: IntMultiplierLUT_3x3_Freq100_uid128
      port map ( clk  => clk,
                 X => tile_14_X,
                 Y => tile_14_Y,
                 R => tile_14_output);

   tile_14_filtered_output <= unsigned(tile_14_output(5 downto 0));
   bh56_w17_4 <= tile_14_filtered_output(0);
   bh56_w18_4 <= tile_14_filtered_output(1);
   bh56_w19_4 <= tile_14_filtered_output(2);
   bh56_w20_8 <= tile_14_filtered_output(3);
   bh56_w21_8 <= tile_14_filtered_output(4);
   bh56_w22_8 <= tile_14_filtered_output(5);
   tile_15_X <= X(16 downto 14);
   tile_15_Y <= Y(2 downto 0);
   tile_15_mult: IntMultiplierLUT_3x3_Freq100_uid133
      port map ( clk  => clk,
                 X => tile_15_X,
                 Y => tile_15_Y,
                 R => tile_15_output);

   tile_15_filtered_output <= unsigned(tile_15_output(5 downto 0));
   bh56_w14_0 <= tile_15_filtered_output(0);
   bh56_w15_0 <= tile_15_filtered_output(1);
   bh56_w16_0 <= tile_15_filtered_output(2);
   bh56_w17_5 <= tile_15_filtered_output(3);
   bh56_w18_5 <= tile_15_filtered_output(4);
   bh56_w19_5 <= tile_15_filtered_output(5);
   tile_16_X <= X(13 downto 11);
   tile_16_Y <= Y(5 downto 3);
   tile_16_mult: IntMultiplierLUT_3x3_Freq100_uid138
      port map ( clk  => clk,
                 X => tile_16_X,
                 Y => tile_16_Y,
                 R => tile_16_output);

   tile_16_filtered_output <= unsigned(tile_16_output(5 downto 0));
   bh56_w14_1 <= tile_16_filtered_output(0);
   bh56_w15_1 <= tile_16_filtered_output(1);
   bh56_w16_1 <= tile_16_filtered_output(2);
   bh56_w17_6 <= tile_16_filtered_output(3);
   bh56_w18_6 <= tile_16_filtered_output(4);
   bh56_w19_6 <= tile_16_filtered_output(5);
   tile_17_X <= X(10 downto 8);
   tile_17_Y <= Y(8 downto 6);
   tile_17_mult: IntMultiplierLUT_3x3_Freq100_uid143
      port map ( clk  => clk,
                 X => tile_17_X,
                 Y => tile_17_Y,
                 R => tile_17_output);

   tile_17_filtered_output <= unsigned(tile_17_output(5 downto 0));
   bh56_w14_2 <= tile_17_filtered_output(0);
   bh56_w15_2 <= tile_17_filtered_output(1);
   bh56_w16_2 <= tile_17_filtered_output(2);
   bh56_w17_7 <= tile_17_filtered_output(3);
   bh56_w18_7 <= tile_17_filtered_output(4);
   bh56_w19_7 <= tile_17_filtered_output(5);
   tile_18_X <= X(7 downto 5);
   tile_18_Y <= Y(11 downto 9);
   tile_18_mult: IntMultiplierLUT_3x3_Freq100_uid148
      port map ( clk  => clk,
                 X => tile_18_X,
                 Y => tile_18_Y,
                 R => tile_18_output);

   tile_18_filtered_output <= unsigned(tile_18_output(5 downto 0));
   bh56_w14_3 <= tile_18_filtered_output(0);
   bh56_w15_3 <= tile_18_filtered_output(1);
   bh56_w16_3 <= tile_18_filtered_output(2);
   bh56_w17_8 <= tile_18_filtered_output(3);
   bh56_w18_8 <= tile_18_filtered_output(4);
   bh56_w19_8 <= tile_18_filtered_output(5);
   tile_19_X <= X(4 downto 2);
   tile_19_Y <= Y(14 downto 12);
   tile_19_mult: IntMultiplierLUT_3x3_Freq100_uid153
      port map ( clk  => clk,
                 X => tile_19_X,
                 Y => tile_19_Y,
                 R => tile_19_output);

   tile_19_filtered_output <= unsigned(tile_19_output(5 downto 0));
   bh56_w14_4 <= tile_19_filtered_output(0);
   bh56_w15_4 <= tile_19_filtered_output(1);
   bh56_w16_4 <= tile_19_filtered_output(2);
   bh56_w17_9 <= tile_19_filtered_output(3);
   bh56_w18_9 <= tile_19_filtered_output(4);
   bh56_w19_9 <= tile_19_filtered_output(5);
   tile_20_X <= X(1 downto 0);
   tile_20_Y <= Y(17 downto 15);
   tile_20_mult: IntMultiplierLUT_2x3_Freq100_uid158
      port map ( clk  => clk,
                 X => tile_20_X,
                 Y => tile_20_Y,
                 R => tile_20_output);

   tile_20_filtered_output <= unsigned(tile_20_output(4 downto 0));
   bh56_w15_5 <= tile_20_filtered_output(0);
   bh56_w16_5 <= tile_20_filtered_output(1);
   bh56_w17_10 <= tile_20_filtered_output(2);
   bh56_w18_10 <= tile_20_filtered_output(3);
   bh56_w19_10 <= tile_20_filtered_output(4);
   tile_21_X <= X(13 downto 13);
   tile_21_Y <= Y(2 downto 1);
   tile_21_mult: IntMultiplierLUT_1x2_Freq100_uid163
      port map ( clk  => clk,
                 X => tile_21_X,
                 Y => tile_21_Y,
                 R => tile_21_output);

   tile_21_filtered_output <= unsigned(tile_21_output(1 downto 0));
   bh56_w14_5 <= tile_21_filtered_output(0);
   bh56_w15_6 <= tile_21_filtered_output(1);
   tile_22_X <= X(10 downto 9);
   tile_22_Y <= Y(5 downto 3);
   tile_22_mult: IntMultiplierLUT_2x3_Freq100_uid165
      port map ( clk  => clk,
                 X => tile_22_X,
                 Y => tile_22_Y,
                 R => tile_22_output);

   tile_22_filtered_output <= unsigned(tile_22_output(4 downto 0));
   bh56_w12_0 <= tile_22_filtered_output(0);
   bh56_w13_0 <= tile_22_filtered_output(1);
   bh56_w14_6 <= tile_22_filtered_output(2);
   bh56_w15_7 <= tile_22_filtered_output(3);
   bh56_w16_6 <= tile_22_filtered_output(4);
   tile_23_X <= X(7 downto 6);
   tile_23_Y <= Y(8 downto 6);
   tile_23_mult: IntMultiplierLUT_2x3_Freq100_uid170
      port map ( clk  => clk,
                 X => tile_23_X,
                 Y => tile_23_Y,
                 R => tile_23_output);

   tile_23_filtered_output <= unsigned(tile_23_output(4 downto 0));
   bh56_w12_1 <= tile_23_filtered_output(0);
   bh56_w13_1 <= tile_23_filtered_output(1);
   bh56_w14_7 <= tile_23_filtered_output(2);
   bh56_w15_8 <= tile_23_filtered_output(3);
   bh56_w16_7 <= tile_23_filtered_output(4);
   tile_24_X <= X(4 downto 3);
   tile_24_Y <= Y(11 downto 9);
   tile_24_mult: IntMultiplierLUT_2x3_Freq100_uid175
      port map ( clk  => clk,
                 X => tile_24_X,
                 Y => tile_24_Y,
                 R => tile_24_output);

   tile_24_filtered_output <= unsigned(tile_24_output(4 downto 0));
   bh56_w12_2 <= tile_24_filtered_output(0);
   bh56_w13_2 <= tile_24_filtered_output(1);
   bh56_w14_8 <= tile_24_filtered_output(2);
   bh56_w15_9 <= tile_24_filtered_output(3);
   bh56_w16_8 <= tile_24_filtered_output(4);
   tile_25_X <= X(1 downto 0);
   tile_25_Y <= Y(14 downto 12);
   tile_25_mult: IntMultiplierLUT_2x3_Freq100_uid180
      port map ( clk  => clk,
                 X => tile_25_X,
                 Y => tile_25_Y,
                 R => tile_25_output);

   tile_25_filtered_output <= unsigned(tile_25_output(4 downto 0));
   bh56_w12_3 <= tile_25_filtered_output(0);
   bh56_w13_3 <= tile_25_filtered_output(1);
   bh56_w14_9 <= tile_25_filtered_output(2);
   bh56_w15_10 <= tile_25_filtered_output(3);
   bh56_w16_9 <= tile_25_filtered_output(4);
   tile_26_X <= X(12 downto 12);
   tile_26_Y <= Y(2 downto 1);
   tile_26_mult: IntMultiplierLUT_1x2_Freq100_uid185
      port map ( clk  => clk,
                 X => tile_26_X,
                 Y => tile_26_Y,
                 R => tile_26_output);

   tile_26_filtered_output <= unsigned(tile_26_output(1 downto 0));
   bh56_w13_4 <= tile_26_filtered_output(0);
   bh56_w14_10 <= tile_26_filtered_output(1);
   tile_27_X <= X(13 downto 13);
   tile_27_Y <= Y(0 downto 0);
   tile_27_mult: IntMultiplierLUT_1x1_Freq100_uid187
      port map ( clk  => clk,
                 X => tile_27_X,
                 Y => tile_27_Y,
                 R => tile_27_output);

   tile_27_filtered_output <= unsigned(tile_27_output(0 downto 0));
   bh56_w13_5 <= tile_27_filtered_output(0);
   tile_28_X <= X(11 downto 11);
   tile_28_Y <= Y(2 downto 2);
   tile_28_mult: IntMultiplierLUT_1x1_Freq100_uid189
      port map ( clk  => clk,
                 X => tile_28_X,
                 Y => tile_28_Y,
                 R => tile_28_output);

   tile_28_filtered_output <= unsigned(tile_28_output(0 downto 0));
   bh56_w13_6 <= tile_28_filtered_output(0);
   tile_29_X <= X(8 downto 8);
   tile_29_Y <= Y(5 downto 4);
   tile_29_mult: IntMultiplierLUT_1x2_Freq100_uid191
      port map ( clk  => clk,
                 X => tile_29_X,
                 Y => tile_29_Y,
                 R => tile_29_output);

   tile_29_filtered_output <= unsigned(tile_29_output(1 downto 0));
   bh56_w12_4 <= tile_29_filtered_output(0);
   bh56_w13_7 <= tile_29_filtered_output(1);
   tile_30_X <= X(5 downto 5);
   tile_30_Y <= Y(8 downto 7);
   tile_30_mult: IntMultiplierLUT_1x2_Freq100_uid193
      port map ( clk  => clk,
                 X => tile_30_X,
                 Y => tile_30_Y,
                 R => tile_30_output);

   tile_30_filtered_output <= unsigned(tile_30_output(1 downto 0));
   bh56_w12_5 <= tile_30_filtered_output(0);
   bh56_w13_8 <= tile_30_filtered_output(1);
   tile_31_X <= X(2 downto 2);
   tile_31_Y <= Y(11 downto 10);
   tile_31_mult: IntMultiplierLUT_1x2_Freq100_uid195
      port map ( clk  => clk,
                 X => tile_31_X,
                 Y => tile_31_Y,
                 R => tile_31_output);

   tile_31_filtered_output <= unsigned(tile_31_output(1 downto 0));
   bh56_w12_6 <= tile_31_filtered_output(0);
   bh56_w13_9 <= tile_31_filtered_output(1);
   tile_32_X <= X(7 downto 7);
   tile_32_Y <= Y(5 downto 5);
   tile_32_mult: IntMultiplierLUT_1x1_Freq100_uid197
      port map ( clk  => clk,
                 X => tile_32_X,
                 Y => tile_32_Y,
                 R => tile_32_output);

   tile_32_filtered_output <= unsigned(tile_32_output(0 downto 0));
   bh56_w12_7 <= tile_32_filtered_output(0);
   tile_33_X <= X(4 downto 4);
   tile_33_Y <= Y(8 downto 8);
   tile_33_mult: IntMultiplierLUT_1x1_Freq100_uid199
      port map ( clk  => clk,
                 X => tile_33_X,
                 Y => tile_33_Y,
                 R => tile_33_output);

   tile_33_filtered_output <= unsigned(tile_33_output(0 downto 0));
   bh56_w12_8 <= tile_33_filtered_output(0);
   tile_34_X <= X(1 downto 1);
   tile_34_Y <= Y(11 downto 11);
   tile_34_mult: IntMultiplierLUT_1x1_Freq100_uid201
      port map ( clk  => clk,
                 X => tile_34_X,
                 Y => tile_34_Y,
                 R => tile_34_output);

   tile_34_filtered_output <= unsigned(tile_34_output(0 downto 0));
   bh56_w12_9 <= tile_34_filtered_output(0);

   -- Adding the constant bits 
   bh56_w12_10 <= '1';
   bh56_w13_10 <= '1';
   bh56_w14_11 <= '1';
   bh56_w15_11 <= '1';


   Compressor_6_3_Freq100_uid204_bh56_uid205_In0 <= "" & bh56_w12_0 & bh56_w12_1 & bh56_w12_2 & bh56_w12_3 & bh56_w12_4 & bh56_w12_5;
   bh56_w12_11 <= Compressor_6_3_Freq100_uid204_bh56_uid205_Out0(0);
   bh56_w13_11 <= Compressor_6_3_Freq100_uid204_bh56_uid205_Out0(1);
   bh56_w14_12 <= Compressor_6_3_Freq100_uid204_bh56_uid205_Out0(2);
   Compressor_6_3_Freq100_uid204_uid205: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid205_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid205_Out0_copy206);
   Compressor_6_3_Freq100_uid204_bh56_uid205_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid205_Out0_copy206; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid209_In0 <= "" & bh56_w12_6 & bh56_w12_7 & bh56_w12_8 & bh56_w12_9;
   Compressor_14_3_Freq100_uid208_bh56_uid209_In1 <= "" & bh56_w13_0;
   bh56_w12_12 <= Compressor_14_3_Freq100_uid208_bh56_uid209_Out0(0);
   bh56_w13_12 <= Compressor_14_3_Freq100_uid208_bh56_uid209_Out0(1);
   bh56_w14_13 <= Compressor_14_3_Freq100_uid208_bh56_uid209_Out0(2);
   Compressor_14_3_Freq100_uid208_uid209: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid209_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid209_In1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid209_Out0_copy210);
   Compressor_14_3_Freq100_uid208_bh56_uid209_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid209_Out0_copy210; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid211_In0 <= "" & bh56_w13_1 & bh56_w13_2 & bh56_w13_3 & bh56_w13_4 & bh56_w13_5 & bh56_w13_6;
   bh56_w13_13 <= Compressor_6_3_Freq100_uid204_bh56_uid211_Out0(0);
   bh56_w14_14 <= Compressor_6_3_Freq100_uid204_bh56_uid211_Out0(1);
   bh56_w15_12 <= Compressor_6_3_Freq100_uid204_bh56_uid211_Out0(2);
   Compressor_6_3_Freq100_uid204_uid211: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid211_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid211_Out0_copy212);
   Compressor_6_3_Freq100_uid204_bh56_uid211_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid211_Out0_copy212; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq100_uid214_bh56_uid215_In0 <= "" & bh56_w13_7 & bh56_w13_8 & bh56_w13_9;
   bh56_w13_14 <= Compressor_3_2_Freq100_uid214_bh56_uid215_Out0(0);
   bh56_w14_15 <= Compressor_3_2_Freq100_uid214_bh56_uid215_Out0(1);
   Compressor_3_2_Freq100_uid214_uid215: Compressor_3_2_Freq100_uid214
      port map ( X0 => Compressor_3_2_Freq100_uid214_bh56_uid215_In0,
                 R => Compressor_3_2_Freq100_uid214_bh56_uid215_Out0_copy216);
   Compressor_3_2_Freq100_uid214_bh56_uid215_Out0 <= Compressor_3_2_Freq100_uid214_bh56_uid215_Out0_copy216; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid217_In0 <= "" & bh56_w14_0 & bh56_w14_1 & bh56_w14_2 & bh56_w14_3 & bh56_w14_4 & bh56_w14_5;
   bh56_w14_16 <= Compressor_6_3_Freq100_uid204_bh56_uid217_Out0(0);
   bh56_w15_13 <= Compressor_6_3_Freq100_uid204_bh56_uid217_Out0(1);
   bh56_w16_10 <= Compressor_6_3_Freq100_uid204_bh56_uid217_Out0(2);
   Compressor_6_3_Freq100_uid204_uid217: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid217_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid217_Out0_copy218);
   Compressor_6_3_Freq100_uid204_bh56_uid217_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid217_Out0_copy218; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid219_In0 <= "" & bh56_w14_8 & bh56_w14_11_d1 & bh56_w14_10 & bh56_w14_9 & bh56_w14_7 & bh56_w14_6;
   bh56_w14_17 <= Compressor_6_3_Freq100_uid204_bh56_uid219_Out0(0);
   bh56_w15_14 <= Compressor_6_3_Freq100_uid204_bh56_uid219_Out0(1);
   bh56_w16_11 <= Compressor_6_3_Freq100_uid204_bh56_uid219_Out0(2);
   Compressor_6_3_Freq100_uid204_uid219: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid219_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid219_Out0_copy220);
   Compressor_6_3_Freq100_uid204_bh56_uid219_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid219_Out0_copy220; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid221_In0 <= "" & bh56_w15_0 & bh56_w15_1 & bh56_w15_2 & bh56_w15_3 & bh56_w15_4 & bh56_w15_5;
   bh56_w15_15 <= Compressor_6_3_Freq100_uid204_bh56_uid221_Out0(0);
   bh56_w16_12 <= Compressor_6_3_Freq100_uid204_bh56_uid221_Out0(1);
   bh56_w17_11 <= Compressor_6_3_Freq100_uid204_bh56_uid221_Out0(2);
   Compressor_6_3_Freq100_uid204_uid221: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid221_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid221_Out0_copy222);
   Compressor_6_3_Freq100_uid204_bh56_uid221_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid221_Out0_copy222; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid223_In0 <= "" & bh56_w15_6 & bh56_w15_7 & bh56_w15_8 & bh56_w15_9 & bh56_w15_10 & bh56_w15_11_d1;
   bh56_w15_16 <= Compressor_6_3_Freq100_uid204_bh56_uid223_Out0(0);
   bh56_w16_13 <= Compressor_6_3_Freq100_uid204_bh56_uid223_Out0(1);
   bh56_w17_12 <= Compressor_6_3_Freq100_uid204_bh56_uid223_Out0(2);
   Compressor_6_3_Freq100_uid204_uid223: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid223_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid223_Out0_copy224);
   Compressor_6_3_Freq100_uid204_bh56_uid223_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid223_Out0_copy224; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid225_In0 <= "" & bh56_w16_0 & bh56_w16_1 & bh56_w16_2 & bh56_w16_3 & bh56_w16_4 & bh56_w16_5;
   bh56_w16_14 <= Compressor_6_3_Freq100_uid204_bh56_uid225_Out0(0);
   bh56_w17_13 <= Compressor_6_3_Freq100_uid204_bh56_uid225_Out0(1);
   bh56_w18_11 <= Compressor_6_3_Freq100_uid204_bh56_uid225_Out0(2);
   Compressor_6_3_Freq100_uid204_uid225: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid225_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid225_Out0_copy226);
   Compressor_6_3_Freq100_uid204_bh56_uid225_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid225_Out0_copy226; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid227_In0 <= "" & bh56_w16_6 & bh56_w16_7 & bh56_w16_8 & bh56_w16_9;
   Compressor_14_3_Freq100_uid208_bh56_uid227_In1 <= "" & bh56_w17_0;
   bh56_w16_15 <= Compressor_14_3_Freq100_uid208_bh56_uid227_Out0(0);
   bh56_w17_14 <= Compressor_14_3_Freq100_uid208_bh56_uid227_Out0(1);
   bh56_w18_12 <= Compressor_14_3_Freq100_uid208_bh56_uid227_Out0(2);
   Compressor_14_3_Freq100_uid208_uid227: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid227_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid227_In1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid227_Out0_copy228);
   Compressor_14_3_Freq100_uid208_bh56_uid227_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid227_Out0_copy228; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid229_In0 <= "" & bh56_w17_1 & bh56_w17_2 & bh56_w17_3 & bh56_w17_4 & bh56_w17_5 & bh56_w17_6;
   bh56_w17_15 <= Compressor_6_3_Freq100_uid204_bh56_uid229_Out0(0);
   bh56_w18_13 <= Compressor_6_3_Freq100_uid204_bh56_uid229_Out0(1);
   bh56_w19_11 <= Compressor_6_3_Freq100_uid204_bh56_uid229_Out0(2);
   Compressor_6_3_Freq100_uid204_uid229: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid229_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid229_Out0_copy230);
   Compressor_6_3_Freq100_uid204_bh56_uid229_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid229_Out0_copy230; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid231_In0 <= "" & bh56_w17_7 & bh56_w17_8 & bh56_w17_9 & bh56_w17_10;
   Compressor_14_3_Freq100_uid208_bh56_uid231_In1 <= "" & bh56_w18_0;
   bh56_w17_16 <= Compressor_14_3_Freq100_uid208_bh56_uid231_Out0(0);
   bh56_w18_14 <= Compressor_14_3_Freq100_uid208_bh56_uid231_Out0(1);
   bh56_w19_12 <= Compressor_14_3_Freq100_uid208_bh56_uid231_Out0(2);
   Compressor_14_3_Freq100_uid208_uid231: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid231_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid231_In1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid231_Out0_copy232);
   Compressor_14_3_Freq100_uid208_bh56_uid231_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid231_Out0_copy232; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid233_In0 <= "" & bh56_w18_1 & bh56_w18_2 & bh56_w18_3 & bh56_w18_4 & bh56_w18_5 & bh56_w18_6;
   bh56_w18_15 <= Compressor_6_3_Freq100_uid204_bh56_uid233_Out0(0);
   bh56_w19_13 <= Compressor_6_3_Freq100_uid204_bh56_uid233_Out0(1);
   bh56_w20_9 <= Compressor_6_3_Freq100_uid204_bh56_uid233_Out0(2);
   Compressor_6_3_Freq100_uid204_uid233: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid233_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid233_Out0_copy234);
   Compressor_6_3_Freq100_uid204_bh56_uid233_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid233_Out0_copy234; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid235_In0 <= "" & bh56_w18_7 & bh56_w18_8 & bh56_w18_9 & bh56_w18_10;
   Compressor_14_3_Freq100_uid208_bh56_uid235_In1 <= "" & bh56_w19_0;
   bh56_w18_16 <= Compressor_14_3_Freq100_uid208_bh56_uid235_Out0(0);
   bh56_w19_14 <= Compressor_14_3_Freq100_uid208_bh56_uid235_Out0(1);
   bh56_w20_10 <= Compressor_14_3_Freq100_uid208_bh56_uid235_Out0(2);
   Compressor_14_3_Freq100_uid208_uid235: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid235_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid235_In1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid235_Out0_copy236);
   Compressor_14_3_Freq100_uid208_bh56_uid235_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid235_Out0_copy236; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid237_In0 <= "" & bh56_w19_1 & bh56_w19_2 & bh56_w19_3 & bh56_w19_4 & bh56_w19_5 & bh56_w19_6;
   bh56_w19_15 <= Compressor_6_3_Freq100_uid204_bh56_uid237_Out0(0);
   bh56_w20_11 <= Compressor_6_3_Freq100_uid204_bh56_uid237_Out0(1);
   bh56_w21_9 <= Compressor_6_3_Freq100_uid204_bh56_uid237_Out0(2);
   Compressor_6_3_Freq100_uid204_uid237: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid237_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid237_Out0_copy238);
   Compressor_6_3_Freq100_uid204_bh56_uid237_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid237_Out0_copy238; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid239_In0 <= "" & bh56_w19_7 & bh56_w19_8 & bh56_w19_9 & bh56_w19_10;
   Compressor_14_3_Freq100_uid208_bh56_uid239_In1 <= "" & "0";
   bh56_w19_16 <= Compressor_14_3_Freq100_uid208_bh56_uid239_Out0(0);
   bh56_w20_12 <= Compressor_14_3_Freq100_uid208_bh56_uid239_Out0(1);
   bh56_w21_10 <= Compressor_14_3_Freq100_uid208_bh56_uid239_Out0(2);
   Compressor_14_3_Freq100_uid208_uid239: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid239_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid239_In1_d1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid239_Out0_copy240);
   Compressor_14_3_Freq100_uid208_bh56_uid239_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid239_Out0_copy240; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid241_In0 <= "" & bh56_w20_0 & bh56_w20_1 & bh56_w20_2 & bh56_w20_3 & bh56_w20_4 & bh56_w20_5;
   bh56_w20_13 <= Compressor_6_3_Freq100_uid204_bh56_uid241_Out0(0);
   bh56_w21_11 <= Compressor_6_3_Freq100_uid204_bh56_uid241_Out0(1);
   bh56_w22_9 <= Compressor_6_3_Freq100_uid204_bh56_uid241_Out0(2);
   Compressor_6_3_Freq100_uid204_uid241: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid241_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid241_Out0_copy242);
   Compressor_6_3_Freq100_uid204_bh56_uid241_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid241_Out0_copy242; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq100_uid214_bh56_uid243_In0 <= "" & bh56_w20_6 & bh56_w20_7 & bh56_w20_8;
   bh56_w20_14 <= Compressor_3_2_Freq100_uid214_bh56_uid243_Out0(0);
   bh56_w21_12 <= Compressor_3_2_Freq100_uid214_bh56_uid243_Out0(1);
   Compressor_3_2_Freq100_uid214_uid243: Compressor_3_2_Freq100_uid214
      port map ( X0 => Compressor_3_2_Freq100_uid214_bh56_uid243_In0,
                 R => Compressor_3_2_Freq100_uid214_bh56_uid243_Out0_copy244);
   Compressor_3_2_Freq100_uid214_bh56_uid243_Out0 <= Compressor_3_2_Freq100_uid214_bh56_uid243_Out0_copy244; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid245_In0 <= "" & bh56_w21_0 & bh56_w21_1 & bh56_w21_2 & bh56_w21_3 & bh56_w21_4 & bh56_w21_5;
   bh56_w21_13 <= Compressor_6_3_Freq100_uid204_bh56_uid245_Out0(0);
   bh56_w22_10 <= Compressor_6_3_Freq100_uid204_bh56_uid245_Out0(1);
   bh56_w23_7 <= Compressor_6_3_Freq100_uid204_bh56_uid245_Out0(2);
   Compressor_6_3_Freq100_uid204_uid245: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid245_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid245_Out0_copy246);
   Compressor_6_3_Freq100_uid204_bh56_uid245_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid245_Out0_copy246; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid249_In0 <= "" & bh56_w21_6 & bh56_w21_7 & bh56_w21_8;
   Compressor_23_3_Freq100_uid248_bh56_uid249_In1 <= "" & bh56_w22_0 & bh56_w22_1;
   bh56_w21_14 <= Compressor_23_3_Freq100_uid248_bh56_uid249_Out0(0);
   bh56_w22_11 <= Compressor_23_3_Freq100_uid248_bh56_uid249_Out0(1);
   bh56_w23_8 <= Compressor_23_3_Freq100_uid248_bh56_uid249_Out0(2);
   Compressor_23_3_Freq100_uid248_uid249: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid249_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid249_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid249_Out0_copy250);
   Compressor_23_3_Freq100_uid248_bh56_uid249_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid249_Out0_copy250; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid251_In0 <= "" & bh56_w22_2 & bh56_w22_3 & bh56_w22_4 & bh56_w22_5 & bh56_w22_6 & bh56_w22_7;
   bh56_w22_12 <= Compressor_6_3_Freq100_uid204_bh56_uid251_Out0(0);
   bh56_w23_9 <= Compressor_6_3_Freq100_uid204_bh56_uid251_Out0(1);
   bh56_w24_7 <= Compressor_6_3_Freq100_uid204_bh56_uid251_Out0(2);
   Compressor_6_3_Freq100_uid204_uid251: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid251_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid251_Out0_copy252);
   Compressor_6_3_Freq100_uid204_bh56_uid251_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid251_Out0_copy252; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid253_In0 <= "" & bh56_w23_0 & bh56_w23_1 & bh56_w23_2 & bh56_w23_3 & bh56_w23_4 & bh56_w23_5;
   bh56_w23_10 <= Compressor_6_3_Freq100_uid204_bh56_uid253_Out0(0);
   bh56_w24_8 <= Compressor_6_3_Freq100_uid204_bh56_uid253_Out0(1);
   bh56_w25_7 <= Compressor_6_3_Freq100_uid204_bh56_uid253_Out0(2);
   Compressor_6_3_Freq100_uid204_uid253: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid253_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid253_Out0_copy254);
   Compressor_6_3_Freq100_uid204_bh56_uid253_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid253_Out0_copy254; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid255_In0 <= "" & bh56_w24_0 & bh56_w24_1 & bh56_w24_2 & bh56_w24_3 & bh56_w24_4 & bh56_w24_5;
   bh56_w24_9 <= Compressor_6_3_Freq100_uid204_bh56_uid255_Out0(0);
   bh56_w25_8 <= Compressor_6_3_Freq100_uid204_bh56_uid255_Out0(1);
   bh56_w26_5 <= Compressor_6_3_Freq100_uid204_bh56_uid255_Out0(2);
   Compressor_6_3_Freq100_uid204_uid255: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid255_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid255_Out0_copy256);
   Compressor_6_3_Freq100_uid204_bh56_uid255_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid255_Out0_copy256; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid257_In0 <= "" & bh56_w25_0 & bh56_w25_1 & bh56_w25_2 & bh56_w25_3 & bh56_w25_4 & bh56_w25_5;
   bh56_w25_9 <= Compressor_6_3_Freq100_uid204_bh56_uid257_Out0(0);
   bh56_w26_6 <= Compressor_6_3_Freq100_uid204_bh56_uid257_Out0(1);
   bh56_w27_5 <= Compressor_6_3_Freq100_uid204_bh56_uid257_Out0(2);
   Compressor_6_3_Freq100_uid204_uid257: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid257_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid257_Out0_copy258);
   Compressor_6_3_Freq100_uid204_bh56_uid257_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid257_Out0_copy258; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid259_In0 <= "" & bh56_w26_0 & bh56_w26_1 & bh56_w26_2 & bh56_w26_3;
   Compressor_14_3_Freq100_uid208_bh56_uid259_In1 <= "" & bh56_w27_0;
   bh56_w26_7 <= Compressor_14_3_Freq100_uid208_bh56_uid259_Out0(0);
   bh56_w27_6 <= Compressor_14_3_Freq100_uid208_bh56_uid259_Out0(1);
   bh56_w28_5 <= Compressor_14_3_Freq100_uid208_bh56_uid259_Out0(2);
   Compressor_14_3_Freq100_uid208_uid259: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid259_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid259_In1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid259_Out0_copy260);
   Compressor_14_3_Freq100_uid208_bh56_uid259_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid259_Out0_copy260; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid261_In0 <= "" & bh56_w27_1 & bh56_w27_2 & bh56_w27_3 & bh56_w27_4;
   Compressor_14_3_Freq100_uid208_bh56_uid261_In1 <= "" & bh56_w28_0;
   bh56_w27_7 <= Compressor_14_3_Freq100_uid208_bh56_uid261_Out0(0);
   bh56_w28_6 <= Compressor_14_3_Freq100_uid208_bh56_uid261_Out0(1);
   bh56_w29_3 <= Compressor_14_3_Freq100_uid208_bh56_uid261_Out0(2);
   Compressor_14_3_Freq100_uid208_uid261: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid261_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid261_In1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid261_Out0_copy262);
   Compressor_14_3_Freq100_uid208_bh56_uid261_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid261_Out0_copy262; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid263_In0 <= "" & bh56_w28_1 & bh56_w28_2 & bh56_w28_3 & bh56_w28_4;
   Compressor_14_3_Freq100_uid208_bh56_uid263_In1 <= "" & "0";
   bh56_w28_7 <= Compressor_14_3_Freq100_uid208_bh56_uid263_Out0(0);
   bh56_w29_4 <= Compressor_14_3_Freq100_uid208_bh56_uid263_Out0(1);
   bh56_w30_3 <= Compressor_14_3_Freq100_uid208_bh56_uid263_Out0(2);
   Compressor_14_3_Freq100_uid208_uid263: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid263_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid263_In1_d1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid263_Out0_copy264);
   Compressor_14_3_Freq100_uid208_bh56_uid263_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid263_Out0_copy264; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq100_uid214_bh56_uid265_In0 <= "" & bh56_w29_0 & bh56_w29_1 & bh56_w29_2;
   bh56_w29_5 <= Compressor_3_2_Freq100_uid214_bh56_uid265_Out0(0);
   bh56_w30_4 <= Compressor_3_2_Freq100_uid214_bh56_uid265_Out0(1);
   Compressor_3_2_Freq100_uid214_uid265: Compressor_3_2_Freq100_uid214
      port map ( X0 => Compressor_3_2_Freq100_uid214_bh56_uid265_In0,
                 R => Compressor_3_2_Freq100_uid214_bh56_uid265_Out0_copy266);
   Compressor_3_2_Freq100_uid214_bh56_uid265_Out0 <= Compressor_3_2_Freq100_uid214_bh56_uid265_Out0_copy266; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid267_In0 <= "" & bh56_w30_0 & bh56_w30_1 & bh56_w30_2;
   Compressor_23_3_Freq100_uid248_bh56_uid267_In1 <= "" & bh56_w31_0 & bh56_w31_1;
   bh56_w30_5 <= Compressor_23_3_Freq100_uid248_bh56_uid267_Out0(0);
   bh56_w31_3 <= Compressor_23_3_Freq100_uid248_bh56_uid267_Out0(1);
   bh56_w32_1 <= Compressor_23_3_Freq100_uid248_bh56_uid267_Out0(2);
   Compressor_23_3_Freq100_uid248_uid267: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid267_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid267_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid267_Out0_copy268);
   Compressor_23_3_Freq100_uid248_bh56_uid267_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid267_Out0_copy268; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq100_uid214_bh56_uid269_In0 <= "" & bh56_w12_10_d1 & bh56_w12_11 & bh56_w12_12;
   bh56_w12_13 <= Compressor_3_2_Freq100_uid214_bh56_uid269_Out0(0);
   bh56_w13_15 <= Compressor_3_2_Freq100_uid214_bh56_uid269_Out0(1);
   Compressor_3_2_Freq100_uid214_uid269: Compressor_3_2_Freq100_uid214
      port map ( X0 => Compressor_3_2_Freq100_uid214_bh56_uid269_In0,
                 R => Compressor_3_2_Freq100_uid214_bh56_uid269_Out0_copy270);
   Compressor_3_2_Freq100_uid214_bh56_uid269_Out0 <= Compressor_3_2_Freq100_uid214_bh56_uid269_Out0_copy270; -- output copy to hold a pipeline register if needed


   Compressor_5_3_Freq100_uid272_bh56_uid273_In0 <= "" & bh56_w13_10_d1 & bh56_w13_11 & bh56_w13_12 & bh56_w13_13 & bh56_w13_14;
   bh56_w13_16 <= Compressor_5_3_Freq100_uid272_bh56_uid273_Out0(0);
   bh56_w14_18 <= Compressor_5_3_Freq100_uid272_bh56_uid273_Out0(1);
   bh56_w15_17 <= Compressor_5_3_Freq100_uid272_bh56_uid273_Out0(2);
   Compressor_5_3_Freq100_uid272_uid273: Compressor_5_3_Freq100_uid272
      port map ( X0 => Compressor_5_3_Freq100_uid272_bh56_uid273_In0,
                 R => Compressor_5_3_Freq100_uid272_bh56_uid273_Out0_copy274);
   Compressor_5_3_Freq100_uid272_bh56_uid273_Out0 <= Compressor_5_3_Freq100_uid272_bh56_uid273_Out0_copy274; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid275_In0 <= "" & bh56_w14_16 & bh56_w14_15 & bh56_w14_14 & bh56_w14_13 & bh56_w14_12 & bh56_w14_17;
   bh56_w14_19 <= Compressor_6_3_Freq100_uid204_bh56_uid275_Out0(0);
   bh56_w15_18 <= Compressor_6_3_Freq100_uid204_bh56_uid275_Out0(1);
   bh56_w16_16 <= Compressor_6_3_Freq100_uid204_bh56_uid275_Out0(2);
   Compressor_6_3_Freq100_uid204_uid275: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid275_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid275_Out0_copy276);
   Compressor_6_3_Freq100_uid204_bh56_uid275_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid275_Out0_copy276; -- output copy to hold a pipeline register if needed


   Compressor_5_3_Freq100_uid272_bh56_uid277_In0 <= "" & bh56_w15_12 & bh56_w15_13 & bh56_w15_14 & bh56_w15_15 & bh56_w15_16;
   bh56_w15_19 <= Compressor_5_3_Freq100_uid272_bh56_uid277_Out0(0);
   bh56_w16_17 <= Compressor_5_3_Freq100_uid272_bh56_uid277_Out0(1);
   bh56_w17_17 <= Compressor_5_3_Freq100_uid272_bh56_uid277_Out0(2);
   Compressor_5_3_Freq100_uid272_uid277: Compressor_5_3_Freq100_uid272
      port map ( X0 => Compressor_5_3_Freq100_uid272_bh56_uid277_In0,
                 R => Compressor_5_3_Freq100_uid272_bh56_uid277_Out0_copy278);
   Compressor_5_3_Freq100_uid272_bh56_uid277_Out0 <= Compressor_5_3_Freq100_uid272_bh56_uid277_Out0_copy278; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid279_In0 <= "" & bh56_w16_10 & bh56_w16_11 & bh56_w16_12 & bh56_w16_13 & bh56_w16_14 & bh56_w16_15;
   bh56_w16_18 <= Compressor_6_3_Freq100_uid204_bh56_uid279_Out0(0);
   bh56_w17_18 <= Compressor_6_3_Freq100_uid204_bh56_uid279_Out0(1);
   bh56_w18_17 <= Compressor_6_3_Freq100_uid204_bh56_uid279_Out0(2);
   Compressor_6_3_Freq100_uid204_uid279: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid279_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid279_Out0_copy280);
   Compressor_6_3_Freq100_uid204_bh56_uid279_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid279_Out0_copy280; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid281_In0 <= "" & bh56_w17_11 & bh56_w17_12 & bh56_w17_13 & bh56_w17_14 & bh56_w17_15 & bh56_w17_16;
   bh56_w17_19 <= Compressor_6_3_Freq100_uid204_bh56_uid281_Out0(0);
   bh56_w18_18 <= Compressor_6_3_Freq100_uid204_bh56_uid281_Out0(1);
   bh56_w19_17 <= Compressor_6_3_Freq100_uid204_bh56_uid281_Out0(2);
   Compressor_6_3_Freq100_uid204_uid281: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid281_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid281_Out0_copy282);
   Compressor_6_3_Freq100_uid204_bh56_uid281_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid281_Out0_copy282; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid283_In0 <= "" & bh56_w18_11 & bh56_w18_12 & bh56_w18_13 & bh56_w18_14 & bh56_w18_15 & bh56_w18_16;
   bh56_w18_19 <= Compressor_6_3_Freq100_uid204_bh56_uid283_Out0(0);
   bh56_w19_18 <= Compressor_6_3_Freq100_uid204_bh56_uid283_Out0(1);
   bh56_w20_15 <= Compressor_6_3_Freq100_uid204_bh56_uid283_Out0(2);
   Compressor_6_3_Freq100_uid204_uid283: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid283_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid283_Out0_copy284);
   Compressor_6_3_Freq100_uid204_bh56_uid283_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid283_Out0_copy284; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid285_In0 <= "" & bh56_w19_11 & bh56_w19_12 & bh56_w19_13 & bh56_w19_14 & bh56_w19_15 & bh56_w19_16;
   bh56_w19_19 <= Compressor_6_3_Freq100_uid204_bh56_uid285_Out0(0);
   bh56_w20_16 <= Compressor_6_3_Freq100_uid204_bh56_uid285_Out0(1);
   bh56_w21_15 <= Compressor_6_3_Freq100_uid204_bh56_uid285_Out0(2);
   Compressor_6_3_Freq100_uid204_uid285: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid285_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid285_Out0_copy286);
   Compressor_6_3_Freq100_uid204_bh56_uid285_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid285_Out0_copy286; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid287_In0 <= "" & bh56_w20_9 & bh56_w20_10 & bh56_w20_11 & bh56_w20_12 & bh56_w20_13 & bh56_w20_14;
   bh56_w20_17 <= Compressor_6_3_Freq100_uid204_bh56_uid287_Out0(0);
   bh56_w21_16 <= Compressor_6_3_Freq100_uid204_bh56_uid287_Out0(1);
   bh56_w22_13 <= Compressor_6_3_Freq100_uid204_bh56_uid287_Out0(2);
   Compressor_6_3_Freq100_uid204_uid287: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid287_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid287_Out0_copy288);
   Compressor_6_3_Freq100_uid204_bh56_uid287_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid287_Out0_copy288; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq100_uid204_bh56_uid289_In0 <= "" & bh56_w21_9 & bh56_w21_10 & bh56_w21_11 & bh56_w21_12 & bh56_w21_13 & bh56_w21_14;
   bh56_w21_17 <= Compressor_6_3_Freq100_uid204_bh56_uid289_Out0(0);
   bh56_w22_14 <= Compressor_6_3_Freq100_uid204_bh56_uid289_Out0(1);
   bh56_w23_11 <= Compressor_6_3_Freq100_uid204_bh56_uid289_Out0(2);
   Compressor_6_3_Freq100_uid204_uid289: Compressor_6_3_Freq100_uid204
      port map ( X0 => Compressor_6_3_Freq100_uid204_bh56_uid289_In0,
                 R => Compressor_6_3_Freq100_uid204_bh56_uid289_Out0_copy290);
   Compressor_6_3_Freq100_uid204_bh56_uid289_Out0 <= Compressor_6_3_Freq100_uid204_bh56_uid289_Out0_copy290; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid291_In0 <= "" & bh56_w22_8 & bh56_w22_9 & bh56_w22_10 & bh56_w22_11;
   Compressor_14_3_Freq100_uid208_bh56_uid291_In1 <= "" & bh56_w23_6;
   bh56_w22_15 <= Compressor_14_3_Freq100_uid208_bh56_uid291_Out0(0);
   bh56_w23_12 <= Compressor_14_3_Freq100_uid208_bh56_uid291_Out0(1);
   bh56_w24_10 <= Compressor_14_3_Freq100_uid208_bh56_uid291_Out0(2);
   Compressor_14_3_Freq100_uid208_uid291: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid291_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid291_In1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid291_Out0_copy292);
   Compressor_14_3_Freq100_uid208_bh56_uid291_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid291_Out0_copy292; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid293_In0 <= "" & bh56_w23_7 & bh56_w23_8 & bh56_w23_9 & bh56_w23_10;
   Compressor_14_3_Freq100_uid208_bh56_uid293_In1 <= "" & bh56_w24_6;
   bh56_w23_13 <= Compressor_14_3_Freq100_uid208_bh56_uid293_Out0(0);
   bh56_w24_11 <= Compressor_14_3_Freq100_uid208_bh56_uid293_Out0(1);
   bh56_w25_10 <= Compressor_14_3_Freq100_uid208_bh56_uid293_Out0(2);
   Compressor_14_3_Freq100_uid208_uid293: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid293_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid293_In1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid293_Out0_copy294);
   Compressor_14_3_Freq100_uid208_bh56_uid293_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid293_Out0_copy294; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq100_uid214_bh56_uid295_In0 <= "" & bh56_w24_7 & bh56_w24_8 & bh56_w24_9;
   bh56_w24_12 <= Compressor_3_2_Freq100_uid214_bh56_uid295_Out0(0);
   bh56_w25_11 <= Compressor_3_2_Freq100_uid214_bh56_uid295_Out0(1);
   Compressor_3_2_Freq100_uid214_uid295: Compressor_3_2_Freq100_uid214
      port map ( X0 => Compressor_3_2_Freq100_uid214_bh56_uid295_In0,
                 R => Compressor_3_2_Freq100_uid214_bh56_uid295_Out0_copy296);
   Compressor_3_2_Freq100_uid214_bh56_uid295_Out0 <= Compressor_3_2_Freq100_uid214_bh56_uid295_Out0_copy296; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid297_In0 <= "" & bh56_w25_6 & bh56_w25_7 & bh56_w25_8 & bh56_w25_9;
   Compressor_14_3_Freq100_uid208_bh56_uid297_In1 <= "" & bh56_w26_4;
   bh56_w25_12 <= Compressor_14_3_Freq100_uid208_bh56_uid297_Out0(0);
   bh56_w26_8 <= Compressor_14_3_Freq100_uid208_bh56_uid297_Out0(1);
   bh56_w27_8 <= Compressor_14_3_Freq100_uid208_bh56_uid297_Out0(2);
   Compressor_14_3_Freq100_uid208_uid297: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid297_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid297_In1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid297_Out0_copy298);
   Compressor_14_3_Freq100_uid208_bh56_uid297_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid297_Out0_copy298; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid299_In0 <= "" & bh56_w26_5 & bh56_w26_6 & bh56_w26_7;
   Compressor_23_3_Freq100_uid248_bh56_uid299_In1 <= "" & bh56_w27_5 & bh56_w27_6;
   bh56_w26_9 <= Compressor_23_3_Freq100_uid248_bh56_uid299_Out0(0);
   bh56_w27_9 <= Compressor_23_3_Freq100_uid248_bh56_uid299_Out0(1);
   bh56_w28_8 <= Compressor_23_3_Freq100_uid248_bh56_uid299_Out0(2);
   Compressor_23_3_Freq100_uid248_uid299: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid299_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid299_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid299_Out0_copy300);
   Compressor_23_3_Freq100_uid248_bh56_uid299_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid299_Out0_copy300; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid301_In0 <= "" & bh56_w28_5 & bh56_w28_6 & bh56_w28_7;
   Compressor_23_3_Freq100_uid248_bh56_uid301_In1 <= "" & bh56_w29_3 & bh56_w29_4;
   bh56_w28_9 <= Compressor_23_3_Freq100_uid248_bh56_uid301_Out0(0);
   bh56_w29_6 <= Compressor_23_3_Freq100_uid248_bh56_uid301_Out0(1);
   bh56_w30_6 <= Compressor_23_3_Freq100_uid248_bh56_uid301_Out0(2);
   Compressor_23_3_Freq100_uid248_uid301: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid301_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid301_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid301_Out0_copy302);
   Compressor_23_3_Freq100_uid248_bh56_uid301_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid301_Out0_copy302; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid303_In0 <= "" & bh56_w30_3 & bh56_w30_4 & bh56_w30_5;
   Compressor_23_3_Freq100_uid248_bh56_uid303_In1 <= "" & bh56_w31_2 & bh56_w31_3;
   bh56_w30_7 <= Compressor_23_3_Freq100_uid248_bh56_uid303_Out0(0);
   bh56_w31_4 <= Compressor_23_3_Freq100_uid248_bh56_uid303_Out0(1);
   bh56_w32_2 <= Compressor_23_3_Freq100_uid248_bh56_uid303_Out0(2);
   Compressor_23_3_Freq100_uid248_uid303: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid303_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid303_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid303_Out0_copy304);
   Compressor_23_3_Freq100_uid248_bh56_uid303_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid303_Out0_copy304; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid305_In0 <= "" & bh56_w32_0 & bh56_w32_1 & "0" & "0";
   Compressor_14_3_Freq100_uid208_bh56_uid305_In1 <= "" & bh56_w33_0;
   bh56_w32_3 <= Compressor_14_3_Freq100_uid208_bh56_uid305_Out0(0);
   bh56_w33_1 <= Compressor_14_3_Freq100_uid208_bh56_uid305_Out0(1);
   bh56_w34_1 <= Compressor_14_3_Freq100_uid208_bh56_uid305_Out0(2);
   Compressor_14_3_Freq100_uid208_uid305: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid305_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid305_In1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid305_Out0_copy306);
   Compressor_14_3_Freq100_uid208_bh56_uid305_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid305_Out0_copy306; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid307_In0 <= "" & bh56_w13_15 & bh56_w13_16 & "0";
   Compressor_23_3_Freq100_uid248_bh56_uid307_In1 <= "" & bh56_w14_18 & bh56_w14_19;
   bh56_w13_17 <= Compressor_23_3_Freq100_uid248_bh56_uid307_Out0(0);
   bh56_w14_20 <= Compressor_23_3_Freq100_uid248_bh56_uid307_Out0(1);
   bh56_w15_20 <= Compressor_23_3_Freq100_uid248_bh56_uid307_Out0(2);
   Compressor_23_3_Freq100_uid248_uid307: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid307_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid307_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid307_Out0_copy308);
   Compressor_23_3_Freq100_uid248_bh56_uid307_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid307_Out0_copy308_d1; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid309_In0 <= "" & bh56_w15_17 & bh56_w15_18 & bh56_w15_19;
   Compressor_23_3_Freq100_uid248_bh56_uid309_In1 <= "" & bh56_w16_16 & bh56_w16_17;
   bh56_w15_21 <= Compressor_23_3_Freq100_uid248_bh56_uid309_Out0(0);
   bh56_w16_19 <= Compressor_23_3_Freq100_uid248_bh56_uid309_Out0(1);
   bh56_w17_20 <= Compressor_23_3_Freq100_uid248_bh56_uid309_Out0(2);
   Compressor_23_3_Freq100_uid248_uid309: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid309_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid309_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid309_Out0_copy310);
   Compressor_23_3_Freq100_uid248_bh56_uid309_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid309_Out0_copy310_d1; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid311_In0 <= "" & bh56_w17_17 & bh56_w17_18 & bh56_w17_19;
   Compressor_23_3_Freq100_uid248_bh56_uid311_In1 <= "" & bh56_w18_17 & bh56_w18_18;
   bh56_w17_21 <= Compressor_23_3_Freq100_uid248_bh56_uid311_Out0(0);
   bh56_w18_20 <= Compressor_23_3_Freq100_uid248_bh56_uid311_Out0(1);
   bh56_w19_20 <= Compressor_23_3_Freq100_uid248_bh56_uid311_Out0(2);
   Compressor_23_3_Freq100_uid248_uid311: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid311_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid311_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid311_Out0_copy312);
   Compressor_23_3_Freq100_uid248_bh56_uid311_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid311_Out0_copy312_d1; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid313_In0 <= "" & bh56_w19_17 & bh56_w19_18 & bh56_w19_19;
   Compressor_23_3_Freq100_uid248_bh56_uid313_In1 <= "" & bh56_w20_15 & bh56_w20_16;
   bh56_w19_21 <= Compressor_23_3_Freq100_uid248_bh56_uid313_Out0(0);
   bh56_w20_18 <= Compressor_23_3_Freq100_uid248_bh56_uid313_Out0(1);
   bh56_w21_18 <= Compressor_23_3_Freq100_uid248_bh56_uid313_Out0(2);
   Compressor_23_3_Freq100_uid248_uid313: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid313_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid313_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid313_Out0_copy314);
   Compressor_23_3_Freq100_uid248_bh56_uid313_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid313_Out0_copy314_d1; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq100_uid214_bh56_uid315_In0 <= "" & bh56_w21_15 & bh56_w21_16 & bh56_w21_17;
   bh56_w21_19 <= Compressor_3_2_Freq100_uid214_bh56_uid315_Out0(0);
   bh56_w22_16 <= Compressor_3_2_Freq100_uid214_bh56_uid315_Out0(1);
   Compressor_3_2_Freq100_uid214_uid315: Compressor_3_2_Freq100_uid214
      port map ( X0 => Compressor_3_2_Freq100_uid214_bh56_uid315_In0,
                 R => Compressor_3_2_Freq100_uid214_bh56_uid315_Out0_copy316);
   Compressor_3_2_Freq100_uid214_bh56_uid315_Out0 <= Compressor_3_2_Freq100_uid214_bh56_uid315_Out0_copy316_d1; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid317_In0 <= "" & bh56_w22_12 & bh56_w22_13 & bh56_w22_14 & bh56_w22_15;
   Compressor_14_3_Freq100_uid208_bh56_uid317_In1 <= "" & "0";
   bh56_w22_17 <= Compressor_14_3_Freq100_uid208_bh56_uid317_Out0(0);
   bh56_w23_14 <= Compressor_14_3_Freq100_uid208_bh56_uid317_Out0(1);
   bh56_w24_13 <= Compressor_14_3_Freq100_uid208_bh56_uid317_Out0(2);
   Compressor_14_3_Freq100_uid208_uid317: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid317_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid317_In1_d1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid317_Out0_copy318);
   Compressor_14_3_Freq100_uid208_bh56_uid317_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid317_Out0_copy318_d1; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq100_uid214_bh56_uid319_In0 <= "" & bh56_w23_11 & bh56_w23_12 & bh56_w23_13;
   bh56_w23_15 <= Compressor_3_2_Freq100_uid214_bh56_uid319_Out0(0);
   bh56_w24_14 <= Compressor_3_2_Freq100_uid214_bh56_uid319_Out0(1);
   Compressor_3_2_Freq100_uid214_uid319: Compressor_3_2_Freq100_uid214
      port map ( X0 => Compressor_3_2_Freq100_uid214_bh56_uid319_In0,
                 R => Compressor_3_2_Freq100_uid214_bh56_uid319_Out0_copy320);
   Compressor_3_2_Freq100_uid214_bh56_uid319_Out0 <= Compressor_3_2_Freq100_uid214_bh56_uid319_Out0_copy320_d1; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid321_In0 <= "" & bh56_w24_10 & bh56_w24_11 & bh56_w24_12;
   Compressor_23_3_Freq100_uid248_bh56_uid321_In1 <= "" & "0" & "0";
   bh56_w24_15 <= Compressor_23_3_Freq100_uid248_bh56_uid321_Out0(0);
   bh56_w25_13 <= Compressor_23_3_Freq100_uid248_bh56_uid321_Out0(1);
   bh56_w26_10 <= Compressor_23_3_Freq100_uid248_bh56_uid321_Out0(2);
   Compressor_23_3_Freq100_uid248_uid321: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid321_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid321_In1_d1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid321_Out0_copy322);
   Compressor_23_3_Freq100_uid248_bh56_uid321_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid321_Out0_copy322_d1; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid323_In0 <= "" & bh56_w25_10 & bh56_w25_11 & bh56_w25_12;
   Compressor_23_3_Freq100_uid248_bh56_uid323_In1 <= "" & bh56_w26_8 & bh56_w26_9;
   bh56_w25_14 <= Compressor_23_3_Freq100_uid248_bh56_uid323_Out0(0);
   bh56_w26_11 <= Compressor_23_3_Freq100_uid248_bh56_uid323_Out0(1);
   bh56_w27_10 <= Compressor_23_3_Freq100_uid248_bh56_uid323_Out0(2);
   Compressor_23_3_Freq100_uid248_uid323: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid323_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid323_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid323_Out0_copy324);
   Compressor_23_3_Freq100_uid248_bh56_uid323_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid323_Out0_copy324_d1; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid325_In0 <= "" & bh56_w27_7 & bh56_w27_8 & bh56_w27_9;
   Compressor_23_3_Freq100_uid248_bh56_uid325_In1 <= "" & bh56_w28_8 & bh56_w28_9;
   bh56_w27_11 <= Compressor_23_3_Freq100_uid248_bh56_uid325_Out0(0);
   bh56_w28_10 <= Compressor_23_3_Freq100_uid248_bh56_uid325_Out0(1);
   bh56_w29_7 <= Compressor_23_3_Freq100_uid248_bh56_uid325_Out0(2);
   Compressor_23_3_Freq100_uid248_uid325: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid325_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid325_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid325_Out0_copy326);
   Compressor_23_3_Freq100_uid248_bh56_uid325_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid325_Out0_copy326_d1; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid327_In0 <= "" & bh56_w29_5 & bh56_w29_6 & "0";
   Compressor_23_3_Freq100_uid248_bh56_uid327_In1 <= "" & bh56_w30_6 & bh56_w30_7;
   bh56_w29_8 <= Compressor_23_3_Freq100_uid248_bh56_uid327_Out0(0);
   bh56_w30_8 <= Compressor_23_3_Freq100_uid248_bh56_uid327_Out0(1);
   bh56_w31_5 <= Compressor_23_3_Freq100_uid248_bh56_uid327_Out0(2);
   Compressor_23_3_Freq100_uid248_uid327: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid327_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid327_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid327_Out0_copy328);
   Compressor_23_3_Freq100_uid248_bh56_uid327_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid327_Out0_copy328_d1; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid329_In0 <= "" & bh56_w32_2 & bh56_w32_3 & "0" & "0";
   Compressor_14_3_Freq100_uid208_bh56_uid329_In1 <= "" & bh56_w33_1;
   bh56_w32_4 <= Compressor_14_3_Freq100_uid208_bh56_uid329_Out0(0);
   bh56_w33_2 <= Compressor_14_3_Freq100_uid208_bh56_uid329_Out0(1);
   bh56_w34_2 <= Compressor_14_3_Freq100_uid208_bh56_uid329_Out0(2);
   Compressor_14_3_Freq100_uid208_uid329: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid329_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid329_In1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid329_Out0_copy330);
   Compressor_14_3_Freq100_uid208_bh56_uid329_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid329_Out0_copy330_d1; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq100_uid214_bh56_uid331_In0 <= "" & bh56_w34_0 & bh56_w34_1 & "0";
   bh56_w34_3 <= Compressor_3_2_Freq100_uid214_bh56_uid331_Out0(0);
   Compressor_3_2_Freq100_uid214_uid331: Compressor_3_2_Freq100_uid214
      port map ( X0 => Compressor_3_2_Freq100_uid214_bh56_uid331_In0,
                 R => Compressor_3_2_Freq100_uid214_bh56_uid331_Out0_copy332);
   Compressor_3_2_Freq100_uid214_bh56_uid331_Out0 <= Compressor_3_2_Freq100_uid214_bh56_uid331_Out0_copy332_d1; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid333_In0 <= "" & bh56_w15_20 & bh56_w15_21 & "0";
   Compressor_23_3_Freq100_uid248_bh56_uid333_In1 <= "" & bh56_w16_18_d1 & bh56_w16_19;
   bh56_w15_22 <= Compressor_23_3_Freq100_uid248_bh56_uid333_Out0(0);
   bh56_w16_20 <= Compressor_23_3_Freq100_uid248_bh56_uid333_Out0(1);
   bh56_w17_22 <= Compressor_23_3_Freq100_uid248_bh56_uid333_Out0(2);
   Compressor_23_3_Freq100_uid248_uid333: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid333_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid333_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid333_Out0_copy334);
   Compressor_23_3_Freq100_uid248_bh56_uid333_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid333_Out0_copy334; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid335_In0 <= "" & bh56_w17_20 & bh56_w17_21 & "0";
   Compressor_23_3_Freq100_uid248_bh56_uid335_In1 <= "" & bh56_w18_19_d1 & bh56_w18_20;
   bh56_w17_23 <= Compressor_23_3_Freq100_uid248_bh56_uid335_Out0(0);
   bh56_w18_21 <= Compressor_23_3_Freq100_uid248_bh56_uid335_Out0(1);
   bh56_w19_22 <= Compressor_23_3_Freq100_uid248_bh56_uid335_Out0(2);
   Compressor_23_3_Freq100_uid248_uid335: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid335_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid335_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid335_Out0_copy336);
   Compressor_23_3_Freq100_uid248_bh56_uid335_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid335_Out0_copy336; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid337_In0 <= "" & bh56_w19_20 & bh56_w19_21 & "0";
   Compressor_23_3_Freq100_uid248_bh56_uid337_In1 <= "" & bh56_w20_17_d1 & bh56_w20_18;
   bh56_w19_23 <= Compressor_23_3_Freq100_uid248_bh56_uid337_Out0(0);
   bh56_w20_19 <= Compressor_23_3_Freq100_uid248_bh56_uid337_Out0(1);
   bh56_w21_20 <= Compressor_23_3_Freq100_uid248_bh56_uid337_Out0(2);
   Compressor_23_3_Freq100_uid248_uid337: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid337_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid337_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid337_Out0_copy338);
   Compressor_23_3_Freq100_uid248_bh56_uid337_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid337_Out0_copy338; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid339_In0 <= "" & bh56_w21_18 & bh56_w21_19 & "0";
   Compressor_23_3_Freq100_uid248_bh56_uid339_In1 <= "" & bh56_w22_16 & bh56_w22_17;
   bh56_w21_21 <= Compressor_23_3_Freq100_uid248_bh56_uid339_Out0(0);
   bh56_w22_18 <= Compressor_23_3_Freq100_uid248_bh56_uid339_Out0(1);
   bh56_w23_16 <= Compressor_23_3_Freq100_uid248_bh56_uid339_Out0(2);
   Compressor_23_3_Freq100_uid248_uid339: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid339_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid339_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid339_Out0_copy340);
   Compressor_23_3_Freq100_uid248_bh56_uid339_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid339_Out0_copy340; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq100_uid214_bh56_uid341_In0 <= "" & bh56_w23_14 & bh56_w23_15 & "0";
   bh56_w23_17 <= Compressor_3_2_Freq100_uid214_bh56_uid341_Out0(0);
   bh56_w24_16 <= Compressor_3_2_Freq100_uid214_bh56_uid341_Out0(1);
   Compressor_3_2_Freq100_uid214_uid341: Compressor_3_2_Freq100_uid214
      port map ( X0 => Compressor_3_2_Freq100_uid214_bh56_uid341_In0,
                 R => Compressor_3_2_Freq100_uid214_bh56_uid341_Out0_copy342);
   Compressor_3_2_Freq100_uid214_bh56_uid341_Out0 <= Compressor_3_2_Freq100_uid214_bh56_uid341_Out0_copy342; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid343_In0 <= "" & bh56_w24_13 & bh56_w24_14 & bh56_w24_15;
   Compressor_23_3_Freq100_uid248_bh56_uid343_In1 <= "" & bh56_w25_13 & bh56_w25_14;
   bh56_w24_17 <= Compressor_23_3_Freq100_uid248_bh56_uid343_Out0(0);
   bh56_w25_15 <= Compressor_23_3_Freq100_uid248_bh56_uid343_Out0(1);
   bh56_w26_12 <= Compressor_23_3_Freq100_uid248_bh56_uid343_Out0(2);
   Compressor_23_3_Freq100_uid248_uid343: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid343_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid343_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid343_Out0_copy344);
   Compressor_23_3_Freq100_uid248_bh56_uid343_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid343_Out0_copy344; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq100_uid248_bh56_uid345_In0 <= "" & bh56_w26_10 & bh56_w26_11 & "0";
   Compressor_23_3_Freq100_uid248_bh56_uid345_In1 <= "" & bh56_w27_10 & bh56_w27_11;
   bh56_w26_13 <= Compressor_23_3_Freq100_uid248_bh56_uid345_Out0(0);
   bh56_w27_12 <= Compressor_23_3_Freq100_uid248_bh56_uid345_Out0(1);
   bh56_w28_11 <= Compressor_23_3_Freq100_uid248_bh56_uid345_Out0(2);
   Compressor_23_3_Freq100_uid248_uid345: Compressor_23_3_Freq100_uid248
      port map ( X0 => Compressor_23_3_Freq100_uid248_bh56_uid345_In0,
                 X1 => Compressor_23_3_Freq100_uid248_bh56_uid345_In1,
                 R => Compressor_23_3_Freq100_uid248_bh56_uid345_Out0_copy346);
   Compressor_23_3_Freq100_uid248_bh56_uid345_Out0 <= Compressor_23_3_Freq100_uid248_bh56_uid345_Out0_copy346; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid347_In0 <= "" & bh56_w29_7 & bh56_w29_8 & "0" & "0";
   Compressor_14_3_Freq100_uid208_bh56_uid347_In1 <= "" & bh56_w30_8;
   bh56_w29_9 <= Compressor_14_3_Freq100_uid208_bh56_uid347_Out0(0);
   bh56_w30_9 <= Compressor_14_3_Freq100_uid208_bh56_uid347_Out0(1);
   bh56_w31_6 <= Compressor_14_3_Freq100_uid208_bh56_uid347_Out0(2);
   Compressor_14_3_Freq100_uid208_uid347: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid347_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid347_In1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid347_Out0_copy348);
   Compressor_14_3_Freq100_uid208_bh56_uid347_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid347_Out0_copy348; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid349_In0 <= "" & bh56_w31_4_d1 & bh56_w31_5 & "0" & "0";
   Compressor_14_3_Freq100_uid208_bh56_uid349_In1 <= "" & bh56_w32_4;
   bh56_w31_7 <= Compressor_14_3_Freq100_uid208_bh56_uid349_Out0(0);
   bh56_w32_5 <= Compressor_14_3_Freq100_uid208_bh56_uid349_Out0(1);
   bh56_w33_3 <= Compressor_14_3_Freq100_uid208_bh56_uid349_Out0(2);
   Compressor_14_3_Freq100_uid208_uid349: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid349_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid349_In1,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid349_Out0_copy350);
   Compressor_14_3_Freq100_uid208_bh56_uid349_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid349_Out0_copy350; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq100_uid208_bh56_uid351_In0 <= "" & bh56_w34_2 & bh56_w34_3 & "0" & "0";
   Compressor_14_3_Freq100_uid208_bh56_uid351_In1 <= "" & "0";
   bh56_w34_4 <= Compressor_14_3_Freq100_uid208_bh56_uid351_Out0(0);
   Compressor_14_3_Freq100_uid208_uid351: Compressor_14_3_Freq100_uid208
      port map ( X0 => Compressor_14_3_Freq100_uid208_bh56_uid351_In0,
                 X1 => Compressor_14_3_Freq100_uid208_bh56_uid351_In1_d2,
                 R => Compressor_14_3_Freq100_uid208_bh56_uid351_Out0_copy352);
   Compressor_14_3_Freq100_uid208_bh56_uid351_Out0 <= Compressor_14_3_Freq100_uid208_bh56_uid351_Out0_copy352; -- output copy to hold a pipeline register if needed

   tmp_bitheapResult_bh56_16 <= bh56_w16_20 & bh56_w15_22 & bh56_w14_20 & bh56_w13_17 & bh56_w12_13_d1 & "0" & "0" & "0" & "0" & "0" & "0" & "0" & "0" & "0" & "0" & "0" & "0";

   bitheapFinalAdd_bh56_In0 <= "0" & bh56_w34_4 & bh56_w33_2 & bh56_w32_5 & bh56_w31_6 & bh56_w30_9 & bh56_w29_9 & bh56_w28_10 & bh56_w27_12 & bh56_w26_12 & bh56_w25_15 & bh56_w24_16 & bh56_w23_16 & bh56_w22_18 & bh56_w21_20 & bh56_w20_19 & bh56_w19_22 & bh56_w18_21 & bh56_w17_22;
   bitheapFinalAdd_bh56_In1 <= "0" & "0" & bh56_w33_3 & "0" & bh56_w31_7 & "0" & "0" & bh56_w28_11 & "0" & bh56_w26_13 & "0" & bh56_w24_17 & bh56_w23_17 & "0" & bh56_w21_21 & "0" & bh56_w19_23 & "0" & bh56_w17_23;
   bitheapFinalAdd_bh56_Cin <= '0';

   bitheapFinalAdd_bh56: IntAdder_19_Freq100_uid354
      port map ( clk  => clk,
                 Cin => bitheapFinalAdd_bh56_Cin,
                 X => bitheapFinalAdd_bh56_In0,
                 Y => bitheapFinalAdd_bh56_In1,
                 R => bitheapFinalAdd_bh56_Out);
   bitheapResult_bh56 <= bitheapFinalAdd_bh56_Out(17 downto 0) & tmp_bitheapResult_bh56_16;
   R <= bitheapResult_bh56(34 downto 16);
end architecture;

--------------------------------------------------------------------------------
--                         IntAdder_28_Freq100_uid357
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 2 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y Cin
-- Output signals: R
--  approx. input signal timings: X: (c1, 5.992000ns)Y: (c2, 2.822000ns)Cin: (c0, 0.000000ns)
--  approx. output signal timings: R: (c2, 4.760000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_28_Freq100_uid357 is
    port (clk : in std_logic;
          X : in  std_logic_vector(27 downto 0);
          Y : in  std_logic_vector(27 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(27 downto 0)   );
end entity;

architecture arch of IntAdder_28_Freq100_uid357 is
signal Rtmp :  std_logic_vector(27 downto 0);
   -- timing of Rtmp: (c2, 4.760000ns)
signal X_d1 :  std_logic_vector(27 downto 0);
   -- timing of X: (c1, 5.992000ns)
signal Cin_d1, Cin_d2 :  std_logic;
   -- timing of Cin: (c0, 0.000000ns)
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            X_d1 <=  X;
            Cin_d1 <=  Cin;
            Cin_d2 <=  Cin_d1;
         end if;
      end process;
   Rtmp <= X_d1 + Y + Cin_d2;
   R <= Rtmp;
end architecture;

--------------------------------------------------------------------------------
--                           Exp_8_23_Freq100_uid6
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: F. de Dinechin, Bogdan Pasca (2008-2021)
--------------------------------------------------------------------------------
-- Pipeline depth: 2 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: ufixX_i XSign
-- Output signals: expY K
--  approx. input signal timings: ufixX_i: (c0, 6.138000ns)XSign: (c0, 0.000000ns)
--  approx. output signal timings: expY: (c2, 4.760000ns)K: (c1, 0.130000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Exp_8_23_Freq100_uid6 is
    port (clk : in std_logic;
          ufixX_i : in  std_logic_vector(33 downto 0);
          XSign : in  std_logic;
          expY : out  std_logic_vector(27 downto 0);
          K : out  std_logic_vector(8 downto 0)   );
end entity;

architecture arch of Exp_8_23_Freq100_uid6 is
   component FixRealKCM_Freq100_uid8 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(9 downto 0);
             R : out  std_logic_vector(7 downto 0)   );
   end component;

   component FixRealKCM_Freq100_uid20 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(7 downto 0);
             R : out  std_logic_vector(34 downto 0)   );
   end component;

   component IntAdder_27_Freq100_uid33 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(26 downto 0);
             Y : in  std_logic_vector(26 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(26 downto 0)   );
   end component;

   component FixFunctionByTable_Freq100_uid35 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(9 downto 0);
             Y : out  std_logic_vector(27 downto 0)   );
   end component;

   component FixFunctionByTable_Freq100_uid44 is
      port ( X : in  std_logic_vector(6 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component IntAdder_18_Freq100_uid48 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(17 downto 0);
             Y : in  std_logic_vector(17 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(17 downto 0)   );
   end component;

   component IntAdder_18_Freq100_uid52 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(17 downto 0);
             Y : in  std_logic_vector(17 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(17 downto 0)   );
   end component;

   component IntMultiplier_17x18_19_Freq100_uid54 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(16 downto 0);
             Y : in  std_logic_vector(17 downto 0);
             R : out  std_logic_vector(18 downto 0)   );
   end component;

   component IntAdder_28_Freq100_uid357 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(27 downto 0);
             Y : in  std_logic_vector(27 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(27 downto 0)   );
   end component;

signal ufixX :  unsigned(6+27 downto 0);
   -- timing of ufixX: (c0, 6.138000ns)
signal xMulIn :  unsigned(6+3 downto 0);
   -- timing of xMulIn: (c0, 6.138000ns)
signal absK, absK_d1 :  std_logic_vector(7 downto 0);
   -- timing of absK: (c0, 8.244000ns)
signal minusAbsK :  std_logic_vector(8 downto 0);
   -- timing of minusAbsK: (c1, 0.130000ns)
signal absKLog2 :  std_logic_vector(34 downto 0);
   -- timing of absKLog2: (c1, 1.552000ns)
signal subOp1 :  std_logic_vector(26 downto 0);
   -- timing of subOp1: (c0, 6.762000ns)
signal subOp2 :  std_logic_vector(26 downto 0);
   -- timing of subOp2: (c1, 1.552000ns)
signal Y :  std_logic_vector(26 downto 0);
   -- timing of Y: (c1, 3.376000ns)
signal A :  std_logic_vector(9 downto 0);
   -- timing of A: (c1, 3.376000ns)
signal Z :  std_logic_vector(16 downto 0);
   -- timing of Z: (c1, 3.376000ns)
signal expA :  std_logic_vector(27 downto 0);
   -- timing of expA: (c1, 5.992000ns)
signal Ztrunc :  std_logic_vector(6 downto 0);
   -- timing of Ztrunc: (c1, 3.376000ns)
signal expZmZm1 :  std_logic_vector(5 downto 0);
   -- timing of expZmZm1: (c1, 4.624000ns)
signal expZmZm1_copy45 :  std_logic_vector(5 downto 0);
   -- timing of expZmZm1_copy45: (c1, 3.376000ns)
signal expZm1adderX :  std_logic_vector(17 downto 0);
   -- timing of expZm1adderX: (c1, 3.376000ns)
signal expZm1adderY :  std_logic_vector(17 downto 0);
   -- timing of expZm1adderY: (c1, 4.624000ns)
signal expZm1 :  std_logic_vector(17 downto 0);
   -- timing of expZm1: (c1, 6.220000ns)
signal expA_T :  std_logic_vector(17 downto 0);
   -- timing of expA_T: (c1, 5.992000ns)
signal expArounded0 :  std_logic_vector(17 downto 0);
   -- timing of expArounded0: (c1, 7.588000ns)
signal expArounded :  std_logic_vector(16 downto 0);
   -- timing of expArounded: (c1, 7.588000ns)
signal lowerProduct :  std_logic_vector(18 downto 0);
   -- timing of lowerProduct: (c2, 2.822000ns)
signal extendedLowerProduct :  std_logic_vector(27 downto 0);
   -- timing of extendedLowerProduct: (c2, 2.822000ns)
signal XSign_d1 :  std_logic;
   -- timing of XSign: (c0, 0.000000ns)
constant g: positive := 4;
constant wE: positive := 8;
constant wF: positive := 23;
constant wFIn: positive := 23;
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            absK_d1 <=  absK;
            XSign_d1 <=  XSign;
         end if;
      end process;
ufixX <= unsigned(ufixX_i);
   xMulIn <= ufixX(33 downto 24); -- fix resize from (6, -27) to (6, -3)
   MulInvLog2: FixRealKCM_Freq100_uid8
      port map ( clk  => clk,
                 X => std_logic_vector(xMulIn),
                 R => absK);
   minusAbsK <= (8 downto 0 => '0') - ('0' & absK_d1);
   K <= minusAbsK when  XSign_d1='1'   else ('0' & absK_d1);
   MulLog2: FixRealKCM_Freq100_uid20
      port map ( clk  => clk,
                 X => absK,
                 R => absKLog2);
   subOp1 <= std_logic_vector(ufixX(26 downto 0)) when XSign='0' else not (std_logic_vector(ufixX(26 downto 0)));
   subOp2 <= absKLog2(26 downto 0) when XSign_d1='1' else not (absKLog2(26 downto 0));
   theYAdder: IntAdder_27_Freq100_uid33
      port map ( clk  => clk,
                 Cin => '1',
                 X => subOp1,
                 Y => subOp2,
                 R => Y);
   -- Now compute the exp of this fixed-point value
   A <= Y(26 downto 17);
   Z <= Y(16 downto 0);
   ExpATable: FixFunctionByTable_Freq100_uid35
      port map ( clk  => clk,
                 X => A,
                 Y => expA);
   Ztrunc <= Z(16 downto 10);
   ExpZmZm1Table: FixFunctionByTable_Freq100_uid44
      port map ( X => Ztrunc,
                 Y => expZmZm1_copy45);
   expZmZm1 <= expZmZm1_copy45; -- output copy to hold a pipeline register if needed
   -- Computing Z + (exp(Z)-1-Z)
   expZm1adderX <= '0' & Z;
   expZm1adderY <= (11 downto 0 => '0') & expZmZm1 ;
   Adder_expZm1: IntAdder_18_Freq100_uid48
      port map ( clk  => clk,
                 Cin => '0',
                 X => expZm1adderX,
                 Y => expZm1adderY,
                 R => expZm1);
   -- Rounding expA to the same accuracy as expZm1
   --   (truncation would not be accurate enough and require one more guard bit)
   expA_T <= expA(27 downto 10);
   Adder_expArounded0: IntAdder_18_Freq100_uid52
      port map ( clk  => clk,
                 Cin => '1',
                 X => expA_T,
                 Y => "000000000000000000",
                 R => expArounded0);
   expArounded <= expArounded0(17 downto 1);
   TheLowerProduct: IntMultiplier_17x18_19_Freq100_uid54
      port map ( clk  => clk,
                 X => expArounded,
                 Y => expZm1,
                 R => lowerProduct);
   extendedLowerProduct <= ((27 downto 19 => '0') & lowerProduct(18 downto 0));
   -- Final addition -- the product MSB bit weight is -k+2 = -8
   TheFinalAdder: IntAdder_28_Freq100_uid357
      port map ( clk  => clk,
                 Cin => '0',
                 X => expA,
                 Y => extendedLowerProduct,
                 R => expY);
end architecture;

--------------------------------------------------------------------------------
--                  RightShifter28_by_max_28_Freq100_uid359
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca (2008-2011), Florent de Dinechin (2008-2019)
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X S
-- Output signals: R
--  approx. input signal timings: X: (c2, 4.760000ns)S: (c2, 4.760000ns)
--  approx. output signal timings: R: (c2, 6.748000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity RightShifter28_by_max_28_Freq100_uid359 is
    port (clk : in std_logic;
          X : in  std_logic_vector(27 downto 0);
          S : in  std_logic_vector(4 downto 0);
          R : out  std_logic_vector(27 downto 0)   );
end entity;

architecture arch of RightShifter28_by_max_28_Freq100_uid359 is
signal ps :  std_logic_vector(4 downto 0);
   -- timing of ps: (c2, 4.760000ns)
signal level0 :  std_logic_vector(27 downto 0);
   -- timing of level0: (c2, 4.760000ns)
signal level1 :  std_logic_vector(28 downto 0);
   -- timing of level1: (c2, 4.760000ns)
signal level2 :  std_logic_vector(30 downto 0);
   -- timing of level2: (c2, 5.694000ns)
signal level3 :  std_logic_vector(34 downto 0);
   -- timing of level3: (c2, 5.694000ns)
signal level4 :  std_logic_vector(42 downto 0);
   -- timing of level4: (c2, 6.748000ns)
signal level5 :  std_logic_vector(58 downto 0);
   -- timing of level5: (c2, 6.748000ns)
begin
   ps<= S;
   level0<= X;
   level1 <=  (0 downto 0 => '0') & level0 when ps(0) = '1' else    level0 & (0 downto 0 => '0');
   level2 <=  (1 downto 0 => '0') & level1 when ps(1) = '1' else    level1 & (1 downto 0 => '0');
   level3 <=  (3 downto 0 => '0') & level2 when ps(2) = '1' else    level2 & (3 downto 0 => '0');
   level4 <=  (7 downto 0 => '0') & level3 when ps(3) = '1' else    level3 & (7 downto 0 => '0');
   level5 <=  (15 downto 0 => '0') & level4 when ps(4) = '1' else    level4 & (15 downto 0 => '0');
   R <= level5(58 downto 31);
end architecture;

--------------------------------------------------------------------------------
--                         IntAdder_33_Freq100_uid362
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 2 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X Y Cin
-- Output signals: R
--  approx. input signal timings: X: (c2, 7.372000ns)Y: (c2, 6.748000ns)Cin: (c0, 0.000000ns)
--  approx. output signal timings: R: (c2, 9.424000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_33_Freq100_uid362 is
    port (clk : in std_logic;
          X : in  std_logic_vector(32 downto 0);
          Y : in  std_logic_vector(32 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(32 downto 0)   );
end entity;

architecture arch of IntAdder_33_Freq100_uid362 is
signal Rtmp :  std_logic_vector(32 downto 0);
   -- timing of Rtmp: (c2, 9.424000ns)
signal Cin_d1, Cin_d2 :  std_logic;
   -- timing of Cin: (c0, 0.000000ns)
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            Cin_d1 <=  Cin;
            Cin_d2 <=  Cin_d1;
         end if;
      end process;
   Rtmp <= X + Y + Cin_d2;
   R <= Rtmp;
end architecture;

--------------------------------------------------------------------------------
--                                  expFloat
--                       (IEEEFPExp_8_23_Freq100_uid2)
-- VHDL generated for Zynq7000 @ 100MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: F. de Dinechin, Bogdan Pasca (2008-2021)
--------------------------------------------------------------------------------
-- Pipeline depth: 3 cycles
-- Clock period (ns): 10
-- Target frequency (MHz): 100
-- Input signals: X
-- Output signals: R
--  approx. input signal timings: X: (c0, 0.000000ns)
--  approx. output signal timings: R: (c3, 0.566000ns)

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity expFloat is
    port (clk : in std_logic;
          X : in  std_logic_vector(31 downto 0);
          R : out  std_logic_vector(31 downto 0)   );
end entity;

architecture arch of expFloat is
   component LeftShifter24_by_max_33_Freq100_uid4 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(23 downto 0);
             S : in  std_logic_vector(5 downto 0);
             R : out  std_logic_vector(56 downto 0)   );
   end component;

   component Exp_8_23_Freq100_uid6 is
      port ( clk : in std_logic;
             ufixX_i : in  std_logic_vector(33 downto 0);
             XSign : in  std_logic;
             expY : out  std_logic_vector(27 downto 0);
             K : out  std_logic_vector(8 downto 0)   );
   end component;

   component RightShifter28_by_max_28_Freq100_uid359 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(27 downto 0);
             S : in  std_logic_vector(4 downto 0);
             R : out  std_logic_vector(27 downto 0)   );
   end component;

   component IntAdder_33_Freq100_uid362 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(32 downto 0);
             Y : in  std_logic_vector(32 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(32 downto 0)   );
   end component;

signal X_s :  std_logic;
   -- timing of X_s: (c0, 0.000000ns)
signal X_e_tmp :  std_logic_vector(7 downto 0);
   -- timing of X_e_tmp: (c0, 0.000000ns)
signal X_exp_is_not_zero :  std_logic;
   -- timing of X_exp_is_not_zero: (c0, 1.368000ns)
signal X_f :  std_logic_vector(23 downto 0);
   -- timing of X_f: (c0, 1.368000ns)
signal X_empty_m :  std_logic;
   -- timing of X_empty_m: (c0, 1.710000ns)
signal X_nan_or_inf, X_nan_or_inf_d1, X_nan_or_inf_d2, X_nan_or_inf_d3 :  std_logic;
   -- timing of X_nan_or_inf: (c0, 1.368000ns)
signal X_nan, X_nan_d1, X_nan_d2, X_nan_d3 :  std_logic;
   -- timing of X_nan: (c0, 1.710000ns)
signal X_sig_nan :  std_logic;
   -- timing of X_sig_nan: (c0, 1.710000ns)
signal X_inf :  std_logic;
   -- timing of X_inf: (c0, 1.710000ns)
signal X_zero :  std_logic;
   -- timing of X_zero: (c0, 1.710000ns)
signal X_normal, X_normal_d1, X_normal_d2, X_normal_d3 :  std_logic;
   -- timing of X_normal: (c0, 1.710000ns)
signal X_e :  std_logic_vector(7 downto 0);
   -- timing of X_e: (c0, 1.368000ns)
signal XSign, XSign_d1, XSign_d2, XSign_d3 :  std_logic;
   -- timing of XSign: (c0, 0.000000ns)
signal XexpField :  std_logic_vector(7 downto 0);
   -- timing of XexpField: (c0, 1.368000ns)
signal e0 :  std_logic_vector(9 downto 0);
   -- timing of e0: (c0, 0.000000ns)
signal shiftVal :  std_logic_vector(9 downto 0);
   -- timing of shiftVal: (c0, 2.736000ns)
signal resultWillBeOne :  std_logic;
   -- timing of resultWillBeOne: (c0, 2.736000ns)
signal mXu :  unsigned(0+23 downto 0);
   -- timing of mXu: (c0, 1.368000ns)
signal maxShift :  std_logic_vector(8 downto 0);
   -- timing of maxShift: (c0, 0.000000ns)
signal overflow0 :  std_logic;
   -- timing of overflow0: (c0, 4.104000ns)
signal shiftValIn :  std_logic_vector(5 downto 0);
   -- timing of shiftValIn: (c0, 2.736000ns)
signal fixX0 :  std_logic_vector(56 downto 0);
   -- timing of fixX0: (c0, 6.138000ns)
signal ufixX :  unsigned(6+27 downto 0);
   -- timing of ufixX: (c0, 6.138000ns)
signal expY :  std_logic_vector(27 downto 0);
   -- timing of expY: (c2, 4.760000ns)
signal K :  std_logic_vector(8 downto 0);
   -- timing of K: (c1, 0.130000ns)
signal needNoNorm :  std_logic;
   -- timing of needNoNorm: (c2, 4.760000ns)
signal expY_norm :  std_logic_vector(27 downto 0);
   -- timing of expY_norm: (c2, 4.760000ns)
signal K_extended, K_extended_d1 :  std_logic_vector(9 downto 0);
   -- timing of K_extended: (c1, 0.130000ns)
signal preshiftSubnormalAmountMin1, preshiftSubnormalAmountMin1_d1 :  std_logic_vector(9 downto 0);
   -- timing of preshiftSubnormalAmountMin1: (c1, 0.130000ns)
signal preshiftSubnormalAmountZero, preshiftSubnormalAmountZero_d1 :  std_logic_vector(9 downto 0);
   -- timing of preshiftSubnormalAmountZero: (c1, 0.130000ns)
signal preshiftSubnormalAmount :  std_logic_vector(9 downto 0);
   -- timing of preshiftSubnormalAmount: (c2, 4.760000ns)
signal notSubnormal :  std_logic;
   -- timing of notSubnormal: (c2, 4.760000ns)
signal flushed_to_zero :  std_logic;
   -- timing of flushed_to_zero: (c2, 4.760000ns)
signal shiftSubnormalAmount :  std_logic_vector(4 downto 0);
   -- timing of shiftSubnormalAmount: (c2, 4.760000ns)
signal prepreRoundBiasSig :  std_logic_vector(27 downto 0);
   -- timing of prepreRoundBiasSig: (c2, 6.748000ns)
signal preRoundBiasSig :  std_logic_vector(32 downto 0);
   -- timing of preRoundBiasSig: (c2, 7.372000ns)
signal roundBit :  std_logic;
   -- timing of roundBit: (c2, 6.748000ns)
signal roundNormAddend :  std_logic_vector(32 downto 0);
   -- timing of roundNormAddend: (c2, 6.748000ns)
signal roundedExpSigRes, roundedExpSigRes_d1 :  std_logic_vector(32 downto 0);
   -- timing of roundedExpSigRes: (c2, 9.424000ns)
signal roundedExpSig :  std_logic_vector(32 downto 0);
   -- timing of roundedExpSig: (c3, 0.566000ns)
signal ofl1, ofl1_d1, ofl1_d2, ofl1_d3 :  std_logic;
   -- timing of ofl1: (c0, 4.728000ns)
signal exp_is_max :  std_logic;
   -- timing of exp_is_max: (c3, 0.566000ns)
signal ofl2 :  std_logic;
   -- timing of ofl2: (c3, 0.566000ns)
signal ofl3, ofl3_d1, ofl3_d2, ofl3_d3 :  std_logic;
   -- timing of ofl3: (c0, 1.710000ns)
signal ofl :  std_logic;
   -- timing of ofl: (c3, 0.566000ns)
signal ufl1 :  std_logic;
   -- timing of ufl1: (c2, 4.760000ns)
signal ufl2, ufl2_d1, ufl2_d2 :  std_logic;
   -- timing of ufl2: (c0, 1.710000ns)
signal ufl3, ufl3_d1, ufl3_d2 :  std_logic;
   -- timing of ufl3: (c0, 4.104000ns)
signal ufl, ufl_d1 :  std_logic;
   -- timing of ufl: (c2, 4.760000ns)
constant g: positive := 4;
constant wE: positive := 8;
constant wF: positive := 23;
constant wFIn: positive := 23;
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            X_nan_or_inf_d1 <=  X_nan_or_inf;
            X_nan_or_inf_d2 <=  X_nan_or_inf_d1;
            X_nan_or_inf_d3 <=  X_nan_or_inf_d2;
            X_nan_d1 <=  X_nan;
            X_nan_d2 <=  X_nan_d1;
            X_nan_d3 <=  X_nan_d2;
            X_normal_d1 <=  X_normal;
            X_normal_d2 <=  X_normal_d1;
            X_normal_d3 <=  X_normal_d2;
            XSign_d1 <=  XSign;
            XSign_d2 <=  XSign_d1;
            XSign_d3 <=  XSign_d2;
            K_extended_d1 <=  K_extended;
            preshiftSubnormalAmountMin1_d1 <=  preshiftSubnormalAmountMin1;
            preshiftSubnormalAmountZero_d1 <=  preshiftSubnormalAmountZero;
            roundedExpSigRes_d1 <=  roundedExpSigRes;
            ofl1_d1 <=  ofl1;
            ofl1_d2 <=  ofl1_d1;
            ofl1_d3 <=  ofl1_d2;
            ofl3_d1 <=  ofl3;
            ofl3_d2 <=  ofl3_d1;
            ofl3_d3 <=  ofl3_d2;
            ufl2_d1 <=  ufl2;
            ufl2_d2 <=  ufl2_d1;
            ufl3_d1 <=  ufl3;
            ufl3_d2 <=  ufl3_d1;
            ufl_d1 <=  ufl;
         end if;
      end process;
   X_s<= X(31);
   X_e_tmp<= X(30 downto 23);
   X_exp_is_not_zero<= '0' when X_e_tmp="00000000" else '1';
   X_f<= X_exp_is_not_zero & X(22 downto 0);
   X_empty_m<= '1' when X(22 downto 0) = "00000000000000000000000" else '0';
   X_nan_or_inf<= '1' when X_e_tmp = "11111111" else '0';
   X_nan <= X_nan_or_inf AND (NOT X_empty_m);
   X_sig_nan <= X_nan_or_inf AND (NOT X_empty_m) AND (NOT X(22));
   X_inf <= X_nan_or_inf AND X_empty_m;
   X_zero <= NOT(X_exp_is_not_zero) AND X_empty_m;
   X_normal <= X_exp_is_not_zero OR X_empty_m;
X_e <= X_e_tmp when X_exp_is_not_zero='1' else  "00000001";
   XSign <= X_s;
   XexpField <= X_e;
   e0 <= conv_std_logic_vector(100, wE+2);  -- bias - (wF+g)
   shiftVal <= ("00" & XexpField) - e0; -- for a left shift
   -- underflow when input is shifted to zero (shiftval<0), in which case exp = 1
   resultWillBeOne <= shiftVal(wE+1);
   --  mantissa with implicit bit
   mXu <= unsigned(X_f);
   -- Partial overflow detection
   maxShift <= conv_std_logic_vector(33, wE+1);  -- wE-2 + wF+g
   overflow0 <= not shiftVal(wE+1) when shiftVal(wE downto 0) > maxShift else '0';
   shiftValIn <= shiftVal(5 downto 0);
   mantissa_shift: LeftShifter24_by_max_33_Freq100_uid4
      port map ( clk  => clk,
                 S => shiftValIn,
                 X => std_logic_vector(mXu),
                 R => fixX0);
   ufixX <=  unsigned(fixX0(56 downto 23)) when resultWillBeOne='0' else "0000000000000000000000000000000000";
   exp_helper: Exp_8_23_Freq100_uid6
      port map ( clk  => clk,
                 XSign => XSign,
                 ufixX_i => std_logic_vector(ufixX),
                 K => K,
                 expY => expY);
   -- Rounding
   needNoNorm <= expY(27);
   expY_norm<= expY(27 downto 0) when needNoNorm='1' else expY(26 downto 0)& "0";
   K_extended<= K(8) & K;
   preshiftSubnormalAmountMin1<= - ( signed(K_extended) + signed(conv_std_logic_vector(127,10)) - 1 -1);
   preshiftSubnormalAmountZero<= - ( signed(K_extended) + signed(conv_std_logic_vector(127,10))-1);
   preshiftSubnormalAmount<= preshiftSubnormalAmountMin1_d1 when needNoNorm='0' else preshiftSubnormalAmountZero_d1;
   notSubnormal <= '1' when preshiftSubnormalAmount(9)='1' or preshiftSubnormalAmount="0000000000"  else '0';
   -- Flush to zero command only works when detected as subnormal (underflow) 
   flushed_to_zero <= '0' when preshiftSubnormalAmount(9 downto 5) = "00000" else '1';
   shiftSubnormalAmount<= conv_std_logic_vector(0, 5) when notSubnormal='1' else
      conv_std_logic_vector(28, 5) when flushed_to_zero='1' else 
      preshiftSubnormalAmount(4 downto 0);
   subnormal_shift: RightShifter28_by_max_28_Freq100_uid359
      port map ( clk  => clk,
                 S => shiftSubnormalAmount,
                 X => expY_norm,
                 R => prepreRoundBiasSig);
   preRoundBiasSig <= (9 downto 0 => '0') & prepreRoundBiasSig(26 downto 4) when notSubnormal = '0'
       else conv_std_logic_vector(127, wE+2)  & expY(26 downto 4) when needNoNorm = '1'
      else conv_std_logic_vector(126, wE+2)  & expY(25 downto 3) ;
   roundBit <= prepreRoundBiasSig(3)  when notSubnormal = '0' else expY(3)  when needNoNorm = '1'    else expY(2) ;
   roundNormAddend <= K_extended_d1 & (22 downto 1 => '0') & roundBit when notSubnormal='1' else (32 downto 1 => '0') & roundBit;
   roundedExpSigOperandAdder: IntAdder_33_Freq100_uid362
      port map ( clk  => clk,
                 Cin => '0',
                 X => preRoundBiasSig,
                 Y => roundNormAddend,
                 R => roundedExpSigRes);
   roundedExpSig <= roundedExpSigRes_d1 when X_nan_or_inf_d3='0' else "111111111111111111111111111111111"; -- number in the normal case
   ofl1 <= not XSign and overflow0 and X_normal; -- input positive, normal,  very large
   exp_is_max<= '1' when (roundedExpSig(30 downto 23)= "11111111") else '0';
   ofl2 <= not XSign_d3 and ((roundedExpSig(wE+wF) and not roundedExpSig(wE+wF+1)) OR exp_is_max) and X_normal_d3; -- input positive, normal, overflowed
   ofl3 <= not XSign and X_inf;  -- input was +infty
   ofl <= ofl1_d3 or ofl2 or ofl3_d3;
   ufl1 <= flushed_to_zero and not notSubnormal and X_normal_d2; -- input normal
   ufl2 <= XSign and X_inf;  -- input was -infty
   ufl3 <= XSign and overflow0  and X_normal; -- input negative, normal,  very large
   ufl <= ufl1 or ufl2_d2 or ufl3_d2;
   R <= "01111111110000000000000000000000" when X_nan_d3='1' else --nan
       "01111111100000000000000000000000" when ofl='1' else -- +infty
       "00000000000000000000000000000000" when ufl_d1='1' else -- 0
       '0' & roundedExpSig(30 downto 0);
end architecture;


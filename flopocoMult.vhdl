--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid15
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid15 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid15 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid20
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid20 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid20 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid27
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid27 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid27 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid32
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid32 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid32 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid39
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid39 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid39 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid44
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid44 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid44 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid51
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid51 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid51 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid56
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid56 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid56 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid63
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid63 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid63 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid68
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid68 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid68 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid75
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid75 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid75 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid80
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid80 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid80 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid87
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid87 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid87 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid92
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid92 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid92 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid99
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid99 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid99 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid104
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid104 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid104 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid111
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid111 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid111 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid116
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid116 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid116 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid123
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid123 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid123 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid128
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid128 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid128 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid135
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid135 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid135 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid140
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid140 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid140 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid147
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid147 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid147 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                          MultTable_Freq300_uid152
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007-2022)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X
-- Output signals: Y

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity MultTable_Freq300_uid152 is
    port (X : in  std_logic_vector(4 downto 0);
          Y : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of MultTable_Freq300_uid152 is
signal Y0 :  std_logic_vector(4 downto 0);
attribute ram_extract: string;
attribute ram_style: string;
attribute ram_extract of Y0: signal is "yes";
attribute ram_style of Y0: signal is "distributed";
signal Y1 :  std_logic_vector(4 downto 0);
begin
   with X  select  Y0 <= 
      "00000" when "00000",
      "00000" when "00001",
      "00000" when "00010",
      "00000" when "00011",
      "00000" when "00100",
      "00000" when "00101",
      "00000" when "00110",
      "00000" when "00111",
      "00000" when "01000",
      "00001" when "01001",
      "00010" when "01010",
      "00011" when "01011",
      "00100" when "01100",
      "00101" when "01101",
      "00110" when "01110",
      "00111" when "01111",
      "00000" when "10000",
      "00010" when "10001",
      "00100" when "10010",
      "00110" when "10011",
      "01000" when "10100",
      "01010" when "10101",
      "01100" when "10110",
      "01110" when "10111",
      "00000" when "11000",
      "00011" when "11001",
      "00110" when "11010",
      "01001" when "11011",
      "01100" when "11100",
      "01111" when "11101",
      "10010" when "11110",
      "10101" when "11111",
      "-----" when others;
   Y1 <= Y0; -- for the possible blockram register
   Y <= Y1;
end architecture;

--------------------------------------------------------------------------------
--                       Compressor_23_3_Freq300_uid156
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X1 X0
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_23_3_Freq300_uid156 is
    port (X1 : in  std_logic_vector(1 downto 0);
          X0 : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(2 downto 0)   );
end entity;

architecture arch of Compressor_23_3_Freq300_uid156 is
signal X :  std_logic_vector(4 downto 0);
signal R0 :  std_logic_vector(2 downto 0);
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
--                       Compressor_3_2_Freq300_uid160
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X0
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_3_2_Freq300_uid160 is
    port (X0 : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of Compressor_3_2_Freq300_uid160 is
signal X :  std_logic_vector(2 downto 0);
signal R0 :  std_logic_vector(1 downto 0);
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
--                       Compressor_14_3_Freq300_uid164
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X1 X0
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_14_3_Freq300_uid164 is
    port (X1 : in  std_logic_vector(0 downto 0);
          X0 : in  std_logic_vector(3 downto 0);
          R : out  std_logic_vector(2 downto 0)   );
end entity;

architecture arch of Compressor_14_3_Freq300_uid164 is
signal X :  std_logic_vector(4 downto 0);
signal R0 :  std_logic_vector(2 downto 0);
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
--                       Compressor_6_3_Freq300_uid172
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X0
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_6_3_Freq300_uid172 is
    port (X0 : in  std_logic_vector(5 downto 0);
          R : out  std_logic_vector(2 downto 0)   );
end entity;

architecture arch of Compressor_6_3_Freq300_uid172 is
signal X :  std_logic_vector(5 downto 0);
signal R0 :  std_logic_vector(2 downto 0);
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
--                        DSPBlock_17x24_Freq300_uid9
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity DSPBlock_17x24_Freq300_uid9 is
    port (clk : in std_logic;
          X : in  std_logic_vector(16 downto 0);
          Y : in  std_logic_vector(23 downto 0);
          R : out  std_logic_vector(40 downto 0)   );
end entity;

architecture arch of DSPBlock_17x24_Freq300_uid9 is
signal Mfull :  std_logic_vector(40 downto 0);
signal M :  std_logic_vector(40 downto 0);
begin
   Mfull <= std_logic_vector(unsigned(X) * unsigned(Y)); -- multiplier
   M <= Mfull(40 downto 0);
   R <= M;
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_1x2_Freq300_uid11
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq300_uid11 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq300_uid11 is
signal replicated :  std_logic_vector(1 downto 0);
signal prod :  std_logic_vector(1 downto 0);
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid13
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid13 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid13 is
   component MultTable_Freq300_uid15 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy16 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid15
      port map ( X => Xtable,
                 Y => Y1_copy16);
   Y1 <= Y1_copy16; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid18
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid18 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid18 is
   component MultTable_Freq300_uid20 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy21 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid20
      port map ( X => Xtable,
                 Y => Y1_copy21);
   Y1 <= Y1_copy21; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_1x2_Freq300_uid23
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq300_uid23 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq300_uid23 is
signal replicated :  std_logic_vector(1 downto 0);
signal prod :  std_logic_vector(1 downto 0);
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid25
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid25 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid25 is
   component MultTable_Freq300_uid27 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy28 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid27
      port map ( X => Xtable,
                 Y => Y1_copy28);
   Y1 <= Y1_copy28; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid30
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid30 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid30 is
   component MultTable_Freq300_uid32 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy33 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid32
      port map ( X => Xtable,
                 Y => Y1_copy33);
   Y1 <= Y1_copy33; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_1x2_Freq300_uid35
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq300_uid35 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq300_uid35 is
signal replicated :  std_logic_vector(1 downto 0);
signal prod :  std_logic_vector(1 downto 0);
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid37
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid37 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid37 is
   component MultTable_Freq300_uid39 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy40 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid39
      port map ( X => Xtable,
                 Y => Y1_copy40);
   Y1 <= Y1_copy40; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid42
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid42 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid42 is
   component MultTable_Freq300_uid44 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy45 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid44
      port map ( X => Xtable,
                 Y => Y1_copy45);
   Y1 <= Y1_copy45; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_1x2_Freq300_uid47
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq300_uid47 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq300_uid47 is
signal replicated :  std_logic_vector(1 downto 0);
signal prod :  std_logic_vector(1 downto 0);
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid49
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid49 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid49 is
   component MultTable_Freq300_uid51 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy52 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid51
      port map ( X => Xtable,
                 Y => Y1_copy52);
   Y1 <= Y1_copy52; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid54
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid54 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid54 is
   component MultTable_Freq300_uid56 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy57 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid56
      port map ( X => Xtable,
                 Y => Y1_copy57);
   Y1 <= Y1_copy57; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_1x2_Freq300_uid59
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq300_uid59 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq300_uid59 is
signal replicated :  std_logic_vector(1 downto 0);
signal prod :  std_logic_vector(1 downto 0);
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid61
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid61 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid61 is
   component MultTable_Freq300_uid63 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy64 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid63
      port map ( X => Xtable,
                 Y => Y1_copy64);
   Y1 <= Y1_copy64; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid66
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid66 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid66 is
   component MultTable_Freq300_uid68 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy69 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid68
      port map ( X => Xtable,
                 Y => Y1_copy69);
   Y1 <= Y1_copy69; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_1x2_Freq300_uid71
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq300_uid71 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq300_uid71 is
signal replicated :  std_logic_vector(1 downto 0);
signal prod :  std_logic_vector(1 downto 0);
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid73
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid73 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid73 is
   component MultTable_Freq300_uid75 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy76 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid75
      port map ( X => Xtable,
                 Y => Y1_copy76);
   Y1 <= Y1_copy76; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid78
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid78 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid78 is
   component MultTable_Freq300_uid80 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy81 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid80
      port map ( X => Xtable,
                 Y => Y1_copy81);
   Y1 <= Y1_copy81; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_1x2_Freq300_uid83
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq300_uid83 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq300_uid83 is
signal replicated :  std_logic_vector(1 downto 0);
signal prod :  std_logic_vector(1 downto 0);
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid85
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid85 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid85 is
   component MultTable_Freq300_uid87 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy88 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid87
      port map ( X => Xtable,
                 Y => Y1_copy88);
   Y1 <= Y1_copy88; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid90
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid90 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid90 is
   component MultTable_Freq300_uid92 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy93 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid92
      port map ( X => Xtable,
                 Y => Y1_copy93);
   Y1 <= Y1_copy93; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_1x2_Freq300_uid95
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq300_uid95 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq300_uid95 is
signal replicated :  std_logic_vector(1 downto 0);
signal prod :  std_logic_vector(1 downto 0);
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                     IntMultiplierLUT_3x2_Freq300_uid97
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid97 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid97 is
   component MultTable_Freq300_uid99 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy100 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid99
      port map ( X => Xtable,
                 Y => Y1_copy100);
   Y1 <= Y1_copy100; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x2_Freq300_uid102
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid102 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid102 is
   component MultTable_Freq300_uid104 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy105 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid104
      port map ( X => Xtable,
                 Y => Y1_copy105);
   Y1 <= Y1_copy105; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_1x2_Freq300_uid107
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq300_uid107 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq300_uid107 is
signal replicated :  std_logic_vector(1 downto 0);
signal prod :  std_logic_vector(1 downto 0);
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x2_Freq300_uid109
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid109 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid109 is
   component MultTable_Freq300_uid111 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy112 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid111
      port map ( X => Xtable,
                 Y => Y1_copy112);
   Y1 <= Y1_copy112; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x2_Freq300_uid114
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid114 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid114 is
   component MultTable_Freq300_uid116 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy117 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid116
      port map ( X => Xtable,
                 Y => Y1_copy117);
   Y1 <= Y1_copy117; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_1x2_Freq300_uid119
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq300_uid119 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq300_uid119 is
signal replicated :  std_logic_vector(1 downto 0);
signal prod :  std_logic_vector(1 downto 0);
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x2_Freq300_uid121
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid121 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid121 is
   component MultTable_Freq300_uid123 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy124 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid123
      port map ( X => Xtable,
                 Y => Y1_copy124);
   Y1 <= Y1_copy124; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x2_Freq300_uid126
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid126 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid126 is
   component MultTable_Freq300_uid128 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy129 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid128
      port map ( X => Xtable,
                 Y => Y1_copy129);
   Y1 <= Y1_copy129; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_1x2_Freq300_uid131
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq300_uid131 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq300_uid131 is
signal replicated :  std_logic_vector(1 downto 0);
signal prod :  std_logic_vector(1 downto 0);
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x2_Freq300_uid133
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid133 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid133 is
   component MultTable_Freq300_uid135 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy136 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid135
      port map ( X => Xtable,
                 Y => Y1_copy136);
   Y1 <= Y1_copy136; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x2_Freq300_uid138
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid138 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid138 is
   component MultTable_Freq300_uid140 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy141 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid140
      port map ( X => Xtable,
                 Y => Y1_copy141);
   Y1 <= Y1_copy141; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_1x2_Freq300_uid143
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_1x2_Freq300_uid143 is
    port (clk : in std_logic;
          X : in  std_logic_vector(0 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_1x2_Freq300_uid143 is
signal replicated :  std_logic_vector(1 downto 0);
signal prod :  std_logic_vector(1 downto 0);
begin
   replicated <= (1 downto 0 => X(0));
   prod <= Y and replicated;
   R <= prod;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x2_Freq300_uid145
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid145 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid145 is
   component MultTable_Freq300_uid147 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy148 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid147
      port map ( X => Xtable,
                 Y => Y1_copy148);
   Y1 <= Y1_copy148; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplierLUT_3x2_Freq300_uid150
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntMultiplierLUT_3x2_Freq300_uid150 is
    port (clk : in std_logic;
          X : in  std_logic_vector(2 downto 0);
          Y : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of IntMultiplierLUT_3x2_Freq300_uid150 is
   component MultTable_Freq300_uid152 is
      port ( X : in  std_logic_vector(4 downto 0);
             Y : out  std_logic_vector(4 downto 0)   );
   end component;

signal Xtable :  std_logic_vector(4 downto 0);
signal Y1 :  std_logic_vector(4 downto 0);
signal Y1_copy153 :  std_logic_vector(4 downto 0);
begin
Xtable <= Y & X;
   R <= Y1;
   TableMult: MultTable_Freq300_uid152
      port map ( X => Xtable,
                 Y => Y1_copy153);
   Y1 <= Y1_copy153; -- output copy to hold a pipeline register if needed
end architecture;

--------------------------------------------------------------------------------
--                         IntAdder_26_Freq300_uid292
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 1 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y Cin
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_26_Freq300_uid292 is
    port (clk : in std_logic;
          X : in  std_logic_vector(25 downto 0);
          Y : in  std_logic_vector(25 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(25 downto 0)   );
end entity;

architecture arch of IntAdder_26_Freq300_uid292 is
signal Cin_1, Cin_1_d1 :  std_logic;
signal X_1, X_1_d1 :  std_logic_vector(26 downto 0);
signal Y_1, Y_1_d1 :  std_logic_vector(26 downto 0);
signal S_1 :  std_logic_vector(26 downto 0);
signal R_1 :  std_logic_vector(25 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            Cin_1_d1 <=  Cin_1;
            X_1_d1 <=  X_1;
            Y_1_d1 <=  Y_1;
         end if;
      end process;
   Cin_1 <= Cin;
   X_1 <= '0' & X(25 downto 0);
   Y_1 <= '0' & Y(25 downto 0);
   S_1 <= X_1_d1 + Y_1_d1 + Cin_1_d1;
   R_1 <= S_1(25 downto 0);
   R <= R_1 ;
end architecture;

--------------------------------------------------------------------------------
--                    IntMultiplier_24x24_48_Freq300_uid5
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Martin Kumm, Florent de Dinechin, Kinga Illyes, Bogdan Popa, Bogdan Pasca, 2012-
--------------------------------------------------------------------------------
-- Pipeline depth: 1 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity IntMultiplier_24x24_48_Freq300_uid5 is
    port (clk : in std_logic;
          X : in  std_logic_vector(23 downto 0);
          Y : in  std_logic_vector(23 downto 0);
          R : out  std_logic_vector(47 downto 0)   );
end entity;

architecture arch of IntMultiplier_24x24_48_Freq300_uid5 is
   component DSPBlock_17x24_Freq300_uid9 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(16 downto 0);
             Y : in  std_logic_vector(23 downto 0);
             R : out  std_logic_vector(40 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq300_uid11 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid13 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid18 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq300_uid23 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid25 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid30 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq300_uid35 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid37 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid42 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq300_uid47 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid49 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid54 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq300_uid59 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid61 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid66 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq300_uid71 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid73 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid78 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq300_uid83 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid85 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid90 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq300_uid95 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid97 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid102 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq300_uid107 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid109 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid114 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq300_uid119 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid121 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid126 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq300_uid131 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid133 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid138 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_1x2_Freq300_uid143 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(0 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid145 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component IntMultiplierLUT_3x2_Freq300_uid150 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(2 downto 0);
             Y : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(4 downto 0)   );
   end component;

   component Compressor_23_3_Freq300_uid156 is
      port ( X1 : in  std_logic_vector(1 downto 0);
             X0 : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_3_2_Freq300_uid160 is
      port ( X0 : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

   component Compressor_14_3_Freq300_uid164 is
      port ( X1 : in  std_logic_vector(0 downto 0);
             X0 : in  std_logic_vector(3 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_6_3_Freq300_uid172 is
      port ( X0 : in  std_logic_vector(5 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component IntAdder_26_Freq300_uid292 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(25 downto 0);
             Y : in  std_logic_vector(25 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(25 downto 0)   );
   end component;

signal XX_m6 :  std_logic_vector(23 downto 0);
signal YY_m6 :  std_logic_vector(23 downto 0);
signal tile_0_X :  std_logic_vector(16 downto 0);
signal tile_0_Y :  std_logic_vector(23 downto 0);
signal tile_0_output :  std_logic_vector(40 downto 0);
signal tile_0_filtered_output :  unsigned(40-0 downto 0);
signal bh7_w0_0 :  std_logic;
signal bh7_w1_0 :  std_logic;
signal bh7_w2_0 :  std_logic;
signal bh7_w3_0 :  std_logic;
signal bh7_w4_0 :  std_logic;
signal bh7_w5_0 :  std_logic;
signal bh7_w6_0 :  std_logic;
signal bh7_w7_0 :  std_logic;
signal bh7_w8_0 :  std_logic;
signal bh7_w9_0 :  std_logic;
signal bh7_w10_0 :  std_logic;
signal bh7_w11_0 :  std_logic;
signal bh7_w12_0 :  std_logic;
signal bh7_w13_0 :  std_logic;
signal bh7_w14_0 :  std_logic;
signal bh7_w15_0 :  std_logic;
signal bh7_w16_0 :  std_logic;
signal bh7_w17_0 :  std_logic;
signal bh7_w18_0 :  std_logic;
signal bh7_w19_0 :  std_logic;
signal bh7_w20_0 :  std_logic;
signal bh7_w21_0 :  std_logic;
signal bh7_w22_0 :  std_logic;
signal bh7_w23_0 :  std_logic;
signal bh7_w24_0 :  std_logic;
signal bh7_w25_0 :  std_logic;
signal bh7_w26_0 :  std_logic;
signal bh7_w27_0 :  std_logic;
signal bh7_w28_0 :  std_logic;
signal bh7_w29_0 :  std_logic;
signal bh7_w30_0 :  std_logic;
signal bh7_w31_0 :  std_logic;
signal bh7_w32_0 :  std_logic;
signal bh7_w33_0 :  std_logic;
signal bh7_w34_0 :  std_logic;
signal bh7_w35_0 :  std_logic;
signal bh7_w36_0 :  std_logic;
signal bh7_w37_0 :  std_logic;
signal bh7_w38_0 :  std_logic;
signal bh7_w39_0 :  std_logic;
signal bh7_w40_0 :  std_logic;
signal tile_1_X :  std_logic_vector(0 downto 0);
signal tile_1_Y :  std_logic_vector(1 downto 0);
signal tile_1_output :  std_logic_vector(1 downto 0);
signal tile_1_filtered_output :  unsigned(1-0 downto 0);
signal bh7_w45_0 :  std_logic;
signal bh7_w46_0 :  std_logic;
signal tile_2_X :  std_logic_vector(2 downto 0);
signal tile_2_Y :  std_logic_vector(1 downto 0);
signal tile_2_output :  std_logic_vector(4 downto 0);
signal tile_2_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w42_0 :  std_logic;
signal bh7_w43_0 :  std_logic;
signal bh7_w44_0 :  std_logic;
signal bh7_w45_1 :  std_logic;
signal bh7_w46_1 :  std_logic;
signal tile_3_X :  std_logic_vector(2 downto 0);
signal tile_3_Y :  std_logic_vector(1 downto 0);
signal tile_3_output :  std_logic_vector(4 downto 0);
signal tile_3_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w39_1 :  std_logic;
signal bh7_w40_1 :  std_logic;
signal bh7_w41_0 :  std_logic;
signal bh7_w42_1 :  std_logic;
signal bh7_w43_1 :  std_logic;
signal tile_4_X :  std_logic_vector(0 downto 0);
signal tile_4_Y :  std_logic_vector(1 downto 0);
signal tile_4_output :  std_logic_vector(1 downto 0);
signal tile_4_filtered_output :  unsigned(1-0 downto 0);
signal bh7_w43_2 :  std_logic;
signal bh7_w44_1 :  std_logic;
signal tile_5_X :  std_logic_vector(2 downto 0);
signal tile_5_Y :  std_logic_vector(1 downto 0);
signal tile_5_output :  std_logic_vector(4 downto 0);
signal tile_5_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w40_2 :  std_logic;
signal bh7_w41_1 :  std_logic;
signal bh7_w42_2 :  std_logic;
signal bh7_w43_3 :  std_logic;
signal bh7_w44_2 :  std_logic;
signal tile_6_X :  std_logic_vector(2 downto 0);
signal tile_6_Y :  std_logic_vector(1 downto 0);
signal tile_6_output :  std_logic_vector(4 downto 0);
signal tile_6_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w37_1 :  std_logic;
signal bh7_w38_1 :  std_logic;
signal bh7_w39_2 :  std_logic;
signal bh7_w40_3 :  std_logic;
signal bh7_w41_2 :  std_logic;
signal tile_7_X :  std_logic_vector(0 downto 0);
signal tile_7_Y :  std_logic_vector(1 downto 0);
signal tile_7_output :  std_logic_vector(1 downto 0);
signal tile_7_filtered_output :  unsigned(1-0 downto 0);
signal bh7_w41_3 :  std_logic;
signal bh7_w42_3 :  std_logic;
signal tile_8_X :  std_logic_vector(2 downto 0);
signal tile_8_Y :  std_logic_vector(1 downto 0);
signal tile_8_output :  std_logic_vector(4 downto 0);
signal tile_8_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w38_2 :  std_logic;
signal bh7_w39_3 :  std_logic;
signal bh7_w40_4 :  std_logic;
signal bh7_w41_4 :  std_logic;
signal bh7_w42_4 :  std_logic;
signal tile_9_X :  std_logic_vector(2 downto 0);
signal tile_9_Y :  std_logic_vector(1 downto 0);
signal tile_9_output :  std_logic_vector(4 downto 0);
signal tile_9_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w35_1 :  std_logic;
signal bh7_w36_1 :  std_logic;
signal bh7_w37_2 :  std_logic;
signal bh7_w38_3 :  std_logic;
signal bh7_w39_4 :  std_logic;
signal tile_10_X :  std_logic_vector(0 downto 0);
signal tile_10_Y :  std_logic_vector(1 downto 0);
signal tile_10_output :  std_logic_vector(1 downto 0);
signal tile_10_filtered_output :  unsigned(1-0 downto 0);
signal bh7_w39_5 :  std_logic;
signal bh7_w40_5 :  std_logic;
signal tile_11_X :  std_logic_vector(2 downto 0);
signal tile_11_Y :  std_logic_vector(1 downto 0);
signal tile_11_output :  std_logic_vector(4 downto 0);
signal tile_11_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w36_2 :  std_logic;
signal bh7_w37_3 :  std_logic;
signal bh7_w38_4 :  std_logic;
signal bh7_w39_6 :  std_logic;
signal bh7_w40_6 :  std_logic;
signal tile_12_X :  std_logic_vector(2 downto 0);
signal tile_12_Y :  std_logic_vector(1 downto 0);
signal tile_12_output :  std_logic_vector(4 downto 0);
signal tile_12_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w33_1 :  std_logic;
signal bh7_w34_1 :  std_logic;
signal bh7_w35_2 :  std_logic;
signal bh7_w36_3 :  std_logic;
signal bh7_w37_4 :  std_logic;
signal tile_13_X :  std_logic_vector(0 downto 0);
signal tile_13_Y :  std_logic_vector(1 downto 0);
signal tile_13_output :  std_logic_vector(1 downto 0);
signal tile_13_filtered_output :  unsigned(1-0 downto 0);
signal bh7_w37_5 :  std_logic;
signal bh7_w38_5 :  std_logic;
signal tile_14_X :  std_logic_vector(2 downto 0);
signal tile_14_Y :  std_logic_vector(1 downto 0);
signal tile_14_output :  std_logic_vector(4 downto 0);
signal tile_14_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w34_2 :  std_logic;
signal bh7_w35_3 :  std_logic;
signal bh7_w36_4 :  std_logic;
signal bh7_w37_6 :  std_logic;
signal bh7_w38_6 :  std_logic;
signal tile_15_X :  std_logic_vector(2 downto 0);
signal tile_15_Y :  std_logic_vector(1 downto 0);
signal tile_15_output :  std_logic_vector(4 downto 0);
signal tile_15_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w31_1 :  std_logic;
signal bh7_w32_1 :  std_logic;
signal bh7_w33_2 :  std_logic;
signal bh7_w34_3 :  std_logic;
signal bh7_w35_4 :  std_logic;
signal tile_16_X :  std_logic_vector(0 downto 0);
signal tile_16_Y :  std_logic_vector(1 downto 0);
signal tile_16_output :  std_logic_vector(1 downto 0);
signal tile_16_filtered_output :  unsigned(1-0 downto 0);
signal bh7_w35_5 :  std_logic;
signal bh7_w36_5 :  std_logic;
signal tile_17_X :  std_logic_vector(2 downto 0);
signal tile_17_Y :  std_logic_vector(1 downto 0);
signal tile_17_output :  std_logic_vector(4 downto 0);
signal tile_17_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w32_2 :  std_logic;
signal bh7_w33_3 :  std_logic;
signal bh7_w34_4 :  std_logic;
signal bh7_w35_6 :  std_logic;
signal bh7_w36_6 :  std_logic;
signal tile_18_X :  std_logic_vector(2 downto 0);
signal tile_18_Y :  std_logic_vector(1 downto 0);
signal tile_18_output :  std_logic_vector(4 downto 0);
signal tile_18_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w29_1 :  std_logic;
signal bh7_w30_1 :  std_logic;
signal bh7_w31_2 :  std_logic;
signal bh7_w32_3 :  std_logic;
signal bh7_w33_4 :  std_logic;
signal tile_19_X :  std_logic_vector(0 downto 0);
signal tile_19_Y :  std_logic_vector(1 downto 0);
signal tile_19_output :  std_logic_vector(1 downto 0);
signal tile_19_filtered_output :  unsigned(1-0 downto 0);
signal bh7_w33_5 :  std_logic;
signal bh7_w34_5 :  std_logic;
signal tile_20_X :  std_logic_vector(2 downto 0);
signal tile_20_Y :  std_logic_vector(1 downto 0);
signal tile_20_output :  std_logic_vector(4 downto 0);
signal tile_20_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w30_2 :  std_logic;
signal bh7_w31_3 :  std_logic;
signal bh7_w32_4 :  std_logic;
signal bh7_w33_6 :  std_logic;
signal bh7_w34_6 :  std_logic;
signal tile_21_X :  std_logic_vector(2 downto 0);
signal tile_21_Y :  std_logic_vector(1 downto 0);
signal tile_21_output :  std_logic_vector(4 downto 0);
signal tile_21_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w27_1 :  std_logic;
signal bh7_w28_1 :  std_logic;
signal bh7_w29_2 :  std_logic;
signal bh7_w30_3 :  std_logic;
signal bh7_w31_4 :  std_logic;
signal tile_22_X :  std_logic_vector(0 downto 0);
signal tile_22_Y :  std_logic_vector(1 downto 0);
signal tile_22_output :  std_logic_vector(1 downto 0);
signal tile_22_filtered_output :  unsigned(1-0 downto 0);
signal bh7_w31_5 :  std_logic;
signal bh7_w32_5 :  std_logic;
signal tile_23_X :  std_logic_vector(2 downto 0);
signal tile_23_Y :  std_logic_vector(1 downto 0);
signal tile_23_output :  std_logic_vector(4 downto 0);
signal tile_23_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w28_2 :  std_logic;
signal bh7_w29_3 :  std_logic;
signal bh7_w30_4 :  std_logic;
signal bh7_w31_6 :  std_logic;
signal bh7_w32_6 :  std_logic;
signal tile_24_X :  std_logic_vector(2 downto 0);
signal tile_24_Y :  std_logic_vector(1 downto 0);
signal tile_24_output :  std_logic_vector(4 downto 0);
signal tile_24_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w25_1 :  std_logic;
signal bh7_w26_1 :  std_logic;
signal bh7_w27_2 :  std_logic;
signal bh7_w28_3 :  std_logic;
signal bh7_w29_4 :  std_logic;
signal tile_25_X :  std_logic_vector(0 downto 0);
signal tile_25_Y :  std_logic_vector(1 downto 0);
signal tile_25_output :  std_logic_vector(1 downto 0);
signal tile_25_filtered_output :  unsigned(1-0 downto 0);
signal bh7_w29_5 :  std_logic;
signal bh7_w30_5 :  std_logic;
signal tile_26_X :  std_logic_vector(2 downto 0);
signal tile_26_Y :  std_logic_vector(1 downto 0);
signal tile_26_output :  std_logic_vector(4 downto 0);
signal tile_26_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w26_2 :  std_logic;
signal bh7_w27_3 :  std_logic;
signal bh7_w28_4 :  std_logic;
signal bh7_w29_6 :  std_logic;
signal bh7_w30_6 :  std_logic;
signal tile_27_X :  std_logic_vector(2 downto 0);
signal tile_27_Y :  std_logic_vector(1 downto 0);
signal tile_27_output :  std_logic_vector(4 downto 0);
signal tile_27_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w23_1 :  std_logic;
signal bh7_w24_1 :  std_logic;
signal bh7_w25_2 :  std_logic;
signal bh7_w26_3 :  std_logic;
signal bh7_w27_4 :  std_logic;
signal tile_28_X :  std_logic_vector(0 downto 0);
signal tile_28_Y :  std_logic_vector(1 downto 0);
signal tile_28_output :  std_logic_vector(1 downto 0);
signal tile_28_filtered_output :  unsigned(1-0 downto 0);
signal bh7_w27_5 :  std_logic;
signal bh7_w28_5 :  std_logic;
signal tile_29_X :  std_logic_vector(2 downto 0);
signal tile_29_Y :  std_logic_vector(1 downto 0);
signal tile_29_output :  std_logic_vector(4 downto 0);
signal tile_29_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w24_2 :  std_logic;
signal bh7_w25_3 :  std_logic;
signal bh7_w26_4 :  std_logic;
signal bh7_w27_6 :  std_logic;
signal bh7_w28_6 :  std_logic;
signal tile_30_X :  std_logic_vector(2 downto 0);
signal tile_30_Y :  std_logic_vector(1 downto 0);
signal tile_30_output :  std_logic_vector(4 downto 0);
signal tile_30_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w21_1 :  std_logic;
signal bh7_w22_1 :  std_logic;
signal bh7_w23_2 :  std_logic;
signal bh7_w24_3 :  std_logic;
signal bh7_w25_4 :  std_logic;
signal tile_31_X :  std_logic_vector(0 downto 0);
signal tile_31_Y :  std_logic_vector(1 downto 0);
signal tile_31_output :  std_logic_vector(1 downto 0);
signal tile_31_filtered_output :  unsigned(1-0 downto 0);
signal bh7_w25_5 :  std_logic;
signal bh7_w26_5 :  std_logic;
signal tile_32_X :  std_logic_vector(2 downto 0);
signal tile_32_Y :  std_logic_vector(1 downto 0);
signal tile_32_output :  std_logic_vector(4 downto 0);
signal tile_32_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w22_2 :  std_logic;
signal bh7_w23_3 :  std_logic;
signal bh7_w24_4 :  std_logic;
signal bh7_w25_6 :  std_logic;
signal bh7_w26_6 :  std_logic;
signal tile_33_X :  std_logic_vector(2 downto 0);
signal tile_33_Y :  std_logic_vector(1 downto 0);
signal tile_33_output :  std_logic_vector(4 downto 0);
signal tile_33_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w19_1 :  std_logic;
signal bh7_w20_1 :  std_logic;
signal bh7_w21_2 :  std_logic;
signal bh7_w22_3 :  std_logic;
signal bh7_w23_4 :  std_logic;
signal tile_34_X :  std_logic_vector(0 downto 0);
signal tile_34_Y :  std_logic_vector(1 downto 0);
signal tile_34_output :  std_logic_vector(1 downto 0);
signal tile_34_filtered_output :  unsigned(1-0 downto 0);
signal bh7_w23_5 :  std_logic;
signal bh7_w24_5 :  std_logic;
signal tile_35_X :  std_logic_vector(2 downto 0);
signal tile_35_Y :  std_logic_vector(1 downto 0);
signal tile_35_output :  std_logic_vector(4 downto 0);
signal tile_35_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w20_2 :  std_logic;
signal bh7_w21_3 :  std_logic;
signal bh7_w22_4 :  std_logic;
signal bh7_w23_6 :  std_logic;
signal bh7_w24_6 :  std_logic;
signal tile_36_X :  std_logic_vector(2 downto 0);
signal tile_36_Y :  std_logic_vector(1 downto 0);
signal tile_36_output :  std_logic_vector(4 downto 0);
signal tile_36_filtered_output :  unsigned(4-0 downto 0);
signal bh7_w17_1 :  std_logic;
signal bh7_w18_1 :  std_logic;
signal bh7_w19_2 :  std_logic;
signal bh7_w20_3 :  std_logic;
signal bh7_w21_4 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid157_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid157_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid157_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w17_2 :  std_logic;
signal bh7_w18_2 :  std_logic;
signal bh7_w19_3 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid157_Out0_copy158 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid161_In0 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid161_Out0 :  std_logic_vector(1 downto 0);
signal bh7_w19_4 :  std_logic;
signal bh7_w20_4 :  std_logic;
signal Compressor_3_2_Freq300_uid160_bh7_uid161_Out0_copy162 :  std_logic_vector(1 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid165_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid165_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid165_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w20_5 :  std_logic;
signal bh7_w21_5 :  std_logic;
signal bh7_w22_5 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid165_Out0_copy166 :  std_logic_vector(2 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid167_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid167_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid167_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w21_6 :  std_logic;
signal bh7_w22_6 :  std_logic;
signal bh7_w23_7 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid167_Out0_copy168 :  std_logic_vector(2 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid169_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid169_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid169_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w22_7 :  std_logic;
signal bh7_w23_8 :  std_logic;
signal bh7_w24_7 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid169_Out0_copy170 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid173_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid173_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w23_9 :  std_logic;
signal bh7_w24_8 :  std_logic;
signal bh7_w25_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid173_Out0_copy174 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid175_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid175_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w24_9 :  std_logic;
signal bh7_w25_8 :  std_logic;
signal bh7_w26_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid175_Out0_copy176 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid177_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid177_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w25_9 :  std_logic;
signal bh7_w26_8 :  std_logic;
signal bh7_w27_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid177_Out0_copy178 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid179_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid179_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w26_9 :  std_logic;
signal bh7_w27_8 :  std_logic;
signal bh7_w28_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid179_Out0_copy180 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid181_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid181_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w27_9 :  std_logic;
signal bh7_w28_8 :  std_logic;
signal bh7_w29_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid181_Out0_copy182 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid183_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid183_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w28_9 :  std_logic;
signal bh7_w29_8 :  std_logic;
signal bh7_w30_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid183_Out0_copy184 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid185_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid185_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w29_9 :  std_logic;
signal bh7_w30_8 :  std_logic;
signal bh7_w31_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid185_Out0_copy186 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid187_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid187_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w30_9 :  std_logic;
signal bh7_w31_8 :  std_logic;
signal bh7_w32_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid187_Out0_copy188 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid189_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid189_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w31_9 :  std_logic;
signal bh7_w32_8 :  std_logic;
signal bh7_w33_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid189_Out0_copy190 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid191_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid191_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w32_9 :  std_logic;
signal bh7_w33_8 :  std_logic;
signal bh7_w34_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid191_Out0_copy192 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid193_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid193_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w33_9 :  std_logic;
signal bh7_w34_8 :  std_logic;
signal bh7_w35_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid193_Out0_copy194 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid195_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid195_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w34_9 :  std_logic;
signal bh7_w35_8 :  std_logic;
signal bh7_w36_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid195_Out0_copy196 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid197_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid197_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w35_9 :  std_logic;
signal bh7_w36_8 :  std_logic;
signal bh7_w37_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid197_Out0_copy198 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid199_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid199_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w36_9 :  std_logic;
signal bh7_w37_8 :  std_logic;
signal bh7_w38_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid199_Out0_copy200 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid201_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid201_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w37_9 :  std_logic;
signal bh7_w38_8 :  std_logic;
signal bh7_w39_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid201_Out0_copy202 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid203_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid203_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w38_9 :  std_logic;
signal bh7_w39_8 :  std_logic;
signal bh7_w40_7 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid203_Out0_copy204 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid205_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid205_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w39_9 :  std_logic;
signal bh7_w40_8 :  std_logic;
signal bh7_w41_5 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid205_Out0_copy206 :  std_logic_vector(2 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid207_In0 :  std_logic_vector(5 downto 0);
signal Compressor_6_3_Freq300_uid172_bh7_uid207_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w40_9 :  std_logic;
signal bh7_w41_6 :  std_logic;
signal bh7_w42_5 :  std_logic;
signal Compressor_6_3_Freq300_uid172_bh7_uid207_Out0_copy208 :  std_logic_vector(2 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid209_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid209_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid209_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w41_7 :  std_logic;
signal bh7_w42_6 :  std_logic;
signal bh7_w43_4 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid209_Out0_copy210 :  std_logic_vector(2 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid211_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid211_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid211_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w42_7 :  std_logic;
signal bh7_w43_5 :  std_logic;
signal bh7_w44_3 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid211_Out0_copy212 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid213_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid213_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid213_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w43_6 :  std_logic;
signal bh7_w44_4 :  std_logic;
signal bh7_w45_2 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid213_Out0_copy214 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid215_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid215_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid215_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w45_3 :  std_logic;
signal bh7_w46_2 :  std_logic;
signal bh7_w47_0 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid215_Out0_copy216 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid217_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid217_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid217_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w19_5 :  std_logic;
signal bh7_w20_6 :  std_logic;
signal bh7_w21_7 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid217_Out0_copy218 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid219_In0 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid219_Out0 :  std_logic_vector(1 downto 0);
signal bh7_w21_8 :  std_logic;
signal bh7_w22_8 :  std_logic;
signal Compressor_3_2_Freq300_uid160_bh7_uid219_Out0_copy220 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid221_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid221_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid221_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w22_9 :  std_logic;
signal bh7_w23_10 :  std_logic;
signal bh7_w24_10 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid221_Out0_copy222 :  std_logic_vector(2 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid223_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid223_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid223_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w24_11 :  std_logic;
signal bh7_w25_10 :  std_logic;
signal bh7_w26_10 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid223_Out0_copy224 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid225_In0 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid225_Out0 :  std_logic_vector(1 downto 0);
signal bh7_w25_11 :  std_logic;
signal bh7_w26_11 :  std_logic;
signal Compressor_3_2_Freq300_uid160_bh7_uid225_Out0_copy226 :  std_logic_vector(1 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid227_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid227_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid227_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w26_12 :  std_logic;
signal bh7_w27_10 :  std_logic;
signal bh7_w28_10 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid227_Out0_copy228 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid229_In0 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid229_Out0 :  std_logic_vector(1 downto 0);
signal bh7_w27_11 :  std_logic;
signal bh7_w28_11 :  std_logic;
signal Compressor_3_2_Freq300_uid160_bh7_uid229_Out0_copy230 :  std_logic_vector(1 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid231_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid231_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid231_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w28_12 :  std_logic;
signal bh7_w29_10 :  std_logic;
signal bh7_w30_10 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid231_Out0_copy232 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid233_In0 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid233_Out0 :  std_logic_vector(1 downto 0);
signal bh7_w29_11 :  std_logic;
signal bh7_w30_11 :  std_logic;
signal Compressor_3_2_Freq300_uid160_bh7_uid233_Out0_copy234 :  std_logic_vector(1 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid235_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid235_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid235_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w30_12 :  std_logic;
signal bh7_w31_10 :  std_logic;
signal bh7_w32_10 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid235_Out0_copy236 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid237_In0 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid237_Out0 :  std_logic_vector(1 downto 0);
signal bh7_w31_11 :  std_logic;
signal bh7_w32_11 :  std_logic;
signal Compressor_3_2_Freq300_uid160_bh7_uid237_Out0_copy238 :  std_logic_vector(1 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid239_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid239_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid239_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w32_12 :  std_logic;
signal bh7_w33_10 :  std_logic;
signal bh7_w34_10 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid239_Out0_copy240 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid241_In0 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid241_Out0 :  std_logic_vector(1 downto 0);
signal bh7_w33_11 :  std_logic;
signal bh7_w34_11 :  std_logic;
signal Compressor_3_2_Freq300_uid160_bh7_uid241_Out0_copy242 :  std_logic_vector(1 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid243_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid243_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid243_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w34_12 :  std_logic;
signal bh7_w35_10 :  std_logic;
signal bh7_w36_10 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid243_Out0_copy244 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid245_In0 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid245_Out0 :  std_logic_vector(1 downto 0);
signal bh7_w35_11 :  std_logic;
signal bh7_w36_11 :  std_logic;
signal Compressor_3_2_Freq300_uid160_bh7_uid245_Out0_copy246 :  std_logic_vector(1 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid247_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid247_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid247_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w36_12 :  std_logic;
signal bh7_w37_10 :  std_logic;
signal bh7_w38_10 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid247_Out0_copy248 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid249_In0 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid249_Out0 :  std_logic_vector(1 downto 0);
signal bh7_w37_11 :  std_logic;
signal bh7_w38_11 :  std_logic;
signal Compressor_3_2_Freq300_uid160_bh7_uid249_Out0_copy250 :  std_logic_vector(1 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid251_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid251_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid251_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w38_12 :  std_logic;
signal bh7_w39_10 :  std_logic;
signal bh7_w40_10 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid251_Out0_copy252 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid253_In0 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid253_Out0 :  std_logic_vector(1 downto 0);
signal bh7_w39_11 :  std_logic;
signal bh7_w40_11 :  std_logic;
signal Compressor_3_2_Freq300_uid160_bh7_uid253_Out0_copy254 :  std_logic_vector(1 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid255_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid255_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid255_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w40_12 :  std_logic;
signal bh7_w41_8 :  std_logic;
signal bh7_w42_8 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid255_Out0_copy256 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid257_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid257_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid257_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w41_9 :  std_logic;
signal bh7_w42_9 :  std_logic;
signal bh7_w43_7 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid257_Out0_copy258 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid259_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid259_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid259_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w43_8 :  std_logic;
signal bh7_w44_5 :  std_logic;
signal bh7_w45_4 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid259_Out0_copy260 :  std_logic_vector(2 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid261_In0 :  std_logic_vector(3 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid261_In1 :  std_logic_vector(0 downto 0);
signal Compressor_14_3_Freq300_uid164_bh7_uid261_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w45_5 :  std_logic;
signal bh7_w46_3 :  std_logic;
signal bh7_w47_1 :  std_logic;
signal Compressor_14_3_Freq300_uid164_bh7_uid261_Out0_copy262 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid263_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid263_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid263_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w21_9 :  std_logic;
signal bh7_w22_10 :  std_logic;
signal bh7_w23_11 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid263_Out0_copy264 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid265_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid265_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid265_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w23_12 :  std_logic;
signal bh7_w24_12 :  std_logic;
signal bh7_w25_12 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid265_Out0_copy266 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid267_In0 :  std_logic_vector(2 downto 0);
signal Compressor_3_2_Freq300_uid160_bh7_uid267_Out0 :  std_logic_vector(1 downto 0);
signal bh7_w25_13 :  std_logic;
signal bh7_w26_13 :  std_logic;
signal Compressor_3_2_Freq300_uid160_bh7_uid267_Out0_copy268 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid269_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid269_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid269_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w26_14 :  std_logic;
signal bh7_w27_12 :  std_logic;
signal bh7_w28_13 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid269_Out0_copy270 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid271_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid271_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid271_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w28_14 :  std_logic;
signal bh7_w29_12 :  std_logic;
signal bh7_w30_13 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid271_Out0_copy272 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid273_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid273_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid273_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w30_14 :  std_logic;
signal bh7_w31_12 :  std_logic;
signal bh7_w32_13 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid273_Out0_copy274 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid275_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid275_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid275_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w32_14 :  std_logic;
signal bh7_w33_12 :  std_logic;
signal bh7_w34_13 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid275_Out0_copy276 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid277_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid277_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid277_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w34_14 :  std_logic;
signal bh7_w35_12 :  std_logic;
signal bh7_w36_13 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid277_Out0_copy278 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid279_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid279_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid279_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w36_14 :  std_logic;
signal bh7_w37_12 :  std_logic;
signal bh7_w38_13 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid279_Out0_copy280 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid281_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid281_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid281_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w38_14 :  std_logic;
signal bh7_w39_12 :  std_logic;
signal bh7_w40_13 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid281_Out0_copy282 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid283_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid283_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid283_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w40_14 :  std_logic;
signal bh7_w41_10 :  std_logic;
signal bh7_w42_10 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid283_Out0_copy284 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid285_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid285_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid285_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w42_11 :  std_logic;
signal bh7_w43_9 :  std_logic;
signal bh7_w44_6 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid285_Out0_copy286 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid287_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid287_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid287_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w44_7 :  std_logic;
signal bh7_w45_6 :  std_logic;
signal bh7_w46_4 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid287_Out0_copy288 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid289_In0 :  std_logic_vector(2 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid289_In1 :  std_logic_vector(1 downto 0);
signal Compressor_23_3_Freq300_uid156_bh7_uid289_Out0 :  std_logic_vector(2 downto 0);
signal bh7_w46_5 :  std_logic;
signal bh7_w47_2 :  std_logic;
signal Compressor_23_3_Freq300_uid156_bh7_uid289_Out0_copy290 :  std_logic_vector(2 downto 0);
signal tmp_bitheapResult_bh7_22, tmp_bitheapResult_bh7_22_d1 :  std_logic_vector(22 downto 0);
signal bitheapFinalAdd_bh7_In0 :  std_logic_vector(25 downto 0);
signal bitheapFinalAdd_bh7_In1 :  std_logic_vector(25 downto 0);
signal bitheapFinalAdd_bh7_Cin :  std_logic;
signal bitheapFinalAdd_bh7_Out :  std_logic_vector(25 downto 0);
signal bitheapResult_bh7 :  std_logic_vector(47 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            tmp_bitheapResult_bh7_22_d1 <=  tmp_bitheapResult_bh7_22;
         end if;
      end process;
   XX_m6 <= X ;
   YY_m6 <= Y ;
   tile_0_X <= X(16 downto 0);
   tile_0_Y <= Y(23 downto 0);
   tile_0_mult: DSPBlock_17x24_Freq300_uid9
      port map ( clk  => clk,
                 X => tile_0_X,
                 Y => tile_0_Y,
                 R => tile_0_output);

   tile_0_filtered_output <= unsigned(tile_0_output(40 downto 0));
   bh7_w0_0 <= tile_0_filtered_output(0);
   bh7_w1_0 <= tile_0_filtered_output(1);
   bh7_w2_0 <= tile_0_filtered_output(2);
   bh7_w3_0 <= tile_0_filtered_output(3);
   bh7_w4_0 <= tile_0_filtered_output(4);
   bh7_w5_0 <= tile_0_filtered_output(5);
   bh7_w6_0 <= tile_0_filtered_output(6);
   bh7_w7_0 <= tile_0_filtered_output(7);
   bh7_w8_0 <= tile_0_filtered_output(8);
   bh7_w9_0 <= tile_0_filtered_output(9);
   bh7_w10_0 <= tile_0_filtered_output(10);
   bh7_w11_0 <= tile_0_filtered_output(11);
   bh7_w12_0 <= tile_0_filtered_output(12);
   bh7_w13_0 <= tile_0_filtered_output(13);
   bh7_w14_0 <= tile_0_filtered_output(14);
   bh7_w15_0 <= tile_0_filtered_output(15);
   bh7_w16_0 <= tile_0_filtered_output(16);
   bh7_w17_0 <= tile_0_filtered_output(17);
   bh7_w18_0 <= tile_0_filtered_output(18);
   bh7_w19_0 <= tile_0_filtered_output(19);
   bh7_w20_0 <= tile_0_filtered_output(20);
   bh7_w21_0 <= tile_0_filtered_output(21);
   bh7_w22_0 <= tile_0_filtered_output(22);
   bh7_w23_0 <= tile_0_filtered_output(23);
   bh7_w24_0 <= tile_0_filtered_output(24);
   bh7_w25_0 <= tile_0_filtered_output(25);
   bh7_w26_0 <= tile_0_filtered_output(26);
   bh7_w27_0 <= tile_0_filtered_output(27);
   bh7_w28_0 <= tile_0_filtered_output(28);
   bh7_w29_0 <= tile_0_filtered_output(29);
   bh7_w30_0 <= tile_0_filtered_output(30);
   bh7_w31_0 <= tile_0_filtered_output(31);
   bh7_w32_0 <= tile_0_filtered_output(32);
   bh7_w33_0 <= tile_0_filtered_output(33);
   bh7_w34_0 <= tile_0_filtered_output(34);
   bh7_w35_0 <= tile_0_filtered_output(35);
   bh7_w36_0 <= tile_0_filtered_output(36);
   bh7_w37_0 <= tile_0_filtered_output(37);
   bh7_w38_0 <= tile_0_filtered_output(38);
   bh7_w39_0 <= tile_0_filtered_output(39);
   bh7_w40_0 <= tile_0_filtered_output(40);
   tile_1_X <= X(23 downto 23);
   tile_1_Y <= Y(23 downto 22);
   tile_1_mult: IntMultiplierLUT_1x2_Freq300_uid11
      port map ( clk  => clk,
                 X => tile_1_X,
                 Y => tile_1_Y,
                 R => tile_1_output);

   tile_1_filtered_output <= unsigned(tile_1_output(1 downto 0));
   bh7_w45_0 <= tile_1_filtered_output(0);
   bh7_w46_0 <= tile_1_filtered_output(1);
   tile_2_X <= X(22 downto 20);
   tile_2_Y <= Y(23 downto 22);
   tile_2_mult: IntMultiplierLUT_3x2_Freq300_uid13
      port map ( clk  => clk,
                 X => tile_2_X,
                 Y => tile_2_Y,
                 R => tile_2_output);

   tile_2_filtered_output <= unsigned(tile_2_output(4 downto 0));
   bh7_w42_0 <= tile_2_filtered_output(0);
   bh7_w43_0 <= tile_2_filtered_output(1);
   bh7_w44_0 <= tile_2_filtered_output(2);
   bh7_w45_1 <= tile_2_filtered_output(3);
   bh7_w46_1 <= tile_2_filtered_output(4);
   tile_3_X <= X(19 downto 17);
   tile_3_Y <= Y(23 downto 22);
   tile_3_mult: IntMultiplierLUT_3x2_Freq300_uid18
      port map ( clk  => clk,
                 X => tile_3_X,
                 Y => tile_3_Y,
                 R => tile_3_output);

   tile_3_filtered_output <= unsigned(tile_3_output(4 downto 0));
   bh7_w39_1 <= tile_3_filtered_output(0);
   bh7_w40_1 <= tile_3_filtered_output(1);
   bh7_w41_0 <= tile_3_filtered_output(2);
   bh7_w42_1 <= tile_3_filtered_output(3);
   bh7_w43_1 <= tile_3_filtered_output(4);
   tile_4_X <= X(23 downto 23);
   tile_4_Y <= Y(21 downto 20);
   tile_4_mult: IntMultiplierLUT_1x2_Freq300_uid23
      port map ( clk  => clk,
                 X => tile_4_X,
                 Y => tile_4_Y,
                 R => tile_4_output);

   tile_4_filtered_output <= unsigned(tile_4_output(1 downto 0));
   bh7_w43_2 <= tile_4_filtered_output(0);
   bh7_w44_1 <= tile_4_filtered_output(1);
   tile_5_X <= X(22 downto 20);
   tile_5_Y <= Y(21 downto 20);
   tile_5_mult: IntMultiplierLUT_3x2_Freq300_uid25
      port map ( clk  => clk,
                 X => tile_5_X,
                 Y => tile_5_Y,
                 R => tile_5_output);

   tile_5_filtered_output <= unsigned(tile_5_output(4 downto 0));
   bh7_w40_2 <= tile_5_filtered_output(0);
   bh7_w41_1 <= tile_5_filtered_output(1);
   bh7_w42_2 <= tile_5_filtered_output(2);
   bh7_w43_3 <= tile_5_filtered_output(3);
   bh7_w44_2 <= tile_5_filtered_output(4);
   tile_6_X <= X(19 downto 17);
   tile_6_Y <= Y(21 downto 20);
   tile_6_mult: IntMultiplierLUT_3x2_Freq300_uid30
      port map ( clk  => clk,
                 X => tile_6_X,
                 Y => tile_6_Y,
                 R => tile_6_output);

   tile_6_filtered_output <= unsigned(tile_6_output(4 downto 0));
   bh7_w37_1 <= tile_6_filtered_output(0);
   bh7_w38_1 <= tile_6_filtered_output(1);
   bh7_w39_2 <= tile_6_filtered_output(2);
   bh7_w40_3 <= tile_6_filtered_output(3);
   bh7_w41_2 <= tile_6_filtered_output(4);
   tile_7_X <= X(23 downto 23);
   tile_7_Y <= Y(19 downto 18);
   tile_7_mult: IntMultiplierLUT_1x2_Freq300_uid35
      port map ( clk  => clk,
                 X => tile_7_X,
                 Y => tile_7_Y,
                 R => tile_7_output);

   tile_7_filtered_output <= unsigned(tile_7_output(1 downto 0));
   bh7_w41_3 <= tile_7_filtered_output(0);
   bh7_w42_3 <= tile_7_filtered_output(1);
   tile_8_X <= X(22 downto 20);
   tile_8_Y <= Y(19 downto 18);
   tile_8_mult: IntMultiplierLUT_3x2_Freq300_uid37
      port map ( clk  => clk,
                 X => tile_8_X,
                 Y => tile_8_Y,
                 R => tile_8_output);

   tile_8_filtered_output <= unsigned(tile_8_output(4 downto 0));
   bh7_w38_2 <= tile_8_filtered_output(0);
   bh7_w39_3 <= tile_8_filtered_output(1);
   bh7_w40_4 <= tile_8_filtered_output(2);
   bh7_w41_4 <= tile_8_filtered_output(3);
   bh7_w42_4 <= tile_8_filtered_output(4);
   tile_9_X <= X(19 downto 17);
   tile_9_Y <= Y(19 downto 18);
   tile_9_mult: IntMultiplierLUT_3x2_Freq300_uid42
      port map ( clk  => clk,
                 X => tile_9_X,
                 Y => tile_9_Y,
                 R => tile_9_output);

   tile_9_filtered_output <= unsigned(tile_9_output(4 downto 0));
   bh7_w35_1 <= tile_9_filtered_output(0);
   bh7_w36_1 <= tile_9_filtered_output(1);
   bh7_w37_2 <= tile_9_filtered_output(2);
   bh7_w38_3 <= tile_9_filtered_output(3);
   bh7_w39_4 <= tile_9_filtered_output(4);
   tile_10_X <= X(23 downto 23);
   tile_10_Y <= Y(17 downto 16);
   tile_10_mult: IntMultiplierLUT_1x2_Freq300_uid47
      port map ( clk  => clk,
                 X => tile_10_X,
                 Y => tile_10_Y,
                 R => tile_10_output);

   tile_10_filtered_output <= unsigned(tile_10_output(1 downto 0));
   bh7_w39_5 <= tile_10_filtered_output(0);
   bh7_w40_5 <= tile_10_filtered_output(1);
   tile_11_X <= X(22 downto 20);
   tile_11_Y <= Y(17 downto 16);
   tile_11_mult: IntMultiplierLUT_3x2_Freq300_uid49
      port map ( clk  => clk,
                 X => tile_11_X,
                 Y => tile_11_Y,
                 R => tile_11_output);

   tile_11_filtered_output <= unsigned(tile_11_output(4 downto 0));
   bh7_w36_2 <= tile_11_filtered_output(0);
   bh7_w37_3 <= tile_11_filtered_output(1);
   bh7_w38_4 <= tile_11_filtered_output(2);
   bh7_w39_6 <= tile_11_filtered_output(3);
   bh7_w40_6 <= tile_11_filtered_output(4);
   tile_12_X <= X(19 downto 17);
   tile_12_Y <= Y(17 downto 16);
   tile_12_mult: IntMultiplierLUT_3x2_Freq300_uid54
      port map ( clk  => clk,
                 X => tile_12_X,
                 Y => tile_12_Y,
                 R => tile_12_output);

   tile_12_filtered_output <= unsigned(tile_12_output(4 downto 0));
   bh7_w33_1 <= tile_12_filtered_output(0);
   bh7_w34_1 <= tile_12_filtered_output(1);
   bh7_w35_2 <= tile_12_filtered_output(2);
   bh7_w36_3 <= tile_12_filtered_output(3);
   bh7_w37_4 <= tile_12_filtered_output(4);
   tile_13_X <= X(23 downto 23);
   tile_13_Y <= Y(15 downto 14);
   tile_13_mult: IntMultiplierLUT_1x2_Freq300_uid59
      port map ( clk  => clk,
                 X => tile_13_X,
                 Y => tile_13_Y,
                 R => tile_13_output);

   tile_13_filtered_output <= unsigned(tile_13_output(1 downto 0));
   bh7_w37_5 <= tile_13_filtered_output(0);
   bh7_w38_5 <= tile_13_filtered_output(1);
   tile_14_X <= X(22 downto 20);
   tile_14_Y <= Y(15 downto 14);
   tile_14_mult: IntMultiplierLUT_3x2_Freq300_uid61
      port map ( clk  => clk,
                 X => tile_14_X,
                 Y => tile_14_Y,
                 R => tile_14_output);

   tile_14_filtered_output <= unsigned(tile_14_output(4 downto 0));
   bh7_w34_2 <= tile_14_filtered_output(0);
   bh7_w35_3 <= tile_14_filtered_output(1);
   bh7_w36_4 <= tile_14_filtered_output(2);
   bh7_w37_6 <= tile_14_filtered_output(3);
   bh7_w38_6 <= tile_14_filtered_output(4);
   tile_15_X <= X(19 downto 17);
   tile_15_Y <= Y(15 downto 14);
   tile_15_mult: IntMultiplierLUT_3x2_Freq300_uid66
      port map ( clk  => clk,
                 X => tile_15_X,
                 Y => tile_15_Y,
                 R => tile_15_output);

   tile_15_filtered_output <= unsigned(tile_15_output(4 downto 0));
   bh7_w31_1 <= tile_15_filtered_output(0);
   bh7_w32_1 <= tile_15_filtered_output(1);
   bh7_w33_2 <= tile_15_filtered_output(2);
   bh7_w34_3 <= tile_15_filtered_output(3);
   bh7_w35_4 <= tile_15_filtered_output(4);
   tile_16_X <= X(23 downto 23);
   tile_16_Y <= Y(13 downto 12);
   tile_16_mult: IntMultiplierLUT_1x2_Freq300_uid71
      port map ( clk  => clk,
                 X => tile_16_X,
                 Y => tile_16_Y,
                 R => tile_16_output);

   tile_16_filtered_output <= unsigned(tile_16_output(1 downto 0));
   bh7_w35_5 <= tile_16_filtered_output(0);
   bh7_w36_5 <= tile_16_filtered_output(1);
   tile_17_X <= X(22 downto 20);
   tile_17_Y <= Y(13 downto 12);
   tile_17_mult: IntMultiplierLUT_3x2_Freq300_uid73
      port map ( clk  => clk,
                 X => tile_17_X,
                 Y => tile_17_Y,
                 R => tile_17_output);

   tile_17_filtered_output <= unsigned(tile_17_output(4 downto 0));
   bh7_w32_2 <= tile_17_filtered_output(0);
   bh7_w33_3 <= tile_17_filtered_output(1);
   bh7_w34_4 <= tile_17_filtered_output(2);
   bh7_w35_6 <= tile_17_filtered_output(3);
   bh7_w36_6 <= tile_17_filtered_output(4);
   tile_18_X <= X(19 downto 17);
   tile_18_Y <= Y(13 downto 12);
   tile_18_mult: IntMultiplierLUT_3x2_Freq300_uid78
      port map ( clk  => clk,
                 X => tile_18_X,
                 Y => tile_18_Y,
                 R => tile_18_output);

   tile_18_filtered_output <= unsigned(tile_18_output(4 downto 0));
   bh7_w29_1 <= tile_18_filtered_output(0);
   bh7_w30_1 <= tile_18_filtered_output(1);
   bh7_w31_2 <= tile_18_filtered_output(2);
   bh7_w32_3 <= tile_18_filtered_output(3);
   bh7_w33_4 <= tile_18_filtered_output(4);
   tile_19_X <= X(23 downto 23);
   tile_19_Y <= Y(11 downto 10);
   tile_19_mult: IntMultiplierLUT_1x2_Freq300_uid83
      port map ( clk  => clk,
                 X => tile_19_X,
                 Y => tile_19_Y,
                 R => tile_19_output);

   tile_19_filtered_output <= unsigned(tile_19_output(1 downto 0));
   bh7_w33_5 <= tile_19_filtered_output(0);
   bh7_w34_5 <= tile_19_filtered_output(1);
   tile_20_X <= X(22 downto 20);
   tile_20_Y <= Y(11 downto 10);
   tile_20_mult: IntMultiplierLUT_3x2_Freq300_uid85
      port map ( clk  => clk,
                 X => tile_20_X,
                 Y => tile_20_Y,
                 R => tile_20_output);

   tile_20_filtered_output <= unsigned(tile_20_output(4 downto 0));
   bh7_w30_2 <= tile_20_filtered_output(0);
   bh7_w31_3 <= tile_20_filtered_output(1);
   bh7_w32_4 <= tile_20_filtered_output(2);
   bh7_w33_6 <= tile_20_filtered_output(3);
   bh7_w34_6 <= tile_20_filtered_output(4);
   tile_21_X <= X(19 downto 17);
   tile_21_Y <= Y(11 downto 10);
   tile_21_mult: IntMultiplierLUT_3x2_Freq300_uid90
      port map ( clk  => clk,
                 X => tile_21_X,
                 Y => tile_21_Y,
                 R => tile_21_output);

   tile_21_filtered_output <= unsigned(tile_21_output(4 downto 0));
   bh7_w27_1 <= tile_21_filtered_output(0);
   bh7_w28_1 <= tile_21_filtered_output(1);
   bh7_w29_2 <= tile_21_filtered_output(2);
   bh7_w30_3 <= tile_21_filtered_output(3);
   bh7_w31_4 <= tile_21_filtered_output(4);
   tile_22_X <= X(23 downto 23);
   tile_22_Y <= Y(9 downto 8);
   tile_22_mult: IntMultiplierLUT_1x2_Freq300_uid95
      port map ( clk  => clk,
                 X => tile_22_X,
                 Y => tile_22_Y,
                 R => tile_22_output);

   tile_22_filtered_output <= unsigned(tile_22_output(1 downto 0));
   bh7_w31_5 <= tile_22_filtered_output(0);
   bh7_w32_5 <= tile_22_filtered_output(1);
   tile_23_X <= X(22 downto 20);
   tile_23_Y <= Y(9 downto 8);
   tile_23_mult: IntMultiplierLUT_3x2_Freq300_uid97
      port map ( clk  => clk,
                 X => tile_23_X,
                 Y => tile_23_Y,
                 R => tile_23_output);

   tile_23_filtered_output <= unsigned(tile_23_output(4 downto 0));
   bh7_w28_2 <= tile_23_filtered_output(0);
   bh7_w29_3 <= tile_23_filtered_output(1);
   bh7_w30_4 <= tile_23_filtered_output(2);
   bh7_w31_6 <= tile_23_filtered_output(3);
   bh7_w32_6 <= tile_23_filtered_output(4);
   tile_24_X <= X(19 downto 17);
   tile_24_Y <= Y(9 downto 8);
   tile_24_mult: IntMultiplierLUT_3x2_Freq300_uid102
      port map ( clk  => clk,
                 X => tile_24_X,
                 Y => tile_24_Y,
                 R => tile_24_output);

   tile_24_filtered_output <= unsigned(tile_24_output(4 downto 0));
   bh7_w25_1 <= tile_24_filtered_output(0);
   bh7_w26_1 <= tile_24_filtered_output(1);
   bh7_w27_2 <= tile_24_filtered_output(2);
   bh7_w28_3 <= tile_24_filtered_output(3);
   bh7_w29_4 <= tile_24_filtered_output(4);
   tile_25_X <= X(23 downto 23);
   tile_25_Y <= Y(7 downto 6);
   tile_25_mult: IntMultiplierLUT_1x2_Freq300_uid107
      port map ( clk  => clk,
                 X => tile_25_X,
                 Y => tile_25_Y,
                 R => tile_25_output);

   tile_25_filtered_output <= unsigned(tile_25_output(1 downto 0));
   bh7_w29_5 <= tile_25_filtered_output(0);
   bh7_w30_5 <= tile_25_filtered_output(1);
   tile_26_X <= X(22 downto 20);
   tile_26_Y <= Y(7 downto 6);
   tile_26_mult: IntMultiplierLUT_3x2_Freq300_uid109
      port map ( clk  => clk,
                 X => tile_26_X,
                 Y => tile_26_Y,
                 R => tile_26_output);

   tile_26_filtered_output <= unsigned(tile_26_output(4 downto 0));
   bh7_w26_2 <= tile_26_filtered_output(0);
   bh7_w27_3 <= tile_26_filtered_output(1);
   bh7_w28_4 <= tile_26_filtered_output(2);
   bh7_w29_6 <= tile_26_filtered_output(3);
   bh7_w30_6 <= tile_26_filtered_output(4);
   tile_27_X <= X(19 downto 17);
   tile_27_Y <= Y(7 downto 6);
   tile_27_mult: IntMultiplierLUT_3x2_Freq300_uid114
      port map ( clk  => clk,
                 X => tile_27_X,
                 Y => tile_27_Y,
                 R => tile_27_output);

   tile_27_filtered_output <= unsigned(tile_27_output(4 downto 0));
   bh7_w23_1 <= tile_27_filtered_output(0);
   bh7_w24_1 <= tile_27_filtered_output(1);
   bh7_w25_2 <= tile_27_filtered_output(2);
   bh7_w26_3 <= tile_27_filtered_output(3);
   bh7_w27_4 <= tile_27_filtered_output(4);
   tile_28_X <= X(23 downto 23);
   tile_28_Y <= Y(5 downto 4);
   tile_28_mult: IntMultiplierLUT_1x2_Freq300_uid119
      port map ( clk  => clk,
                 X => tile_28_X,
                 Y => tile_28_Y,
                 R => tile_28_output);

   tile_28_filtered_output <= unsigned(tile_28_output(1 downto 0));
   bh7_w27_5 <= tile_28_filtered_output(0);
   bh7_w28_5 <= tile_28_filtered_output(1);
   tile_29_X <= X(22 downto 20);
   tile_29_Y <= Y(5 downto 4);
   tile_29_mult: IntMultiplierLUT_3x2_Freq300_uid121
      port map ( clk  => clk,
                 X => tile_29_X,
                 Y => tile_29_Y,
                 R => tile_29_output);

   tile_29_filtered_output <= unsigned(tile_29_output(4 downto 0));
   bh7_w24_2 <= tile_29_filtered_output(0);
   bh7_w25_3 <= tile_29_filtered_output(1);
   bh7_w26_4 <= tile_29_filtered_output(2);
   bh7_w27_6 <= tile_29_filtered_output(3);
   bh7_w28_6 <= tile_29_filtered_output(4);
   tile_30_X <= X(19 downto 17);
   tile_30_Y <= Y(5 downto 4);
   tile_30_mult: IntMultiplierLUT_3x2_Freq300_uid126
      port map ( clk  => clk,
                 X => tile_30_X,
                 Y => tile_30_Y,
                 R => tile_30_output);

   tile_30_filtered_output <= unsigned(tile_30_output(4 downto 0));
   bh7_w21_1 <= tile_30_filtered_output(0);
   bh7_w22_1 <= tile_30_filtered_output(1);
   bh7_w23_2 <= tile_30_filtered_output(2);
   bh7_w24_3 <= tile_30_filtered_output(3);
   bh7_w25_4 <= tile_30_filtered_output(4);
   tile_31_X <= X(23 downto 23);
   tile_31_Y <= Y(3 downto 2);
   tile_31_mult: IntMultiplierLUT_1x2_Freq300_uid131
      port map ( clk  => clk,
                 X => tile_31_X,
                 Y => tile_31_Y,
                 R => tile_31_output);

   tile_31_filtered_output <= unsigned(tile_31_output(1 downto 0));
   bh7_w25_5 <= tile_31_filtered_output(0);
   bh7_w26_5 <= tile_31_filtered_output(1);
   tile_32_X <= X(22 downto 20);
   tile_32_Y <= Y(3 downto 2);
   tile_32_mult: IntMultiplierLUT_3x2_Freq300_uid133
      port map ( clk  => clk,
                 X => tile_32_X,
                 Y => tile_32_Y,
                 R => tile_32_output);

   tile_32_filtered_output <= unsigned(tile_32_output(4 downto 0));
   bh7_w22_2 <= tile_32_filtered_output(0);
   bh7_w23_3 <= tile_32_filtered_output(1);
   bh7_w24_4 <= tile_32_filtered_output(2);
   bh7_w25_6 <= tile_32_filtered_output(3);
   bh7_w26_6 <= tile_32_filtered_output(4);
   tile_33_X <= X(19 downto 17);
   tile_33_Y <= Y(3 downto 2);
   tile_33_mult: IntMultiplierLUT_3x2_Freq300_uid138
      port map ( clk  => clk,
                 X => tile_33_X,
                 Y => tile_33_Y,
                 R => tile_33_output);

   tile_33_filtered_output <= unsigned(tile_33_output(4 downto 0));
   bh7_w19_1 <= tile_33_filtered_output(0);
   bh7_w20_1 <= tile_33_filtered_output(1);
   bh7_w21_2 <= tile_33_filtered_output(2);
   bh7_w22_3 <= tile_33_filtered_output(3);
   bh7_w23_4 <= tile_33_filtered_output(4);
   tile_34_X <= X(23 downto 23);
   tile_34_Y <= Y(1 downto 0);
   tile_34_mult: IntMultiplierLUT_1x2_Freq300_uid143
      port map ( clk  => clk,
                 X => tile_34_X,
                 Y => tile_34_Y,
                 R => tile_34_output);

   tile_34_filtered_output <= unsigned(tile_34_output(1 downto 0));
   bh7_w23_5 <= tile_34_filtered_output(0);
   bh7_w24_5 <= tile_34_filtered_output(1);
   tile_35_X <= X(22 downto 20);
   tile_35_Y <= Y(1 downto 0);
   tile_35_mult: IntMultiplierLUT_3x2_Freq300_uid145
      port map ( clk  => clk,
                 X => tile_35_X,
                 Y => tile_35_Y,
                 R => tile_35_output);

   tile_35_filtered_output <= unsigned(tile_35_output(4 downto 0));
   bh7_w20_2 <= tile_35_filtered_output(0);
   bh7_w21_3 <= tile_35_filtered_output(1);
   bh7_w22_4 <= tile_35_filtered_output(2);
   bh7_w23_6 <= tile_35_filtered_output(3);
   bh7_w24_6 <= tile_35_filtered_output(4);
   tile_36_X <= X(19 downto 17);
   tile_36_Y <= Y(1 downto 0);
   tile_36_mult: IntMultiplierLUT_3x2_Freq300_uid150
      port map ( clk  => clk,
                 X => tile_36_X,
                 Y => tile_36_Y,
                 R => tile_36_output);

   tile_36_filtered_output <= unsigned(tile_36_output(4 downto 0));
   bh7_w17_1 <= tile_36_filtered_output(0);
   bh7_w18_1 <= tile_36_filtered_output(1);
   bh7_w19_2 <= tile_36_filtered_output(2);
   bh7_w20_3 <= tile_36_filtered_output(3);
   bh7_w21_4 <= tile_36_filtered_output(4);

   -- Adding the constant bits 
      -- All the constant bits are zero, nothing to add


   Compressor_23_3_Freq300_uid156_bh7_uid157_In0 <= "" & bh7_w17_0 & bh7_w17_1 & "0";
   Compressor_23_3_Freq300_uid156_bh7_uid157_In1 <= "" & bh7_w18_0 & bh7_w18_1;
   bh7_w17_2 <= Compressor_23_3_Freq300_uid156_bh7_uid157_Out0(0);
   bh7_w18_2 <= Compressor_23_3_Freq300_uid156_bh7_uid157_Out0(1);
   bh7_w19_3 <= Compressor_23_3_Freq300_uid156_bh7_uid157_Out0(2);
   Compressor_23_3_Freq300_uid156_uid157: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid157_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid157_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid157_Out0_copy158);
   Compressor_23_3_Freq300_uid156_bh7_uid157_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid157_Out0_copy158; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq300_uid160_bh7_uid161_In0 <= "" & bh7_w19_0 & bh7_w19_1 & bh7_w19_2;
   bh7_w19_4 <= Compressor_3_2_Freq300_uid160_bh7_uid161_Out0(0);
   bh7_w20_4 <= Compressor_3_2_Freq300_uid160_bh7_uid161_Out0(1);
   Compressor_3_2_Freq300_uid160_uid161: Compressor_3_2_Freq300_uid160
      port map ( X0 => Compressor_3_2_Freq300_uid160_bh7_uid161_In0,
                 R => Compressor_3_2_Freq300_uid160_bh7_uid161_Out0_copy162);
   Compressor_3_2_Freq300_uid160_bh7_uid161_Out0 <= Compressor_3_2_Freq300_uid160_bh7_uid161_Out0_copy162; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid165_In0 <= "" & bh7_w20_0 & bh7_w20_1 & bh7_w20_2 & bh7_w20_3;
   Compressor_14_3_Freq300_uid164_bh7_uid165_In1 <= "" & bh7_w21_0;
   bh7_w20_5 <= Compressor_14_3_Freq300_uid164_bh7_uid165_Out0(0);
   bh7_w21_5 <= Compressor_14_3_Freq300_uid164_bh7_uid165_Out0(1);
   bh7_w22_5 <= Compressor_14_3_Freq300_uid164_bh7_uid165_Out0(2);
   Compressor_14_3_Freq300_uid164_uid165: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid165_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid165_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid165_Out0_copy166);
   Compressor_14_3_Freq300_uid164_bh7_uid165_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid165_Out0_copy166; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid167_In0 <= "" & bh7_w21_1 & bh7_w21_2 & bh7_w21_3 & bh7_w21_4;
   Compressor_14_3_Freq300_uid164_bh7_uid167_In1 <= "" & bh7_w22_0;
   bh7_w21_6 <= Compressor_14_3_Freq300_uid164_bh7_uid167_Out0(0);
   bh7_w22_6 <= Compressor_14_3_Freq300_uid164_bh7_uid167_Out0(1);
   bh7_w23_7 <= Compressor_14_3_Freq300_uid164_bh7_uid167_Out0(2);
   Compressor_14_3_Freq300_uid164_uid167: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid167_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid167_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid167_Out0_copy168);
   Compressor_14_3_Freq300_uid164_bh7_uid167_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid167_Out0_copy168; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid169_In0 <= "" & bh7_w22_1 & bh7_w22_2 & bh7_w22_3 & bh7_w22_4;
   Compressor_14_3_Freq300_uid164_bh7_uid169_In1 <= "" & bh7_w23_0;
   bh7_w22_7 <= Compressor_14_3_Freq300_uid164_bh7_uid169_Out0(0);
   bh7_w23_8 <= Compressor_14_3_Freq300_uid164_bh7_uid169_Out0(1);
   bh7_w24_7 <= Compressor_14_3_Freq300_uid164_bh7_uid169_Out0(2);
   Compressor_14_3_Freq300_uid164_uid169: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid169_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid169_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid169_Out0_copy170);
   Compressor_14_3_Freq300_uid164_bh7_uid169_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid169_Out0_copy170; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid173_In0 <= "" & bh7_w23_1 & bh7_w23_2 & bh7_w23_3 & bh7_w23_4 & bh7_w23_5 & bh7_w23_6;
   bh7_w23_9 <= Compressor_6_3_Freq300_uid172_bh7_uid173_Out0(0);
   bh7_w24_8 <= Compressor_6_3_Freq300_uid172_bh7_uid173_Out0(1);
   bh7_w25_7 <= Compressor_6_3_Freq300_uid172_bh7_uid173_Out0(2);
   Compressor_6_3_Freq300_uid172_uid173: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid173_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid173_Out0_copy174);
   Compressor_6_3_Freq300_uid172_bh7_uid173_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid173_Out0_copy174; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid175_In0 <= "" & bh7_w24_0 & bh7_w24_1 & bh7_w24_2 & bh7_w24_3 & bh7_w24_4 & bh7_w24_5;
   bh7_w24_9 <= Compressor_6_3_Freq300_uid172_bh7_uid175_Out0(0);
   bh7_w25_8 <= Compressor_6_3_Freq300_uid172_bh7_uid175_Out0(1);
   bh7_w26_7 <= Compressor_6_3_Freq300_uid172_bh7_uid175_Out0(2);
   Compressor_6_3_Freq300_uid172_uid175: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid175_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid175_Out0_copy176);
   Compressor_6_3_Freq300_uid172_bh7_uid175_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid175_Out0_copy176; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid177_In0 <= "" & bh7_w25_0 & bh7_w25_1 & bh7_w25_2 & bh7_w25_3 & bh7_w25_4 & bh7_w25_5;
   bh7_w25_9 <= Compressor_6_3_Freq300_uid172_bh7_uid177_Out0(0);
   bh7_w26_8 <= Compressor_6_3_Freq300_uid172_bh7_uid177_Out0(1);
   bh7_w27_7 <= Compressor_6_3_Freq300_uid172_bh7_uid177_Out0(2);
   Compressor_6_3_Freq300_uid172_uid177: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid177_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid177_Out0_copy178);
   Compressor_6_3_Freq300_uid172_bh7_uid177_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid177_Out0_copy178; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid179_In0 <= "" & bh7_w26_0 & bh7_w26_1 & bh7_w26_2 & bh7_w26_3 & bh7_w26_4 & bh7_w26_5;
   bh7_w26_9 <= Compressor_6_3_Freq300_uid172_bh7_uid179_Out0(0);
   bh7_w27_8 <= Compressor_6_3_Freq300_uid172_bh7_uid179_Out0(1);
   bh7_w28_7 <= Compressor_6_3_Freq300_uid172_bh7_uid179_Out0(2);
   Compressor_6_3_Freq300_uid172_uid179: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid179_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid179_Out0_copy180);
   Compressor_6_3_Freq300_uid172_bh7_uid179_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid179_Out0_copy180; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid181_In0 <= "" & bh7_w27_0 & bh7_w27_1 & bh7_w27_2 & bh7_w27_3 & bh7_w27_4 & bh7_w27_5;
   bh7_w27_9 <= Compressor_6_3_Freq300_uid172_bh7_uid181_Out0(0);
   bh7_w28_8 <= Compressor_6_3_Freq300_uid172_bh7_uid181_Out0(1);
   bh7_w29_7 <= Compressor_6_3_Freq300_uid172_bh7_uid181_Out0(2);
   Compressor_6_3_Freq300_uid172_uid181: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid181_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid181_Out0_copy182);
   Compressor_6_3_Freq300_uid172_bh7_uid181_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid181_Out0_copy182; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid183_In0 <= "" & bh7_w28_0 & bh7_w28_1 & bh7_w28_2 & bh7_w28_3 & bh7_w28_4 & bh7_w28_5;
   bh7_w28_9 <= Compressor_6_3_Freq300_uid172_bh7_uid183_Out0(0);
   bh7_w29_8 <= Compressor_6_3_Freq300_uid172_bh7_uid183_Out0(1);
   bh7_w30_7 <= Compressor_6_3_Freq300_uid172_bh7_uid183_Out0(2);
   Compressor_6_3_Freq300_uid172_uid183: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid183_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid183_Out0_copy184);
   Compressor_6_3_Freq300_uid172_bh7_uid183_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid183_Out0_copy184; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid185_In0 <= "" & bh7_w29_0 & bh7_w29_1 & bh7_w29_2 & bh7_w29_3 & bh7_w29_4 & bh7_w29_5;
   bh7_w29_9 <= Compressor_6_3_Freq300_uid172_bh7_uid185_Out0(0);
   bh7_w30_8 <= Compressor_6_3_Freq300_uid172_bh7_uid185_Out0(1);
   bh7_w31_7 <= Compressor_6_3_Freq300_uid172_bh7_uid185_Out0(2);
   Compressor_6_3_Freq300_uid172_uid185: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid185_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid185_Out0_copy186);
   Compressor_6_3_Freq300_uid172_bh7_uid185_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid185_Out0_copy186; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid187_In0 <= "" & bh7_w30_0 & bh7_w30_1 & bh7_w30_2 & bh7_w30_3 & bh7_w30_4 & bh7_w30_5;
   bh7_w30_9 <= Compressor_6_3_Freq300_uid172_bh7_uid187_Out0(0);
   bh7_w31_8 <= Compressor_6_3_Freq300_uid172_bh7_uid187_Out0(1);
   bh7_w32_7 <= Compressor_6_3_Freq300_uid172_bh7_uid187_Out0(2);
   Compressor_6_3_Freq300_uid172_uid187: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid187_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid187_Out0_copy188);
   Compressor_6_3_Freq300_uid172_bh7_uid187_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid187_Out0_copy188; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid189_In0 <= "" & bh7_w31_0 & bh7_w31_1 & bh7_w31_2 & bh7_w31_3 & bh7_w31_4 & bh7_w31_5;
   bh7_w31_9 <= Compressor_6_3_Freq300_uid172_bh7_uid189_Out0(0);
   bh7_w32_8 <= Compressor_6_3_Freq300_uid172_bh7_uid189_Out0(1);
   bh7_w33_7 <= Compressor_6_3_Freq300_uid172_bh7_uid189_Out0(2);
   Compressor_6_3_Freq300_uid172_uid189: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid189_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid189_Out0_copy190);
   Compressor_6_3_Freq300_uid172_bh7_uid189_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid189_Out0_copy190; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid191_In0 <= "" & bh7_w32_0 & bh7_w32_1 & bh7_w32_2 & bh7_w32_3 & bh7_w32_4 & bh7_w32_5;
   bh7_w32_9 <= Compressor_6_3_Freq300_uid172_bh7_uid191_Out0(0);
   bh7_w33_8 <= Compressor_6_3_Freq300_uid172_bh7_uid191_Out0(1);
   bh7_w34_7 <= Compressor_6_3_Freq300_uid172_bh7_uid191_Out0(2);
   Compressor_6_3_Freq300_uid172_uid191: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid191_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid191_Out0_copy192);
   Compressor_6_3_Freq300_uid172_bh7_uid191_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid191_Out0_copy192; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid193_In0 <= "" & bh7_w33_0 & bh7_w33_1 & bh7_w33_2 & bh7_w33_3 & bh7_w33_4 & bh7_w33_5;
   bh7_w33_9 <= Compressor_6_3_Freq300_uid172_bh7_uid193_Out0(0);
   bh7_w34_8 <= Compressor_6_3_Freq300_uid172_bh7_uid193_Out0(1);
   bh7_w35_7 <= Compressor_6_3_Freq300_uid172_bh7_uid193_Out0(2);
   Compressor_6_3_Freq300_uid172_uid193: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid193_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid193_Out0_copy194);
   Compressor_6_3_Freq300_uid172_bh7_uid193_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid193_Out0_copy194; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid195_In0 <= "" & bh7_w34_0 & bh7_w34_1 & bh7_w34_2 & bh7_w34_3 & bh7_w34_4 & bh7_w34_5;
   bh7_w34_9 <= Compressor_6_3_Freq300_uid172_bh7_uid195_Out0(0);
   bh7_w35_8 <= Compressor_6_3_Freq300_uid172_bh7_uid195_Out0(1);
   bh7_w36_7 <= Compressor_6_3_Freq300_uid172_bh7_uid195_Out0(2);
   Compressor_6_3_Freq300_uid172_uid195: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid195_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid195_Out0_copy196);
   Compressor_6_3_Freq300_uid172_bh7_uid195_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid195_Out0_copy196; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid197_In0 <= "" & bh7_w35_0 & bh7_w35_1 & bh7_w35_2 & bh7_w35_3 & bh7_w35_4 & bh7_w35_5;
   bh7_w35_9 <= Compressor_6_3_Freq300_uid172_bh7_uid197_Out0(0);
   bh7_w36_8 <= Compressor_6_3_Freq300_uid172_bh7_uid197_Out0(1);
   bh7_w37_7 <= Compressor_6_3_Freq300_uid172_bh7_uid197_Out0(2);
   Compressor_6_3_Freq300_uid172_uid197: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid197_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid197_Out0_copy198);
   Compressor_6_3_Freq300_uid172_bh7_uid197_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid197_Out0_copy198; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid199_In0 <= "" & bh7_w36_0 & bh7_w36_1 & bh7_w36_2 & bh7_w36_3 & bh7_w36_4 & bh7_w36_5;
   bh7_w36_9 <= Compressor_6_3_Freq300_uid172_bh7_uid199_Out0(0);
   bh7_w37_8 <= Compressor_6_3_Freq300_uid172_bh7_uid199_Out0(1);
   bh7_w38_7 <= Compressor_6_3_Freq300_uid172_bh7_uid199_Out0(2);
   Compressor_6_3_Freq300_uid172_uid199: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid199_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid199_Out0_copy200);
   Compressor_6_3_Freq300_uid172_bh7_uid199_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid199_Out0_copy200; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid201_In0 <= "" & bh7_w37_0 & bh7_w37_1 & bh7_w37_2 & bh7_w37_3 & bh7_w37_4 & bh7_w37_5;
   bh7_w37_9 <= Compressor_6_3_Freq300_uid172_bh7_uid201_Out0(0);
   bh7_w38_8 <= Compressor_6_3_Freq300_uid172_bh7_uid201_Out0(1);
   bh7_w39_7 <= Compressor_6_3_Freq300_uid172_bh7_uid201_Out0(2);
   Compressor_6_3_Freq300_uid172_uid201: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid201_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid201_Out0_copy202);
   Compressor_6_3_Freq300_uid172_bh7_uid201_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid201_Out0_copy202; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid203_In0 <= "" & bh7_w38_0 & bh7_w38_1 & bh7_w38_2 & bh7_w38_3 & bh7_w38_4 & bh7_w38_5;
   bh7_w38_9 <= Compressor_6_3_Freq300_uid172_bh7_uid203_Out0(0);
   bh7_w39_8 <= Compressor_6_3_Freq300_uid172_bh7_uid203_Out0(1);
   bh7_w40_7 <= Compressor_6_3_Freq300_uid172_bh7_uid203_Out0(2);
   Compressor_6_3_Freq300_uid172_uid203: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid203_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid203_Out0_copy204);
   Compressor_6_3_Freq300_uid172_bh7_uid203_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid203_Out0_copy204; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid205_In0 <= "" & bh7_w39_0 & bh7_w39_1 & bh7_w39_2 & bh7_w39_3 & bh7_w39_4 & bh7_w39_5;
   bh7_w39_9 <= Compressor_6_3_Freq300_uid172_bh7_uid205_Out0(0);
   bh7_w40_8 <= Compressor_6_3_Freq300_uid172_bh7_uid205_Out0(1);
   bh7_w41_5 <= Compressor_6_3_Freq300_uid172_bh7_uid205_Out0(2);
   Compressor_6_3_Freq300_uid172_uid205: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid205_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid205_Out0_copy206);
   Compressor_6_3_Freq300_uid172_bh7_uid205_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid205_Out0_copy206; -- output copy to hold a pipeline register if needed


   Compressor_6_3_Freq300_uid172_bh7_uid207_In0 <= "" & bh7_w40_0 & bh7_w40_1 & bh7_w40_2 & bh7_w40_3 & bh7_w40_4 & bh7_w40_5;
   bh7_w40_9 <= Compressor_6_3_Freq300_uid172_bh7_uid207_Out0(0);
   bh7_w41_6 <= Compressor_6_3_Freq300_uid172_bh7_uid207_Out0(1);
   bh7_w42_5 <= Compressor_6_3_Freq300_uid172_bh7_uid207_Out0(2);
   Compressor_6_3_Freq300_uid172_uid207: Compressor_6_3_Freq300_uid172
      port map ( X0 => Compressor_6_3_Freq300_uid172_bh7_uid207_In0,
                 R => Compressor_6_3_Freq300_uid172_bh7_uid207_Out0_copy208);
   Compressor_6_3_Freq300_uid172_bh7_uid207_Out0 <= Compressor_6_3_Freq300_uid172_bh7_uid207_Out0_copy208; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid209_In0 <= "" & bh7_w41_0 & bh7_w41_1 & bh7_w41_2 & bh7_w41_3;
   Compressor_14_3_Freq300_uid164_bh7_uid209_In1 <= "" & bh7_w42_0;
   bh7_w41_7 <= Compressor_14_3_Freq300_uid164_bh7_uid209_Out0(0);
   bh7_w42_6 <= Compressor_14_3_Freq300_uid164_bh7_uid209_Out0(1);
   bh7_w43_4 <= Compressor_14_3_Freq300_uid164_bh7_uid209_Out0(2);
   Compressor_14_3_Freq300_uid164_uid209: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid209_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid209_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid209_Out0_copy210);
   Compressor_14_3_Freq300_uid164_bh7_uid209_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid209_Out0_copy210; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid211_In0 <= "" & bh7_w42_1 & bh7_w42_2 & bh7_w42_3 & bh7_w42_4;
   Compressor_14_3_Freq300_uid164_bh7_uid211_In1 <= "" & bh7_w43_0;
   bh7_w42_7 <= Compressor_14_3_Freq300_uid164_bh7_uid211_Out0(0);
   bh7_w43_5 <= Compressor_14_3_Freq300_uid164_bh7_uid211_Out0(1);
   bh7_w44_3 <= Compressor_14_3_Freq300_uid164_bh7_uid211_Out0(2);
   Compressor_14_3_Freq300_uid164_uid211: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid211_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid211_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid211_Out0_copy212);
   Compressor_14_3_Freq300_uid164_bh7_uid211_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid211_Out0_copy212; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid213_In0 <= "" & bh7_w43_1 & bh7_w43_2 & bh7_w43_3;
   Compressor_23_3_Freq300_uid156_bh7_uid213_In1 <= "" & bh7_w44_0 & bh7_w44_1;
   bh7_w43_6 <= Compressor_23_3_Freq300_uid156_bh7_uid213_Out0(0);
   bh7_w44_4 <= Compressor_23_3_Freq300_uid156_bh7_uid213_Out0(1);
   bh7_w45_2 <= Compressor_23_3_Freq300_uid156_bh7_uid213_Out0(2);
   Compressor_23_3_Freq300_uid156_uid213: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid213_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid213_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid213_Out0_copy214);
   Compressor_23_3_Freq300_uid156_bh7_uid213_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid213_Out0_copy214; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid215_In0 <= "" & bh7_w45_0 & bh7_w45_1 & "0";
   Compressor_23_3_Freq300_uid156_bh7_uid215_In1 <= "" & bh7_w46_0 & bh7_w46_1;
   bh7_w45_3 <= Compressor_23_3_Freq300_uid156_bh7_uid215_Out0(0);
   bh7_w46_2 <= Compressor_23_3_Freq300_uid156_bh7_uid215_Out0(1);
   bh7_w47_0 <= Compressor_23_3_Freq300_uid156_bh7_uid215_Out0(2);
   Compressor_23_3_Freq300_uid156_uid215: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid215_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid215_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid215_Out0_copy216);
   Compressor_23_3_Freq300_uid156_bh7_uid215_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid215_Out0_copy216; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid217_In0 <= "" & bh7_w19_4 & bh7_w19_3 & "0";
   Compressor_23_3_Freq300_uid156_bh7_uid217_In1 <= "" & bh7_w20_5 & bh7_w20_4;
   bh7_w19_5 <= Compressor_23_3_Freq300_uid156_bh7_uid217_Out0(0);
   bh7_w20_6 <= Compressor_23_3_Freq300_uid156_bh7_uid217_Out0(1);
   bh7_w21_7 <= Compressor_23_3_Freq300_uid156_bh7_uid217_Out0(2);
   Compressor_23_3_Freq300_uid156_uid217: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid217_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid217_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid217_Out0_copy218);
   Compressor_23_3_Freq300_uid156_bh7_uid217_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid217_Out0_copy218; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq300_uid160_bh7_uid219_In0 <= "" & bh7_w21_6 & bh7_w21_5 & "0";
   bh7_w21_8 <= Compressor_3_2_Freq300_uid160_bh7_uid219_Out0(0);
   bh7_w22_8 <= Compressor_3_2_Freq300_uid160_bh7_uid219_Out0(1);
   Compressor_3_2_Freq300_uid160_uid219: Compressor_3_2_Freq300_uid160
      port map ( X0 => Compressor_3_2_Freq300_uid160_bh7_uid219_In0,
                 R => Compressor_3_2_Freq300_uid160_bh7_uid219_Out0_copy220);
   Compressor_3_2_Freq300_uid160_bh7_uid219_Out0 <= Compressor_3_2_Freq300_uid160_bh7_uid219_Out0_copy220; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid221_In0 <= "" & bh7_w22_7 & bh7_w22_6 & bh7_w22_5;
   Compressor_23_3_Freq300_uid156_bh7_uid221_In1 <= "" & bh7_w23_9 & bh7_w23_8;
   bh7_w22_9 <= Compressor_23_3_Freq300_uid156_bh7_uid221_Out0(0);
   bh7_w23_10 <= Compressor_23_3_Freq300_uid156_bh7_uid221_Out0(1);
   bh7_w24_10 <= Compressor_23_3_Freq300_uid156_bh7_uid221_Out0(2);
   Compressor_23_3_Freq300_uid156_uid221: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid221_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid221_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid221_Out0_copy222);
   Compressor_23_3_Freq300_uid156_bh7_uid221_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid221_Out0_copy222; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid223_In0 <= "" & bh7_w24_6 & bh7_w24_9 & bh7_w24_8 & bh7_w24_7;
   Compressor_14_3_Freq300_uid164_bh7_uid223_In1 <= "" & bh7_w25_6;
   bh7_w24_11 <= Compressor_14_3_Freq300_uid164_bh7_uid223_Out0(0);
   bh7_w25_10 <= Compressor_14_3_Freq300_uid164_bh7_uid223_Out0(1);
   bh7_w26_10 <= Compressor_14_3_Freq300_uid164_bh7_uid223_Out0(2);
   Compressor_14_3_Freq300_uid164_uid223: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid223_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid223_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid223_Out0_copy224);
   Compressor_14_3_Freq300_uid164_bh7_uid223_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid223_Out0_copy224; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq300_uid160_bh7_uid225_In0 <= "" & bh7_w25_9 & bh7_w25_8 & bh7_w25_7;
   bh7_w25_11 <= Compressor_3_2_Freq300_uid160_bh7_uid225_Out0(0);
   bh7_w26_11 <= Compressor_3_2_Freq300_uid160_bh7_uid225_Out0(1);
   Compressor_3_2_Freq300_uid160_uid225: Compressor_3_2_Freq300_uid160
      port map ( X0 => Compressor_3_2_Freq300_uid160_bh7_uid225_In0,
                 R => Compressor_3_2_Freq300_uid160_bh7_uid225_Out0_copy226);
   Compressor_3_2_Freq300_uid160_bh7_uid225_Out0 <= Compressor_3_2_Freq300_uid160_bh7_uid225_Out0_copy226; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid227_In0 <= "" & bh7_w26_6 & bh7_w26_9 & bh7_w26_8 & bh7_w26_7;
   Compressor_14_3_Freq300_uid164_bh7_uid227_In1 <= "" & bh7_w27_6;
   bh7_w26_12 <= Compressor_14_3_Freq300_uid164_bh7_uid227_Out0(0);
   bh7_w27_10 <= Compressor_14_3_Freq300_uid164_bh7_uid227_Out0(1);
   bh7_w28_10 <= Compressor_14_3_Freq300_uid164_bh7_uid227_Out0(2);
   Compressor_14_3_Freq300_uid164_uid227: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid227_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid227_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid227_Out0_copy228);
   Compressor_14_3_Freq300_uid164_bh7_uid227_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid227_Out0_copy228; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq300_uid160_bh7_uid229_In0 <= "" & bh7_w27_9 & bh7_w27_8 & bh7_w27_7;
   bh7_w27_11 <= Compressor_3_2_Freq300_uid160_bh7_uid229_Out0(0);
   bh7_w28_11 <= Compressor_3_2_Freq300_uid160_bh7_uid229_Out0(1);
   Compressor_3_2_Freq300_uid160_uid229: Compressor_3_2_Freq300_uid160
      port map ( X0 => Compressor_3_2_Freq300_uid160_bh7_uid229_In0,
                 R => Compressor_3_2_Freq300_uid160_bh7_uid229_Out0_copy230);
   Compressor_3_2_Freq300_uid160_bh7_uid229_Out0 <= Compressor_3_2_Freq300_uid160_bh7_uid229_Out0_copy230; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid231_In0 <= "" & bh7_w28_6 & bh7_w28_9 & bh7_w28_8 & bh7_w28_7;
   Compressor_14_3_Freq300_uid164_bh7_uid231_In1 <= "" & bh7_w29_6;
   bh7_w28_12 <= Compressor_14_3_Freq300_uid164_bh7_uid231_Out0(0);
   bh7_w29_10 <= Compressor_14_3_Freq300_uid164_bh7_uid231_Out0(1);
   bh7_w30_10 <= Compressor_14_3_Freq300_uid164_bh7_uid231_Out0(2);
   Compressor_14_3_Freq300_uid164_uid231: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid231_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid231_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid231_Out0_copy232);
   Compressor_14_3_Freq300_uid164_bh7_uid231_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid231_Out0_copy232; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq300_uid160_bh7_uid233_In0 <= "" & bh7_w29_9 & bh7_w29_8 & bh7_w29_7;
   bh7_w29_11 <= Compressor_3_2_Freq300_uid160_bh7_uid233_Out0(0);
   bh7_w30_11 <= Compressor_3_2_Freq300_uid160_bh7_uid233_Out0(1);
   Compressor_3_2_Freq300_uid160_uid233: Compressor_3_2_Freq300_uid160
      port map ( X0 => Compressor_3_2_Freq300_uid160_bh7_uid233_In0,
                 R => Compressor_3_2_Freq300_uid160_bh7_uid233_Out0_copy234);
   Compressor_3_2_Freq300_uid160_bh7_uid233_Out0 <= Compressor_3_2_Freq300_uid160_bh7_uid233_Out0_copy234; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid235_In0 <= "" & bh7_w30_6 & bh7_w30_9 & bh7_w30_8 & bh7_w30_7;
   Compressor_14_3_Freq300_uid164_bh7_uid235_In1 <= "" & bh7_w31_6;
   bh7_w30_12 <= Compressor_14_3_Freq300_uid164_bh7_uid235_Out0(0);
   bh7_w31_10 <= Compressor_14_3_Freq300_uid164_bh7_uid235_Out0(1);
   bh7_w32_10 <= Compressor_14_3_Freq300_uid164_bh7_uid235_Out0(2);
   Compressor_14_3_Freq300_uid164_uid235: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid235_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid235_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid235_Out0_copy236);
   Compressor_14_3_Freq300_uid164_bh7_uid235_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid235_Out0_copy236; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq300_uid160_bh7_uid237_In0 <= "" & bh7_w31_9 & bh7_w31_8 & bh7_w31_7;
   bh7_w31_11 <= Compressor_3_2_Freq300_uid160_bh7_uid237_Out0(0);
   bh7_w32_11 <= Compressor_3_2_Freq300_uid160_bh7_uid237_Out0(1);
   Compressor_3_2_Freq300_uid160_uid237: Compressor_3_2_Freq300_uid160
      port map ( X0 => Compressor_3_2_Freq300_uid160_bh7_uid237_In0,
                 R => Compressor_3_2_Freq300_uid160_bh7_uid237_Out0_copy238);
   Compressor_3_2_Freq300_uid160_bh7_uid237_Out0 <= Compressor_3_2_Freq300_uid160_bh7_uid237_Out0_copy238; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid239_In0 <= "" & bh7_w32_6 & bh7_w32_9 & bh7_w32_8 & bh7_w32_7;
   Compressor_14_3_Freq300_uid164_bh7_uid239_In1 <= "" & bh7_w33_6;
   bh7_w32_12 <= Compressor_14_3_Freq300_uid164_bh7_uid239_Out0(0);
   bh7_w33_10 <= Compressor_14_3_Freq300_uid164_bh7_uid239_Out0(1);
   bh7_w34_10 <= Compressor_14_3_Freq300_uid164_bh7_uid239_Out0(2);
   Compressor_14_3_Freq300_uid164_uid239: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid239_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid239_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid239_Out0_copy240);
   Compressor_14_3_Freq300_uid164_bh7_uid239_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid239_Out0_copy240; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq300_uid160_bh7_uid241_In0 <= "" & bh7_w33_9 & bh7_w33_8 & bh7_w33_7;
   bh7_w33_11 <= Compressor_3_2_Freq300_uid160_bh7_uid241_Out0(0);
   bh7_w34_11 <= Compressor_3_2_Freq300_uid160_bh7_uid241_Out0(1);
   Compressor_3_2_Freq300_uid160_uid241: Compressor_3_2_Freq300_uid160
      port map ( X0 => Compressor_3_2_Freq300_uid160_bh7_uid241_In0,
                 R => Compressor_3_2_Freq300_uid160_bh7_uid241_Out0_copy242);
   Compressor_3_2_Freq300_uid160_bh7_uid241_Out0 <= Compressor_3_2_Freq300_uid160_bh7_uid241_Out0_copy242; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid243_In0 <= "" & bh7_w34_6 & bh7_w34_9 & bh7_w34_8 & bh7_w34_7;
   Compressor_14_3_Freq300_uid164_bh7_uid243_In1 <= "" & bh7_w35_6;
   bh7_w34_12 <= Compressor_14_3_Freq300_uid164_bh7_uid243_Out0(0);
   bh7_w35_10 <= Compressor_14_3_Freq300_uid164_bh7_uid243_Out0(1);
   bh7_w36_10 <= Compressor_14_3_Freq300_uid164_bh7_uid243_Out0(2);
   Compressor_14_3_Freq300_uid164_uid243: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid243_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid243_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid243_Out0_copy244);
   Compressor_14_3_Freq300_uid164_bh7_uid243_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid243_Out0_copy244; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq300_uid160_bh7_uid245_In0 <= "" & bh7_w35_9 & bh7_w35_8 & bh7_w35_7;
   bh7_w35_11 <= Compressor_3_2_Freq300_uid160_bh7_uid245_Out0(0);
   bh7_w36_11 <= Compressor_3_2_Freq300_uid160_bh7_uid245_Out0(1);
   Compressor_3_2_Freq300_uid160_uid245: Compressor_3_2_Freq300_uid160
      port map ( X0 => Compressor_3_2_Freq300_uid160_bh7_uid245_In0,
                 R => Compressor_3_2_Freq300_uid160_bh7_uid245_Out0_copy246);
   Compressor_3_2_Freq300_uid160_bh7_uid245_Out0 <= Compressor_3_2_Freq300_uid160_bh7_uid245_Out0_copy246; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid247_In0 <= "" & bh7_w36_6 & bh7_w36_9 & bh7_w36_8 & bh7_w36_7;
   Compressor_14_3_Freq300_uid164_bh7_uid247_In1 <= "" & bh7_w37_6;
   bh7_w36_12 <= Compressor_14_3_Freq300_uid164_bh7_uid247_Out0(0);
   bh7_w37_10 <= Compressor_14_3_Freq300_uid164_bh7_uid247_Out0(1);
   bh7_w38_10 <= Compressor_14_3_Freq300_uid164_bh7_uid247_Out0(2);
   Compressor_14_3_Freq300_uid164_uid247: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid247_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid247_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid247_Out0_copy248);
   Compressor_14_3_Freq300_uid164_bh7_uid247_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid247_Out0_copy248; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq300_uid160_bh7_uid249_In0 <= "" & bh7_w37_9 & bh7_w37_8 & bh7_w37_7;
   bh7_w37_11 <= Compressor_3_2_Freq300_uid160_bh7_uid249_Out0(0);
   bh7_w38_11 <= Compressor_3_2_Freq300_uid160_bh7_uid249_Out0(1);
   Compressor_3_2_Freq300_uid160_uid249: Compressor_3_2_Freq300_uid160
      port map ( X0 => Compressor_3_2_Freq300_uid160_bh7_uid249_In0,
                 R => Compressor_3_2_Freq300_uid160_bh7_uid249_Out0_copy250);
   Compressor_3_2_Freq300_uid160_bh7_uid249_Out0 <= Compressor_3_2_Freq300_uid160_bh7_uid249_Out0_copy250; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid251_In0 <= "" & bh7_w38_6 & bh7_w38_9 & bh7_w38_8 & bh7_w38_7;
   Compressor_14_3_Freq300_uid164_bh7_uid251_In1 <= "" & bh7_w39_6;
   bh7_w38_12 <= Compressor_14_3_Freq300_uid164_bh7_uid251_Out0(0);
   bh7_w39_10 <= Compressor_14_3_Freq300_uid164_bh7_uid251_Out0(1);
   bh7_w40_10 <= Compressor_14_3_Freq300_uid164_bh7_uid251_Out0(2);
   Compressor_14_3_Freq300_uid164_uid251: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid251_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid251_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid251_Out0_copy252);
   Compressor_14_3_Freq300_uid164_bh7_uid251_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid251_Out0_copy252; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq300_uid160_bh7_uid253_In0 <= "" & bh7_w39_9 & bh7_w39_8 & bh7_w39_7;
   bh7_w39_11 <= Compressor_3_2_Freq300_uid160_bh7_uid253_Out0(0);
   bh7_w40_11 <= Compressor_3_2_Freq300_uid160_bh7_uid253_Out0(1);
   Compressor_3_2_Freq300_uid160_uid253: Compressor_3_2_Freq300_uid160
      port map ( X0 => Compressor_3_2_Freq300_uid160_bh7_uid253_In0,
                 R => Compressor_3_2_Freq300_uid160_bh7_uid253_Out0_copy254);
   Compressor_3_2_Freq300_uid160_bh7_uid253_Out0 <= Compressor_3_2_Freq300_uid160_bh7_uid253_Out0_copy254; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid255_In0 <= "" & bh7_w40_6 & bh7_w40_9 & bh7_w40_8 & bh7_w40_7;
   Compressor_14_3_Freq300_uid164_bh7_uid255_In1 <= "" & bh7_w41_4;
   bh7_w40_12 <= Compressor_14_3_Freq300_uid164_bh7_uid255_Out0(0);
   bh7_w41_8 <= Compressor_14_3_Freq300_uid164_bh7_uid255_Out0(1);
   bh7_w42_8 <= Compressor_14_3_Freq300_uid164_bh7_uid255_Out0(2);
   Compressor_14_3_Freq300_uid164_uid255: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid255_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid255_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid255_Out0_copy256);
   Compressor_14_3_Freq300_uid164_bh7_uid255_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid255_Out0_copy256; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid257_In0 <= "" & bh7_w41_7 & bh7_w41_6 & bh7_w41_5;
   Compressor_23_3_Freq300_uid156_bh7_uid257_In1 <= "" & bh7_w42_7 & bh7_w42_6;
   bh7_w41_9 <= Compressor_23_3_Freq300_uid156_bh7_uid257_Out0(0);
   bh7_w42_9 <= Compressor_23_3_Freq300_uid156_bh7_uid257_Out0(1);
   bh7_w43_7 <= Compressor_23_3_Freq300_uid156_bh7_uid257_Out0(2);
   Compressor_23_3_Freq300_uid156_uid257: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid257_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid257_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid257_Out0_copy258);
   Compressor_23_3_Freq300_uid156_bh7_uid257_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid257_Out0_copy258; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid259_In0 <= "" & bh7_w43_6 & bh7_w43_5 & bh7_w43_4;
   Compressor_23_3_Freq300_uid156_bh7_uid259_In1 <= "" & bh7_w44_2 & bh7_w44_4;
   bh7_w43_8 <= Compressor_23_3_Freq300_uid156_bh7_uid259_Out0(0);
   bh7_w44_5 <= Compressor_23_3_Freq300_uid156_bh7_uid259_Out0(1);
   bh7_w45_4 <= Compressor_23_3_Freq300_uid156_bh7_uid259_Out0(2);
   Compressor_23_3_Freq300_uid156_uid259: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid259_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid259_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid259_Out0_copy260);
   Compressor_23_3_Freq300_uid156_bh7_uid259_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid259_Out0_copy260; -- output copy to hold a pipeline register if needed


   Compressor_14_3_Freq300_uid164_bh7_uid261_In0 <= "" & bh7_w45_3 & bh7_w45_2 & "0" & "0";
   Compressor_14_3_Freq300_uid164_bh7_uid261_In1 <= "" & bh7_w46_2;
   bh7_w45_5 <= Compressor_14_3_Freq300_uid164_bh7_uid261_Out0(0);
   bh7_w46_3 <= Compressor_14_3_Freq300_uid164_bh7_uid261_Out0(1);
   bh7_w47_1 <= Compressor_14_3_Freq300_uid164_bh7_uid261_Out0(2);
   Compressor_14_3_Freq300_uid164_uid261: Compressor_14_3_Freq300_uid164
      port map ( X0 => Compressor_14_3_Freq300_uid164_bh7_uid261_In0,
                 X1 => Compressor_14_3_Freq300_uid164_bh7_uid261_In1,
                 R => Compressor_14_3_Freq300_uid164_bh7_uid261_Out0_copy262);
   Compressor_14_3_Freq300_uid164_bh7_uid261_Out0 <= Compressor_14_3_Freq300_uid164_bh7_uid261_Out0_copy262; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid263_In0 <= "" & bh7_w21_8 & bh7_w21_7 & "0";
   Compressor_23_3_Freq300_uid156_bh7_uid263_In1 <= "" & bh7_w22_9 & bh7_w22_8;
   bh7_w21_9 <= Compressor_23_3_Freq300_uid156_bh7_uid263_Out0(0);
   bh7_w22_10 <= Compressor_23_3_Freq300_uid156_bh7_uid263_Out0(1);
   bh7_w23_11 <= Compressor_23_3_Freq300_uid156_bh7_uid263_Out0(2);
   Compressor_23_3_Freq300_uid156_uid263: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid263_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid263_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid263_Out0_copy264);
   Compressor_23_3_Freq300_uid156_bh7_uid263_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid263_Out0_copy264; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid265_In0 <= "" & bh7_w23_7 & bh7_w23_10 & "0";
   Compressor_23_3_Freq300_uid156_bh7_uid265_In1 <= "" & bh7_w24_11 & bh7_w24_10;
   bh7_w23_12 <= Compressor_23_3_Freq300_uid156_bh7_uid265_Out0(0);
   bh7_w24_12 <= Compressor_23_3_Freq300_uid156_bh7_uid265_Out0(1);
   bh7_w25_12 <= Compressor_23_3_Freq300_uid156_bh7_uid265_Out0(2);
   Compressor_23_3_Freq300_uid156_uid265: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid265_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid265_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid265_Out0_copy266);
   Compressor_23_3_Freq300_uid156_bh7_uid265_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid265_Out0_copy266; -- output copy to hold a pipeline register if needed


   Compressor_3_2_Freq300_uid160_bh7_uid267_In0 <= "" & bh7_w25_11 & bh7_w25_10 & "0";
   bh7_w25_13 <= Compressor_3_2_Freq300_uid160_bh7_uid267_Out0(0);
   bh7_w26_13 <= Compressor_3_2_Freq300_uid160_bh7_uid267_Out0(1);
   Compressor_3_2_Freq300_uid160_uid267: Compressor_3_2_Freq300_uid160
      port map ( X0 => Compressor_3_2_Freq300_uid160_bh7_uid267_In0,
                 R => Compressor_3_2_Freq300_uid160_bh7_uid267_Out0_copy268);
   Compressor_3_2_Freq300_uid160_bh7_uid267_Out0 <= Compressor_3_2_Freq300_uid160_bh7_uid267_Out0_copy268; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid269_In0 <= "" & bh7_w26_12 & bh7_w26_11 & bh7_w26_10;
   Compressor_23_3_Freq300_uid156_bh7_uid269_In1 <= "" & bh7_w27_11 & bh7_w27_10;
   bh7_w26_14 <= Compressor_23_3_Freq300_uid156_bh7_uid269_Out0(0);
   bh7_w27_12 <= Compressor_23_3_Freq300_uid156_bh7_uid269_Out0(1);
   bh7_w28_13 <= Compressor_23_3_Freq300_uid156_bh7_uid269_Out0(2);
   Compressor_23_3_Freq300_uid156_uid269: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid269_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid269_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid269_Out0_copy270);
   Compressor_23_3_Freq300_uid156_bh7_uid269_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid269_Out0_copy270; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid271_In0 <= "" & bh7_w28_12 & bh7_w28_11 & bh7_w28_10;
   Compressor_23_3_Freq300_uid156_bh7_uid271_In1 <= "" & bh7_w29_11 & bh7_w29_10;
   bh7_w28_14 <= Compressor_23_3_Freq300_uid156_bh7_uid271_Out0(0);
   bh7_w29_12 <= Compressor_23_3_Freq300_uid156_bh7_uid271_Out0(1);
   bh7_w30_13 <= Compressor_23_3_Freq300_uid156_bh7_uid271_Out0(2);
   Compressor_23_3_Freq300_uid156_uid271: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid271_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid271_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid271_Out0_copy272);
   Compressor_23_3_Freq300_uid156_bh7_uid271_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid271_Out0_copy272; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid273_In0 <= "" & bh7_w30_12 & bh7_w30_11 & bh7_w30_10;
   Compressor_23_3_Freq300_uid156_bh7_uid273_In1 <= "" & bh7_w31_11 & bh7_w31_10;
   bh7_w30_14 <= Compressor_23_3_Freq300_uid156_bh7_uid273_Out0(0);
   bh7_w31_12 <= Compressor_23_3_Freq300_uid156_bh7_uid273_Out0(1);
   bh7_w32_13 <= Compressor_23_3_Freq300_uid156_bh7_uid273_Out0(2);
   Compressor_23_3_Freq300_uid156_uid273: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid273_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid273_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid273_Out0_copy274);
   Compressor_23_3_Freq300_uid156_bh7_uid273_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid273_Out0_copy274; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid275_In0 <= "" & bh7_w32_12 & bh7_w32_11 & bh7_w32_10;
   Compressor_23_3_Freq300_uid156_bh7_uid275_In1 <= "" & bh7_w33_11 & bh7_w33_10;
   bh7_w32_14 <= Compressor_23_3_Freq300_uid156_bh7_uid275_Out0(0);
   bh7_w33_12 <= Compressor_23_3_Freq300_uid156_bh7_uid275_Out0(1);
   bh7_w34_13 <= Compressor_23_3_Freq300_uid156_bh7_uid275_Out0(2);
   Compressor_23_3_Freq300_uid156_uid275: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid275_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid275_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid275_Out0_copy276);
   Compressor_23_3_Freq300_uid156_bh7_uid275_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid275_Out0_copy276; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid277_In0 <= "" & bh7_w34_12 & bh7_w34_11 & bh7_w34_10;
   Compressor_23_3_Freq300_uid156_bh7_uid277_In1 <= "" & bh7_w35_11 & bh7_w35_10;
   bh7_w34_14 <= Compressor_23_3_Freq300_uid156_bh7_uid277_Out0(0);
   bh7_w35_12 <= Compressor_23_3_Freq300_uid156_bh7_uid277_Out0(1);
   bh7_w36_13 <= Compressor_23_3_Freq300_uid156_bh7_uid277_Out0(2);
   Compressor_23_3_Freq300_uid156_uid277: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid277_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid277_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid277_Out0_copy278);
   Compressor_23_3_Freq300_uid156_bh7_uid277_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid277_Out0_copy278; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid279_In0 <= "" & bh7_w36_12 & bh7_w36_11 & bh7_w36_10;
   Compressor_23_3_Freq300_uid156_bh7_uid279_In1 <= "" & bh7_w37_11 & bh7_w37_10;
   bh7_w36_14 <= Compressor_23_3_Freq300_uid156_bh7_uid279_Out0(0);
   bh7_w37_12 <= Compressor_23_3_Freq300_uid156_bh7_uid279_Out0(1);
   bh7_w38_13 <= Compressor_23_3_Freq300_uid156_bh7_uid279_Out0(2);
   Compressor_23_3_Freq300_uid156_uid279: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid279_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid279_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid279_Out0_copy280);
   Compressor_23_3_Freq300_uid156_bh7_uid279_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid279_Out0_copy280; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid281_In0 <= "" & bh7_w38_12 & bh7_w38_11 & bh7_w38_10;
   Compressor_23_3_Freq300_uid156_bh7_uid281_In1 <= "" & bh7_w39_11 & bh7_w39_10;
   bh7_w38_14 <= Compressor_23_3_Freq300_uid156_bh7_uid281_Out0(0);
   bh7_w39_12 <= Compressor_23_3_Freq300_uid156_bh7_uid281_Out0(1);
   bh7_w40_13 <= Compressor_23_3_Freq300_uid156_bh7_uid281_Out0(2);
   Compressor_23_3_Freq300_uid156_uid281: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid281_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid281_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid281_Out0_copy282);
   Compressor_23_3_Freq300_uid156_bh7_uid281_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid281_Out0_copy282; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid283_In0 <= "" & bh7_w40_12 & bh7_w40_11 & bh7_w40_10;
   Compressor_23_3_Freq300_uid156_bh7_uid283_In1 <= "" & bh7_w41_9 & bh7_w41_8;
   bh7_w40_14 <= Compressor_23_3_Freq300_uid156_bh7_uid283_Out0(0);
   bh7_w41_10 <= Compressor_23_3_Freq300_uid156_bh7_uid283_Out0(1);
   bh7_w42_10 <= Compressor_23_3_Freq300_uid156_bh7_uid283_Out0(2);
   Compressor_23_3_Freq300_uid156_uid283: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid283_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid283_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid283_Out0_copy284);
   Compressor_23_3_Freq300_uid156_bh7_uid283_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid283_Out0_copy284; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid285_In0 <= "" & bh7_w42_5 & bh7_w42_9 & bh7_w42_8;
   Compressor_23_3_Freq300_uid156_bh7_uid285_In1 <= "" & bh7_w43_8 & bh7_w43_7;
   bh7_w42_11 <= Compressor_23_3_Freq300_uid156_bh7_uid285_Out0(0);
   bh7_w43_9 <= Compressor_23_3_Freq300_uid156_bh7_uid285_Out0(1);
   bh7_w44_6 <= Compressor_23_3_Freq300_uid156_bh7_uid285_Out0(2);
   Compressor_23_3_Freq300_uid156_uid285: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid285_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid285_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid285_Out0_copy286);
   Compressor_23_3_Freq300_uid156_bh7_uid285_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid285_Out0_copy286; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid287_In0 <= "" & bh7_w44_3 & bh7_w44_5 & "0";
   Compressor_23_3_Freq300_uid156_bh7_uid287_In1 <= "" & bh7_w45_5 & bh7_w45_4;
   bh7_w44_7 <= Compressor_23_3_Freq300_uid156_bh7_uid287_Out0(0);
   bh7_w45_6 <= Compressor_23_3_Freq300_uid156_bh7_uid287_Out0(1);
   bh7_w46_4 <= Compressor_23_3_Freq300_uid156_bh7_uid287_Out0(2);
   Compressor_23_3_Freq300_uid156_uid287: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid287_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid287_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid287_Out0_copy288);
   Compressor_23_3_Freq300_uid156_bh7_uid287_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid287_Out0_copy288; -- output copy to hold a pipeline register if needed


   Compressor_23_3_Freq300_uid156_bh7_uid289_In0 <= "" & bh7_w46_3 & "0" & "0";
   Compressor_23_3_Freq300_uid156_bh7_uid289_In1 <= "" & bh7_w47_0 & bh7_w47_1;
   bh7_w46_5 <= Compressor_23_3_Freq300_uid156_bh7_uid289_Out0(0);
   bh7_w47_2 <= Compressor_23_3_Freq300_uid156_bh7_uid289_Out0(1);
   Compressor_23_3_Freq300_uid156_uid289: Compressor_23_3_Freq300_uid156
      port map ( X0 => Compressor_23_3_Freq300_uid156_bh7_uid289_In0,
                 X1 => Compressor_23_3_Freq300_uid156_bh7_uid289_In1,
                 R => Compressor_23_3_Freq300_uid156_bh7_uid289_Out0_copy290);
   Compressor_23_3_Freq300_uid156_bh7_uid289_Out0 <= Compressor_23_3_Freq300_uid156_bh7_uid289_Out0_copy290; -- output copy to hold a pipeline register if needed

   tmp_bitheapResult_bh7_22 <= bh7_w22_10 & bh7_w21_9 & bh7_w20_6 & bh7_w19_5 & bh7_w18_2 & bh7_w17_2 & bh7_w16_0 & bh7_w15_0 & bh7_w14_0 & bh7_w13_0 & bh7_w12_0 & bh7_w11_0 & bh7_w10_0 & bh7_w9_0 & bh7_w8_0 & bh7_w7_0 & bh7_w6_0 & bh7_w5_0 & bh7_w4_0 & bh7_w3_0 & bh7_w2_0 & bh7_w1_0 & bh7_w0_0;

   bitheapFinalAdd_bh7_In0 <= "0" & bh7_w47_2 & bh7_w46_5 & bh7_w45_6 & bh7_w44_7 & bh7_w43_9 & bh7_w42_11 & bh7_w41_10 & bh7_w40_14 & bh7_w39_12 & bh7_w38_14 & bh7_w37_12 & bh7_w36_14 & bh7_w35_12 & bh7_w34_14 & bh7_w33_12 & bh7_w32_14 & bh7_w31_12 & bh7_w30_14 & bh7_w29_12 & bh7_w28_14 & bh7_w27_12 & bh7_w26_14 & bh7_w25_13 & bh7_w24_12 & bh7_w23_12;
   bitheapFinalAdd_bh7_In1 <= "0" & "0" & bh7_w46_4 & "0" & bh7_w44_6 & "0" & bh7_w42_10 & "0" & bh7_w40_13 & "0" & bh7_w38_13 & "0" & bh7_w36_13 & "0" & bh7_w34_13 & "0" & bh7_w32_13 & "0" & bh7_w30_13 & "0" & bh7_w28_13 & "0" & bh7_w26_13 & bh7_w25_12 & "0" & bh7_w23_11;
   bitheapFinalAdd_bh7_Cin <= '0';

   bitheapFinalAdd_bh7: IntAdder_26_Freq300_uid292
      port map ( clk  => clk,
                 Cin => bitheapFinalAdd_bh7_Cin,
                 X => bitheapFinalAdd_bh7_In0,
                 Y => bitheapFinalAdd_bh7_In1,
                 R => bitheapFinalAdd_bh7_Out);
   bitheapResult_bh7 <= bitheapFinalAdd_bh7_Out(24 downto 0) & tmp_bitheapResult_bh7_22_d1;
   R <= bitheapResult_bh7(47 downto 0);
end architecture;

--------------------------------------------------------------------------------
--                         IntAdder_33_Freq300_uid295
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 2 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y Cin
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_33_Freq300_uid295 is
    port (clk : in std_logic;
          X : in  std_logic_vector(32 downto 0);
          Y : in  std_logic_vector(32 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(32 downto 0)   );
end entity;

architecture arch of IntAdder_33_Freq300_uid295 is
signal Rtmp :  std_logic_vector(32 downto 0);
signal X_d1 :  std_logic_vector(32 downto 0);
signal Y_d1, Y_d2 :  std_logic_vector(32 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            X_d1 <=  X;
            Y_d1 <=  Y;
            Y_d2 <=  Y_d1;
         end if;
      end process;
   Rtmp <= X_d1 + Y_d2 + Cin;
   R <= Rtmp;
end architecture;

--------------------------------------------------------------------------------
--                                 multFloat
--                      (FPMult_8_23_uid2_Freq300_uid3)
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin 2008-2021
--------------------------------------------------------------------------------
-- Pipeline depth: 2 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X Y
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity multFloat is
    port (clk : in std_logic;
          X : in  std_logic_vector(8+23+2 downto 0);
          Y : in  std_logic_vector(8+23+2 downto 0);
          R : out  std_logic_vector(8+23+2 downto 0)   );
end entity;

architecture arch of multFloat is
   component IntMultiplier_24x24_48_Freq300_uid5 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(23 downto 0);
             Y : in  std_logic_vector(23 downto 0);
             R : out  std_logic_vector(47 downto 0)   );
   end component;

   component IntAdder_33_Freq300_uid295 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(32 downto 0);
             Y : in  std_logic_vector(32 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(32 downto 0)   );
   end component;

signal sign, sign_d1, sign_d2 :  std_logic;
signal expX :  std_logic_vector(7 downto 0);
signal expY :  std_logic_vector(7 downto 0);
signal expSumPreSub :  std_logic_vector(9 downto 0);
signal bias :  std_logic_vector(9 downto 0);
signal expSum, expSum_d1 :  std_logic_vector(9 downto 0);
signal sigX :  std_logic_vector(23 downto 0);
signal sigY :  std_logic_vector(23 downto 0);
signal sigProd :  std_logic_vector(47 downto 0);
signal excSel :  std_logic_vector(3 downto 0);
signal exc, exc_d1, exc_d2 :  std_logic_vector(1 downto 0);
signal norm :  std_logic;
signal expPostNorm :  std_logic_vector(9 downto 0);
signal sigProdExt, sigProdExt_d1 :  std_logic_vector(47 downto 0);
signal expSig :  std_logic_vector(32 downto 0);
signal sticky, sticky_d1 :  std_logic;
signal guard :  std_logic;
signal round :  std_logic;
signal expSigPostRound :  std_logic_vector(32 downto 0);
signal excPostNorm :  std_logic_vector(1 downto 0);
signal finalExc :  std_logic_vector(1 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            sign_d1 <=  sign;
            sign_d2 <=  sign_d1;
            expSum_d1 <=  expSum;
            exc_d1 <=  exc;
            exc_d2 <=  exc_d1;
            sigProdExt_d1 <=  sigProdExt;
            sticky_d1 <=  sticky;
         end if;
      end process;
   sign <= X(31) xor Y(31);
   expX <= X(30 downto 23);
   expY <= Y(30 downto 23);
   expSumPreSub <= ("00" & expX) + ("00" & expY);
   bias <= CONV_STD_LOGIC_VECTOR(127,10);
   expSum <= expSumPreSub - bias;
   sigX <= "1" & X(22 downto 0);
   sigY <= "1" & Y(22 downto 0);
   SignificandMultiplication: IntMultiplier_24x24_48_Freq300_uid5
      port map ( clk  => clk,
                 X => sigX,
                 Y => sigY,
                 R => sigProd);
   excSel <= X(33 downto 32) & Y(33 downto 32);
   with excSel  select  
   exc <= "00" when  "0000" | "0001" | "0100", 
          "01" when "0101",
          "10" when "0110" | "1001" | "1010" ,
          "11" when others;
   norm <= sigProd(47);
   -- exponent update
   expPostNorm <= expSum_d1 + ("000000000" & norm);
   -- significand normalization shift
   sigProdExt <= sigProd(46 downto 0) & "0" when norm='1' else
                         sigProd(45 downto 0) & "00";
   expSig <= expPostNorm & sigProdExt(47 downto 25);
   sticky <= sigProdExt(24);
   guard <= '0' when sigProdExt_d1(23 downto 0)="000000000000000000000000" else '1';
   round <= sticky_d1 and ( (guard and not(sigProdExt_d1(25))) or (sigProdExt_d1(25) ))  ;
   RoundingAdder: IntAdder_33_Freq300_uid295
      port map ( clk  => clk,
                 Cin => round,
                 X => expSig,
                 Y => "000000000000000000000000000000000",
                 R => expSigPostRound);
   with expSigPostRound(32 downto 31)  select 
   excPostNorm <=  "01"  when  "00",
                               "10"             when "01", 
                               "00"             when "11"|"10",
                               "11"             when others;
   with exc_d2  select  
   finalExc <= exc_d2 when  "11"|"10"|"00",
                       excPostNorm when others; 
   R <= finalExc & sign_d2 & expSigPostRound(30 downto 0);
end architecture;


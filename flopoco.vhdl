--------------------------------------------------------------------------------
--                RightShifterSticky26_by_max_25_Freq300_uid4
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca (2008-2011), Florent de Dinechin (2008-2019)
--------------------------------------------------------------------------------
-- Pipeline depth: 3 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X S
-- Output signals: R Sticky

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity RightShifterSticky26_by_max_25_Freq300_uid4 is
    port (clk : in std_logic;
          X : in  std_logic_vector(25 downto 0);
          S : in  std_logic_vector(4 downto 0);
          R : out  std_logic_vector(25 downto 0);
          Sticky : out  std_logic   );
end entity;

architecture arch of RightShifterSticky26_by_max_25_Freq300_uid4 is
signal ps, ps_d1, ps_d2 :  std_logic_vector(4 downto 0);
signal Xpadded :  std_logic_vector(25 downto 0);
signal level5, level5_d1 :  std_logic_vector(25 downto 0);
signal stk4, stk4_d1 :  std_logic;
signal level4, level4_d1 :  std_logic_vector(25 downto 0);
signal stk3 :  std_logic;
signal level3, level3_d1 :  std_logic_vector(25 downto 0);
signal stk2 :  std_logic;
signal level2, level2_d1 :  std_logic_vector(25 downto 0);
signal stk1, stk1_d1 :  std_logic;
signal level1, level1_d1, level1_d2 :  std_logic_vector(25 downto 0);
signal stk0 :  std_logic;
signal level0 :  std_logic_vector(25 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            ps_d1 <=  ps;
            ps_d2 <=  ps_d1;
            level5_d1 <=  level5;
            stk4_d1 <=  stk4;
            level4_d1 <=  level4;
            level3_d1 <=  level3;
            level2_d1 <=  level2;
            stk1_d1 <=  stk1;
            level1_d1 <=  level1;
            level1_d2 <=  level1_d1;
         end if;
      end process;
   ps<= S;
   Xpadded <= X;
   level5<= Xpadded;
   stk4 <= '1' when (level5_d1(15 downto 0)/="0000000000000000" and ps(4)='1')   else '0';
   level4 <=  level5_d1 when  ps(4)='0'    else (15 downto 0 => '0') & level5_d1(25 downto 16);
   stk3 <= '1' when (level4_d1(7 downto 0)/="00000000" and ps_d1(3)='1') or stk4_d1 ='1'   else '0';
   level3 <=  level4 when  ps(3)='0'    else (7 downto 0 => '0') & level4(25 downto 8);
   stk2 <= '1' when (level3_d1(3 downto 0)/="0000" and ps_d1(2)='1') or stk3 ='1'   else '0';
   level2 <=  level3 when  ps(2)='0'    else (3 downto 0 => '0') & level3(25 downto 4);
   stk1 <= '1' when (level2_d1(1 downto 0)/="00" and ps_d1(1)='1') or stk2 ='1'   else '0';
   level1 <=  level2 when  ps(1)='0'    else (1 downto 0 => '0') & level2(25 downto 2);
   stk0 <= '1' when (level1_d2(0 downto 0)/="0" and ps_d2(0)='1') or stk1_d1 ='1'   else '0';
   level0 <=  level1 when  ps(0)='0'    else (0 downto 0 => '0') & level1(25 downto 1);
   R <= level0;
   Sticky <= stk0;
end architecture;

--------------------------------------------------------------------------------
--                          IntAdder_27_Freq300_uid6
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 4 cycles
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

entity IntAdder_27_Freq300_uid6 is
    port (clk : in std_logic;
          X : in  std_logic_vector(26 downto 0);
          Y : in  std_logic_vector(26 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(26 downto 0)   );
end entity;

architecture arch of IntAdder_27_Freq300_uid6 is
signal Cin_0, Cin_0_d1 :  std_logic;
signal X_0, X_0_d1, X_0_d2, X_0_d3, X_0_d4 :  std_logic_vector(19 downto 0);
signal Y_0, Y_0_d1, Y_0_d2, Y_0_d3 :  std_logic_vector(19 downto 0);
signal S_0 :  std_logic_vector(19 downto 0);
signal R_0 :  std_logic_vector(18 downto 0);
signal Cin_1 :  std_logic;
signal X_1, X_1_d1, X_1_d2, X_1_d3, X_1_d4 :  std_logic_vector(8 downto 0);
signal Y_1, Y_1_d1, Y_1_d2, Y_1_d3 :  std_logic_vector(8 downto 0);
signal S_1 :  std_logic_vector(8 downto 0);
signal R_1 :  std_logic_vector(7 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            Cin_0_d1 <=  Cin_0;
            X_0_d1 <=  X_0;
            X_0_d2 <=  X_0_d1;
            X_0_d3 <=  X_0_d2;
            X_0_d4 <=  X_0_d3;
            Y_0_d1 <=  Y_0;
            Y_0_d2 <=  Y_0_d1;
            Y_0_d3 <=  Y_0_d2;
            X_1_d1 <=  X_1;
            X_1_d2 <=  X_1_d1;
            X_1_d3 <=  X_1_d2;
            X_1_d4 <=  X_1_d3;
            Y_1_d1 <=  Y_1;
            Y_1_d2 <=  Y_1_d1;
            Y_1_d3 <=  Y_1_d2;
         end if;
      end process;
   Cin_0 <= Cin;
   X_0 <= '0' & X(18 downto 0);
   Y_0 <= '0' & Y(18 downto 0);
   S_0 <= X_0_d4 + Y_0_d3 + Cin_0_d1;
   R_0 <= S_0(18 downto 0);
   Cin_1 <= S_0(19);
   X_1 <= '0' & X(26 downto 19);
   Y_1 <= '0' & Y(26 downto 19);
   S_1 <= X_1_d4 + Y_1_d3 + Cin_1;
   R_1 <= S_1(7 downto 0);
   R <= R_1 & R_0 ;
end architecture;

--------------------------------------------------------------------------------
--                            LZC_26_Freq300_uid8
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007)
--------------------------------------------------------------------------------
-- Pipeline depth: 2 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: I
-- Output signals: O

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity LZC_26_Freq300_uid8 is
    port (clk : in std_logic;
          I : in  std_logic_vector(25 downto 0);
          O : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of LZC_26_Freq300_uid8 is
signal level5 :  std_logic_vector(30 downto 0);
signal digit4, digit4_d1 :  std_logic;
signal level4, level4_d1 :  std_logic_vector(14 downto 0);
signal digit3 :  std_logic;
signal level3 :  std_logic_vector(6 downto 0);
signal digit2 :  std_logic;
signal level2, level2_d1 :  std_logic_vector(2 downto 0);
signal lowBits :  std_logic_vector(1 downto 0);
signal outHighBits, outHighBits_d1 :  std_logic_vector(2 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            digit4_d1 <=  digit4;
            level4_d1 <=  level4;
            level2_d1 <=  level2;
            outHighBits_d1 <=  outHighBits;
         end if;
      end process;
   -- pad input to the next power of two minus 1
   level5 <= I & "11111";
   -- Main iteration for large inputs
   digit4<= '1' when level5(30 downto 15) = "0000000000000000" else '0';
   level4<= level5(14 downto 0) when digit4='1' else level5(30 downto 16);
   digit3<= '1' when level4_d1(14 downto 7) = "00000000" else '0';
   level3<= level4_d1(6 downto 0) when digit3='1' else level4_d1(14 downto 8);
   digit2<= '1' when level3(6 downto 3) = "0000" else '0';
   level2<= level3(2 downto 0) when digit2='1' else level3(6 downto 4);
   -- Finish counting with one LUT
   with level2_d1  select  lowBits <= 
      "11" when "000",
      "10" when "001",
      "01" when "010",
      "01" when "011",
      "00" when others;
   outHighBits <= digit4_d1 & digit3 & digit2 & "";
   O <= outHighBits_d1 & lowBits ;
end architecture;

--------------------------------------------------------------------------------
--                   LeftShifter27_by_max_26_Freq300_uid10
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca (2008-2011), Florent de Dinechin (2008-2019)
--------------------------------------------------------------------------------
-- Pipeline depth: 3 cycles
-- Clock period (ns): 3.33333
-- Target frequency (MHz): 300
-- Input signals: X S
-- Output signals: R

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity LeftShifter27_by_max_26_Freq300_uid10 is
    port (clk : in std_logic;
          X : in  std_logic_vector(26 downto 0);
          S : in  std_logic_vector(4 downto 0);
          R : out  std_logic_vector(52 downto 0)   );
end entity;

architecture arch of LeftShifter27_by_max_26_Freq300_uid10 is
signal ps, ps_d1 :  std_logic_vector(4 downto 0);
signal level0, level0_d1, level0_d2 :  std_logic_vector(26 downto 0);
signal level1, level1_d1 :  std_logic_vector(27 downto 0);
signal level2 :  std_logic_vector(29 downto 0);
signal level3 :  std_logic_vector(33 downto 0);
signal level4 :  std_logic_vector(41 downto 0);
signal level5 :  std_logic_vector(57 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            ps_d1 <=  ps;
            level0_d1 <=  level0;
            level0_d2 <=  level0_d1;
            level1_d1 <=  level1;
         end if;
      end process;
   ps<= S;
   level0<= X;
   level1<= level0_d2 & (0 downto 0 => '0') when ps(0)= '1' else     (0 downto 0 => '0') & level0_d2;
   level2<= level1_d1 & (1 downto 0 => '0') when ps_d1(1)= '1' else     (1 downto 0 => '0') & level1_d1;
   level3<= level2 & (3 downto 0 => '0') when ps_d1(2)= '1' else     (3 downto 0 => '0') & level2;
   level4<= level3 & (7 downto 0 => '0') when ps_d1(3)= '1' else     (7 downto 0 => '0') & level3;
   level5<= level4 & (15 downto 0 => '0') when ps_d1(4)= '1' else     (15 downto 0 => '0') & level4;
   R <= level5(52 downto 0);
end architecture;

--------------------------------------------------------------------------------
--                         IntAdder_31_Freq300_uid13
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 9 cycles
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

entity IntAdder_31_Freq300_uid13 is
    port (clk : in std_logic;
          X : in  std_logic_vector(30 downto 0);
          Y : in  std_logic_vector(30 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(30 downto 0)   );
end entity;

architecture arch of IntAdder_31_Freq300_uid13 is
signal Rtmp :  std_logic_vector(30 downto 0);
signal X_d1 :  std_logic_vector(30 downto 0);
signal Y_d1, Y_d2, Y_d3, Y_d4, Y_d5, Y_d6, Y_d7, Y_d8, Y_d9 :  std_logic_vector(30 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            X_d1 <=  X;
            Y_d1 <=  Y;
            Y_d2 <=  Y_d1;
            Y_d3 <=  Y_d2;
            Y_d4 <=  Y_d3;
            Y_d5 <=  Y_d4;
            Y_d6 <=  Y_d5;
            Y_d7 <=  Y_d6;
            Y_d8 <=  Y_d7;
            Y_d9 <=  Y_d8;
         end if;
      end process;
   Rtmp <= X_d1 + Y_d9 + Cin;
   R <= Rtmp;
end architecture;

--------------------------------------------------------------------------------
--                                 plusFloat
--                       (IEEEFPAdd_8_23_Freq300_uid2)
-- VHDL generated for Zynq7000 @ 300MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Valentin Huguet (2016)
--------------------------------------------------------------------------------
-- Pipeline depth: 10 cycles
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

entity plusFloat is
    port (clk : in std_logic;
          X : in  std_logic_vector(31 downto 0);
          Y : in  std_logic_vector(31 downto 0);
          R : out  std_logic_vector(31 downto 0)   );
end entity;

architecture arch of plusFloat is
   component RightShifterSticky26_by_max_25_Freq300_uid4 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(25 downto 0);
             S : in  std_logic_vector(4 downto 0);
             R : out  std_logic_vector(25 downto 0);
             Sticky : out  std_logic   );
   end component;

   component IntAdder_27_Freq300_uid6 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(26 downto 0);
             Y : in  std_logic_vector(26 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(26 downto 0)   );
   end component;

   component LZC_26_Freq300_uid8 is
      port ( clk : in std_logic;
             I : in  std_logic_vector(25 downto 0);
             O : out  std_logic_vector(4 downto 0)   );
   end component;

   component LeftShifter27_by_max_26_Freq300_uid10 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(26 downto 0);
             S : in  std_logic_vector(4 downto 0);
             R : out  std_logic_vector(52 downto 0)   );
   end component;

   component IntAdder_31_Freq300_uid13 is
      port ( clk : in std_logic;
             X : in  std_logic_vector(30 downto 0);
             Y : in  std_logic_vector(30 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(30 downto 0)   );
   end component;

signal expFracX :  std_logic_vector(30 downto 0);
signal expFracY :  std_logic_vector(30 downto 0);
signal expXmExpY :  std_logic_vector(8 downto 0);
signal expYmExpX :  std_logic_vector(8 downto 0);
signal swap :  std_logic;
signal newX, newX_d1, newX_d2, newX_d3, newX_d4 :  std_logic_vector(31 downto 0);
signal newY, newY_d1, newY_d2, newY_d3, newY_d4 :  std_logic_vector(31 downto 0);
signal expDiff, expDiff_d1 :  std_logic_vector(8 downto 0);
signal expNewX, expNewX_d1, expNewX_d2, expNewX_d3, expNewX_d4, expNewX_d5, expNewX_d6, expNewX_d7 :  std_logic_vector(7 downto 0);
signal expNewY, expNewY_d1 :  std_logic_vector(7 downto 0);
signal signNewX, signNewX_d1, signNewX_d2, signNewX_d3, signNewX_d4, signNewX_d5, signNewX_d6, signNewX_d7, signNewX_d8, signNewX_d9, signNewX_d10 :  std_logic;
signal signNewY, signNewY_d1, signNewY_d2, signNewY_d3, signNewY_d4, signNewY_d5, signNewY_d6, signNewY_d7, signNewY_d8, signNewY_d9, signNewY_d10 :  std_logic;
signal EffSub, EffSub_d1, EffSub_d2, EffSub_d3, EffSub_d4, EffSub_d5, EffSub_d6, EffSub_d7, EffSub_d8, EffSub_d9 :  std_logic;
signal xExpFieldZero, xExpFieldZero_d1, xExpFieldZero_d2, xExpFieldZero_d3, xExpFieldZero_d4, xExpFieldZero_d5, xExpFieldZero_d6 :  std_logic;
signal yExpFieldZero, yExpFieldZero_d1, yExpFieldZero_d2, yExpFieldZero_d3 :  std_logic;
signal xExpFieldAllOnes, xExpFieldAllOnes_d1, xExpFieldAllOnes_d2, xExpFieldAllOnes_d3 :  std_logic;
signal yExpFieldAllOnes, yExpFieldAllOnes_d1, yExpFieldAllOnes_d2, yExpFieldAllOnes_d3 :  std_logic;
signal xSigFieldZero :  std_logic;
signal ySigFieldZero :  std_logic;
signal xIsNaN, xIsNaN_d1 :  std_logic;
signal yIsNaN, yIsNaN_d1 :  std_logic;
signal xIsInfinity, xIsInfinity_d1, xIsInfinity_d2, xIsInfinity_d3, xIsInfinity_d4, xIsInfinity_d5, xIsInfinity_d6 :  std_logic;
signal yIsInfinity, yIsInfinity_d1, yIsInfinity_d2, yIsInfinity_d3, yIsInfinity_d4, yIsInfinity_d5, yIsInfinity_d6 :  std_logic;
signal xIsZero, xIsZero_d1, xIsZero_d2, xIsZero_d3, xIsZero_d4, xIsZero_d5, xIsZero_d6 :  std_logic;
signal yIsZero, yIsZero_d1, yIsZero_d2, yIsZero_d3, yIsZero_d4, yIsZero_d5, yIsZero_d6 :  std_logic;
signal bothSubNormals :  std_logic;
signal resultIsNaN, resultIsNaN_d1, resultIsNaN_d2, resultIsNaN_d3, resultIsNaN_d4, resultIsNaN_d5 :  std_logic;
signal significandNewX :  std_logic_vector(23 downto 0);
signal significandNewY :  std_logic_vector(23 downto 0);
signal allShiftedOut :  std_logic;
signal rightShiftValue, rightShiftValue_d1 :  std_logic_vector(4 downto 0);
signal shiftCorrection, shiftCorrection_d1 :  std_logic;
signal finalRightShiftValue :  std_logic_vector(4 downto 0);
signal significandY00 :  std_logic_vector(25 downto 0);
signal shiftedSignificandY :  std_logic_vector(25 downto 0);
signal stickyLow, stickyLow_d1, stickyLow_d2, stickyLow_d3, stickyLow_d4 :  std_logic;
signal summandY :  std_logic_vector(26 downto 0);
signal summandX :  std_logic_vector(26 downto 0);
signal carryIn :  std_logic;
signal significandZ :  std_logic_vector(26 downto 0);
signal z1, z1_d1, z1_d2 :  std_logic;
signal z0, z0_d1, z0_d2 :  std_logic;
signal lzcZInput :  std_logic_vector(25 downto 0);
signal lzc :  std_logic_vector(4 downto 0);
signal leftShiftVal :  std_logic_vector(4 downto 0);
signal normalizedSignificand :  std_logic_vector(52 downto 0);
signal significandPreRound :  std_logic_vector(22 downto 0);
signal lsb, lsb_d1 :  std_logic;
signal roundBit, roundBit_d1 :  std_logic;
signal stickyBit, stickyBit_d1 :  std_logic;
signal deltaExp :  std_logic_vector(7 downto 0);
signal fullCancellation, fullCancellation_d1, fullCancellation_d2, fullCancellation_d3 :  std_logic;
signal expPreRound, expPreRound_d1 :  std_logic_vector(7 downto 0);
signal expSigPreRound :  std_logic_vector(30 downto 0);
signal roundUpBit :  std_logic;
signal expSigR, expSigR_d1 :  std_logic_vector(30 downto 0);
signal resultIsZero :  std_logic;
signal resultIsInf :  std_logic;
signal constInf, constInf_d1, constInf_d2, constInf_d3, constInf_d4, constInf_d5, constInf_d6, constInf_d7, constInf_d8, constInf_d9, constInf_d10 :  std_logic_vector(30 downto 0);
signal constNaN, constNaN_d1, constNaN_d2, constNaN_d3, constNaN_d4, constNaN_d5, constNaN_d6, constNaN_d7, constNaN_d8, constNaN_d9, constNaN_d10 :  std_logic_vector(30 downto 0);
signal expSigR2 :  std_logic_vector(30 downto 0);
signal signR :  std_logic;
signal computedR :  std_logic_vector(31 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            newX_d1 <=  newX;
            newX_d2 <=  newX_d1;
            newX_d3 <=  newX_d2;
            newX_d4 <=  newX_d3;
            newY_d1 <=  newY;
            newY_d2 <=  newY_d1;
            newY_d3 <=  newY_d2;
            newY_d4 <=  newY_d3;
            expDiff_d1 <=  expDiff;
            expNewX_d1 <=  expNewX;
            expNewX_d2 <=  expNewX_d1;
            expNewX_d3 <=  expNewX_d2;
            expNewX_d4 <=  expNewX_d3;
            expNewX_d5 <=  expNewX_d4;
            expNewX_d6 <=  expNewX_d5;
            expNewX_d7 <=  expNewX_d6;
            expNewY_d1 <=  expNewY;
            signNewX_d1 <=  signNewX;
            signNewX_d2 <=  signNewX_d1;
            signNewX_d3 <=  signNewX_d2;
            signNewX_d4 <=  signNewX_d3;
            signNewX_d5 <=  signNewX_d4;
            signNewX_d6 <=  signNewX_d5;
            signNewX_d7 <=  signNewX_d6;
            signNewX_d8 <=  signNewX_d7;
            signNewX_d9 <=  signNewX_d8;
            signNewX_d10 <=  signNewX_d9;
            signNewY_d1 <=  signNewY;
            signNewY_d2 <=  signNewY_d1;
            signNewY_d3 <=  signNewY_d2;
            signNewY_d4 <=  signNewY_d3;
            signNewY_d5 <=  signNewY_d4;
            signNewY_d6 <=  signNewY_d5;
            signNewY_d7 <=  signNewY_d6;
            signNewY_d8 <=  signNewY_d7;
            signNewY_d9 <=  signNewY_d8;
            signNewY_d10 <=  signNewY_d9;
            EffSub_d1 <=  EffSub;
            EffSub_d2 <=  EffSub_d1;
            EffSub_d3 <=  EffSub_d2;
            EffSub_d4 <=  EffSub_d3;
            EffSub_d5 <=  EffSub_d4;
            EffSub_d6 <=  EffSub_d5;
            EffSub_d7 <=  EffSub_d6;
            EffSub_d8 <=  EffSub_d7;
            EffSub_d9 <=  EffSub_d8;
            xExpFieldZero_d1 <=  xExpFieldZero;
            xExpFieldZero_d2 <=  xExpFieldZero_d1;
            xExpFieldZero_d3 <=  xExpFieldZero_d2;
            xExpFieldZero_d4 <=  xExpFieldZero_d3;
            xExpFieldZero_d5 <=  xExpFieldZero_d4;
            xExpFieldZero_d6 <=  xExpFieldZero_d5;
            yExpFieldZero_d1 <=  yExpFieldZero;
            yExpFieldZero_d2 <=  yExpFieldZero_d1;
            yExpFieldZero_d3 <=  yExpFieldZero_d2;
            xExpFieldAllOnes_d1 <=  xExpFieldAllOnes;
            xExpFieldAllOnes_d2 <=  xExpFieldAllOnes_d1;
            xExpFieldAllOnes_d3 <=  xExpFieldAllOnes_d2;
            yExpFieldAllOnes_d1 <=  yExpFieldAllOnes;
            yExpFieldAllOnes_d2 <=  yExpFieldAllOnes_d1;
            yExpFieldAllOnes_d3 <=  yExpFieldAllOnes_d2;
            xIsNaN_d1 <=  xIsNaN;
            yIsNaN_d1 <=  yIsNaN;
            xIsInfinity_d1 <=  xIsInfinity;
            xIsInfinity_d2 <=  xIsInfinity_d1;
            xIsInfinity_d3 <=  xIsInfinity_d2;
            xIsInfinity_d4 <=  xIsInfinity_d3;
            xIsInfinity_d5 <=  xIsInfinity_d4;
            xIsInfinity_d6 <=  xIsInfinity_d5;
            yIsInfinity_d1 <=  yIsInfinity;
            yIsInfinity_d2 <=  yIsInfinity_d1;
            yIsInfinity_d3 <=  yIsInfinity_d2;
            yIsInfinity_d4 <=  yIsInfinity_d3;
            yIsInfinity_d5 <=  yIsInfinity_d4;
            yIsInfinity_d6 <=  yIsInfinity_d5;
            xIsZero_d1 <=  xIsZero;
            xIsZero_d2 <=  xIsZero_d1;
            xIsZero_d3 <=  xIsZero_d2;
            xIsZero_d4 <=  xIsZero_d3;
            xIsZero_d5 <=  xIsZero_d4;
            xIsZero_d6 <=  xIsZero_d5;
            yIsZero_d1 <=  yIsZero;
            yIsZero_d2 <=  yIsZero_d1;
            yIsZero_d3 <=  yIsZero_d2;
            yIsZero_d4 <=  yIsZero_d3;
            yIsZero_d5 <=  yIsZero_d4;
            yIsZero_d6 <=  yIsZero_d5;
            resultIsNaN_d1 <=  resultIsNaN;
            resultIsNaN_d2 <=  resultIsNaN_d1;
            resultIsNaN_d3 <=  resultIsNaN_d2;
            resultIsNaN_d4 <=  resultIsNaN_d3;
            resultIsNaN_d5 <=  resultIsNaN_d4;
            rightShiftValue_d1 <=  rightShiftValue;
            shiftCorrection_d1 <=  shiftCorrection;
            stickyLow_d1 <=  stickyLow;
            stickyLow_d2 <=  stickyLow_d1;
            stickyLow_d3 <=  stickyLow_d2;
            stickyLow_d4 <=  stickyLow_d3;
            z1_d1 <=  z1;
            z1_d2 <=  z1_d1;
            z0_d1 <=  z0;
            z0_d2 <=  z0_d1;
            lsb_d1 <=  lsb;
            roundBit_d1 <=  roundBit;
            stickyBit_d1 <=  stickyBit;
            fullCancellation_d1 <=  fullCancellation;
            fullCancellation_d2 <=  fullCancellation_d1;
            fullCancellation_d3 <=  fullCancellation_d2;
            expPreRound_d1 <=  expPreRound;
            expSigR_d1 <=  expSigR;
            constInf_d1 <=  constInf;
            constInf_d2 <=  constInf_d1;
            constInf_d3 <=  constInf_d2;
            constInf_d4 <=  constInf_d3;
            constInf_d5 <=  constInf_d4;
            constInf_d6 <=  constInf_d5;
            constInf_d7 <=  constInf_d6;
            constInf_d8 <=  constInf_d7;
            constInf_d9 <=  constInf_d8;
            constInf_d10 <=  constInf_d9;
            constNaN_d1 <=  constNaN;
            constNaN_d2 <=  constNaN_d1;
            constNaN_d3 <=  constNaN_d2;
            constNaN_d4 <=  constNaN_d3;
            constNaN_d5 <=  constNaN_d4;
            constNaN_d6 <=  constNaN_d5;
            constNaN_d7 <=  constNaN_d6;
            constNaN_d8 <=  constNaN_d7;
            constNaN_d9 <=  constNaN_d8;
            constNaN_d10 <=  constNaN_d9;
         end if;
      end process;

   -- Exponent difference and swap
   expFracX <= X(30 downto 0);
   expFracY <= Y(30 downto 0);
   expXmExpY <= ('0' & X(30 downto 23)) - ('0'  & Y(30 downto 23)) ;
   expYmExpX <= ('0' & Y(30 downto 23)) - ('0'  & X(30 downto 23)) ;
   swap <= '0' when expFracX >= expFracY else '1';
   newX <= X when swap = '0' else Y;
   newY <= Y when swap = '0' else X;
   expDiff <= expXmExpY when swap = '0' else expYmExpX;
   expNewX <= newX(30 downto 23);
   expNewY <= newY(30 downto 23);
   signNewX <= newX(31);
   signNewY <= newY(31);
   EffSub <= signNewX_d1 xor signNewY_d1;
   -- Special case dectection
   xExpFieldZero <= '1' when expNewX_d1="00000000" else '0';
   yExpFieldZero <= '1' when expNewY_d1="00000000" else '0';
   xExpFieldAllOnes <= '1' when expNewX_d1="11111111" else '0';
   yExpFieldAllOnes <= '1' when expNewY_d1="11111111" else '0';
   xSigFieldZero <= '1' when newX_d4(22 downto 0)="00000000000000000000000" else '0';
   ySigFieldZero <= '1' when newY_d4(22 downto 0)="00000000000000000000000" else '0';
   xIsNaN <= xExpFieldAllOnes_d3 and not xSigFieldZero;
   yIsNaN <= yExpFieldAllOnes_d3 and not ySigFieldZero;
   xIsInfinity <= xExpFieldAllOnes_d3 and xSigFieldZero;
   yIsInfinity <= yExpFieldAllOnes_d3 and ySigFieldZero;
   xIsZero <= xExpFieldZero_d3 and xSigFieldZero;
   yIsZero <= yExpFieldZero_d3 and ySigFieldZero;
   bothSubNormals <=  xExpFieldZero and yExpFieldZero;
   resultIsNaN <=  xIsNaN_d1 or yIsNaN_d1  or  (xIsInfinity_d1 and yIsInfinity_d1 and EffSub_d4);
   significandNewX <= not(xExpFieldZero) & newX_d1(22 downto 0);
   significandNewY <= not(yExpFieldZero) & newY_d1(22 downto 0);

   -- Significand alignment
   allShiftedOut <= '1' when (expDiff_d1 >= 26) else '0';
   rightShiftValue <= expDiff_d1(4 downto 0) when allShiftedOut='0' else CONV_STD_LOGIC_VECTOR(26,5) ;
   shiftCorrection <= '1' when (yExpFieldZero='1' and xExpFieldZero='0') else '0'; -- only other cases are: both normal or both subnormal
   finalRightShiftValue <= rightShiftValue_d1 - ("0000" & shiftCorrection_d1);
   significandY00 <= significandNewY & "00";
   RightShifterComponent: RightShifterSticky26_by_max_25_Freq300_uid4
      port map ( clk  => clk,
                 S => finalRightShiftValue,
                 X => significandY00,
                 R => shiftedSignificandY,
                 Sticky => stickyLow);
   summandY <= ('0' & shiftedSignificandY) xor (26 downto 0 => EffSub_d1);


   -- Significand addition
   summandX <= '0' & significandNewX & '0' & '0';
   carryIn <= EffSub_d3 and not stickyLow;
   fracAdder: IntAdder_27_Freq300_uid6
      port map ( clk  => clk,
                 Cin => carryIn,
                 X => summandX,
                 Y => summandY,
                 R => significandZ);

   -- Cancellation detection, renormalization (see explanations in IEEEFPAdd.cpp) 
   z1 <=  significandZ(26); -- bit of weight 1
   z0 <=  significandZ(25); -- bit of weight 0
   lzcZInput <= significandZ(26 downto 1);
   IEEEFPAdd_8_23_Freq300_uid2LeadingZeroCounter: LZC_26_Freq300_uid8
      port map ( clk  => clk,
                 I => lzcZInput,
                 O => lzc);
   leftShiftVal <= 
      lzc when ((z1_d2='1') or (z1_d2='0' and z0_d2='1' and xExpFieldZero_d6='1') or (z1_d2='0' and z0_d2='0' and xExpFieldZero_d6='0' and lzc<=expNewX_d7)  or (xExpFieldZero_d6='0' and lzc>=26) ) 
      else (expNewX_d7(4 downto 0)) when (xExpFieldZero_d6='0' and (lzc < 26) and (("000"&lzc)>=expNewX_d7)) 
       else "0000"&'1';
   LeftShifterComponent: LeftShifter27_by_max_26_Freq300_uid10
      port map ( clk  => clk,
                 S => leftShiftVal,
                 X => significandZ,
                 R => normalizedSignificand);
   significandPreRound <= normalizedSignificand(25 downto 3); -- remove the implicit zero/one
   lsb <= normalizedSignificand(3);
   roundBit <= normalizedSignificand(2);
   stickyBit <= stickyLow_d4 or  normalizedSignificand(1)or  normalizedSignificand(0);
   deltaExp <=    -- value to subtract to exponent for normalization
      "00000000" when ( (z1_d2='0' and z0_d2='1' and xExpFieldZero_d6='0')
          or  (z1_d2='0' and z0_d2='0' and xExpFieldZero_d6='1') )
      else "11111111" when ( (z1_d2='1')  or  (z1_d2='0' and z0_d2='1' and xExpFieldZero_d6='1'))
      else ("000" & lzc)-'1' when (z1_d2='0' and z0_d2='0' and xExpFieldZero_d6='0' and lzc<=expNewX_d7 and lzc<26)      else expNewX_d7;
   fullCancellation <= '1' when (lzc>=26) else '0';
   expPreRound <= expNewX_d7 - deltaExp; -- we may have a first overflow here
   expSigPreRound <= expPreRound_d1 & significandPreRound; 
   -- Final rounding, with the mantissa overflowing in the exponent  
   roundUpBit <= '1' when roundBit_d1='1' and (stickyBit_d1='1' or (stickyBit_d1='0' and lsb_d1='1')) else '0';
   roundingAdder: IntAdder_31_Freq300_uid13
      port map ( clk  => clk,
                 Cin => roundUpBit,
                 X => expSigPreRound,
                 Y => "0000000000000000000000000000000",
                 R => expSigR);
   -- Final packing
   resultIsZero <= '1' when (fullCancellation_d3='1' and expSigR_d1(30 downto 23)="00000000") else '0';
   resultIsInf <= '1' when resultIsNaN_d5='0' and (((xIsInfinity_d6='1' and yIsInfinity_d6='1'  and EffSub_d9='0')  or (xIsInfinity_d6='0' and yIsInfinity_d6='1')  or (xIsInfinity_d6='1' and yIsInfinity_d6='0')  or  (expSigR_d1(30 downto 23)="11111111"))) else '0';
   constInf <= "11111111" & "00000000000000000000000";
   constNaN <= "1111111111111111111111111111111";
   expSigR2 <= constInf_d10 when resultIsInf='1' else constNaN_d10 when resultIsNaN_d5='1' else expSigR_d1;
   signR <= '0' when ((resultIsNaN_d5='1'  or (resultIsZero='1' and xIsInfinity_d6='0' and yIsInfinity_d6='0')) and (xIsZero_d6='0' or yIsZero_d6='0' or (signNewX_d10 /= signNewY_d10)) )  else signNewX_d10;
   computedR <= signR & expSigR2;
   R <= computedR;
end architecture;


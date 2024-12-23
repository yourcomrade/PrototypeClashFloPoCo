-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.Test_testBench_types.all;
use testBench_slv2string_B426B45701B03559.all;

entity testBench is
  port(result : out boolean);
end;

architecture structural of testBench is
  signal \c$ds_app_arg\               : Test_testBench_types.index_9;
  signal \c$ds_app_arg_0\             : Test_testBench_types.Tuple6;
  signal s                            : Test_testBench_types.index_9 := to_unsigned(0,4);
  -- Test.hs:61:5-7
  signal \Test.testBench_clk\         : Test_testBench_types.clk_XilinxSystem;
  signal z                            : Test_testBench_types.index_27;
  signal result_1                     : Test_testBench_types.index_14;
  signal \c$ds_app_arg_1\             : std_logic_vector(31 downto 0);
  signal \c$result_rec\               : boolean;
  signal s_0                          : Test_testBench_types.index_14 := to_unsigned(0,4);
  signal f1                           : boolean;
  signal \f'\                         : boolean := false;
  -- Test.hs:54:1-9
  signal \c$Test.testBench_app_arg\   : std_logic_vector(31 downto 0);
  -- Test.hs:54:1-9
  signal \c$Test.testBench_app_arg_0\ : Test_testBench_types.rst_XilinxSystem;
  signal \c$ds_app_arg_selection_res\ : boolean;
  signal \c$vec\                      : Test_testBench_types.array_of_Tuple6(0 to 8);
  signal result_selection_res         : boolean;
  signal \c$vec_0\                    : Test_testBench_types.array_of_std_logic_vector_32(0 to 13);

begin
  \c$ds_app_arg_selection_res\ <= s < to_unsigned(8,4);

  \c$ds_app_arg\ <= s + to_unsigned(1,4) when \c$ds_app_arg_selection_res\ else
                    s;

  \c$vec\ <= Test_testBench_types.array_of_Tuple6'( ( Tuple6_sel0_std_logic_vector_0 => std_logic_vector'(x"3F800000")
                                                  , Tuple6_sel1_std_logic_vector_1 => std_logic_vector'(x"40000000")
                                                  , Tuple6_sel2_std_logic_vector_2 => std_logic_vector'(x"40400000")
                                                  , Tuple6_sel3_std_logic_0 => '0'
                                                  , Tuple6_sel4_std_logic_1 => '0'
                                                  , Tuple6_sel5_std_logic_vector_3 => std_logic_vector'("00") )
                                                  , ( Tuple6_sel0_std_logic_vector_0 => std_logic_vector'(x"3FC00000")
                                                  , Tuple6_sel1_std_logic_vector_1 => std_logic_vector'(x"40200000")
                                                  , Tuple6_sel2_std_logic_vector_2 => std_logic_vector'(x"40600000")
                                                  , Tuple6_sel3_std_logic_0 => '0'
                                                  , Tuple6_sel4_std_logic_1 => '0'
                                                  , Tuple6_sel5_std_logic_vector_3 => std_logic_vector'("00") )
                                                  , ( Tuple6_sel0_std_logic_vector_0 => std_logic_vector'(x"40000000")
                                                  , Tuple6_sel1_std_logic_vector_1 => std_logic_vector'(x"40400000")
                                                  , Tuple6_sel2_std_logic_vector_2 => std_logic_vector'(x"40800000")
                                                  , Tuple6_sel3_std_logic_0 => '0'
                                                  , Tuple6_sel4_std_logic_1 => '0'
                                                  , Tuple6_sel5_std_logic_vector_3 => std_logic_vector'("00") )
                                                  , ( Tuple6_sel0_std_logic_vector_0 => std_logic_vector'(x"40200000")
                                                  , Tuple6_sel1_std_logic_vector_1 => std_logic_vector'(x"40600000")
                                                  , Tuple6_sel2_std_logic_vector_2 => std_logic_vector'(x"40900000")
                                                  , Tuple6_sel3_std_logic_0 => '0'
                                                  , Tuple6_sel4_std_logic_1 => '0'
                                                  , Tuple6_sel5_std_logic_vector_3 => std_logic_vector'("00") )
                                                  , ( Tuple6_sel0_std_logic_vector_0 => std_logic_vector'(x"40400000")
                                                  , Tuple6_sel1_std_logic_vector_1 => std_logic_vector'(x"40800000")
                                                  , Tuple6_sel2_std_logic_vector_2 => std_logic_vector'(x"40A00000")
                                                  , Tuple6_sel3_std_logic_0 => '0'
                                                  , Tuple6_sel4_std_logic_1 => '0'
                                                  , Tuple6_sel5_std_logic_vector_3 => std_logic_vector'("00") )
                                                  , ( Tuple6_sel0_std_logic_vector_0 => std_logic_vector'(x"40600000")
                                                  , Tuple6_sel1_std_logic_vector_1 => std_logic_vector'(x"40900000")
                                                  , Tuple6_sel2_std_logic_vector_2 => std_logic_vector'(x"40B00000")
                                                  , Tuple6_sel3_std_logic_0 => '0'
                                                  , Tuple6_sel4_std_logic_1 => '0'
                                                  , Tuple6_sel5_std_logic_vector_3 => std_logic_vector'("00") )
                                                  , ( Tuple6_sel0_std_logic_vector_0 => std_logic_vector'(x"40800000")
                                                  , Tuple6_sel1_std_logic_vector_1 => std_logic_vector'(x"40A00000")
                                                  , Tuple6_sel2_std_logic_vector_2 => std_logic_vector'(x"40C00000")
                                                  , Tuple6_sel3_std_logic_0 => '0'
                                                  , Tuple6_sel4_std_logic_1 => '0'
                                                  , Tuple6_sel5_std_logic_vector_3 => std_logic_vector'("00") )
                                                  , ( Tuple6_sel0_std_logic_vector_0 => std_logic_vector'(x"40900000")
                                                  , Tuple6_sel1_std_logic_vector_1 => std_logic_vector'(x"40B00000")
                                                  , Tuple6_sel2_std_logic_vector_2 => std_logic_vector'(x"40D00000")
                                                  , Tuple6_sel3_std_logic_0 => '0'
                                                  , Tuple6_sel4_std_logic_1 => '0'
                                                  , Tuple6_sel5_std_logic_vector_3 => std_logic_vector'("00") )
                                                  , ( Tuple6_sel0_std_logic_vector_0 => std_logic_vector'(x"40A00000")
                                                  , Tuple6_sel1_std_logic_vector_1 => std_logic_vector'(x"40C00000")
                                                  , Tuple6_sel2_std_logic_vector_2 => std_logic_vector'(x"40E00000")
                                                  , Tuple6_sel3_std_logic_0 => '0'
                                                  , Tuple6_sel4_std_logic_1 => '0'
                                                  , Tuple6_sel5_std_logic_vector_3 => std_logic_vector'("00") ) );

  -- index begin
  indexVec : block
    signal vec_index : integer range 0 to 9-1;
  begin
    vec_index <= to_integer((signed(std_logic_vector(resize(s,64)))))
    -- pragma translate_off
                 mod 9
    -- pragma translate_on
                 ;
    \c$ds_app_arg_0\ <= \c$vec\(vec_index);
  end block;
  -- index end

  -- register begin
  s_register : process(\Test.testBench_clk\)
  begin
    if rising_edge(\Test.testBench_clk\) then
      if \c$Test.testBench_app_arg_0\ =  '1'  then
        s <= to_unsigned(0,4);
      else
        s <= \c$ds_app_arg\;
      end if;
    end if;
  end process;
  -- register end

  -- tbClockGen begin
  -- pragma translate_off
  clkGen : process is
    constant half_periodH : time := 10000000 fs / 2;
    constant half_periodL : time := 10000000 fs - half_periodH;
  begin
    \Test.testBench_clk\ <= '0';
    wait for 100000 ps;
    while (not \c$result_rec\) loop
      \Test.testBench_clk\ <= not \Test.testBench_clk\;
      wait for half_periodH;
      \Test.testBench_clk\ <= not \Test.testBench_clk\;
      wait for half_periodL;
    end loop;
    wait;
  end process;
  -- pragma translate_on
  -- tbClockGen end

  z <= resize(s_0,5) + resize(to_unsigned(1,4),5);

  result_selection_res <= z > to_unsigned(13,5);

  result_1 <= to_unsigned(13,4) when result_selection_res else
              resize(z,4);

  \c$vec_0\ <= Test_testBench_types.array_of_std_logic_vector_32'( std_logic_vector'(x"00000000")
                                                                 , std_logic_vector'(x"00000000")
                                                                 , std_logic_vector'(x"00000000")
                                                                 , std_logic_vector'(x"00000000")
                                                                 , std_logic_vector'(x"00000000")
                                                                 , std_logic_vector'(x"40400000")
                                                                 , std_logic_vector'(x"C0600000")
                                                                 , std_logic_vector'(x"3F800000")
                                                                 , std_logic_vector'(x"40C00000")
                                                                 , std_logic_vector'(x"00000000")
                                                                 , std_logic_vector'(x"00000000")
                                                                 , std_logic_vector'(x"00000000")
                                                                 , std_logic_vector'(x"00000000")
                                                                 , std_logic_vector'(x"00000000") );

  -- index begin
  indexVec_0 : block
    signal vec_index_0 : integer range 0 to 14-1;
  begin
    vec_index_0 <= to_integer((signed(std_logic_vector(resize(s_0,64)))))
    -- pragma translate_off
                 mod 14
    -- pragma translate_on
                 ;
    \c$ds_app_arg_1\ <= \c$vec_0\(vec_index_0);
  end block;
  -- index end

  \c$result_rec\ <= \f'\ when \f'\ else
                    f1;

  -- register begin
  s_0_register : process(\Test.testBench_clk\)
  begin
    if rising_edge(\Test.testBench_clk\) then
      if \c$Test.testBench_app_arg_0\ =  '1'  then
        s_0 <= to_unsigned(0,4);
      else
        s_0 <= result_1;
      end if;
    end if;
  end process;
  -- register end

  -- assert begin
  r_assert : block
    -- pragma translate_off
    signal actual : std_logic_vector(31 downto 0);
    signal expected : std_logic_vector(31 downto 0);
    -- pragma translate_on
  begin
    -- pragma translate_off
    actual <= (\c$Test.testBench_app_arg\);
    expected <= \c$ds_app_arg_1\;
    process(\Test.testBench_clk\) is
    begin
      if (rising_edge(\Test.testBench_clk\)) then
        assert (toSLV(actual) = toSLV(expected)) report (("outputVerifier") & ", expected: " & testBench_slv2string_B426B45701B03559.slv2string(toSLV(expected)) & ", actual: " & testBench_slv2string_B426B45701B03559.slv2string(toSLV(actual))) severity error;
      end if;
    end process;
    -- pragma translate_on
    f1 <= \f'\;
  end block;
  -- assert end

  -- register begin
  f_register : process(\Test.testBench_clk\)
  begin
    if rising_edge(\Test.testBench_clk\) then
      if \c$Test.testBench_app_arg_0\ =  '1'  then
        \f'\ <= false;
      else
        \f'\ <= (s_0 = to_unsigned(13,4));
      end if;
    end if;
  end process;
  -- register end

  fmaFloat_inst_block : block
    component fmaFloat port
      ( clk : in Test_testBench_types.clk_XilinxSystem
      ; A : in std_logic_vector(31 downto 0)
      ; B : in std_logic_vector(31 downto 0)
      ; C : in std_logic_vector(31 downto 0)
      ; negateAB : in std_logic
      ; negateC : in std_logic
      ; RndMode : in std_logic_vector(1 downto 0)
      ; R : out std_logic_vector(31 downto 0) );
    end component;
    signal A_port        : std_logic_vector(31 downto 0);
    signal B_port        : std_logic_vector(31 downto 0);
    signal C_port        : std_logic_vector(31 downto 0);
    signal negateAB_port : std_logic;
    signal negateC_port  : std_logic;
    signal RndMode_port  : std_logic_vector(1 downto 0);
  begin
    A_port <= \c$ds_app_arg_0\.Tuple6_sel0_std_logic_vector_0;

    B_port <= \c$ds_app_arg_0\.Tuple6_sel1_std_logic_vector_1;

    C_port <= \c$ds_app_arg_0\.Tuple6_sel2_std_logic_vector_2;

    negateAB_port <= \c$ds_app_arg_0\.Tuple6_sel3_std_logic_0;

    negateC_port <= \c$ds_app_arg_0\.Tuple6_sel4_std_logic_1;

    RndMode_port <= \c$ds_app_arg_0\.Tuple6_sel5_std_logic_vector_3;

    fmaFloat_inst : fmaFloat
      port map
        ( clk      => \Test.testBench_clk\
        , A        => A_port
        , B        => B_port
        , C        => C_port
        , negateAB => negateAB_port
        , negateC  => negateC_port
        , RndMode  => RndMode_port
        , R        => \c$Test.testBench_app_arg\ );


  end block;

  -- resetGen begin
  resetGen : block
    constant reset_delay : time := 100000 ps - 1 ps + (integer'(1) * 10000 ps);
  begin
  -- pragma translate_off
  \c$Test.testBench_app_arg_0\
    <= '1',
       '0' after reset_delay;
  -- pragma translate_on
  end block;
  -- resetGen end

  result <= \c$result_rec\;


end;


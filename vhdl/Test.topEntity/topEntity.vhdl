-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.Test_topEntity_types.all;

entity topEntity is
  port(-- clock
       carg   : in Test_topEntity_types.clk_XilinxSystem;
       carg_0 : in std_logic_vector(33 downto 0);
       carg_1 : in std_logic_vector(33 downto 0);
       carg_2 : in std_logic_vector(33 downto 0);
       carg_3 : in std_logic_vector(33 downto 0);
       carg_4 : in std_logic_vector(33 downto 0);
       result : out std_logic_vector(33 downto 0));
end;

architecture structural of topEntity is
  signal \c$app_arg\   : std_logic_vector(33 downto 0);
  signal \c$app_arg_0\ : std_logic_vector(33 downto 0);
  signal \c$app_arg_1\ : std_logic_vector(33 downto 0);
  signal \c$app_arg_2\ : std_logic_vector(33 downto 0) := (std_logic_vector'(0 to 33 => '-'));
  signal result_1      : std_logic_vector(33 downto 0) := (std_logic_vector'(0 to 33 => '-'));
  signal result_2      : std_logic_vector(33 downto 0) := (std_logic_vector'(0 to 33 => '-'));
  signal result_3      : std_logic_vector(33 downto 0) := (std_logic_vector'(0 to 33 => '-'));
  signal result_4      : std_logic_vector(33 downto 0) := (std_logic_vector'(0 to 33 => '-'));
  signal result_5      : std_logic_vector(33 downto 0) := (std_logic_vector'(0 to 33 => '-'));
  signal result_6      : std_logic_vector(33 downto 0) := (std_logic_vector'(0 to 33 => '-'));
  signal result_7      : std_logic_vector(33 downto 0) := (std_logic_vector'(0 to 33 => '-'));
  signal result_8      : std_logic_vector(33 downto 0) := (std_logic_vector'(0 to 33 => '-'));
  signal result_9      : std_logic_vector(33 downto 0) := (std_logic_vector'(0 to 33 => '-'));

begin
  multFloat_inst_block : block
    component multFloat port
      ( clk : in Test_topEntity_types.clk_XilinxSystem
      ; X : in std_logic_vector(33 downto 0)
      ; Y : in std_logic_vector(33 downto 0)
      ; R : out std_logic_vector(33 downto 0) );
    end component;
  begin
    multFloat_inst : multFloat
      port map
        ( clk => carg
        , X   => \c$app_arg_1\
        , Y   => \c$app_arg_0\
        , R   => \c$app_arg\ );


  end block;

  plusFloat_inst_block : block
    component plusFloat port
      ( clk : in Test_topEntity_types.clk_XilinxSystem
      ; X : in std_logic_vector(33 downto 0)
      ; Y : in std_logic_vector(33 downto 0)
      ; R : out std_logic_vector(33 downto 0) );
    end component;
  begin
    plusFloat_inst : plusFloat
      port map
        ( clk => carg
        , X   => carg_2
        , Y   => carg_3
        , R   => \c$app_arg_0\ );


  end block;

  plusFloat_inst_block_0 : block
    component plusFloat port
      ( clk : in Test_topEntity_types.clk_XilinxSystem
      ; X : in std_logic_vector(33 downto 0)
      ; Y : in std_logic_vector(33 downto 0)
      ; R : out std_logic_vector(33 downto 0) );
    end component;
  begin
    plusFloat_inst_0 : plusFloat
      port map
        ( clk => carg
        , X   => carg_0
        , Y   => carg_1
        , R   => \c$app_arg_1\ );


  end block;

  -- delay begin
  capp_arg_2_delay : process(carg)
  begin
    if rising_edge(carg) then
      \c$app_arg_2\ <= carg_4;
    end if;
  end process;
  -- delay end

  -- delay begin
  result_1_delay : process(carg)
  begin
    if rising_edge(carg) then
      result_1 <= \c$app_arg_2\;
    end if;
  end process;
  -- delay end

  -- delay begin
  result_2_delay : process(carg)
  begin
    if rising_edge(carg) then
      result_2 <= result_1;
    end if;
  end process;
  -- delay end

  -- delay begin
  result_3_delay : process(carg)
  begin
    if rising_edge(carg) then
      result_3 <= result_2;
    end if;
  end process;
  -- delay end

  -- delay begin
  result_4_delay : process(carg)
  begin
    if rising_edge(carg) then
      result_4 <= result_3;
    end if;
  end process;
  -- delay end

  -- delay begin
  result_5_delay : process(carg)
  begin
    if rising_edge(carg) then
      result_5 <= result_4;
    end if;
  end process;
  -- delay end

  -- delay begin
  result_6_delay : process(carg)
  begin
    if rising_edge(carg) then
      result_6 <= result_5;
    end if;
  end process;
  -- delay end

  -- delay begin
  result_7_delay : process(carg)
  begin
    if rising_edge(carg) then
      result_7 <= result_6;
    end if;
  end process;
  -- delay end

  -- delay begin
  result_8_delay : process(carg)
  begin
    if rising_edge(carg) then
      result_8 <= result_7;
    end if;
  end process;
  -- delay end

  -- delay begin
  result_9_delay : process(carg)
  begin
    if rising_edge(carg) then
      result_9 <= result_8;
    end if;
  end process;
  -- delay end

  plusFloat_inst_block_1 : block
    component plusFloat port
      ( clk : in Test_topEntity_types.clk_XilinxSystem
      ; X : in std_logic_vector(33 downto 0)
      ; Y : in std_logic_vector(33 downto 0)
      ; R : out std_logic_vector(33 downto 0) );
    end component;
  begin
    plusFloat_inst_1 : plusFloat
      port map
        ( clk => carg
        , X   => result_9
        , Y   => \c$app_arg\
        , R   => result );


  end block;


end;


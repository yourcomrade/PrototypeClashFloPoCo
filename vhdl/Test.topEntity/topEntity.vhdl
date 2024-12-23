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
       carg_0 : in std_logic_vector(31 downto 0);
       carg_1 : in std_logic_vector(31 downto 0);
       carg_2 : in std_logic_vector(31 downto 0);
       carg_3 : in std_logic_vector(31 downto 0);
       carg_4 : in std_logic_vector(31 downto 0);
       result : out std_logic_vector(31 downto 0));
end;

architecture structural of topEntity is
  signal \c$app_arg\   : std_logic_vector(31 downto 0);
  signal \c$app_arg_0\ : std_logic_vector(31 downto 0);
  signal \c$app_arg_1\ : std_logic_vector(31 downto 0);
  signal \c$app_arg_2\ : std_logic_vector(31 downto 0);
  signal \c$app_arg_3\ : std_logic_vector(31 downto 0) := (std_logic_vector'(0 to 31 => '-'));
  signal result_1      : std_logic_vector(31 downto 0) := (std_logic_vector'(0 to 31 => '-'));
  signal result_2      : std_logic_vector(31 downto 0) := (std_logic_vector'(0 to 31 => '-'));

begin
  plusFloat_inst_block : block
    component plusFloat port
      ( clk : in Test_topEntity_types.clk_XilinxSystem
      ; X : in std_logic_vector(31 downto 0)
      ; Y : in std_logic_vector(31 downto 0)
      ; R : out std_logic_vector(31 downto 0) );
    end component;
  begin
    plusFloat_inst : plusFloat
      port map
        ( clk => carg
        , X   => result_2
        , Y   => \c$app_arg_0\
        , R   => \c$app_arg\ );


  end block;

  fmaFloat_inst_block : block
    component fmaFloat port
      ( clk : in Test_topEntity_types.clk_XilinxSystem
      ; A : in std_logic_vector(31 downto 0)
      ; B : in std_logic_vector(31 downto 0)
      ; C : in std_logic_vector(31 downto 0)
      ; negateAB : in std_logic
      ; negateC : in std_logic
      ; RndMode : in std_logic_vector(1 downto 0)
      ; R : out std_logic_vector(31 downto 0) );
    end component;
    signal C_port        : std_logic_vector(31 downto 0);
    signal negateAB_port : std_logic;
    signal negateC_port  : std_logic;
    signal RndMode_port  : std_logic_vector(1 downto 0);
  begin
    C_port <= std_logic_vector'(x"00000000");

    negateAB_port <= '0';

    negateC_port <= '0';

    RndMode_port <= std_logic_vector'("00");

    fmaFloat_inst : fmaFloat
      port map
        ( clk      => carg
        , A        => \c$app_arg_2\
        , B        => \c$app_arg_1\
        , C        => C_port
        , negateAB => negateAB_port
        , negateC  => negateC_port
        , RndMode  => RndMode_port
        , R        => \c$app_arg_0\ );


  end block;

  plusFloat_inst_block_0 : block
    component plusFloat port
      ( clk : in Test_topEntity_types.clk_XilinxSystem
      ; X : in std_logic_vector(31 downto 0)
      ; Y : in std_logic_vector(31 downto 0)
      ; R : out std_logic_vector(31 downto 0) );
    end component;
  begin
    plusFloat_inst_0 : plusFloat
      port map
        ( clk => carg
        , X   => carg_2
        , Y   => carg_3
        , R   => \c$app_arg_1\ );


  end block;

  plusFloat_inst_block_1 : block
    component plusFloat port
      ( clk : in Test_topEntity_types.clk_XilinxSystem
      ; X : in std_logic_vector(31 downto 0)
      ; Y : in std_logic_vector(31 downto 0)
      ; R : out std_logic_vector(31 downto 0) );
    end component;
  begin
    plusFloat_inst_1 : plusFloat
      port map
        ( clk => carg
        , X   => carg_0
        , Y   => carg_1
        , R   => \c$app_arg_2\ );


  end block;

  -- delay begin
  capp_arg_3_delay : process(carg)
  begin
    if rising_edge(carg) then
      \c$app_arg_3\ <= carg_4;
    end if;
  end process;
  -- delay end

  -- delay begin
  result_1_delay : process(carg)
  begin
    if rising_edge(carg) then
      result_1 <= \c$app_arg_3\;
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

  expFloat_inst_block : block
    component expFloat port
      ( clk : in Test_topEntity_types.clk_XilinxSystem
      ; X : in std_logic_vector(31 downto 0)
      ; R : out std_logic_vector(31 downto 0) );
    end component;
  begin
    expFloat_inst : expFloat
      port map
        ( clk => carg
        , X   => \c$app_arg\
        , R   => result );


  end block;


end;


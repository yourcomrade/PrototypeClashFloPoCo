-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.FlopoCo_topEntity_types.all;

entity topEntity is
  port(-- clock
       carg   : in FlopoCo_topEntity_types.clk_XilinxSystem;
       carg_0 : in std_logic_vector(31 downto 0);
       carg_1 : in std_logic_vector(31 downto 0);
       carg_2 : in std_logic_vector(31 downto 0);
       result : out std_logic_vector(31 downto 0));
end;

architecture structural of topEntity is
  signal \c$app_arg\ : std_logic_vector(31 downto 0);

begin
  plusFloat_inst_block : block
    component plusFloat port
      ( clk : in std_logic
      ; X : in std_logic_vector(31 downto 0)
      ; Y : in std_logic_vector(31 downto 0)
      ; R : out std_logic_vector(31 downto 0) );
    end component;
  begin
    plusFloat_inst : plusFloat
      port map
        ( clk => carg
        , X   => carg_1
        , Y   => carg_2
        , R   => \c$app_arg\ );


  end block;

  plusFloat_inst_block_0 : block
    component plusFloat port
      ( clk : in std_logic
      ; X : in std_logic_vector(31 downto 0)
      ; Y : in std_logic_vector(31 downto 0)
      ; R : out std_logic_vector(31 downto 0) );
    end component;
  begin
    plusFloat_inst_0 : plusFloat
      port map
        ( clk => carg
        , X   => carg_0
        , Y   => \c$app_arg\
        , R   => result );


  end block;


end;


-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.VGAtopEntity_types.all;

entity VGAtopEntity is
  port(-- clock
       clk   : in VGAtopEntity_types.clk_XilinxSystem;
       -- reset
       rst   : in VGAtopEntity_types.rst_XilinxSystem;
       -- enable
       en    : in VGAtopEntity_types.en_XilinxSystem;
       sw    : in std_logic_vector(11 downto 0);
       hsync : out std_logic;
       vsync : out std_logic;
       rgb   : out std_logic_vector(11 downto 0));
end;

architecture structural of VGAtopEntity is
  -- Test.hs:33:1-9
  signal \c$b_app_arg\ : boolean := false;
  signal result_0      : std_logic_vector(11 downto 0);
  signal \c$app_arg\   : std_logic_vector(11 downto 0) := std_logic_vector'(x"000");
  -- Test.hs:33:1-9
  signal b             : boolean;
  -- Test.hs:33:1-9
  signal ds            : VGAtopEntity_types.Tuple6;
  signal result        : VGAtopEntity_types.Tuple3;

begin
  -- register begin
  cb_app_arg_register : process(clk)
  begin
    if rising_edge(clk) then
      if rst =  '1'  then
        \c$b_app_arg\ <= false;
      elsif en then
        \c$b_app_arg\ <= false;
      end if;
    end if;
  end process;
  -- register end

  result_0 <= sw when b else
              std_logic_vector'(x"000");

  -- register begin
  capp_arg_register : process(clk)
  begin
    if rising_edge(clk) then
      if rst =  '1'  then
        \c$app_arg\ <= std_logic_vector'(x"000");
      elsif en then
        \c$app_arg\ <= result_0;
      end if;
    end if;
  end process;
  -- register end

  result <= ( Tuple3_sel0_std_logic_0 => ds.Tuple6_sel1_std_logic_1
            , Tuple3_sel1_std_logic_1 => ds.Tuple6_sel2_std_logic_2
            , Tuple3_sel2_std_logic_vector => \c$app_arg\ );

  b <= \c$b_app_arg\ or (((std_logic_vector'(0 => ds.Tuple6_sel0_std_logic_0))) = std_logic_vector'("1"));

  vga_controller_inst_block : block
    signal video_on : std_logic;
    signal hsync_1  : std_logic;
    signal vsync_1  : std_logic;
    signal p_tick   : std_logic;
    signal x        : std_logic_vector(9 downto 0);
    signal y        : std_logic_vector(9 downto 0);
    component vga_controller port
      ( clk_100MHz : in VGAtopEntity_types.clk_XilinxSystem
      ; reset : in VGAtopEntity_types.rst_XilinxSystem
      ; video_on : out std_logic
      ; hsync : out std_logic
      ; vsync : out std_logic
      ; p_tick : out std_logic
      ; x : out std_logic_vector(9 downto 0)
      ; y : out std_logic_vector(9 downto 0) );
    end component;
  begin
    vga_controller_inst : vga_controller
      port map
        ( clk_100MHz => clk
        , reset      => rst
        , video_on   => video_on
        , hsync      => hsync_1
        , vsync      => vsync_1
        , p_tick     => p_tick
        , x          => x
        , y          => y );

    ds <= ( Tuple6_sel0_std_logic_0 => video_on
          , Tuple6_sel1_std_logic_1 => hsync_1
          , Tuple6_sel2_std_logic_2 => vsync_1
          , Tuple6_sel3_std_logic_3 => p_tick
          , Tuple6_sel4_std_logic_vector_0 => x
          , Tuple6_sel5_std_logic_vector_1 => y );


  end block;

  hsync <= result.Tuple3_sel0_std_logic_0;

  vsync <= result.Tuple3_sel1_std_logic_1;

  rgb <= result.Tuple3_sel2_std_logic_vector;


end;


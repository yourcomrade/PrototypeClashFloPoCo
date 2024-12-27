library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package VGAtopEntity_types is
  subtype en_XilinxSystem is boolean;
  subtype rst_XilinxSystem is std_logic;



  type Tuple6 is record
    Tuple6_sel0_std_logic_0 : std_logic;
    Tuple6_sel1_std_logic_1 : std_logic;
    Tuple6_sel2_std_logic_2 : std_logic;
    Tuple6_sel3_std_logic_3 : std_logic;
    Tuple6_sel4_std_logic_vector_0 : std_logic_vector(9 downto 0);
    Tuple6_sel5_std_logic_vector_1 : std_logic_vector(9 downto 0);
  end record;
  subtype clk_XilinxSystem is std_logic;
  type Tuple3 is record
    Tuple3_sel0_std_logic_0 : std_logic;
    Tuple3_sel1_std_logic_1 : std_logic;
    Tuple3_sel2_std_logic_vector : std_logic_vector(11 downto 0);
  end record;
  function toSLV (b : in boolean) return std_logic_vector;
  function fromSLV (sl : in std_logic_vector) return boolean;
  function tagToEnum (s : in signed) return boolean;
  function dataToTag (b : in boolean) return signed;
  function toSLV (sl : in std_logic) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic;
  function toSLV (slv : in std_logic_vector) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic_vector;
  function toSLV (p : Tuple6) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple6;
  function toSLV (p : Tuple3) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple3;
end;

package body VGAtopEntity_types is
  function toSLV (b : in boolean) return std_logic_vector is
  begin
    if b then
      return "1";
    else
      return "0";
    end if;
  end;
  function fromSLV (sl : in std_logic_vector) return boolean is
  begin
    if sl = "1" then
      return true;
    else
      return false;
    end if;
  end;
  function tagToEnum (s : in signed) return boolean is
  begin
    if s = to_signed(0,64) then
      return false;
    else
      return true;
    end if;
  end;
  function dataToTag (b : in boolean) return signed is
  begin
    if b then
      return to_signed(1,64);
    else
      return to_signed(0,64);
    end if;
  end;
  function toSLV (sl : in std_logic) return std_logic_vector is
  begin
    return std_logic_vector'(0 => sl);
  end;
  function fromSLV (slv : in std_logic_vector) return std_logic is
    alias islv : std_logic_vector (0 to slv'length - 1) is slv;
  begin
    return islv(0);
  end;
  function toSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
  end;
  function fromSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
  end;
  function toSLV (p : Tuple6) return std_logic_vector is
  begin
    return (toSLV(p.Tuple6_sel0_std_logic_0) & toSLV(p.Tuple6_sel1_std_logic_1) & toSLV(p.Tuple6_sel2_std_logic_2) & toSLV(p.Tuple6_sel3_std_logic_3) & toSLV(p.Tuple6_sel4_std_logic_vector_0) & toSLV(p.Tuple6_sel5_std_logic_vector_1));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple6 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 1)),fromSLV(islv(2 to 2)),fromSLV(islv(3 to 3)),fromSLV(islv(4 to 13)),fromSLV(islv(14 to 23)));
  end;
  function toSLV (p : Tuple3) return std_logic_vector is
  begin
    return (toSLV(p.Tuple3_sel0_std_logic_0) & toSLV(p.Tuple3_sel1_std_logic_1) & toSLV(p.Tuple3_sel2_std_logic_vector));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple3 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 1)),fromSLV(islv(2 to 13)));
  end;
end;


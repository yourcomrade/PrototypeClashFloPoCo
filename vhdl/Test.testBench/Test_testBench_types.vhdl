library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package Test_testBench_types is

  subtype rst_XilinxSystem is std_logic;
  subtype index_14 is unsigned(3 downto 0);
  subtype index_27 is unsigned(4 downto 0);


  subtype clk_XilinxSystem is std_logic;
  type Tuple6 is record
    Tuple6_sel0_std_logic_vector_0 : std_logic_vector(31 downto 0);
    Tuple6_sel1_std_logic_vector_1 : std_logic_vector(31 downto 0);
    Tuple6_sel2_std_logic_vector_2 : std_logic_vector(31 downto 0);
    Tuple6_sel3_std_logic_0 : std_logic;
    Tuple6_sel4_std_logic_1 : std_logic;
    Tuple6_sel5_std_logic_vector_3 : std_logic_vector(1 downto 0);
  end record;
  type array_of_Tuple6 is array (integer range <>) of Tuple6;
  type array_of_std_logic_vector_32 is array (integer range <>) of std_logic_vector(31 downto 0);
  subtype index_9 is unsigned(3 downto 0);
  function toSLV (slv : in std_logic_vector) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic_vector;
  function toSLV (sl : in std_logic) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic;
  function toSLV (u : in unsigned) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return unsigned;
  function toSLV (b : in boolean) return std_logic_vector;
  function fromSLV (sl : in std_logic_vector) return boolean;
  function tagToEnum (s : in signed) return boolean;
  function dataToTag (b : in boolean) return signed;
  function toSLV (p : Tuple6) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple6;
  function toSLV (value :  array_of_Tuple6) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_Tuple6;
  function toSLV (value :  array_of_std_logic_vector_32) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_std_logic_vector_32;
end;

package body Test_testBench_types is
  function toSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
  end;
  function fromSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
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
  function toSLV (u : in unsigned) return std_logic_vector is
  begin
    return std_logic_vector(u);
  end;
  function fromSLV (slv : in std_logic_vector) return unsigned is
    alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return unsigned(islv);
  end;
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
  function toSLV (p : Tuple6) return std_logic_vector is
  begin
    return (toSLV(p.Tuple6_sel0_std_logic_vector_0) & toSLV(p.Tuple6_sel1_std_logic_vector_1) & toSLV(p.Tuple6_sel2_std_logic_vector_2) & toSLV(p.Tuple6_sel3_std_logic_0) & toSLV(p.Tuple6_sel4_std_logic_1) & toSLV(p.Tuple6_sel5_std_logic_vector_3));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple6 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 31)),fromSLV(islv(32 to 63)),fromSLV(islv(64 to 95)),fromSLV(islv(96 to 96)),fromSLV(islv(97 to 97)),fromSLV(islv(98 to 99)));
  end;
  function toSLV (value :  array_of_Tuple6) return std_logic_vector is
    alias ivalue    : array_of_Tuple6(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 100);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 100) + 1 to i*100) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_Tuple6 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_Tuple6(0 to slv'length / 100 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 100 to (i+1) * 100 - 1));
    end loop;
    return result;
  end;
  function toSLV (value :  array_of_std_logic_vector_32) return std_logic_vector is
    alias ivalue    : array_of_std_logic_vector_32(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 32);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 32) + 1 to i*32) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_std_logic_vector_32 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_std_logic_vector_32(0 to slv'length / 32 - 1);
  begin
    for i in result'range loop
      result(i) := islv(i * 32 to (i+1) * 32 - 1);
    end loop;
    return result;
  end;
end;


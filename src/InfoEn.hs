{- |
  Copyright   :  (C) 2024, QBayLogic B.V.
  License     :  BSD2
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
  
-}
module InfoEn  (
    InfoEntity(..)
)where
import Prelude 
-- |This is the record to store information about entity(VHDL) or module(Verilog, SystemVerilog)
data InfoEntity = InfoEntity {
    name:: Maybe String, -- ^ Name of entity(VHDL) or module(Verilog, SystemVerilog)
    freq :: Maybe Int,  -- ^ Frequency of target of FPGA (optional)
    pipedep ::Maybe Int, -- ^ Pipeline depth of this entity(VHDL) or module(Verilog, SystemVerilog) (optional)
    insig:: Maybe [ String], -- ^ List of input signals name of entity(VHDL) or module(Verilog, SystemVerilog)
    outsig::Maybe [String] -- ^ List of output signals name of entity(VHDL) or module(Verilog, SystemVerilog)
    } deriving(Show)
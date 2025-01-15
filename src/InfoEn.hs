{-# LANGUAGE TemplateHaskell #-}
{- |
  Copyright   :  (C) 2024, QBayLogic B.V.
  License     :  BSD2
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
  
-}
module InfoEn  (
    InfoEntity(..)
)where
import Prelude 

import Language.Haskell.TH.Syntax
-- |This is the record to store information about entity(VHDL) or module(Verilog, SystemVerilog)
data InfoEntity = InfoEntity {
  -- | Name of entity(VHDL) or module(Verilog, SystemVerilog)
    name:: Maybe String, 
    -- | Frequency of target of FPGA (optional)
    freq :: Maybe Int,  
    -- | Pipeline depth of this entity(VHDL) or module(Verilog, SystemVerilog) (optional)
    pipedep ::Maybe Int, 
     -- | List of input signals name of entity(VHDL) or module(Verilog, SystemVerilog)
    insig:: Maybe [ String],
    -- | List of output signals name of entity(VHDL) or module(Verilog, SystemVerilog)
    outsig::Maybe [String] 
    } deriving(Show, Lift)
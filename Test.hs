module Test where

import Clash.Explicit.Prelude
import Clash.Class.BitPack
import FlopoCo 
{-
topEntity ::
  Clock XilinxSystem ->
  DSignal XilinxSystem 0 (BitVector 34) ->
  DSignal XilinxSystem 0 (BitVector 34) ->
  DSignal XilinxSystem 0 (BitVector 34) ->
  DSignal XilinxSystem 0 (BitVector 34) ->
  DSignal XilinxSystem 0 (BitVector 34) ->
  DSignal XilinxSystem (0 + N + N2 + N) (BitVector 34)
topEntity clk x y z w m = plusFloat clk (delayI undefined enableGen clk m) (multFloat clk a b)
    where
        a = delayI undefined enableGen clk (plusFloat clk x y)
        b = delayI undefined enableGen clk (plusFloat clk z w)

-}


-- VGA Test Top Module
topEntity
  :: Clock XilinxSystem   -- ^ System Clock (100 MHz)
  -> Reset XilinxSystem   -- ^ Reset Signal
  -> Enable XilinxSystem
  -> Signal XilinxSystem (BitVector 12)  -- ^ Switches input (for color selection)
  -> ( Signal XilinxSystem Bit         -- ^ Horizontal Sync
     , Signal XilinxSystem Bit         -- ^ Vertical Sync
     , Signal XilinxSystem (BitVector 12)  -- ^ RGB Output
     )
topEntity clk rst en sw = (hsync, vsync, rgbOut)
  where
    -- VGA Controller instantiation
    (videoOn, hsync, vsync, _, _, _) = vga_controller clk rst

    -- Resetting register
    resetting :: Signal XilinxSystem Bool
    resetting = register clk rst en False (pure False)

    -- RGB register with conditional input
    rgbReg :: Signal XilinxSystem (BitVector 12)
    rgbReg = register clk rst en 0 (mux (resetting .||. (fmap bitToBool videoOn)) sw (pure 0))

    -- Output
    rgbOut :: Signal XilinxSystem (BitVector 12)
    rgbOut = rgbReg
-- Synthesize topEntity
{-# ANN topEntity
  (Synthesize
    { t_name   = "VGAtopEntity"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortName "en"
                 , PortName "sw"
                 ]
    , t_output = PortProduct "" [PortName "hsync", PortName "vsync", PortName "rgb"]
    }) #-}
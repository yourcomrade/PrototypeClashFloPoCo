module Test where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Class.BitPack
import FlopoCo 

topEntity ::
  Clock XilinxSystem ->
  DSignal XilinxSystem 0 Float ->
  DSignal XilinxSystem 0 Float ->
  DSignal XilinxSystem 0 Float ->
  DSignal XilinxSystem 0 Float ->
  DSignal XilinxSystem 0 Float ->
  DSignal XilinxSystem (0 + N + N2 + N +N3) Float
topEntity clk x y z w m = expFloat clk (plusFloat clk (delayI undefined enableGen clk m) aXb)
    where
        a = delayI undefined enableGen clk (plusFloat clk x y)
        b = delayI undefined enableGen clk (plusFloat clk z w)
        aXb = fmaFloat clk a b c negab negc rnd
        c = pure (fromIntegral 0)
        negab = pure (fromIntegral 0)
        negc = pure (fromIntegral 0)
        rnd =pure (fromIntegral 0)
{-
inputVec :: Vec 4 (Float, Float, Float, Bit, Bit, BitVector 2)
inputVec =
  $(listToVecTH
      [ (1.0::Float, 2.0::Float, 3.0::Float, 0::Bit, 0::Bit, 0::(BitVector 2))
      , (1.5, -2.5, 0.0, 1, 0, 0)
      , (0.0, 0.0, 0.0, 0, 1, 0)
      , (3.0, 3.0, 3.0, 1, 1, 0)
      ])
-}
inputVec :: Vec 9 (Float, Float, Float, Bit, Bit, BitVector 2)
inputVec =
  $(listToVecTH
      [ (1.0::Float, 2.0::Float, 3.0::Float, 0::Bit, 0::Bit, 0::(BitVector 2))
      , (1.5, 2.5, 3.5, 0, 0, 0)
      , (2.0, 3.0, 4.0, 0, 0, 0)
      , (2.5, 3.5, 4.5, 0, 0, 0)
      , (3.0, 4.0, 5.0, 0, 0, 0)
      , (3.5, 4.5, 5.5, 0, 0, 0)
      , (4.0, 5.0, 6.0, 0, 0, 0)
      , (4.5, 5.5, 6.5, 0, 0, 0)
      , (5.0, 6.0, 7.0, 0, 0, 0) ])

expectedVec :: Vec 14 Float
expectedVec =
  $(listToVecTH [0.0::Float,0.0, 0.0, 0.0, 0.0,3.0, -3.5, 1.0, 6.0, 0.0, 0.0, 0.0, 0.0, 0.0])
  

testBench :: Signal XilinxSystem Bool
testBench = done
  where
    testInput =
      stimuliGenerator clk rst inputVec
    expectedOutput =
      outputVerifier' clk rst expectedVec
    done = expectedOutput (toSignal $ fmaFloat clk a b c negab negc rnd)
    clk = tbClockGen (not <$> done)
    rst = resetGen
    -- Unbundle the test input into individual signals
     -- Extract individual signals and convert to DSignal
    (a, b, c, negab, negc, rnd) =
      (fromSignal ia, fromSignal ib, fromSignal ic,
       fromSignal inegab, fromSignal inegc, fromSignal irnd)
      where
        (ia, ib, ic, inegab, inegc, irnd) = unbundle testInput

{-
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
-}
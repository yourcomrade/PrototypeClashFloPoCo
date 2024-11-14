module Test where

import Clash.Explicit.Prelude
import FlopoCo 

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
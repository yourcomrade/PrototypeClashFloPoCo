{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module FlopoCo where

import Clash.Explicit.Prelude
import Data.String.Interpolate (__i)
import Clash.Annotations.Primitive (Primitive(..), HDL(..))

import Control.Monad.State (State, liftIO)
import qualified Data.List as L
import Data.Text
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Clash.Backend (Backend)
import Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta(..), TemplateKind(..), emptyBlackBoxMeta)
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
  (BlackBox (..), BlackBoxContext, EntityOrComponent(..), TemplateFunction(..))
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL


import qualified FloPoCoCall as FPCC
import qualified Clash.Netlist.Id as Id
import Clash.Promoted.Nat.TH(decLiteralD)
import Clash.Promoted.Nat(SNat(..))
import Help(num)
-- | floating point addition, assume pipeline depth of 2



type N = $(num)

--type N = 10

-- Type-level version of `num`
-- numNat :: forall n. KnownNat n => SNat n
-- numNat = fromInteger num
xp :: SNat N
xp = SNat::SNat N
plusFloat
  :: forall n
   . Clock XilinxSystem
  -> DSignal XilinxSystem n Float
  -> DSignal XilinxSystem n Float
  -> DSignal XilinxSystem (n + N) Float
plusFloat clk a b =
  delayN xp undefined enableGen clk (liftA2 (+) a b)
{-# ANN plusFloat (
    let
      primName = show 'plusFloat
      tfName = show 'plusFloatBBF
    in
      InlineYamlPrimitive [minBound..] [__i|
        BlackBoxHaskell:
          name: #{primName}
          templateFunction: #{tfName}
          workInfo: Always
      |]) #-}
{-# OPAQUE plusFloat #-}
-- Template Haskell Splice to generate entity name
-- entityNameTH :: String
-- entityNameTH = $(FloPoCoGen.generateFloPoCoEntity "/home/minh/flopoco/build/bin/flopoco"
--                              ["frequency=300", "target=Zynq7000", "IEEEFPAdd", "wE=8", "wF=23","name=plusFloat", "registerLargeTables=1"])

-- plusFloatGen :: String -> Int -> Q Dec
-- plusFloatGen mhz = plusFloatGen mhz

-- $(plusFloatGen "plusFloat300Mhz" 300) 
--
-- ====>
--
-- plusFloat300Mhz ::
--   Clock dom ->
--   DSignal dom n Float ->
--   DSignal dom n Float ->
--   DSignal dom (n+8) Float
--

plusFloatBBF :: BlackBoxFunction
plusFloatBBF _isD _primName _args _resTys = do
  -- Call Flopoco
  -- do something here to call flopo and get
  --

  -- let entityName = parseEntityName output
  let entityName = "plusFloat"
  
  let meta = emptyBlackBoxMeta {bbKind = TDecl}
      bb = BBFunction (show 'plusFloatTF) 0 (plusFloatTF entityName)
  pure (Right (meta, bb))



plusFloatTF ::
  HasCallStack =>
  Text ->
  TemplateFunction
plusFloatTF entityName =
  TemplateFunction
    [0,1,2]
    (const True)
    (plusFloatBBTF entityName)

plusFloatBBTF ::
  forall s .
  Backend s =>
  Text ->
  BlackBoxContext ->
  State s Doc
plusFloatBBTF entityName bbCtx
  | [ clk, a, b
    ] <- L.map fst (DSL.tInputs bbCtx)
  , [result] <- DSL.tResults bbCtx
  = do

    plusFloatInstName <- Id.makeBasic "plusFloat_inst"

    let
      compInps =
        [ ("clk", N.Bit)
        , ("X", DSL.ety a)
        , ("Y", DSL.ety b) ]
      compOuts =
        [ ("R", DSL.ety result) ]

    DSL.declaration "plusFloat_inst_block" $ do
      DSL.compInBlock entityName compInps compOuts

      let
        inps =
          [ ("clk", clk )
          , ("X", a)
          , ("Y", b)
          ]

        outs =
          [ ("R", result)
          ]

      DSL.instDecl Empty (Id.unsafeMake entityName) plusFloatInstName
        [] inps outs


topEntity ::
  Clock XilinxSystem ->
  DSignal XilinxSystem 0 Float ->
  DSignal XilinxSystem 0 Float ->
  DSignal XilinxSystem 0 Float ->
  DSignal XilinxSystem (0 + N + N) Float
topEntity clk x y z =
  plusFloat clk
    (delayI undefined enableGen clk x)
    (plusFloat clk y z)



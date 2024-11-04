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
--import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
  (BlackBox (..), BlackBoxContext, EntityOrComponent(..), TemplateFunction(..))
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL

import Data.Maybe (fromMaybe, isJust, fromJust)

import qualified FloPoCoCall as FPCC
import qualified Clash.Netlist.Id as Id
import Clash.Promoted.Nat.TH(decLiteralD)
import Clash.Promoted.Nat(SNat(..))
import Help( infoEn)
import Tem(getPipeDep, flopocoPrim, generateBlackBoxFunction, generateTemplateFunction)
import Lexer
import Clash.Promoted.Nat.Unsafe (unsafeSNat)
import Clash.Annotations.BitRepresentation
import System.IO.Unsafe
-- | floating point addition, assume pipeline depth of 2
import Language.Haskell.TH.Syntax (Name)
import Debug.Trace (trace, traceShow)
import Clash.Primitives.Types (Primitive(BlackBoxHaskell, workInfo))
import Clash.Driver.Bool (OverridingBool(Always))
import Text.Show.Pretty(ppShow)
import Lexer (lengthMaybeStrings)

plusFloatBBTF ::
  forall s .
  Backend s =>
  Text ->
  BlackBoxContext ->
  State s Doc
plusFloatBBTF  entityName bbCtx
  | [ clk, a, b
    ] <- L.map fst (DSL.tInputs bbCtx)
  , [result] <- DSL.tResults bbCtx
  = do

    plusFloatInstName <- Id.makeBasic (entityName <> "_inst")

    let
      compInps =
        [ ("clk", N.Bit)
        , ("X", DSL.ety a)
        , ("Y", DSL.ety b) ]
      compOuts =
        [ ("R", DSL.ety result) ]

    DSL.declaration (entityName <> "_inst_block") $ do
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
  | otherwise = error $ ppShow bbCtx
--type N = 10

-- Type-level version of `num`
-- numNat :: forall n. KnownNat n => SNat n
-- numNat = fromInteger num
type N = $(getPipeDep infoEn)

xp :: SNat N
xp = SNat::SNat N
{-# OPAQUE plusFloat #-}
plusFloat
  :: forall n . 
  Clock XilinxSystem
  -> DSignal XilinxSystem n Float
  -> DSignal XilinxSystem n Float
  -> DSignal XilinxSystem (n + N) Float
plusFloat clk a b =
  delayN xp undefined enableGen clk (liftA2 (+) a b)
$(generateTemplateFunction (Data.Text.pack (fromJust (name infoEn))) (lengthMaybeStrings (insig infoEn)))
$(generateBlackBoxFunction (fromJust (name infoEn)))
{-
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

-}

{-# ANN plusFloat (flopocoPrim 'plusFloat 'plusFloatBBF) #-}


{-
data FloatException 
  = ZeroExp
  | NormalExp
  | InfExp
  | NaNExp
{-# ANN module (DataReprAnn $(liftQ [t|FloatException|]) 2 [ConstrRepr 'ZeroExp   0b11 0b00 []
                                                           ,ConstrRepr 'NormalExp 0b11 0b01 []
                                                           ,ConstrRepr 'InfExp    0b11 0b10 []
                                                           ,ConstrRepr 'NaNExp    0b11 0b11 []
                                                           ]) #-}

data FlopocoFloat 
  = FlopocoFloat FloatException Float
{-# ANN module (DataReprAnn $(liftQ [t|FloatException|]) 34 [ConstrRepr 'FlopocoFloat 0b0 0b0 [3 `shiftL` 32, (1 `shiftL` 32)-1]]) #-}
-}

{-

plusFloatX ::
  Clock XilinxSystem ->
  Signal XilinxSystem (Maybe Float) ->
  Signal XilinxSystem (Maybe Float) ->
  Signal XilinxSystem (Maybe Float)
plusFloatX clk a b =
  let
    floPoCoPath = "/home/minh/flopoco/build/bin/flopoco"
    args = ["frequency=300", "target=Zynq7000", "IEEEFPAdd", "wE=8", "wF=23","name=plusFloat", "registerLargeTables=1"]
    filePath = "flopoco.vhdl"

    xpX = unsafeSNat (toInteger (unsafePerformIO (genPipeDep floPoCoPath args filePath)))
    aX = fmap (fromMaybe 0.0) a
    bX = fmap (fromMaybe 0.0) b
    valInp = liftA2 (&&) (fmap isJust a) (fmap isJust b)
    yX = toSignal (plusFloat clk (fromSignal aX) (fromSignal bX))
    valOut = toSignal (delayN xpX False enableGen clk (fromSignal valInp))
  in
    mux valOut (fmap Just yX) (pure Nothing)
-}
-- Template Haskell Splice to generate entity name
-- entityNameTH :: String
-- entityNameTH = $(FloPoCoGen.generateFloPoCoEntity "/home/minh/flopoco/build/bin/flopoco"
--                              ["frequency=300", "target=Zynq7000", "IEEEFPAdd", "wE=8", "wF=23","name=plusFloat", "registerLargeTables=1"])

-- plusFloatGen :: String -> Int -> Q Dec
-- plusFloatGen mhz = plusFloatGen mhz

-- --$(plusFloatGen "plusFloat300Mhz" 300) 
--
-- ====>
--
-- plusFloat300Mhz ::
--   Clock dom ->
--   DSignal dom n Float ->
--   DSignal dom n Float ->
--   DSignal dom (n+8) Float
--
{-
plusFloatBBF :: BlackBoxFunction
plusFloatBBF _isD _primName _args _resTys = do
  -- Call Flopoco
  -- do something here to call flopo and get
  --

  -- let entityName = parseEntityName output
  let entityName = "plusFloat"
  
  let meta = emptyBlackBoxMeta {bbKind = TDecl}
      bb = BBFunction (show 'plusFloatTF) 0 (plusFloatTF entityName)
  -- Debugging output
  --trace (show meta) $ return ()
  --trace (show bb) $ return ()
  pure (Right (meta, bb))
-}
{-
plusFloatBBF :: BlackBoxFunction
plusFloatBBF _ _ _ _ = do
  -- Call Flopoco
  -- do something here to call flopo and get
  --

  -- let entityName = parseEntityName output
  let entityName = "plusFloat"
  
 
  -- Debugging output
  --trace (show meta) $ return ()
  --trace (show bb) $ return ()
  pure (Right ((emptyBlackBoxMeta {bbKind = TDecl}), (BBFunction ("plusFloatTF") 0 (plusFloatTF entityName))))
-}

-- --$(generateBlackBoxFunction "plusFloat")
{-
plusFloatTF ::
  HasCallStack =>
  Text ->
  TemplateFunction
plusFloatTF entityName =
  TemplateFunction
    [0..2]
    (const True)
    (plusFloatBBTF entityName)
-}




--plusFloatBBTF _ _ = error "qq"
{-
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
  | otherwise = error $ ppShow bbCtx
-}
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
{-

topEntity2 ::
  Clock XilinxSystem ->
  Signal XilinxSystem Float ->
  Signal XilinxSystem Float ->
  Signal XilinxSystem Float ->
  Signal XilinxSystem (Maybe Float)
topEntity2 clk x y z =
  plusFloatX clk
    (fmap Just x)
    (plusFloatX clk (fmap Just y) (fmap Just z))


-}
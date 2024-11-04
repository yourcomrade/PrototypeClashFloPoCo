{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}


module Tem (
    getPipeDep,
    flopocoPrim,
    genFloPoCoInfoEntity,
    liftInfoEntity,
    floPoCoPath,
    args,
    filePath,
    generateBlackBoxFunction,
    
    
) where

import qualified FloPoCoCall as FPCC
import Lexer
import Control.Monad.State (MonadIO, liftIO)
import Prelude
import Language.Haskell.TH.Syntax
import Data.String.Interpolate (__i)

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


import Clash.Annotations.Primitive (Primitive(..))



floPoCoPath = "/home/minh/flopoco/build/bin/flopoco"
args = ["frequency=300", "target=Zynq7000", "IEEEFPAdd", "wE=8", "wF=23","name=plusFloat", "registerLargeTables=1"]
filePath = "flopoco.vhdl"




getPipeDep:: InfoEntity -> Q Type

getPipeDep infoen = do
    qRunIO $ print infoen

    return ( LitT ( NumTyLit (fromIntegral (convertMaybeToInt(pipedep infoen))))) 
        
genFloPoCoInfoEntity :: String -> [String] -> String -> Q Exp
genFloPoCoInfoEntity floPoCoPath args fileName = do
    output <- liftIO $ FPCC.callFloPoCoWSLWithInput floPoCoPath args ""
    if "Error" `elem` lines output
    then fail "Error in FloPoCo output"
    else do
        result <- processFile getLastInfoEntity fileName
        liftIO $ print result
        case result of
            Just infoentity -> [| $(liftInfoEntity infoentity) |]  -- Lift InfoEntity as expression
            Nothing -> fail "Failed to generate InfoEntity"

-- Helper function to lift InfoEntity to Q Exp
liftInfoEntity :: InfoEntity -> Q Exp
liftInfoEntity (InfoEntity n f p ins outs) =
    [| InfoEntity $(lift n) $(lift f) $(lift p) $(lift ins) $(lift outs) |]
-- Convert a type-level natural number (from a literal) to SNat
--numToSNat :: Int -> Q Type
--numToSNat n = [t| SNat $(LitT (NumTyLit (fromIntegral n))) |]
-- Function to convert Int to SNat type
{-
data Nat = Z | S Nat

data SNat (n :: Nat) where
    SZero :: SNat 'Z
    SSucc :: SNat n -> SNat ('S n)

numToSNat :: Integer -> Q Type
numToSNat n = case n of
    0 -> [t| SNat 'SZero |]
    _ -> let go x = if x == 0 then SZero else SSucc (go (x - 1))
          in [t| SNat $(LitT $ NumTyLit n) |]

-}
flopocoPrim :: Name -> Name -> Primitive
flopocoPrim fprimName ftfName = let
  primName = fprimName
  tfName = ftfName
  in
    InlineYamlPrimitive [minBound..] [__i|
        BlackBoxHaskell:
          name: #{primName}
          templateFunction: #{tfName}
          workInfo: Always
    |]

 -- Function to generate a black box function with dynamic names
generateBlackBoxFunction :: String -> Q [Dec]
generateBlackBoxFunction baseName = do
    let entityName = baseName
    let tfName = mkName (entityName <> "TF")
    let bbfName = mkName (entityName <> "BBF")

     -- Create the black box meta using a template
    --let metaDec = ValD (VarP (mkName "meta")) (NormalB [d| emptyBlackBoxMeta { bbKind = TDecl } |]) []
    --let bbDec = ValD (VarP (mkName "bb")) (NormalB [d| BBFunction (show tfName) 0 (fName entityName) |]) []

    -- Create the function declaration
    let funcDec = FunD bbfName  
            [ Clause [WildP, WildP, WildP, WildP] 
                     (NormalB
                        (AppE
                            (VarE 'pure)
                            (AppE
                                (ConE 'Right)
                                (TupE
                                    [ 
                                    
                                    Just (RecUpdE (VarE 'emptyBlackBoxMeta) [('bbKind, ConE 'TDecl)])
                                    ,Just 
                                    (AppE   
                                        (AppE
                                            (AppE (ConE 'BBFunction) (LitE (StringL (entityName <> "TF") ))) -- 1st arg
                                            (LitE (IntegerL 0)))                                  -- 2nd arg
                                            (AppE (VarE tfName) (LitE (StringL entityName))))     -- 3rd arg
                                    ]
                                )
                            )
                        )
                    )
                     [] 
            ]

    
    -- Create the function signature for the black box function
    let funcSig = SigD bbfName (ConT ''BlackBoxFunction) -- Adjust the type to match the function signature

    -- Return both the type signature and the function definition
    return [funcSig, funcDec]

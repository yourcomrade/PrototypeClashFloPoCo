{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Help (
    genPipeDep,
    floPoCoPath,
    args,
    filePath,
    num,
    flopocoPrim,
    
) where

import qualified FloPoCoCall as FPCC
import Lexer
import Control.Monad.State (MonadIO, liftIO)
import Prelude
import Language.Haskell.TH.Syntax
import Data.String.Interpolate (__i)


import Clash.Annotations.Primitive (Primitive(..))
import Data.String.Interpolate.Types (InterpSegment(Expression))



floPoCoPath = "/home/minh/flopoco/build/bin/flopoco"
args = ["frequency=300", "target=Zynq7000", "IEEEFPAdd", "wE=8", "wF=23","name=plusFloat", "registerLargeTables=1"]
filePath = "flopoco.vhdl"

genPipeDep::MonadIO m => String -> [String] -> String -> m Int

genPipeDep floPoCoPath args filepath = do
    output <- liftIO $ FPCC.callFloPoCoWSLWithInput floPoCoPath args ""
    if "Error" `elem` lines output then
        return (-1)
    else do
        result <- processFile getLastInfoEntity filepath  -- Ensure processFile returns Int
        liftIO $ print result
        case result of
            Just infoentity -> return $ convertMaybeToInt (pipedep infoentity)
            Nothing -> return (-1)
        

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
num:: Q Type
num = do
    qRunIO $ putStrLn "From num Q Type"
    delay <- qRunIO $ genPipeDep floPoCoPath args filePath
    qRunIO $ print delay
    return $ LitT $ NumTyLit $ fromIntegral delay
    --return $ numToSNat $ fromIntegral delay

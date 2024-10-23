{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Help (
    genPipeDep,
    floPoCoPath,
    args,
    filePath,
    num
) where

import qualified FloPoCoCall as FPCC 
import Lexer
import Control.Monad.State (MonadIO, liftIO)
import Prelude
import Language.Haskell.TH.Syntax

genPipeDep::MonadIO m => String -> [String] -> String -> m Int

genPipeDep floPoCoPath args filepath = do
    output <- liftIO $ FPCC.callFloPoCoWSLWithInput floPoCoPath args ""
    if "Error" `elem` lines output then
        return (-1)
    else do
        result <- processFile getLastInfoEntity filepath  -- Ensure processFile returns Int
        return result
        

floPoCoPath = "/home/minh/flopoco/build/bin/flopoco"
args = ["frequency=300", "target=Zynq7000", "IEEEFPAdd", "wE=8", "wF=23","name=plusFloat", "registerLargeTables=1"]
filePath = "flopoco.vhdl"
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
num:: Q Type
num = do
    delay <- qRunIO $ genPipeDep floPoCoPath args filePath
    return $ LitT $ NumTyLit $ fromIntegral delay
    --return $ numToSNat $ fromIntegral delay
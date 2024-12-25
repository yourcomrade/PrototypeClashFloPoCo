
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Tem (
    getPipeDep,
    flopocoPrim,
    genFloPoCoInfoEntity,
    liftInfoEntity,
    floPoCoPath,
    args,
    filePath,
    args2,
    filePath2,
    args3,
    filePath3,
    generateBlackBoxFunction,
    generateTemplateFunction,
    generateBlackBoxTemplateFunction,
    generateBlackBoxTemplateFunctionProd,
    generateBlackBox,
    generateBlackBoxProd,
    
    
) where

import qualified FloPoCoCall as FPCC
import Lexer
import Control.Monad.State (liftIO)
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
import qualified Clash.Netlist.Types as NT
import qualified Clash.Primitives.DSL as DSL
import qualified Clash.Netlist.Id as Id

import Clash.Annotations.Primitive (Primitive(..))
import Data.Text (Text, unpack)
import Control.Monad.State.Lazy (State)
import Text.Show.Pretty(ppShow)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import InfoEn

floPoCoPath = "/home/minh/flopoco/build/bin/flopoco"
args = ["frequency=100", "target=Zynq7000", "IEEEFPAdd", "wE=8", "wF=23","name=plusFloat", "outputFile=flopocoAdd.vhdl","registerLargeTables=1"]
filePath = "flopocoAdd.vhdl"


args2 = ["frequency=100", "target=Zynq7000", "IEEEFPFMA", "wE=8", "wF=23","name=fmaFloat", "outputFile=flopocoFMA.vhdl","registerLargeTables=1"]
filePath2 = "flopocoFMA.vhdl"
args3 = ["frequency=100", "target=Zynq7000", "IEEEFPExp", "wE=8", "wF=23","name=expFloat", "outputFile=flopocoExp.vhdl","registerLargeTables=1"]
filePath3 = "flopocoExp.vhdl"


getPipeDep:: InfoEntity -> Q Type

getPipeDep infoen = do
    qRunIO $ print infoen

    return ( LitT ( NumTyLit (fromIntegral (convertMaybeToInt(pipedep infoen))))) 
        
genFloPoCoInfoEntity :: String -> [String] -> String -> Q Exp
genFloPoCoInfoEntity floPoCoPathName argsName fileName = do
    output <- liftIO $ FPCC.callFloPoCoWithInput floPoCoPathName argsName ""
    
    if "Error" `elem` lines output
    then fail "Error in FloPoCo output"
    else do
        result <- processVHDLFile getLastInfoEntity fileName
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

convertMaybeToInt :: Maybe Int -> Int
convertMaybeToInt = fromMaybe 0

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
                    []]

    
    -- Create the function signature for the black box function
    let funcSig = SigD bbfName (ConT ''BlackBoxFunction) -- Adjust the type to match the function signature

    -- Return both the type signature and the function definition
    return [funcSig, funcDec]

generateTemplateFunction :: Text -> Int -> Q [Dec]
generateTemplateFunction entityName lensignal = do
    let entityNamestr = unpack entityName
    let tfNamestr = entityNamestr <> "TF"
    let bbtfNamestr = entityNamestr <> "BBTF"
    let tfName = mkName tfNamestr
    let entityNameName = mkName entityNamestr
    let bbtfName = mkName bbtfNamestr
    -- Create the function definition
    let funcDec = FunD tfName 
            [ Clause [VarP entityNameName] 
                (NormalB 
                (AppE 
                    (AppE
                        (AppE (ConE 'TemplateFunction) (ListE [LitE (IntegerL (toInteger i) ) | i <- [0..lensignal]])) -- 1st arg
                        (AppE (VarE 'const ) (ConE 'True)))                                              -- 2nd arg
                    (AppE (VarE bbtfName) (VarE entityNameName)))                                           -- 3rd arg
                ) 
                []
            ]

    -- Create the type declaration
    let funcSig = SigD tfName (ForallT [] [ConT ''HasCallStack](AppT (AppT ArrowT  (ConT ''Text)) (ConT ''TemplateFunction)))

    -- Return both the type signature and the function definition
    return [funcSig, funcDec]

toLowercaseList :: [String] -> [String]
toLowercaseList = map lowercaseFirst
  where
    lowercaseFirst []     = []
    lowercaseFirst (x:xs) = toLower x : xs

generateBlackBoxTemplateFunction :: InfoEntity -> Q [Dec]
generateBlackBoxTemplateFunction infoen = do
    let inputNamesListstr = fromMaybe [] (insig infoen)
    let outputNamesListstr = fromMaybe [] (outsig infoen)
    let inputNamesList = (map mkName  ( toLowercaseList inputNamesListstr))
    let outputNamesList = (map mkName (toLowercaseList outputNamesListstr))
    let entityNamestr = fromMaybe "" (name infoen)
    let entityNameName = mkName (entityNamestr)
    let bbtfName = mkName (entityNamestr <> "BBTF")
    let entityNameInststr = (entityNamestr <> "_inst")
    let entityNameInst = mkName entityNameInststr 
    let entityNameInstBlockstr = (entityNamestr <> "_inst_block")
    let entityNameInstBlock = mkName  entityNameInstBlockstr
    let entityNameInstNamestr = (entityNamestr <> "InstName")
    let entityNameInstName = mkName entityNameInstNamestr

    let compInps = mkName "compInps"
    let compOuts = mkName "compOuts"

    let inps = mkName "inps"
    let outs = mkName "outs"
    let bbCtx = mkName "bbCtx"
    let s_0 = mkName "s"
    -- Create the function definition
    -- let funDec = FunD bbtfName [Clause [VarP entityNameName,VarP bbCtx] (GuardedB [(PatG [BindS (ListP [VarP x| x <- inputNamesList]) (AppE (AppE (VarE 'Prelude.map) (VarE 'Prelude.fst)) (AppE (VarE 'DSL.tInputs) (VarE bbCtx))),BindS (ListP [VarP r | r <- outputNamesList]) (AppE (VarE 'DSL.tResults) (VarE bbCtx))],DoE Nothing [BindS (VarP entityNameInstName) (AppE (VarE 'Id.makeBasic) (LitE (StringL entityNameInststr))),LetS [ValD (VarP compInps) (NormalB (ListE [ TupE [Just (LitE (StringL x)), Just (AppE (VarE 'DSL.ety)(VarE y))] | (x, y) <- zip inputNamesListstr inputNamesList]))  [],ValD (VarP compOuts) (NormalB (ListE [TupE [Just (LitE (StringL x)), Just (AppE (VarE 'DSL.ety ) (VarE y))] | (x, y) <- zip outputNamesListstr outputNamesList]) ) []],NoBindS (InfixE (Just (AppE (VarE 'DSL.declaration) (LitE (StringL entityNameInstBlockstr)))) (VarE '($)) (Just (DoE Nothing [NoBindS (AppE (AppE (AppE (VarE 'DSL.compInBlock) (VarE entityNameName)) (VarE compInps)) (VarE compOuts)),LetS [ValD (VarP inps) (NormalB (ListE [TupE [Just (LitE (StringL x)), Just (VarE y)] | (x,y) <- Prelude.zip inputNamesListstr inputNamesList])) [],ValD (VarP outs) (NormalB (ListE [TupE [Just (LitE (StringL x)),Just (VarE y)] | (x,y) <- Prelude.zip outputNamesListstr outputNamesList])) []],NoBindS (AppE (AppE (AppE (AppE (AppE (AppE (VarE 'DSL.instDecl) (ConE 'N.Empty)) (AppE (VarE 'Id.unsafeMake) (VarE entityNameName))) (VarE entityNameInstName)) (ConE '[])) (VarE inps)) (VarE outs))])))]),(NormalG (VarE 'otherwise),AppE (VarE 'error) (AppE (VarE 'Text.Show.Pretty.ppShow) (VarE bbCtx)))]) []]
    let funDec
         = FunD
            bbtfName
             [Clause
                [VarP entityNameName, VarP bbCtx]
                (GuardedB
                   [(PatG
                       [BindS
                          (ListP [VarP x | x <- inputNamesList])
                          (AppE
                             (AppE (VarE 'Prelude.map) (VarE 'Prelude.fst))
                             (AppE (VarE 'DSL.tInputs) (VarE bbCtx))),
                        BindS
                          (ListP [VarP r | r <- outputNamesList])
                          (AppE (VarE 'DSL.tResults) (VarE bbCtx))], 
                     DoE
                       Nothing
                       [BindS
                          (VarP entityNameInstName)
                          (AppE (VarE 'Id.makeBasic) (LitE (StringL entityNameInststr))),
                        LetS
                          [ValD
                             (VarP compInps)
                             (NormalB
                                (ListE
                                   [TupE
                                      [Just (LitE (StringL x)),
                                       Just (AppE (VarE 'DSL.ety) (VarE y))] |
                                      (x, y) <- zip inputNamesListstr inputNamesList]))
                             [],
                           ValD
                             (VarP compOuts)
                             (NormalB
                                (ListE
                                   [TupE
                                      [Just (LitE (StringL x)),
                                       Just (AppE (VarE 'DSL.ety) (VarE y))] |
                                      (x, y) <- zip outputNamesListstr outputNamesList]))
                             []],
                        NoBindS
                          (InfixE
                             (Just
                                (AppE
                                   (VarE 'DSL.declaration) (LitE (StringL entityNameInstBlockstr))))
                             (VarE '($))
                             (Just
                                (DoE
                                   Nothing
                                   [NoBindS
                                      (AppE
                                         (AppE
                                            (AppE (VarE 'DSL.compInBlock) (VarE entityNameName))
                                            (VarE compInps))
                                         (VarE compOuts)),
                                    LetS
                                      [ValD
                                         (VarP inps)
                                         (NormalB
                                            (ListE
                                               [TupE [Just (LitE (StringL x)), Just (VarE y)] |
                                                  (x, y) <- Prelude.zip
                                                              inputNamesListstr inputNamesList]))
                                         [],
                                       ValD
                                         (VarP outs)
                                         (NormalB
                                            (ListE
                                               [TupE [Just (LitE (StringL x)), Just (VarE y)] |
                                                  (x, y) <- Prelude.zip
                                                              outputNamesListstr outputNamesList]))
                                         []],
                                    NoBindS
                                      (AppE
                                         (AppE
                                            (AppE
                                               (AppE
                                                  (AppE
                                                     (AppE (VarE 'DSL.instDecl) (ConE 'NT.Empty))
                                                     (AppE
                                                        (VarE 'Id.unsafeMake)
                                                        (VarE entityNameName)))
                                                  (VarE entityNameInstName))
                                               (ConE '[]))
                                            (VarE inps))
                                         (VarE outs))])))]),
                    (NormalG (VarE 'otherwise), 
                     AppE
                       (VarE 'error)
                       (AppE (VarE 'Text.Show.Pretty.ppShow) (VarE bbCtx)))])
                []]
   
    
    -- let funSig =  SigD bbtfName (ForallT [ PlainTV s_0 SpecifiedSpec ] [ AppT (ConT ''Clash.Backend.Backend) (VarT s_0) ]( AppT (AppT ArrowT (ConT ''Text)) ( AppT (AppT ArrowT (ConT ''Clash.Netlist.Types.BlackBoxContext)) ( AppT (AppT (ConT ''State) (VarT s_0)) (ConT ''Data.Text.Prettyprint.Doc.Extra.Doc)))))
    let funSig
         = SigD
             bbtfName
             (ForallT
                [PlainTV s_0 SpecifiedSpec]
                [AppT (ConT ''Clash.Backend.Backend) (VarT s_0)]
                (AppT
                   (AppT ArrowT (ConT ''Text))
                   (AppT
                      (AppT ArrowT (ConT ''Clash.Netlist.Types.BlackBoxContext))
                      (AppT
                         (AppT (ConT ''State) (VarT s_0))
                         (ConT ''Data.Text.Prettyprint.Doc.Extra.Doc)))))    

    return [funSig, funDec]

lengthMaybeStrings :: Maybe [String] -> Int
lengthMaybeStrings (Just strs) = length strs
lengthMaybeStrings Nothing       = 0 

generateBlackBox:: InfoEntity -> Q [Dec]
generateBlackBox infoen = do
    let entityNameNamestr = "entityName"
    let entityNameName = mkName entityNameNamestr
    let entityNamestr = fromMaybe "" (name infoen)
    let entityName = mkName entityNamestr

    let inputNamesListstr = fromMaybe [] (insig infoen)
    let outputNamesListstr = fromMaybe [] (outsig infoen)
    let inputNamesList = map mkName  ( toLowercaseList inputNamesListstr)
    let outputNamesList = map mkName (toLowercaseList outputNamesListstr)
   
    let bbfNamestr = entityNamestr <> "BBF"
    let bbfName = mkName bbfNamestr
    let tfNamestr = entityNamestr <> "TF"
    let tfName = mkName tfNamestr
    let bbtfNamestr = entityNamestr <> "BBTF"
    let bbtfName = mkName bbtfNamestr


    let entityNameInststr = entityNamestr <> "_inst"
    let entityNameInst = mkName entityNameInststr
    let entityNameInstBlockstr = entityNamestr <> "_inst_block"
    let entityNameInstBlock = mkName  entityNameInstBlockstr
    let entityNameInstNamestr = entityNamestr <> "InstName"
    let entityNameInstName = mkName entityNameInstNamestr

    let compInps = mkName "compInps"
    let compOuts = mkName "compOuts"

    let inps = mkName "inps"
    let outs = mkName "outs"
    let bbCtx = mkName "bbCtx"
    let s = mkName "s"

   

    let lensignal = lengthMaybeStrings (insig infoen)
    -- Create BlackBoxTemplateFunction definition declaration
    let bbtffunDec
         = FunD
            bbtfName
             [Clause
                [VarP entityNameName, VarP bbCtx]
                (GuardedB
                   [(PatG
                       [BindS
                          (ListP [VarP x | x <- inputNamesList])
                          (AppE
                             (AppE (VarE 'Prelude.map) (VarE 'Prelude.fst))
                             (AppE (VarE 'DSL.tInputs) (VarE bbCtx))),
                        BindS
                          (ListP [VarP r | r <- outputNamesList])
                          (AppE (VarE 'DSL.tResults) (VarE bbCtx))], 
                     DoE
                       Nothing
                       [BindS
                          (VarP entityNameInstName)
                          (AppE (VarE 'Id.makeBasic) (LitE (StringL entityNameInststr))),
                        LetS
                          [ValD
                             (VarP compInps)
                             (NormalB
                                (ListE
                                   [TupE
                                      [Just (LitE (StringL x)),
                                       Just (AppE (VarE 'DSL.ety) (VarE y))] |
                                      (x, y) <- zip inputNamesListstr inputNamesList]))
                             [],
                           ValD
                             (VarP compOuts)
                             (NormalB
                                (ListE
                                   [TupE
                                      [Just (LitE (StringL x)),
                                       Just (AppE (VarE 'DSL.ety) (VarE y))] |
                                      (x, y) <- zip outputNamesListstr outputNamesList]))
                             []],
                        NoBindS
                          (InfixE
                             (Just
                                (AppE
                                   (VarE 'DSL.declaration) (LitE (StringL entityNameInstBlockstr))))
                             (VarE '($))
                             (Just
                                (DoE
                                   Nothing
                                   [NoBindS
                                      (AppE
                                         (AppE
                                            (AppE (VarE 'DSL.compInBlock) (VarE entityNameName))
                                            (VarE compInps))
                                         (VarE compOuts)),
                                    LetS
                                      [ValD
                                         (VarP inps)
                                         (NormalB
                                            (ListE
                                               [TupE [Just (LitE (StringL x)), Just (VarE y)] |
                                                  (x, y) <- Prelude.zip
                                                              inputNamesListstr inputNamesList]))
                                         [],
                                       ValD
                                         (VarP outs)
                                         (NormalB
                                            (ListE
                                               [TupE [Just (LitE (StringL x)), Just (VarE y)] |
                                                  (x, y) <- Prelude.zip
                                                              outputNamesListstr outputNamesList]))
                                         []],
                                    NoBindS
                                      (AppE
                                         (AppE
                                            (AppE
                                               (AppE
                                                  (AppE
                                                     (AppE (VarE 'DSL.instDecl) (ConE 'NT.Empty))
                                                     (AppE
                                                        (VarE 'Id.unsafeMake)
                                                        (VarE entityNameName)))
                                                  (VarE entityNameInstName))
                                               (ConE '[]))
                                            (VarE inps))
                                         (VarE outs))])))]),
                    (NormalG (VarE 'otherwise), 
                     AppE
                       (VarE 'error)
                       (AppE (VarE 'Text.Show.Pretty.ppShow) (VarE bbCtx)))])
                []]
    
    -- Create BlackBoxTemplateFunction signature declaration 
    let bbtffunSig
         = SigD
             bbtfName
             (ForallT
                [PlainTV s SpecifiedSpec]
                [AppT (ConT ''Clash.Backend.Backend) (VarT s)]
                (AppT
                   (AppT ArrowT (ConT ''Text))
                   (AppT
                      (AppT ArrowT (ConT ''Clash.Netlist.Types.BlackBoxContext))
                      (AppT
                         (AppT (ConT ''State) (VarT s))
                         (ConT ''Data.Text.Prettyprint.Doc.Extra.Doc)))))    
    -- Create TemplateFunction definition declaration
    let tffuncDec = FunD tfName 
            [ Clause [VarP entityNameName] 
                (NormalB 
                (AppE 
                    (AppE
                        (AppE (ConE 'TemplateFunction) (ListE [LitE (IntegerL (toInteger i) ) | i <- [0..lensignal]])) -- 1st arg
                        (AppE (VarE 'const ) (ConE 'True)))                                              -- 2nd arg
                    (AppE (VarE bbtfName) (VarE entityNameName)))                                           -- 3rd arg
                ) 
                []
            ]

    -- Create TemplateFunction signature declaration
    let tffuncSig = SigD tfName (ForallT [] [ConT ''HasCallStack](AppT (AppT ArrowT  (ConT ''Text)) (ConT ''TemplateFunction)))
    -- Create BlackBoxFunction definition declaration
    let bbffuncDec = FunD bbfName  
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
                                            (AppE (ConE 'BBFunction) (LitE (StringL tfNamestr ))) -- 1st arg
                                            (LitE (IntegerL 0)))                                  -- 2nd arg
                                            (AppE (VarE tfName) (LitE (StringL entityNamestr))))     -- 3rd arg
                                    ]
                                )
                            )
                        )
                    )
                    []]

    
    -- Create the function signature for the black box function
    let bbffuncSig = SigD bbfName (ConT ''BlackBoxFunction) -- Adjust the type to match the function signature
    
    return [bbtffunSig, bbtffunDec, tffuncSig, tffuncDec, bbffuncSig, bbffuncDec]



generateBlackBoxTemplateFunctionProd :: InfoEntity -> Q [Dec]
generateBlackBoxTemplateFunctionProd infoen = do
   let entityNameNamestr = "entityName"
   let entityNameName = mkName entityNameNamestr 
   let entityNamestr = fromMaybe "" (name infoen)
   let entityName = mkName entityNamestr

   let inputNamesListstr = fromMaybe [] (insig infoen )
   let outputNamesListstr = fromMaybe [] (outsig infoen)
   let inputNamesList = map mkName  ( toLowercaseList inputNamesListstr)
   let outputNamesList = map mkName (toLowercaseList outputNamesListstr)
   
   let bbfNamestr = entityNamestr <> "BBF"
   let bbfName = mkName bbfNamestr
   let tfNamestr = entityNamestr <> "TF"
   let tfName = mkName tfNamestr
   let bbtfNamestr = entityNamestr <> "BBTF"
   let bbtfName = mkName bbtfNamestr


   let entityNameInststr = entityNamestr <> "_inst"
   let entityNameInst = mkName entityNameInststr
   let entityNameInstBlockstr = entityNamestr <> "_inst_block"
   let entityNameInstBlock = mkName  entityNameInstBlockstr
   let entityNameInstNamestr = entityNamestr <> "InstName"
   let entityNameInstName = mkName entityNameInstNamestr 

   let compInps = mkName "compInps"
   let compOuts = mkName "compOuts"

   let inps = mkName "inps"
   let outs = mkName "outs"
   let bbCtx = mkName "bbCtx" 
   let s = mkName "s"
   let result = mkName "result"
   let resTyps = mkName "resTyps"
   let declares = mkName "declares"
   let outname = mkName "outname"
   let typ = mkName "typ" 

   

   --let bbtffunDec  =FunD bbtfName [Clause [VarP entityName,VarP bbCtx] (GuardedB [(PatG [BindS (ListP [VarP x | x <- inputNamesList]) (AppE (AppE (VarE 'Prelude.map) (VarE 'Prelude.fst)) (AppE (VarE 'DSL.tInputs) (VarE bbCtx))),BindS (ListP [VarP result]) (AppE (VarE 'DSL.tResults) (VarE bbCtx)),BindS (ConP 'N.Product [] [WildP,WildP,VarP resTyps]) (AppE (VarE 'DSL.ety) (VarE result))],DoE Nothing [BindS (VarP entityNameInstName) (AppE (VarE 'Id.makeBasic) (LitE (StringL entityNameInststr))),LetS [ValD (VarP compInps) (NormalB (ListE [TupE [Just (LitE (StringL x)),Just (AppE (VarE 'DSL.ety) (VarE y))]| (x,y) <- zip inputNamesListstr inputNamesList])) [],ValD (VarP compOuts) (NormalB (AppE (AppE (VarE 'Prelude.zip) (ListE [LitE (StringL x)| x <- outputNamesListstr])) (VarE resTyps))), LetS [ValD (ListP [VarP x| x <- outputNamesList]) (NormalB (VarE declares)) []],NoBindS (InfixE (Just (AppE (AppE (VarE 'DSL.declarationReturn) (VarE bbCtx)) (LitE (StringL entityNameInstBlockstr)))) (VarE '($)) (Just (DoE Nothing [BindS (VarP declares) (AppE (AppE (VarE 'Prelude.mapM) (LamE [TupP [VarP outname,VarP typ]] (AppE (AppE (VarE 'DSL.declare) (VarE outname)) (VarE typ)))) (AppE (AppE (VarE 'Prelude.zip) (ListE [LitE (StringL x)| x <- outputNamesListstr])) (VarE resTyps))),LetS [ValD (ListP [VarP x| x <- outputNamesList]) (NormalB (VarE declares)) []],LetS [ValD (VarP inps) (NormalB (ListE [TupE [Just (LitE (StringL x)),Just (VarE y)]| (x,y) <- zip inputNamesListstr inputNamesList])) [],ValD (VarP outs) (NormalB (ListE [TupE [Just (LitE (StringL x)),Just (VarE y)]| (x,y) <- zip outputNamesListstr outputNamesList])) []], NoBindS (AppE (AppE (AppE (VarE 'DSL.compInBlock) (VarE entityName)) (VarE compInps)) (VarE compOuts)),NoBindS (AppE (AppE (AppE (AppE (AppE (AppE (VarE 'DSL.instDecl) (ConE 'Clash.Netlist.Types.Empty)) (AppE (VarE 'Id.unsafeMake) (VarE entityName))) (VarE entityNameInstName)) (ConE '[])) (VarE inps)) (VarE outs)),NoBindS (AppE (VarE 'pure) (ListE [AppE (AppE (VarE 'DSL.constructProduct) (AppE (VarE 'DSL.ety) (VarE result))) (ListE [VarE x| x <- outputNamesList])]))])))]),(NormalG (VarE 'otherwise),AppE (VarE 'error) (AppE (VarE 'ppShow) (VarE bbCtx)))]) []] 
   
   let bbtffunDec =  FunD bbtfName [Clause
         [VarP entityNameName, VarP bbCtx]
         (GuardedB
            [(PatG 
               [BindS 
                  (ListP [VarP x | x <- inputNamesList]) 
                  (AppE 
                     (AppE (VarE 'Prelude.map) (VarE 'Prelude.fst)) 
                     (AppE (VarE 'DSL.tInputs) (VarE bbCtx))),
               BindS (ListP [VarP result]) (AppE (VarE 'DSL.tResults) (VarE bbCtx)),
               BindS (ConP 'NT.Product [] [WildP,WildP,VarP resTyps]) (AppE (VarE 'DSL.ety) (VarE result))], 
               DoE
                  Nothing 
                  [BindS
                     (VarP entityNameInstName)
                     (AppE (VarE 'Id.makeBasic) (LitE (StringL entityNameInststr))),
                  LetS
                     [ValD
                        (VarP compInps)
                        (NormalB
                        (ListE
                           [TupE [Just (LitE (StringL x)),Just (AppE (VarE 'DSL.ety) (VarE y))]| (x,y) <- zip inputNamesListstr inputNamesList]))
                     [],
                     ValD
                        (VarP compOuts)
                        (NormalB
                        (AppE
                           (AppE
                              (VarE 'zip) (ListE [LitE (StringL x) | x <- outputNamesListstr]))
                           (VarE resTyps)))
                     []],
                  NoBindS
                     (InfixE
                        (Just
                           (AppE
                              (AppE (VarE 'DSL.declarationReturn) (VarE bbCtx))
                              (LitE (StringL entityNameInstBlockstr))))
                        (VarE '($))
                        (Just
                           (DoE
                              Nothing
                              [BindS
                                 (VarP declares)
                                 (AppE
                                    (AppE
                                       (VarE 'Prelude.mapM)
                                       (LamE [TupP [VarP outname,VarP typ]] (AppE (AppE (VarE 'DSL.declare) (VarE outname)) (VarE typ))))
                                    (AppE
                                       (AppE
                                          (VarE 'Prelude.zip) (ListE [LitE (StringL x) | x <- outputNamesListstr]))
                                          (VarE resTyps))),
                              LetS 
                                 [ValD 
                                    (ListP [VarP x| x <- outputNamesList]) 
                                    (NormalB (VarE declares)) []],
                              LetS
                                 [ValD
                                    (VarP inps)
                                    (NormalB
                                       (ListE [TupE [Just (LitE (StringL x)),Just (VarE y)]| (x, y) <- zip inputNamesListstr inputNamesList]))[],
                                 ValD
                                    (VarP outs)
                                    (NormalB
                                       (ListE [TupE [Just (LitE (StringL x)), Just (VarE y)] |(x, y) <- zip outputNamesListstr outputNamesList]))[]],
                              NoBindS
                                 (AppE
                                    (AppE
                                       (AppE (VarE 'DSL.compInBlock) (VarE entityNameName)) (VarE compInps))
                                       (VarE compOuts)),
                              NoBindS
                                 (AppE
                                    (AppE
                                       (AppE
                                          (AppE
                                             (AppE
                                                (AppE (VarE 'DSL.instDecl) (ConE 'Empty))
                                                (AppE (VarE 'Id.unsafeMake) (VarE entityNameName)))
                                             (VarE entityNameInstName))
                                          (ConE '[]))
                                       (VarE inps))
                                    (VarE outs)),
                              NoBindS
                                 (AppE
                                    (VarE 'pure)
                                    (ListE
                                       [AppE
                                          (AppE
                                             (VarE 'DSL.constructProduct) (AppE (VarE 'DSL.ety) (VarE result)))
                                             (ListE [VarE x | x <- outputNamesList])]))])))]),
            (NormalG (VarE 'otherwise), AppE (VarE 'error) (AppE (VarE 'ppShow) (VarE bbCtx)))])
         []]
      
   

   let bbtffunSig = SigD bbtfName (ForallT [PlainTV s SpecifiedSpec] [AppT (ConT ''Clash.Backend.Backend) (VarT s)] (AppT (AppT ArrowT (ConT ''Text)) (AppT (AppT ArrowT (ConT ''Clash.Netlist.Types.BlackBoxContext)) (AppT (AppT (ConT ''State) (VarT s)) (ConT ''Data.Text.Prettyprint.Doc.Extra.Doc))))) 

   return [bbtffunSig, bbtffunDec]
   
generateBlackBoxProd :: InfoEntity -> Q [Dec]
generateBlackBoxProd infoen = do
   let entityNameNamestr = "entityName"
   let entityNameName = mkName entityNameNamestr 
   let entityNamestr = fromMaybe "" (name infoen)
   let entityName = mkName entityNamestr

   let inputNamesListstr = fromMaybe [] (insig infoen )
   let outputNamesListstr = fromMaybe [] (outsig infoen)
   let inputNamesList = map mkName  ( toLowercaseList inputNamesListstr)
   let outputNamesList = map mkName (toLowercaseList outputNamesListstr)
   
   let bbfNamestr = entityNamestr <> "BBF"
   let bbfName = mkName bbfNamestr
   let tfNamestr = entityNamestr <> "TF"
   let tfName = mkName tfNamestr
   let bbtfNamestr = entityNamestr <> "BBTF"
   let bbtfName = mkName bbtfNamestr


   let entityNameInststr = entityNamestr <> "_inst"
   let entityNameInst = mkName entityNameInststr
   let entityNameInstBlockstr = entityNamestr <> "_inst_block"
   let entityNameInstBlock = mkName  entityNameInstBlockstr
   let entityNameInstNamestr = entityNamestr <> "InstName"
   let entityNameInstName = mkName entityNameInstNamestr 

   let compInps = mkName "compInps"
   let compOuts = mkName "compOuts"

   let inps = mkName "inps"
   let outs = mkName "outs"
   let bbCtx = mkName "bbCtx" 
   let s = mkName "s"
   let result = mkName "result"
   let resTyps = mkName "resTyps"
   let declares = mkName "declares"
   let outname = mkName "outname"
   let typ = mkName "typ" 
   
   let lensignal = lengthMaybeStrings (insig infoen)
   
   let bbtffunDec =  FunD bbtfName [Clause
         [VarP entityNameName, VarP bbCtx]
         (GuardedB
            [(PatG 
               [BindS 
                  (ListP [VarP x | x <- inputNamesList]) 
                  (AppE 
                     (AppE (VarE 'Prelude.map) (VarE 'Prelude.fst)) 
                     (AppE (VarE 'DSL.tInputs) (VarE bbCtx))),
               BindS (ListP [VarP result]) (AppE (VarE 'DSL.tResults) (VarE bbCtx)),
               BindS (ConP 'NT.Product [] [WildP,WildP,VarP resTyps]) (AppE (VarE 'DSL.ety) (VarE result))], 
               DoE
                  Nothing 
                  [BindS
                     (VarP entityNameInstName)
                     (AppE (VarE 'Id.makeBasic) (LitE (StringL entityNameInststr))),
                  LetS
                     [ValD
                        (VarP compInps)
                        (NormalB
                        (ListE
                           [TupE [Just (LitE (StringL x)),Just (AppE (VarE 'DSL.ety) (VarE y))]| (x,y) <- zip inputNamesListstr inputNamesList]))
                     [],
                     ValD
                        (VarP compOuts)
                        (NormalB
                        (AppE
                           (AppE
                              (VarE 'zip) (ListE [LitE (StringL x) | x <- outputNamesListstr]))
                           (VarE resTyps)))
                     []],
                  NoBindS
                     (InfixE
                        (Just
                           (AppE
                              (AppE (VarE 'DSL.declarationReturn) (VarE bbCtx))
                              (LitE (StringL entityNameInstBlockstr))))
                        (VarE '($))
                        (Just
                           (DoE
                              Nothing
                              [BindS
                                 (VarP declares)
                                 (AppE
                                    (AppE
                                       (VarE 'Prelude.mapM)
                                       (LamE [TupP [VarP outname,VarP typ]] (AppE (AppE (VarE 'DSL.declare) (VarE outname)) (VarE typ))))
                                    (AppE
                                       (AppE
                                          (VarE 'Prelude.zip) (ListE [LitE (StringL x) | x <- outputNamesListstr]))
                                          (VarE resTyps))),
                              LetS 
                                 [ValD 
                                    (ListP [VarP x| x <- outputNamesList]) 
                                    (NormalB (VarE declares)) []],
                              LetS
                                 [ValD
                                    (VarP inps)
                                    (NormalB
                                       (ListE [TupE [Just (LitE (StringL x)),Just (VarE y)]| (x, y) <- zip inputNamesListstr inputNamesList]))[],
                                 ValD
                                    (VarP outs)
                                    (NormalB
                                       (ListE [TupE [Just (LitE (StringL x)), Just (VarE y)] |(x, y) <- zip outputNamesListstr outputNamesList]))[]],
                              NoBindS
                                 (AppE
                                    (AppE
                                       (AppE (VarE 'DSL.compInBlock) (VarE entityNameName)) (VarE compInps))
                                       (VarE compOuts)),
                              NoBindS
                                 (AppE
                                    (AppE
                                       (AppE
                                          (AppE
                                             (AppE
                                                (AppE (VarE 'DSL.instDecl) (ConE 'Empty))
                                                (AppE (VarE 'Id.unsafeMake) (VarE entityNameName)))
                                             (VarE entityNameInstName))
                                          (ConE '[]))
                                       (VarE inps))
                                    (VarE outs)),
                              NoBindS
                                 (AppE
                                    (VarE 'pure)
                                    (ListE
                                       [AppE
                                          (AppE
                                             (VarE 'DSL.constructProduct) (AppE (VarE 'DSL.ety) (VarE result)))
                                             (ListE [VarE x | x <- outputNamesList])]))])))]),
            (NormalG (VarE 'otherwise), AppE (VarE 'error) (AppE (VarE 'ppShow) (VarE bbCtx)))])
         []]
      
   

   let bbtffunSig = SigD bbtfName (ForallT [PlainTV s SpecifiedSpec] [AppT (ConT ''Clash.Backend.Backend) (VarT s)] (AppT (AppT ArrowT (ConT ''Text)) (AppT (AppT ArrowT (ConT ''Clash.Netlist.Types.BlackBoxContext)) (AppT (AppT (ConT ''State) (VarT s)) (ConT ''Data.Text.Prettyprint.Doc.Extra.Doc))))) 

-- Create TemplateFunction definition declaration
   let tffuncDec = FunD tfName 
            [ Clause [VarP entityNameName] 
                (NormalB 
                (AppE 
                    (AppE
                        (AppE (ConE 'TemplateFunction) (ListE [LitE (IntegerL (toInteger i) ) | i <- [0..lensignal]])) -- 1st arg
                        (AppE (VarE 'const ) (ConE 'True)))                                              -- 2nd arg
                    (AppE (VarE bbtfName) (VarE entityNameName)))                                           -- 3rd arg
                ) 
                []
            ]

    -- Create TemplateFunction signature declaration
   let tffuncSig = SigD tfName (ForallT [] [ConT ''HasCallStack](AppT (AppT ArrowT  (ConT ''Text)) (ConT ''TemplateFunction)))
    -- Create BlackBoxFunction definition declaration
   let bbffuncDec = FunD bbfName  
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
                                            (AppE (ConE 'BBFunction) (LitE (StringL tfNamestr ))) -- 1st arg
                                            (LitE (IntegerL 0)))                                  -- 2nd arg
                                            (AppE (VarE tfName) (LitE (StringL entityNamestr))))     -- 3rd arg
                                    ]
                                )
                            )
                        )
                    )
                    []]

    
    -- Create the function signature for the black box function
   let bbffuncSig = SigD bbfName (ConT ''BlackBoxFunction) -- Adjust the type to match the function signature

   return [bbtffunSig, bbtffunDec, tffuncSig, tffuncDec, bbffuncSig, bbffuncDec]
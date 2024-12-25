{- |
  Copyright   :  (C) 2024, QBayLogic B.V.
  License     :  BSD2
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
  Test driver
-}
module FloPoCoCall(
    callFloPoCoWithInput  -- add this line to export the function
    ) where

import Prelude
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Process
import System.IO (hPutStrLn, hFlush, hGetContents, hClose)
import qualified Control.Monad.Fail as Fail
import System.Info(os)
-- | This function is to call FloPoCo to generate VHDL files from provided input
-- Work on Linux. For Window users, it can be used to call FloPoCo
-- directly from WSL without modification  
--
-- Example usage with IO:
--
-- > main::IO
-- > main = do
-- >  _ <- callFloPoCoWithInput "/home/user/flopoco/build/bin/flopoco"
-- >                              ["frequency=300", "target=Zynq7000", "FPAdd", "wE=8", "wF=23", "name=SinglePrecisionFPAdd", "registerLargeTables=1"]
-- >                              ""
-- >  return ()


callFloPoCoWithInput 
  :: (MonadIO m, Fail.MonadFail m) => 
  String -- ^ The path where FloPoCo is installed or built
  -> [String] -- ^ List of input for FloPoCo
  -> String -- ^ Dummy input
  -> m String -- ^ Return the output generated by FloPoCo
callFloPoCoWithInput floPoCoPath args dummyinput= do
  let (command, finalArgs) = if os == "mingw32"
        then ("wsl", floPoCoPath : args)  -- On Windows, prepend "wsl"
        else (floPoCoPath, args)         -- On Linux, use the binary directly

  -- Create the process with stdin, stdout, stderr pipes
  (Just hin, Just hout, Just herr, _) <- liftIO $ createProcess (proc command finalArgs)
    { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

  -- Feed input to the process
  liftIO $ do
    hPutStrLn hin dummyinput
    hFlush hin
    hClose hin

  -- Read output from stdout
  output <- liftIO $ hGetContents hout

  -- Optionally print the output (for debugging purposes)
  -- liftIO $ putStrLn output

  -- Read error output from stderr (optional, for debugging)
  errOutput <- liftIO $ hGetContents herr
  liftIO $ putStrLn errOutput

  -- Return the actual output as the result
  return output

main3 ::IO()
main3 = do
  _ <- callFloPoCoWithInput "/home/minh/flopoco/build/bin/flopoco"
                               ["frequency=300", "target=Zynq7000", "FPAdd", "wE=8", "wF=23", "name=SinglePrecisionFPAdd", "registerLargeTables=1"]
                               ""
  return ()

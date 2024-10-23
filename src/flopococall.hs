module FloPoCoCall(
    callFloPoCoWSLWithInput  -- add this line to export the function
    ) where

import Prelude (String, IO, Maybe(..), putStrLn, ($), unwords, show, return) 

import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Process
import System.IO (hPutStrLn, hFlush, hGetContents, hClose)
import qualified Control.Monad.Fail

-- Function to call FloPoCo via WSL and feed input
callFloPoCoWSLWithInput :: (MonadIO m, Control.Monad.Fail.MonadFail m) =>  String -> [String] -> String -> m String
callFloPoCoWSLWithInput floPoCoPath args input = do
  -- Create the process with stdin, stdout, stderr pipes
  (Just hin, Just hout, Just herr, _) <- liftIO $ createProcess (proc "wsl" (floPoCoPath : args))
    { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

  -- Feed input to the process
  liftIO $ do
    hPutStrLn hin input
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

-- Example usage
main :: IO ()
main = do
  _ <- callFloPoCoWSLWithInput "/home/minh/flopoco/build/bin/flopoco"
                               ["frequency=300", "target=Zynq7000", "FPAdd", "wE=8", "wF=23", "name=SinglePrecisionFPAdd", "registerLargeTables=1"]
                               ""
  return ()

module Main ( main ) where

import Control.Applicative ( (<$>) )
import System.Directory ( getCurrentDirectory )
import System.Environment ( getArgs )

import qualified CmdLine as CMD
import qualified Database as DB
import qualified Insert as INS

runMode :: CMD.Mode -> IO ()
runMode (CMD.Init mp) = do
  p <- case mp of
    Just ap -> return ap
    Nothing -> getCurrentDirectory
  
  DB.initDb p
  
runMode (CMD.InsertChk files) = mapM_ INS.insertChk files

main :: IO ()
main = do
  mode <- CMD.parseMode <$> getArgs
  
  case mode of
    Left x  -> error x
    Right m -> runMode m

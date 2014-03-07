
module Main ( main ) where

import qualified Data.ByteString.Lazy as BSL
import System.Environment ( getArgs )

import qualified Network.FCP as FCP

insertChk :: FilePath -> IO ()
insertChk file = do
  conn <- FCP.connect "localhost" 9481
  cont <- BSL.readFile file
  FCP.sendRequest conn $ FCP.ClientPutDirect "CHK@" "foo" cont
  FCP.processMessages conn $ \msg -> do
    print msg
    return True
  
parseArgs :: [String] -> IO ()
parseArgs ("insertCHK" : files) = mapM_ insertChk files
parseArgs args = error $ "error parsing options " ++ show args

main :: IO ()
main = getArgs >>= parseArgs

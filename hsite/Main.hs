
module Main ( main ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import Network.Mime ( defaultMimeLookup )
import System.Environment ( getArgs )
import System.IO ( hFlush, stdout )
import Text.Read ( readMaybe )

import qualified Network.FCP as FCP
import Progress

insertChk :: FilePath -> IO ()
insertChk file = do
  conn <- FCP.connect "10.11.0.1" 9481
  cont <- BSL.readFile file
  let mime = Just $ BSC.unpack $ defaultMimeLookup $ Text.pack file
  FCP.sendRequest conn $ FCP.ClientPut "CHK@" mime (Just file) "foo" (FCP.DirectPut cont)
  FCP.processMessages conn $ \msg -> case FCP.msgName msg of
    "URIGenerated" -> clearProgress >> case FCP.msgField "URI" msg of
        Nothing  -> error "got no URI in URIGenerated message?!"
        Just uri -> putStrLn uri >> hFlush stdout >> return False
    "SimpleProgress" -> do
      let 
        cur = FCP.msgField "Succeeded" msg >>= readMaybe
        tot = FCP.msgField "Total" msg >>= readMaybe

      case (cur, tot) of
        (Just c, Just t) -> drawProgress c t
        _                -> return ()

      return True
    _ -> return True
  
parseArgs :: [String] -> IO ()
parseArgs ("insertCHK" : files) = mapM_ insertChk files
parseArgs args = error $ "error parsing options " ++ show args

main :: IO ()
main = getArgs >>= parseArgs

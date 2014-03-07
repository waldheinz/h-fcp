
module Main ( main ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import Network.Mime ( defaultMimeLookup )
import System.Environment ( getArgs )

import qualified Network.FCP as FCP

insertChk :: FilePath -> IO ()
insertChk file = do
  conn <- FCP.connect "localhost" 9481
  cont <- BSL.readFile file
  let mime = Just $ BSC.unpack $ defaultMimeLookup $ Text.pack file
  FCP.sendRequest conn $ FCP.ClientPut "CHK@" mime "foo" (FCP.DirectPut cont)
  FCP.processMessages conn $ \msg -> case FCP.msgName msg of
    "URIGenerated" -> do
      case FCP.msgField "URI" msg of
        Nothing  -> error "got no URI in URIGenerated message?!"
        Just uri -> putStrLn (uri ++ "/" ++ file) >> return False
    _ -> return True
  
parseArgs :: [String] -> IO ()
parseArgs ("insertCHK" : files) = mapM_ insertChk files
parseArgs args = error $ "error parsing options " ++ show args

main :: IO ()
main = getArgs >>= parseArgs


module Insert (
  checkInsertState, insertChk, traverseFiles,
  ) where

import Control.Monad ( forM, unless, when )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA ( bytestringDigest, sha1 )
import Data.Maybe ( catMaybes )
import qualified Data.Text as Text
import qualified Network.FCP as FCP
import Network.Mime ( defaultMimeLookup )
import System.Directory ( doesDirectoryExist, getCurrentDirectory, getDirectoryContents )
import System.FilePath ( (</>) )
import System.IO ( hFileSize, hFlush, stdout, withFile, IOMode(..) )
import Text.Read ( readMaybe )

import qualified Database as DB
import Progress

hashContents :: BSL.ByteString -> BS.ByteString
hashContents = BSL.toStrict . bytestringDigest . sha1

data InsertState
  = Fresh
  | LocalChange 
  | UpToDate String
  deriving ( Show )

checkInsertState :: DB.SiteDb -> FilePath -> IO InsertState
checkInsertState db file = DB.getFileState db file >>= \fs -> case fs of
  Nothing -> return Fresh
  Just state -> case state of
    (size, hash, Just uri, Just _) -> withFile file ReadMode $ \fh -> do
      size' <- hFileSize fh
      if size /= size'
        then return LocalChange
        else do
          cont <- BSL.hGetContents fh
          if hash == hashContents cont
            then return $ UpToDate uri
            else return LocalChange
    _ -> return Fresh
  
traverseFiles :: FilePath -> (FilePath -> IO a) -> IO [a]
traverseFiles fp act = do
  cs <- getDirectoryContents fp
  rs <- forM cs $ \c -> doesDirectoryExist (fp </> c) >>= \dir ->
    if dir
    then if (c == "." || c == ".." || c == ".hsite")
         then return []
         else traverseFiles (fp </> c) act
    else act (fp </> c) >>= \r -> return [r]

  return $ concat rs

insertChk :: DB.SiteDb -> FilePath -> IO ()
insertChk db fn = do
  conn <- FCP.connect "hsite" "127.0.0.1" 9481
  withFile fn ReadMode $ \fh -> do
    cont <- BSL.hGetContents fh
    size <- hFileSize fh
    
    let
      mime = Just $ BSC.unpack $ defaultMimeLookup $ Text.pack fn
      fi = (fn, size, BSL.toStrict $ bytestringDigest $ sha1 cont)

    DB.needsInsert db fi >>= \ni -> when ni $ do
      DB.addFile db fi
      FCP.sendRequest conn $ FCP.ClientPut "CHK@" mime (Just fn) "foo" (FCP.DirectPut cont)
      FCP.processMessages conn $ \msg -> case FCP.msgName msg of
        "URIGenerated" -> clearProgress >> case FCP.msgField "URI" msg of
            Nothing  -> error "got no URI in URIGenerated message?!"
            Just uri -> putStrLn uri >> hFlush stdout >> DB.updateFileUri db fn uri >> return True
        "SimpleProgress" -> do
          let
            cur = FCP.msgField "Succeeded" msg >>= readMaybe
            tot = FCP.msgField "Total" msg >>= readMaybe

          case (cur, tot) of
            (Just c, Just t) -> drawProgress c t
            _                -> return ()
          return True
        "PutSuccessful" -> DB.insertDone db fn >> return False
        "PutFailed" -> putStrLn "put FAILED" >> return False
        _ -> return True

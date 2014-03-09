
module Insert (
  InsertState(..), checkInsertState, insertChk, insertSite, traverseFiles,
  ) where

import Control.Monad ( forM, forM_, when )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA ( bytestringDigest, sha1 )
import Data.IORef
import qualified Data.Text as Text
import qualified Network.FCP as FCP
import Network.Mime ( defaultMimeLookup )
import System.Directory ( doesDirectoryExist, getDirectoryContents )
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

insertSite :: DB.SiteDb -> IO ()
insertSite db = do
  todo <- traverseFiles (DB.siteBasePath db) $ \file -> do
    p <- DB.relPath db file
    checkInsertState db file >>= \st -> case st of
      UpToDate uri -> return (file, (p, fileMime file, FCP.RedirectPut uri))
      _ -> withFile file ReadMode $ \fh -> do
        cont <- BSL.hGetContents fh
        size <- hFileSize fh
        DB.addFile db (file, size, hashContents cont)
        return (file, (p, fileMime file, FCP.DirectPut cont))

  conn <- FCP.connect "hsite-insert" "127.0.0.1" 9481
  FCP.sendRequest conn $ FCP.ClientPutComplexDir "CHK@" "bar" (Just "index.html") $ map snd todo
  trackPutProgress conn >>= \result -> case result of
    PutFailed -> putStrLn "ouch, put failed"
    PutSuccess uri -> forM_ todo $ \(file, (p, _, t)) -> case t of
      FCP.RedirectPut _ -> return ()
      _                 -> DB.insertDone db file $ uri ++ "/" ++ p
    
fileMime :: FilePath -> String
fileMime = BSC.unpack . defaultMimeLookup . Text.pack

data PutResult
  = PutSuccess String
  | PutFailed

trackPutProgress :: FCP.Connection -> IO PutResult
trackPutProgress conn = do
  result <- newIORef PutFailed
  
  FCP.processMessages conn $ \msg -> case FCP.msgName msg of
    "URIGenerated" -> clearProgress >> case FCP.msgField "URI" msg of
      Nothing  -> error "got no URI in URIGenerated message?!"
      Just uri -> putStrLn uri >> hFlush stdout >> return True
    
    "SimpleProgress" -> do
      let
        cur = FCP.msgField "Succeeded" msg >>= readMaybe
        tot = FCP.msgField "Total" msg >>= readMaybe

      case (cur, tot) of
        (Just c, Just t) -> drawProgress c t
        _                -> return ()
      return True
      
    "PutSuccessful" -> do
      case FCP.msgField "URI" msg of
        Nothing  -> putStrLn "no URI in PutSuccess message?!"
        Just uri -> writeIORef result $ PutSuccess uri
      return False
      
    "PutFailed" -> putStrLn "put FAILED" >> return False
    _ -> return True

  readIORef result

insertChk :: DB.SiteDb -> FilePath -> IO ()
insertChk db fn = do
  conn <- FCP.connect "hsite" "127.0.0.1" 9481
  withFile fn ReadMode $ \fh -> do
    cont <- BSL.hGetContents fh
    size <- hFileSize fh
    
    let
      mime = Just $ fileMime fn
      fi = (fn, size, hashContents cont)

    DB.needsInsert db fi >>= \ni -> when ni $ do
      DB.addFile db fi
      FCP.sendRequest conn $ FCP.ClientPut "CHK@" mime Nothing "foo" (FCP.DirectPut cont)
      trackPutProgress conn >>= \result -> case result of
        PutFailed -> putStrLn "put failed, sorry"
        PutSuccess uri -> DB.insertDone db fn uri

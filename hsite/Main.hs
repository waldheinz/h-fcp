
module Main ( main ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( forM )
import qualified Network.FCP as FCP
import System.Directory ( getCurrentDirectory, makeRelativeToCurrentDirectory )
import System.Environment ( getArgs )
import System.Posix ( getFileStatus, fileSize, FileOffset )
import Text.Printf ( printf )

import qualified CmdLine as CMD
import qualified Database as DB
import qualified Insert as INS

getNode :: IO (String, Int)
getNode = return ("127.0.0.1", 9481)

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)

runMode :: CMD.Mode -> IO ()
runMode (CMD.GenKeys name) = DB.withDb $ \db -> do
  mk <- DB.loadKeys db
  case mk of
    Just _  -> putStrLn "you already have generated keys"
    Nothing -> do
      (host, port) <- getNode
      c <- FCP.connect "hsite-genkey" host port
      FCP.sendRequest c $ FCP.GenerateSsk Nothing
      FCP.processMessages c $ \msg -> case FCP.msgName msg of
        "SSKKeypair" -> do
          
          let
            iuri = FCP.msgField "InsertURI" msg
            ruri = FCP.msgField "RequestURI" msg
        
          case (iuri, ruri) of
            (Just i, Just r) -> DB.saveKeys db (name, 0, i, r)
            _                -> error "message did not contain URIs?!"

          return False
        _ -> return True
        
runMode (CMD.Init mp) = do
  p <- case mp of
    Just ap -> return ap
    Nothing -> getCurrentDirectory
  
  DB.initDb p

runMode (CMD.Insert chk) = DB.withDb $ \db ->
  if chk
  then INS.insertSite db "CHK@"
  else DB.loadKeys db >>= \k -> case k of
    Nothing -> error "you can only do --chk inserts until you generated keys"
    Just (name, rev, iuri, ruri) -> do
      let uri = (iuri ++ name ++ "-" ++ show rev)
      INS.insertSite db uri
      DB.saveKeys db (name, rev + 1, iuri, ruri)
      putStrLn $ "your latest revision is at:"
      putStrLn $ "USK" ++ (drop 3 ruri) ++ name ++ "/" ++ show rev
      
runMode (CMD.InsertFiles files) = DB.withDb $ \db -> mapM_ (INS.insertChk db) files
runMode (CMD.Status) = DB.withDb $ \db -> do
  list <- INS.traverseFiles (DB.siteBasePath db) $ \file -> do
    st <- INS.checkInsertState db file
    sz <- getFileSize file
    return (file, st, sz)
  
  putStrLn $ "Files needing an insert:"

  let
    pretty sz
      | sz < 1024               = s 1 ++ " B"
      | sz < 1024 * 1024        = s 1024 ++ " KiB"
      | sz < 1024 * 1024 * 1024 = (s (1024 * 1024)) ++ " MiB"
      | otherwise = (s (1024 * 1024 * 1024)) ++ " GiB"
      where
        s :: Int -> String
        s d = printf "%.2f" (fromIntegral sz / (fromIntegral d :: Float))
        
  tot <- forM list $ \(file, st, sz) -> do
    p <- makeRelativeToCurrentDirectory file
    case st of
      INS.UpToDate _  -> return 0
      INS.Fresh       -> (putStrLn $ "     fresh: " ++ p ++ " (" ++ pretty sz ++ ")") >> return sz
      INS.LocalChange -> (putStrLn $ "  modified: " ++ p ++ " (" ++ pretty sz ++ ")") >> return sz

  putStrLn $ (pretty $ sum tot) ++ " in " ++ (show $ length tot) ++ " files."
  
main :: IO ()
main = do
  mode <- CMD.parseMode <$> getArgs
  
  case mode of
    Left x  -> error x
    Right m -> runMode m

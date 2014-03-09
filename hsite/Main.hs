
module Main ( main ) where

import Control.Applicative ( (<$>) )
import qualified Network.FCP as FCP
import System.Directory ( getCurrentDirectory )
import System.Environment ( getArgs )

import qualified CmdLine as CMD
import qualified Database as DB
import qualified Insert as INS

getNode :: IO (String, Int)
getNode = return ("127.0.0.1", 9481)

runMode :: CMD.Mode -> IO ()
runMode (CMD.GenKeys) = DB.withDb $ \db -> do
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
            (Just i, Just r) -> DB.saveKeys db (i, r)
            _                -> error "message did not contain URIs?!"

          return False
        _ -> return True
        
runMode (CMD.Init mp) = do
  p <- case mp of
    Just ap -> return ap
    Nothing -> getCurrentDirectory
  
  DB.initDb p

runMode (CMD.InsertChk) = DB.withDb $ \db -> INS.insertSite db
runMode (CMD.InsertFiles files) = DB.withDb $ \db -> mapM_ (INS.insertChk db) files
runMode (CMD.Status) = DB.withDb $ \db -> do
  _ <- INS.traverseFiles (DB.siteBasePath db) $ \file -> do
    INS.checkInsertState db file >>= \st -> putStrLn $ file ++ ": " ++ show st
  return ()
  
main :: IO ()
main = do
  mode <- CMD.parseMode <$> getArgs
  
  case mode of
    Left x  -> error x
    Right m -> runMode m

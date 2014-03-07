
module Network.FCP (
  Connection, connect, processMessages,
  
  -- * Client Requests
  ClientRequest(..), sendRequest, getNode,
  
  -- * Raw Messages
  Message, msgName, msgFields, msgPayload
  ) where

import qualified Data.ByteString.Lazy as BSL
import Data.Char ( toLower )
import qualified Data.Map.Strict as Map
import Network
import System.IO
import Text.Printf

data Connection = Conn
     { cHandle :: Handle
     }

write :: Handle -> String -> IO ()
write h s = do
  hPrintf h "%s\n" s
--  printf "> %s\n" s

sendMessage :: Connection -> Message -> IO ()
sendMessage c (Message name fields payload) = do
  let
    h = cHandle c
    field (n, v) = write h $ n ++ "=" ++ v
    
  write h name
  mapM_ field $ Map.toList fields

  case payload of
    Nothing -> write h "EndMessage"
    Just pl -> do
      field ("DataLength", show $ BSL.length pl)
      write h "Data"
      BSL.hPut h pl
      
  hFlush h
    
connect :: String -> Int -> IO Connection
connect host port = do
  h <- connectTo host (PortNumber $ fromIntegral port)
  hSetBuffering h NoBuffering
  let
    c = Conn h

  sendMessage c $ Message "ClientHello" (Map.fromList
    [ ("Name", "hfcp"), ("ExpectedVersion", "2.0") ]) Nothing

  return c

readln :: Connection -> IO String
readln c = do
  let
    h = cHandle c
  s <- hGetLine h
--  putStrLn $ "< " ++ s
  return s

getNode :: Connection -> Bool -> Bool -> IO ()
getNode c priv vol = sendMessage c $ Message "GetNode" (Map.fromList
                     [ ("WithPrivate", map toLower $ show priv)
                     , ("WithVolatile", map toLower $ show vol)
                     ])
                     Nothing

data Message = Message
                  { msgName    :: String
                  , msgFields  :: (Map.Map String String)
                  , msgPayload :: Maybe BSL.ByteString
                  } deriving ( Show )

mkMessage :: String -> [(String, String)] -> Maybe BSL.ByteString -> Message
mkMessage name fields payload = Message name (Map.fromList fields) payload

readMessage :: Connection -> IO Message
readMessage c = do
  name <- readln c
  (fields, hasData) <- readFields
  
  let
    msg = mkMessage name fields
    
  if hasData
     then case lookup "DataLength" fields of
       Nothing  -> error $ "binary message " ++ name ++ "but no DataLength given"
       Just dls -> BSL.hGet (cHandle c) (read dls) >>= \d -> return $ msg (Just d)
    else return $ msg Nothing

  where
    readFields = go [] where
      go xs = readln c >>= \ln -> case ln of
        "EndMessage" -> return (xs, False)
        "Data"       -> return (xs, True)
        _            -> go (parse ln : xs)
      parse ln = let (n, v) = break (== '=') ln in (n, tail v)

processMessages :: Connection -> (Message -> IO Bool) -> IO ()
processMessages c fn = do
  m <- readMessage c
  more <- fn m
  if more then processMessages c fn else return () 

data ClientRequest =
  ClientPutDirect
  { cpdUri        :: String
  , cpdIdentifier :: String
  , cpdData       :: BSL.ByteString
  }

sendRequest :: Connection -> ClientRequest -> IO ()
sendRequest c (ClientPutDirect uri ident d) = sendMessage c $
  mkMessage "ClientPut" [("URI", uri), ("Identifier", ident), ("Global", "true")] (Just d)
  

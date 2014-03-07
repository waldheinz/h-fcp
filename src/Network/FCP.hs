
module Network.FCP (
  Connection, connect, processMessages,
  
  -- * Client Requests
  ClientRequest(..), sendRequest, getNode,
  
  -- * Raw Messages
  Message, msgName, msgFields, msgPayload
  ) where

import Control.Monad ( forever )
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

readMessage :: Connection -> IO [(String, String)]
readMessage c = go [] where
  go xs = readln c >>= \ln -> case ln of
    "EndMessage" -> return xs
    fv -> go xs >>= \rest -> return $ (parse ln) : rest
  parse ln = let (n, v) = break (== '=') ln in (n, tail v)

processMessages :: Connection -> (Message -> IO Bool) -> IO ()
processMessages c fn = do
  n <- readln c
  m <- case n of
    "NodeHello" -> readMessage c
    "NodeData" -> readMessage c
    _ -> error $ "unknown message " ++ n
  
  more <- fn $ Message n (Map.fromList m) Nothing
  if more then processMessages c fn else return () 

data ClientRequest =
  ClientPutDirect
  { cpdUri        :: String
  , cpdIdentifier :: String
  , cpdData       :: BSL.ByteString
  }

sendRequest :: Connection -> ClientRequest -> IO ()
sendRequest c (ClientPutDirect uri id d) = return ()
  


module Network.FCP (
  Connection, connect, processMessages,
  -- * Client Requests

  getNode,
  
  -- * Raw Messages
  RawMessage, rawMsgName, rawMsgMap
  ) where

import Control.Monad ( forever )
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

sendMessage :: Connection -> String -> [(String, String)] -> IO ()
sendMessage c name fields = do
  let
    h = cHandle c
    field (n, v) = write h $ n ++ "=" ++ v
    
  write h name
  mapM_ field fields
  write h "EndMessage"
  hFlush h
    
connect :: String -> Int -> IO Connection
connect host port = do
  h <- connectTo host (PortNumber $ fromIntegral port)
  hSetBuffering h NoBuffering
  let
    c = Conn h

  sendMessage c "ClientHello"
    [ ("Name", "hfcp"), ("ExpectedVersion", "2.0") ]

  return c

readln :: Connection -> IO String
readln c = do
  let
    h = cHandle c
  s <- hGetLine h
--  putStrLn $ "< " ++ s
  return s

getNode :: Connection -> Bool -> Bool -> IO ()
getNode c priv vol = sendMessage c "GetNode"
                     [ ("WithPrivate", map toLower $ show priv)
                     , ("WithVolatile", map toLower $ show vol)
                     ]

data RawMessage = RawMessage
                  { rawMsgMap  :: (Map.Map String String)
                  , rawMsgName :: String
                  } deriving ( Show )

readMessage :: Connection -> IO [(String, String)]
readMessage c = go [] where
  go xs = readln c >>= \ln -> case ln of
    "EndMessage" -> return xs
    fv -> go xs >>= \rest -> return $ (parse ln) : rest
  parse ln = let (n, v) = break (== '=') ln in (n, tail v)

processMessages :: Connection -> (RawMessage -> IO Bool) -> IO ()
processMessages c fn = do
  n <- readln c
  m <- case n of
    "NodeHello" -> readMessage c
    "NodeData" -> readMessage c
    _ -> error $ "unknown message " ++ n
  
  more <- fn $ RawMessage (Map.fromList m) n
  if more then processMessages c fn else return () 
  


module Main (
  main
  ) where

import Control.Monad ( forM_ )
import qualified Data.Map.Strict as Map
import System.Environment ( getArgs, getProgName )

import qualified Network.FCP as FCP

server = "10.11.0.1"
port = 9481

data ValueType = Counter | Gauge
type ValueSet = (String, String, [(String, String, ValueType, String)])

bandwidth :: ValueSet
bandwidth = ("Bandwidth", "rate",
  [ ("rate_in" , "input rate", Gauge, "totalInputRate")
  , ("rate_out", "output rate", Gauge, "totalOutputRate")
  ])

fetchCount :: ValueSet
fetchCount = ("Fetch Count", "# of fetches",
              [ ("chk_local" , "CHK local" , Counter, "chkLocalFetchCount" )
              , ("chk_remote", "CHK remote", Counter, "chkRemoteFetchCount")
              , ("ssk_local" , "SSK local" , Counter, "sskLocalFetchCount" )
              , ("ssk_remote", "SSK remote", Counter, "sskRemoteFetchCount")
              ])

fetchSuccess :: ValueSet
fetchSuccess = ("Fetch Success", "P(success)",
                [ ("chk_local" , "CHK local" , Gauge, "chkLocalFetchPSuccess" )
                , ("chk_remote", "CHK remote", Gauge, "chkRemoteFetchPSuccess")
                , ("ssk_local" , "SSK local" , Gauge, "sskLocalFetchPSuccess" )
                , ("ssk_remote", "SSK remote", Gauge, "sskRemoteFetchPSuccess")
                ])

printConfig :: ValueSet -> IO ()
printConfig (t, vl, vs) = do
  putStrLn $ "graph_category freenet"
  putStrLn $ "graph_title " ++ t
  putStrLn $ "graph_vlabel " ++ vl
  forM_ vs $ \(n, l, t, _) -> do
    putStrLn $ n ++ ".label " ++ l
    case t of
      Gauge -> putStrLn $ n ++ ".type GAUGE"
      Counter -> do
        putStrLn $ n ++ ".type DERIVE"
        putStrLn $ n ++ ".min 0"
      
printStats :: ValueSet -> FCP.RawMessage -> IO ()
printStats (_, _, vs) m = forM_ vs $ \(n, _, t, vn) ->
  putStrLn $ n ++ ".value " ++ ((FCP.rawMsgMap m) Map.! ("volatile." ++ vn))

printValues :: ValueSet -> IO ()
printValues vs = do
  c <- FCP.connect server port
  FCP.processMessages c $ \rm -> do
    case FCP.rawMsgName rm of
      "NodeHello" -> FCP.getNode c False True >> return True
      "NodeData" -> printStats vs rm >> return False
      x -> error $ "can't deal with " ++ x

main :: IO ()
main = do
  vs <- getProgName >>= \pn -> return $ case pn of
    "fn_fetch_count"   -> fetchCount
    "fn_fetch_success" -> fetchSuccess
    _                  -> bandwidth
  args <- getArgs
  
  case args of
    ["config"] -> printConfig vs
    _ -> printValues vs


module Main (
  main
  ) where

import Control.Monad ( forM_ )
import qualified Data.Map.Strict as Map
import System.Environment ( getArgs )

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

printConfig :: ValueSet -> IO ()
printConfig (t, vl, vs) = do
  putStrLn $ "graph_category freenet"
  putStrLn $ "graph_title " ++ t
  putStrLn $ "graph_vlabel " ++ vl
  forM_ vs $ \(n, l, t, _) -> do
    putStrLn $ n ++ ".label " ++ l
    putStrLn $ n ++ ".type " ++ case t of
      Gauge   -> "GAUGE"
      Counter -> "COUNTER"
      
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
  let vs = bandwidth
  args <- getArgs
  
  case args of
    ["config"] -> printConfig vs
    _ -> printValues vs

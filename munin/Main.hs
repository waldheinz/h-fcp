
module Main (
  main
  ) where

import Control.Monad ( forM_ )
import Data.Maybe ( fromMaybe )
import qualified Data.Map.Strict as Map
import System.Environment ( getArgs, getEnvironment, getProgName )

import qualified Network.FCP as FCP

data ValueType = Counter | Gauge
type ValueSet = (String, String, [(String, String, ValueType, String)])

bandwidth :: ValueSet
bandwidth = ("Bandwidth Usage", "rate",
  [ ("rate_in" , "input rate", Gauge, "totalInputRate")
  , ("rate_out", "output rate", Gauge, "totalOutputRate")
  , ("recent_rate_in", "recent input rate", Gauge, "recentInputRate")
  , ("recent_rate_out", "recent output rate", Gauge, "recentOutputRate")
  ])
            
bandwidthDelays :: ValueSet
bandwidthDelays = ("BW Limit Delays", "delay in [ms]",
                   [ ("delay",      "Delay", Gauge, "bwlimitDelayTime"),
                     ("delay_rt",   "Delay", Gauge, "bwlimitDelayTimeRT"),
                     ("delay_bulk", "Delay", Gauge, "bwlimitDelayTimeBulk")
                   ])
                  
cacheAccessChk :: ValueSet
cacheAccessChk = ("CHK Cache Access Rates", "requests per ${graph_period}",
                  [ ("chk_cache_access", "CHK Cache Accesses", Counter, "cacheAccesses")
                  , ("chk_cache_hits",   "CHK Cache Hits",     Counter, "cachedStoreHits")
                  , ("chk_cache_writes", "CHK Cache Writes",   Counter, "cachedStoreWrites")
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

remotes :: ValueSet
remotes = ("Remote Transfers", "# of remotes",
           [ ("chk_insert" , "CHK inserts" , Gauge, "numberOfRemoteCHKInserts")
           , ("chk_request", "CHK requests", Gauge, "numberOfRemoteCHKRequests")
           , ("ssk_insert" , "SSK inserts" , Gauge, "numberOfRemoteSSKInserts")
           , ("ssk_request", "SSK requests", Gauge, "numberOfRemoteSSKRequests")
           ])

opennetSize :: ValueSet
opennetSize = ("Opennet Size Estimate", "nodes",
               [ ("sz_024_hrs", "24 hours" , Gauge, "opennetSizeEstimate24hourRecent")
               , ("sz_048_hrs", "48 hours" , Gauge, "opennetSizeEstimate48hourRecent")
               , ("sz_072_hrs", "72 hours" , Gauge, "opennetSizeEstimate72hourRecent")
               , ("sz_096_hrs", "96 hours" , Gauge, "opennetSizeEstimate96hourRecent")
               , ("sz_120_hrs", "120 hours", Gauge, "opennetSizeEstimate120hourRecent")
               , ("sz_144_hrs", "144 hours", Gauge, "opennetSizeEstimate144hourRecent")
               ])

storeAccessChk :: ValueSet
storeAccessChk = ("CHK Store Access Rates", "requests",
                  [ ("chk_store_access", "CHK Store Accesses", Counter, "storeAccesses")
                  , ("chk_store_hits",   "CHK Store Hits",     Counter, "storeHits")
                  , ("chk_store_writes", "CHK Store Writes",   Counter, "storeWrites")
                  ])

storeSuccess :: ValueSet
storeSuccess = ("Store Success Rates", "hit %",
                [ ("chk_cache", "CHK Cache", Gauge, "percentCachedStoreHitsOfAccesses")
                , ("chk_store", "CHK Store", Gauge, "percentStoreHitsOfAccesses")
                ])
               
threadCount :: ValueSet
threadCount = ("Thread Count", "threads",
               [ ("threads_running", "Running Threads", Gauge, "runningThreadCount") ])
              
allValueSets :: [(String, ValueSet)]
allValueSets =
  [ ("bandwidth",        bandwidth)
  , ("bw_limit_delay",   bandwidthDelays)
  , ("cache_access_chk", cacheAccessChk)
  , ("fetch_count",      fetchCount)
  , ("fetch_success",    fetchSuccess)
  , ("opennet_size",     opennetSize)
  , ("remotes",          remotes)
  , ("store_access_chk", storeAccessChk)
  , ("store_success",    storeSuccess)
  , ("threads",          threadCount)
  ]
  
printConfig :: ValueSet -> IO ()
printConfig (t, vl, vs) = do
  putStrLn   "graph_category freenet"
  putStrLn $ "graph_title " ++ t
  putStrLn $ "graph_vlabel " ++ vl
  forM_ vs $ \(n, l, tp, _) -> do
    putStrLn $ n ++ ".label " ++ l
    case tp of
      Gauge -> putStrLn $ n ++ ".type GAUGE"
      Counter -> do
        putStrLn $ n ++ ".type DERIVE"
        putStrLn $ n ++ ".min 0"
      
printStats :: ValueSet -> FCP.Message -> IO ()
printStats (_, _, vs) m = forM_ vs $ \(n, _, _, vn) ->
  putStrLn $ n ++ ".value " ++ (FCP.msgFields m Map.! ("volatile." ++ vn))
  
printValues :: ValueSet -> IO ()
printValues vs = do
  (host, port) <- getTarget
  c <- FCP.connect "hmunin" host port
  FCP.processMessages c $ \rm -> case FCP.msgName rm of
    "NodeHello" -> FCP.sendRequest c (FCP.GetNode False True) >> return True
    "NodeData"  -> printStats vs rm >> return False
    x           -> error $ "can't deal with " ++ x

progNamePrefix :: String
progNamePrefix = "fn_"

showProgNameHelp :: String -> IO ()
showProgNameHelp cname = do

  putStrLn "The executable name decides which statistics are generated. Possible values are:"
  putStrLn ""

  forM_ allValueSets $ \(pn, (desc, _, _)) ->
    putStrLn $ progNamePrefix ++ pn ++ " -> " ++ desc

  putStrLn ""
  putStrLn $ "You see, \"" ++ cname ++ "\" is not among them. Maybe you want to create a symlink?"

getTarget :: IO (String, Int)
getTarget = do
  env <- getEnvironment

  let
    host = fromMaybe "localhost" (lookup "host" env)
    port = read $ fromMaybe "9481" (lookup "port" env)

  return (host, port)

main :: IO ()
main = do
  pn <- getProgName

  case lookup (drop (length progNamePrefix) pn) allValueSets of
    Nothing -> showProgNameHelp pn
    Just vs -> getArgs >>= \args -> case args of
      ["config"]  -> printConfig vs
      _           -> printValues vs

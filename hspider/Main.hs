{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy      as BSL
import           Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.HashMap.Strict       as HMS
import           Data.IORef                (atomicModifyIORef, modifyIORef',
                                            newIORef, readIORef, writeIORef)
import           Data.String               (fromString)
import qualified Network.FCP               as FCP
import           System.Directory          (createDirectoryIfMissing, doesFileExist)
import           System.FilePath           (takeDirectory, (</>), makeRelative)
import           Text.HTML.TagSoup         (fromAttrib, parseTags, (~==))

data State = State
    {
        fcpConn :: FCP.Connection,
        nextId  :: Integer
    }

extractLinks :: String -> BSL.ByteString -> [ String ]
extractLinks baseUrl bs = links' where
    tags = parseTags bs
    links = map (fromAttrib "href") $ filter (~== ("<a>" :: String)) tags
    links' =  map (\l -> baseUrl </> (toString l)) links

data FetchJob = FetchJob
    { uri :: String
    }

outBase :: String
outBase = "hspider-store"

main :: IO ()
main = do
    putStrLn "hspider starting"
    c <- FCP.connect "hspider" "localhost" 9481
    todo <- newIORef $ [ startUrl ]
    running <- newIORef $ HMS.empty
    nextId <- newIORef (1 :: Integer )

    let
        storeFile url = outBase ++ "/" ++ url

        startOne = do
            tds <- readIORef todo
            case tds of
                [] -> putStrLn "nothing to do"
                (x:xs) -> do
                    let
                        sf = storeFile x

                    exists <- doesFileExist sf
                    writeIORef todo xs

                    if exists
                        then do
                            putStrLn $ "already have " ++ x
                        else do
                            i <- atomicModifyIORef nextId (\x -> (x + 1, x))
                            modifyIORef' running (\r -> HMS.insert (show i) x r)
                            putStrLn $ "starting " ++ x
                            FCP.sendRequest c $ FCP.ClientGet x (show i)

        saveFile url bs = do
            let
                fname = storeFile url

            createDirectoryIfMissing True (takeDirectory fname)
            BSL.writeFile fname bs

    FCP.processMessages c $ \msg -> case FCP.msgName msg of
        "AllData" -> do
            case FCP.msgPayload msg of
                Nothing -> putStrLn "got AllData without payload?!"
                Just bs -> do
                    case FCP.msgField "Identifier" msg of
                        Nothing -> putStrLn "got AllData without identifier?!"
                        Just i -> do
                            murl <- atomicModifyIORef running (\r -> (HMS.delete i r, HMS.lookup i r))
                            case murl of
                                Nothing -> putStrLn $ "could not find download " ++ i
                                Just url -> do
                                    saveFile url bs
                                    case FCP.msgField "Metadata.ContentType" msg of

                                        Just "text/html" -> do
                                            modifyIORef' todo (\t -> (t ++ extractLinks (takeDirectory url) bs))
                                            startOne
                                        _ -> return ()

            return True

        _ -> do
            -- putStrLn $ show msg
            startOne
            return True

    return ()

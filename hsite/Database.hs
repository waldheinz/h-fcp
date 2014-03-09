
{-# LANGUAGE OverloadedStrings #-}

module Database (
  SiteDb, withDb, initDb,
  
  -- * working with the DB
  needsInsert, addFile, updateFileUri, insertDone
  
  ) where

import Control.Applicative ( (<$>) )
import Control.Exception ( finally )
import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO
import qualified Database.SQLite.Simple as SQL
import System.Directory ( canonicalizePath, createDirectory, doesDirectoryExist, getCurrentDirectory )
import System.FilePath ( (</>), makeRelative, takeDirectory )

import Paths_h_fcp

dbDir :: FilePath -> FilePath
dbDir base = base </> ".hsite"

data SiteDb = MDB
               { mdbConn     :: SQL.Connection
               , mdbBasePath :: FilePath
               }

initDb :: FilePath -> IO ()
initDb p = do
  putStrLn $ "initializing DB in " ++ (dbDir p)
  
  ex <- doesDirectoryExist (dbDir p)
  if ex
    then error $ "directory does already exist"
    else do
      createDirectory $ dbDir p
      initQuery <- getDataFileName "data/create-tables.sql" >>= TIO.readFile
      SQL.withConnection (dbDir p </> "index.db") $ \c -> do
        SQL.execute_ c $ SQL.Query initQuery
        return ()

openDb :: FilePath -> IO SiteDb
openDb dir = do
  c <- SQL.open (dir </> "index.db")
  return $ MDB c $ takeDirectory dir

closeDb :: SiteDb -> IO ()
closeDb db = SQL.close $ mdbConn db

-- | finds the DB folder relative to the current directory by walking
--   upwards the tree until a ".hsite" directory is found
findDbFolder :: IO (Maybe FilePath)
findDbFolder = getCurrentDirectory >>= go where
  go d = do
    here <- doesDirectoryExist $ dbDir d
    if here
      then return $ (Just $ dbDir d)
      else let d' = takeDirectory d in if (d' == d)
                                       then return Nothing
                                       else go d'

withDb :: (SiteDb -> IO a) -> IO a
withDb act = findDbFolder >>= \mdbf -> case mdbf of
  Nothing  -> error "no database found, maybe try \"init\"?"
  Just dbf -> do
    db <- openDb dbf
    finally (act db) (closeDb db)

-----------------------------------------------------------------
-- working with the DB
-----------------------------------------------------------------

relPath :: SiteDb -> FilePath -> IO FilePath
relPath db p = makeRelative (mdbBasePath db) <$> canonicalizePath p

type FileInfo = (FilePath, Integer, BS.ByteString)

needsInsert :: SiteDb -> FileInfo -> IO Bool
needsInsert db (absPath, size, hash) = do
  let
    c = mdbConn db
    q = "SELECT 1 FROM files WHERE file_name = ? AND file_size = ? AND file_sha1 = ? AND file_last_insert NOT NULL"
    
  p <- relPath db absPath
  xs <- SQL.query c q (p, size, hash) :: IO [SQL.Only Int]
  return $ null xs
  
addFile :: SiteDb -> FileInfo -> IO ()
addFile db (absPath, size, hash) = do
  let
    c = mdbConn db
    q = "REPLACE INTO files (file_name, file_size, file_sha1) VALUES (?, ?, ?)"
    
  p <- relPath db absPath
  SQL.execute c q (p, size, hash)

updateFileUri :: SiteDb -> FilePath -> String -> IO ()
updateFileUri db absPath uri = do
  let
    c = mdbConn db
    q = "UPDATE files SET file_last_insert = CASE WHEN file_uri != ? THEN NULL ELSE file_uri END, file_uri = ? WHERE file_name = ?"

  p <- relPath db absPath    
  SQL.execute c q (uri, uri, p)

insertDone :: SiteDb -> FilePath -> IO ()
insertDone db absPath = do
  let
    c = mdbConn db
    q = "UPDATE files SET file_last_insert = datetime('now') WHERE file_name = ?"

  p <- relPath db absPath
  SQL.execute c q $ SQL.Only p
  

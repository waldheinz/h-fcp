
module CmdLine (
  Mode(..), parseMode
  ) where

data Mode
     = Init (Maybe FilePath)
     | InsertFiles [FilePath]
     deriving ( Show )

parseMode :: [String] -> Either String Mode
parseMode ("init" : []) = Right $ Init Nothing
parseMode ("init" : d : _) = Right $ Init $ Just d
parseMode ("insertFiles" : files) = Right $ InsertFiles files
parseMode (x:_) = Left $ "unknown mode " ++ x
parseMode [] = Left "no mode given"

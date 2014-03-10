
module CmdLine (
  Mode(..), parseMode
  ) where

data Mode
     = GenKeys String
     | Init (Maybe FilePath)
     | Insert Bool
     | InsertFiles [FilePath]
     | Status
     deriving ( Show )

parseMode :: [String] -> Either String Mode
parseMode ("keygen" : name : _) = Right $ GenKeys name
parseMode ("init" : []) = Right $ Init Nothing
parseMode ("init" : d : _) = Right $ Init $ Just d
parseMode ("insert" : "--chk": _) = Right $ Insert True
parseMode ("insert" : _) = Right $ Insert False
parseMode ("insertFiles" : files) = Right $ InsertFiles files
parseMode ("status" : _) = Right Status
parseMode (x:_) = Left $ "unknown mode " ++ x
parseMode [] = Left "no mode given"

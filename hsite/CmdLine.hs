
module CmdLine (
  Mode(..), parseMode
  ) where

data Mode
     = Init (Maybe FilePath)
     | InsertChk
     | InsertFiles [FilePath]
     | Status
     deriving ( Show )

parseMode :: [String] -> Either String Mode
parseMode ("init" : []) = Right $ Init Nothing
parseMode ("init" : d : _) = Right $ Init $ Just d
parseMode ("insertChk" : _) = Right $ InsertChk
parseMode ("insertFiles" : files) = Right $ InsertFiles files
parseMode ("status" : _) = Right Status
parseMode (x:_) = Left $ "unknown mode " ++ x
parseMode [] = Left "no mode given"


module CmdLine (
  Mode(..), parseMode
  ) where

data Mode
     = Init (Maybe FilePath)
     | InsertChk [FilePath]
     deriving ( Show )

parseMode :: [String] -> Either String Mode
parseMode ("init" : []) = Right $ Init Nothing
parseMode ("init" : d : _) = Right $ Init $ Just d
parseMode ("insertChk" : files) = Right $ InsertChk files
parseMode (x:_) = Left $ "unknown mode " ++ x
parseMode [] = Left "no mode given"

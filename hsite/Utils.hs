
module Utils (
  prettySize
  ) where

import Text.Printf ( printf )

prettySize :: Integral a => a -> String
prettySize sz
  | sz < 1024               = s 1 ++ " B"
  | sz < 1024 * 1024        = s 1024 ++ " KiB"
  | sz < 1024 * 1024 * 1024 = (s (1024 * 1024)) ++ " MiB"
  | otherwise = (s (1024 * 1024 * 1024)) ++ " GiB"
  where
    s :: Int -> String
    s d = printf "%.2f" (fromIntegral sz / (fromIntegral d :: Float))
    

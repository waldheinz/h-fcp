
module Progress (
  drawProgress, clearProgress
  ) where

import Control.Monad ( when )
import System.Console.ANSI
import System.IO ( hFlush, hIsTerminalDevice, stdout )

screenWidth :: Int
screenWidth = 74

ifTerminal :: IO () -> IO ()
ifTerminal act = hIsTerminalDevice stdout >>= \t -> when t act

clearProgress :: IO ()
clearProgress = ifTerminal $ clearFromCursorToLineBeginning >> setCursorColumn 0

drawProgress :: Int -> Int -> IO ()
drawProgress tot cur = ifTerminal $ do
  setCursorColumn 0
  putStr $ (show tot) ++ "/" ++ (show cur)
  hFlush stdout
  return ()

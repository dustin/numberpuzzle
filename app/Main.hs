module Main where

import NumberPuzzle;
import Control.Monad(forM_);
import System.Environment (getArgs)

main :: IO ()
main = do
  (want:vals) <- map read <$> getArgs
  forM_ (solve vals want) print

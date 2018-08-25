module Main where

import NumberPuzzle;
import Control.Monad(forM_);

main :: IO ()
main = forM_ (solve [1,3,4,6] 24) print

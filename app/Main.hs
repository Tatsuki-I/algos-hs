module Main where

import           Sort.ParallelQuickSort
import           Sort.QuickSort

main :: IO ()
main = print (parallelQuickSort [1 .. 10000])
--main = print (quickSort [1 .. 10000])

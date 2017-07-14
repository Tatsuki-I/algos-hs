module Main where

import           Sort.InsertionSort

main :: IO ()
main = print (insertionSort [6, 5, 4, 3, 2, 1])

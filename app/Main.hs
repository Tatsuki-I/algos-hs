module Main where

import Sort.QuickSort
import Sort.InsertSort

main :: IO ()
main = print $ insertSort [6,5,4,3,2,1]

module Main where

import Sort.QuickSort
import Sort.InsertSort
import Sort.SelectionSort

main :: IO ()
main = print $ selectionSort [6,5,4,3,2,1]

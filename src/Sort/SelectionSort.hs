module Sort.SelectionSort where

import           Data.List

selectionSort :: Ord a => [a] -> [a]
selectionSort []   = []
selectionSort list = minX : selectionSort (delete minX list)
    where
        minX = minimum list

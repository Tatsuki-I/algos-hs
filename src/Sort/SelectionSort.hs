module Sort.SelectionSort where

import           Data.List

selectionSort :: Ord a => [a] -> [a]
selectionSort []   = []
selectionSort list = minX : selectionSort (delete minX list)
    where
        minX = minimum list

separateMinimum :: Ord a => [a] -> Maybe (a, [a])
separateMinimum []   = Nothing
separateMinimum list = Just (x, xs)
    where
        x  = minimum list
        xs = delete x list

selectionSort' :: Ord a => [a] -> [a]
selectionSort' = unfoldr separateMinimum

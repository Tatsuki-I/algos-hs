module Sort.MergeSort where

import           Data.List

merge :: Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x : xs) (y : ys)
        | x <= y    = x : merge xs (y : ys)
        | otherwise = y : merge (x : xs) ys

halve :: [a] -> ([a], [a])
halve list = (take lsLength list, drop lsLength list)
    where
        lsLength = div (length list) 2

mergeSort :: Ord a => [a] -> [a]
mergeSort []   = []
mergeSort [x]  = [x]
mergeSort list = merge (mergeSort $ fst hlist) (mergeSort $ snd hlist)
    where
        hlist = halve list

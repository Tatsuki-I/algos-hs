module Sort.ParallelQuickSort where

import           Data.List
import Control.Parallel

parallelQuickSort :: Ord a => [a] -> [a]
parallelQuickSort []       = []
parallelQuickSort (x : xs) = (forceList lesser) `par` 
        (forceList greater) `par`
        lesser ++ x : greater
            where
                lesser  = parallelQuickSort [y | y <- xs, y < x]
                greater = parallelQuickSort [y | y <- xs, y >= x]

forceList :: [a] -> ()
forceList [] = ()
forceList (x : xs) = x `pseq` forceList xs

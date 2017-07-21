module Sort.SelectionSort where

import           Data.List
import Control.Monad
import Control.Monad.Fix

selectionSort :: Ord a => [a] -> [a]
--selectionSort []   = []
selectionSort = fix (((:) <$>  minimum <*>) . (. (delete =<< minimum)))
--selectionSort = fix (liftM2 (:) minimum . (. (delete =<< minimum)))
{-
selectionSort list = minX : selectionSort (delete minX list)
    where
        minX = minimum list
        -}

separateMinimum :: Ord a => [a] -> Maybe (a, [a])
separateMinimum []   = Nothing
separateMinimum list = Just (x, xs)
    where
        x  = minimum list
        xs = delete x list

selectionSort' :: Ord a => [a] -> [a]
selectionSort' = unfoldr separateMinimum

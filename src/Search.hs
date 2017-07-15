module Search where

sequentialSearch :: Eq a => [a] -> a -> Bool
sequentialSearch [] _ = False
sequentialSearch (x : xs) target
        | x == target = True
        | otherwise   = sequentialSearch xs target

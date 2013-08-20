module Texture.Utils where

import Data.List (sortBy)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

maybeHead [] = Nothing
maybeHead (x:_) = Just x

sortByFst :: Ord a => [(a, b)] -> [(a, b)]
sortByFst = sortBy (\a b -> compare (fst a) (fst b))


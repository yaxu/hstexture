module Texture.Utils where

import Data.List (sortBy)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

maybeHead [] = Nothing
maybeHead (x:_) = Just x

sortByFst :: Ord a => [(a, b)] -> [(a, b)]
sortByFst = sortBy (\a b -> compare (fst a) (fst b))

parenthesise :: String -> String
parenthesise x = "(" ++ x ++ ")"
                                          
spaces :: Int -> String
spaces n = take n $ repeat ' '

single :: [a] -> Maybe a
single (x:[]) = Just x
single _ = Nothing

fromJust' (Just x) = x
fromJust' Nothing = error "nothing is just"

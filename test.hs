
import Types

tests = and [fits [Int, Float,WildCard] (Param 0) [] Int,
             not $ fits [Int, Float,WildCard] (Param 1) [] Int,
             fits [Int, Float,WildCard] (Param 2) [] Int,
             not $ fits [] (Pattern Int) [] (Pattern String),
             fits [] (Pattern Int) [] (Pattern Int),
             fits [] (OneOf [Int, String]) [] (OneOf [Int, Float]),
             fits [] (OneOf [Int, String]) [] String
            ]

things :: [Thing]
things = [Thing 0 "hello" [] String String (0,1) Nothing Nothing,
          Thing 1 "38" [] Int Int (23,4) Nothing Nothing,
          Thing 2 "32" [] Int Int (4,5) Nothing Nothing,
          Thing 3 "38" [] Int Int (5,44) Nothing Nothing,
          Thing 4 "58" [] Int Int (2,2) Nothing Nothing,
          Thing 5 "38" [] Int Int (2,3) Nothing Nothing,
          Thing 6 "+" [number] (F (Param 0) 
                                (F (Param 0) (Param 0))
                               ) 
                               (F (Param 0) 
                                (F (Param 0) (Param 0))
                               ) (2,4) Nothing Nothing
         -- Thing 7 "*" [] (F Int (F Int Int)) (F Int (F Int Int)) (54,4) Nothing Nothing
         ]


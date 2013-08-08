
import Texture.Types

tests = and [fits (Sig [Int, Float,WildCard] (Param 0)) (Sig [] Int),
             not $ fits (Sig [Int, Float,WildCard] (Param 1)) (Sig [] Int),
             fits (Sig [Int, Float,WildCard] (Param 2)) (Sig [] Int),
             not $ fits (Sig [] (Pattern Int)) (Sig [] (Pattern String)),
             fits (Sig [] (Pattern Int)) (Sig [] (Pattern Int)),
             fits (Sig [] (OneOf [Int, String])) (Sig [] (OneOf [Int, Float])),
             fits (Sig [] (OneOf [Int, String])) (Sig [] String)
            ]

things :: [Datum]
things = [--Datum 0 "hello" (Sig [] String) (Sig [] String) (0,1) Nothing Nothing,
          Datum 1 "38" (Sig [] Int) (Sig [] Int) (23,4) Nothing Nothing,
          Datum 2 "30" (Sig [] Int) (Sig [] Int) (23,6) Nothing Nothing,
          Datum 6 "+" (Sig [number] (F (Param 0) 
                                     (F (Param 0) (Param 0))
                                    ))
                      (Sig [number] (F (Param 0) 
                                     (F (Param 0) (Param 0))
                                    ))
                      (2,4)
                      Nothing Nothing
         -- Datum 7 "*" [] (F Int (F Int Int)) (F Int (F Int Int)) (54,4) Nothing Nothing
         ]


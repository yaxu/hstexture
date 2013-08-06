module Types where

import Data.List
import Data.Maybe
import Control.Applicative

data Type = 
  F Type Type
  | String
  | Float
  | Int
  | OneOf [Type]
  | Pattern Type
  | WildCard
  | Param Int

instance Eq Type where
  F a a' == F b b' = and [a == b,
                          a' == b'
                         ]
  String == String = True
  Float == Float = True
  Int == Int = True
  OneOf as == OneOf bs = as == bs
  Pattern a == Pattern b = a == b
  WildCard == WildCard = True
  Param a == Param b = a == b
  _ == _ = False
  
instance Show Type where
  show (F a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show String = "s"
  show Float = "f"
  show Int = "i"
  show WildCard = "*"
  show (Pattern t) = "p [" ++ (show t) ++ "]"
  show (OneOf t) = "? [" ++ (show t) ++ "]"
  show (Param n) = "param#" ++ (show n)

number = OneOf [Float, Int]

isFunction :: Type -> Bool
isFunction (F _ _) = True
isFunction _ = False

type Id = Int
type Location = (Float, Float)
type Proximity = Float

data Thing = Thing {ident  :: Int,
                    name   :: String,
                    params :: [Type],
                    is     :: Type,
                    applied_as :: Type,
                    loc    :: Location,
                    prev   :: Maybe Thing,
                    next   :: Maybe Thing
                   }

instance Show Thing where
  show t = intercalate "\n" $ map (\(s, f) -> s ++ ": " ++ (f t))
                                  [("ident", show . ident),
                                   ("name", show . name),
                                   ("params", show .params),
                                   ("is", show . is),
                                   ("applied_as", show . applied_as),
                                   ("loc", show . loc)
                                   --("prev", show . prev),
                                   --("next", show . next)
                                  ]

instance Eq Thing where
  a == b = (ident a) == (ident b)

update :: Thing -> [Thing] -> [Thing]
update thing things = map f things
  where n = ident thing
        f x | ident x == n = thing
            | otherwise = x

stringToType :: String -> Type
stringToType [] = String
stringToType s = scanType Int s
  where scanType t [] = t
        scanType Int ('.':[]) = String
        scanType Int (c:s) | elem c ['0' .. '9'] = scanType Int s
                           | c == '.' = scanType Float s
                           | otherwise = String
        scanType Float (c:s) | elem c ['0' .. '9'] = scanType Float s
                             | otherwise = String

fits :: [Type] -> Type -> [Type] -> Type -> Bool
fits _ WildCard _ _ = True
fits _ _ _ WildCard = True

fits pA (F a a') pB (F b b') = (fits pA a pB b) && (fits pA a' pB b')

fits pA (OneOf as) pB (OneOf bs) = 
  intersectBy (\a b -> fits pA a pB b) as bs /= []

fits pA (OneOf as) pB b = or $ map (\x -> fits pA x pB b) as
fits pA a pB (OneOf bs) = or $ map (\x -> fits pA a pB x) bs

fits pA (Pattern a) pB (Pattern b) = fits pA a pB b

fits pA a pB (Param b) = fits pA a pB (pB !! b)
fits pA (Param a) pB b = fits pA (pA !! a) pB b

fits _ Float _ Float = True
fits _ Int _ Int  = True
fits _ String _ String = True

fits _ _ _ _ = False

resolve :: (([Type], Type), ([Type], Type)) -> (([Type], Type), ([Type], Type))

resolve ((pA, a), (pB, Param nB)) = 
  (a', (setIndex pB' nB b', Param nB))
  where (a', (pB', b')) = resolve ((pA, a), (pB, pB !! nB))

resolve ((pA, Param nA), (pB, b)) = 
  ((setIndex pA' nA a', Param nA), b')
  where ((pA', a'), b') = resolve ((pA, pA !! nA), (pB, b))

resolve x@((pA, WildCard), (pB, WildCard)) = x
resolve ((pA, WildCard), (pB, b)) = ((pA, b), (pB, b))
resolve ((pA, a), (pB, WildCard)) = ((pA, a), (pB, a))

resolve ((pA, OneOf as), (pB, OneOf bs)) = 
  resolve ((pA, fst match), (pB, snd match))
  where match = head $ matchPairs (\a b -> fits pA a pB b) as bs

-- Bit of a cheat?
resolve ((pA, a), (pB, OneOf bs)) = 
  resolve ((pA, OneOf [a]), (pB, OneOf bs))
resolve ((pA, OneOf as), (pB, b)) = 
  resolve ((pA, OneOf as), (pB, OneOf [b]))

resolve a = a

matchPairs :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
matchPairs _  [] _  =  []
matchPairs _  _  [] =  []
matchPairs eq xs ys =  [(x,y) | x <- xs, y <- ys, eq x y]

--fits pA (OneOf as) pB b = or $ map (\x -> fits pA x pB b) as
--fits pA a pB (OneOf bs) = or $ map (\x -> fits pA a pB x) bs

-- replace element i in xs with x
setIndex :: [a] -> Int -> a -> [a]
setIndex xs i x = (take (i-1) xs) ++ (x:(drop i xs))

resolve' :: (Type, Type) -> (Type, Type)

resolve' (WildCard, WildCard) = (WildCard, WildCard)
resolve' (a, WildCard) = (a, a)
resolve' (WildCard, b) = (b, b)

resolve' (OneOf as, OneOf bs) = 
  (OneOf $ intersect as bs, OneOf $ intersect as bs)
resolve' (a, OneOf bs) = (a, a)
resolve' (OneOf as, b) = (b, b)


lookupParam :: [Type] -> Type -> Type
lookupParam pX (Param n) = pX !! n
lookupParam pX (OneOf xs) = OneOf $ map (lookupParam pX) xs
lookupParam _ x = x

sqr :: Num a => a -> a
sqr x = x * x

dist :: Thing -> Thing -> Float
dist a b = sqrt ((sqr $ (fst $ loc a) - (fst $ loc b))
                 + (sqr $ (snd $ loc a) - (snd $ loc b))
                )
build :: [Thing] -> [Thing]
build things = set $ link things
  where set (Just (a,b)) = build $ update a $ update b $ things
        set Nothing = things

link :: [Thing] -> Maybe (Thing, Thing)
link things = fmap (updateLink . snd) $ maybeHead $ sortByFst $ 
              concatMap dists $ filter (isFunction . applied_as) things
  where dists :: Thing -> [(Float, (Thing, Thing))]
        dists x
          = map (\fitter -> (dist x fitter, (x, fitter))) fitters
          where 
            fitters = filter 
                      (\thing -> 
                        (thing /= x 
                         && 
                         fits (params x) a (params thing) (applied_as thing)
                        )
                      )
                      things
            (F a b) = applied_as x

-- Apply b to a, and resolve the wildcards and "oneof"s

updateLink :: (Thing, Thing) -> (Thing, Thing)
updateLink (a, b) = (a {next = Just b, 
                        params = pA,
                        is = tA,
                        applied_as = result (applied_as a)
                       }, 
                     b {prev = Just a,
                        params = pB,
                        is = tB
                       }
                    )
  where ((pA, tA), (pB, tB)) = resolve ((params a, is a), (params b, is b))



result :: Type -> Type
result (F _ x) = x
result _ = error "No result of non-function"

maybeHead [] = Nothing
maybeHead (x:_) = Just x

sortByFst :: Ord a => [(a, b)] -> [(a, b)]
sortByFst = sortBy (\a b -> compare (fst a) (fst b))


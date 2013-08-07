module Texture.Types where

import Data.List
import Data.Maybe
import Control.Applicative
import Data.Tuple (swap)

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

data Sig = Sig {params :: [Type],
                is :: Type
               }
instance Show Sig where
  show s = ps ++ (show $ is s)
    where ps | params s == [] = ""
             | otherwise = show ps ++ " => "


functions = [('+', Sig [number] $ F (Param 0) (F (Param 0) (Param 0))),
             ('-', Sig [number] $ F (Param 0) (F (Param 0) (Param 0))),
             ('/', Sig [number] $ F (Param 0) (F (Param 0) (Param 0))),
             ('*', Sig [number] $ F (Param 0) (F (Param 0) (Param 0)))
            ]

data Datum = Datum {ident      :: Int,
                    name       :: String,
                    sig        :: Sig,
                    applied_as :: Sig,
                    location   :: (Float, Float),
                    prev       :: Maybe Datum,
                    next       :: Maybe Datum
                   }

{-
data Datum = Datum {ident  :: Int,
                    name   :: String,
                    params :: [Type],
                    is     :: Type,
                    applied_as :: Type,
                    location :: (Float, Float),
                    prev   :: Maybe Datum,
                    next   :: Maybe Datum
                   }
-}
instance Show Type where
  show (F a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show String = "s"
  show Float = "f"
  show Int = "i"
  show WildCard = "*"
  show (Pattern t) = "p [" ++ (show t) ++ "]"
  show (OneOf ts) = "?" ++ (show ts)
  show (Param n) = "param#" ++ (show n)

number = OneOf [Float, Int]

isFunction :: Type -> Bool
isFunction (F _ _) = True
isFunction _ = False

type Id = Int
type Proximity = Float


instance Show Datum where
  show t = intercalate "\n" $ map (\(s, f) -> s ++ ": " ++ (f t))
                                  [("ident", show . ident),
                                   ("name", show . name),
                                   ("signature", show . sig),
                                   ("applied_as", show . applied_as),
                                   ("location", show . location)
                                   --("prev", show . prev),
                                   --("next", show . next)
                                  ]

instance Eq Datum where
  a == b = (ident a) == (ident b)

update :: Datum -> [Datum] -> [Datum]
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

fits :: Sig -> Sig -> Bool
fits (Sig _ WildCard) _ = True
fits _ (Sig _ WildCard) = True

fits (Sig pA (F a a')) (Sig pB (F b b')) = 
  (fits (Sig pA a) (Sig pB b)) && (fits (Sig pA a') (Sig pB b'))

fits (Sig pA (OneOf as)) (Sig pB (OneOf bs)) = 
  intersectBy (\a b -> fits (Sig pA a) (Sig pB b)) as bs /= []

fits (Sig pA (OneOf as)) (Sig pB b) = 
  or $ map (\x -> fits (Sig pA x) (Sig pB b)) as
fits (Sig pA a) (Sig pB (OneOf bs)) = 
  or $ map (\x -> fits (Sig pA a) (Sig pB x)) bs

fits (Sig pA (Pattern a)) (Sig pB (Pattern b)) = fits (Sig pA a) (Sig pB b)

fits (Sig pA a) (Sig pB (Param b)) = fits (Sig pA a) (Sig pB (pB !! b))
fits (Sig pA (Param a)) (Sig pB b) = fits (Sig pA (pA !! a)) (Sig pB b)

fits (Sig _ Float) (Sig _ Float)   = True
fits (Sig _ Int) (Sig _ Int)       = True
fits (Sig _ String) (Sig _ String) = True

fits _ _ = False

simplify :: Type -> Type
simplify x@(OneOf []) = x -- shouldn't happen..
simplify (OneOf (x:[])) = x
simplify (OneOf xs) = OneOf $ nub xs
simplify x = x

resolve :: Sig -> Sig -> Sig

resolve (Sig pA (F iA oA)) (Sig pB (F iB oB)) = Sig pA'' (F i o)
  where Sig pA' i  = resolve (Sig pA iA) (Sig pB iB)
        Sig pA'' o = resolve (Sig pA' oA) (Sig pB oB)

resolve (Sig pA (Param nA)) sb = Sig (setIndex pA' nA a') (Param nA)
  where (Sig pA' a') = resolve (Sig pA (pA !! nA)) sb

resolve sa (Sig pB (Param nB)) = resolve sa (Sig pB (pB !! nB))

-- TODO - support Params inside OneOfs
resolve (Sig pA (OneOf as)) (Sig pB b) = Sig pA (simplify t)
  where t = OneOf $ map (\a -> is $ resolve (Sig pA a) (Sig pB b)) matches
        matches = filter (\a -> fits (Sig pA a) (Sig pB b)) as 

resolve (Sig pA a) (Sig pB (OneOf bs)) = Sig pA (simplify t)
  where t = OneOf $ map (\b -> is $ resolve (Sig pA a) (Sig pB b)) matches
        matches = filter (\b -> fits (Sig pA a) (Sig pB b)) bs 

resolve a (Sig _ WildCard) = a
resolve (Sig _ WildCard) b = b

--  resolve ((pA, fst match), (pB, snd match))
--  where match = head $ matchPairs (\a b -> fits pA a pB b) as bs

{-
-- Bit of a cheat?
resolve ((pA, a), (pB, OneOf bs)) = 
  resolve ((pA, OneOf [a]), (pB, OneOf bs))
resolve ((pA, OneOf as), (pB, b)) = 
  resolve ((pA, OneOf as), (pB, OneOf [b]))
-}

resolve a b = a

matchPairs :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
matchPairs _  [] _  =  []
matchPairs _  _  [] =  []
matchPairs eq xs ys =  [(x,y) | x <- xs, y <- ys, eq x y]

--fits pA (OneOf as) pB b = or $ map (\x -> fits pA x pB b) as
--fits pA a pB (OneOf bs) = or $ map (\x -> fits pA a pB x) bs

-- replace element i in xs with x
setIndex :: [a] -> Int -> a -> [a]
setIndex xs i x = (take (i) xs) ++ (x:(drop (i+1) xs))

{-
resolve' :: (Type, Type) -> (Type, Type)

resolve' (WildCard, WildCard) = (WildCard, WildCard)
resolve' (a, WildCard) = (a, a)
resolve' (WildCard, b) = (b, b)

resolve' (OneOf as, OneOf bs) = 
  (OneOf $ intersect as bs, OneOf $ intersect as bs)
resolve' (a, OneOf bs) = (a, a)
resolve' (OneOf as, b) = (b, b)
-}

lookupParam :: [Type] -> Type -> Type
lookupParam pX (Param n) = pX !! n
lookupParam pX (OneOf xs) = OneOf $ map (lookupParam pX) xs
lookupParam _ x = x

sqr :: Num a => a -> a
sqr x = x * x

dist :: Datum -> Datum -> Float
dist a b = sqrt ((sqr $ (fst $ location a) - (fst $ location b))
                 + (sqr $ (snd $ location a) - (snd $ location b))
                )
build :: [Datum] -> [Datum]
build things = set $ link things
  where set (Just (a,b)) = build $ update a $ update b $ things
        set Nothing = things

link :: [Datum] -> Maybe (Datum, Datum)
link things = fmap (updateLink . snd) $ maybeHead $ sortByFst $ 
              concatMap dists $ filter (isFunction . is . applied_as) things
  where dists :: Datum -> [(Float, (Datum, Datum))]
        dists x
          = map (\fitter -> (dist x fitter, (x, fitter))) fitters
          where 
            fitters = filter 
                      (\thing -> 
                        (thing /= x 
                         && 
                         fits (applied_as x) (applied_as thing)
                        )
                      )
                      things
            (F a b) = is $ applied_as x

-- Apply b to a, and resolve the wildcards and "oneof"s

updateLink :: (Datum, Datum) -> (Datum, Datum)
updateLink (a, b) = (a {next = Just b, 
                        sig = s,
                        applied_as = output (applied_as a)
                       }, 
                     b {prev = Just a}
                    )
  where s = resolve (input $ applied_as a) (applied_as $ b)

output :: Sig -> Sig
output (Sig ps (F _ x)) = Sig ps x
output _ = error "No output from non-function"

input :: Sig -> Sig
input (Sig ps (F x _)) = Sig ps x
input _ = error "No input to non-function"

maybeHead [] = Nothing
maybeHead (x:_) = Just x

sortByFst :: Ord a => [(a, b)] -> [(a, b)]
sortByFst = sortBy (\a b -> compare (fst a) (fst b))


module Texture.Types where

import Data.Maybe
import Control.Applicative
import Data.Tuple (swap)
import Debug.Trace (trace)
import Data.List (intercalate, intersectBy, nub)

import Texture.Utils

--type Id = Int
--type Proximity = Float

data Type = 
  F Type Type
  | String
  | Float
  | Int
  | Bool
  | Colour
  | Osc
  | Action
  | OscStream
  | OneOf [Type]
  | Pattern Type
  | WildCard
  | Param Int
  | ListCon Type
  | SimpleList Type

instance Eq Type where
  F a a' == F b b' = and [a == b,
                          a' == b'
                         ]
  String == String = True
  Float == Float = True
  Bool == Bool = True
  Colour == Colour = True
  Int == Int = True
  Osc == Osc = True
  Action == Action = True
  OscStream == OscStream = True
  OneOf as == OneOf bs = as == bs
  Pattern a == Pattern b = a == b
  WildCard == WildCard = True
  Param a == Param b = a == b
  ListCon a == ListCon b = a == b
  SimpleList a == SimpleList b = a == b
  _ == _ = False

-- Type signature
data Sig = Sig {params :: [Type],
                is :: Type
               }
           deriving Eq

instance Show Sig where
  show s = ps ++ (show $ is s)
    where ps | params s == [] = ""
             | otherwise = show (params s) ++ " => "


showFunctions :: String
showFunctions = concatMap f functions
  where f (s, t) = s ++ " :: " ++ show t ++ "\n"

functions :: [(String, Sig)]
functions = 
  [("+", numOp),
   ("-", numOp),
   ("/", floatOp),
   ("*", numOp),
   -- ("fast", Sig [WildCard] $ F (Float) (F (Pattern $ Param 0) (Pattern $ Param 0))),
   -- ("slow", Sig [WildCard] $ F (Float) (F (Pattern $ Param 0) (Pattern $ Param 0))),
   ("every", Sig [threadTypes] $ F (Int) 
             (F (F (SimpleList $ Param 0) (SimpleList $ Param 0)) 
                (F (SimpleList $ Param 0) (SimpleList $ Param 0))
             )
   ),
   ("cycle", Sig [threadTypes] $ F (SimpleList $ Param 0) (SimpleList $ Param 0)),
   ("offset", Sig [threadTypes] $ F Int $ F (SimpleList $ Param 0) (SimpleList $ Param 0)),
   ("rev", Sig [threadTypes] $ F (SimpleList $ Param 0) (SimpleList $ Param 0)),
   ("double", Sig [threadTypes] $ F (SimpleList $ Param 0) (SimpleList $ Param 0)),
   ("backforth", Sig [threadTypes] $ F (SimpleList $ Param 0) (SimpleList $ Param 0)),
   ("shift", Sig [threadTypes] $ F (SimpleList $ Param 0) (SimpleList $ Param 0)),
   ("invert", Sig [threadTypes] $ F (SimpleList $ Param 0) (SimpleList $ Param 0)),
   (":", Sig [threadTypes] $ F (Param 0) (F (SimpleList (Param 0)) (SimpleList (Param 0)))),
   ("[]", Sig [threadTypes] $ SimpleList (Param 0)),
   ("up", Sig [] Bool),
   ("down", Sig [] Bool),
   -- ("red", Sig [] Colour),
   -- ("orange", Sig [] Colour),
   -- ("blue", Sig [] Colour),
   ("2", Sig [] Int),
   ("3", Sig [] Int),
   ("4", Sig [] Int),
   ("5", Sig [] Int),
   ("zipAnd", Sig [] $ F (SimpleList Bool) $ F (SimpleList Bool) $ SimpleList Bool),
   ("zipOr", Sig [] $ F (SimpleList Bool) $ F (SimpleList Bool) $ SimpleList Bool),
   ("zipXor", Sig [] $ F (SimpleList Bool) $ F (SimpleList Bool) $ SimpleList Bool)
   -- ("weave", Sig [] $ F (SimpleList Colour) $ F (SimpleList Colour) $ F (SimpleList Bool) Action)
   ]
  where threadTypes = OneOf [Bool,Colour]
        numOp = Sig [number] $ F (Param 0) $ F (Param 0) (Param 0)
        floatOp = Sig [] $ F Float (F Float Float)
        floatPat = Sig [] $ Pattern Float
        mapper = Sig [WildCard, WildCard] $ F (F (Param 0) (Param 1)) $ F (Pattern (Param 0)) (Pattern (Param 1))
        stringToOsc = Sig [] $ F (Pattern String) (Pattern Osc)
        floatToOsc = Sig [] $ F (Pattern Float) (Pattern Osc)

{-
prepender a = Sig [] $ F (List a) (List a)
prependString = prepender String
-}
{-
[ :: List a -> Pattern a
] :: a -> List a
bd :: List a -> List a
-}
data Datum = Datum {ident      :: Int,
                    token      :: String,
                    sig        :: Sig,
                    applied_as :: Sig,
                    location   :: (Float, Float),
                    applied_location :: (Float, Float),
                    parentId   :: Maybe Int,
                    --childId      :: Maybe Int,
                    childIds :: [Int]
                   }

instance Show Type where
  show (F a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show String = "s"
  show Float = "f"
  show Bool = "#"
  show Action = "io"
  show Colour = "c"
  show Int = "i"
  show (OneOf ts) = "?" ++ (show ts)
  show (Pattern t) = "p [" ++ (show t) ++ "]"
  show WildCard = "*"
  show (Param n) = "param#" ++ (show n)
  show (Osc) = "osc"
  show (OscStream) = "stream"
  show (ListCon t) = "list [" ++ (show t) ++ "]"
  show (SimpleList t) = "simplelist [" ++ (show t) ++ "]"

printDists :: [Datum] -> IO ()
printDists ds = mapM_ (\(a, b) -> putStrLn (token a ++ " -> " ++ token b ++ ": " ++ show (dist a b))) ps
  where ps = paired ds

paired :: [Datum] -> [(Datum, Datum)]
paired ds = map (\x -> (x, datumByIdent (fromJust $ parentId x) ds)) children
  where children = filter ((/= Nothing) . parentId) ds

walkTreesWhere :: (Datum -> Bool) -> [Datum] -> [String]
walkTreesWhere f ds = map (walkTree ds) $ tops
  where tops = filter f $ filter ((== Nothing) . parentId) ds

walkTree :: [Datum] -> Datum -> String

walkTree ds d@(Datum {token = "["}) = 
  "(parseBP_E \"" ++ contents ++ "\")"
  where contents = intercalate " " $ map token (tail $ offspring ds d)
walkTree ds d@(Datum {token = "]"}) = ""

walkTree ds d = value d ++ ps -- ++ " :: " ++ (show $ applied_as d) ++ " parent " ++ (show $ parentId d)
  where ps = concatMap (" " ++) $ map (parenthesise . recurse) (children ds d)
        recurse = walkTree ds
  
number = OneOf [Float, Int]

isFunction :: Type -> Bool
isFunction (F _ _) = True
isFunction _ = False

isListCon :: Type -> Bool
isListCon (ListCon _) = True
isListCon _ = False

isPattern :: Type -> Bool
isPattern (Pattern _) = True
isPattern _ = False

typeIsBits :: Type -> Bool
typeIsBits (SimpleList Bool) = True
typeIsBits _ = False

isPatTransform :: Type -> Bool
isPatTransform (F a b) = (isPattern a) && (isPattern b)
isPatTransform _ = False

-- Look for a function from pattern to pattern, and try to find and
-- return a compatible pattern applied to its parent

guessTransform :: [Datum] -> Datum -> Maybe (Datum, Type, String)
guessTransform ds a | isMetaP = do p <- (parent a ds) 
                                   sibling <- (single $ fitters p)
                                   return $ (a, (fromJust' $ patternType $ appliedConcreteType sibling), (walkTree ds a) ++ " $ " ++ (walkTree ds sibling))
                    | otherwise = Nothing
  where isMetaP = hasParent a && isPatTransform (appliedConcreteType a)
        fitters p = filter (\b -> fits (applied_as b) aInput) (children ds p)
        aInput = input $ applied_as a

patternType :: Type -> Maybe Type
patternType (Pattern t) = Just t
patternType _ = Nothing

appliedType :: Datum -> Type
appliedType = is . applied_as

appliedConcreteType :: Datum -> Type
appliedConcreteType = concreteType . applied_as

concreteType :: Sig -> Type
concreteType (Sig ps (Pattern x)) = Pattern (concreteType (Sig ps x))
concreteType (Sig ps (Param n)) = concreteType (Sig ps (ps !! n))
concreteType (Sig _ x) = x

wantsParam :: Datum -> Bool
wantsParam d = (parentId d) == Nothing && (isFunction t
                                           || isListCon t
                                          )
  where t = is $ applied_as $ d

parent :: Datum -> [Datum] -> Maybe Datum
parent d ds = do i <- parentId d
                 return $ datumByIdent i ds

children :: [Datum] -> Datum -> [Datum]
children ds d = map (\childId -> datumByIdent childId ds) (childIds d)

offspring :: [Datum] -> Datum -> [Datum]
offspring ds d = cs ++ concatMap (offspring ds) cs
  where cs = children ds d
                                  
hasParent :: Datum -> Bool
hasParent = isJust . parentId

hasChild :: Datum -> Bool
hasChild = not . null . childIds

instance Show Datum where
  show t = intercalate "\n" $ map (\(s, f) -> s ++ ": " ++ (f t))
           [("ident", show . ident),
                                   ("token", show . token),
                                   ("signature", show . sig),
                                   ("applied_as", show . applied_as),
                                   ("applied", show . childIds),
                                   ("location", show . location),
                                   ("parent", showSub . parentId),
                                   ("children", show . childIds)
                                  ]
    where showSub Nothing = "None"
          showSub (Just i) = "#" ++ (show $ i)

instance Eq Datum where
  a == b = (ident a) == (ident b)

isOscPattern :: Sig -> Bool
isOscPattern t = fits t (Sig [] $ Pattern Osc)

isAction :: Sig -> Bool
isAction t = fits t (Sig [] $ Action)

isBits :: Sig -> Bool
isBits t = fits t (Sig [] $ SimpleList Bool)

value :: Datum -> String
value x@(Datum {sig = (Sig {is = String})}) = "\"" ++ token x ++ "\""
-- Wrap in parenthesis so that infix operators work
value x@(Datum {sig = (Sig {is = F _ _})}) = "(" ++ token x ++ ")"
value x = token x

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

stringToSig :: String -> Sig
stringToSig s = fromMaybe def $ lookup s functions
  where def = Sig [] (stringToType s)

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

fits (Sig pA (ListCon a)) (Sig pB (ListCon b)) = fits (Sig pA a) (Sig pB b)

fits (Sig pA (SimpleList a)) (Sig pB (SimpleList b)) = fits (Sig pA a) (Sig pB b)

fits (Sig pA a) (Sig pB (Param b)) = fits (Sig pA a) (Sig pB (pB !! b))
fits (Sig pA (Param a)) (Sig pB b) = fits (Sig pA (pA !! a)) (Sig pB b)

fits (Sig _ Float) (Sig _ Float)   = True
fits (Sig _ Bool) (Sig _ Bool)     = True
fits (Sig _ Action) (Sig _ Action)     = True
fits (Sig _ Colour) (Sig _ Colour) = True
fits (Sig _ Int) (Sig _ Int)       = True
fits (Sig _ String) (Sig _ String) = True
fits (Sig _ OscStream) (Sig _ OscStream) = True
fits (Sig _ Osc) (Sig _ Osc) = True

fits _ _ = False

simplify :: Type -> Type
simplify x@(OneOf []) = x -- shouldn't happen..
simplify (OneOf (x:[])) = x
simplify (OneOf xs) = OneOf $ nub xs
simplify x = x


-- Resolve type signature a being applied to a function with type
-- signature b. At this point we know they're compatible, but type
-- parameters and "OneOf"s need resolving.

(!!!) :: [Type] -> Int -> Type
a !!! b | b < length a = a !! b
        | otherwise = error $ "oh dear, " ++ (show $ 1 + b) ++ "th of " ++ show a

resolve :: Sig -> Sig -> Sig

resolve (Sig pA (F iA oA)) (Sig pB (F iB oB)) = Sig pA'' (F i o)
  where Sig pA' i  = resolve (Sig pA iA) (Sig pB iB)
        Sig pA'' o = resolve (Sig pA' oA) (Sig pB oB)

resolve (Sig pA (Param nA)) sb = Sig (setIndex pA nA a') (Param nA)
  where (Sig pA' a') = resolve (Sig pA (pA !!! nA)) sb

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

resolve (Sig pA (Pattern a)) (Sig pB (Pattern b)) = Sig pA' (Pattern a')
  where (Sig pA' a') =  resolve (Sig pA a) (Sig pB b)

resolve (Sig pA (ListCon a)) (Sig pB (ListCon b)) = Sig pA' (ListCon a')
  where (Sig pA' a') =  resolve (Sig pA a) (Sig pB b)

resolve (Sig pA (SimpleList a)) (Sig pB (SimpleList b)) = Sig pA' (SimpleList a')
  where (Sig pA' a') =  resolve (Sig pA a) (Sig pB b)

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

lookupParam :: [Type] -> Type -> Type
lookupParam pX (Param n) | n < length pX = pX !! n
                         | otherwise = error "Can't happen."
lookupParam pX (OneOf xs) = OneOf $ map (lookupParam pX) xs
lookupParam _ x = x

sqr :: Num a => a -> a
sqr x = x * x

dist :: Datum -> Datum -> Float
dist f v = sqrt ((sqr $ (fst $ applied_location f) - (fst $ location v))
                 + (sqr $ (snd $ applied_location f) - (snd $ location v))
                )
           
-- Recursively build the parse tree
build :: [Datum] -> [Datum]
build things | linked == Nothing = things
             | otherwise = build $ fromJust linked
  where linked = link things

-- Find a link
link :: [Datum] -> Maybe [Datum]
link things = fmap ((updateLinks things) . snd) $ maybeHead $ sortByFst $ tmp 
  where unlinked = filter (not . hasParent) things
        tmp = concatMap (dists unlinked) fs
        fs = filter wantsParam things

dists :: [Datum] -> Datum -> [(Float, (Datum, Datum))]
dists values f = map (\fitter -> (dist f fitter, (f, fitter))) fitters
  where 
    fitters = filter 
              (\value -> 
                (value /= f 
                 && 
                 fits (input $ applied_as f) (applied_as value)
                )
              )
              values

-- Apply b to a, and resolve the wildcards and "oneof"s

updateLinks :: [Datum] -> (Datum, Datum) -> [Datum]
updateLinks ds (a, b) = appendChild ds' (a', b')
  where s = resolve (input $ applied_as a) (applied_as $ b)
        a' = a {applied_as = (output (applied_as a)) {params = params s},
                applied_location = applied_location b
               }
        b' = b
        ds' = update a' ds


appendChild :: [Datum] -> (Datum, Datum) -> [Datum]
appendChild ds (a, b) = update a' $ update b' $ ds
  where a' = a {childIds = (childIds a) ++ [ident b]}
        b' = b {parentId = Just $ ident a}

datumByIdent :: Int -> [Datum] -> Datum
datumByIdent i ds = head $ filter (\d -> ident d == i) ds

output :: Sig -> Sig
output (Sig ps (F _ x)) = Sig ps x
output x@(Sig _ (ListCon _)) = x
output _ = error "No output from non-function"

input :: Sig -> Sig
input (Sig ps (F x _)) = Sig ps x
input (Sig ps (ListCon x)) = Sig ps x
input _ = error "No input to non-function"



module Texture.Weave where

import Text.Printf
import Data.Colour.SRGB
import Data.Colour.Names

data Weave = Weave {wWarp :: [Colour Double],
                    wWeft :: [Colour Double],
                    wBits :: [Bool]
                   }

defaultWeave :: Weave
defaultWeave = Weave {wWarp = [black],
                      wWeft = [white],
                      wBits = cycle [up, down]
                     }

up :: Bool
up = True

down :: Bool
down = False

width = 16
height = 12

every :: Int -> ([a] -> [a]) -> [a] -> [a]
every _ _ [] = []
every 0 _ xs = xs
-- take width added after the application of `f` to make sure
-- transformations don't go outside the row
every n f xs = (take width $ f $ take width xs) ++ (take (width * (n-1)) $ drop width xs) ++ every n f (drop (width*n) xs)

twill :: Int -> [a] -> [a]
twill 0 xs = xs
twill _ [] = []
twill n xs = take width row ++ twill n (drop width row)
  where row = drop n xs

offset :: Int -> [a] -> [a]
offset = twill

-- take width added after the application of `f` to make sure
-- transformations don't go outside the row
each n f xs = (take width $ f $ take width xs) ++ (take (width * (n-1)) $ drop width xs) ++ every n f (drop (width*n) xs)

shift :: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

rev :: [a] -> [a]
rev [] = []
rev xs = (reverse $ take width xs) ++ (rev $ drop width xs)

invert :: [Bool] -> [Bool]
invert = map not

double :: [a] -> [a]
double [] = []
double (x:xs) = (x:x:double xs)

backforth :: [a] -> [a]
backforth = every 2 reverse

to2d :: [Bool] -> [[Bool]]
to2d [] = []
to2d bits = (take width bits) : (to2d $ drop width bits)


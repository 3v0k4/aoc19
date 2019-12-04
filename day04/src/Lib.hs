module Lib
    ( firstStar
    , secondStar
    , double
    , six
    , noDec
    ) where

-- 123456
-- within input
-- two adj =
-- never dec

import Data.List
import Data.List.Split

input = "382345-843167"

double :: Int -> Bool
double = any (flip (>=) 2) . fmap length . group . show

six :: Int -> Bool
six = (==) 6 . length . show

noDec :: Int -> Bool
noDec i = i == (read . sort . show $ i)

cond :: Int -> Bool
cond = and . sequence [double, six, noDec]

firstStar :: IO ()
firstStar = do
  let [a,b] = fmap read $ splitOn "-" input
  print . length . filter cond $ [a..b]

exactlyDouble :: Int -> Bool
exactlyDouble = elem 2 . fmap length . group . show

cond2 :: Int -> Bool
cond2 = and . sequence [exactlyDouble, six, noDec]

secondStar :: IO ()
secondStar = do
  let [a,b] = fmap read $ splitOn "-" input
  print . length . filter cond2 $ [a..b]

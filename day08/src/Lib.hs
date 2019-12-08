module Lib
    ( firstStar
    , secondStar
    , layers
    ) where

import Data.List
import Data.List.Split
import Data.Function

firstStar :: IO ()
firstStar = do
  x <- readFile "src/input"
  print . onesTimesTwos . fewest0 . layers 25 6 . filter ((/=) '\n') $ x

layers :: Int -> Int -> String -> [String]
layers w t = chunksOf (w*t)

onesTimesTwos :: String -> Int
onesTimesTwos s = count '1' s * count '2' s

fewest0 :: [String] -> String
fewest0 = minimumBy (compare `on` count '0')

count :: Char -> String -> Int
count c = length . filter (== c)

secondStar :: IO ()
secondStar = do
  x <- readFile "src/input"
  putStrLn . render . stack . layers 25 6 . filter ((/=) '\n') $ x

data Pixel = Zro | One | Trn
instance Semigroup Pixel where
  Trn <> y = y
  x   <> y = x

pixel :: Char -> Pixel
pixel '0' = Zro
pixel '1' = One
pixel '2' = Trn

paint :: Pixel -> Char
paint Zro = ' '
paint One = 'O'
paint Trn = ' '

stack :: [String] -> [Pixel]
stack = zipS . fmap (fmap pixel)

zipS :: Semigroup a => [[a]] -> [a]
zipS [x]    = x
zipS (x:xs) = zipWith (<>) x $ zipS xs

render :: [Pixel] -> String
render = unlines . chunksOf 25 . fmap paint

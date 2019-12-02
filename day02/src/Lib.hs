{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    , firstStar
    , secondStar
    ) where

import Data.Array
import Data.Text (Text)
import Data.Text as T (splitOn, unpack)
import Data.Text.IO as T (readFile)
import Data.Foldable

emptyArray :: Array Int Int
emptyArray = listArray (1,0) []

restore :: [(Int,Int)] -> Array Int Int -> Array Int Int
restore xs ys = ys // xs

extract :: Array Int Int -> Maybe Int
extract xs
  | null xs   = Nothing
  | otherwise = Just $ xs ! 0

run :: Array Int Int -> Array Int Int
run xs =
  go 0 xs
  where
    go i ys
      | null ys = emptyArray
      | otherwise =
          case ys ! i of
            1  -> go (i+4) (calc (+) ys i)
            2  -> go (i+4) (calc (*) ys i)
            99 -> ys
            op -> emptyArray

    calc op ys i =
      ys // [(o, r)]
      where
        i1 = ys ! (i+1)
        i2 = ys ! (i+2)
        o  = ys ! (i+3)
        r  = (ys ! i1) `op` (ys ! i2)

firstStar :: IO ()
firstStar = do
  xs :: [Int] <- fmap read . fmap T.unpack . T.splitOn "," <$> T.readFile "src/input"
  let ys = listArray (0, length xs - 1) xs
  print . extract . run . restore [(1,12),(2,2)] $ ys

secondStar :: IO ()
secondStar = do
  xs :: [Int] <- fmap read . fmap T.unpack . T.splitOn "," <$> T.readFile "src/input"
  let ys = listArray (0, length xs - 1) xs
  print . head $
    [ 100 * n + v
    | n <- [0..99]
    , v <- [0..99]
    , let r = extract . run . restore [(1,n), (2,v)] $ ys
    , r == Just 19690720
    ]

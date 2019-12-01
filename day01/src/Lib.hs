{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( calculate1
    , calculate2
    ) where

import System.IO

calculateFuels1 :: [Int] -> Int
calculateFuels1 masses =
    sum . fmap calculateFuel $ masses

calculateFuel :: Int -> Int
calculateFuel mass = floor (fromIntegral mass / 3) - 2

calculateFuels2 :: [Int] -> Int
calculateFuels2 masses =
    sum . fmap calculateFuel2 $ masses

calculateFuel2 :: Int -> Int
calculateFuel2 mass =
    go 0 mass
    where
        go :: Int -> Int -> Int
        go acc mass
            | needsFuels mass = go (acc + calculateFuel mass) (calculateFuel mass)
            | otherwise       = acc
        needsFuels mass = calculateFuel mass > 0

calculate1 :: IO ()
calculate1 = do
  masses :: [Int] <- fmap read . lines <$> readFile "src/input"
  print $ calculateFuels1 masses

calculate2 :: IO ()
calculate2 = do
  masses :: [Int] <- fmap read . lines <$> readFile "src/input"
  print $ calculateFuels2 masses

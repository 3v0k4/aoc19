{-# LANGUAGE ParallelListComp #-}

module Lib
    ( firstStar
    , secondStar
    ) where

import Data.List
import Data.List.Split
import Linear.V2
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

firstStar :: IO ()
firstStar = do
  xs <- lines <$> readFile "src/input"
  let [ys, zs] = fmap (visited . parse) xs
  print $ closestManhattan ys zs

visited :: [V2 Int] -> Set (V2 Int)
visited = Set.fromList . scanl1 (+)

closestManhattan :: Set (V2 Int) -> Set (V2 Int) -> Int
closestManhattan xs ys = minimum . Set.map manhattan $ xs `Set.intersection` ys

manhattan :: V2 Int -> Int
manhattan (V2 x y) = abs x + abs y

parse :: String -> [V2 Int]
parse = concatMap coord . splitOn ","
  where
    coord (x:xs) = replicate (read xs) $ case x of
      'R' -> V2   1    0
      'L' -> V2 (-1)   0
      'U' -> V2   0    1
      'D' -> V2   0  (-1)

secondStar :: IO ()
secondStar = do
  xs <- lines <$> readFile "src/input"
  let [ys, zs] = fmap (visited2 . parse) xs
  print $ closestSteps ys zs

visited2 :: [V2 Int] -> Map (V2 Int) Int
visited2 = Map.fromListWith min . flip zip [1..] . scanl1 (+)

closestSteps :: Map (V2 Int) Int -> Map (V2 Int) Int -> Int
closestSteps xs ys = minimum $ Map.intersectionWith (+) xs ys

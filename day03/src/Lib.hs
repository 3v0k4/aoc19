{-# LANGUAGE ParallelListComp #-}

module Lib
    ( firstStar
    , secondStar
    ) where

import Data.List
import Data.List.Split
import Data.Maybe (fromJust)

firstStar :: IO ()
firstStar = do
  xs <- fmap (splitOn ",") . lines <$> readFile "src/input"
  case xs of (x:y:_) -> print $ closestM x y

secondStar :: IO ()
secondStar = do
  xs <- fmap (splitOn ",") . lines <$> readFile "src/input"
  case xs of (x:y:_) -> print $ closestS x y

origin = (0,0)

type Coord = (Int,Int)
type Line = (Coord,Coord)

linez :: [String] -> [Line]
linez xs = go xs origin []
  where
    go []     _ acc = reverse acc
    go (z:zs) (x,y) acc = go zs new (line : acc)
      where
        line = ((x,y), new)

        new =
          case read <$> splitAt 1 z of
            ("R",t) -> (x+t,y)
            ("L",t) -> (x-t,y)
            ("U",t) -> (x,y+t)
            ("D",t) -> (x,y-t)

overlap :: Line -> Line -> [Coord]
overlap ((xa,ya),(xb,yb)) ((wa,za),(wb,zb)) =
  if parallel then
    []
  else
    [ (x,y)
    | x <- [min xa xb..max xa xb] `intersect` [min wa wb..max wa wb]
    | y <- [min ya yb..max ya yb] `intersect` [min za zb..max za zb]
    ]
  where
    parallel = (xa == xb && wa == wb && xa /= wa) || (ya == yb && za == zb && ya /= za)

overlaps :: [Line] -> [Line] -> [Coord]
overlaps xs ys = concat $
  [ overlap l1 l2
  | l1 <- xs
  , l2 <- ys
  ]

closestM :: [String] -> [String] -> Int
closestM w1 w2 =
  minimum ds
  where
    xs = overlaps (linez w1) (linez w2)
    ys = filter (/= (0,0)) xs
    ds = manh origin <$> ys

manh :: (Int,Int) -> (Int,Int) -> Int
manh (x,y) (x',y') = abs (x-x') + abs (y-y')

closestS :: [String] -> [String] -> Int
closestS w1 w2 =
  minimum ds
  where
    xs = overlaps (linez w1) (linez w2)
    ys = filter (/= (0,0)) xs
    ds = (\c -> steps w1 c + steps w2 c) <$> ys

steps :: [String] -> Coord -> Int
steps xs c@(x,y) = fst $ fromJust $ find ((==) c . snd) $ zip [0..] $ coords (flatten xs)

flatten :: [String] -> [String]
flatten xs = expand `concatMap` xs
  where
    expand s = replicate (read t) d
      where
        (d,t) = splitAt 1 s

coords :: [String] -> [(Int,Int)]
coords xs = go [origin] origin xs
  where
    go acc _     []     = reverse acc
    go acc (x,y) (z:zs) =
      case z of
        "R" -> let c = (x+1,y) in go (c:acc) c zs
        "L" -> let c = (x-1,y) in go (c:acc) c zs
        "U" -> let c = (x,y+1) in go (c:acc) c zs
        "D" -> let c = (x,y-1) in go (c:acc) c zs

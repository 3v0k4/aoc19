module Lib
    ( firstStar
    , secondStar
    ) where

import Data.Tree
import Data.List.Split
import Data.List
import Data.Map
import Data.Maybe
import Data.Monoid
import Data.Tree.Zipper
import Control.Applicative
import Control.Monad

toMap :: String -> Map String [String]
toMap = fromListWith (<>) . catMaybes . fmap tupleUp . splitOn "\n"
  where
    tupleUp :: String -> Maybe (String, [String])
    tupleUp s = case splitOn ")" s of
      [a,b] -> Just (a,[b])
      _     -> Nothing

buildTree :: Map String [String] -> Tree String
buildTree m = go m "COM"
  where
    go :: Map String [String] -> String -> Tree String
    go m l = Node l $ fmap (go m) (fromMaybe [] (m!?l))

orbits :: Tree a -> Int
orbits = sum . zipWith (*) [0..] . fmap length . levels

firstStar :: IO ()
firstStar = do
  x <- readFile "src/input"
  print . orbits . buildTree . toMap $ x

-- Tree.Zipper utils

childrenF :: TreePos Full a -> [TreePos Full a]
childrenF = go . firstChild
  where go = catMaybes . takeWhile isJust . iterate (>>= next)

goTo :: Eq a => a -> TreePos Full a -> Maybe (TreePos Full a)
goTo x t = if label t == x then Just t else go x (childrenF t)
  where
    go :: Eq a => a -> [TreePos Full a] -> Maybe (TreePos Full a)
    go y = join . find isJust . fmap (goTo y)

ancestors :: TreePos Full a -> [TreePos Full a]
ancestors = tail . catMaybes . takeWhile isJust . iterate (>>= parent) . Just

ancestorLabelsTill :: Eq a => a -> TreePos Full a -> [a]
ancestorLabelsTill l = fmap label . ancestors . fromJust . goTo l

transfers :: TreePos Full String -> Int
transfers t = fromJust $ liftA2 (+) y2c s2c
  where
    yous = ancestorLabelsTill "YOU" t
    sans = ancestorLabelsTill "SAN" t
    commonAncestor = head $ yous `intersect` sans
    y2c  = commonAncestor `elemIndex` yous
    s2c  = commonAncestor `elemIndex` sans

secondStar :: IO ()
secondStar = do
  x <- readFile "src/input"
  print . transfers . fromTree . buildTree . toMap $ x

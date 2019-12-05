{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( firstStar
    , secondStar
    ) where

import Data.Array
import Data.Text (Text)
import Data.Text as T (splitOn, unpack)
import Data.Text.IO as T (readFile)

run :: Int -> Array Int Int -> Int
run inp xs = go 0 xs []
  where
    go i ys os = case split (ys ! i) of
      (1,ms)  -> go (i+4) (calc (+) ys i ms) os
      (2,ms)  -> go (i+4) (calc (*) ys i ms) os
      (3,ms)  -> go (i+2) (input ys i ms) os
      (4,ms)  -> go (i+2) ys (first ys i 0 : os)
      (5,ms)  -> go (up (/=) ys i ms) ys os
      (6,ms)  -> go (up (==) ys i ms) ys os
      (7,ms)  -> go (i+4) (comp (<) ys i ms) os
      (8,ms)  -> go (i+4) (comp (==) ys i ms) os
      (99,ms) -> head os

    split i = parse . padL 5 '0' . show $ i
      where parse (m3:m2:m1:o:p:_) = (read [o,p], [read [m1], read [m2], read [m3]])

    up op ys i (m1:m2:_) = if (i1 `op` 0) then i2 else i+3
      where
        i1 = first ys i m1
        i2 = second ys i m2

    input ys i _ = ys // [(i1, inp)]
      where i1 = first ys i 1

    calc op ys i (m1:m2:_) = ys // [(output ys i, r)]
      where r = first ys i m1 `op` second ys i m2

    comp op ys i (m1:m2:_) = ys // [(output ys i, r)]
      where r = if (first ys i m1 `op` second ys i m2) then 1 else 0

    first ys i m = if m == 0 then ys ! (ys ! (i+1)) else ys ! (i+1)
    second ys i m = if m == 0 then ys ! (ys ! (i+2)) else ys ! (i+2)
    output ys i = ys ! (i+3)

padL :: Int -> Char -> String -> String
padL n c s
    | length s < n  = replicate (n - length s) c ++ s
    | otherwise     = s

parse :: Text -> Array Int Int
parse string =
  let xs = fmap read . fmap T.unpack . T.splitOn "," $ string
  in listArray (0, length xs - 1) xs

firstStar :: IO ()
firstStar = do
  xs <- T.readFile "src/input"
  print . run 1 . parse $ xs

secondStar :: IO ()
secondStar = do
  xs <- T.readFile "src/input"
  print . run 5 . parse $ xs

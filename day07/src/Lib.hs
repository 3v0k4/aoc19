{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( firstStar
  , secondStar
  )
where

import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Data.Text                      ( Text )
import           Data.Text                     as T
                                                ( splitOn
                                                , unpack
                                                , pack
                                                , justifyRight
                                                )
import           Data.Text.IO                  as T
                                                ( readFile )
import           Data.Vector                    ( Vector
                                                , (!)
                                                , (//)
                                                , fromList
                                                )
import           Data.Foldable

firstStar :: IO ()
firstStar = do
  xs <- T.readFile "src/input"
  print . runAll [0 .. 4] . parse $ xs

parse :: Text -> Vector Int
parse = fromList . fmap read . fmap T.unpack . T.splitOn ","

data State = S
  { _ip :: Int
  , _input :: [Int]
  , _output :: [Int]
  , _program :: Vector Int
  , _status :: Status
  }
data Status = Hlt [Int] | Inp [Int] | Run

loop :: [Int] -> [Int] -> Vector Int -> Int
loop [a, b, c, d, e] input xs = go amps input
 where
  go (a : as) input' =
    let newAmp = step (a { _input = _input a <> input' })
    in  case newAmp of
          S _ _ _ _ Run       -> go (newAmp : as) []
          S _ _ _ _ (Hlt out) -> if null as then head out else go as out
          S _ _ _ _ (Inp out) -> go (as <> [newAmp]) out

  amps =
    [ S 0 [a] [] xs Run
    , S 0 [b] [] xs Run
    , S 0 [c] [] xs Run
    , S 0 [d] [] xs Run
    , S 0 [e] [] xs Run
    ]

data Mode = Pos | Imm deriving (Eq)
toMode :: Char -> Mode
toMode '0' = Pos
toMode '1' = Imm

step :: State -> State
step state@S {..} = go
 where
  go = case instruction of
    (1, ms) -> state { _ip = _ip + 4, _program = clc (+) ms, _status = Run }
    (2, ms) -> state { _ip = _ip + 4, _program = clc (*) ms, _status = Run }
    (3, ms) -> if null _input
      then state { _status = Inp _output, _output = [] }
      else state { _ip      = _ip + 2
                 , _input   = tail _input
                 , _program = input
                 , _status  = Run
                 }
    (4, ms) ->
      state { _ip = _ip + 2, _output = i1 Pos : _output, _status = Run }
    (5 , ms) -> state { _ip = jmp (/=) ms, _status = Run }
    (6 , ms) -> state { _ip = jmp (==) ms, _status = Run }
    (7 , ms) -> state { _ip = _ip + 4, _program = cmp (<) ms, _status = Run }
    (8 , ms) -> state { _ip = _ip + 4, _program = cmp (==) ms, _status = Run }
    (99, ms) -> state { _status = Hlt _output, _output = [] }

  instruction = parse . padLeft 5 '0' . show $ _program ! _ip
   where
    parse (m3 : m2 : m1 : o : p : _) =
      (read [o, p], [toMode m1, toMode m2, toMode m3])
    padLeft i c = T.unpack . T.justifyRight 5 '0' . T.pack

  jmp op (m1 : m2 : _) = if (i1 m1 `op` 0) then i2 m2 else _ip + 3

  input = _program // [(i1 Imm, head _input)]

  clc op (m1 : m2 : _) = _program // [(out, r)] where r = i1 m1 `op` i2 m2

  cmp op (m1 : m2 : _) = _program // [(out, r)]
    where r = if (i1 m1 `op` i2 m2) then 1 else 0

  i1 m =
    if m == Pos then _program ! (_program ! (_ip + 1)) else _program ! (_ip + 1)
  i2 m =
    if m == Pos then _program ! (_program ! (_ip + 2)) else _program ! (_ip + 2)
  out = _program ! (_ip + 3)

runAll :: [Int] -> Vector Int -> Int
runAll phases xs = maximum $ [ loop p [0] xs | p <- permutations phases ]

secondStar :: IO ()
secondStar = do
  xs <- T.readFile "src/input"
  print . runAll [5 .. 9] . parse $ xs

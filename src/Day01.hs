{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day01
       ( Input
       , filename
       , parser
       , part1
       , part2
       ) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A
import qualified Parsers as P

filename :: String
filename = "data/Day01.txt"

type Input = [Int]

parser :: A.Parser Input
parser = P.listOfParser (pDir 'L' (-1) <|> pDir 'R' 1)

pDir :: Char -> Int -> A.Parser Int
pDir c m = do
  _ <- A.char c
  x <- A.decimal
  pure (m * x)

part1 :: Input -> Int
part1 = part1' 0 50

part1' :: Int -> Int -> Input -> Int
part1' n _ [] = n
part1' n p (x:xs) = 
  let
    p' = (p + x) `mod` 100
    n' = if p' == 0 then n + 1 else n
  in 
    part1' n' p' xs

part2 :: Input -> Int
part2 = part2' 0 50

part2' :: Int -> Int -> Input -> Int
part2' n _ [] = n
part2' n p (x:xs) =
  let
    px = p + x
    r = (abs x) `div` 100
    p' = px `mod` 100
    n' = if (p' == 0) then n + 1
         else if (p > 0) && (x < 0) && ((abs x) `mod` 100 > p) then n + 1
         else if (p > 0) && (x > 0) && ((x `mod` 100) + p > 100) then n + 1
         else n
  in
    part2' (n' + r) p' xs
  

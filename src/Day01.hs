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

import Control.Applicative
import Data.Attoparsec.Text
import Debug.Trace

filename :: String
filename = "data/Day01.txt"

type Input = [Int]

parser :: Parser Input
parser = many1' pInput

pInput :: Parser Int
pInput = pLeft <|> pRight

pLeft :: Parser Int
pLeft = do
  _ <- char 'L'
  x <- decimal
  _ <- endOfLine
  pure ((-1) * x)

pRight :: Parser Int
pRight = do
  _ <- char 'R'
  x <- decimal
  _ <- endOfLine
  pure x

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
    p' = trace ("N: " <> show n <> ". P: " <> show p <> ". X: " <> show x <> ". PX: " <> show px) $ px `mod` 100
    n' = if (p' == 0) then n + 1
         else if (p > 0) && (x < 0) && ((abs x) `mod` 100 > p) then n + 1
         else if (p > 0) && (x > 0) && ((x `mod` 100) + p > 100) then n + 1
         else n
  in
    part2' (n' + r) p' xs
  

{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day05
       ( Input
       , filename
       , parser
       , part1
       , part2
       ) where

import qualified Data.Attoparsec.Text as A
import qualified Data.List as L
import qualified Parsers as P

filename :: String
filename = "data/Day05.txt"

type Input = ([(Integer, Integer)], [Integer])

parser :: A.Parser Input
parser = do
  rs <- P.listOfParser pRange
  A.endOfLine
  xs <- P.listOfIntegersParser
  pure $ (rs, xs)

pRange :: A.Parser (Integer, Integer)
pRange = do
  a <- A.decimal
  _ <- A.char '-'
  b <- A.decimal
  pure (a,b)

part1 :: Input -> Int
part1 (rs, xs) = length $ filter (\x -> any (\(a,b) -> x >= a && x <= b) rs) xs

part2 :: Input -> Integer
part2 (rs, _) = sumRanges $ mergeRanges $ L.sortOn fst rs

mergeRanges :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeRanges [] = []
mergeRanges [a] = [a]
mergeRanges ((a,b):(c,d):xs) = 
    if (b >= c) 
    then mergeRanges ((a,m):xs)
    else (a,b):mergeRanges((c,d):xs)
  where 
    m = maximum [b,d]

sumRanges :: [(Integer, Integer)] -> Integer
sumRanges xs = foldr (\(x,y) b -> (y - x) + b + 1) 0 xs
{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day03
       ( Input
       , filename
       , parser
       , part1
       , part2
       ) where

import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Parsers as P

filename :: String
filename = "data/Day03.txt"

type Input = [[Int]]

parser :: A.Parser Input
parser = P.listOfParser $ P.rowOfParser Nothing pDigit

pDigit :: A.Parser Int
pDigit = A.choice (map mkP [1..9])
  where
    mkP i = do
      _ <- A.char (C.intToDigit i)
      pure i

part1 :: Input -> Int
part1 = sum . map (maxJoltage 0 2)

maxJoltage :: Int -> Int -> [Int] -> Int
maxJoltage acc n xs = 
    if n == 1 then
      acc * 10 + x
    else
      maxJoltage (acc * 10 + x) (n - 1) ys
  where
    l = length xs - n + 1
    x = L.maximum (take l xs)
    i = M.fromJust $ L.elemIndex x xs
    ys = drop (i + 1) xs

part2 :: Input -> Int
part2 = sum . map (maxJoltage 0 12)
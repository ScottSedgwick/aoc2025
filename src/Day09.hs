{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
module Day09
       ( Input
       , filename
       , parser1
       , parser2
       , part1
       , part2
       ) where

import qualified Data.Attoparsec.Text as A
import qualified Data.List as L
import qualified Parsers as P
import Debug.Trace ( trace ) 

filename :: String
filename = "data/Day09.txt"

type P2 = (Int, Int)
type Input = [P2]
type Rect = (P2, P2)

parser1 :: A.Parser Input
parser1 = P.listOfParser pP2

pP2 :: A.Parser P2
pP2 = do
  x <- A.decimal
  _ <- A.char ','
  y <- A.decimal
  pure (x,y)

part1 :: Input -> Int
part1 xs = maximum as
  where
    ps = [(a,b) | a <- xs, b <- xs, a /= b]
    as = map area ps

area :: Rect -> Int
area ((x1,y1),(x2,y2)) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

parser2 :: A.Parser Input
parser2 = parser1

part2 :: Input -> Int
part2 xs = trace ("PS: " <> show ps <> "\nRS: " <> show rs <> "\nAS: " <> show as) $ maximum as
  where
    ps = [(a,b) | a <- xs, b <- xs, a /= b]
    rs = filter (isValid ps) ps
    as = map area rs

-- an invalid rectangle is one that has any point of any other rectangle inside it
isValid :: [Rect] -> Rect -> Bool
isValid xs p = not $ L.any (\x -> hasPointInside p x) xs

hasPointInside :: Rect -> Rect -> Bool
hasPointInside ((px1,py1),(px2,py2)) ((x1,y1),(x2,y2)) 
  =  (between [px1,px2] x1 && between [py1,py2] y1)
  || (between [px1,px2] x2 && between [py1,py2] y2)

between :: [Int] -> Int -> Bool
between bs x = x > minimum bs && x < maximum bs
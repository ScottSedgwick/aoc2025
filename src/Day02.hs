{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day02
       ( Input
       , filename
       , parser
       , part1
       , part2
       , isBad2
       ) where

import qualified Data.Attoparsec.Text as A
import Parsers

filename :: String
filename = "data/Day02.txt"

type Input = [(Int,Int)]

parser :: A.Parser Input
parser = rowOfParser (Just ',') pIntPair

pIntPair :: A.Parser (Int, Int)
pIntPair = do
    x <- A.decimal
    _ <- A.char '-'
    y <- A.decimal
    pure (x,y)

part1 :: Input -> Int
part1 = sum . filter (isBad1 . show) . concatMap (\(x,y) -> [x .. y])

isBad1 :: String -> Bool
isBad1 = isBad 2

isBad :: Int -> String -> Bool
isBad n s = (length ys == n) && allSame ys
  where
    l = length s
    sl = l `div` n
    ys = split n sl s

allSame :: Eq a => [a] -> Bool
allSame (x:xs) = all (==x) xs
allSame _ = False

split :: Int -> Int -> [a] -> [[a]]
split _ _ [] = []
split 1 _ xs = [xs]
split n l xs = take l xs : split (n - 1) l (drop l xs)

part2 :: Input -> Int
part2 = sum . filter (isBad2 . show) . concatMap (\(x,y) -> [x .. y])

isBad2 :: String -> Bool
isBad2 s = any (\n -> isBad n s) ns
  where
    l = length s
    maxn = maximum [l, 2]
    ns = [2..maxn]
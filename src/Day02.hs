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

import Control.Applicative
import qualified Data.Attoparsec.Text as A
import Debug.Trace
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
part1 xs = sum $ filter (isBad1 . show) ys
  where
    ys = concatMap (\(x,y) -> [x .. y]) xs

isBad1 :: String -> Bool
isBad1 s = isBad 2 s

allSame :: Eq a => [a] -> Bool
allSame [] = False
allSame [_] = False
allSame (x:xs) = all (==x) xs

split :: Int -> Int -> [a] -> [[a]]
split _ _ [] = []
split 1 _ xs = [xs]
split n l xs = take l xs : split (n - 1) l (drop l xs)


isBad :: Int -> String -> Bool
isBad n s = res
  where
    l = length s
    sl = l `div` n
    ys = split n sl s
    res = ((l `mod` n) == 0) && (length ys == n) && allSame ys

part2 :: Input -> Int
part2 xs = sum zs
  where
    ys = concatMap (\(x,y) -> [x .. y]) xs
    zs = filter (isBad2 . show) ys

isBad2 :: String -> Bool
isBad2 s = any (\n -> isBad n s) ns
  where
    l = length s
    maxn = maximum [l, 2]
    ns = [2..maxn]
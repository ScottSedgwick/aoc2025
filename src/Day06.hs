{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day06
       ( Input
       , filename
       , parser1
       , parser2
       , part1
       , part2
       ) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Text as T
import qualified Parsers as P

filename :: String
filename = "data/Day06.txt"

data Op = Add | Mult deriving stock (Show, Eq)

type Input = [([Int], Op)]

parser1 :: A.Parser Input
parser1 = do
  xs <- P.rowOfParser' (Just A.skipSpace) A.decimal
  ys <- P.rowOfParser' (Just A.skipSpace) (P.pValue '*' Mult <|> P.pValue '+' Add)
  let zs = rotl $ split (length ys) xs
  pure (zip zs ys)

split :: Int -> [a] -> [[a]]
split _ [] = []
split n xs = take n xs : split n (drop n xs)

rotl :: [[x]] -> [[x]]
rotl = reverse . L.transpose . map reverse

part1 :: Input -> Int
part1 = sum . map calculate

calculate :: ([Int], Op) -> Int
calculate (xs, Mult) = foldr (\a b -> a * b) 1 xs
calculate (xs, Add ) = foldr (\a b -> a + b) 0 xs

parser2 :: A.Parser Input
parser2 = do
  xs <- lines <$> T.unpack <$> A.takeText
  let sep = L.replicate (length xs - 1) ' '
  let ys = map toOp (words (last xs))
  let zs = map (map (\x -> read x :: Int)) $ S.splitOn [sep] $ rotl $ init xs
  pure $ zip zs ys

toOp :: String -> Op
toOp "+" = Add
toOp _ = Mult

part2 :: Input -> Int
part2 = sum . map calculate
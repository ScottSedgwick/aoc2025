{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day04
       ( Input
       , filename
       , parser
       , part1
       , part2
       ) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as A
import qualified Data.Maybe as M
import qualified Data.Map as Map
import qualified Parsers as P
import qualified Utils as U

filename :: String
filename = "data/Day04.txt"

data Content = NoRoll
             | Roll
             deriving stock (Show, Eq)

type Input = Map.Map (Int, Int) Content

parser :: A.Parser Input
parser = U.mapFrom2dList <$> ( P.listOfParser $ P.rowOfParser Nothing (P.pValue '.' NoRoll <|> P.pValue '@' Roll) )

part1 :: Input -> Int
part1 = length . getOpens

getOpens :: Input -> [(Int, Int)]
getOpens xs = filter (isOpen xs) [(x,y) | x <- [0..lx], y <- [0..ly]]
  where
    (lx, ly) = maximum (Map.keys xs)

isOpen :: Input -> (Int, Int) -> Bool
isOpen xs p = M.maybe False (\c -> (c == Roll) && countAdjacent p xs < 4) (Map.lookup p xs)

countAdjacent :: (Int, Int) -> Input -> Int
countAdjacent p = length . filter (==Roll) . adjacents p

adjacents :: (Int, Int) -> Input -> [Content]
adjacents p xs = M.catMaybes (map (\d -> Map.lookup (p `U.tupleAdd` d) xs) ds)
  where
    ds = [ (-1,-1), (-1, 0), (-1,1), (0,-1), (0,1), (1,-1), (1, 0), (1,1) ]

part2 :: Input -> Int
part2 = p2 0

p2 :: Int -> Input -> Int
p2 acc xs = if ps == []
            then acc
            else p2 (acc + length ps) (removeRolls ps xs)
  where
    ps = getOpens xs

removeRolls :: [(Int, Int)] -> Input -> Input
removeRolls [] xs = xs
removeRolls (p:ps) xs = removeRolls ps (Map.insert p NoRoll xs)
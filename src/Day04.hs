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
import Utils (Pos2(..), mapFrom2dList)

filename :: String
filename = "data/Day04.txt"

data Content = NoRoll
             | Roll
             deriving stock (Show, Eq)

type Input = Map.Map Pos2 Content

parser :: A.Parser Input
parser = mapFrom2dList <$> ( P.listOfParser $ P.rowOfParser Nothing (P.pValue '.' NoRoll <|> P.pValue '@' Roll) )

part1 :: Input -> Int
part1 = length . getOpens

getOpens :: Input -> [Pos2]
getOpens xs = filter (isOpen xs) [Pos2 (x,y) | x <- [0..lx], y <- [0..ly]]
  where
    (Pos2 (lx, ly)) = maximum (Map.keys xs)

isOpen :: Input -> Pos2 -> Bool
isOpen xs p = M.maybe False (\c -> (c == Roll) && countAdjacent p xs < 4) (Map.lookup p xs)

countAdjacent :: Pos2 -> Input -> Int
countAdjacent p = length . filter (==Roll) . adjacents p

adjacents :: Pos2 -> Input -> [Content]
adjacents p xs = M.catMaybes (map (\d -> Map.lookup (p + d) xs) ds)
  where
    ds = [ Pos2 (-1,-1), Pos2 (-1, 0), Pos2 (-1,1), Pos2 (0,-1), Pos2 (0,1), Pos2 (1,-1), Pos2 (1, 0), Pos2 (1,1) ]

part2 :: Input -> Int
part2 = p2 0

p2 :: Int -> Input -> Int
p2 acc xs = if ps == []
            then acc
            else p2 (acc + length ps) (removeRolls ps xs)
  where
    ps = getOpens xs

removeRolls :: [Pos2] -> Input -> Input
removeRolls [] xs = xs
removeRolls (p:ps) xs = removeRolls ps (Map.insert p NoRoll xs)
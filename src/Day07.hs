{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Day07
       ( Input
       , filename
       , parser1
       , parser2
       , part1
       , part2
       ) where

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Array.Unboxed (range, (!), elems, indices, listArray, bounds, UArray, Array)
import qualified Data.Attoparsec.Text as A
import Data.Data (Data)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.Set as S
import GHC.Generics (Generic)
import qualified Parsers as P
import Utils (Pos2(..), mapFrom2dList, south, east, west, toInt)
import Debug.Trace 

filename :: String
filename = "data/Day07.txt"

data Cell = Split | Space | Start deriving stock (Show, Eq)

type Input = M.Map Pos2 Cell

parser1 :: A.Parser Input
parser1 = mapFrom2dList <$> ( P.listOfParser $ P.rowOfParser Nothing (P.pValue '.' Space <|> P.pValue '^' Split <|> P.pValue 'S' Start) )

part1 :: Input -> Int
part1 xs = IS.size rs
  where
    s = findStart xs
    m = findEndRow xs
    ss = getSplitters xs
    rs = getUsed (s + south + south) m ss

findStart :: Input -> Pos2
findStart xs = fst $ head $ M.toList $ M.filter (==Start) xs

findEndRow :: Input -> Int
findEndRow xs = x
  where
    (Pos2 (x,_)) = fst $ last $ M.toList xs

getSplitters :: Input -> S.Set Pos2
getSplitters xs = S.fromList $ M.keys ys
  where
    ys = M.filter (==Split) xs

getUsed :: Pos2 -> Int -> S.Set Pos2 -> IS.IntSet
getUsed pos mx ss = go pos IS.empty
  where
    go !p !us =
        if (getX p >= mx)
        then us
        else if ((toInt p) `IS.member` us)
             then us
             else if (p `S.member` ss)
                  then go pse (go psw us')
                  else go pss us
      where
        pss = p + south + south
        pse = pss + east
        psw = pss + west
        us' = IS.insert (toInt p) us

getX :: Pos2 -> Int
getX (Pos2 (x,_)) = x

parser2 :: A.Parser Input
parser2 = parser1

-- Start at the bottom row, and work up until we get to the Start

-- data Coord = C !Int !Int
--   deriving (Read, Show, Ord, Eq, Generic, Data)

part2 :: Input -> Int
part2 = undefined -- xs = sum [beam ! i | i <- range (C hir loc, C hir hic) ]
  -- where
  --   input = M.foldWithKey pos2ToCoord [] xs
  --   beam = simulateBeam input
  --   (C _ loc, C hir hic) = bounds input

-- pos2ToCoord :: Pos2 -> Cell -> [(Coord, Cell)] -> [(Coord, Cell)]
-- pos2ToCoord (Pos2(x,y)) c xs = (C x y) : xs

-- simulateBeam :: UArray Coord Cell -> Array Coord Int
-- simulateBeam input = counts
--   where
--     check i xs = if arrIx input i `elem` map Just xs then counts ! i else 0
--     counts = listArray (bounds input)
--       [ if 'S' == input ! i then 1 else u + l + r
--       | i <- indices input
--       , let u = check (above i) Start
--             l = check (above (left i)) Split
--             r = check (above (right i)) Split
--       ]
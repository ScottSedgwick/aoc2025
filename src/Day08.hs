{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
module Day08
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
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Parsers as P
import Utils (Pos2(..), mapFrom2dList, south, east, west, toInt)
import Debug.Trace 

filename :: String
filename = "data/Day08.txt"

type P3 = (Int,Int,Int)
type Input = [P3]

parser1 :: A.Parser Input
parser1 = P.listOfParser pTriple

pTriple :: A.Parser P3
pTriple = do
  x <- A.decimal
  _ <- A.char ','
  y <- A.decimal
  _ <- A.char ','
  z <- A.decimal
  pure (x,y,z)


part1 :: Input -> Int
part1 xs = trace ("Length xs: " <> show (length xs) <> ".\nLength ps: " <> show (length ds) <> ".\nPS: " <> show ds <> ".\nGS: " <> show gs) $ length ds
  where
    ps = everySecond $ L.sort $ [(a,b) | a <- xs, b <- xs, a /= b]
    ds = L.sortBy compareDist $ map (\(x,y) -> (dist (x,y), x, y)) ps
    gs = joinGroups 10 [] ds

joinGroups :: Int -> [[P3]] -> [(Float, P3, P3)] -> [[P3]]
joinGroups 1 acc _  = acc
joinGroups _ acc [] = acc
joinGroups n acc ((_, p1, p2):xs) = 
    if p1 `L.notElem` acc' && p2 `L.notElem` acc'
    then joinGroups (n-1) ([p1, p2] : acc) xs
    else joinGroups (n-1) (map addPts acc) xs
  where
    acc' = concat acc
    addPts ps = if p1 `L.elem` ps 
                then p2:ps
                else if p2 `L.elem` ps 
                     then p1:ps 
                     else ps

connect :: Int -> Input -> [Input] -> [Input]
connect 1 _  ys = ys
connect n xs ys = connect (n - 1) xs' ys'
  where
    (x, y) = closest xs ys
    xs' = L.delete x xs
    ys' = addPos x y (L.delete [x] ys)

everySecond :: [a] -> [a]
everySecond (a:b:xs) = a : everySecond xs
everySecond _ = []

closest :: Input -> [Input] -> (P3, P3)
closest xs ys = (x, y)
  where
    ps = [(dist (x, y), x, y) | x <- xs, y <- concat ys, x /= y]
    (_,x,y) = L.minimumBy (\(d1,_,_) (d2,_,_) -> d2 `compare` d1) ps

addPos :: P3 -> P3 -> [Input] -> [Input]
addPos x y ys = map (\zs -> if y `L.elem` zs then x:zs else zs) ys

dist :: (P3, P3) -> Float
dist ((x1, y1, z1), (x2, y2, z2)) = sqrt $ fromIntegral (dx * dx + dy * dy + dz * dz)
  where
    dx = abs (x2 - x1)
    dy = abs (y2 - y1)
    dz = abs (z2 - z1)

compareDist :: (Float, P3, P3) -> (Float, P3, P3) -> Ordering
compareDist (a, x, _) (b, y, _) = 
  case (a `compare` b) of
    EQ -> x `compare` y
    z -> z

parser2 :: A.Parser Input
parser2 = parser1

part2 :: Input -> Int
part2 = undefined
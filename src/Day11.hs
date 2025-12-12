{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StrictData #-}
module Day11
       ( Input
       , filename
       , parser
       , solution
       ) where

import qualified Data.Attoparsec.Text as A
import           Data.Char (ord)
import qualified Data.List as L
import qualified Data.IntMap as M
import           Data.MemoTrie (memo2)
import qualified Parsers as P
import Debug.Trace ( trace ) 

filename :: String
filename = "data/Day11.txt"

type Input = M.IntMap [Int]

parser :: A.Parser Input
parser = M.fromList <$> P.listOfParser pDevice

pDevice :: A.Parser (Int, [Int])
pDevice = do
  a <- p3char
  _ <- A.char ':'
  _ <- A.char ' '
  bs <- P.rowOfParser (Just ' ') p3char
  pure (a, bs)

p3char :: A.Parser Int
p3char = do
  a <- A.letter
  b <- A.letter
  c <- A.letter
  pure $ strVal [a,b,c]

strVal :: String -> Int
strVal [a,b,c] = getVal c + (26 * (getVal b + (26 * getVal a)))
strVal _ = 0

getVal :: Char -> Int
getVal c = ord c - ord 'a'

solution :: Input -> IO ()
solution xs = do
  let paths = memo2 \src dst ->
          if src == dst then (1 :: Int)
          else sum [paths nxt dst | nxt <- M.findWithDefault [] src xs]

  let mkRoute x []    = paths x (strVal "out")
      mkRoute x (y:z) = paths x y * mkRoute y z

  let solve src mids = sum (map (mkRoute src) (L.permutations mids))
  
  putStrLn "Part 1:"
  print $ solve (strVal "you") []

  putStrLn "Part 2:"
  print $ solve (strVal "svr") [(strVal "fft"), (strVal "dac")]
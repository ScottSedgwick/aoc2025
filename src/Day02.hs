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
       ) where

import Control.Applicative
import Data.Attoparsec.Text
import Debug.Trace

filename :: String
filename = "data/Day02.txt"

type Input = [Int]

parser :: Parser Input
parser = undefined

part1 :: Input -> Int
part1 = undefined

part2 :: Input -> Int
part2 = undefined  

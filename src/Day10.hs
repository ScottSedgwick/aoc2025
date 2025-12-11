{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}
module Day10
       ( Input
       , filename
       , parser1
       , parser2
       , part1
       , part2
       ) where

import           Algorithm.Search (aStar)
import           Control.Applicative ((<|>))
import           Control.Monad (replicateM)
import qualified Data.Attoparsec.Text as A
import           Data.Bits ((.|.), bit, xor)
import           Data.Int (Int32)
import qualified Parsers as P
import Data.SBV (SymbolicT, optLexicographic, sAll, sAnd, sInteger_, minimize, (.==), (.>=), constrain, getModelValue)

filename :: String
filename = "data/Day10.txt"

data Machine = Machine
  { lights :: Int32
  , buttons :: [[Int]]
  , jolts :: [Integer]
  } deriving stock (Show, Eq)

type Input = [Machine]

parser1 :: A.Parser Input
parser1 = P.listOfParser pMachine

pMachine :: A.Parser Machine
pMachine = do
  _ <- A.char '['
  ls <- P.rowOfParser Nothing (P.pValue '.' False <|> P.pValue '#' True)
  _ <- A.char ']'
  _ <- A.char ' '
  bs <- P.rowOfParser (Just ' ') pButton
  _ <- A.char '{'
  js <- P.rowOfParser (Just ',') A.decimal
  _ <- A.char '}'
  pure $ Machine { lights = convertLights ls, buttons = bs, jolts = js }

convertLights :: [Bool] -> Int32
convertLights xs = convertInts bs
  where
    ps = zip [0..] xs
    ts = filter snd ps
    bs = map fst ts

convertInts :: [Int] -> Int32
convertInts = foldr (\a b -> b .|. bit a) 0

pButton :: A.Parser [Int]
pButton = do
  _ <- A.char '('
  xs <- P.rowOfParser (Just ',') A.decimal
  _ <- A.char ')'
  pure xs

part1 :: Input -> Int
part1 xs = sum ps
  where
    ps = map getLeastPresses xs

-- Use aStar to find the least cost traversal of the graph.
getLeastPresses :: Machine -> Int
getLeastPresses (Machine { lights = ls, buttons = bs, jolts = _}) = 
    case aStar neighbours nCost estRemainCost isDone 0 of
      Nothing -> -1000000
      Just (cost, _) -> cost
  where
    neighbours s = map (xor s) (map convertInts bs)
    nCost _ _ = 1
    estRemainCost _ = 8
    isDone s = s == ls

parser2 :: A.Parser Input
parser2 = parser1

part2 :: Input -> IO Integer
part2 xs = do
  -- Solve each machine separately
  ys <- traverse solve2 xs
  -- Add the sums together
  pure (sum ys)

-- optLexicographic :: Satisfiable a => a -> IO SMTResult
-- getModelValue :: SymVal b => String -> a -> Maybe b
solve2 :: Machine -> IO Integer
solve2 (Machine { lights = _, buttons = bs, jolts = js}) = do
  let modelName = "Smallest Button Press Count"
  -- Run the linear solver
  res <- optLexicographic (solver modelName bs js)
  -- Extact the final result, if it exists.
  case getModelValue modelName res of
    Just x -> pure x
    Nothing -> error "No solution"

-- constrain :: (SolverContext m, QuantifiedBool a) => a -> m ()
-- sAll :: (a -> SBool) -> [a] -> SBool
-- sAnd :: [SBool] -> SBool
-- minimize :: Metric a => String -> SBV a -> Symbolic () 
solver :: String -> [[Int]] -> [Integer] -> SymbolicT IO ()
solver modelName bs js = do
    -- Create coefficient list
    cs <- replicateM (length bs) sInteger_
    -- Constrain coefficients to be positive
    constrain (sAll (.>= 0) cs)
    -- Constrain coefficients to generate the correct amount of jolt
    constrain (sAnd [ fromIntegral j .== sum [c | (c, b) <- zip cs bs, i `elem` b] | (i, j) <- zip [0..] js ])
    -- Tell the solver to seek the minimum value of the sum of coefficients
    minimize modelName (sum cs)


module Utils 
  ( index2dList
  , mapFrom2dList
  , tupleAdd
  ) where

import qualified Data.Map as M

index2dList :: [[a]] -> [((Int, Int), a)]
index2dList [] = []
index2dList xs = irow 0 xs
  where
    irow _ [] = []
    irow r (y:ys) = icol r 0 y <> irow (r + 1) ys
    icol _ _ [] = []
    icol r c (z:zs) = ((r,c), z) : icol r (c + 1) zs

mapFrom2dList :: [[a]] -> M.Map (Int, Int) a
mapFrom2dList = M.fromList . index2dList

tupleAdd :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
tupleAdd (a,b) (c,d) = (a + c, b + d)
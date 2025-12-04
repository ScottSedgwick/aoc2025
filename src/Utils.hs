{-# LANGUAGE FlexibleInstances #-}
module Utils 
  ( Pos2(..)
  , index2dList
  , mapFrom2dList
  ) where

import qualified Data.Map as M

newtype Pos2 = Pos2 (Int, Int)

index2dList :: [[a]] -> [(Pos2, a)]
index2dList [] = []
index2dList xs = irow 0 xs
  where
    irow _ [] = []
    irow r (y:ys) = icol r 0 y <> irow (r + 1) ys
    icol _ _ [] = []
    icol r c (z:zs) = (Pos2 (r,c), z) : icol r (c + 1) zs

mapFrom2dList :: [[a]] -> M.Map Pos2 a
mapFrom2dList = M.fromList . index2dList

instance Num Pos2 where
  (+) (Pos2 (a,b)) (Pos2 (c,d)) = Pos2 (a+c, b+d)
  (-) (Pos2 (a,b)) (Pos2 (c,d)) = Pos2 (a-c, b-d)
  (*) (Pos2 (a,b)) (Pos2 (c,d)) = Pos2 (a*c, b*d)
  abs (Pos2 (a,b))              = Pos2 (abs a, abs b)
  signum (Pos2 (a,b))           = Pos2 (if a < 0 then (-1) else 1, if (b < 0) then (-1) else 1)
  fromInteger x                 = Pos2 (fromInteger x, fromInteger x)

instance Eq Pos2 where
  (==) (Pos2 (a,b)) (Pos2 (c,d)) = a == c && b == d

instance Ord Pos2 where
  compare (Pos2 (a,b)) (Pos2 (c,d)) =
    case compare a c of
      EQ -> compare b d
      x  -> x

instance Show Pos2 where
  show (Pos2 p) = show p
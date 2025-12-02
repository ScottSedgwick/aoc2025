module Parsers
  ( charToDigit
  , letters
  , pLines
  , listOfIntsParser
  , listOfParser
  , rowOfIntsParser
  , rowOfParser
  ) where

import qualified Data.Attoparsec.Text as A
import qualified Data.List as L
import qualified Data.Text as T

letters :: A.Parser String
letters = A.many1' A.letter

pLines :: A.Parser [String]
pLines = do
    xs <- T.unpack <$> A.takeText
    pure $ L.lines xs


listOfIntsParser :: A.Parser [Int]
listOfIntsParser = listOfParser A.decimal

listOfParser :: A.Parser a -> A.Parser [a]
listOfParser p = A.many1' $ do
    x <- p 
    A.option () A.endOfLine
    pure x

rowOfIntsParser :: A.Parser [Int]
rowOfIntsParser = rowOfParser (Just ',') A.decimal

rowOfParser :: Maybe Char -> A.Parser a -> A.Parser [a]
rowOfParser Nothing    p = A.many1' p
rowOfParser (Just sep) p = do
    xs <- A.many1' $ do
        x <- p
        _ <- A.option sep (A.char sep)
        pure x
    pure xs

charToDigit :: Char -> Int
charToDigit '0' = 0
charToDigit '1' = 1
charToDigit '2' = 2
charToDigit '3' = 3
charToDigit '4' = 4
charToDigit '5' = 5
charToDigit '6' = 6
charToDigit '7' = 7
charToDigit '8' = 8
charToDigit '9' = 9
charToDigit _  = -1
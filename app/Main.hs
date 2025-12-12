module Main (main) where

import Data.Attoparsec.Text (IResult(..), parse)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Day11 (filename, parser, solution)


main :: IO ()
main = do
    txt <- T.readFile filename
    case process True (parse parser txt) of
        Left  e -> putStrLn $ "Error: " <> e
        Right x -> do
            print x
            solution x    

process :: Bool -> IResult T.Text a -> Either String a
process _ (Done _ r)   = Right r
process c (Partial f)  = if c 
                         then process False (f "")
                         else Left "Partial"
process _ (Fail _ _ e) = Left e

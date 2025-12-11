module Main (main) where

import Data.Attoparsec.Text (IResult(..), parse)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Day10 (filename, parser1, part1, parser2, part2)


main :: IO ()
main = do
    txt <- T.readFile filename
    case process True (parse parser1 txt) of
        Left  e -> putStrLn $ "Error: " <> e
        Right x -> do
            print x
            putStrLn "Part1: "
            putStrLn $ show (part1 x)
    case process True (parse parser2 txt) of
        Left  e -> putStrLn $ "Error: " <> e
        Right x -> do
            -- print x
            putStrLn "Part2: "
            p2 <- part2 x
            putStrLn $ show p2
    

process :: Bool -> IResult T.Text a -> Either String a
process _ (Done _ r)   = Right r
process c (Partial f)  = if c 
                         then process False (f "")
                         else Left "Partial"
process _ (Fail _ _ e) = Left e

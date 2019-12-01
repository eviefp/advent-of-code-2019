module Main where

import Day1.Day1
import Prelude

main :: IO ()
main =
    show . solve2 <$> readFile "day-01-input-1"
        >>= putStrLn

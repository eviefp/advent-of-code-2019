module Main where

-- import Day1.Day1
-- import Day2
-- import Day3
-- import Day5
import Day6

import Prelude

main :: IO ()
main =
    solve2 <$> readFile "day-06-input-1"
        >>= maybe
                (putStrLn "wot")
                (print)

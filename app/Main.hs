module Main where

-- import Day1.Day1
-- import Day2
-- import Day3
import Day5

import Prelude

main :: IO ()
main =
    solve1 <$> readFile "day-05-input-1"
        >>= maybe
                (putStrLn "wot")
                (print)

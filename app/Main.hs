module Main where

-- import Day1.Day1
-- import Day2
import Day3
import Prelude

main :: IO ()
main =
    solve2 <$> readFile "day-03-input-1"
        >>= maybe
                (putStrLn "wot")
                print

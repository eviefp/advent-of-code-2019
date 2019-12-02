module Main where

-- import Day1.Day1
import Day2
import Prelude

main :: IO ()
main =
    solve2 <$> readFile "day-02-input-1"
        >>= \(Just (noun, verb))-> print (noun * 100 + verb)

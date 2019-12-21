module Main where

-- import Day1.Day1
import Day2
-- import Day3
-- import Day5
-- import Day6
-- import Day7
--import Day8

import Control.Monad.State.Strict (runState)
import Data.Machine.Process
import Data.Machine.Source
import Data.Machine.Type
import IntCalc
import Text.Megaparsec            (many, parseMaybe)

import Prelude

main :: IO ()
main = pure ()
    -- readFile "day-08-input-1" >>= solve2 (25, 6)

input5 :: IO String
input5 = readFile "day-05-input-1"

input7 :: IO String
input7 = readFile "day-07-input-1"

raw :: String -> [Int]
raw inp =
    let res = parseMaybe (many parseItem) inp
    in case res of Just x -> x

m' = construct intMachine
m = repeated (Value 5) ~> m'

runMachine machine inputs =
    runT machine `runState` State (Value <$> inputs) (Index 0)

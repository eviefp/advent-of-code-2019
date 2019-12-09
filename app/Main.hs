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
import IntCalc.IntCalc
import Text.Megaparsec            (many, parseMaybe)

import Prelude

main :: IO ()
main = pure ()
    -- readFile "day-08-input-1" >>= solve2 (25, 6)

input5 :: IO String
input5 = readFile "day-05-input-1"

raw :: IO [Int]
raw = do
    res <- parseMaybe (many parseItem) <$> input5
    pure $ case res of Just x -> x

m' = construct intMachine
m = repeated (Value 1) ~> m'

runMachine inputs =
    runT m `runState` State (Value <$> inputs) (Index 0)

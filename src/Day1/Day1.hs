module Day1.Day1
    ( solve1
    , solve2
    ) where

import Data.Foldable (fold)
import Data.Function (fix)
import Data.Monoid   (Sum (..))
import Prelude

solve1 :: String -> Int
solve1 = go . lines
  where
    go :: [String] -> Int
    go =
        getSum
            . fold
            . fmap (Sum . calculateFuel . read @Int)

solve2 :: String -> Int
solve2 = go . lines
  where
    go :: [String] -> Int
    go =
        getSum
            . fold
            . fmap (Sum . fix it . read @Int)

    it :: (Int -> Int) -> Int -> Int
    it rec x =
        let
            next = calculateFuel x
        in
            if next <= 0
                then 0
                else next + rec next

calculateFuel :: Int -> Int
calculateFuel = (subtract 2) . (`div` 3)


module Day8 where

import Data.Foldable   (traverse_)
import Data.Function   (on)
import Data.List       (minimumBy, transpose)
import Data.List.Extra (chunksOf)
import Prelude

parseInput :: String -> [Int]
parseInput xs = fmap (read . pure) . take (length  xs - 1) $ xs

layers :: (Int, Int) -> [Int] -> [[Int]]
layers (w,h) = chunksOf (w*h)

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)

solve1 :: String -> Int
solve1 =
    go . minimumBy (compare `on` countElem 0) . layers (25, 6) . parseInput
  where
    go :: [Int] -> Int
    go xs = countElem 1 xs * countElem 2 xs

solve2 :: (Int, Int) -> String -> IO ()
solve2 (w, h) =
    traverse_ putStrLn
        . fmap toPrintable
        . chunksOf w
        . fmap (head . dropWhile (== 2))
        . transpose
        . chunksOf (w * h)
        . parseInput
  where
    toPrintable :: [Int] -> String
    toPrintable = fmap i2c
    i2c :: Int -> Char
    i2c 1 = '#'
    i2c _ = ' '

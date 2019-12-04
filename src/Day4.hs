module Day4 where

import Data.List (group)
import Prelude

range :: [Int]
range = [245182 .. 790572]

consecutives :: [a] -> [(a,a)]
consecutives = zip <*> tail

increasing :: Ord a => [a] -> Bool
increasing = all (uncurry (<=)) . consecutives

duplicate :: Eq a => [a] -> Bool
duplicate = any (uncurry (==)) . consecutives

solve1' :: [Int]
solve1' = fmap read . filter (increasing &&& duplicate) . fmap show $ range

singleDuplicate :: Eq a => [a] -> Bool
singleDuplicate = any ((== 2) . length) . group

solve2' :: [Int]
solve2' = fmap read . filter (increasing &&& singleDuplicate) . fmap show $ range

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) f g a = f a && g a

module Day3 where

import           Control.Lens               ((&), (+~), (-~), (^.))
import           Control.Monad              (join)
import           Control.Monad.State.Strict (State, evalState, get, put)
import           Data.Bifunctor             (bimap)
import           Data.Function              (on)
import           Data.Functor.Foldable      (ListF (..), cataA)
import           Data.Generics.Product      (field)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Prelude
import           Text.Megaparsec            (Parsec, many, optional, parseMaybe,
                                             (<|>))
import           Text.Megaparsec.Char       (char, newline)
import           Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Direction
    = DUp
    | DRight
    | DDown
    | DLeft
    deriving (Show)

data Point = Point
    { getX :: Int
    , getY :: Int
    } deriving (Generic, Eq, Ord, Show)

parseItem :: Parser [Direction]
parseItem = dir 'U' DUp <|> dir 'R' DRight <|> dir 'D' DDown <|> dir 'L' DLeft
  where
    dir :: Char -> Direction -> Parser [Direction]
    dir chr ctor =  (`replicate` ctor) <$> value chr

    value :: Char -> Parser Int
    value chr = char chr *> decimal <* optional (char ',')

parseFile :: Parser ([Direction], [Direction])
parseFile =
    (,)
        <$> (join <$> many parseItem <* newline)
        <*> (join <$> many parseItem <* optional newline)

origin :: Point
origin = Point 0 0

solve1 :: String -> Maybe Int
solve1 input =
    mDistance
        . Set.foldr findMin (Point 999999 999999)
        . uncurry Set.intersection
        . bimap toPoints toPoints
        <$> parseMaybe parseFile input
  where
    findMin :: Point -> Point -> Point
    findMin p1 p2
      | mDistance p1 < mDistance p2 = p1
      | otherwise = p2

solve2 :: String -> Maybe Int
solve2 input =
    snd
        . Map.foldrWithKey findMin (origin, 9999999)
        . uncurry (Map.intersectionWith (+))
        . bimap toDistancePoints toDistancePoints
        <$> parseMaybe parseFile input
  where
    findMin :: Point -> Int -> (Point, Int) -> (Point, Int)
    findMin currentPoint currentCost (minPoint, minCost)
      | currentCost < minCost = (currentPoint, currentCost)
      | otherwise = (minPoint, minCost)


toDistancePoints :: [Direction] -> Map Point Int
toDistancePoints dir = evalState go (origin, 0)
  where
    go :: State (Point, Int) (Map Point Int)
    go = cataA rec (reverse dir)

    rec =
        \case
            Cons a mas ->
                mas >>=
                    \as ->
                        (\(k,v) -> Map.insertWith min k v as)
                            <$> moveDirection a
            Nil       -> pure mempty

    moveDirection :: Direction -> State (Point, Int) (Point, Int)
    moveDirection d = do
        (point, value) <- get
        let
            result = (move point d, value + 1)
        put result
        pure result

toPoints :: [Direction] -> Set Point
toPoints dir = evalState go origin
  where
    go :: State Point (Set Point)
    go = cataA rec (reverse dir)

    rec =
        \case
            Cons a mas ->
                mas >>=
                    \as -> (`Set.insert` as) <$> moveDirection a
            Nil       -> pure mempty

    moveDirection :: Direction -> State Point Point
    moveDirection d = do
        point <- get
        let result = move point d
        put result
        pure result

move :: Point -> Direction -> Point
move p d =
    p & case d of
        DUp    -> field @"getY" +~ 1
        DRight -> field @"getX" +~ 1
        DDown  -> field @"getY" -~ 1
        DLeft  -> field @"getX" -~ 1

mDistance :: Point -> Int
mDistance p = p ^. field @"getX" <+> p ^. field @"getY"

(<+>) :: Int -> Int -> Int
(<+>) = (+) `on` abs
infixl 6 <+>

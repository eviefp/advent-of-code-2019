module Day3 where

import           Control.Lens               (both, sumOf, view, (%~), (&), (+~),
                                             (-~))
import           Control.Lens.Unsound       (lensProduct)
import           Control.Monad              (join)
import           Control.Monad.State.Strict (State, evalState, get, put)
import           Data.Bifunctor             (Bifunctor, bimap)
import           Data.Generics.Product      (field)
import qualified Data.Map                   as Map
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
    uncurry Set.intersection
        . bimapBoth (Set.fromList . toResult move origin)
        <$> parseMaybe parseFile input
            >>= fmap manhattanDistance . Set.foldr findMin Nothing
  where
    findMin :: Point -> Maybe Point -> Maybe Point
    findMin p1 (Just p2)
      | manhattanDistance p1 < manhattanDistance p2 = Just p1
      | otherwise = Just p2
    findMin p1 Nothing = Just p1

    manhattanDistance :: Point -> Int
    manhattanDistance p =
        p
            & view (lensProduct (field @"getX") (field @"getY"))
            & both %~ abs
            & sumOf both
    -- but also this works all the same:
    -- sumOf (types @Int . to abs)

solve2 :: String -> Maybe Int
solve2 input =
    uncurry (Map.intersectionWith (+))
        . bimapBoth (Map.fromList . toResult step initialState)
        <$> parseMaybe parseFile input
            >>= fmap snd . Map.foldrWithKey findMin Nothing
  where
    findMin :: Point -> Int -> Maybe (Point, Int) -> Maybe (Point, Int)
    findMin currentPoint currentCost (Just (minPoint, minCost))
      | currentCost < minCost = Just (currentPoint, currentCost)
      | otherwise = Just (minPoint, minCost)
    findMin currentPoint currentCost Nothing = Just (currentPoint, currentCost)

    step :: Direction -> (Point, Int) -> (Point, Int)
    step d = bimap (move d) (+1)

    initialState = (origin, 0)

bimapBoth :: Bifunctor f => (a -> b) -> f a a -> f b b
bimapBoth f = bimap f f

toResult :: forall a. (Direction -> a -> a) -> a -> [Direction] -> [a]
toResult step initialState = (`evalState` initialState) . traverse go
  where
    go :: Direction -> State a a
    go d =
        step d <$> get
            >>= \x -> x <$ put x

move :: Direction -> Point -> Point
move d p =
    p & case d of
        DUp    -> field @"getY" +~ 1
        DRight -> field @"getX" +~ 1
        DDown  -> field @"getY" -~ 1
        DLeft  -> field @"getX" -~ 1


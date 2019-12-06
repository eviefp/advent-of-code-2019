module Day6 where

import           Control.Lens                      (traversed, (^..))
import           Control.Monad                     (join)
import           Data.Generics.Product             (types)
import qualified Data.Graph.Inductive.Graph        as Graph
import qualified Data.Graph.Inductive.PatriciaTree as Graph
import qualified Data.Graph.Inductive.Query.BFS    as Graph
import           Data.Ix                           (range)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Maybe                        (maybeToList)
import qualified Data.Tuple                        as Tuple
import           Data.Void                         (Void)
import           GHC.Generics                      (Generic)
import           Prelude
import           Text.Megaparsec                   (Parsec, many, parseMaybe)
import           Text.Megaparsec.Char              (alphaNumChar, char, newline)

type Parser = Parsec Void String

newtype Body = Body
    { getBodyName :: String
    } deriving (Generic, Eq, Ord, Show)

data Orbit = Orbit
    { from :: Body
    , to   :: Body
    } deriving (Generic, Show)

bodyParser :: Parser Body
bodyParser = Body <$> many alphaNumChar

orbitParser :: Parser Orbit
orbitParser = Orbit <$> bodyParser <*> (char ')' *> bodyParser)

inputParser :: Parser [Orbit]
inputParser = many (orbitParser <* newline)

solve1 :: String -> Maybe Int
solve1 input =
    calculateOrbits . makeOrbitalGraph
    <$> parseMaybe inputParser input

calculateOrbits :: Graph.Gr Body Orbit -> Int
calculateOrbits gr =
    sum
        . fmap (subtract 1)
        . filter (> 0)
        . fmap length
        . fmap (`Graph.bfs` gr)
        . range
        . Graph.nodeRange
        $ gr

makeOrbitalGraph :: [Orbit] -> Graph.Gr Body Orbit
makeOrbitalGraph orbits =
    Graph.mkGraph
        (fmap Tuple.swap . Map.toList $ uniqueBodies)
        edges
  where
    uniqueBodies :: Map Body Int
    uniqueBodies =
        Map.fromList
            $ zip
                (orbits ^.. traversed . types @Body)
                [1..]

    edges :: [(Int, Int, Orbit)]
    edges =
        join
            . maybeToList
            . traverse orbitToEdge
            $ orbits

    orbitToEdge :: Orbit -> Maybe (Int, Int, Orbit)
    orbitToEdge orbit@Orbit { from, to } =
        (,,)
            <$> Map.lookup from uniqueBodies
            <*> Map.lookup to uniqueBodies
            <*> pure orbit

solve2 :: String -> Maybe Int
solve2 input =
    subtract 2
            . length
            . findPath (Body "YOU") (Body "SAN")
            . makeBiDirectional
            . makeOrbitalGraph
        <$> parseMaybe inputParser input

makeBiDirectional :: Graph.Gr Body Orbit -> Graph.Gr Body Orbit
makeBiDirectional gr =
    (`Graph.insEdges` gr)
        . fmap (\(f, t, Orbit {from, to}) -> (t, f, Orbit to from))
        . Graph.labEdges
        $ gr

findPath :: Body -> Body -> Graph.Gr Body Orbit -> Maybe Graph.Path
findPath from to gr =
    Graph.esp <$> find from <*> find to <*> pure gr
  where
    find :: Body -> Maybe Graph.Node
    find body =
        lookup body
            . fmap Tuple.swap
            $ Graph.labNodes gr

module Day2
    ( solve1
    , solve2
    ) where

import Control.Lens               (ix, use, (%=), (+=), (.=), (^.), (^?), _head)
import Control.Monad              (guard)
import Control.Monad.Loops        (iterateWhile)
import Control.Monad.State.Strict (MonadState (..), execState, gets)
import Data.Generics.Product      (field)
import Data.Maybe                 (fromMaybe)
import Data.Void                  (Void)
import GHC.Generics               (Generic)
import Prelude
import Text.Megaparsec            (Parsec, many, parseMaybe, (<|>))
import Text.Megaparsec.Char       (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Operation
    = Add
    | Multiply
    | Halt
    deriving (Generic, Show)

newtype Index = Index
    { getIndex :: Int
    } deriving (Generic, Num, Show)

data Instruction = Instruction
    { operation :: Operation
    , lhs       :: Index
    , rhs       :: Index
    , result    :: Index
    } deriving (Generic, Show)

data IntMachine = IntMachine
    { values             :: [Int]
    , instructionPointer :: Int
    } deriving (Generic, Show)


parseItem :: Parser Int
parseItem = decimal <* (char ',' <|> newline)

solve1 :: String -> Maybe Int
solve1 input = parseMaybe (many parseItem) input >>= evaluateProgram 12 2

solve2 :: String -> Maybe (Int, Int)
solve2 input =
    parseMaybe (many parseItem) input
        >>= recurse

recurse :: [Int] -> Maybe (Int, Int)
recurse program = rec' ^? _head
  where
    rec' = do
        noun <- [0 .. 99]
        verb <- [0 .. 99]
        result <- maybe [] pure $ evaluateProgram noun verb program
        guard $ result == 19690720
        pure (noun, verb)

evaluateProgram :: Int -> Int -> [Int] -> Maybe Int
evaluateProgram noun verb v =
    execState
        go'
        (IntMachine v 0)
        ^? field @"values" . _head
  where
    go' :: MonadState IntMachine m => m ()
    go' = do
        field @"values" . ix 1 .= noun
        field @"values" . ix 2 .= verb
        () <$ iterateWhile isNotHalt evaluateStep

isNotHalt :: Operation -> Bool
isNotHalt =
    \case
        Halt -> False
        _    -> True

evaluateStep :: MonadState IntMachine m => m Operation
evaluateStep = do
    instr <- getNextInstruction
    evaluateInstruction instr
    pure $ instr ^. field @"operation"

getNextInstruction :: MonadState IntMachine m => m Instruction
getNextInstruction = do
    result <- gets toInstruction
    field @"instructionPointer" += 4
    pure result

toInstruction :: IntMachine -> Instruction
toInstruction IntMachine { values, instructionPointer } =
    case values ^? ix instructionPointer of
        Just 1 ->
            fromMaybe halt $
                Instruction
                    <$> pure Add
                    <*> (Index <$> values ^? ix (instructionPointer + 1))
                    <*> (Index <$> values ^? ix (instructionPointer + 2))
                    <*> (Index <$> values ^? ix (instructionPointer + 3))
        Just 2 ->
            fromMaybe halt $
                Instruction
                    <$> pure Multiply
                    <*> (Index <$> values ^? ix (instructionPointer + 1))
                    <*> (Index <$> values ^? ix (instructionPointer + 2))
                    <*> (Index <$> values ^? ix (instructionPointer + 3))
        _      -> halt

halt :: Instruction
halt =
    Instruction
        { operation = Halt
        , lhs = -1
        , rhs = -1
        , result = -1
        }

evaluateInstruction :: MonadState IntMachine m => Instruction -> m ()
evaluateInstruction Instruction { operation, lhs, rhs, result } = do
    lhs' <- getAtIndex lhs
    rhs' <- getAtIndex rhs
    case (,) <$> lhs' <*> rhs' of
        Nothing -> pure ()
        Just (l, r) ->
            evaluate operation l r result

getAtIndex :: MonadState IntMachine m => Index -> m (Maybe Int)
getAtIndex (Index i) = do
    v <- use (field @"values")
    pure $ v ^? ix i

evaluate ::  MonadState IntMachine m => Operation -> Int -> Int -> Index -> m ()
evaluate op lhs rhs Index { getIndex } = do
    field @"values" . ix getIndex %= case op of
        Add      -> const $ lhs + rhs
        Multiply -> const $ lhs * rhs
        Halt     -> id

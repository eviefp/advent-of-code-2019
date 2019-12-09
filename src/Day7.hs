module Day7 where

import Control.Lens          ((%~), (&), (.~), (^.))
import Data.Generics.Product (field)
import Data.List             (permutations)
import Data.Maybe            (catMaybes)
import Data.Semigroup        (Max (..))
import Day2                  (parseItem)
import Day5                  (Instruction (..), IntMachine (..), Output (..),
                              evaluateInstruction, evaluateProgram,
                              getNextInstruction)
import GHC.Generics          (Generic)
import Prelude
import Text.Megaparsec       (many, parseMaybe)

import Debug.Trace

solve1 :: String -> Maybe Int
solve1 input = findThrusterSignal <$> parseMaybe (many parseItem) input

findThrusterSignal :: [Int] -> Int
findThrusterSignal raw =
    getMax
        . foldMap Max
        . catMaybes
        . fmap (runProgram raw)
        $ permutations [0..4]

runProgram :: [Int] -> [Int] -> Maybe Int
runProgram raw (i1:i2:i3:i4:i5:[]) =
    evaluateProgram i1 0 raw
        >>= \a -> evaluateProgram i2 a raw
        >>= \b -> evaluateProgram i3 b raw
        >>= \c -> evaluateProgram i4 c raw
        >>= \d -> evaluateProgram i5 d raw
runProgram _ _ = Nothing

solve2 :: String -> Maybe Int
solve2 input = findFeedbackThusterSignal <$> parseMaybe (many parseItem) input

findFeedbackThusterSignal :: [Int] -> Int
findFeedbackThusterSignal raw =
    getMax
        . foldMap Max
        . catMaybes
        . fmap (runFeedbackProgram raw)
        $ permutations [5..9]

data Amplifiers = Amplifiers
    { ampA :: IntMachine
    , ampB :: IntMachine
    , ampC :: IntMachine
    , ampD :: IntMachine
    , ampE :: IntMachine
    } deriving (Generic, Show)

runFeedbackProgram :: [Int] -> [Int] -> Maybe Int
runFeedbackProgram raw (i1:i2:i3:i4:i5:[]) =
    runSteps 0 $ makeAmplifiers raw i1 i2 i3 i4 i5

runFeedbackProgram _ _ = Nothing

runSteps :: Int -> Amplifiers -> Maybe Int
runSteps i0 amps =
    case runStep i0 amps of
        Nothing               -> Nothing
        Just (Just amps', i1) -> runSteps i1 amps'
        Just (Nothing, ix)    -> Just ix

makeAmplifiers :: [Int] -> Int -> Int -> Int -> Int -> Int -> Amplifiers
makeAmplifiers raw a b c d e =
    Amplifiers
        (machine a)
        (machine b)
        (machine c)
        (machine d)
        (machine e)
  where
    machine x = IntMachine raw 0 [x]

runStep :: Int -> Amplifiers -> Maybe (Maybe Amplifiers, Int)
runStep i amps@Amplifiers { ampA, ampB, ampC, ampD, ampE } = do
    (a, aim') <- evaluateStep (ampA & field @"values" %~ (++ [i]))
    traceShowM a
    (b, bim') <- evaluateStep (ampB & field @"values" %~ (++ [a]))
    traceShowM b
    (c, cim') <- evaluateStep (ampC & field @"values" %~ (++ [b]))
    traceShowM c
    (d, dim') <- evaluateStep (ampD & field @"values" %~ (++ [c]))
    traceShowM d
    (e, eim') <- evaluateStep (ampE & field @"values" %~ (++ [d]))
    traceShowM e
    case (,,,,) <$> aim' <*> bim' <*> cim' <*> dim' <*> eim' of
        Nothing ->
            Just (Nothing, e)
        Just (aim, bim, cim, dim, eim) ->
            Just
                (Just $ amps
                    & field @"ampA"
                        . field @"instructionPointer" .~
                            (aim ^. field @"instructionPointer")
                    & field @"ampA"
                        . field @"values" .~
                            (aim ^. field @"values")

                    & field @"ampB"
                        . field @"instructionPointer" .~
                            (bim ^. field @"instructionPointer")
                    & field @"ampB"
                        . field @"values" .~
                            (bim ^. field @"values")

                    & field @"ampC"
                        . field @"instructionPointer" .~
                            (cim ^. field @"instructionPointer")
                    & field @"ampC"
                        . field @"values" .~
                            (cim ^. field @"values")

                    & field @"ampD"
                        . field @"instructionPointer" .~
                            (dim ^. field @"instructionPointer")
                    & field @"ampD"
                        . field @"values" .~
                            (dim ^. field @"values")

                    & field @"ampE"
                        . field @"instructionPointer" .~
                            (eim ^. field @"instructionPointer")
                    & field @"ampE"
                        . field @"values" .~
                            (eim ^. field @"values")
                , e
                )


evaluateStep :: IntMachine -> Maybe (Int, Maybe IntMachine)
evaluateStep im =  do
    (out, im') <- runUntilOutput im
    pure (out, runUntilInputOrHalt im')

runUntilOutput :: IntMachine -> Maybe (Int, IntMachine)
runUntilOutput IntMachine { instructionPointer, values, inputs } =
    getNextInstruction instructionPointer values
        >>= \case
                i@(Output _) ->
                    evaluateInstruction inputs instructionPointer values i
                        >>= fromOutput
                i ->
                    evaluateInstruction inputs instructionPointer values i
                        >>= fromNonOutput
                        >>= runUntilOutput
  where
    fromOutput :: (Output, IntMachine) -> Maybe (Int, IntMachine)
    fromOutput =
        \case
            (Result x, im) -> Just (x, im)
            _ -> Nothing

runUntilInputOrHalt :: IntMachine -> Maybe IntMachine
runUntilInputOrHalt im@IntMachine { instructionPointer, values, inputs } =
    getNextInstruction instructionPointer values
        >>= \case
                Input
                  | null inputs -> Just im
                i ->
                    evaluateInstruction inputs instructionPointer values i
                        >>= fromNonOutput
                        >>= runUntilInputOrHalt

fromNonOutput :: (Output, IntMachine) -> Maybe IntMachine
fromNonOutput (_, im) = Just im


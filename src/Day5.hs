module Day5 where

import Control.Lens    (ix, (&), (.~), (^?))
import Data.Bool       (bool)
import Data.List       (unfoldr)
import GHC.Generics    (Generic)
import Prelude
import Text.Megaparsec (many, parseMaybe)

import Day2 (IntMachine (..), parseItem)

data Output
    = Result Int
    | NoOutput
    | ProgramStopped
    deriving (Generic, Show)

data Instruction
    = Add BinOp
    | Mul BinOp
    | Halt
    | Input
    | Output Target
    | JumpIfNonZero Target Target
    | JumpIfZero Target Target
    | LessThan BinOp
    | Equals BinOp
    deriving (Generic, Show)

data Target = Index | Value
    deriving (Generic, Show)

data BinOp = BinOp
    { lhs :: Target
    , rhs :: Target
    }
    deriving (Generic, Show)

solve1 :: String -> Maybe [Output]
solve1 input = parseMaybe (many parseItem) input >>= evaluateProgram

evaluateProgram :: [Int] -> Maybe [Output]
evaluateProgram raw = check $ unfoldr go (IntMachine raw 0)
  where
    check = Just
    -- check :: [Output] -> Maybe Int
    -- check (Result 0 : xs) = check xs
    -- check (Result x : []) = Just x
    -- check (NoOutput : xs) = check xs
    -- check _               = Nothing
    go :: IntMachine -> Maybe (Output, IntMachine)
    go IntMachine { values, instructionPointer } =
        getNextInstruction instructionPointer values
            >>= evaluateInstruction instructionPointer values

getNextInstruction :: Int -> [Int] -> Maybe Instruction
getNextInstruction instructionPointer values =
    values ^? ix instructionPointer
        >>= toInstruction
  where
    toInstruction :: Int -> Maybe Instruction
    toInstruction =
        \case
            99    -> Just $ Halt
            1     -> Just $ Add $ BinOp Index Index
            101   -> Just $ Add $ BinOp Value Index
            1001  -> Just $ Add $ BinOp Index Value
            1101  -> Just $ Add $ BinOp Value Value

            2     -> Just $ Mul $ BinOp Index Index
            102   -> Just $ Mul $ BinOp Value Index
            1002  -> Just $ Mul $ BinOp Index Value
            1102  -> Just $ Mul $ BinOp Value Value

            3     -> Just Input
            4     -> Just $ Output Index
            104   -> Just $ Output Value

            5     -> Just $ JumpIfNonZero Index Index
            105   -> Just $ JumpIfNonZero Value Index
            1005  -> Just $ JumpIfNonZero Index Value
            1105  -> Just $ JumpIfNonZero Value Value

            6     -> Just $ JumpIfZero Index Index
            106   -> Just $ JumpIfZero Value Index
            1006  -> Just $ JumpIfZero Index Value
            1106  -> Just $ JumpIfZero Value Value

            7     -> Just $ LessThan $ BinOp Index Index
            107   -> Just $ LessThan $ BinOp Value Index
            1007  -> Just $ LessThan $ BinOp Index Value
            1107  -> Just $ LessThan $ BinOp Value Value

            8     -> Just $ Equals $ BinOp Index Index
            108   -> Just $ Equals $ BinOp Value Index
            1008  -> Just $ Equals $ BinOp Index Value
            1108  -> Just $ Equals $ BinOp Value Value

            _     -> Nothing

evaluateInstruction  :: Int -> [Int] -> Instruction -> Maybe (Output, IntMachine)
evaluateInstruction instructionPointer values instr = do
    case instr of
        Add binOp                 -> runBinOp (+) binOp
        Mul binOp                 -> runBinOp (*) binOp
        Halt                      -> Nothing
        Input                     -> runInput
        Output t                  -> runOutput t
        JumpIfNonZero cond target -> runJump (/= 0) cond target
        JumpIfZero    cond target -> runJump (== 0) cond target
        LessThan binop            -> runBinOp ((bool 0 1 .) . (<)) binop
        Equals binop              -> runBinOp ((bool 0 1 .) . (==)) binop
  where
    runBinOp :: (Int -> Int -> Int) -> BinOp -> Maybe (Output, IntMachine)
    runBinOp op BinOp { lhs, rhs } = do
        lhs' <- getTarget lhs (instructionPointer + 1)
        rhs' <- getTarget rhs (instructionPointer + 2)
        let result = op lhs' rhs'
        output <- getOutput (instructionPointer + 3)
        pure
            ( NoOutput
            , IntMachine
                ( values & ix output .~ result )
                ( instructionPointer + 4 )
            )
    runOutput :: Target -> Maybe (Output, IntMachine)
    runOutput t = do
        result <- getTarget t (instructionPointer + 1)
        pure
            ( Result result
            , IntMachine
                values
                (instructionPointer + 2)
            )
    runInput :: Maybe (Output, IntMachine)
    runInput = do
        output <- getOutput (instructionPointer + 1)
        pure
            ( NoOutput
            , IntMachine
                (values & ix output .~ 5)
                (instructionPointer + 2)
            )
    runJump :: (Int -> Bool) -> Target -> Target -> Maybe (Output, IntMachine)
    runJump f condT target = do
        cond <- getTarget condT (instructionPointer + 1)
        out <- getTarget target (instructionPointer + 2)
        pure
            ( NoOutput
            , IntMachine
                values
                ( if f cond
                    then out
                    else instructionPointer + 3
                )
            )
    getTarget :: Target -> Int -> Maybe Int
    getTarget Value i = values ^? ix i
    getTarget Index i = values ^? ix i >>= \idx -> values ^? ix idx
    getOutput :: Int -> Maybe Int
    getOutput = getTarget Value

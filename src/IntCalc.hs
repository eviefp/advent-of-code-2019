module IntCalc where

import Control.Applicative       (Alternative, empty, (<|>))
import Control.Lens              (ix, preuse, to, use, (+=), (.=), (^?))
import Control.Lens.Combinators  (reversed, _Show)
import Control.Monad.State.Class (MonadState)
import Data.Generics.Product     (field)
import Data.Machine.Is           (Is)
import Data.Machine.Plan         (PlanT, await, stop, yield)
import GHC.Generics              (Generic)
import Prelude                   hiding (pred)

data State = State
    { memory             :: ![Value]
    , instructionPointer :: !Index
    } deriving (Generic, Show)

newtype Value = Value
    { getValue :: Int
    } deriving (Generic, Show, Eq, Num, Ord)
newtype Index = Index
    { getIndex :: Int
    } deriving (Generic, Show)

data ParameterMode = IndexMode | ImmediateMode
    deriving (Generic, Show)

data Instruction
    = Halt
    | Add ParameterMode ParameterMode
    | Multiply ParameterMode ParameterMode
    | Input
    | Output ParameterMode
    | JumpIfNonZero ParameterMode ParameterMode
    | JumpIfZero ParameterMode ParameterMode
    | LessThan ParameterMode ParameterMode
    | Equals ParameterMode ParameterMode
    deriving (Generic, Show)

type Plan m a = MonadState State m => PlanT (Is Value) Value m a

paramMode :: a -> a -> ParameterMode -> a
paramMode index immediate =
    \case
        IndexMode -> index
        ImmediateMode -> immediate

liftMaybe :: Alternative m => Maybe a -> m a
liftMaybe = maybe empty pure

intMachine :: Plan m Value
intMachine =
    getNextInstruction >>= evaluateInstruction

getNextInstruction :: Plan m Instruction
getNextInstruction =
    readAtInstructionPointer
        >>= \instr ->
            advanceInstructionPointer
                >> (toInstructionDay5 instr <|> pure Halt)

readAt :: Index -> Plan m Value
readAt (Index index) =
    preuse (field @"memory" . ix index) >>= liftMaybe

readAtInstructionPointer :: Plan m Value
readAtInstructionPointer =
    use (field @"instructionPointer")
        >>= readAt

updateAtInstructionPointer :: Value -> Plan m ()
updateAtInstructionPointer value =
    readAtInstructionPointer >>= writeAt value . asIndex

writeAt :: Value -> Index -> Plan m ()
writeAt value (Index index) =
    field @"memory" . ix index .= value

asIndex :: Value -> Index
asIndex = Index . getValue

advanceInstructionPointer :: Plan m ()
advanceInstructionPointer =
    field @"instructionPointer" . field @"getIndex" += 1

toInstructionDay5 :: forall m. Monad m => Alternative m => Value -> m Instruction
toInstructionDay5 v =
    liftMaybe (getNthDigit 0 v)
        >>= \case
            1 -> parseBinaryOp Add
            2 -> parseBinaryOp Multiply
            3 -> pure Input
            4 -> Output <$> parseMode 2 v
            5 -> parseBinaryOp JumpIfNonZero
            6 -> parseBinaryOp JumpIfZero
            7 -> parseBinaryOp LessThan
            8 -> parseBinaryOp Equals
            _ -> empty
  where
    parseBinaryOp
        :: (ParameterMode -> ParameterMode -> Instruction)
        -> m Instruction
    parseBinaryOp ctor = ctor <$> parseMode 2 v <*> parseMode 3 v

    parseMode :: Int -> Value -> m ParameterMode
    parseMode index value =
        case getNthDigit index value of
            Just 1 -> pure ImmediateMode
            _      -> pure IndexMode

    getNthDigit :: Int -> Value -> Maybe Int
    getNthDigit n value =
        value ^?
            field @"getValue"
            . to show
            . reversed
            . ix n
            . to pure
            . _Show @Int

evaluateInstruction :: Instruction -> Plan m Value
evaluateInstruction =
    \case
        Halt ->
            stop
        Add mode1 mode2 ->
            binOp (+) mode1 mode2
        Multiply mode1 mode2 ->
            binOp (*) mode1 mode2
        Input ->
            await >>= storeResult >> intMachine
        Output mode ->
            readParam mode >>= yield >> intMachine
        JumpIfNonZero mode1 mode2 ->
            jumpIf (/= 0) mode1 mode2
        JumpIfZero mode1 mode2 ->
            jumpIf (== 0) mode1 mode2
        LessThan mode1 mode2 ->
            binOp lt mode1 mode2
        Equals mode1 mode2 ->
            binOp eq mode1 mode2
  where
    lt :: Value -> Value -> Value
    lt lhs rhs
      | lhs < rhs = 1
      | otherwise = 0
    eq :: Value -> Value -> Value
    eq lhs rhs
      | lhs == rhs = 1
      | otherwise = 0
    binOp
        :: (Value -> Value -> Value)
        -> ParameterMode
        -> ParameterMode
        -> Plan m Value
    binOp op mode1 mode2 =
        op <$> readParam mode1 <*> readParam mode2
            >>= storeResult
            >> intMachine

    jumpIf :: (Value -> Bool) -> ParameterMode -> ParameterMode -> Plan m Value
    jumpIf pred mode1 mode2 = do
        cond <- pred <$> readParam mode1
        out <- readParam mode2
        if cond
            then setInstructionPointer (asIndex out) >> intMachine
            else intMachine

    setInstructionPointer :: Index -> Plan m ()
    setInstructionPointer idx = field @"instructionPointer" .= idx

readParam :: ParameterMode -> Plan m Value
readParam mode =
    readAtInstructionPointer
        >>= \idx -> advanceInstructionPointer
        >> paramMode (readAt (asIndex idx)) (pure idx) mode

storeResult :: Value -> Plan m ()
storeResult value =
    readAtInstructionPointer
        >>= writeAt value . asIndex
        >> advanceInstructionPointer

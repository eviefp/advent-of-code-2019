module IntCalc.IntCalc where

import Control.Applicative       (Alternative, empty, (<|>))
import Control.Lens              (ix, preuse, to, use, (+=), (.=), (^?))
import Control.Lens.Combinators  (reversed, _Show)
import Control.Monad.State.Class (MonadState)
import Data.Generics.Product     (field)
import Data.Machine.Is           (Is)
import Data.Machine.Plan         (PlanT, await, stop, yield)
import GHC.Generics              (Generic)
import Prelude

data State = State
    { memory             :: ![Value]
    , instructionPointer :: !Index
    } deriving (Generic, Show)

newtype Value = Value
    { getValue :: Int
    } deriving (Generic, Show, Num)
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
                >> toInstructionDay5 instr

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
    liftMaybe (getNthDigit (-1) v)
        >>= \case
            0 -> pure Halt
            1 -> Add <$> parseMode 1 v <*> parseMode 2 v
            2 -> Multiply <$> parseMode 1 v <*> parseMode 2 v
            3 -> pure Input
            4 -> Output <$> parseMode 1 v
            _ -> empty
  where
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
            . ix (n + 1)
            . to pure
            . _Show @Int

evaluateInstruction :: Instruction -> Plan m Value
evaluateInstruction =
    \case
        Halt ->
            stop
        Add mode1 mode2 ->
            (+) <$> readParam mode1 <*> readParam mode2
                >>= storeResult
                >> intMachine
        Multiply mode1 mode2 ->
            (*) <$> readParam mode1 <*> readParam mode2
                >>= storeResult
                >> intMachine
        Input ->
            await >>= storeResult >> intMachine
        Output mode ->
            readParam mode >>= yield >> intMachine

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

module Circuit where

import           Control.Applicative        ((<|>))
import           Control.Lens               (at, ix, non, use, zoom, (.=),
                                             (<%=), _1, _2)
import           Control.Monad.State.Class  (MonadState)
import qualified Control.Monad.State.Strict as S (State, get, runState)
import           Data.Functor.Identity      (Identity (..))
import           Data.Generics.Product      (field)
import           Data.Machine.Is            (Is)
import           Data.Machine.Plan          (yield)
import           Data.Machine.Process
import           Data.Machine.Runner        (runT1)
import           Data.Machine.Source
import           Data.Machine.Type
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           GHC.Generics               (Generic)
import           IntCalc
import           Prelude

import Debug.Trace

data CircuitState = CircuitState
    { vm    :: Map Value State
    , start :: State
    } deriving (Generic, Show)

type IntMachine m = MachineT m (Is Value) Value

phasedIntPlan :: MonadState State m => Value -> Plan m Value
phasedIntPlan phase =
    yield phase >> intMachine

phasedMachine :: MonadState State m => Value -> IntMachine m
phasedMachine = construct . phasedIntPlan

purePhasedMachine
    :: MonadState CircuitState m
    => Value
    -> IntMachine m
purePhasedMachine v = fitM go $ phasedMachine v
  where
    go :: MonadState CircuitState m => S.State State a -> m a
    go ms = do
        st <- fromMaybe <$> use (field @"start") <*> use (field @"vm" . at v)
        let (ret, newSt) = S.runState ms st
        field @"vm" . at v .= Just newSt
        pure ret

go' :: MonadState (Map Int String, String) m => Int -> S.State String a -> m a
go' i ms = do
    st <- fromMaybe <$> use _2 <*> use (_1 . at i)
    let (ret, newSt) = S.runState ms st
    _1 . at i .= Just newSt
    pure ret

runCircuit :: MonadState CircuitState m => Value -> [Value] -> IntMachine m
runCircuit i0 (x1:x2:x3:x4:x5:[]) =
    source [x1, i0]
        ~> machine
  where
    machine =
        purePhasedMachine x2
            ~> purePhasedMachine x3
            ~> purePhasedMachine x4
            ~> purePhasedMachine x5
            ~> purePhasedMachine x1
            ~> dropping 1
runCircuit _ _ = stopped

runStep :: MonadState CircuitState m => [Value] -> IntMachine m
runStep (x1:x2:x3:x4:x5:[]) =
    purePhasedMachine x2
        ~> purePhasedMachine x3
        ~> purePhasedMachine x4
        ~> purePhasedMachine x5
        ~> purePhasedMachine x1

feedbackLoop :: MonadState CircuitState m => Value -> [Value] -> m Value
feedbackLoop seed vals = do
    -- source [head vals, seed]
    --     ~> runStep vals
    --     ~> runStep vals
    step <- runT $ runCircuit seed vals
    case step of
        []      -> pure seed
        [seed'] -> feedbackLoop seed' vals
        xs      -> traceShowM xs >> pure seed

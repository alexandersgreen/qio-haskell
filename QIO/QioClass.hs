
-- | This module defines the functions that can be used run the classical subset
-- of QIO. That is, QIO computations that only use classical unitary operations.
module QIO.QioClass where

import Data.Maybe as Maybe
import Data.Monoid as Monoid
import Control.Monad.State
import QIO.QioSyn
import QIO.Heap

-- | A classical unitary operation is defined as a function that will
-- update the current classical state.
newtype UnitaryC =  U {unU :: Int -> HeapMap -> HeapMap}

-- | The classical unitary type forms a Monoid
instance Semigroup UnitaryC where
    (U f) <> (U g) = U (\ fv h -> g fv (f fv h))

instance Monoid UnitaryC where
    mempty = U (\ fv bs -> bs)

-- | A single qubit rotation can be converted into the classical unitary type,
-- if it is indeed classical (otherwise an error is thrown).
uRotC :: Qbit -> Rotation -> UnitaryC
uRotC x f | f==rnot    = U (\ _ h -> update h x (not (fromJust (h ? x))))
          | f==rid     = mempty
          | otherwise  = error "not classical"

-- | A swap operation can be defined in the classical unitary type.
uSwapC :: Qbit -> Qbit -> UnitaryC
uSwapC x y = U (\ _ h -> hswap h x y )

-- | A conditional operation can be defined in the classical unitary type.
uCondC :: Qbit -> (Bool -> UnitaryC) -> UnitaryC
uCondC x br = U (\ fv h -> update (unU (br (fromJust (h ? x))) fv (forget h x)) x (fromJust (h ? x)))

-- | A let operation can be defined in the classical unitary type.
uLetC :: Bool -> (Qbit -> UnitaryC) -> UnitaryC
uLetC b ux = U (\ fv h -> unU (ux (Qbit fv)) (fv+1) (update h (Qbit fv) b))

-- | A unitary can be run by converting it into the classical unitary type.
runUC :: U -> UnitaryC
runUC UReturn = mempty
runUC (Rot x r u) = uRotC x r `mappend` runUC u
runUC (Swap x y u) = uSwapC x y `mappend` runUC u
runUC (Cond x us u) = uCondC x (runUC.us) `mappend` runUC u
runUC (Ulet b xu u) = uLetC b (runUC.xu) `mappend` runUC u

-- | A classical state consists of the next free qubit reference, along with
-- a Heap that represents the overall state of the current qubits in scope.
data StateC = StateC {fv :: Int, heap :: HeapMap}

-- | An initial state is defined as an empty heap, with 0 set as the next
-- free qubit referece
initialStateC :: StateC
initialStateC = StateC 0 initial

-- | A QIO computation can be converted into a stateful computation, over
-- a state of type "StateC".
runQStateC :: QIO a -> State StateC a
runQStateC (QReturn a) = return a
runQStateC (MkQbit b xq) = do (StateC fv h) <- get
                              put (StateC (fv+1) (update h (Qbit fv) b))
                              runQStateC (xq (Qbit fv))
runQStateC (ApplyU u q) = do (StateC fv h) <- get
                             put (StateC fv (unU (runUC u) fv h))
                             runQStateC q
runQStateC (Meas x qs) = do (StateC _ h) <- get
                            runQStateC (qs (fromJust (h ? x)))

-- | We can run a classical QIO computation by converting it into a stateful
-- computation, and evaluating that using the initial state.
runC :: QIO a -> a
runC q = evalState (runQStateC q) initialStateC

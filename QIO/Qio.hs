
-- | This module defines the functions that can be used to simulate the running of
-- QIO computations.
module QIO.Qio where

import Data.List
import qualified System.Random as Random
import Data.Monoid as Monoid
import Data.Maybe as Maybe
import Control.Monad.State
import QIO.QioSyn
import QIO.Vec
import QIO.VecEq
import QIO.Heap
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

-- | A "Pure" state can be thought of as a vector of classical basis states, stored
-- as Heaps, along with complex amplitudes.
type Pure = VecEqL CC HeapMap

-- | The state of a qubit can be updated in a Pure state, by mapping the update
-- operation over each Heap.
updateP :: Pure -> Qbit -> Bool -> Pure
updateP p x b = VecEqL (map (\ (h,pa) -> (update h x b,pa)) (unVecEqL p))

-- | A "Unitary" can be thought of as an operation on a HeapMap that may produce
-- a Pure state.
newtype Unitary =  U {unU :: Int -> HeapMap -> Pure }

-- | The Unitary type forms a Monoid
instance Monoid Unitary where
    mempty = U (\ fv h -> unEmbed $ return h)
    mappend (U f) (U g) = U (\ fv h -> unEmbed $ do h' <- Embed $ f fv h
                                                    h'' <- Embed $ g fv  h'
                                                    return h''
                                                 )

-- | A function that checks if a given "Rotation" is in face unitary. Note that
-- this is currently a dummy stub function, and states that any rotation is
-- unitary. (This is only o.k. at the moment as all the rotations defined in the
-- QIO library are unitary, but won't catch un-unitary user-defined Rotations)
unitaryRot :: Rotation -> Bool
unitaryRot r = True
-- TODO: update to check that the rotation is unitary...

-- | Given the four complex numbers that make up a 2-by-2 matrix, we can create
-- a Unitary that applies the rotation to the given qubit.
uMatrix :: Qbit -> (CC,CC,CC,CC) -> Unitary
uMatrix q (m00,m01,m10,m11) = U (\ fv h -> (if (fromJust(h ? q)) 
                                           then   (m01 <.> (unEmbed $ return (update h q False))) 
                                                  <+> (m11 <.> (unEmbed $ return h)) 
                                           else   (m00 <.> (unEmbed $ return h)) 
                                                  <+> (m10 <.> (unEmbed $ return (update h q True)))))

-- | A rotation can be converted into a "Unitary", using the 'uMatrix' function
uRot :: Qbit -> Rotation -> Unitary
uRot q r = if (unitaryRot r) then (uMatrix q (r (False,False),
                                              r (False,True),
                                              r (True,False),
                                              r (True,True)))
                             else error "Non unitary Rotation!"

-- | A swap operation can be defined as a Unitary
uSwap :: Qbit -> Qbit -> Unitary
uSwap x y = U (\ fv h -> unEmbed $ return (hswap h x y ))

-- | A conditional operation can be defined as a Unitary
uCond :: Qbit -> (Bool -> Unitary) -> Unitary
--uCond x us = U (\ fv h -> updateP (unU (us (h ? x)) fv (forget h x)) x (h ? x))
uCond x us = U (\ fv h -> unU (us (fromJust(h ? x))) fv h )
--whether or not to forget? (if not then no runtime error for conditionals)

-- | A let operation can be defined as a Unitary
uLet :: Bool -> (Qbit -> Unitary) -> Unitary
uLet b ux = U (\fv h -> unU (ux (Qbit fv)) (fv + 1) (update h (Qbit fv) b))
--doesn't enforce unitary
-- need Unitary -> [Qbit] ???

-- | Any member of the "U" type can be \"run\" by converting it into a Unitary.
runU :: U -> Unitary
runU UReturn = mempty
runU (Rot x a u) = uRot x a `mappend` runU u
runU (Swap x y u) = uSwap x y `mappend` runU u
runU (Cond x us u) = uCond x (runU.us) `mappend` runU u
runU (Ulet b xu u) = uLet b (runU.xu) `mappend` runU u

-- | A quantum state is a defined as the next free qubit reference, along with the
-- Pure state that represents the overall quantum state
data StateQ = StateQ { free :: Int, pureState :: Pure }

-- | The initial 'StateQ'
initialStateQ :: StateQ
initialStateQ = StateQ 0 (unEmbed $ return initial)

-- | Given a Pure state, return a sum of all the amplitudes.
pa :: Pure -> RR
pa (VecEqL as) = foldr (\ (_,k) p -> p + amp k) 0 as

-- | A Split, is defined as a probability, along with the two Pure states. 
data Split = Split { p :: RR, ifTrue,ifFalse :: Pure }

-- | Given a Pure state, we can create a Split, by essentially splitting the
-- state into the part where the qubit is True, and the part where the qubit is
-- False. This is how measurements are implemented in QIO.
split :: Pure -> Qbit -> Split
split (VecEqL as) x =
    let pas = pa (VecEqL as)
        (ift',iff') = partition (\ (h,_) -> (fromJust(h ? x))) as
        ift = VecEqL ift'
        iff = VecEqL iff'
        p_ift = if pas==0 then 0 else (pa ift)/pas
    in Split p_ift ift iff

-- | We can extend a Monad into a PMonad if it defines a way of probabilistically
-- merging two computations, based on a given probability.
class Monad m => PMonad m where
    merge :: RR -> m a -> m a -> m a

-- | IO forms a PMonad, using the random number generator to pick one of the
-- \"merged\" computations probabilistically.
instance PMonad IO where
    merge pr ift iff = do pp <- Random.randomRIO (0,1.0)
                          if pr > pp then ift else iff

-- | The type Prob is defined as a wrapper around Vectors with Real probabilities.
data Prob a = Prob {unProb :: Vec RR a}

-- | We can show a probability distribution by filtering out elements with
-- a zero probability.
instance Show a => Show (Prob a) where
    show (Prob (Vec ps)) = show (filter (\ (a,p) -> p>0) ps)

instance Functor Prob where
    fmap = liftM
 
instance Applicative Prob where
    pure = Prob . return
    (<*>) = ap

-- | Prob forms a Monad
instance Monad Prob where
    return = pure
    (Prob ps) >>= f = Prob (ps >>= unProb . f)

-- | Prob is also a PMonad, where the result of both computations are combined into
-- a probability distribution.
instance PMonad Prob where
    merge pr (Prob ift) (Prob iff) = Prob ((pr <**> ift) <++> ((1-pr) <**> iff))

-- | Given a PMonad, a QIO Computation can be converted into a Stateful computation
-- over a quantum state (of type 'StateQ').
evalWith :: PMonad m => QIO a -> State StateQ (m a)
evalWith (QReturn a) = return (return a)
evalWith (MkQbit b g) = do (StateQ f p) <- get 
                           put (StateQ (f+1) (updateP p (Qbit f) b))
                           evalWith (g (Qbit f))
evalWith (ApplyU u q) = do (StateQ f p) <- get
                           put (StateQ f (unEmbed $ do x <- Embed $ p
                                                       x' <-Embed $ uu f x
                                                       return x'
                                          )
                                )
                           evalWith q  
                               where U uu = runU u
evalWith (Meas x g) = do (StateQ f p) <- get
                         (let Split pr ift iff = split p x
                          in if pr < 0 || pr > 1 then error "pr < 0 or >1" 
                             else do put (StateQ f ift)
                                     pift <- evalWith (g True)
                                     put (StateQ f iff)
                                     piff <- evalWith (g False)
                                     return (merge pr pift piff))

-- | A QIO computation is evaluated by converting it into a stateful computation
-- and then evaluating that over the initial state.
eval :: PMonad m => QIO a -> m a
eval p = evalState (evalWith p) initialStateQ

-- | Running a QIO computation evaluates it in the IO PMonad
run :: QIO a -> IO a
run = eval

-- | Simulating a QIO computation evaluates it in the Prob PMonad
sim :: QIO a -> Prob a
sim = eval

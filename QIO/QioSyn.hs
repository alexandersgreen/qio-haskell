{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}

-- | This module defines the Syntax of the Quantum IO Monad, which is an embedded
-- language for writing quantum computations.
module QIO.QioSyn where

import Data.Monoid as Monoid
import Data.Complex
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

-- | For Real numbers, we simply use the built in Double type
type RR = Double

-- | For Complex numbers, we use the built in Complex numbers, over our Real
-- number type (i.e. Double)
type CC = Complex RR

-- | The amplitude of a complex number is the magnitude squared.
amp :: CC -> RR
amp k = (magnitude k)*(magnitude k)

-- | The type of Qubits in QIO are simply integer references.
newtype Qbit = Qbit Int deriving (Num, Enum, Eq, Ord)


-- | A rotation is in essence a two-by-two complex valued matrix
type Rotation = ((Bool,Bool) -> CC)

-- | The underlying data type of a U unitary operation
data U = UReturn | Rot Qbit Rotation U
       | Swap Qbit Qbit U | Cond Qbit (Bool -> U) U | Ulet Bool (Qbit -> U) U

-- | The underlying data type of a QIO Computation
data QIO a = QReturn a | MkQbit Bool (Qbit -> QIO a) | ApplyU U (QIO a)
           | Meas Qbit (Bool -> QIO a)


-- | The type "U" forms a Monoid
instance Semigroup U where
    --mempty = UReturn
    UReturn <> u = u
    (Rot x a u) <> u' = Rot x a (u <> u')
    (Swap x y u) <> u' = Swap x y (u <> u')
    (Cond x br u') <> u'' = Cond x br (u' <> u'')
    (Ulet b f u) <> u' = Ulet b f (u <> u')

instance Monoid  U where
  mempty = UReturn


-- | Apply the given rotation to the given qubit
rot :: Qbit -> Rotation -> U
rot x r = Rot x r UReturn

-- | Swap the state of the two given qubits
swap :: Qbit -> Qbit -> U
swap x y = Swap x y UReturn

-- | Apply the conditional unitary, depending on the value of the given qubit
cond :: Qbit -> (Bool -> U) -> U
cond x br = Cond x br UReturn

-- | Introduce an Ancilla qubit in the given state, for use in the sub-unitary
ulet :: Bool -> (Qbit -> U) -> U
ulet b ux = Ulet b ux UReturn

-- | Returns the inverse (or reverse) of the given unitary operation
urev :: U -> U
urev UReturn = UReturn
urev (Rot x r u) = urev u <> rot x (rrev r)
urev (Swap x y u) = urev u <> swap x y
urev (Cond x br u) = urev u <> cond x (urev.br)
urev (Ulet b xu u) = urev u <> ulet b (urev.xu)

-- | Apply a not rotation to the given qubit
unot :: Qbit -> U
unot x = rot x rnot

-- | Apply a hadamard rotation to the given qubit
uhad :: Qbit -> U
uhad x = rot x rhad

-- | Apply a phase rotation (of the given angle) to the given qubit
uphase :: Qbit -> RR -> U
uphase x r = rot x (rphase r)

instance Functor QIO where
    fmap = liftM

instance Applicative QIO  where
    pure  = QReturn
    (<*>) = ap

-- | The "QIO" type forms a Monad
instance Monad QIO where
    return = pure
    (QReturn a) >>= f = f a
    (MkQbit b g) >>= f = MkQbit b (\ x -> g x >>= f)
    (ApplyU u q) >>= f = ApplyU u (q >>= f)
    (Meas x g) >>= f = Meas x (\ b -> g b >>= f)

-- | Initialise a qubit in the given state (adding it to the overall quantum state)
mkQbit :: Bool -> QIO Qbit
mkQbit b = MkQbit b return

-- | Apply the given unitary operation to the current quantum state
applyU :: U -> QIO ()
applyU u =  ApplyU u (return ())

-- | Measure the given qubit, and return the measurement outcome (note that this
-- operation may affect the overall quantum state, as a measurement is destructive)
measQbit :: Qbit -> QIO Bool
measQbit x = Meas x return


-- | The identity rotation
rid :: Rotation
rid (x,y) = if x==y then 1 else 0

-- | The not rotation
rnot :: Rotation
rnot (x,y) = if x==y then 0 else 1

-- | The hadamard rotation
rhad :: Rotation
rhad (x,y) = if x && y then -h else h where h = (1/sqrt 2)

-- | The phase rotation
rphase :: RR -> Rotation
rphase _ (False,False)  = 1
rphase r (True,True)    = exp(0:+r)
rphase _ (_,_)          = 0

-- | Returns the inverse (or reverse) of the given rotation
rrev :: Rotation -> Rotation
rrev r (False,True)   = conjugate (r (True,False))
rrev r (True,False)   = conjugate (r (False,True))
rrev r xy             = conjugate (r xy)

-- | Rotations can be compared for equality.
-- They are equal if the define the same matrix.
instance Eq Rotation where
    f == g =    (f (False,False)  == g (False,False))
             && (f (False,True)   == g (False,True))
             && (f (True,False)   == g (True,False))
             && (f (True,True)    == g (True,True))
    f /= g =    (f (False,False)  /= g (False,False))
             || (f (False,True)   /= g (False,True))
             || (f (True,False)   /= g (True,False))
             || (f (True,True)    /= g (True,True))


-- | We can display a qubit reference
instance Show Qbit where
    show (Qbit q) = "(Qbit:" ++ show q ++ ")"

-- | We can display the matrix representation of a rotation
instance Show Rotation where
    show f = "(" ++ (show (f (False,False))) ++ "," ++ (show (f (False,True))) ++ "," ++ (show (f (True,False))) ++ "," ++ (show (f (True,True))) ++ ")"

-- | We can display a representation of a unitary
instance Show U where
    show u = show' u 0 (-1)

-- | A helper function for the show instance of U
show' :: U -> Int -> Int -> String
show' (UReturn) x fv = ""
show' (Rot q a u) x fv = spaces x ++ "Rotate " ++ show q ++ " by " ++ show a ++ ".\n" ++ show' u x fv
show' (Swap q1 q2 u) x fv = spaces x ++ "Swap " ++ show q1 ++ " and " ++ show q2 ++ ".\n" ++ show' u x fv
show' (Cond q f u) x fv = spaces x ++ "Cond (if " ++ show q ++ " then \n" ++ spaces (x+1) ++ "(\n" ++ show' (f True) (x+1) fv ++ spaces (x+1) ++ ")\n" ++ spaces x ++ "else \n" ++ spaces (x+1) ++ "(\n" ++ show' (f False) (x+1) fv ++ spaces (x+1) ++ ")\n" ++ show' u x fv
show' (Ulet b f u) x fv = spaces x ++ "Ulet " ++ show b ++ " (\\" ++ show (Qbit fv) ++ "->\n " ++ show' (f (Qbit fv)) x (fv-1) ++ ")\n" ++ show' u x fv

-- | A helper function that returns a string of 4\x\ spaces.
spaces :: Int -> String
spaces 0 = ""
spaces n = if (n < 0) then error "spaces: negative argument"
                      else "    " ++ spaces (n-1)

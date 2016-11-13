{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module defines the Syntax of the Quantum IO Monad, which is
-- an embedded language for writing quantum computations. It is an
-- alternative definition using the approach of defining F-Algebras.
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

-- | We can display a qubit reference
instance Show Qbit where
	show (Qbit q) = "(Qbit:" ++ show q ++ ")"

-- | A rotation is in essence a two-by-two complex valued matrix
type Rotation = ((Bool,Bool) -> CC)

-- | We can display the matrix representation of a rotation
instance Show Rotation where
    show f = "(" ++ (show (f (False,False))) ++ "," ++ (show (f (False,True))) ++ "," ++ (show (f (True,False))) ++ "," ++ (show (f (True,True))) ++ ")"

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

-- | The non-recursive data type definition of a unitary operation
data UFunctor u = UReturn 
                | Rot Qbit Rotation u
                | Swap Qbit Qbit u 
                | Cond Qbit (Bool -> u) u 
                | Ulet Bool (Qbit -> u) u

-- | In order to define an F-Algebra, 'UFunctor' must be a functor.
instance Functor UFunctor where
    fmap eval UReturn = UReturn
    fmap eval (Rot q r u) = Rot q r (eval u)  
    fmap eval (Swap q1 q2 u) = Swap q1 q2 (eval u)
    fmap eval (Cond q f u) = Cond q (eval . f) (eval u)
    fmap eval (Ulet b f u) = Ulet b (eval . f) (eval u)

-- | The fix point type construtor.
newtype Fix f = Fx (f (Fix f))

-- | We can define the inverse of Fx
unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x

-- | We fix the non-recursice data-type in order to get our type 'U'
-- of unitary operations.
type U = Fix UFunctor

-- | The type of an F-Algebra.
type Algebra f a = f a -> a

-- | The type of the initial algebra for UFunctor
type UInitialAlgebra = Algebra UFunctor U

-- | We can now define the initial algebra for U
uInitialAlgebra :: UInitialAlgebra
uInitialAlgebra = Fx

-- | We can use a catamorphism to abstract evaluation over a given
-- algebra
cata :: Functor f => Algebra f a -> Fix f -> a
cata algebra = algebra . fmap (cata algebra) . unFix

-- | The type "U" forms a Monoid. 
instance Monoid U where
    mempty = Fx UReturn
    mappend (Fx UReturn) u = u
    mappend (Fx (Rot x a u)) u' = Fx $ Rot x a (mappend u u')
    mappend (Fx (Swap x y u)) u' = Fx $ Swap x y (mappend u u')
    mappend (Fx (Cond x br u')) u'' = Fx $ Cond x br (mappend u' u'')
    mappend (Fx (Ulet b f u)) u' = Fx $ Ulet b f (mappend u u')

-- | Apply the given rotation to the given qubit
rot :: Qbit -> Rotation -> U
rot x r = Fx $ Rot x r mempty

-- | Swap the state of the two given qubits
swap :: Qbit -> Qbit -> U
swap x y = Fx $ Swap x y mempty

-- | Apply the conditional unitary, depending on the value of the given qubit
cond :: Qbit -> (Bool -> U) -> U
cond x br = Fx $ Cond x br mempty

-- | Introduce an Ancilla qubit in the given state, for use in the sub-unitary
ulet :: Bool -> (Qbit -> U) -> U
ulet b ux = Fx $ Ulet b ux mempty

-- | Apply a not rotation to the given qubit
unot :: Qbit -> U
unot x = rot x rnot

-- | Apply a hadamard rotation to the given qubit
uhad :: Qbit -> U
uhad x = rot x rhad

-- | Apply a phase rotation (of the given angle) to the given qubit
uphase :: Qbit -> RR -> U
uphase x r = rot x (rphase r) 

-- | Returns the inverse (or reverse) of the given unitary operation,
-- using an F-Algebra
urev :: U -> U
urev = cata urev_algebra
 where 
   urev_algebra :: UFunctor U -> U
   urev_algebra UReturn = Fx UReturn
   urev_algebra (Rot x r u) = u `mappend` rot x (rrev r)
   urev_algebra (Swap x y u) = u `mappend` swap x y
   urev_algebra (Cond x br u) = u `mappend` cond x br
   urev_algebra (Ulet b xu u) = u `mappend` ulet b xu

-- | We can display a representation of a unitary, using an F-Algebra
instance Show U where
  show = cata showU_algebra
    where
      showU_algebra :: UFunctor String -> String
      showU_algebra UReturn = ""
      showU_algebra (Rot q r u) = 
       "Rotate " ++ show q ++ ":" ++ show r ++ "\n" ++ u 
      showU_algebra (Swap q1 q2 u) = 
       "Swap " ++ show q1 ++ " and " ++ show q2 ++ "\n" ++ u
      showU_algebra (Cond q br u) = 
       "Cond " ++ show q ++ " \\b -> if b then (\n"
       ++ unlines (map (' ':) (lines $ br True))
       ++ ") else (\n"
       ++ unlines (map (' ':) (lines $ br False))
       ++ ")\n" ++ u
      showU_algebra (Ulet b xu u) =
       let fv = find_fv xu in
       "Ulet " ++ show fv ++ " = " ++ (if b then "1" else "0") ++ " in (\n" 
       ++ unlines (map (' ':) (lines $ xu fv))
       ++ ")\n" ++ u
      -- this is currently a dummy function
      find_fv :: (Qbit -> String) -> Qbit
      find_fv _ = -1      

-- | The non-recursive data type definition of a QIO computation
data QIOFunctor a q = QReturn a 
                    | MkQbit Bool (Qbit -> q) 
                    | ApplyU U q
                    | Meas Qbit (Bool -> q)

-- | In order to define an F-Algebra, 'UF' must be a functor.
instance Functor (QIOFunctor a) where
    fmap eval (QReturn a) = QReturn a
    fmap eval (MkQbit b f) = MkQbit b (eval . f)
    fmap eval (ApplyU u q) = ApplyU u (eval q)
    fmap eval (Meas q f) = Meas q (eval . f)

-- | We fix the non-recursice data-type in order to get our type 'U'
-- of unitary operations.
type QIOprim a = Fix (QIOFunctor a)

-- | The type of the initial algebra for UFunctor
type QIOInitialAlgebra a = Algebra (QIOFunctor a) (QIOprim a)

-- | We can now define the initial algebra for U
qioInitialAlgebra :: QIOInitialAlgebra a
qioInitialAlgebra = Fx
  
-- | The "QIO" type forms a Monad, by wrapping 'QIOprim'
data QIO a = Apply (Fix (QIOFunctor a))

-- | We can remove the wrapper.
primQIO :: QIO a -> QIOprim a
primQIO (Apply q) = q

instance Functor QIO where
    fmap = liftM
 
instance Applicative QIO  where
    pure  = Apply . Fx . QReturn
    (<*>) = ap

-- | The wrapper type 'ApplyFix' forms a Monad
instance Monad QIO where
    return = pure
    (Apply (Fx (QReturn a))) >>= f = f a
    (Apply (Fx (MkQbit b g))) >>= f = Apply . Fx $ 
      MkQbit b  (\q -> primQIO $ (Apply (g q)) >>= f)
    (Apply (Fx (ApplyU u q))) >>= f = Apply . Fx $ 
      ApplyU u $ primQIO (Apply q >>= f)
    (Apply (Fx (Meas x g))) >>= f = Apply . Fx $ 
      Meas x (\b -> primQIO $ (Apply (g b)) >>= f)

-- | Initialise a qubit in the given state (adding it to the overall quantum state)
mkQbit :: Bool -> QIO Qbit
mkQbit b = Apply . Fx $ MkQbit b (\q -> primQIO (return q))

-- | Apply the given unitary operation to the current quantum state
applyU :: U -> QIO ()
applyU u =  Apply . Fx $ ApplyU u $ primQIO (return ())

-- | Measure the given qubit, and return the measurement outcome (note that this
-- operation may affect the overall quantum state, as a measurement is destructive)
measQbit :: Qbit -> QIO Bool
measQbit x = Apply . Fx $ Meas x (\b -> primQIO (return b))

-- | We can show a QIO computation, using an F-Algebra
instance (Show a) => Show (QIO a) where
  show = (cata showQIO_algebra) . primQIO
   where
    showQIO_algebra :: (Show a) => Algebra (QIOFunctor a) String       
    showQIO_algebra (QReturn a) = 
       "Return: " ++ show a ++ "\n"
    showQIO_algebra (MkQbit b f) = 
       "Init" ++ (if b then "1" else "0") ++ "\n"
       ++ f 0
    showQIO_algebra (ApplyU u qio) = 
       "Apply Unitary: (\n"
       ++ unlines (map (' ':) (lines $ show u))
       ++ ")\n" ++ qio
    showQIO_algebra (Meas q f) = 
       "Measure " ++ show q ++ " \\b -> if b then (\n"
       ++ unlines (map (' ':) (lines $ f True))
       ++ ") else (\n"
       ++ unlines (map (' ':) (lines $ f False))
       ++ ")\n"

-- | We can count the number of each primitive operation using an F-Algebra
count :: QIO a -> (Int,Int,Int)
count = (cata count_algebra) . primQIO
 where
  count_algebra :: Algebra (QIOFunctor a) (Int,Int,Int)
  count_algebra (QReturn _) = (0,0,0)
  count_algebra (MkQbit b f) = let (mk,ap,ms) = f 0 in
                               (mk+1,ap,ms)
  count_algebra (ApplyU _ (mk,ap,ms)) = (mk,ap+1,ms)
  count_algebra (Meas q f) = let (mk,ap,ms) = f False in
                             (mk,ap,ms+1)
     
toffoli :: Qbit -> Qbit -> Qbit -> U
toffoli q1 q2 q3 = 
  cond q1 (\b1 -> if b1 then (
   cond q2 (\b2 -> if b2 then (unot q3) 
    else mempty)) else mempty)

and :: Bool -> Bool -> QIO Bool
and a b = do
 q1 <- mkQbit a
 q2 <- mkQbit b
 q3 <- mkQbit False
 applyU (toffoli q1 q2 q3)
 measQbit q3

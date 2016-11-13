{-# LANGUAGE GADTs, FlexibleInstances #-}

-- | This module defines a class of Vectors over types with Equality, along with
-- an instance of this class using lists of pairs. In the context of QIO, these
-- Vectors are used to hold the amplitudes of various basis-states within a
-- superposition.
module QIO.VecEq where

import QIO.QioSyn
import QIO.Heap
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

-- | Any type that fulfills this type class is a Vector over types with equality
class VecEq v where
    -- | An empty instance of the vector
    vzero :: v x a
    -- | Two Vectors can be combined
    (<+>) :: (Eq a, Num x) => v x a -> v x a -> v x a
    -- | A Vector can be multiplied by a scalar
    (<.>) :: (Num x, Eq x) => x -> v x a -> v x a
    -- | The amplitude of a given element can be accessed
    (<@>) :: (Eq a, Num x) => a -> v x a -> x
    -- | The vector can be created from a list of pairs
    fromList  :: [(a,x)] -> v x a
    -- | The cevtor can be converted into a list of pairs
    toList    :: v x a -> [(a,x)] 

-- | This type is a wrapper around a list of pairs.
newtype VecEqL x a = VecEqL {unVecEqL :: [(a,x)]} deriving Show

-- | An empty VecEqL is a wrapper around the empty list
vEqZero :: VecEqL x a
vEqZero = VecEqL []
          
-- | A basis state with the given amplitude can be added to a VecEqL by adding
-- the amplitudes if the state is already in the vector, or by inserting the
-- base state if it isn't already in the vector.
add :: (Eq a,Num x) => (a,x) -> VecEqL x a -> VecEqL x a
add (a,x) (VecEqL axs) = VecEqL (addV' axs)
    where addV' [] = [(a,x)]
          addV' ((by @ (b,y)):bys) | a == b = (b,x+y):bys
                                   | otherwise = by:(addV' bys)

-- | Combining two vectors is achieved by folding the add operation over
-- the second vector
vEqPlus :: (Eq a, Num x) => VecEqL x a -> VecEqL x a -> VecEqL x a
(VecEqL as) `vEqPlus` vbs = foldr add vbs as

-- | Scalar multiplcation is achieved by mapping the multiplication over
-- each pair in the vector. Multiplication by 0 is a special case, and will
-- remove all the basis states from the vector.
vEqTimes :: (Num x, Eq x) => x -> VecEqL x a -> VecEqL x a
l `vEqTimes` (VecEqL bs) | l==0 = VecEqL []
                         | otherwise = VecEqL (map (\ (b,k) -> (b,l*k)) bs)
          
-- | The amplitude of an element can be found by looking through each element
-- until the matchinf one is found.
vEqAt :: (Eq a, Num x) => a -> VecEqL x a -> x
a `vEqAt` (VecEqL []) = 0
a `vEqAt` (VecEqL ((a',b):abs)) | a == a' = b
                                | otherwise = a `vEqAt` (VecEqL abs)
          

-- | VecEqL is an instance of the VecEq class
instance VecEq VecEqL where
      vzero = vEqZero
      (<+>) = vEqPlus
      (<.>) = vEqTimes
      (<@>) = vEqAt
      fromList as = VecEqL as
      toList (VecEqL as) = as

-- | An EqMonad is a monad that has Return and Bind operations that depend on
-- the type in the monad being a member of the Eq class
class EqMonad m where
    eqReturn :: Eq a => a -> m a
    eqBind   :: (Eq a, Eq b) => m a -> (a -> m b) -> m b 

-- | Any VecEq over \v\, along with a Numeric tpye \x\ is an EqMonad.
instance (VecEq v, Num x, Eq x) => EqMonad (v x) where
    eqReturn a = fromList [(a,1)]
    eqBind va f = case toList va of
                   ([]) -> vzero
                   ((a,x):[]) -> x <.> f a
                   ((a,x):vas) -> (x <.> f a) <+> ((fromList vas) `eqBind` f)

-- | We can define a datatype that holds EqMonad operations, so that it can
-- be defined as a Monad. 
data AsMonad m a where
   Embed  :: (EqMonad m, Eq a) => m a -> AsMonad m a
   Return :: EqMonad m => a -> AsMonad m a
   Bind   :: EqMonad m => AsMonad m a -> (a -> AsMonad m b) -> AsMonad m b
 
instance EqMonad m => Functor (AsMonad m) where
    fmap = liftM
 
instance EqMonad m => Applicative (AsMonad m)  where
    pure  = Return
    (<*>) = ap

-- | We can define an AsMonad over an EqMonad, as a Monad
instance EqMonad m => Monad (AsMonad m) where
   return = pure
   (>>=) = Bind

-- | Given Equality, we can unembed the EqMonad operations from an AsMonad
unEmbed :: Eq a => AsMonad m a -> m a
unEmbed (Embed m) = m
unEmbed (Return a) = eqReturn a
unEmbed (Bind (Embed m) f) = m `eqBind` (unEmbed.f)
unEmbed (Bind (Return a) f) = unEmbed (f a)
unEmbed (Bind (Bind m f) g) = unEmbed (Bind m (\x -> Bind (f x) g))







    

 

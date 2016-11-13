{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

-- | This module defines a type class for quantum data types, as well as some
-- instances of this class for pairs, lists, and quantum integers
module QIO.Qdata where

import Data.Monoid as Monoid
import QIO.QioSyn

-- | The 'Qdata' type class defines the operation a quantum datatype must implement.
class Qdata a qa | a -> qa, qa -> a where
    mkQ :: a -> QIO qa
    measQ :: qa -> QIO a
    letU :: a -> (qa -> U) -> U
    condQ :: qa -> (a -> U) -> U

-- | The lowest-level instance of Qdata is the relation between Booleans and Qubits.
instance Qdata Bool Qbit where
    mkQ = mkQbit
    measQ = measQbit
    letU b xu = ulet b xu
    condQ q br = cond q br

-- | A pair of quantum data types is itself a quantum data type.
instance (Qdata a qa,Qdata b qb) => Qdata (a,b) (qa,qb) where

    mkQ (a,b) = do qa <- mkQ a
                   qb <- mkQ b
                   return (qa,qb)

    measQ (qa,qb) = do a <- measQ qa
                       b <- measQ qb
                       return (a,b)

    letU (a,b) xyu = letU a (\ x -> letU b (\ y -> xyu (x,y)))

    condQ (qa,qb) br = condQ qa (\x -> condQ qb (\y -> br (x,y))) 

-- | A list of quantum data is also a quantum data type
instance Qdata a qa => Qdata [a] [qa] where
    mkQ n = sequence (map mkQ n)
    measQ qs = sequence (map measQ qs)
    letU as xsu = letU' as []
        where letU' [] xs = xsu xs
              letU' (a:as) xs = letU a (\ x -> letU' as (xs++[x]))
    condQ qs qsu = condQ' qs []
        where condQ' [] xs = qsu xs
              condQ' (a:as) xs = condQ a (\ x -> condQ' as (xs++[x]))

-- | A recursive conditional on a list of quantum data
condQRec :: Qdata a qa => [qa] -> [(a -> U)] -> U
condQRec [] [] = mempty
condQRec (q:qs) (u:us) = (condQ q u) `mappend` condQRec qs us

-- | Quantum integers are of a fixed length, which is defined by this constant.
-- Currently, this is set to 4.
qIntSize :: Int
qIntSize = 4

-- | A Quantum integer is a wrapper around a fixed-length list of qubits
newtype QInt = QInt [Qbit] deriving Show

-- | Convert an integer to a list of Booleans
int2bits :: Int -> [Bool]
int2bits n = int2bits' n qIntSize
             where int2bits' 0 0 = []
                   int2bits' _ 0 = error "int2bits: too large"
                   int2bits' n l = ((n `mod` 2) /= 0) : int2bits' (n `div` 2) (l-1)

-- | Convert a list of Booleans to an integer
bits2int :: [Bool] -> Int
bits2int [] = 0
bits2int (b:bs) = (2*bits2int bs)+(if b then 1 else 0)

-- | quantum integers form a quantum data type, relating them to the classical
-- Haskell Int type.
instance Qdata Int QInt where
    mkQ n = do qn <- mkQ (int2bits n)
               return (QInt qn)
    measQ (QInt qbs) = 
        do bs <- measQ qbs
           return (bits2int bs)
    letU n xu = letU (int2bits n) (\ bs -> xu (QInt bs))
    condQ (QInt qi) qiu = condQ qi (\ x -> qiu (bits2int x)) 

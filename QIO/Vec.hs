-- | This module defines a Vector as a list of pairs. 
-- In the context of QIO, a Vector is the type used to represent a probability
-- distribution.
module QIO.Vec where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

-- | A Vector over types 'x' and 'a' is a wrapper around list of 
-- pairs of 'a' and 'x'.
newtype Vec x a = Vec {unVec :: [(a,x)]} deriving Show

-- | An empty Vector is defined as the empty list
empty :: Vec x a
empty = Vec []
 
-- | The \"probability\" of an object in a Vector, is the sum of all the
-- probabilities associated with that object.
(<@@>) :: (Num x,Eq a) => Vec x a -> a -> x
(Vec ms) <@@> a = foldr (\(b,k) m -> if a == b then m + k else m) 0 ms

-- | A Vector can be multiplied by a scalar, by mapping the multiplcation
-- over each probability in the vector.
(<**>) :: Num x => x -> (Vec x a) -> Vec x a
l <**> (Vec as) = (Vec (map (\ (a,k) -> (a,l*k)) as))

-- | Two Vectors can be added, using list concatenation.
(<++>) :: (Vec x a) -> (Vec x a) -> Vec x a
(Vec as) <++> (Vec bs) = (Vec (as ++ bs))

instance Num n => Functor (Vec n) where
    fmap = liftM
 
instance Num n => Applicative (Vec n)  where
    pure  a = Vec [(a,1)]
    (<*>) = ap

-- | Vectors, over Numeric types, can be defined as a Monad.
instance Num n => Monad (Vec n) where
    return = pure
    (Vec ms) >>= f = Vec [(b,i*j) | (a,i) <- ms, (b,j) <- unVec (f a)]
   

 





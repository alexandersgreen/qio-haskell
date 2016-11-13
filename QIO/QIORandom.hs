
-- | This module implements various functions that return a probabilistic result, 
-- defined as unitary operators, and quantum computations.
module QIO.QIORandom where

import Data.Monoid as Monoid
import QIO.QioSyn
import QIO.Qdata
import QIO.Qio
import Data.Complex

-- | The exponentiated Pauli-X rotation
rX :: RR -> Rotation
rX r (x,y) = if x==y then (cos (r/2):+0) else (0:+ (-(sin (r/2))))

-- | The exponentiated Pauli-Y rotation
rY :: RR -> Rotation
rY r (x,y) = if x==y then (cos (r/2):+0) else (s * sin (r/2):+0) where s = if x then 1 else -1

-- | Applies a Hadamard rotation to each qubit in the given list of qubits
hadamards :: [Qbit] -> U
hadamards [] = mempty
hadamards (q:qs) = uhad q `mappend` hadamards qs

-- | returns the highest integer power of 2 that is less than or equal to \x\
pow2 :: Int -> Int
pow2 x = pow2' 0
  where pow2' y | 2^(y+1) > x = 2^y
            | otherwise = pow2' (y+1)

-- | A rotation that, given a qubit in state 0, leaves it in a super-position of
-- 0 and 1, such that the probability of measuring as state 0 is \ps\.
weightedU :: RR -> Qbit -> U
weightedU ps q | sqrt ps <= 1 = rot q (rX (2*(acos (sqrt ps))))
           | otherwise = error ("weightedU: Invalid Probability: " ++ show ps)      
-- | A QIO computation that uses the "weightedU" unitary, to return a Bool that
-- has a probablity of \pf\ of being False.
weightedBool :: RR -> QIO Bool
weightedBool pf = do q <- mkQbit False
                     applyU (weightedU pf q)
                     measQ q

-- | removes any leading Falses from a list of booleans
rlf :: [Bool] -> [Bool]
rlf (False:bs) = rlf bs
rlf bs = bs

-- | removes any leading Falses from the (big-endian) bit-wise representation
-- of the given Int.
rlf_l :: Int -> [Bool]
rlf_l x = rlf (reverse (int2bits x))

-- | returns the number of bits left after calling the "flf_l" function
rlf_n :: Int -> Int
rlf_n x = length (rlf_l x)

-- | Given an Int \max\ that is the largest number required to be represented in
-- a quantum register, this function trims the front off the given register, to
-- leave the number of qubits required to represent \max\.
trim :: Int -> [Qbit] -> [Qbit]
trim max qbs = drop ((length qbs)-(rlf_n max)) qbs

-- | Given an Int \max\, and a quantum register in the state \max\, this function 
-- defines a unitary operation that will leave the quantum register in state that
-- has equal probability of being measured in any of the states 0 to \max\.
randomU :: Int -> [Qbit] -> U
randomU max qbs = randomU' max (trim max qbs)
 where
  randomU' _ [] = mempty
  randomU' 0 _ = mempty
  randomU' max (q:qbs) = weightedU (fromIntegral ((max+1)-p)/fromIntegral (max+1)) q
                 `mappend`
                 condQ q (\x -> if x then (randomU (max-p) qbs) 
                                else (hadamards qbs))
                  where p = pow2 max

-- | A quantum computation that will return a quantum integer in a state that
-- has equal probabilities of being measured in any of the state 0 to \max\.
randomQInt :: Int -> QIO QInt
randomQInt max = do 
  qbs <- mkQ (reverse (int2bits max))
  applyU (randomU max qbs)
  return (QInt (reverse qbs))

-- | A quantum computation that will return a quantum integer in a state that
-- has equal probabilities of being measured in any of the state \min\ to \max\.
randomQIO :: (Int,Int) -> QIO Int
randomQIO (min,max) = do q <- randomInt (max-min)
                         return (q + min)

-- | A quantum computation that measures the outcome of "randomQInt"
randomInt :: Int -> QIO Int
randomInt max = do 
  q <- randomQInt max
  measQ q

-- | A quantum computation that returns an integer that is equally likely to be
-- any number in the range 0 to \x\-1
random :: Int -> QIO Int
random x = randomInt (x-1)
            
-- | This function uses a Quantum computation to simulate the roll of a dice
dice :: IO Int
dice = do 
  x <- run (randomInt 5)
  return (x+1)

-- | This function simulates the given number of repitions of dice rolls
dice_rolls :: Int -> IO [Int]
dice_rolls 0 = return []
dice_rolls y = do 
  x <- dice
  xs <- dice_rolls (y-1)
  return (x:xs)

-- | Returns the number of occurences of 1 through 6 in the given list of Ints
occs :: [Int] -> (Int,Int,Int,Int,Int,Int)
occs rs = (rs' 1,rs' 2,rs' 3,rs' 4,rs' 5,rs' 6)
  where 
   rs' x = length ([y|y<-rs,y==x])

-- | Returns the number of occurences of 1 through 6 in the given number of
-- rolls of the dice.
probs' :: Int -> IO (Int,Int,Int,Int,Int,Int)
probs' x = do 
  xs <- dice_rolls x
  return (occs xs)

-- | Returns the percentage of occurences of 1 through 6, after the given number
-- of rolls of the dice.
probs :: Int -> IO (RR,RR,RR,RR,RR,RR)
probs x = do 
  (a,b,c,d,e,f) <- probs' x
  return (fromIntegral a/x',fromIntegral b/x',fromIntegral c/x',fromIntegral d/x',fromIntegral e/x',fromIntegral f/x')
    where x' = fromIntegral x

           

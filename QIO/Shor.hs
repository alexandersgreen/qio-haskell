
-- | This module defines the QIO computation that represents Shor's factorisation
-- algorithm. It makes use of the Arithmetic library, and the Quantum Fourier 
-- Transform.
module QIO.Shor where

import Data.Monoid as Monoid
import QIO.QIORandom
import QIO.QioSyn
import QIO.Qdata
import QIO.Qio
import QIO.QExamples
import QIO.QArith
import QIO.Qft
import System.Time

-- | The inverse Quantum Fourier Transform is defined by reversing the QFT
qftI :: QInt -> U
qftI (QInt i) = urev (qft i)

-- | The Hadamard transform can be mapped over the qubits in a Quantum Integer.
hadamardsI :: QInt -> U
hadamardsI (QInt xs) = hadamards xs

-- | The overall \"phase-estimation\" structure of Shor's algorithm.
shorU :: QInt -> QInt -> Int -> Int -> U
shorU k i1 x n = hadamardsI k `mappend` modExp n x k i1 `mappend` qftI k

-- | A quantum computation the implementes shor's algorithm, returning the period
-- of the function.
shor :: Int -> Int -> QIO Int
shor x n = do 
  i0 <- mkQ 0
  i1 <- mkQ 1
  applyU (shorU i0 i1 x n)
  measQ i0

-- | A classical (inefficient) implementation of the period finding subroutine 
period :: Int -> Int -> Int
period m q = r where (_,r) = reduce (m,q)

-- | A wrapper for Shor's algorithm, that returns prime factors of \n\.
factor :: Int -> QIO (Int,Int)
factor n | even n = return (2,2)
         | otherwise = do x <- rand_coprime n
                          a <- shor x n
                          let xa = x^(half a) 
                            in if odd a || xa == (n-1) `mod` n || a == 0
                               then factor n
                               else return (gcd (xa+1) n,gcd (xa-1) n)
--this function can only be run too, for similar reasons to the rand_co'
--function below

-- | This function simulates the running of a QIO computation, whilst using
-- System.Time functions to time how long the simulation took.
runTime :: QIO a -> IO a
runTime a = do 
  start <- getClockTime
  result <- run a
  stop <- getClockTime
  putStr ("The total time taken was " ++ (timeDiffToString (diffClockTimes stop start) ++ "\n"))
  return result

-- | Times the running of various subroutines within the factorisation algorithm.
factorV' :: Int -> IO (Int,Int)
factorV' n | even n = return (2,2)
           | otherwise = do 
  start <- getClockTime
  putStr ("Started at " ++ (show start) ++ "\n")
  x <- run (rand_coprime n)
  putStr ("Calling \"shor " ++ show x ++ " " ++ show n ++ "\"\n")
  a <- run (shor x n)
  stop <- getClockTime
  putStr ("Shor took " ++ (timeDiffToString (diffClockTimes stop start)) ++ "\n")
  putStr ("period a = " ++ show a)
  let xa = x^(half a) 
    in do putStr (", giving xa = " ++ show xa ++ "\n")
          if odd a || xa == (n-1) `mod` n || (gcd (xa+1) n,gcd (xa-1) n) == (1,n) || (gcd (xa+1) n,gcd (xa-1) n) == (n,1) || (gcd (xa+1) n,gcd (xa-1) n) == (1,1)
           then do putStr "Recalling factorV\n"
                   factorV' n
           else do putStr "Result: " 
                   return (gcd (xa+1) n,gcd (xa-1) n)

-- | Calls the 'factorV'', and times the overall factorisation.
factorV :: Int -> IO ()
factorV n = do start <- getClockTime
               (a,b) <- factorV' n
               stop <- getClockTime
               putStr ( "Factors of "
                       ++ (show n) 
                       ++ " include "
                       ++ (show a)
                       ++ " and "
                       ++ (show b)
                       ++ ".\n The total time taken was "
                       ++ (timeDiffToString (diffClockTimes stop start) ++ "\n"))

-- | This function defines a quantum computation that returns a random index, that
-- is used to pick from a list of integers that are co-prime to \n\.
rand_coprime :: Int -> QIO Int
rand_coprime n = do x <- randomQIO (0,(length cps)-1)
                    return (cps!!x)
                     where cps = [x | x <- [0..n], gcd x n == 1]

-- | A different implementation of "rand_coprime", that defines a quantum
-- computation that returns a random number between 2 and \n\, that is then
-- returned if it is co-prime to \n\.
rand_co' :: Int -> QIO Int
rand_co' n = do 
  x <- randomQIO (2,n)
  if gcd x n == 1 then return x else rand_co' n
--simulating this (with the sim function) gives rise to infinite paths in
--the computation, e.g. each path where gcd x n /= 1. However, this function
--can still be run (with the run function) always returning a single value.

-- | Integer division by 2.
half :: Int -> Int
half x = floor (fromIntegral x/2.0)

-- | Reduces a pair of integers, by dividing them by thier gcd, 
-- until their gcd is 1.
reduce :: (Int,Int) -> (Int,Int)
reduce (x,y) = if g == 1 then (x,y) else (floor ((fromIntegral x)/(fromIntegral g)),floor ((fromIntegral y)/(fromIntegral g)))
               where g = gcd x y







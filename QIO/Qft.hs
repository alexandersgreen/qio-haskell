
-- | This module provides an implementation of the Quantum Fourier Transform
-- in QIO.
module QIO.Qft where

import Data.Monoid as Monoid
import QIO.QioSyn
import QIO.Qio
import QIO.Qdata

-- | Defines the unitary the represents appliying a Quantum Fourier Transform
-- to the given quantum register.
qft :: [Qbit] -> U
qft qs = condQ qs (\bs -> qftAcu qs bs [])

-- | The definition of the QFT unitary makes use of an accumulator, to repeatedly
-- apply smaller QFTs to the tail of the given quantum register.
qftAcu :: [Qbit] -> [Bool] -> [Bool] -> U
qftAcu [] [] _ = mempty
qftAcu (q:qs) (b:bs) cs = qftBase cs q `mappend` qftAcu qs bs (b:cs)

-- | The \"base\" step involved in a QFT is a series of controlled rotations.
qftBase :: [Bool] -> Qbit -> U
qftBase bs q =  f' bs q 2
    where f' [] q _ = uhad q
          f' (b:bs) q x = if b then (rotK x q) `mappend` f' bs q (x+1) 
                  else f' bs q (x+1)

--need to change this into a conQRec???
-- e.g. qft [Qbit 0]
-- = condQ [Qbit 0] (\(b:bs) -> uhad 0 `mappend` mempty)
-- but gives  cond 0 (\x -> if x then uhad 0 else uhad 0) which is forbidden

-- | The rotation used in the QFT is a phase rotation, parameterised by the 
-- angle 1/(2^\k\)
rotK :: Int -> Qbit -> U
rotK k q = uphase q (1.0/(2.0^k))

-- | A test of the QFT unitary, over a quantum integer initialised to \n\.
tryQft :: Int -> QIO Int
tryQft n = do 
  QInt qs <- mkQ n
  applyU(qft qs)
  x <- measQ (QInt qs)
  return x

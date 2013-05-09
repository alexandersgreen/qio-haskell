
-- | This module contains QIO unitaries that represent various Arithmetic 
-- functions. These are exactly the Arithmetic functions required to implement 
-- Shor's algorithm.
module QIO.QArith where

import Data.Monoid as Monoid
import QIO.QioSyn
import QIO.Qdata
import QIO.QioClass
import QIO.Qio
import QIO.QExamples

-- | A swap operation can be applied to two QInts, by mapping qubit swap operations
-- over the underlying qubits that make up a QInt.
swapQInt :: QInt -> QInt -> U
swapQInt (QInt xs) (QInt ys) = swapQInt' xs ys
			       where swapQInt' [] [] = mempty
				     swapQInt' (x:xs) (y:ys) = (swap x y) `mappend` swapQInt' xs ys

-- | ifElseQ defines a quantum If statement, whereby depending on the state of
-- the given (control) qubit, one of two unitaries are applied.
ifElseQ :: Qbit -> U -> U -> U
ifElseQ qa t f = cond qa (\ qa -> if qa then t else f)

-- | ifQ defines a special case of ifElseQ, where the Else part of the computation
-- is simply the identity.
ifQ :: Qbit -> U -> U
ifQ qa t = ifElseQ qa t mempty

-- | A controlled-not operations, that applies a Not to the second qubit, 
-- depending on the state of the first qubit.
cnot :: Qbit -> Qbit -> U
cnot qa qb = ifQ qa (unot qb)

-- | A three-qubit adder.
addBit :: Qbit -> Qbit -> Qbit -> U
addBit qc qa qb = 
    cnot qa qb `mappend`
    cnot qc qb

-- | Calculates the carry (qu)bit. 
carry :: Qbit -> Qbit -> Qbit -> Qbit -> U
carry qci qa qb qcsi = 
    cond qci (\ ci ->
              cond qa (\ a ->
                       cond qb (\ b ->
                                if ci && a || ci && b || a && b
                                then unot qcsi
                                else mempty)))

-- | uses the 'addBit' and 'carry' unitaries to add the contents of two quantum
-- registers, setting an overflow bit if necessary. This unitary makes use of a
-- letU construct to introduce ancilla bits as necessary.
addBits :: [Qbit] -> [Qbit] -> Qbit -> U
addBits qas qbs qc' = 
    letU False (addBits' qas qbs)
    where addBits' [] [] qc = ifQ qc (unot qc')
          addBits' (qa:qas) (qb:qbs) qc =
              letU False (\ qc' -> carry qc qa qb qc' `mappend`
                                   addBits' qas qbs qc'`mappend`
                                   urev (carry qc qa qb qc')) `mappend`
              addBit qc qa qb

-- | An alternate implementation of 'addBits' that is explicitly given
-- a register of ancilla qubits for all the intermediate 'carry' results.
addBits' :: [Qbit] -> [Qbit] -> [Qbit] -> Qbit -> U
addBits' [] [] [] qc = mempty
addBits' (qa:qas) (qb:qbs) (qc':qcs') qc =
    (carry qc qa qb qc' `mappend`
     addBits' qas qbs qcs' qc'`mappend`
     urev (carry qc qa qb qc')) `mappend`
    addBit qc qa qb

-- | Defines the QIO unitary that adds two QInts, with an overflow qubit 
adder :: QInt -> QInt -> Qbit -> U
adder (QInt qas) (QInt qbs) qc = addBits qas qbs qc 

-- | A small function to test the adder unitary
tadder :: (Int,(Int,Bool)) -> QIO (Int,(Int,Bool))
tadder xyc = do q @ (qx,(qy,qc)) <- mkQ xyc
                applyU (adder qx qy qc)
                xyc <- measQ q
                return xyc

-- | A small function to test applying the adder unitary in reverse, ie.
-- this defines subtraction.
tRadder :: (Int,(Int,Bool)) -> QIO (Int,(Int,Bool))
tRadder xyc = do q @ (qx,(qy,qc)) <- mkQ xyc
                 applyU (urev (adder qx qy qc))
                 xyc <- measQ q
                 return xyc

-- | A small function to test applying the adder unitary, and then applying
-- the reverse of the adder unitary, which should give the identity function.
tBiAdder :: (Int,(Int,Bool)) -> QIO (Int,(Int,Bool))
tBiAdder xyc = do q @ (qx,(qy,qc)) <- mkQ xyc
		  applyU (adder qx qy qc)
		  applyU (urev (adder qx qy qc))
                  xyc <- measQ q
	          return xyc

-- | This unitary is for modular addition, and is done modulo some fixed
-- classical modulus, given as the first Int argument.
adderMod :: Int -> QInt -> QInt -> U
adderMod n qa qb =
    letU n (\ qn ->
       letU False (\ qz ->
          letU False (\ qc -> 
             adder qa qb qc
             `mappend` -- b = a+b, c=False
             urev (adder qn qb qc)
             `mappend` -- b = a+b-N, c = a+b < N
             cond qc (\ c -> if c then unot qz else mempty)
             `mappend` -- z = c = a+b < N
             cond qz (\ z -> if z then adder qn qb qc else mempty)
             `mappend` -- b = a+b mod N, c = False, z = a+b < N
             urev (adder qa qb qc)
             `mappend` -- if a+b < N then a=a,b=b,c=False 
                       -- else a=a,b=a+b mod N - b,c=True
                       -- z = not c
             cond qc (\ c -> if c then mempty else unot qz)
             `mappend` -- z = False
             adder qa qb qc))) -- b = a+b mod N, c=False, z=False

-- | A small function to test the modular addition unitary.
tadderMod :: Int -> (Int,Int) -> QIO (Int,Int)
tadderMod n ab = do q @ (qa,qb) <- mkQ ab
                    applyU (adderMod n qa qb)
                    ab <- measQ q
                    return ab

-- | This unitary defines modular multiplication, whereby the integer 'n' is the
-- the modulus, and the integer 'a' is the scalar by which to multiply the quantum
-- integer 'x'. The result is added to the quantum integer 'y', ie. if 'y' is in 
-- state 0 before the operation, then it is left in the sate a*x mod n.
multMod :: Int -> Int -> QInt -> QInt -> U
multMod n a (QInt x) y = multMod' n a x y 1
                         where multMod' _ _ [] _ _ = mempty
			       multMod' n a (x:xs) y p = cond x (\x -> (if x then (letU ((p*a) `mod` n) (\ qa -> (adderMod n qa y)) `mappend` (multMod' n a xs y (p*2)))
                                                                             else multMod' n a xs y (p*2)))
		               
-- | A small function for testing the modular multiplication unitary. This function
-- initialises 'y' as zero, so the output is as expected.
tmultMod :: Int -> Int -> Int -> QIO (Int,Int)
tmultMod n a x = do y <- mkQ 0
                    x' <- mkQ x
                    applyU(multMod n a x' y)
                    qy <- measQ y
                    qx <- measQ x'
                    return (qx,qy)

-- | A unitary that adds a single qubit control to modular multiplication
condMultMod :: Qbit -> Int -> Int -> QInt -> QInt -> U
condMultMod q n a x y = ifQ q (multMod n a x y)

------------------------------------------------------------------------------

-- | A classical Haskell function that returns the smalles positive inverse
-- of \a\ `mod \n\ (if one exists). That is, the smallest positive integer
-- \x\, such that \x\*\a\ `mod` \n\ equals 1. 
inverseMod :: Int -> Int -> Int
inverseMod n a = case imods of
  [] -> error ("inverseMod: no inverse of "++(show a)++" mod "++(show n)++ " found")
  (x:_) -> x
 where
  imods = [x | x <- [1..n], ((x*a) `mod` n) == 1]

-------------------------------------------------------------------------------

-- | The unitary that represents modular exponentiation is constructed in terms
-- of multiple \"steps\". This function defines those steps.
modExpStep :: Qbit -> Int -> Int -> QInt -> Int -> U
modExpStep qc n a o p = letU 0 (\z ->                (condMultMod qc n p'                o z) 
			             `mappend` (ifQ qc (swapQInt o z))
                                     `mappend` (urev (condMultMod qc n (inverseMod n p') o z)))
				  where p' | (a^(2^p)) == 0 = error "modExpStep: arguments too large"
					   | otherwise = (a^(2^p)) `mod` n

-- | A QIO computation that forms a test of the 'modExpStep' unitary
modExpStept :: Int -> Int -> Int -> Int -> QIO Int
modExpStept i n a p = do q <- mkQ True
		         one <- mkQ i
		         applyU (modExpStep q n a one p)	      
		         r <- measQ one	     
		         return r

-- | This function defines a unitary that implements modular exponentiation, as
-- required in Shor's algorithm. Given classical arguments \n\ and \a\, a quantum
-- register containg \x\, and a quantum register \o\ in state 1, this unitary will 
-- leave the quantum register \o\ in the state \a\^\x\ mod \n\.
modExp :: Int -> Int -> QInt -> QInt -> U
modExp n a (QInt x) o = modExp' n a x o 0
                        where modExp' _ _ [] _ _ = mempty
			      modExp' n a (x:xs) o p =           modExpStep x n a o p 
						      `mappend` (modExp' n a xs o (p+1))

-- | A QIO computation that forms a test of the modular exponentiation unitary.
modExpt :: Int -> (Int,Int) -> QIO Int
modExpt n (a,x) = do qx <- mkQ x
		     one <- mkQ 1
                     applyU (modExp n a qx one)
                     r <- measQ one
		     return r



		       







                     






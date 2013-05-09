
-- | This module contains some simple examples of quantum computations written
-- using the Quantum IO Monad.
module QIO.QExamples where

import Data.Monoid as Monoid
import QIO.QioSyn
import QIO.Qdata
import QIO.QioClass
import QIO.Qio

-- | Initialise a qubit in the |0> state
q0 :: QIO Qbit
q0 = mkQ False

-- | Initialise a qubit in the |1> state
q1 :: QIO Qbit
q1  =  mkQ True

-- | Initialise a qubit in the |+> state. This is done by applying a Hadamard
-- gate to the |0> state.    
qPlus :: QIO Qbit
qPlus  =  do  qa <- q0
              applyU (uhad qa)
              return qa

-- | Initialise a qubit in the |-> state. This is done by applying a Hadamard
-- gate to the |1> state.
qMinus :: QIO Qbit
qMinus  =  do  qa <- q1
               applyU (uhad qa)
               return qa

-- | Create a random Boolean value, by measuring the state |+> 
randBit :: QIO Bool
randBit  =  do  qa <- qPlus
                x <- measQbit qa
                return x

-- | This function can be used to "share" the state of one qubit, with another
-- newly initialised qubit. This is not the same as "cloning", as the two qubits
-- will be in an entangled state. "sharing" is achieved by simply initialising
-- a new qubit in state |0>, and then applying a controlled-not to that qubit, 
-- depending on the state of the given qubit.
share :: Qbit -> QIO Qbit
share  qa  =  do  qb <- q0
                  applyU (cond qa (\a -> if a then (unot qb)
                                              else (mempty)  )  )
                  return qb

-- | A Bell state can be created by sharing the |+> state
bell :: QIO (Qbit, Qbit)
bell = do  qa <- qPlus
           qb <- share qa
           return (qa,qb)

-- | This function creates a Bell state, and then measures it. The resulting pair
-- of Booleans will always be in the same state as one another.
test_bell :: QIO (Bool,Bool)
test_bell  =  do  qb <- bell
                  b <- measQ qb
                  return b

-- | This function initiaslised a qubit in the state corresponding to the given
-- Boolean value. The Hadamard transform (which is self-inverse) is applied to
-- the qubit twice, and then the qubit is measured. This should correspond to
-- the identity function on the given Boolean value.
hadTwice :: Bool -> QIO Bool
hadTwice x = do q <- mkQ x
		applyU (uhad q `mappend` uhad q)
		b <- measQ q
		return b

-- | A different implementation of 'hadTwice' where QIO is used to apply two
-- unitaries, each of which is a single Hadamard gate, as opposed to a single
-- unitary, which is two Hadamard gates. 
hadTwice' :: Bool -> QIO Bool
hadTwice' x = do q <- mkQ x
	 	 applyU (uhad q)
                 applyU (uhad q)
		 b <- measQ q
		 return b

----------------------------------------------
---- Teleportation ---------------------------
----------------------------------------------

-- | The operations that Alice must perform in the classic quantum teleportation
-- example.
alice :: Qbit -> Qbit -> QIO (Bool,Bool)
alice aq eq  =  do  applyU (cond aq (\a -> if a then (unot eq)
                                              else (mempty)  )  )
                    applyU (uhad aq)
                    cd <- measQ (aq,eq)
                    return cd

-- | A definition of the Pauli-Z gate.
uZZ :: Qbit -> U
uZZ qb = (uphase qb pi)

-- | The unitary operations that Bob must perform in the classic quantum
-- teleportation example.
bobsU :: (Bool,Bool) -> Qbit -> U
bobsU  (False,False)  eq  =  mempty
bobsU  (False,True)  eq  =  (unot eq)
bobsU  (True,False)  eq  =  (uZZ eq)
bobsU  (True,True)  eq  =	     ((unot eq) 
       		    	   `mappend` (uZZ eq))

-- | The overall operations that Bob must perform in the classic quantum
-- teleportation example
bob :: Qbit -> (Bool,Bool) -> QIO Qbit
bob eq cd  =  do  applyU (bobsU cd eq)
                  return eq

-- | The overall QIO computation that teleports the state of single qubit
teleportation :: Qbit -> QIO Qbit
teleportation iq  =  do  (eq1,eq2) <- bell
                         cd <- alice iq eq1
                         tq <- bob eq2 cd
                         return tq

-- | A small test function of quantum teleportation, which teleports a
-- bell state, and then measures it.
test_teleport :: QIO (Bool,Bool)
test_teleport = do (q1,q2) <- bell
                   tq2 <- teleportation q2
		   result <- measQ (q1,tq2)
		   return result

-- | teleports a qubit in the state |1>
teleport_true' :: QIO Qbit
teleport_true' = do q <- q1
                    tq <- teleportation q
                    return tq

-- | teleports a qubit in the state |1>, and then measures it
teleport_true :: QIO Bool
teleport_true = do q <- teleport_true'
                   result <- measQ q
                   return result

-- | teleports a qubit in the state |+>
teleport_random' :: QIO Qbit
teleport_random' = do q <- qPlus
                      tq <- teleportation q
                      return tq

-- | teleports a qubit in the state |+>, and then measures it.
teleport_random :: QIO Bool
teleport_random = do q <- teleport_random'
                     result <- measQ q
                     return result

-----------------------------------------------
----- Deutsch's Algorithm ---------------------
-----------------------------------------------

-- | The implementation of Deutsch's algorithm requires a unitary to represent
-- the "oracle" function.
u :: (Bool -> Bool) -> Qbit -> Qbit -> U
u f x y = cond x (\ b -> if f b then unot y else mempty)

-- | Deutsch's algorithm takes an "oracle" function, and returns a Boolean
-- that states whether the given function is balanced, or consant.
deutsch :: (Bool -> Bool) -> QIO Bool
deutsch f = do
              x <- qPlus
              y <- qMinus
              applyU (u f x y)
              applyU (uhad x)
              measQ x

-----------------------------------------------

-- | A test QIO computation that is infinite in one measurement path. This is
-- a problem if we try to calculate the probability distribution of possible
-- results, as the infinite path will be followed.
problem :: QIO Bool
problem = do q <- qPlus
             x <- measQ q
             if x then return x else problem
-- can be run returning True, but cannot be simulated!             

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | This module contains the definition of a Type Class that represents a Heap.
-- In the context of QIO, a Heap is the type used to represent a classical 
-- basis state. An instance of a Heap is also defined, that makes use of a Map.
module QIO.Heap where

import qualified Data.Map as Map
import Data.Maybe as Maybe
import QIO.QioSyn

-- | The Heap Type Class
class Eq h => Heap h where
      -- | define an 'initial' (i.e. empty) Heap
      initial :: h
      -- | 'update' the value of a Qubit within the Heap to the given Boolen value
      update :: h -> Qbit -> Bool -> h
      -- | Lookup the value of the given Qubit in the Heap (if it exists)
      (?) :: h -> Qbit -> Maybe Bool
      -- | remove the given Qubit from the Heap
      forget :: h -> Qbit -> h
      -- | Swap the values associated with two Qubits within the Heap
      hswap :: h -> Qbit -> Qbit -> h
      hswap h x y = update (update h y (fromJust (h ? x))) x (fromJust (h ? y)) 

-- | HeapMap is simply a type synonym for a Map from Qubits to Boolean values
type HeapMap = Map.Map Qbit Bool

-- | A HeapMap is an instance of the Heap type class, where the Heap functions
-- can make use of the underlying Map functions.
instance Heap HeapMap where
       initial = Map.empty
       update h q b = Map.insert q b h
       h ? q = Map.lookup q h
       forget h q = Map.delete q h


    



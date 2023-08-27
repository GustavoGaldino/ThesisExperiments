module DataStructures.LazyPairingHeap (
    envSetup,
    addNDistinctFrom,
    addAll,
    remove,
    clear,
    contains,
    containsAll,
    retainAll,
    removeAll,
    iterator,
    toList
) where

import qualified Data.Edison.Coll.LazyPairingHeap as LPH
import qualified Commons as C

-- Add n distinct elements starting from m to the heap.
addNDistinctFrom :: LPH.Heap Int -> Int -> Int -> LPH.Heap Int
addNDistinctFrom h 0 _ = h
addNDistinctFrom h n m =
    let
        elemToAdd = m + n - 1
    in
        addNDistinctFrom (LPH.insert elemToAdd h) (n - 1) m

envSetup :: Int -> Maybe Int -> C.Experimenter
envSetup baseElems Nothing = C.LazyPairingHeap (addNDistinctFrom LPH.empty baseElems 0) LPH.empty
envSetup baseElems (Just opElems) = C.LazyPairingHeap (addNDistinctFrom LPH.empty baseElems 0) (addNDistinctFrom LPH.empty opElems 0)

addAll :: LPH.Heap Int -> LPH.Heap Int -> LPH.Heap Int
addAll = LPH.union

remove :: LPH.Heap Int -> LPH.Heap Int
remove h = if LPH.null h then h else LPH.deleteMin h
        
clear :: LPH.Heap Int -> LPH.Heap Int
clear h 
    | LPH.null h  = h
    | otherwise = clear (remove h)

contains :: LPH.Heap Int -> Int -> Bool
contains h e = LPH.member e h

containsAll :: LPH.Heap Int -> LPH.Heap Int -> Bool
containsAll h t = all (`LPH.member` h) (toList t)

iterator :: LPH.Heap Int -> LPH.Heap Int
iterator = id

removeAll :: LPH.Heap Int -> LPH.Heap Int -> LPH.Heap Int
removeAll h t
    | LPH.null t = h
    | otherwise = removeAll (LPH.delete (LPH.minElem t) h) (LPH.deleteMin t)

retainAll :: LPH.Heap Int -> LPH.Heap Int -> LPH.Heap Int
retainAll h t = foldl (\acc x -> if contains t x then LPH.insert x acc else acc) LPH.empty (toList h)

toList :: LPH.Heap Int -> [Int]
toList h
    | LPH.null h  = []
    | otherwise = LPH.minElem h : toList (LPH.deleteMin h)

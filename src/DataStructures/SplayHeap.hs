module DataStructures.SplayHeap (
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

import qualified Data.Edison.Coll.SplayHeap as SH
import qualified Commons as C

-- Add n distinct elements starting from m to the heap.
addNDistinctFrom :: SH.Heap Int -> Int -> Int -> SH.Heap Int
addNDistinctFrom h 0 _ = h
addNDistinctFrom h n m =
    let
        elemToAdd = m + n - 1
    in
        addNDistinctFrom (SH.insert elemToAdd h) (n - 1) m

envSetup :: Int -> Maybe Int -> C.Experimenter
envSetup baseElems Nothing = C.SplayHeap (addNDistinctFrom SH.empty baseElems 0) SH.empty
envSetup baseElems (Just opElems) = C.SplayHeap (addNDistinctFrom SH.empty baseElems 0) (addNDistinctFrom SH.empty opElems 0)

addAll :: SH.Heap Int -> SH.Heap Int -> SH.Heap Int
addAll = SH.union

remove :: SH.Heap Int -> SH.Heap Int
remove h = if SH.null h then h else SH.deleteMin h
        
clear :: SH.Heap Int -> SH.Heap Int
clear h 
    | SH.null h  = h
    | otherwise = clear (remove h)

contains :: SH.Heap Int -> Int -> Bool
contains h e = SH.member e h

containsAll :: SH.Heap Int -> SH.Heap Int -> Bool
containsAll h t = all (`SH.member` h) (toList t)

iterator :: SH.Heap Int -> SH.Heap Int
iterator = id

removeAll :: SH.Heap Int -> SH.Heap Int -> SH.Heap Int
removeAll h t
    | SH.null t = h
    | otherwise = removeAll (SH.delete (SH.minElem t) h) (SH.deleteMin t)

retainAll :: SH.Heap Int -> SH.Heap Int -> SH.Heap Int
retainAll h t = foldl (\acc x -> if contains t x then SH.insert x acc else acc) SH.empty (toList h)

toList :: SH.Heap Int -> [Int]
toList h
    | SH.null h  = []
    | otherwise = SH.minElem h : toList (SH.deleteMin h)

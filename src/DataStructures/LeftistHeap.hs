module DataStructures.LeftistHeap (
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

import qualified Data.Edison.Coll.LeftistHeap as LH
import qualified Commons as C

-- Add n distinct elements starting from m to the heap.
addNDistinctFrom :: LH.Heap Int -> Int -> Int -> LH.Heap Int
addNDistinctFrom h 0 _ = h
addNDistinctFrom h n m =
    let
        elemToAdd = m + n - 1
    in
        addNDistinctFrom (LH.insert elemToAdd h) (n - 1) m

envSetup :: Int -> Maybe Int -> C.Experimenter
envSetup baseElems Nothing = C.LeftistHeap (addNDistinctFrom LH.empty baseElems 0) LH.empty
envSetup baseElems (Just opElems) = C.LeftistHeap (addNDistinctFrom LH.empty baseElems 0) (addNDistinctFrom LH.empty opElems 0)

addAll :: LH.Heap Int -> LH.Heap Int -> LH.Heap Int
addAll = LH.union

remove :: LH.Heap Int -> LH.Heap Int
remove h = if LH.null h then h else LH.deleteMin h
        
clear :: LH.Heap Int -> LH.Heap Int
clear h 
    | LH.null h  = h
    | otherwise = clear (remove h)

contains :: LH.Heap Int -> Int -> Bool
contains h e = LH.member e h

containsAll :: LH.Heap Int -> LH.Heap Int -> Bool
containsAll h t = all (`LH.member` h) (toList t)

iterator :: LH.Heap Int -> LH.Heap Int
iterator = id

removeAll :: LH.Heap Int -> LH.Heap Int -> LH.Heap Int
removeAll h t
    | LH.null t = h
    | otherwise = removeAll (LH.delete (LH.minElem t) h) (LH.deleteMin t)

retainAll :: LH.Heap Int -> LH.Heap Int -> LH.Heap Int
retainAll h t = foldl (\acc x -> if contains t x then LH.insert x acc else acc) LH.empty (toList h)

toList :: LH.Heap Int -> [Int]
toList h
    | LH.null h  = []
    | otherwise = LH.minElem h : toList (LH.deleteMin h)

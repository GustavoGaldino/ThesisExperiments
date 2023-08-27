module DataStructures.SkewHeap (
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

import qualified Data.Edison.Coll.SkewHeap as SKH
import qualified Commons as C

-- Add n distinct elements starting from m to the heap.
addNDistinctFrom :: SKH.Heap Int -> Int -> Int -> SKH.Heap Int
addNDistinctFrom h 0 _ = h
addNDistinctFrom h n m =
    let
        elemToAdd = m + n - 1
    in
        addNDistinctFrom (SKH.insert elemToAdd h) (n - 1) m

envSetup :: Int -> Maybe Int -> C.Experimenter
envSetup baseElems Nothing = C.SkewHeap (addNDistinctFrom SKH.empty baseElems 0) SKH.empty
envSetup baseElems (Just opElems) = C.SkewHeap (addNDistinctFrom SKH.empty baseElems 0) (addNDistinctFrom SKH.empty opElems 0)

addAll :: SKH.Heap Int -> SKH.Heap Int -> SKH.Heap Int
addAll = SKH.union

remove :: SKH.Heap Int -> SKH.Heap Int
remove h = if SKH.null h then h else SKH.deleteMin h
        
clear :: SKH.Heap Int -> SKH.Heap Int
clear h 
    | SKH.null h  = h
    | otherwise = clear (remove h)

contains :: SKH.Heap Int -> Int -> Bool
contains h e = SKH.member e h

containsAll :: SKH.Heap Int -> SKH.Heap Int -> Bool
containsAll h t = all (`SKH.member` h) (toList t)

iterator :: SKH.Heap Int -> SKH.Heap Int
iterator = id

removeAll :: SKH.Heap Int -> SKH.Heap Int -> SKH.Heap Int
removeAll h t
    | SKH.null t = h
    | otherwise = removeAll (SKH.delete (SKH.minElem t) h) (SKH.deleteMin t)

retainAll :: SKH.Heap Int -> SKH.Heap Int -> SKH.Heap Int
retainAll h t = foldl (\acc x -> if contains t x then SKH.insert x acc else acc) SKH.empty (toList h)

toList :: SKH.Heap Int -> [Int]
toList h
    | SKH.null h  = []
    | otherwise = SKH.minElem h : toList (SKH.deleteMin h)

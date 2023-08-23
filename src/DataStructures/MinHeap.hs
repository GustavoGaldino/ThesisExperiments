module DataStructures.MinHeap (
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

import qualified Data.Edison.Coll.MinHeap as M
import qualified Data.Edison.Coll.SplayHeap as S
import qualified Commons as C

type MyMinHeap a = M.Min (S.Heap a) a

-- Add n distinct elements starting from m to the heap.
addNDistinctFrom :: MyMinHeap Int -> Int -> Int -> MyMinHeap Int
addNDistinctFrom h 0 _ = h
addNDistinctFrom h n m =
    let
        elemToAdd = m + n - 1
    in
        addNDistinctFrom (M.insert elemToAdd h) (n - 1) m

envSetup :: Int -> Maybe Int -> C.Experimenter
envSetup baseElems Nothing = C.MinHeap (addNDistinctFrom M.empty baseElems 0) M.empty
envSetup baseElems (Just opElems) = C.MinHeap (addNDistinctFrom M.empty baseElems 0) (addNDistinctFrom M.empty opElems 0)

addAll :: MyMinHeap Int -> MyMinHeap Int -> MyMinHeap Int
addAll = M.union

remove :: MyMinHeap Int -> MyMinHeap Int
remove h = if M.null h then h else M.deleteMin h
        
clear :: MyMinHeap Int -> MyMinHeap Int
clear h = M.empty

contains :: MyMinHeap Int -> Int -> Bool
contains h e = M.member e h

containsAll :: MyMinHeap Int -> MyMinHeap Int -> Bool
containsAll h t = all (`M.member` h) (toList t)

iterator :: MyMinHeap Int -> MyMinHeap Int
iterator = id

removeAll :: MyMinHeap Int-> MyMinHeap Int -> MyMinHeap Int
removeAll h t = h

retainAll :: MyMinHeap Int -> MyMinHeap Int -> MyMinHeap Int
retainAll h t = t

toList :: MyMinHeap Int -> [Int]
toList _ = []


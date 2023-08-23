module DataStructures.AssocList (
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

import qualified Data.Edison.Assoc.AssocList as A
import qualified Commons as C

-- It's easier to use the 'insert' function for AssocList, 
-- which requires both a key and a value. We'll assume key = value.
addNDistinctFrom :: A.FM Int Int -> Int -> Int -> A.FM Int Int
addNDistinctFrom s 0 _ = s
addNDistinctFrom s n m =
    let
        elemToAdd =  m + n - 1
        nextNumber = n - 1
    in
        addNDistinctFrom (A.insert elemToAdd elemToAdd s) nextNumber m

envSetup :: Int -> Maybe Int -> C.Experimenter
envSetup baseElems Nothing = C.AssocList (addNDistinctFrom A.empty baseElems 0) A.empty
envSetup baseElems (Just opElems) = C.AssocList (addNDistinctFrom A.empty baseElems 0) (addNDistinctFrom A.empty opElems 0)

addAll :: A.FM Int Int -> A.FM Int Int -> A.FM Int Int
addAll = A.union

remove :: A.FM Int Int -> A.FM Int Int
remove s = if A.null s then s else A.deleteMin s
        
clear :: A.FM Int Int -> A.FM Int Int
clear = const A.empty

myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll pred (x:xs)
    | pred x    = myAll pred xs
    | otherwise = False


contains :: Int -> A.FM Int Int -> Bool
contains e s = A.member e s

containsAll :: A.FM Int Int -> A.FM Int Int -> Bool
containsAll s t = myAll (`contains` s) (A.keys t)


iterator :: A.FM Int Int -> A.FM Int Int
iterator = A.map id

removeAll :: A.FM Int Int -> A.FM Int Int -> A.FM Int Int
removeAll s t = A.difference s t

retainAll :: A.FM Int Int -> A.FM Int Int -> A.FM Int Int
retainAll s t = A.filterWithKey (\k _ -> A.member k t) s

toList :: A.FM Int Int -> [(Int, Int)]
toList fm = A.foldWithKey (\k v acc -> (k, v):acc) [] fm
module DataStructures.StandardMap (
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

import qualified Data.Edison.Assoc.StandardMap as SM
import qualified Commons as C

addNDistinctFrom :: SM.FM Int Int -> Int -> Int -> SM.FM Int Int
addNDistinctFrom s 0 _ = s
addNDistinctFrom s n m =
    let
        elemToAdd =  m + n - 1
        nextNumber = n - 1
    in
        addNDistinctFrom (SM.insert elemToAdd elemToAdd s) nextNumber m

envSetup :: Int -> Maybe Int -> C.Experimenter
envSetup baseElems Nothing = C.StandardMap (addNDistinctFrom SM.empty baseElems 0) SM.empty
envSetup baseElems (Just opElems) = C.StandardMap (addNDistinctFrom SM.empty baseElems 0) (addNDistinctFrom SM.empty opElems 0)

addAll :: SM.FM Int Int -> SM.FM Int Int -> SM.FM Int Int
addAll = SM.union

remove :: SM.FM Int Int -> SM.FM Int Int
remove s = if SM.null s then s else SM.deleteMin s
        
clear :: SM.FM Int Int -> SM.FM Int Int
clear = const SM.empty

myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll pred (x:xs)
    | pred x    = myAll pred xs
    | otherwise = False


contains :: Int -> SM.FM Int Int -> Bool
contains e s = SM.member e s

containsAll :: SM.FM Int Int -> SM.FM Int Int -> Bool
containsAll s t = myAll (`contains` s) (SM.keys t)


iterator :: SM.FM Int Int -> SM.FM Int Int
iterator = SM.map id

removeAll :: SM.FM Int Int -> SM.FM Int Int -> SM.FM Int Int
removeAll s t = SM.difference s t

retainAll :: SM.FM Int Int -> SM.FM Int Int -> SM.FM Int Int
retainAll s t = SM.filterWithKey (\k _ -> SM.member k t) s

toList :: SM.FM Int Int -> [(Int, Int)]
toList fm = SM.foldWithKey (\k v acc -> (k, v):acc) [] fm
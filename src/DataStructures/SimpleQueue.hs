module DataStructures.SimpleQueue (
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

import qualified Data.Edison.Seq.SimpleQueue as S
import qualified Commons as C

addNDistinctFrom :: S.Seq Int -> Int -> Int -> S.Seq Int
addNDistinctFrom s 0 _ = s
addNDistinctFrom s n m =
    let
        elemToAdd =  m + n - 1
        nextNumber = n - 1
        cons = if even n then S.rcons else S.lcons
    in
        addNDistinctFrom (elemToAdd `cons` s) nextNumber m

envSetup :: Int -> Maybe Int -> C.Experimenter
envSetup baseElems Nothing = C.SimpleQueue (addNDistinctFrom S.empty baseElems 0) S.empty
envSetup baseElems (Just opElems) = C.SimpleQueue (addNDistinctFrom S.empty baseElems 0) (addNDistinctFrom S.empty opElems 0)

addAll :: S.Seq Int -> S.Seq Int -> S.Seq Int
addAll = S.append

remove :: S.Seq Int -> S.Seq Int
remove s = if S.null s then s else S.ltail s
        
clear :: S.Seq Int -> S.Seq Int
clear s = if S.null s then s else clear $ remove s

contains :: S.Seq Int -> Int -> Bool
contains s e = not . S.null . S.filter ( (==) e ) $ s

containsAll :: S.Seq Int -> S.Seq Int -> Bool
containsAll s t = S.foldr (&&) True . S.map ( s `contains` ) $ t

iterator :: S.Seq Int -> S.Seq Int
iterator = S.map ( id )

removeAll :: S.Seq Int -> S.Seq Int -> S.Seq Int
removeAll s t = S.filter ( not . ( t `contains` ) ) s

retainAll :: S.Seq Int -> S.Seq Int -> S.Seq Int
retainAll s t = S.filter (t `contains`) s

toList :: S.Seq Int -> [Int]
toList = S.toList


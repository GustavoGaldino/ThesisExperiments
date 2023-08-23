module DataStructures.StandardSet (
    addNDistinctFromSet,
    envSetupSet,
    addAllSet,
    removeSet,
    clearSet,
    containsSet,
    containsAllSet,
    iteratorSet,
    removeAllSet,
    retainAllSet,
    toListSet
) where

import qualified Data.Edison.Coll.StandardSet as SS
import qualified Commons as C

addNDistinctFromSet :: SS.Set Int -> Int -> Int -> SS.Set Int
addNDistinctFromSet s 0 _ = s
addNDistinctFromSet s n m = 
    let 
        elemToAdd = m + n - 1 
    in 
        addNDistinctFromSet (SS.insert elemToAdd s) (n-1) m

envSetupSet :: Int -> Maybe Int -> C.Experimenter
envSetupSet baseElems Nothing = 
    C.StandardSet (addNDistinctFromSet SS.empty baseElems 0) SS.empty
envSetupSet baseElems (Just opElems) = 
    C.StandardSet (addNDistinctFromSet SS.empty baseElems 0) (addNDistinctFromSet SS.empty opElems 0)


addAllSet :: SS.Set Int -> SS.Set Int -> SS.Set Int
addAllSet = SS.union

removeSet :: SS.Set Int -> SS.Set Int
removeSet s =
    case SS.minView s of
        Just (_, rest) -> rest
        Nothing        -> s

clearSet :: SS.Set Int -> SS.Set Int
clearSet _ = SS.empty

containsSet :: SS.Set Int -> Int -> Bool
containsSet s e = SS.member e s

containsAllSet :: SS.Set Int -> SS.Set Int -> Bool
containsAllSet s t = SS.fold (\e acc -> acc && (e `SS.member` s)) True t

iteratorSet :: SS.Set Int -> SS.Set Int
iteratorSet s = SS.fold (\e acc -> SS.insert e acc) SS.empty s

removeAllSet :: SS.Set Int -> SS.Set Int -> SS.Set Int
removeAllSet s t = s `SS.difference` t

retainAllSet :: SS.Set Int -> SS.Set Int -> SS.Set Int
retainAllSet s t = s `SS.intersection` t

toListSet :: SS.Set Int -> [Int]
toListSet s = SS.fold (\e acc -> e:acc) [] s
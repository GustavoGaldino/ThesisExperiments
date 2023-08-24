module DataStructures.UnbalancedSet (
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

import qualified Data.Edison.Coll.UnbalancedSet as US
import qualified Commons as C

addNDistinctFromSet :: US.Set Int -> Int -> Int -> US.Set Int
addNDistinctFromSet s 0 _ = s
addNDistinctFromSet s n m = 
    let 
        elemToAdd = m + n - 1 
    in 
        addNDistinctFromSet (US.insert elemToAdd s) (n-1) m

envSetupSet :: Int -> Maybe Int -> C.Experimenter
envSetupSet baseElems Nothing = 
    C.UnbalancedSet (addNDistinctFromSet US.empty baseElems 0) US.empty
envSetupSet baseElems (Just opElems) = 
    C.UnbalancedSet (addNDistinctFromSet US.empty baseElems 0) (addNDistinctFromSet US.empty opElems 0)


addAllSet :: US.Set Int -> US.Set Int -> US.Set Int
addAllSet = US.union

removeSet :: US.Set Int -> US.Set Int
removeSet s =
    case US.minView s of
        Just (_, rest) -> rest
        Nothing        -> s

clearSet :: US.Set Int -> US.Set Int
clearSet _ = US.empty

containsSet :: US.Set Int -> Int -> Bool
containsSet s e = US.member e s

containsAllSet :: US.Set Int -> US.Set Int -> Bool
containsAllSet s t = US.fold (\e acc -> acc && (e `US.member` s)) True t

iteratorSet :: US.Set Int -> US.Set Int
iteratorSet s = US.fold (\e acc -> US.insert e acc) US.empty s

removeAllSet :: US.Set Int -> US.Set Int -> US.Set Int
removeAllSet s t = s `US.difference` t

retainAllSet :: US.Set Int -> US.Set Int -> US.Set Int
retainAllSet s t = s `US.intersection` t

toListSet :: US.Set Int -> [Int]
toListSet s = US.fold (\e acc -> e:acc) [] s
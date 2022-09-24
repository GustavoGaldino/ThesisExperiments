module DataStructures.SimpleSeq (
    envSetup,
    addExperiment,
    addAllExperiment,
    clearExperiment,
    containsExperiment,
    containsAllExperiment,
    iteratorExperiment,
    removeExperiment,
    removeAllExperiment,
    retainAllExperiment,
    toListExperiment
) where

import Control.DeepSeq
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
envSetup baseElems Nothing = C.SimpleSeq (addNDistinctFrom S.empty baseElems 0) undefined
envSetup baseElems (Just opElems) = C.SimpleSeq (addNDistinctFrom S.empty baseElems 0) (addNDistinctFrom S.empty opElems 0)

addExperiment :: C.ExperimentFunction
addExperiment _ Nothing = error "Add experiment needs operator elements"
addExperiment (C.SimpleSeq ds t) (Just opElems) = C.SimpleSeq (addNDistinctFrom ds opElems 0) t

addAll :: S.Seq Int -> S.Seq Int -> S.Seq Int
addAll = S.append

addAllExperiment :: C.ExperimentFunction
addAllExperiment (C.SimpleSeq ds t) _ = C.SimpleSeq (addAll ds t) t

remove :: S.Seq Int -> S.Seq Int
remove s = if S.null s then s else S.ltail s

clear :: S.Seq Int -> S.Seq Int
clear s = if S.null s then s else clear $ remove s

clearExperiment :: C.ExperimentFunction
clearExperiment (C.SimpleSeq ds t) _ = C.SimpleSeq (clear ds) t

contains :: S.Seq Int -> Int -> Bool
contains s e = not . S.null . S.filter ( (==) e ) $ s

containsExperiment :: C.ExperimentFunction
containsExperiment experimenter@(C.SimpleSeq ds _) _ = contains ds elemToSearch `deepseq` experimenter
    where
        elemToSearch = 9999999

containsAll :: S.Seq Int -> S.Seq Int -> Bool
containsAll s t = S.foldr (&&) True . S.map ( s `contains` ) $ t

containsAllExperiment :: C.ExperimentFunction
containsAllExperiment experimenter@(C.SimpleSeq ds t) _ = containsAll ds t `deepseq` experimenter

iterator :: S.Seq Int -> S.Seq Int
iterator = S.map ( id )

iteratorExperiment :: C.ExperimentFunction
iteratorExperiment (C.SimpleSeq ds t) _ = C.SimpleSeq (iterator ds) t

removeExperiment :: C.ExperimentFunction
removeExperiment (C.SimpleSeq ds t) _ = C.SimpleSeq (remove ds) t

removeAll :: S.Seq Int -> S.Seq Int -> S.Seq Int
removeAll s t = S.filter ( not . ( t `contains` ) ) s

removeAllExperiment :: C.ExperimentFunction
removeAllExperiment (C.SimpleSeq ds t) _ = C.SimpleSeq (removeAll ds t) t

retainAll :: S.Seq Int -> S.Seq Int -> S.Seq Int
retainAll s t = S.filter (t `contains`) s

retainAllExperiment :: C.ExperimentFunction
retainAllExperiment (C.SimpleSeq ds t) _ = C.SimpleSeq (retainAll ds t) t

toList :: S.Seq Int -> [Int]
toList = S.toList

toListExperiment :: C.ExperimentFunction
toListExperiment experimenter@(C.SimpleSeq ds _) _ = toList ds `deepseq` experimenter

import System.Environment
import Control.DeepSeq

import qualified Data.Edison.Seq.SimpleQueue as S

data DataStructure = Seq deriving Show
data Experiment = Add | AddAll | Clear | Contains | ContainsAll | Iterator | Remove deriving Show

rawStringToDataStructure :: String -> DataStructure
rawStringToDataStructure "Seq" = Seq
rawStringToDataStructure _ = error "Could not match any data structure"

rawStringToExperiment :: String -> Experiment
rawStringToExperiment "Add" = Add
rawStringToExperiment "AddAll" = AddAll
rawStringToExperiment "Clear" = Clear
rawStringToExperiment "Contains" = Contains
rawStringToExperiment "ContainsAll" = ContainsAll
rawStringToExperiment "Iterator" = Iterator
rawStringToExperiment "Remove" = Remove
rawStringToExperiment _ = error "Could not match any experiment"

dataStructureAndExperimentToExperimentFunction :: DataStructure -> Experiment -> IO ()
dataStructureAndExperimentToExperimentFunction Seq Add = seqAddExperiment
dataStructureAndExperimentToExperimentFunction Seq AddAll = seqAddAllExperiment
dataStructureAndExperimentToExperimentFunction Seq Clear = seqClearExperiment
dataStructureAndExperimentToExperimentFunction Seq Contains = seqContainsExperiment
dataStructureAndExperimentToExperimentFunction Seq ContainsAll = seqContainsAllExperiment
dataStructureAndExperimentToExperimentFunction Seq Iterator = seqIteratorExperiment
dataStructureAndExperimentToExperimentFunction Seq Remove = seqRemoveExperiment

baseNElems :: Int
baseNElems = 100000

addFromNElems :: Int
addFromNElems = 100000

addAllFromNElems :: Int
addAllFromNElems  = 1000

addAllNRepeats :: Int
addAllNRepeats = 1000

clearNElems :: Int
clearNElems = 100000

containsNRepeats  :: Int
containsNRepeats = 1000

containsElement :: Int
containsElement = 9999999

containsAllNRepeats :: Int
containsAllNRepeats = 5000

containsAllSearchInNElems :: Int
containsAllSearchInNElems = baseNElems

containsAllSearchForNElems :: Int
containsAllSearchForNElems = 1000

removeFromNElems :: Int
removeFromNElems = baseNElems

removeNRepeats :: Int
removeNRepeats = 10000

instance (NFData a) => NFData (S.Seq a) where
    rnf s = S.strictWith rnf s `seq` ()

main :: IO ()
main = do
    args <- getArgs
    let ds = rawStringToDataStructure $ head args
    let experiment = rawStringToExperiment $ args !! 1
    experimentResult <- dataStructureAndExperimentToExperimentFunction ds experiment
    experimentResult  `deepseq` return ()

seqAddExperiment :: IO ()
seqAddExperiment = benchmark `deepseq` return ()
    where
        ds = addEnvSetup
        benchmark = addNDistinctFrom ds addFromNElems 0

seqAddAllExperiment :: IO ()
seqAddAllExperiment = benchmark `deepseq` return ()
    where
        (s, t) = addAllEnvSetup
        benchmark = addAllNTimes s t addAllNRepeats

seqClearExperiment :: IO ()
seqClearExperiment = benchmark `deepseq` return ()
    where
        ds = clearEnvSetup
        benchmark = clear ds

seqContainsExperiment :: IO ()
seqContainsExperiment = benchmark `deepseq` return ()
    where
        ds = containsEnvSetup
        benchmark = containsNTimes ds containsElement containsNRepeats

seqContainsAllExperiment :: IO ()
seqContainsAllExperiment = benchmark `deepseq` return ()
    where
        (s, t) = containsAllEnvSetup
        benchmark = containsAllNTimes s t containsAllNRepeats

seqIteratorExperiment :: IO ()
seqIteratorExperiment = benchmark `deepseq` return ()
    where
        ds = iteratorEnvSetup
        benchmark = iterator ds

seqRemoveExperiment :: IO ()
seqRemoveExperiment = benchmark `deepseq` return ()
    where
        ds = removeEnvSetup
        benchmark = removeNTimes ds removeNRepeats

addNDistinctFrom :: S.Seq Int -> Int -> Int -> S.Seq Int
addNDistinctFrom seq 0 _ = seq
addNDistinctFrom seq n m =
    let
        elemToAdd =  m + n - 1
        nextNumber = n - 1
        cons = if even n then S.rcons else S.lcons
    in
        addNDistinctFrom ( elemToAdd `cons` seq ) nextNumber m

addAll :: S.Seq Int -> S.Seq Int -> S.Seq Int
addAll = S.append

addAllNTimes :: S.Seq Int -> S.Seq Int -> Int -> S.Seq Int
addAllNTimes s _ 0 = s
addAllNTimes s t n = deepseq ( addAll s t ) ( addAllNTimes s t ( n - 1 ) )

remove :: S.Seq Int -> S.Seq Int
remove s = if S.null s then s else S.ltail s

clear :: S.Seq Int -> S.Seq Int
clear s = if S.null s then s else clear $ remove s

contains :: S.Seq Int -> Int -> Bool
contains s e = not . S.null . S.filter ( (==) e ) $ s

containsNTimes :: S.Seq Int -> Int -> Int -> Bool
containsNTimes _ _ 0 = False
containsNTimes s e n = ( (||) ( containsNTimes s e ( n - 1 ) ) ) $!! ( contains s e )

containsAll :: S.Seq Int -> S.Seq Int -> Bool
containsAll s t = S.foldr (&&) True . S.map ( s `contains` ) $ t

containsAllNTimes :: S.Seq Int -> S.Seq Int -> Int -> Bool
containsAllNTimes _ _ 0 = False
containsAllNTimes s t n = ( (||) ( containsAllNTimes s t ( n - 1 ) ) ) $!! ( s `containsAll` t )

iterator :: S.Seq Int -> S.Seq Int
iterator = S.map ( id )

removeNTimes :: S.Seq Int -> Int -> S.Seq Int
removeNTimes s 0 = s
removeNTimes s n = deepseq ( remove s ) ( removeNTimes s ( n - 1 ) )

defaultEnv :: S.Seq Int
defaultEnv = addNDistinctFrom S.empty baseNElems 0

addAllEnvSetup :: (S.Seq Int, S.Seq Int)
addAllEnvSetup = (defaultEnv, addNDistinctFrom S.empty addAllFromNElems 0)

addEnvSetup :: S.Seq Int
addEnvSetup = defaultEnv

clearEnvSetup :: S.Seq Int
clearEnvSetup = addNDistinctFrom S.empty clearNElems 0

containsEnvSetup :: S.Seq Int
containsEnvSetup = defaultEnv

containsAllEnvSetup :: (S.Seq Int, S.Seq Int)
containsAllEnvSetup = (addNDistinctFrom S.empty containsAllSearchInNElems 0, addNDistinctFrom S.empty containsAllSearchForNElems 0)

iteratorEnvSetup :: S.Seq Int
iteratorEnvSetup = defaultEnv

removeEnvSetup :: S.Seq Int
removeEnvSetup = addNDistinctFrom S.empty removeFromNElems 0

import System.Environment
import Control.DeepSeq

import qualified Data.Edison.Seq.SimpleQueue as S

data DataStructure = Seq deriving Show
data Experiment = Add | AddAll | Clear | Contains deriving Show

rawStringToDataStructure :: String -> DataStructure
rawStringToDataStructure "Seq" = Seq
rawStringToDataStructure _ = error "Could not match any data structure"

rawStringToExperiment :: String -> Experiment
rawStringToExperiment "Add" = Add
rawStringToExperiment "AddAll" = AddAll
rawStringToExperiment "Clear" = Clear
rawStringToExperiment "Contains" = Contains
rawStringToExperiment _ = error "Could not match any experiment"

dataStructureAndExperimentToExperimentFunction :: DataStructure -> Experiment -> IO ()
dataStructureAndExperimentToExperimentFunction Seq Add = seqAddExperiment
dataStructureAndExperimentToExperimentFunction Seq AddAll = seqAddAllExperiment
dataStructureAndExperimentToExperimentFunction Seq Clear = seqClearExperiment
dataStructureAndExperimentToExperimentFunction Seq Contains = seqContainsExperiment

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

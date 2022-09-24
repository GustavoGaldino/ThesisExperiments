import System.Environment
import Control.DeepSeq

import qualified Data.Edison.Seq.SimpleQueue as S

data DataStructure = Seq deriving Show
data Experiment = Add | AddAll | Clear | Contains | ContainsAll | Iterator | Remove | RemoveAll | RetainAll | ToList deriving Show

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
rawStringToExperiment "RemoveAll" = RemoveAll
rawStringToExperiment "RetainAll" = RetainAll
rawStringToExperiment "ToList" = ToList
rawStringToExperiment _ = error "Could not match any experiment"

experimentToExperimentFunction :: Experiment -> ExperimentFunction
experimentToExperimentFunction Add = addExperiment
experimentToExperimentFunction AddAll = addAllExperiment
experimentToExperimentFunction Clear = clearExperiment
experimentToExperimentFunction Contains = containsExperiment
experimentToExperimentFunction ContainsAll = containsAllExperiment
experimentToExperimentFunction Iterator = iteratorExperiment
experimentToExperimentFunction Remove = removeExperiment
experimentToExperimentFunction RemoveAll = removeAllExperiment
experimentToExperimentFunction RetainAll = retainAllExperiment
experimentToExperimentFunction ToList = toListExperiment

simpleSeqEnvSetup :: Int -> Maybe Int -> Experimenter
simpleSeqEnvSetup baseElems Nothing = SimpleSeq (addNDistinctFrom S.empty baseElems 0) undefined
simpleSeqEnvSetup baseElems (Just opElems) = SimpleSeq (addNDistinctFrom S.empty baseElems 0) (addNDistinctFrom S.empty opElems 0)

dataStructureToExperimenterSetup :: DataStructure -> Int -> Maybe Int -> Experimenter
dataStructureToExperimenterSetup Seq = simpleSeqEnvSetup

instance (NFData a) => NFData (S.Seq a) where
    rnf s = S.strictWith rnf s `seq` ()

data Experimenter = SimpleSeq (S.Seq Int) (S.Seq Int)
instance NFData Experimenter where
    rnf (SimpleSeq ds _) = rnf ds

type ExperimentFunction = Experimenter -> Maybe Int -> Experimenter

addNDistinctFrom :: S.Seq Int -> Int -> Int -> S.Seq Int
addNDistinctFrom s 0 _ = s
addNDistinctFrom s n m =
    let
        elemToAdd =  m + n - 1
        nextNumber = n - 1
        cons = if even n then S.rcons else S.lcons
    in
        addNDistinctFrom (elemToAdd `cons` s) nextNumber m

main :: IO ()
main = do
    args <- getArgs
    let ds = rawStringToDataStructure $ head args
    let experiment = rawStringToExperiment $ args !! 1
    let iters = read (args !! 2) :: Int
    let baseElems = read (args !! 3) :: Int
    let opElems = if (length args) > 4 then Just (read (args !! 4) :: Int) else Nothing
    let experimentF = experimentToExperimentFunction experiment
    let experimenter = dataStructureToExperimenterSetup ds baseElems opElems
    runNExperimentFunction iters opElems experimenter experimentF

runNExperimentFunction :: Int -> Maybe Int -> Experimenter -> ExperimentFunction -> IO ()
runNExperimentFunction 0 _ _ _ = return ()
runNExperimentFunction iters opElems experimenter expF = expF experimenter opElems `deepseq` runNExperimentFunction (iters-1) opElems experimenter expF

addExperiment :: ExperimentFunction
addExperiment _ Nothing = error "Add experiment needs operator elements"
addExperiment (SimpleSeq ds t) (Just opElems) = SimpleSeq (addNDistinctFrom ds opElems 0) t

addAll :: S.Seq Int -> S.Seq Int -> S.Seq Int
addAll = S.append

addAllExperiment :: ExperimentFunction
addAllExperiment (SimpleSeq ds t) _ = SimpleSeq (addAll ds t) t

remove :: S.Seq Int -> S.Seq Int
remove s = if S.null s then s else S.ltail s

clear :: S.Seq Int -> S.Seq Int
clear s = if S.null s then s else clear $ remove s

clearExperiment :: ExperimentFunction
clearExperiment (SimpleSeq ds t) _ = SimpleSeq (clear ds) t

contains :: S.Seq Int -> Int -> Bool
contains s e = not . S.null . S.filter ( (==) e ) $ s

containsExperiment :: ExperimentFunction
containsExperiment experimenter@(SimpleSeq ds _) _ = contains ds elemToSearch `deepseq` experimenter
    where
        elemToSearch = 9999999

containsAll :: S.Seq Int -> S.Seq Int -> Bool
containsAll s t = S.foldr (&&) True . S.map ( s `contains` ) $ t

containsAllExperiment :: ExperimentFunction
containsAllExperiment experimenter@(SimpleSeq ds t) _ = containsAll ds t `deepseq` experimenter

iterator :: S.Seq Int -> S.Seq Int
iterator = S.map ( id )

iteratorExperiment :: ExperimentFunction
iteratorExperiment (SimpleSeq ds t) _ = SimpleSeq (iterator ds) t

removeExperiment :: ExperimentFunction
removeExperiment (SimpleSeq ds t) _ = SimpleSeq (remove ds) t

removeAll :: S.Seq Int -> S.Seq Int -> S.Seq Int
removeAll s t = S.filter ( not . ( t `contains` ) ) s

removeAllExperiment :: ExperimentFunction
removeAllExperiment (SimpleSeq ds t) _ = SimpleSeq (removeAll ds t) t

retainAll :: S.Seq Int -> S.Seq Int -> S.Seq Int
retainAll s t = S.filter (t `contains`) s

retainAllExperiment :: ExperimentFunction
retainAllExperiment (SimpleSeq ds t) _ = SimpleSeq (retainAll ds t) t

toList :: S.Seq Int -> [Int]
toList = S.toList

toListExperiment :: ExperimentFunction
toListExperiment experimenter@(SimpleSeq ds _) _ = toList ds `deepseq` experimenter

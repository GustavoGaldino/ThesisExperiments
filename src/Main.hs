import System.Environment

import Control.DeepSeq

import qualified Commons as C
import qualified DataStructures.SimpleSeq as SS

-- import qualified Data.Edison.Seq.SimpleQueue as S

-- Supported data structures
data DataStructure = Seq deriving Show

-- Supported experiments (all of them)
data Experiment = Add | AddAll | Clear | Contains | ContainsAll | Iterator | Remove | RemoveAll | RetainAll | ToList deriving Show

-- Parse data structure input string
rawStringToDataStructure :: String -> DataStructure
rawStringToDataStructure "Seq" = Seq
rawStringToDataStructure _ = error "Could not match any data structure"

-- Parse experiment input string
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

-- Map parsed experiment input to experiment function
experimentToExperimentFunction :: Experiment -> C.ExperimentFunction
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

-- Map parsed data structure input to data structure setup (base data structure to run experiments on)
dataStructureToExperimenterSetup :: DataStructure -> Int -> Maybe Int -> C.Experimenter
dataStructureToExperimenterSetup Seq = SS.envSetup

-- Runs a given experiment function N times forcing evaluation through intermediate steps
runNExperimentFunction :: Int -> Maybe Int -> C.Experimenter -> C.ExperimentFunction -> IO ()
runNExperimentFunction 0 _ _ _ = return ()
runNExperimentFunction iters opElems experimenter expF = expF experimenter opElems `deepseq` runNExperimentFunction (iters-1) opElems experimenter expF

-- Experiment functions (delegates to data structure module implementation)
addExperiment :: C.ExperimentFunction
addExperiment _ Nothing = error "Add experiment needs operator elements"
addExperiment ds@(C.SimpleSeq _ _) opElems = SS.addExperiment ds opElems

addAllExperiment :: C.ExperimentFunction
addAllExperiment ds@(C.SimpleSeq _ _) opElems = SS.addAllExperiment ds opElems

clearExperiment :: C.ExperimentFunction
clearExperiment ds@(C.SimpleSeq _ _) opElems = SS.clearExperiment ds opElems

containsExperiment :: C.ExperimentFunction
containsExperiment ds@(C.SimpleSeq _ _) opElems = SS.containsExperiment ds opElems

containsAllExperiment :: C.ExperimentFunction
containsAllExperiment ds@(C.SimpleSeq _ _) opElems = SS.containsAllExperiment ds opElems

iteratorExperiment :: C.ExperimentFunction
iteratorExperiment ds@(C.SimpleSeq _ _) opElems = SS.iteratorExperiment ds opElems

removeExperiment :: C.ExperimentFunction
removeExperiment ds@(C.SimpleSeq _ _) opElems = SS.removeExperiment ds opElems

removeAllExperiment :: C.ExperimentFunction
removeAllExperiment ds@(C.SimpleSeq _ _) opElems = SS.removeAllExperiment ds opElems

retainAllExperiment :: C.ExperimentFunction
retainAllExperiment ds@(C.SimpleSeq _ _) opElems = SS.removeAllExperiment ds opElems

toListExperiment :: C.ExperimentFunction
toListExperiment ds@(C.SimpleSeq _ _) opElems = SS.toListExperiment ds opElems

-- Read execution arguments, parse it and run experiment
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

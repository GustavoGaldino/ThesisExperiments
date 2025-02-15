import System.Environment

import Control.DeepSeq

import qualified Commons as C
import qualified DataStructures.Main as M
import qualified DataStructures.SimpleQueue as SQ
import qualified DataStructures.StandardSet as SS
import qualified DataStructures.AssocList as AL
import qualified DataStructures.StandardMap as SM
import qualified DataStructures.UnbalancedSet as US
import qualified DataStructures.MinHeap as MH
import qualified DataStructures.SplayHeap as SH
import qualified DataStructures.LazyPairingHeap as LPH
import qualified DataStructures.SkewHeap as SKH
import qualified DataStructures.LeftistHeap as LH

-- Supported data structures
data DataStructure = SimpleQueue | StandardSet | AssocList | MinHeap | StandardMap | UnbalancedSet | SplayHeap | LazyPairingHeap | SkewHeap | LeftistHeap deriving Show

-- Supported experiments (all of them)
data Experiment = Add | AddAll | Clear | Contains | ContainsAll | Iterator | Remove | RemoveAll | RetainAll | ToList deriving Show

-- Parse data structure input string
rawStringToDataStructure :: String -> DataStructure
rawStringToDataStructure "SimpleQueue" = SimpleQueue
rawStringToDataStructure "StandardSet" = StandardSet
rawStringToDataStructure "AssocList" = AssocList
rawStringToDataStructure "MinHeap" = MinHeap
rawStringToDataStructure "StandardMap" = StandardMap
rawStringToDataStructure "UnbalancedSet" = UnbalancedSet
rawStringToDataStructure "SplayHeap" = SplayHeap 
rawStringToDataStructure "LazyPairingHeap" = LazyPairingHeap
rawStringToDataStructure "SkewHeap" = SkewHeap
rawStringToDataStructure "LeftistHeap" = LeftistHeap
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
dataStructureToExperimenterSetup SimpleQueue = SQ.envSetup
dataStructureToExperimenterSetup StandardSet = SS.envSetupSet
dataStructureToExperimenterSetup AssocList = AL.envSetup
dataStructureToExperimenterSetup MinHeap = MH.envSetup
dataStructureToExperimenterSetup StandardMap = SM.envSetup
dataStructureToExperimenterSetup UnbalancedSet = US.envSetupSet
dataStructureToExperimenterSetup SplayHeap = SH.envSetup
dataStructureToExperimenterSetup LazyPairingHeap = LPH.envSetup
dataStructureToExperimenterSetup SkewHeap = SKH.envSetup
dataStructureToExperimenterSetup LeftistHeap = LH.envSetup

-- Runs a given experiment function N times forcing evaluation through intermediate steps
runNExperimentFunction :: Int -> Maybe Int -> C.Experimenter -> C.ExperimentFunction -> IO ()
runNExperimentFunction 0 _ _ _ = return ()
runNExperimentFunction iters opElems experimenter expF = expF experimenter opElems `deepseq` runNExperimentFunction (iters-1) opElems experimenter expF

-- Experiment functions (delegates to data structure module implementation)
addExperiment :: C.ExperimentFunction
addExperiment _ Nothing = error "Add experiment needs operator elements"
addExperiment ds@(C.SimpleQueue _ _) opElems = M.addExperiment ds opElems
addExperiment ds@(C.StandardSet _ _) opElems = M.addExperiment ds opElems
addExperiment ds@(C.AssocList _ _) opElems = M.addExperiment ds opElems
addExperiment ds@(C.MinHeap _ _) opElems = M.addExperiment ds opElems
addExperiment ds@(C.StandardMap _ _) opElems = M.addExperiment ds opElems
addExperiment ds@(C.UnbalancedSet _ _) opElems = M.addExperiment ds opElems
addExperiment ds@(C.SplayHeap _ _) opElems = M.addExperiment ds opElems
addExperiment ds@(C.LazyPairingHeap _ _) opElems = M.addExperiment ds opElems
addExperiment ds@(C.SkewHeap _ _) opElems = M.addExperiment ds opElems
addExperiment ds@(C.LeftistHeap _ _) opElems = M.addExperiment ds opElems

addAllExperiment :: C.ExperimentFunction
addAllExperiment ds@(C.SimpleQueue _ _) opElems = M.addAllExperiment ds opElems
addAllExperiment ds@(C.StandardSet _ _) opElems = M.addAllExperiment ds opElems
addAllExperiment ds@(C.AssocList _ _) opElems = M.addAllExperiment ds opElems
addAllExperiment ds@(C.MinHeap _ _) opElems = M.addAllExperiment ds opElems
addAllExperiment ds@(C.StandardMap _ _) opElems = M.addAllExperiment ds opElems
addAllExperiment ds@(C.UnbalancedSet _ _) opElems = M.addAllExperiment ds opElems
addAllExperiment ds@(C.SplayHeap _ _) opElems = M.addAllExperiment ds opElems
addAllExperiment ds@(C.LazyPairingHeap _ _) opElems = M.addAllExperiment ds opElems
addAllExperiment ds@(C.SkewHeap _ _) opElems = M.addAllExperiment ds opElems
addAllExperiment ds@(C.LeftistHeap _ _) opElems = M.addAllExperiment ds opElems

clearExperiment :: C.ExperimentFunction
clearExperiment ds@(C.SimpleQueue _ _) opElems = M.clearExperiment ds opElems
clearExperiment ds@(C.StandardSet _ _) opElems = M.clearExperiment ds opElems
clearExperiment ds@(C.AssocList _ _) opElems = M.clearExperiment ds opElems
clearExperiment ds@(C.MinHeap _ _) opElems = M.clearExperiment ds opElems
clearExperiment ds@(C.StandardMap _ _) opElems = M.clearExperiment ds opElems
clearExperiment ds@(C.UnbalancedSet _ _) opElems = M.clearExperiment ds opElems
clearExperiment ds@(C.SplayHeap _ _) opElems = M.clearExperiment ds opElems
clearExperiment ds@(C.LazyPairingHeap _ _) opElems = M.clearExperiment ds opElems
clearExperiment ds@(C.SkewHeap _ _) opElems = M.clearExperiment ds opElems
clearExperiment ds@(C.LeftistHeap _ _) opElems = M.clearExperiment ds opElems

containsExperiment :: C.ExperimentFunction
containsExperiment ds@(C.SimpleQueue _ _) opElems = M.containsExperiment ds opElems
containsExperiment ds@(C.StandardSet _ _) opElems = M.containsExperiment ds opElems
containsExperiment ds@(C.AssocList _ _) opElems = M.containsExperiment ds opElems
containsExperiment ds@(C.MinHeap _ _) opElems = M.containsExperiment ds opElems
containsExperiment ds@(C.StandardMap _ _) opElems = M.containsExperiment ds opElems
containsExperiment ds@(C.UnbalancedSet _ _) opElems = M.containsExperiment ds opElems
containsExperiment ds@(C.SplayHeap _ _) opElems = M.containsExperiment ds opElems
containsExperiment ds@(C.LazyPairingHeap _ _) opElems = M.containsExperiment ds opElems
containsExperiment ds@(C.SkewHeap _ _) opElems = M.containsExperiment ds opElems
containsExperiment ds@(C.LeftistHeap _ _) opElems = M.containsExperiment ds opElems

containsAllExperiment :: C.ExperimentFunction
containsAllExperiment ds@(C.SimpleQueue _ _) opElems = M.containsAllExperiment ds opElems
containsAllExperiment ds@(C.StandardSet _ _) opElems = M.containsAllExperiment ds opElems
containsAllExperiment ds@(C.AssocList _ _) opElems = M.containsAllExperiment ds opElems
containsAllExperiment ds@(C.MinHeap _ _) opElems = M.containsAllExperiment ds opElems
containsAllExperiment ds@(C.StandardMap _ _) opElems = M.containsAllExperiment ds opElems
containsAllExperiment ds@(C.UnbalancedSet _ _) opElems = M.containsAllExperiment ds opElems
containsAllExperiment ds@(C.SplayHeap _ _) opElems = M.containsAllExperiment ds opElems
containsAllExperiment ds@(C.LazyPairingHeap _ _) opElems = M.containsAllExperiment ds opElems
containsAllExperiment ds@(C.SkewHeap _ _) opElems = M.containsAllExperiment ds opElems
containsAllExperiment ds@(C.LeftistHeap _ _) opElems = M.containsAllExperiment ds opElems

iteratorExperiment :: C.ExperimentFunction
iteratorExperiment ds@(C.SimpleQueue _ _) opElems = M.iteratorExperiment ds opElems
iteratorExperiment ds@(C.StandardSet _ _) opElems = M.iteratorExperiment ds opElems
iteratorExperiment ds@(C.AssocList _ _) opElems = M.iteratorExperiment ds opElems
iteratorExperiment ds@(C.MinHeap _ _) opElems = M.iteratorExperiment ds opElems
iteratorExperiment ds@(C.StandardMap _ _) opElems = M.iteratorExperiment ds opElems
iteratorExperiment ds@(C.UnbalancedSet _ _) opElems = M.iteratorExperiment ds opElems
iteratorExperiment ds@(C.SplayHeap _ _) opElems = M.iteratorExperiment ds opElems
iteratorExperiment ds@(C.LazyPairingHeap _ _) opElems = M.iteratorExperiment ds opElems
iteratorExperiment ds@(C.SkewHeap _ _) opElems = M.iteratorExperiment ds opElems
iteratorExperiment ds@(C.LeftistHeap _ _) opElems = M.iteratorExperiment ds opElems

removeExperiment :: C.ExperimentFunction
removeExperiment ds@(C.SimpleQueue _ _) opElems = M.removeExperiment ds opElems
removeExperiment ds@(C.StandardSet _ _) opElems = M.removeExperiment ds opElems
removeExperiment ds@(C.AssocList _ _) opElems = M.removeExperiment ds opElems
removeExperiment ds@(C.MinHeap _ _) opElems = M.removeExperiment ds opElems
removeExperiment ds@(C.StandardMap _ _) opElems = M.removeExperiment ds opElems
removeExperiment ds@(C.UnbalancedSet _ _) opElems = M.removeExperiment ds opElems
removeExperiment ds@(C.SplayHeap _ _) opElems = M.removeExperiment ds opElems
removeExperiment ds@(C.LazyPairingHeap _ _) opElems = M.removeExperiment ds opElems
removeExperiment ds@(C.SkewHeap _ _) opElems = M.removeExperiment ds opElems
removeExperiment ds@(C.LeftistHeap _ _) opElems = M.removeExperiment ds opElems

removeAllExperiment :: C.ExperimentFunction
removeAllExperiment ds@(C.SimpleQueue _ _) opElems = M.removeAllExperiment ds opElems
removeAllExperiment ds@(C.StandardSet _ _) opElems = M.removeAllExperiment ds opElems
removeAllExperiment ds@(C.AssocList _ _) opElems = M.removeAllExperiment ds opElems
removeAllExperiment ds@(C.MinHeap _ _) opElems = M.removeAllExperiment ds opElems
removeAllExperiment ds@(C.StandardMap _ _) opElems = M.removeAllExperiment ds opElems
removeAllExperiment ds@(C.UnbalancedSet _ _) opElems = M.removeAllExperiment ds opElems
removeAllExperiment ds@(C.SplayHeap _ _) opElems = M.removeAllExperiment ds opElems
removeAllExperiment ds@(C.LazyPairingHeap _ _) opElems = M.removeAllExperiment ds opElems
removeAllExperiment ds@(C.SkewHeap _ _) opElems = M.removeAllExperiment ds opElems
removeAllExperiment ds@(C.LeftistHeap _ _) opElems = M.removeAllExperiment ds opElems

retainAllExperiment :: C.ExperimentFunction
retainAllExperiment ds@(C.SimpleQueue _ _) opElems = M.retainAllExperiment ds opElems
retainAllExperiment ds@(C.StandardSet _ _) opElems = M.retainAllExperiment ds opElems
retainAllExperiment ds@(C.AssocList _ _) opElems = M.retainAllExperiment ds opElems
retainAllExperiment ds@(C.MinHeap _ _) opElems = M.retainAllExperiment ds opElems
retainAllExperiment ds@(C.StandardMap _ _) opElems = M.retainAllExperiment ds opElems
retainAllExperiment ds@(C.UnbalancedSet _ _) opElems = M.retainAllExperiment ds opElems
retainAllExperiment ds@(C.SplayHeap _ _) opElems = M.retainAllExperiment ds opElems
retainAllExperiment ds@(C.LazyPairingHeap _ _) opElems = M.retainAllExperiment ds opElems
retainAllExperiment ds@(C.SkewHeap _ _) opElems = M.retainAllExperiment ds opElems
retainAllExperiment ds@(C.LeftistHeap _ _) opElems = M.retainAllExperiment ds opElems

toListExperiment :: C.ExperimentFunction
toListExperiment ds@(C.SimpleQueue _ _) opElems = M.toListExperiment ds opElems
toListExperiment ds@(C.StandardSet _ _) opElems = M.toListExperiment ds opElems
toListExperiment ds@(C.AssocList _ _) opElems = M.toListExperiment ds opElems
toListExperiment ds@(C.MinHeap _ _) opElems = M.toListExperiment ds opElems
toListExperiment ds@(C.StandardMap _ _) opElems = M.toListExperiment ds opElems
toListExperiment ds@(C.UnbalancedSet _ _) opElems = M.toListExperiment ds opElems
toListExperiment ds@(C.SplayHeap _ _) opElems = M.toListExperiment ds opElems
toListExperiment ds@(C.LazyPairingHeap _ _) opElems = M.toListExperiment ds opElems
toListExperiment ds@(C.SkewHeap _ _) opElems = M.toListExperiment ds opElems
toListExperiment ds@(C.LeftistHeap _ _) opElems = M.toListExperiment ds opElems

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
    -- Force evaluation of base and operation data structures before experiment starts
    return $  deepseq experimenter ()
    -- Run the experiment <iters> times
    runNExperimentFunction iters opElems experimenter experimentF

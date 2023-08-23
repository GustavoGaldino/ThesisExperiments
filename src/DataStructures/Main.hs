module DataStructures.Main (
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
import qualified DataStructures.SimpleQueue as SQ
import qualified DataStructures.StandardSet as SS
import qualified DataStructures.AssocList as AL
import qualified DataStructures.MinHeap as MH
import Commons as C

addExperiment :: C.ExperimentFunction
addExperiment _ Nothing = error "Add experiment needs operator elements"
addExperiment (C.SimpleQueue ds t) (Just opElems) = C.SimpleQueue (SQ.addNDistinctFrom ds opElems 0) t
addExperiment (C.StandardSet ds t) (Just opElems) = C.StandardSet (SS.addNDistinctFromSet ds opElems 0) t
addExperiment (C.AssocList ds t) (Just opElems) = C.AssocList (AL.addNDistinctFrom ds opElems 0) t
addExperiment (C.MinHeap ds t) (Just opElems) = C.MinHeap (MH.addNDistinctFrom ds opElems 0) t

addAllExperiment :: C.ExperimentFunction
addAllExperiment (C.SimpleQueue ds t) _ = C.SimpleQueue (SQ.addAll ds t) t
addAllExperiment (C.StandardSet ds t) _ = C.StandardSet (SS.addAllSet ds t) t
addAllExperiment (C.AssocList ds t) _ = C.AssocList (AL.addAll ds t) t
addAllExperiment (C.MinHeap ds t) _ = C.MinHeap (MH.addAll ds t) t

clearExperiment :: C.ExperimentFunction
clearExperiment (C.SimpleQueue ds t) _ = C.SimpleQueue (SQ.clear ds) t
clearExperiment (C.StandardSet ds t) _ = C.StandardSet (SS.clearSet ds) t
clearExperiment (C.AssocList ds t) _ = C.AssocList (AL.clear ds) t
clearExperiment (C.MinHeap ds t) _ = C.MinHeap (MH.clear ds) t

containsExperiment :: C.ExperimentFunction
containsExperiment experimenter@(C.SimpleQueue ds _) _ = let elemToSearch = 9999999 in SQ.contains ds elemToSearch `deepseq` experimenter
containsExperiment experimenter@(C.StandardSet ds _) _ = let elemToSearch = 9999999 in SS.containsSet ds elemToSearch `deepseq` experimenter
containsExperiment experimenter@(C.AssocList ds _) _ = let elemToSearch = 9999999 in AL.contains elemToSearch ds `deepseq` experimenter
containsExperiment experimenter@(C.MinHeap ds _) _ = let elemToSearch = 9999999 in MH.contains ds elemToSearch `deepseq` experimenter
    where
        elemToSearch = 9999999

containsAllExperiment :: C.ExperimentFunction
containsAllExperiment experimenter@(C.SimpleQueue ds t) _ = SQ.containsAll ds t `deepseq` experimenter
containsAllExperiment experimenter@(C.StandardSet ds t) _ = SS.containsAllSet ds t `deepseq` experimenter
containsAllExperiment experimenter@(C.AssocList ds t) _ = AL.containsAll ds t `deepseq` experimenter
containsAllExperiment experimenter@(C.MinHeap ds t) _ = MH.containsAll ds t `deepseq` experimenter

iteratorExperiment :: C.ExperimentFunction
iteratorExperiment (C.SimpleQueue ds t) _ = C.SimpleQueue (SQ.iterator ds) t
iteratorExperiment (C.StandardSet ds t) _ = C.StandardSet (SS.iteratorSet ds) t
iteratorExperiment (C.AssocList ds t) _ = C.AssocList (AL.iterator ds) t
iteratorExperiment (C.MinHeap ds t) _ = C.MinHeap (MH.iterator ds) t

removeExperiment :: C.ExperimentFunction
removeExperiment (C.SimpleQueue ds t) _ = C.SimpleQueue (SQ.remove ds) t
removeExperiment (C.StandardSet ds t) _ = C.StandardSet (SS.removeSet ds) t
removeExperiment (C.AssocList ds t) _ = C.AssocList (AL.remove ds) t
removeExperiment (C.MinHeap ds t) _ = C.MinHeap (MH.remove ds) t

removeAllExperiment :: C.ExperimentFunction
removeAllExperiment (C.SimpleQueue ds t) _ = C.SimpleQueue (SQ.removeAll ds t) t
removeAllExperiment (C.StandardSet ds t) _ = C.StandardSet (SS.removeAllSet ds t) t
removeAllExperiment (C.AssocList ds t) _ = C.AssocList (AL.removeAll ds t) t
removeAllExperiment (C.MinHeap ds t) _ = C.MinHeap (MH.removeAll ds t) t

retainAllExperiment :: C.ExperimentFunction
retainAllExperiment (C.SimpleQueue ds t) _ = C.SimpleQueue (SQ.retainAll ds t) t
retainAllExperiment (C.StandardSet ds t) _ = C.StandardSet (SS.retainAllSet ds t) t
retainAllExperiment (C.AssocList ds t) _ = C.AssocList (AL.retainAll ds t) t
retainAllExperiment (C.MinHeap ds t) _ = C.MinHeap (MH.retainAll ds t) t

toListExperiment :: C.ExperimentFunction
toListExperiment experimenter@(C.SimpleQueue ds _) _ = SQ.toList ds `deepseq` experimenter
toListExperiment experimenter@(C.StandardSet ds _) _ = SS.toListSet ds `deepseq` experimenter
toListExperiment experimenter@(C.AssocList ds _) _ = AL.toList ds `deepseq` experimenter
toListExperiment experimenter@(C.MinHeap ds _) _ = MH.toList ds `deepseq` experimenter

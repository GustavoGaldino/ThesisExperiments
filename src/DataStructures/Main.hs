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
import qualified DataStructures.StandardMap as SM
import qualified DataStructures.UnbalancedSet as US
import qualified DataStructures.SplayHeap as SH
import qualified DataStructures.LazyPairingHeap as LPH
import qualified DataStructures.SkewHeap as SKH
import qualified DataStructures.LeftistHeap as LH
import Commons as C

addExperiment :: C.ExperimentFunction
addExperiment _ Nothing = error "Add experiment needs operator elements"
addExperiment (C.SimpleQueue ds t) (Just opElems) = C.SimpleQueue (SQ.addNDistinctFrom ds opElems 0) t
addExperiment (C.StandardSet ds t) (Just opElems) = C.StandardSet (SS.addNDistinctFromSet ds opElems 0) t
addExperiment (C.AssocList ds t) (Just opElems) = C.AssocList (AL.addNDistinctFrom ds opElems 0) t
addExperiment (C.MinHeap ds t) (Just opElems) = C.MinHeap (MH.addNDistinctFrom ds opElems 0) t
addExperiment (C.StandardMap ds t) (Just opElems) = C.StandardMap (SM.addNDistinctFrom ds opElems 0) t
addExperiment (C.UnbalancedSet ds t) (Just opElems) = C.UnbalancedSet (US.addNDistinctFromSet ds opElems 0) t
addExperiment (C.SplayHeap ds t) (Just opElems) = C.SplayHeap (SH.addNDistinctFrom ds opElems 0) t
addExperiment (C.LazyPairingHeap ds t) (Just opElems) = C.LazyPairingHeap (LPH.addNDistinctFrom ds opElems 0) t
addExperiment (C.SkewHeap ds t) (Just opElems) = C.SkewHeap (SKH.addNDistinctFrom ds opElems 0) t
addExperiment (C.LeftistHeap ds t) (Just opElems) = C.LeftistHeap (LH.addNDistinctFrom ds opElems 0) t

addAllExperiment :: C.ExperimentFunction
addAllExperiment (C.SimpleQueue ds t) _ = C.SimpleQueue (SQ.addAll ds t) t
addAllExperiment (C.StandardSet ds t) _ = C.StandardSet (SS.addAllSet ds t) t
addAllExperiment (C.AssocList ds t) _ = C.AssocList (AL.addAll ds t) t
addAllExperiment (C.MinHeap ds t) _ = C.MinHeap (MH.addAll ds t) t
addAllExperiment (C.StandardMap ds t) _ = C.StandardMap (SM.addAll ds t) t
addAllExperiment (C.UnbalancedSet ds t) _ = C.UnbalancedSet (US.addAllSet ds t) t
addAllExperiment (C.SplayHeap ds t) _ = C.SplayHeap (SH.addAll ds t) t
addAllExperiment (C.LazyPairingHeap ds t) _ = C.LazyPairingHeap (LPH.addAll ds t) t
addAllExperiment (C.SkewHeap ds t) _ = C.SkewHeap (SKH.addAll ds t) t
addAllExperiment (C.LeftistHeap ds t) _ = C.LeftistHeap (LH.addAll ds t) t

clearExperiment :: C.ExperimentFunction
clearExperiment (C.SimpleQueue ds t) _ = C.SimpleQueue (SQ.clear ds) t
clearExperiment (C.StandardSet ds t) _ = C.StandardSet (SS.clearSet ds) t
clearExperiment (C.AssocList ds t) _ = C.AssocList (AL.clear ds) t
clearExperiment (C.MinHeap ds t) _ = C.MinHeap (MH.clear ds) t
clearExperiment (C.StandardMap ds t) _ = C.StandardMap (SM.clear ds) t
clearExperiment (C.UnbalancedSet ds t) _ = C.UnbalancedSet (US.clearSet ds) t
clearExperiment (C.SplayHeap ds t) _ = C.SplayHeap (SH.clear ds) t
clearExperiment (C.LazyPairingHeap ds t) _ = C.LazyPairingHeap (LPH.clear ds) t
clearExperiment (C.SkewHeap ds t) _ = C.SkewHeap (SKH.clear ds) t
clearExperiment (C.LeftistHeap ds t) _ = C.LeftistHeap (LH.clear ds) t

containsExperiment :: C.ExperimentFunction
containsExperiment experimenter@(C.SimpleQueue ds _) _ = let elemToSearch = 9999999 in SQ.contains ds elemToSearch `deepseq` experimenter
containsExperiment experimenter@(C.StandardSet ds _) _ = let elemToSearch = 9999999 in SS.containsSet ds elemToSearch `deepseq` experimenter
containsExperiment experimenter@(C.AssocList ds _) _ = let elemToSearch = 9999999 in AL.contains elemToSearch ds `deepseq` experimenter
containsExperiment experimenter@(C.MinHeap ds _) _ = let elemToSearch = 9999999 in MH.contains ds elemToSearch `deepseq` experimenter
containsExperiment experimenter@(C.StandardMap ds _) _ = let elemToSearch = 9999999 in SM.contains elemToSearch ds `deepseq` experimenter
containsExperiment experimenter@(C.UnbalancedSet ds _) _ = let elemToSearch = 9999999 in US.containsSet ds elemToSearch `deepseq` experimenter
containsExperiment experimenter@(C.SplayHeap ds _) _ = let elemToSearch = 9999999 in SH.contains ds elemToSearch `deepseq` experimenter
containsExperiment experimenter@(C.LazyPairingHeap ds _) _ = let elemToSearch = 9999999 in LPH.contains ds elemToSearch `deepseq` experimenter
containsExperiment experimenter@(C.SkewHeap ds _) _ = let elemToSearch = 9999999 in SKH.contains ds elemToSearch `deepseq` experimenter
containsExperiment experimenter@(C.LeftistHeap ds _) _ = let elemToSearch = 9999999 in LH.contains ds elemToSearch `deepseq` experimenter

containsAllExperiment :: C.ExperimentFunction
containsAllExperiment experimenter@(C.SimpleQueue ds t) _ = SQ.containsAll ds t `deepseq` experimenter
containsAllExperiment experimenter@(C.StandardSet ds t) _ = SS.containsAllSet ds t `deepseq` experimenter
containsAllExperiment experimenter@(C.AssocList ds t) _ = AL.containsAll ds t `deepseq` experimenter
containsAllExperiment experimenter@(C.MinHeap ds t) _ = MH.containsAll ds t `deepseq` experimenter
containsAllExperiment experimenter@(C.StandardMap ds t) _ = SM.containsAll ds t `deepseq` experimenter
containsAllExperiment experimenter@(C.UnbalancedSet ds t) _ = US.containsAllSet ds t `deepseq` experimenter
containsAllExperiment experimenter@(C.SplayHeap ds t) _ = SH.containsAll ds t `deepseq` experimenter
containsAllExperiment experimenter@(C.LazyPairingHeap ds t) _ = LPH.containsAll ds t `deepseq` experimenter
containsAllExperiment experimenter@(C.SkewHeap ds t) _ = SKH.containsAll ds t `deepseq` experimenter
containsAllExperiment experimenter@(C.LeftistHeap ds t) _ = LH.containsAll ds t `deepseq` experimenter

iteratorExperiment :: C.ExperimentFunction
iteratorExperiment (C.SimpleQueue ds t) _ = C.SimpleQueue (SQ.iterator ds) t
iteratorExperiment (C.StandardSet ds t) _ = C.StandardSet (SS.iteratorSet ds) t
iteratorExperiment (C.AssocList ds t) _ = C.AssocList (AL.iterator ds) t
iteratorExperiment (C.MinHeap ds t) _ = C.MinHeap (MH.iterator ds) t
iteratorExperiment (C.StandardMap ds t) _ = C.StandardMap (SM.iterator ds) t
iteratorExperiment (C.UnbalancedSet ds t) _ = C.UnbalancedSet (US.iteratorSet ds) t
iteratorExperiment (C.SplayHeap ds t) _ = C.SplayHeap (SH.iterator ds) t
iteratorExperiment (C.LazyPairingHeap ds t) _ = C.LazyPairingHeap (LPH.iterator ds) t
iteratorExperiment (C.SkewHeap ds t) _ = C.SkewHeap (SKH.iterator ds) t
iteratorExperiment (C.LeftistHeap ds t) _ = C.LeftistHeap (LH.iterator ds) t

removeExperiment :: C.ExperimentFunction
removeExperiment (C.SimpleQueue ds t) _ = C.SimpleQueue (SQ.remove ds) t
removeExperiment (C.StandardSet ds t) _ = C.StandardSet (SS.removeSet ds) t
removeExperiment (C.AssocList ds t) _ = C.AssocList (AL.remove ds) t
removeExperiment (C.MinHeap ds t) _ = C.MinHeap (MH.remove ds) t
removeExperiment (C.StandardMap ds t) _ = C.StandardMap (SM.remove ds) t
removeExperiment (C.UnbalancedSet ds t) _ = C.UnbalancedSet (US.removeSet ds) t
removeExperiment (C.SplayHeap ds t) _ = C.SplayHeap (SH.remove ds) t
removeExperiment (C.LazyPairingHeap ds t) _ = C.LazyPairingHeap (LPH.remove ds) t
removeExperiment (C.SkewHeap ds t) _ = C.SkewHeap (SKH.remove ds) t
removeExperiment (C.LeftistHeap ds t) _ = C.LeftistHeap (LH.remove ds) t

removeAllExperiment :: C.ExperimentFunction
removeAllExperiment (C.SimpleQueue ds t) _ = C.SimpleQueue (SQ.removeAll ds t) t
removeAllExperiment (C.StandardSet ds t) _ = C.StandardSet (SS.removeAllSet ds t) t
removeAllExperiment (C.AssocList ds t) _ = C.AssocList (AL.removeAll ds t) t
removeAllExperiment (C.MinHeap ds t) _ = C.MinHeap (MH.removeAll ds t) t
removeAllExperiment (C.StandardMap ds t) _ = C.StandardMap (SM.removeAll ds t) t
removeAllExperiment (C.UnbalancedSet ds t) _ = C.UnbalancedSet (US.removeAllSet ds t) t
removeAllExperiment (C.SplayHeap ds t) _ = C.SplayHeap (SH.removeAll ds t) t
removeAllExperiment (C.LazyPairingHeap ds t) _ = C.LazyPairingHeap (LPH.removeAll ds t) t
removeAllExperiment (C.SkewHeap ds t) _ = C.SkewHeap (SKH.removeAll ds t) t
removeAllExperiment (C.LeftistHeap ds t) _ = C.LeftistHeap (LH.removeAll ds t) t

retainAllExperiment :: C.ExperimentFunction
retainAllExperiment (C.SimpleQueue ds t) _ = C.SimpleQueue (SQ.retainAll ds t) t
retainAllExperiment (C.StandardSet ds t) _ = C.StandardSet (SS.retainAllSet ds t) t
retainAllExperiment (C.AssocList ds t) _ = C.AssocList (AL.retainAll ds t) t
retainAllExperiment (C.MinHeap ds t) _ = C.MinHeap (MH.retainAll ds t) t
retainAllExperiment (C.StandardMap ds t) _ = C.StandardMap (SM.retainAll ds t) t
retainAllExperiment (C.UnbalancedSet ds t) _ = C.UnbalancedSet (US.retainAllSet ds t) t
retainAllExperiment (C.SplayHeap ds t) _ = C.SplayHeap (SH.retainAll ds t) t
retainAllExperiment (C.LazyPairingHeap ds t) _ = C.LazyPairingHeap (LPH.retainAll ds t) t
retainAllExperiment (C.SkewHeap ds t) _ = C.SkewHeap (SKH.retainAll ds t) t
retainAllExperiment (C.LeftistHeap ds t) _ = C.LeftistHeap (LH.retainAll ds t) t

toListExperiment :: C.ExperimentFunction
toListExperiment experimenter@(C.SimpleQueue ds _) _ = SQ.toList ds `deepseq` experimenter
toListExperiment experimenter@(C.StandardSet ds _) _ = SS.toListSet ds `deepseq` experimenter
toListExperiment experimenter@(C.AssocList ds _) _ = AL.toList ds `deepseq` experimenter
toListExperiment experimenter@(C.MinHeap ds _) _ = MH.toList ds `deepseq` experimenter
toListExperiment experimenter@(C.StandardMap ds _) _ = SM.toList ds `deepseq` experimenter
toListExperiment experimenter@(C.UnbalancedSet ds _) _ = US.toListSet ds `deepseq` experimenter
toListExperiment experimenter@(C.SplayHeap ds _) _ = SH.toList ds `deepseq` experimenter
toListExperiment experimenter@(C.LazyPairingHeap ds _) _ = LPH.toList ds `deepseq` experimenter
toListExperiment experimenter@(C.SkewHeap ds _) _ = SKH.toList ds `deepseq` experimenter
toListExperiment experimenter@(C.LeftistHeap ds _) _ = LH.toList ds `deepseq` experimenter

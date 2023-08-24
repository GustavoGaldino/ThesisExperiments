{-# LANGUAGE FlexibleInstances #-}

module Commons (
    ExperimentFunction,
    Experimenter (..)
) where

import Control.DeepSeq
import qualified Data.Edison.Seq.SimpleQueue as SQ
import qualified Data.Edison.Coll.StandardSet as SS
import qualified Data.Edison.Assoc.AssocList as AL
import qualified Data.Edison.Assoc.StandardMap as SM
import qualified Data.Edison.Coll.MinHeap as MH
import qualified Data.Edison.Coll.UnbalancedSet as US
import qualified Data.Edison.Coll.SplayHeap as S

type MyMinHeap a = MH.Min (S.Heap a) a

type ExperimentFunction = Experimenter -> Maybe Int -> Experimenter

data Experimenter 
    = SimpleQueue (SQ.Seq Int) (SQ.Seq Int) 
    | StandardSet (SS.Set Int) (SS.Set Int) 
    | AssocList (AL.FM Int Int) (AL.FM Int Int) 
    | MinHeap (MyMinHeap Int) (MyMinHeap Int)
    | StandardMap (SM.FM Int Int) (SM.FM Int Int)
    | UnbalancedSet (US.Set Int) (US.Set Int)
    | SplayHeap (S.Heap Int) (S.Heap Int)

-- We need to force SimpleQueue evaluation to execute experiments N times
instance (NFData a) => NFData (SQ.Seq a) where
    rnf s = SQ.strictWith rnf s `seq` ()

instance (NFData v) => NFData (AL.FM k v) where
    rnf al = AL.strictWith rnf al `seq` ()

instance (NFData a, Ord a) => NFData (MH.Min (S.Heap a) a) where
    rnf mh = MH.strictWith rnf mh `seq` ()

instance (NFData a) => NFData (US.Set a) where
    rnf us = US.strictWith rnf us `seq` ()

instance (NFData sh) => NFData (S.Heap sh) where
    rnf us = S.strictWith rnf us `seq` ()

-- Intermediate results are set in the base data structure in SimpleSeq, so need to force it
instance NFData Experimenter where
    rnf (SimpleQueue ds t) = rnf ds `seq` rnf t `seq` ()
    rnf (StandardSet ds t) = rnf ds `seq` rnf t `seq` ()
    rnf (AssocList ds t) = rnf ds `seq` rnf t `seq` ()
    rnf (MinHeap ds t) = rnf ds `seq` rnf t `seq` ()
    rnf (StandardMap ds t) = rnf ds `seq` rnf t `seq` ()
    rnf (UnbalancedSet ds t) = rnf ds `seq` rnf t `seq` ()
    rnf (SplayHeap ds t) = rnf ds `seq` rnf t `seq` ()

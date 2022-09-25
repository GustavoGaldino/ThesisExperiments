module Commons (
    ExperimentFunction,
    Experimenter (..)
) where

import Control.DeepSeq
import qualified Data.Edison.Seq.SimpleQueue as SQ

type ExperimentFunction = Experimenter -> Maybe Int -> Experimenter

data Experimenter = SimpleQueue (SQ.Seq Int) (SQ.Seq Int)

-- We need to force SimpleQueue evaluation to execute experiments N times
instance (NFData a) => NFData (SQ.Seq a) where
    rnf s = SQ.strictWith rnf s `seq` ()

-- Intermediate results are set in the base data structure in SimpleSeq, so need to force it
instance NFData Experimenter where
    rnf (SimpleQueue ds _) = rnf ds

import System.Environment
import Control.DeepSeq

import qualified Data.Edison.Seq.SimpleQueue as S

data DataStructure = Seq deriving Show
data Experiment = Add deriving Show

dataStructureAndExperimentToExperimentFunction :: DataStructure -> Experiment -> IO ()
dataStructureAndExperimentToExperimentFunction Seq Add = seqAddExperiment

rawStringToDataStructure :: String -> DataStructure
rawStringToDataStructure "Seq" = Seq
rawStringToDataStructure _ = error "Could not match any data structure"

rawStringToExperiment :: String -> Experiment
rawStringToExperiment "Add" = Add
rawStringToExperiment _ = error "Could not match any experiment"

baseNElems :: Int
baseNElems = 100000

addFromNElems :: Int
addFromNElems = 100000

addEnvSetup :: S.Seq Int
addEnvSetup = defaultEnv

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

addNDistinctFrom :: S.Seq Int -> Int -> Int -> S.Seq Int
addNDistinctFrom seq 0 _ = seq
addNDistinctFrom seq n m =
    let
        elemToAdd =  m + n - 1
        nextNumber = n - 1
        cons = if even n then S.rcons else S.lcons
    in
        addNDistinctFrom ( elemToAdd `cons` seq ) nextNumber m

defaultEnv :: S.Seq Int
defaultEnv = addNDistinctFrom S.empty baseNElems 0


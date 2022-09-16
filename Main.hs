import System.Environment

import qualified Data.Edison.Seq.SimpleQueue as S


data EdisonDataStructure = EdisonSeq deriving Show
data EdisonExperiment = Add deriving Show

dataStructureAndExperimentToExperimentFunction :: EdisonDataStructure -> EdisonExperiment -> IO ()
dataStructureAndExperimentToExperimentFunction EdisonSeq Add = seqAddExperiment

rawStringToEdisonDataStructure :: String -> EdisonDataStructure
rawStringToEdisonDataStructure "EdisonSeq" = EdisonSeq
rawStringToEdisonDataStructure _ = error "Could not match any edison data structure"

rawStringToEdisonExperiment :: String -> EdisonExperiment
rawStringToEdisonExperiment "Add" = Add
rawStringToEdisonExperiment _ = error "Could not match any edison experiment"

baseNElems :: Int
baseNElems = 100000

addFromNElems :: Int
addFromNElems = 100000

addEnvSetup :: S.Seq Int
addEnvSetup = defaultEnv

main :: IO ()
main = do
    args <- getArgs
    let ds = rawStringToEdisonDataStructure $ head args
    let experiment = rawStringToEdisonExperiment $ args !! 1
    seq (dataStructureAndExperimentToExperimentFunction ds experiment) (return ())
    --return (S.lhead $ temp upper) >>= putStrLn . show

seqAddExperiment :: IO ()
seqAddExperiment = benchmark `seq` return ()
    where
        ds = addEnvSetup
        benchmark = addNDistinctFrom ds addFromNElems 0

temp :: Int -> S.Seq Int
temp v = foldl (\set val -> S.lcons val set) S.empty [1..v]

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



import qualified Data.Edison.Seq.SimpleQueue as S
import System.Environment

baseNElems :: Int
baseNElems = 100000000

addFromNElems :: Int
addFromNElems = 100000000

addEnvSetup :: S.Seq Int
addEnvSetup = defaultEnv

main2 :: IO ()
main2 = do
    upperl <- map (\v -> read v :: Int) `fmap` getArgs
    let upper = head upperl
    return (S.lhead $ temp upper) >>= putStrLn . show

main :: IO ()
main = benchmark `seq` return ()
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

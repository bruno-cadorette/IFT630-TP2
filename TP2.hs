import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Control.Parallel.Strategies
import Criterion.Main
import Data.List(foldl')
import Control.Exception
import Data.Maybe (fromMaybe)
import Data.Either (either)


type MapOccurence = Map.Map BS.ByteString Int

sequentiel::[BS.ByteString] -> MapOccurence
sequentiel = Map.unionsWith (+) . map count
    where
        count = foldl'(\acc x-> f x acc) Map.empty . filter (\x->BS.length x > 5) . BS.words
        --le param de la fct est du type Maybe b. Si la clef n'est pas la, on l'initialise a 0
        f = Map.alter(\x-> Just ( 1 + (fromMaybe 0 x)))

mapReducePar::Int -> [BS.ByteString] -> MapOccurence
mapReducePar tailleChunk files =
    if (length files) <= tailleChunk then
        sequentiel files
    else 
    runEval $ do
        let (g, d) = splitAt ((length files) `quot` 2) files
        rg <- rpar $ mapReducePar tailleChunk g
        rd <- rpar $ mapReducePar tailleChunk d
        return $ Map.unionWith (+) rg rd
        
benchmark ::Int -> [BS.ByteString] -> IO ()
benchmark n xs =         
    defaultMain [bgroup "main" construireTests ]
    where
        construireTests =
            let 
                chunkToTest = [2^i|i<-[1..(truncate  $ logBase 2 $ fromIntegral  n)]]
                benchSeq =  [bench "seq" $ whnf sequentiel xs]
                benchPar = map (\i-> bench ("par " ++ show i ) $ whnf (mapReducePar i) xs) chunkToTest
            in benchSeq ++ benchPar

testIndividuel::([BS.ByteString]->MapOccurence)->[BS.ByteString]->IO(MapOccurence)
testIndividuel func xs = do
    try (return $! func xs) >>= either afficherErreur afficherResultat
    where
    
        afficherResultat dict = do
            let (k,v) = Map.findMax dict
            putStrLn $ "Le mot le plus utilisé est " ++ show k ++ " avec " ++ show v ++ " occurences. Les résultats peuvent être visionné dans le fichier \"resultat.txt\""
            writeFile "resultat.txt" (show dict)
            return dict
            
        afficherErreur::SomeException->IO MapOccurence
        afficherErreur erreur = do
            putStrLn $ "Il y a eu une erreur lors de l'execution, voici le message d'erreur : " ++ show erreur
            return Map.empty
    
comparatifResultats xs = do
    seq <- testIndividuel sequentiel xs
    par <- testIndividuel (mapReducePar 32) xs
    putStrLn $ if seq == par then 
        "La version paralelle donne le même résultat que la version séquentiel." 
    else
        "La version paralelle n'est pas égal a la version séquentiel :("
main :: IO()
main = do
    xs <- mapM BS.readFile files
    comparatifResultats xs
    where
        n = 1000
        files = replicate n "test.txt"
        
        

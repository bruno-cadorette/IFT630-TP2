import Data.List
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent.Async
import Data.Either(partitionEithers)
import Criterion.Main
    
compterMots = Map.unionsWith (+) . map compter
    where 
        compter = Map.fromList . map (\x->(head x, length x)) . group . sort . filter (\x->BS.length x > 5) . BS.words
separerEn _ [] = []
separerEn n xs = 
    let (x,reste) = splitAt n xs
    in x:separerEn n reste
delegerTache _ [] = do return Map.empty
delegerTache n fichiers = do
    let xs = separerEn n fichiers
    th <- mapM (\x->async $ do return $ compterMots x ) $ xs
    (errors, results) <- fmap partitionEithers $ mapM waitCatch th
    mapM_ (appendFile "error.txt" . show) errors
    return $ Map.unionsWith (+) results

exep (Left error) = do 
    appendFile "error.txt" $ show error
    return Map.empty
exep (Right a) = do return a    

arbre n fichiers = do
    let len = length fichiers
    if len >= n then do
        return $! compterMots fichiers
    else do
        let (g, d) = splitAt (len `quot` 2) fichiers
        th1 <- async(arbre n g)
        th2 <- async(arbre n d)
         
        rg <- waitCatch th1 >>= exep --gestion d'erreur
        rd <- waitCatch th2 >>= exep
        return $! Map.unionWith (+) rg rd
        
buildTest n xs =
    let 
        chunkATenter = [2^i|i<-[1..(truncate  $ logBase 2 $ fromIntegral  n)]]
        benchSeq =  [bench "seq" $ whnf compterMots xs]
        --benchPar = (map (\i-> bench ("par " ++ show i ) $ whnfIO $ delegerTache i xs) chunkATenter)
        benchArbre = (map (\i-> bench ("arbre " ++ show i ) $ whnfIO $ arbre i xs) chunkATenter)
        
    in benchSeq ++ benchArbre
    
main = do
    xs <- mapM BS.readFile fichiers
    defaultMain [bgroup "main" $ buildTest n $! xs ]
    where
        n = 100
        fichiers = replicate n "test.txt"
        

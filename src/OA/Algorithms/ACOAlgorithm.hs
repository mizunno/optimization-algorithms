module OA.Algorithms.ACOAlgorithm(
    runACO,
    ACOInfo
) where

import           Control.Monad
import           Control.Monad.State
import           OA.Core.ProblemACO
import           OA.Utils.RandState
import           System.Random

--evalState (acoAlgorithm gh acoInfo (\edges -> sum (map (weight) edges))) (mkStdGen 1)
acoAlgorithm :: Graph -> ACOInfo -> ([Edge] -> Double) -> RandState [Edge]
acoAlgorithm g@(Graph n e) (ACOInfo na a b ph ef ite) f = replicateM na (initialAnt n) >>= go e ite
    where
        go _ 0 ants = return $ getBestTour ants
        go edges ite ants = do
            let antsWithTour = map (makeTour a b (Graph n edges)) ants
            let updatedEdges = map (pheromoneUpdate f . tour) antsWithTour
            let evaporatedEdges = concatMap (evaporation ef) updatedEdges
            go evaporatedEdges (ite-1) antsWithTour


--let acoInfo = (ACOInfo 2 1 1 1.0 0.5 10)
data ACOInfo = ACOInfo {
    numAnts           :: Int,
    alpha             :: Integer,
    beta              :: Integer,
    ph                :: Double,
    evaporationFactor :: Double,
    iteraciones       :: Int
}

runACO graph acoinfo f = do
    g <- getStdGen
    let best = evalState (acoAlgorithm graph acoinfo f) g
    return best

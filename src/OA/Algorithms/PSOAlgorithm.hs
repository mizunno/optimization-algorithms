module OA.Algorithms.PSOAlgorithm (
    runPSO,
    PSOInfo (..)
) where

import           Control.Monad.State
import           OA.Core.ProblemPSO
import           OA.Utils.RandState
import           System.Random

---------------------------------
-- Particle Swarm Optimization --
---------------------------------

psoAlgorithm :: PSOInfo -> ([Double] -> Double) -> RandState [Double]
psoAlgorithm (PSOInfo dim b@(inf,sup) num vMax ite) f = initialSwarm num dim b vMax >>= go ite (randomDoubleList dim b)
    where
        go 0 gbest _ = gbest
        go ite gbest swarm = do
            _gbest <- gbest
            let swarm' = map (updateLocalBest f) swarm
            let gbest' = updateGlobalBest f _gbest swarm'
            swarm'' <- mapM (updateVelocity b vMax gbest' f) swarm'
            go (ite-1) (return gbest') swarm''

data PSOInfo = PSOInfo {
    dimension    :: Int,
    bounds       :: (Double,Double),
    numParticles :: Int,
    vMax         :: Double,
    iterations   :: Int
}

runPSO psoinfo f = do
    g <- getStdGen
    let best = evalState (psoAlgorithm psoinfo f) g
    return best

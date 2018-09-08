module OA.Algorithms.PSOAlgorithm (
    psoAlgorithm,
    PSOInfo (..)
) where

import           Control.Monad
import           OA.Core.ProblemPSO
import           OA.Utils.RandState

---------------------------------
-- Particle Swarm Optimization --
---------------------------------

psoAlgorithm :: PSOInfo -> Int -> ([Double] -> Double) -> RandState [Double]
psoAlgorithm (PSOInfo dim b@(inf,sup) num vMax) ite f = initialSwarm num dim b vMax >>= go ite (replicate dim 0)
    where
        go 0 gbest _ = return gbest
        go ite gbest swarm = do
            let swarm' = map (updateLocalBest f) swarm
            let gbest' = updateGlobalBest f gbest swarm'
            swarm'' <- mapM (updateVelocity b vMax gbest' f) swarm'
            go (ite-1) gbest' swarm''

data PSOInfo = PSOInfo {
    dimension    :: Int,
    bounds       :: (Double,Double),
    numParticles :: Int,
    vMax         :: Double
}

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
psoAlgorithm (PSOInfo dim b@(inf,sup) num vMax) ite f = initialSwarm num dim b >>= (\swarm -> go ite (replicate dim 0) swarm)
    where
        go 0 best _ = return best
        go ite best swarm = do
            let swarm' = map (updateBestLocal f) swarm
            let best' = updateBestGlobal f best swarm'
            swarm'' <- mapM (updateVelocity best' f) swarm'
            go (ite-1) best' swarm''

data PSOInfo = PSOInfo {
    dimension    :: Int,
    bounds       :: (Double,Double),
    numParticles :: Int,
    vMax         :: Double
}

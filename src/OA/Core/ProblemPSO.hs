{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OA.Core.ProblemPSO(
    Particle(..),
    initialSwarm,
    updateBestLocal,
    updateBestGlobal,
    updateVelocity,
    fixVelocity
) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Vector
import           OA.Utils.RandState
import           OA.Utils.Utils
import           System.Random

type Swarm = [Particle]

data Particle = Particle {
    currentPosition :: [Double],
    bestPosition    :: [Double],
    velocity        :: [Double]
} deriving (Show)

-- |Generate the initial swarm with 'n' particles, of 'd' dimension and bounded by 'b' (inf,sup)
initialSwarm :: Int -> Int -> (Double,Double) -> Double -> RandState Swarm
initialSwarm n d b vMax = replicateM n $ randomParticle d b vMax

-- |Auxiliary function to construct a random particle
randomParticle :: Int -> (Double,Double) -> Double -> RandState Particle
randomParticle d b vMax = do
    c <- randomDoubleList d b
    v <- randomDoubleList d (-vMax,vMax)
    return $ Particle c c v

allCurrentPositions :: Swarm -> [[Double]]
allCurrentPositions = map currentPosition

allBestsPositions :: Swarm -> [[Double]]
allBestsPositions = map bestPosition

allVelocities :: Swarm -> [[Double]]
allVelocities = map velocity

-- |Evaluate all the swarm in 'f'
evalFitnessSwarm :: ([Double] -> Double) -> Swarm -> [Double]
evalFitnessSwarm f swarm = map f $ allCurrentPositions swarm

-- |Evaluate a particle in 'f'
evalFitness :: ([Double] -> Double) -> Particle -> Double
evalFitness f particle = f $ currentPosition particle

-- |Update the previous best position of each particle
updateBestLocal :: Ord a => ([Double] -> a) -> Particle -> Particle
updateBestLocal f p@(Particle c b v) = if f c > f b then Particle c c v else p

-- |Update the global best position visited so far
updateBestGlobal :: (Ord b, Num b) => ([Double] -> b) -> [Double] -> Swarm -> [Double]
updateBestGlobal f best swarm = if f best' > f best then best' else best
    where best' = argMax (allBestsPositions swarm) f

-- |Update the velocity of a particle
updateVelocity :: (Double,Double) -> Double -> [Double] -> ([Double] -> Double) -> Particle -> RandState Particle
updateVelocity bound vMax pbest f p@(Particle c b v) = do
    r1 <- randomRangeD (0,1)
    r2 <- randomRangeD (0,1)
    w <- randomInertiaWeight
    let c1 = (2 * evalFitness f p) / (evalFitness f p + f pbest)
    let c2 = (2 * f pbest) / (evalFitness f p + f pbest)
    let v' = (w |.| v) |+| ((r1*c1) |.| (pbest |-| c)) |+| ((r2*c2) |.| (b |-| c))
    let c' = c |+| v'
    return $ Particle (fixPosition bound c') b (fixVelocity vMax v')

-- |Fix velocity if exceed the limit vMax
fixVelocity :: Double -> [Double] -> [Double]
fixVelocity vMax = map (flower . fupper)
    where fupper vi = if vi > vMax then vMax else vi
          flower vi = if vi < -vMax then -vMax else vi

-- |Restrict the search space
fixPosition :: (Double,Double) -> [Double] -> [Double]
fixPosition (inf,sup) = map (flower . fupper)
    where fupper xi = if xi > sup then sup else xi
          flower xi = if xi < inf then inf else xi

-- |Random inertia weight used
randomInertiaWeight :: RandState Double
randomInertiaWeight = randomRangeD (0,1)

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OA.Core.ProblemPSO(
    Particle(..)
) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Vector
import           OA.Utils.RandState
import           OA.Utils.Utils
import           System.Random

--psoAlgorithm :: PSOInfo -> Int -> ([Double] -> Double) -> RandState Particle
psoAlgorithm (PSOInfo dim b@(inf,sup) num vMax) ite f = initialSwarm num dim b >>= (\swarm -> go ite [1,1] swarm)
    where
        go 0 best _ = return best
        go ite best swarm = do
            let swarm2 = map (updateBestLocal f) swarm
            let best2 = updateBestGlobal f best swarm2
            swarm3 <- mapM (updateVelocity best2 f) swarm2
            go (ite-1) best2 swarm3



type Swarm = [Particle]

data PSOInfo = PSOInfo {
    dimension    :: Int,
    bounds       :: (Double,Double),
    numParticles :: Int,
    vMax         :: Double
}

data Particle = Particle {
    currentPosition :: [Double],
    bestPosition    :: [Double],
    velocity        :: [Double]
} deriving (Show)

initialSwarm :: Int -> Int -> (Double,Double) -> RandState Swarm
initialSwarm n d b = replicateM n $ randomParticle d b

randomParticle :: Int -> (Double,Double) -> RandState Particle
randomParticle d b = do
    c <- randomDoubleList d b
    v <- randomDoubleList d b
    return $ Particle c c v

f [x,y] = -(10*2 + x^2 + y^2 - 10*cos(2*pi*x) - 10*cos(2*pi*y))

easomFunction [x,y] = -(-cos(x)*cos(y)*exp(-(x-pi)^2+(y-pi)^2))

allCurrentPositions :: Swarm -> [[Double]]
allCurrentPositions swarm = map currentPosition swarm

allBestsPositions :: Swarm -> [[Double]]
allBestsPositions swarm = map bestPosition swarm

allVelocities :: Swarm -> [[Double]]
allVelocities swarm = map velocity swarm

evalFitnessSwarm :: ([Double] -> Double) -> Swarm -> [Double]
evalFitnessSwarm f swarm = map f $ allCurrentPositions swarm

evalFitness :: ([Double] -> Double) -> Particle -> Double
evalFitness f particle = f $ currentPosition particle

updateBestLocal :: Ord a => ([Double] -> a) -> Particle -> Particle
updateBestLocal f p@(Particle c b v) = if f c > f b then Particle c c v else p

updateBestGlobal :: (Ord b, Num b) => ([Double] -> b) -> [Double] -> Swarm -> [Double]
updateBestGlobal f best swarm = if f best' > f best then best' else best
    where best' = argMax (allBestsPositions swarm) f


updateVelocity pbest f p@(Particle c b v) = do
    r1 <- randomRangeD (0,1)
    r2 <- randomRangeD (0,1)
    let c1 = (2 * evalFitness f p) / (evalFitness f p + f pbest)
    let c2 = (2 * f pbest) / (evalFitness f p + f pbest)
    let v' = v |+| ((r1*c1) |.| (pbest |-| c)) |+| ((r2*c2) |.| (b |-| c))
    return $ Particle (c |+| v') b v'

(|-|) :: [Double] -> [Double] -> [Double]
x |-| y = zipWith (-) x y

(|+|) :: [Double] -> [Double] -> [Double]
x |+| y = zipWith (+) x y

(|*|) :: [Double] -> [Double] -> [Double]
x |*| y = zipWith (*) x y

(|.|) :: Double -> [Double] -> [Double]
x |.| vector = map (*x) vector

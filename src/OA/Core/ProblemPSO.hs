{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OA.Core.ProblemPSO(
    Particle(..)
) where

import           Control.Monad
import           Control.Monad.State
import           OA.Utils.RandState
import           System.Random

-- class ProblemPSO f where

--     initialParticles :: f -> Int -> Int -> (Double,Double) -> RandState [Particle]

data PSOInfo = PSOInfo {
    dimension    :: Int,
    bounds       :: (Double,Double),
    numParticles :: Int
}

data Particle = Particle {
    currentPosition :: [Double],
    bestPosition    :: [Double],
    velocity        :: [Double]
} deriving (Show)

initialParticles :: Int -> Int -> (Double,Double) -> RandState [Particle]
initialParticles n d b = replicateM n $ randomParticle d b

randomParticle :: Int -> (Double,Double) -> RandState Particle
randomParticle d b = do
    c <- randomDoubleList d b
    v <- randomDoubleList d b
    return $ Particle c c v

-- evalFitness :: (Double -> Double) -> Swarm -> Double
-- evalFitness f swarm = mapM f swarm

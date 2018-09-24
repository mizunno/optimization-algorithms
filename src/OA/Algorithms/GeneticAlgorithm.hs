module OA.Algorithms.GeneticAlgorithm (
    runGA,
    GAInfo (..)
) where

import           Control.Monad.State
import           OA.Core.ProblemGA
import           OA.Utils.RandState
import           OA.Utils.Utils
import           System.Random

-----------------------
-- Genetic Algorithm --
-----------------------

-- |Genetic Algorithm
geneticAlgorithm :: (ProblemGA p s) => p s -> GAInfo -> RandState [s]
geneticAlgorithm prob (GAInfo pSize mutRate gen fitLB) = initialPopulation prob pSize >>= evolve mutRate gen
    where
        evolve _ 0 pop = return pop
        evolve mutRate gen pop = do
                pops <- selection prob pop
                popc <- crossover prob pops
                popm <- mutation prob popc mutRate
                let best = argMax popm (fitnessGA prob)
                let value = fitnessGA prob best
                if value >= fitLB then
                    evolve mutRate 0 popm
                else
                    evolve mutRate (gen-1) popm


data GAInfo = GAInfo {
    populationSize    :: Int,
    mutationRate      :: Double,
    generations       :: Int,
    fitnessLowerBound :: Double
}

runGA prob gainfo = do
    g <- getStdGen
    let pop = evalState (geneticAlgorithm prob gainfo) g
    let best = argMax pop (fitnessGA prob)
    let value = fitnessGA prob best
    return (best,value)

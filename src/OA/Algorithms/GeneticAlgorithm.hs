module OA.Algorithms.GeneticAlgorithm (
    geneticAlgorithm,
    GAInfo (..)
) where

import           OA.Core.ProblemGA
import           OA.Utils.RandState
import           OA.Utils.Utils

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

module OA.Algorithms.GeneticAlgorithm (
    geneticAlgorithm,
    GAInfo (..)
) where

import OA.Core.ProblemGA
import OA.Utils.Utils
import OA.Utils.RandState

-----------------------
-- Genetic Algorithm --
-----------------------

-- |Genetic Algorithm
geneticAlgorithm :: (ProblemGA p s) => p s -> GAInfo -> RandState [s]
geneticAlgorithm prob (GAInfo pSize mutRate gen fitLB)= evolve mutRate gen fitLB (initialPopulation prob pSize)
    where
        evolve _ 0 _ pop = pop
        evolve mutRate gen fitLB pop = do
                pop <- pop
                pops <- selection prob pop
                popc <- crossover prob pops
                popm <- mutation prob popc mutRate
                let best = argMax popm (fitnessGA prob)
                let value = fitnessGA prob best
                if value >= fitLB then
                    evolve mutRate 0 fitLB (return popm)
                else
                    evolve mutRate (gen-1) fitLB (return popm) 


data GAInfo = GAInfo {
    populationSize :: Int,
    mutationRate :: Double,
    generations :: Int,
    fitnessLowerBound :: Double
}
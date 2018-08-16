module OA.Algorithms.GeneticAlgorithm (
    geneticAlgorithm
) where

import OA.Core.ProblemGA
import OA.Utils.Utils
import OA.Utils.RandState
import Control.Monad.State
import Control.Monad

--geneticAlgorithm = undefined

geneticAlgorithm :: (ProblemGA p s) => p s -> Double -> Int -> RandState [s]
geneticAlgorithm prob mutationRate gen = evolve mutationRate gen (initialPopulation prob)
    where
        evolve _ 0 pop = pop
        evolve mutationRate gen pop = do
                pop <- pop
                pops <- selection prob pop
                popc <- crossover prob pops
                popm <- mutation prob popc mutationRate
                evolve mutationRate (gen-1) (return popm)
            --where best = argMax (pop) (fitness prob)

            --(best, fitness prob best)
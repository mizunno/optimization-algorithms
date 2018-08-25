{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module OA.Core.ProblemGA(
    ProblemGA (..)
) where

import OA.Utils.RandState

class ProblemGA p s where

    -- |Initial population
    initialPopulation :: p s -> Int -> RandState [s]

    -- |Selection method
    selection :: p s -> [s] -> RandState [s]

    -- |Crossover method
    crossover :: p s -> [s] -> RandState [s]
    
    -- |Mutation method
    mutation :: p s -> [s] -> Double -> RandState [s]

    -- |Function to evaluate a given solution
    fitnessGA :: p s -> s -> Double


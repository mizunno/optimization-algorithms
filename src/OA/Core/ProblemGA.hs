{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module OA.Core.ProblemGA(
    ProblemGA (..)
) where

class ProblemGA p s where

    initialPopulation :: p s -> [s]

    selection :: p s -> [s] -> [s]

    crossover :: p s -> s -> s -> [s]

    mutation :: p s -> s -> Double -> s

    fitness :: p s -> s -> Double


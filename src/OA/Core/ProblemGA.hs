{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module OA.Core.ProblemGA(
    ProblemGA (..)
) where

import OA.Utils.RandState

class ProblemGA p s where

    initialPopulation :: p s -> RandState [s]

    selection :: p s -> [s] -> RandState [s]

    crossover :: p s -> [s] -> RandState [s]

    mutation :: p s -> [s] -> Double -> RandState [s]

    fitnessGA :: p s -> s -> Double


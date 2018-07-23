{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module OA.Algorithms.HillClimbing (
    hillClimbing
) where

import OA.Core.Problem
import OA.Utils.Utils

-----------------------------
-- Hill Climbing algorithm --
-----------------------------

-- |Hill Climbing algorithm
hillClimbing :: (Problem p s) => p s -> (s,Double)
hillClimbing problem = climb $ initial problem
            where 
                climb solution = if valueNeighbour <= valueSolution
                    then (solution,valueSolution)
                    else climb neighbour
                    where
                        valueNeighbour = fitness problem neighbour
                        valueSolution = fitness problem solution
                        neighbour = argMax (neighborhood problem solution) (fitness problem)
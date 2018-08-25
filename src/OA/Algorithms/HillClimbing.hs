{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module OA.Algorithms.HillClimbing (
    hillClimbing,
) where

import OA.Core.Problem
import OA.Utils.Utils
import OA.Utils.RandState

-----------------------------
-- Hill Climbing algorithm --
-----------------------------

-- |Hill Climbing algorithm
hillClimbing :: (Problem p s) => p s -> RandState s
hillClimbing problem = initial problem >>= (\ini -> climb ini)
            where 
                climb solution = if valueNeighbour <= valueSolution
                    then return solution
                    else climb neighbour
                    where
                        valueNeighbour = fitness problem neighbour
                        valueSolution = fitness problem solution
                        neighbour = argMax (neighborhood problem solution) (fitness problem)
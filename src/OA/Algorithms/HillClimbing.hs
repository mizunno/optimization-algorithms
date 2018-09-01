{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OA.Algorithms.HillClimbing (
    hillClimbing,
) where

import           OA.Core.Problem
import           OA.Utils.RandState
import           OA.Utils.Utils

-----------------------------
-- Hill Climbing algorithm --
-----------------------------

-- |Hill Climbing algorithm
hillClimbing :: (Problem p s) => p s -> RandState s
hillClimbing problem = initial problem >>= climb
            where
                climb solution = if valueNeighbour <= valueSolution
                    then return solution
                    else climb neighbour
                    where
                        valueNeighbour = fitness problem neighbour
                        valueSolution = fitness problem solution
                        neighbour = argMax (neighborhood problem solution) (fitness problem)

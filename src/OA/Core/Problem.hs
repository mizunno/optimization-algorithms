{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module OA.Core.Problem (
    Problem (..)
) where


{- 
Typeclass for a problem:
    - 'p' represents the problem
    - 's' represents a solution
At this moment, this implementation its thinking to use with Hill Climbing. But
soon will be an abstract class to use with other algorithms.
-}

class Problem p s where

    -- |Initial solution
    initial :: p s -> s

    -- |Function that evaluate a given solution
    fitness :: p s -> s -> Double

    -- |Returns a list with solutions that can be reached from 's'. It
    -- needs a function to tweak the current solution.
    neighborhood :: p s -> s -> [s]
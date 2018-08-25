{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module OA.Core.Problem (
    Problem (..)
) where

import OA.Utils.RandState

class Problem p s where

    -- |Initial solution
    initial :: p s -> RandState s

    -- |Function that evaluate a given solution
    fitness :: p s -> s -> Double

    -- |Returns a list with solutions that can be reached from 's'
    neighborhood :: p s -> s -> [s]

    -- |Function to update temperature. Only with 
    tempUpdate :: (Fractional a) => p s -> a -> Int -> a
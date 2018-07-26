{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module OA.Algorithms.SimulatedAnnealing (
    simulatedAnnealing
) where

import OA.Core.Problem
import OA.Utils.Utils
import OA.Utils.RandState

----------------------------------
-- Simulate Annealing algorithm --
----------------------------------

{- TODO:
    - Re-implement with RandState monad
-}

-- |Simulated Annealing algorithm
simulatedAnnealing' :: (Problem p s) => p s -> Int -> Double -> IO (s,Double)
simulatedAnnealing' prob ite temp = cool ite ite temp (initial prob)
    where
        cool ite0 ite temp current = let t' = temperatureUpdate temp ite in
            if t' <= 1
                then return (current, fitness prob current)
                else do
                    next <- randomChoiceIO (neighborhood prob current)
                    let valueNext = fitness prob next
                    let valueCurrent = fitness prob current
                    let deltaE = valueNext - valueCurrent
                    chooseBadSolution <- probabilityIO (exp $ deltaE / t')
                    if deltaE > 0 || chooseBadSolution
                        then 
                            if ite == 0 then cool ite0 ite0 t' next
                                        else cool ite0 (ite-1) t' next
                        else 
                            if ite == 0 then cool ite0 ite0 t' current
                                        else cool ite0 (ite-1) t' current


simulatedAnnealing :: (Problem p s) => p s -> Int -> Double -> RandState (s,Double)
simulatedAnnealing prob ite temp = cool ite ite temp (initial prob)
    where
        cool ite0 ite temp current = let t' = temperatureUpdate temp ite in
            if t' <= 1
                then return (current, fitness prob current)
                else do
                    next <- randomChoice' (neighborhood prob current)
                    let valueNext = fitness prob next
                    let valueCurrent = fitness prob current
                    let deltaE = valueNext - valueCurrent
                    chooseBadSolution <- probability (exp $ deltaE / t')
                    if deltaE > 0 || chooseBadSolution
                        then
                            if ite == 0 then cool ite0 ite0 t' next
                                        else cool ite0 (ite-1) t' next
                        else
                            if ite == 0 then cool ite0 ite0 t' current
                                        else cool ite0 (ite-1) t' current

-- |By default, geometric function to update temp
temperatureUpdate :: (Fractional a) => a -> Int -> a
temperatureUpdate t 0 = 0.85 * t
temperatureUpdate t ite = t
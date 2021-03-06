module OA.Algorithms.SimulatedAnnealing (
    runSA,
    SAInfo (..)
) where

import           Control.Monad.State
import           OA.Core.Problem
import           OA.Utils.RandState
import           OA.Utils.Utils
import           System.Random

----------------------------------
-- Simulate Annealing algorithm --
----------------------------------

simulatedAnnealing :: (Problem p s) => p s -> SAInfo -> RandState s
simulatedAnnealing prob (SAInfo ite temp) = initial prob >>= cool ite ite temp
    where
        cool ite0 ite temp current = let t' = tempUpdate prob temp ite in
            if t' <= 1
                then return current
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


data SAInfo = SAInfo {
    iterations  :: Int,
    temperature :: Double
}

runSA prob sainfo = do
    g <- getStdGen
    let best = evalState (simulatedAnnealing prob sainfo) g
    let value = fitness prob best
    return (best,value)

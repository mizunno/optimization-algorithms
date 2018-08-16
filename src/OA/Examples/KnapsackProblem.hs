{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module OA.Examples.KnapsackProblem (
    
) where

import OA.Core.Problem
import OA.Core.ProblemGA
import OA.Utils.Utils
import OA.Utils.Operators
import OA.Algorithms.SimulatedAnnealing
import OA.Algorithms.HillClimbing
import OA.Algorithms.GeneticAlgorithm
import System.Random
import OA.Utils.RandState
import Control.Monad.State

----------------------------------
-- Instance of Knapsack Problem --
----------------------------------

{- 
Data structure that model the problem. Where, 
    - 'size' represents the knap capacity
    - 'numObjects' represents the total number of objects
    - 'weights' contains the weight of each objects
    - 'values' contains the value of each object
-}

data Napsack s = NS { 
    size :: Int, 
    numObjects :: Int,
    weights :: [Int],
    values :: [Int]
    } deriving (Show)

instance ProblemGA Napsack [Int] where

    initialPopulation (NS _ _ _ _) = return popEx

    selection p@(NS _ _ _ _) pop = rouletteWheelSelection pop (fitnessGA p)

    crossover (NS _ _ _ _) pop = crossPopulation pop onePointCrossover

    mutation (NS _ _ _ _) pop mr = mutatePopulation mr pop
    
    fitnessGA (NS size numO weights values) solution = if w <= 15 
        then fromIntegral v 
        else fromIntegral $ v - w * 10
        where
            w = sum [w | (x,w) <- zip solution weights, x == 1]
            v = sum [v | (x,v) <- zip solution values, x == 1]



instance Problem Napsack [Int] where

    -- generate the initial solution randomly
    initial (NS size numO weights values) = randomBinaryList (mkStdGen 927456021) numO

    fitness (NS size numO weights values) solution = if w <= 15 
        then fromIntegral v 
        else fromIntegral $ v - w * 10
        where
            w = sum [w | (x,w) <- zip solution weights, x == 1]
            v = sum [v | (x,v) <- zip solution values, x == 1]

    neighborhood NS{} solution = [bitFlip solution n | n <- [0..length solution - 1]]

-- List of weights
ws :: [Int]
ws = [5,4,5,3,5,8,5,9]

-- List of values
vs :: [Int]
vs = [3,3,7,2,6,7,5,8]

knapSack :: Napsack [Int]
knapSack = NS 15 8 ws vs

-- Funtions to run an algorithm

runSA = do
    print "Resolving with Simulated Annealing..."
    let (solution,value) = evalState (simulatedAnnealing knapSack 100 100.0) (mkStdGen 1)
    print $ "Solution: " ++ show solution
    print $ "Fitness value: " ++ show value

runHC = do 
    print "Resolving with Hill Climbing..."
    let (solution,value) = hillClimbing knapSack
    print $ "Solution: " ++ show solution
    print $ "Fitness value: " ++ show value

runGA = do
    print "Resolving with Genetic Algorithm..."
    let pop = evalState (geneticAlgorithm knapSack 0.5 100) (mkStdGen 1)
    print $ pop
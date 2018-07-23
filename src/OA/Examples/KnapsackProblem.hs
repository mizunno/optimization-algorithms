{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module OA.Examples.KnapsackProblem (
    
) where

import OA.Core.Problem
import OA.Utils.Utils
import OA.Utils.Operators
import OA.Algorithms.SimulatedAnnealing
import OA.Algorithms.HillClimbing
import System.Random

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
    values :: [Int]} 
    deriving (Show)

instance Problem Napsack [Int] where

    -- generate the initial solution randomly
    initial (NS size numO weights values) = randomBinaryList (mkStdGen 927456021) numO

    fitness (NS size numO weights values) solution = if w <= 15 
        then fromIntegral $ v 
        else fromIntegral $ v - w * 10
        where
            w = sum $ [w | (x,w) <- zip solution weights, x == 1]
            v = sum $ [v | (x,v) <- zip solution values, x == 1]

    neighborhood (NS _ _ _ _) solution = [bitFlip solution n | n <- [0..(length solution)-1]]

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
    putStrLn ""
    putStrLn "Resolving with Simulated Annealing..."
    (solution,value) <- simulatedAnnealing knapSack 100 100.0
    putStrLn $ show $ "Solution: " ++ show solution
    putStrLn $ show $ "Fitness value: " ++ show value
    putStrLn ""

runHC = do 
    putStrLn ""
    putStrLn "Resolving with Hill Climbing..."
    let (solution,value) = hillClimbing knapSack
    putStrLn $ show $ "Solution: " ++ show solution
    putStrLn $ show $ "Fitness value: " ++ show value
    putStrLn ""
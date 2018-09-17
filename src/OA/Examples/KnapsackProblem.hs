{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OA.Examples.KnapsackProblem (
    runGA,
    runSA,
    runHC
) where

import           Control.Monad.State
import           OA.Algorithms.GeneticAlgorithm
import           OA.Algorithms.HillClimbing
import           OA.Algorithms.SimulatedAnnealing
import           OA.Core.Problem
import           OA.Core.ProblemGA
import           OA.Utils.Operators
import           OA.Utils.RandState
import           OA.Utils.Utils
import           System.Random

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
    size       :: Int,
    numObjects :: Int,
    weights    :: [Int],
    values     :: [Int]
    } deriving (Show)

instance ProblemGA Napsack [Int] where

    initialPopulation (NS _ numO _ _) pSize = replicateM pSize $ randomBinaryList numO
    selection p@NS{} pop = tournamentSelection pop (fitnessGA p) 2
    crossover NS{} pop = crossPopulation pop onePointCrossover
    mutation NS{} pop mr = mutatePopulation mr pop
    fitnessGA (NS size numO weights values) solution = fromIntegral $ if w > size then v-w*10 else v
        where
            v = sum $ zipWith (*) solution values
            w = sum $ zipWith (*) solution weights


instance Problem Napsack [Int] where

    initial (NS _ numO _ _) = randomBinaryList numO

    fitness (NS size numO weights values) solution = fromIntegral $ if w > size then v-w*10 else v
        where
            v = sum $ zipWith (*) solution values
            w = sum $ zipWith (*) solution weights

    neighborhood NS{} solution = concatMap (neig2) $ [bitFlip solution n | n <- [0..length solution - 1]]

    tempUpdate _ t ite = constantUpdate t ite 1

neig2 solution = [bitFlip solution n | n <- [0..length solution - 1]]


-- Funtions to run an algorithm

saInfo :: SAInfo
saInfo = SAInfo 200 100.0

runSA = do
    g <- getStdGen
    print "Resolving with Simulated Annealing..."
    let solution = evalState (simulatedAnnealing p2 saInfo) g
    let value = fitness p2 solution
    print $ "Solution: " ++ show solution
    print $ "Fitness value: " ++ show value

runHC = do
    g <- getStdGen
    print "Resolving with Hill Climbing..."
    let solution = evalState (hillClimbing p3) g
    let value = fitness p3 solution
    print $ "Solution: " ++ show solution
    print $ "Fitness value: " ++ show value

gaInfo :: GAInfo
gaInfo = GAInfo 4096 0.15 100 0

runGA = do
    g <- getStdGen
    print "Resolving with Genetic Algorithm..."
    let pop = evalState (geneticAlgorithm p4 gaInfo) g
    let best = argMax pop (fitnessGA p4)
    let value = fitnessGA p4 best
    print $ "Solution: " ++ show best
    print $ "Fitness value: " ++ show value
    print $ bests == best

p0 :: Napsack [Int]
p0 = NS 15 8 [5,4,5,3,5,8,5,9] [3,3,7,2,6,7,5,8] 

p1 :: Napsack [Int]
p1 = NS 165 10 [23,31,29,44,53,38,63,85,89,82] [92,57,49,68,60,43,67,84,87,72]

p1Solution :: [Int]
p1Solution = [1,1,1,1,0,1,0,0,0,0]

p2 :: Napsack [Int]
p2 = NS 26 5 [12,7,11,8,9] [24,13,23,15,16]

p2Solution = [0,1,1,1,0]

p3 :: Napsack [Int]
p3 = NS 190 6 [56,59,80,64,75,17] [50,50,64,46,50,5]

p3Solution = [1,1,0,0,1,0]


ws' :: [Int]
ws' = [382745,799601,909247,729069,467902,44328,34610,698150,823460,903959,853665,551830,610856,670702,488960,951111,323046,446298,931161,31385,496951,264724,224916,169684]

vs' :: [Int]
vs' = [825594,1677009,1676628,1523970,943972,97426,69666,1296457,1679693,1902996,1844992,1049289,1252836,1319836,953277,2067538,675367,853655,1826027,65731,901489,577243,466257,369261]

bests :: [Int]
bests = [1,1,0,1,1,1,0,0,0,1,1,0,1,0,0,1,0,0,0,0,0,1,1,1]

p4 :: Napsack [Int]
p4 = NS 6404180 (length ws') ws' vs'

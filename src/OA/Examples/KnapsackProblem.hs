{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OA.Examples.KnapsackProblem (
    knapsackGA,
    knapsackSA,
    knapsackHC
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

data Knapsack s = KS {
    size       :: Int,
    numObjects :: Int,
    weights    :: [Int],
    values     :: [Int]
} deriving (Show)

instance ProblemGA Knapsack [Int] where

    initialPopulation (KS _ numO _ _) pSize = replicateM pSize $ randomBinaryList numO
    selection p@KS{} pop = tournamentSelection pop (fitnessGA p) 2
    crossover KS{} pop = crossPopulation pop onePointCrossover
    mutation KS{} pop mr = mutatePopulation mr pop
    fitnessGA (KS size numO weights values) solution = fromIntegral $ if w > size then v-w*10 else v
        where
            v = sum $ zipWith (*) solution values
            w = sum $ zipWith (*) solution weights


instance Problem Knapsack [Int] where

    initial (KS _ numO _ _) = randomBinaryList numO

    fitness (KS size numO weights values) solution = fromIntegral $ if w > size then v-w*10 else v
        where
            v = sum $ zipWith (*) solution values
            w = sum $ zipWith (*) solution weights

    neighborhood KS{} solution = concatMap neig2 [bitFlip solution n | n <- [0..length solution - 1]]

    tempUpdate _ t ite = constantUpdate t ite 1

neig2 solution = [bitFlip solution n | n <- [0..length solution - 1]]


-- Funtions to run an algorithm

saInfo :: SAInfo
saInfo = SAInfo 200 100.0

knapsackSA = do
    print "Solving with Simulated Annealing..."
    (solution,value) <- runSA p3 saInfo
    print $ "Solution: " ++ show solution
    print $ "Fitness value: " ++ show value

knapsackHC = do
    print "Solving with Hill Climbing..."
    (solution,value) <- runHC p3
    print $ "Solution: " ++ show solution
    print $ "Fitness value: " ++ show value

gaInfo :: GAInfo
gaInfo = GAInfo 128 0.15 100 18

knapsackGA = do
    print "Solving with Genetic Algorithm..."
    (best,value) <- runGA p4 gaInfo
    print $ "Solution: " ++ show best
    print $ "Fitness value: " ++ show value

p0 :: Knapsack [Int]
p0 = KS 15 8 [5,4,5,3,5,8,5,9] [3,3,7,2,6,7,5,8] 

p1 :: Knapsack [Int]
p1 = KS 165 10 [23,31,29,44,53,38,63,85,89,82] [92,57,49,68,60,43,67,84,87,72]

p1Solution :: [Int]
p1Solution = [1,1,1,1,0,1,0,0,0,0]

p2 :: Knapsack [Int]
p2 = KS 26 5 [12,7,11,8,9] [24,13,23,15,16]

p2Solution = [0,1,1,1,0]

p3 :: Knapsack [Int]
p3 = KS 190 6 [56,59,80,64,75,17] [50,50,64,46,50,5]

p3Solution = [1,1,0,0,1,0]


ws' :: [Int]
ws' = [382745,799601,909247,729069,467902,44328,34610,698150,823460,903959,853665,551830,
        610856,670702,488960,951111,323046,446298,931161,31385,496951,264724,224916,169684]

vs' :: [Int]
vs' = [825594,1677009,1676628,1523970,943972,97426,69666,1296457,1679693,1902996,1844992,1049289,
        1252836,1319836,953277,2067538,675367,853655,1826027,65731,901489,577243,466257,369261]

bests :: [Int]
bests = [1,1,0,1,1,1,0,0,0,1,1,0,1,0,0,1,0,0,0,0,0,1,1,1]

p4 :: Knapsack [Int]
p4 = KS 6404180 (length ws') ws' vs'
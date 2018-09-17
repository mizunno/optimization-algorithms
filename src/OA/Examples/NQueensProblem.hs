{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OA.Examples.NQueensProblem (
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
-- Instance of N-Queens Problem --
----------------------------------

data NQueens s = NQ {
    numQueens :: Int
} deriving (Show)

rowsClashes xs = (length xs) - (length $ uniques xs)

diagClashes xs = length $ filter (== True) [abs(i-j) == abs(q1-q2) | (i,q1) <- zip [1..length xs] xs, (j,q2) <- zip [1..length xs] xs, i /= j]

instance Problem NQueens [Int] where

    initial (NQ numQ) = return $ [0..numQ-1]

    fitness (NQ numQ) solution = fromIntegral $ - ((diagClashes solution) * 10)

    neighborhood (NQ numQ) solution = uniques [twoOpt i j solution | i <- [0..length solution -1 ], j <- [0..length solution - 1]]

    tempUpdate _ t ite = geometricUpdate t ite 0.9



instance ProblemGA NQueens [Int] where

    initialPopulation (NQ numQ) pSize = return $ [[0,1,2,3],[3,2,1,0],[0,1,2,3],[3,2,1,0]]
    selection p@(NQ numQ) pop = tournamentSelection pop (fitnessGA p) 2
    crossover (NQ numQ) pop = crossPopulation pop onePointCrossover
    mutation (NQ numQ) pop mr = mapM (twoOptRWithProb mr) pop
    fitnessGA (NQ numQ) solution = fromIntegral $ - ((diagClashes solution) * 10)
                  

nq :: NQueens [Int]
nq = NQ 4

runHC = do
    g <- getStdGen
    print "Resolving with Hill Climbing..."
    let solution = evalState (hillClimbing nq) g
    let value = fitness nq solution
    print $ "Solution: " ++ show solution
    print $ "Fitness value: " ++ show value

saInfo :: SAInfo
saInfo = SAInfo 100 100.0
    
runSA = do
    g <- getStdGen
    print "Resolving with Simulated Annealing..."
    let solution = evalState (simulatedAnnealing nq saInfo) g
    let value = fitness nq solution
    print $ "Solution: " ++ show solution
    print $ "Fitness value: " ++ show value

gaInfo :: GAInfo
gaInfo = GAInfo 2 0.15 1000 10

runGA = do
    g <- getStdGen
    print "Resolving with Genetic Algorithm..."
    let pop = evalState (geneticAlgorithm nq gaInfo) g
    let best = argMax pop (fitnessGA nq)
    let value = fitnessGA nq best
    print $ "Solution: " ++ show best
    print $ "Fitness value: " ++ show value
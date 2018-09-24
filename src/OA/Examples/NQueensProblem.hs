{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OA.Examples.NQueensProblem (
    nqueensSA,
    nqueensHC,
    nqueensGA
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

newtype NQueens s = NQ {
    numQueens :: Int
} deriving (Show)

rowsClashes xs = length xs - length (uniques xs)

diagClashes xs = length $ filter (== True) [abs(i-j) == abs(q1-q2) | (i,q1) <- zip [1..length xs] xs, (j,q2) <- zip [1..length xs] xs, i /= j]

instance Problem NQueens [Int] where

    initial (NQ numQ) = return [0..numQ-1]

    fitness (NQ numQ) solution = fromIntegral $ - (diagClashes solution * 10)

    neighborhood (NQ numQ) solution = uniques [twoOpt i j solution | i <- [0..length solution -1 ], j <- [0..length solution - 1]]

    tempUpdate _ t ite = geometricUpdate t ite 0.9



instance ProblemGA NQueens [Int] where

    initialPopulation (NQ numQ) pSize = replicateM pSize $ shuffle [0..numQ-1]
    selection p@(NQ numQ) pop = tournamentSelection pop (fitnessGA p) 2
    crossover (NQ numQ) pop = crossPopulation pop onePointCrossover
    mutation (NQ numQ) pop mr = mapM (twoOptRWithProb mr) pop
    fitnessGA (NQ numQ) solution = fromIntegral $ - (diagClashes solution * 10)
                  

nq :: NQueens [Int]
nq = NQ 16

saInfo :: SAInfo
saInfo = SAInfo 100 100.0

gaInfo :: GAInfo
gaInfo = GAInfo 128 0.15 1000 0

nqueensSA = do
    print "Solving with Simulated Annealing..."
    (solution,value) <- runSA nq saInfo
    print $ "Solution: " ++ show solution
    print $ "Fitness value: " ++ show value

nqueensHC = do
    print "Solving with Hill Climbing..."
    (solution,value) <- runHC nq
    print $ "Solution: " ++ show solution
    print $ "Fitness value: " ++ show value

nqueensGA = do
    print "Solving with Genetic Algorithm..."
    (best,value) <- runGA nq gaInfo
    print $ "Solution: " ++ show best
    print $ "Fitness value: " ++ show value
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OA.Examples.TSP (
    tspHC,
    tspGA
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
import           Data.Map as M

---------------------------------------------
-- Instance of Travelling Salesman Problem --
---------------------------------------------

xs = [1,2,3,4,5,6,7,8]

data TSP s = TSP {
    numCities        :: Int,
    cities           :: [Int],
    edgesDistance    :: Map (Int,Int) Int
} deriving (Show)

edges = M.fromList [((1,2),10),((1,7),9),((2,1),10),((2,7),15),((2,3),12),((3,2),12),((3,7),11),((3,4),7),((3,5),19),((4,3),7),((4,6),20),((4,5),16),((5,4),16),((5,3),19),
    ((5,7),12),((5,6),10),((6,5),10),((6,7),5),((6,4),20),((7,1),9),((7,2),15),((7,3),11),((7,5),12),((7,6),5)]

cs = [1,2,3,4,5,6,7]

tsp :: TSP [Int]
tsp = TSP 7 cs edges

getDistances :: TSP [Int] -> [(Int,Int)] -> [Int]
getDistances _ [] = []
getDistances p@(TSP n c ed) (x:xs) = findWithDefault 0 x ed : getDistances p xs

checkIfExists :: TSP [Int] -> [(Int,Int)] -> [Bool]
checkIfExists _ [] = []
checkIfExists p@(TSP n c ed) (x:xs) = M.member x ed : checkIfExists p xs

evalTour :: TSP [Int] -> [Int] -> Int
evalTour p@(TSP n c ed) xs = sum (getDistances p (getEdges xs xs)) + penalty
    where penalty = if all (==True) (checkIfExists p (getEdges xs xs)) then 0 else 10^3

instance ProblemGA TSP [Int] where

    initialPopulation (TSP numCities _ _) pSize = replicateM pSize $ shuffle [1..7]
    selection p@TSP{} pop = fittestSelection pop (fitnessGA p)
    crossover TSP{} pop = crossPopulation pop onePointCrossover
    mutation TSP{} pop mr = mapM (twoOptRWithProb mr) pop
    fitnessGA p@TSP{} solution = fromIntegral $ - evalTour p solution

instance Problem TSP [Int] where

    initial (TSP numCities _ _) = return [1..numCities]

    fitness p@(TSP numCities _ _) solution = fromIntegral $ - evalTour p solution

    neighborhood (TSP numCities _ _) solution = uniques [twoOpt i j solution | i <- [0..length solution -1 ], j <- [0..length solution - 1]]

    tempUpdate _ t ite = geometricUpdate t ite 0.9

gaInfo :: GAInfo
gaInfo = GAInfo 128 0.15 1000 0

getEdges :: [Int] -> [Int] -> [(Int,Int)]
getEdges [x] ys = [(x, head xs)]
getEdges (x:xs) ys = (x, head xs) : getEdges xs ys

tspGA = do
    print "Resolving with Genetic Algorithm..."
    (best,value) <- runGA tsp gaInfo
    print $ "Solution: " ++ show best
    print $ "Fitness value: " ++ show value

tspHC = do
    print "Resolving with Hill Climbing..."
    (best,value) <- runHC tsp
    print $ "Solution: " ++ show best
    print $ "Fitness value: " ++ show value
module OA.Core.ProblemACO(
    Graph (..),
    Node (..),
    Edge (..),
    Ant (..),
    makeTour,
    evaporation,
    pheromoneUpdate,
    initialAnt,
    initializePheromone,
    getBestTour
) where

import           OA.Utils.RandState
import           OA.Utils.Utils

{- eds = [(Edge (Node 1) (Node 2) 1 1), (Edge (Node 2) (Node 1) 1 1)]
nods = [(Node 1), (Node 2)]
gh = Graph nods eds -}

data Graph = Graph {
    nodes :: [Node],
    edges :: [Edge]
} deriving (Show,Eq)

newtype Node = Node {id :: Int} deriving(Show,Eq)

data Edge = Edge {
    nodeA     :: Node,
    nodeB     :: Node,
    weight    :: Double,
    pheromone :: Double
} deriving (Show,Eq)

data Ant = Ant {
    visited           :: [Node],
    tour              :: [Edge],
    distanceTraveled :: Double
} deriving (Show)

probChooseEdge :: [Edge] -> Integer -> Integer -> Edge -> Double
probChooseEdge edges alpha beta e = pheromone e ^ alpha * (1 / weight e)^beta / totalsum
    where totalsum = sum [pheromone ij ^ alpha * weight ij ^ beta | ij <- edges]

deleteNode :: Node -> [Node] -> [Node]
deleteNode node = filter (/= node)

evaporation :: Double -> [Edge] -> [Edge]
evaporation p = map (\e -> Edge (nodeA e) (nodeB e) (weight e) ((1-p)*pheromone e))

pheromoneUpdate :: ([Edge] -> Double) -> [Edge] -> [Edge]
pheromoneUpdate f edges = map (\e -> Edge (nodeA e) (nodeB e) (weight e) (pheromone e + 1/f edges)) edges

expand :: Node -> Ant -> [Edge] -> [Edge]
expand v ant = filter (\e -> nodeA e == v && notElem (nodeB e) (visited ant))

initializePheromone :: Double -> [Edge] -> [Edge]
initializePheromone ph = map (\e -> Edge (nodeA e) (nodeB e) (weight e) ph)

initialAnt :: [Node] -> RandState Ant
initialAnt nodes = do
    n <- randomChoice' nodes
    return (Ant [n] [] 0)

--makeTour 1 1 gh (Ant [(Node 1)] [] 0)
makeTour :: Integer -> Integer -> Graph -> Ant -> Ant
makeTour a b (Graph n e) ant@(Ant v t dt) = go ant (deleteNode (head v) n) (length n-1)
    where
        go ant nodes 0 = ant
        go ant@(Ant v t dt) nodes ite = do
            let current = last v
            let roads = expand current ant e
            if null roads then
                go ant nodes 0
            else
                do
                    let next = argMax roads (probChooseEdge e a b)
                    let v' = reverse (nodeB next:v)
                    let t' = reverse (next:t)
                    let dt' = dt + weight next
                    go (Ant v' t' dt') (filter (/= head v') n) (ite-1)

getBestTour :: [Ant] -> [Edge]
getBestTour ants = tour $ argMax ants distanceTraveled

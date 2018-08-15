module OA.Utils.Operators (
    bitFlip,
    bitFlipR,
    onePointCrossover
) where

import OA.Utils.Utils
import OA.Utils.RandState
import Data.List as L
import Data.Ord as O
import System.Random
import Control.Monad

type Gen = Int
type Chromosome = [Gen]
type Population = [Chromosome]
type Fitness = (Chromosome -> Double)
--type Crossover = (Chromosome -> Chromosome -> RandState Chromosome)

-- |Bit Flip operator. Flip one bit at given position. Pure functional.
bitFlip :: Chromosome -> Int -> Chromosome
bitFlip (0:xs) 0 = 1:xs
bitFlip (1:xs) 0 = 0:xs
bitFlip (x:xs) i = x : bitFlip xs (i-1)

-- |Random bit flip operator
bitFlipR :: Chromosome -> RandState Chromosome
bitFlipR chromosome = do
    p <- randomRange $ length chromosome - 1
    let mutatedChromosome = bitFlip chromosome p
    return mutatedChromosome

-- |Takes two chromosomes and combine them cutting at one point
onePointCrossover :: Chromosome -> Chromosome -> RandState [Chromosome]
onePointCrossover chr1 chr2 = do
    p <- randomRange $ length chr1 - 1
    let chr1' = splitAt p chr1
    let chr2' = splitAt p chr2
    return [fst chr1' ++ snd chr2', snd chr1' ++ fst chr2']

onePointCrossover' :: Chromosome -> Chromosome -> RandState [Chromosome]
onePointCrossover' chr1 chr2 = do
    p <- randomRange $ length chr1 - 1
    let chr1_1 = [i | (i,n) <- zip chr1 [0..length chr1 - 1], n <= p]
    let chr1_2 = [i | (i,n) <- zip chr1 [0..length chr1 -1], n > p]
    let chr2_1 = [i | (i,n) <- zip chr2 [0..length chr2 - 1], n <= p]
    let chr2_2 = [i | (i,n) <- zip chr2 [0..length chr2 - 1], n > p]
    return [chr1_1 ++ chr2_2, chr1_2 ++ chr2_1]

-- |Takes two chromosomes and combine them cutting at two points
twoPointCrossover :: Chromosome -> Chromosome -> RandState [Chromosome]
twoPointCrossover chr1 chr2 = do
    p1 <- randomRange $ length chr1 - 1
    p2 <- randomRange $ length chr1 - 1
    let chr1_1 = [i | (i,n) <- zip chr1 [0..length chr1 - 1], n < p1]
    let chr1_2 = [i | (i,n) <- zip chr1 [0..length chr1 - 1], n >= p1 && n <= p2]
    let chr1_3 = [i | (i,n) <- zip chr1 [0..length chr1 - 1], n > p2]
    let chr2_1 = [i | (i,n) <- zip chr2 [0..length chr2 - 1], n < p1]
    let chr2_2 = [i | (i,n) <- zip chr2 [0..length chr2 - 1], n >= p1 && n <= p2]
    let chr2_3 = [i | (i,n) <- zip chr2 [0..length chr2 - 1], n > p2]
    return [chr1_1 ++ chr2_2 ++ chr1_3, chr2_1 ++ chr1_2 ++ chr2_3]

-- |Returns the probability of a chromosome to be selected
probabilityToBeSelected :: Population -> Fitness -> Chromosome-> Double
probabilityToBeSelected pop fitness chr = (fitness chr) / total
    where total = sum $ map (fitness) pop

-- |Select the fittest chromosomes
fittestSelection :: Population -> Fitness -> Population
fittestSelection pop fitness = (take (div (length pop) 2) . reverse) $ L.sortOn (fitness) pop


-- |Roulette Wheel Selection method
rouletteWheelSelection :: Population -> Fitness -> RandState Population
rouletteWheelSelection pop fitness = spin pop [] (div (length pop) 2)
    where spin pop pop' ite = if ite == 0 then return pop' else do
            p <- randomRange01
            let chrIndex = select pop p fitness
            let chr = pop !! chrIndex
            spin pop (chr:pop') (ite-1)

choose :: Ord a => a -> [a] -> a
choose p [x] = x
choose p (x:y:ys) = if x <= p && p < y then x
                    else choose p (y:ys)

select :: Population -> Double -> Fitness -> Int
select pop p fitness = chrIndex
    where pi = map (probabilityToBeSelected pop fitness) pop
          piAcum = (scanl1 (+) $ pi)
          chrIndex = elemIndex' (choose p piAcum) piAcum 0

-- |It crosses all the chromosomes in a population
--populationCrossover :: Population -> Crossover -> RandState Population
populationCrossover pop crossFunction = do
    let (pop1,pop2) = splitAt (div (length pop) 2) pop
    pop' <- zipWithM (crossFunction) pop1 pop2
    return pop' 


-- Funciones de ejemplo
fitness :: Fitness
fitness chr = fromIntegral $ sum chr

popEx :: Population
popEx = [[1,1],[2,2],[1,2],[3,1],[6,0],[0,0]]
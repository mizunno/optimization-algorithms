module OA.Utils.RandState (
    randomRange,
    randomRange01,
    randomRange2,
    randomRangeD,
    getR,
    randomBinary,
    randomBinaryList,
    randomDoubleList,
    randomBoundedList,
    shuffle,
    RandState
) where

import           Control.Monad
import           Control.Monad.State
import           System.Random

type RandState = State StdGen

randomRange :: Int -> RandState Int
randomRange upperBound = state $ randomR (0, upperBound)

randomRange2 :: (Int,Int) -> RandState Int
randomRange2 (inf,sup) = state $ randomR (inf, sup)

randomRange01 :: RandState Double
randomRange01 = randomRange 10 >>= (\p -> return $ fromIntegral p / 10)

randomRangeD :: (Double, Double) -> RandState Double
randomRangeD (inf,sup) = state $ randomR (inf, sup)

randomBinary :: RandState Int
randomBinary = state $ randomR (0, 1)

randomBinaryList :: Int -> RandState [Int]
randomBinaryList n = replicateM n randomBinary

randomBoundedList :: (Int,Int) -> Int -> RandState [Int]
randomBoundedList (inf,sup) n = replicateM n $ randomRange2 (inf,sup)

randomDoubleList :: Int -> (Double, Double) -> RandState [Double]
randomDoubleList n bound = replicateM n $ randomRangeD bound

shuffle :: [a] -> RandState [a]
shuffle x = if length x < 2 then return x else do
    i <- randomRange2 (0, length x - 1)
    r <- shuffle (take i x ++ drop (i+1) x)
    return (x!!i : r)

getR :: RandState Int
getR = state random

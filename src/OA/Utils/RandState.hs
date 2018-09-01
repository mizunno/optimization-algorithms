module OA.Utils.RandState (
    randomRange,
    randomRange01,
    randomRange2,
    getR,
    randomBinary,
    randomBinaryList,
    randomDoubleList,
    RandState
) where

import System.Random
import Control.Monad.State
import Control.Monad

type RandState = State StdGen

randomRange :: Int -> RandState Int
randomRange upperBound = state $ randomR (0, upperBound)

randomRange2 :: (Int,Int) -> RandState Int
randomRange2 (min,max) = state $ randomR (min, max)

randomRange01 :: RandState Double
randomRange01 = randomRange 10 >>= (\p -> return $ fromIntegral p / 10)

randomRangeD :: (Double, Double) -> RandState Double
randomRangeD (min,max) = state $ randomR (min, max)

randomBinary :: RandState Int
randomBinary = state $ randomR (0, 1)

randomBinaryList :: Int -> RandState [Int]
randomBinaryList n = replicateM n randomBinary

randomDoubleList :: Int -> (Double, Double) -> RandState [Double]
randomDoubleList n bound = replicateM n $ randomRangeD bound

getR :: RandState Int
getR = state random
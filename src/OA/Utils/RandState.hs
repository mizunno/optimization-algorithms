module OA.Utils.RandState (
    randomRange,
    randomRange01,
    getR,
    randomBinary,
    randomBinaryList,
    RandState
) where

import System.Random
import Control.Monad.State
import Control.Monad

type RandState = State StdGen

randomRange :: Int -> RandState Int
randomRange upperBound = state $ randomR (1, upperBound)

randomRange01 :: RandState Double
randomRange01 = randomRange 10 >>= (\p -> return $ fromIntegral p / 10)

randomRange01' :: RandState Double
randomRange01' = do
    p <- randomRange 10
    let p' = fromIntegral p / 10
    return $ p'

randomBinary :: RandState Int
randomBinary = state $ randomR (0, 1)

randomBinaryList :: Int -> RandState [Int]
randomBinaryList l = replicateM l randomBinary

getR :: RandState Int
getR = state random
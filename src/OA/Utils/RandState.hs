module OA.Utils.RandState (
    randomRange,
    randomRange01,
    getR,
    RandState
) where

import System.Random
import Control.Monad.State
import Control.Monad

type RandState = State StdGen

randomRange :: Int -> RandState Int
randomRange upperBound = state $ randomR (1, upperBound)

randomRange01 :: RandState Double
randomRange01 = do
    p <- randomRange 10
    let p' = fromIntegral p / 10
    return $ p'

getR :: RandState Int
getR = state random
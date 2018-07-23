module OA.Utils.RandState (
    randomRange,
    getR,
    RandState
) where

import System.Random
import Control.Monad.State
import Control.Monad

type RandState = State StdGen

randomRange :: Int -> RandState Int
randomRange upperBound = state $ randomR (1, upperBound)

getR :: RandState Int
getR = state $ random
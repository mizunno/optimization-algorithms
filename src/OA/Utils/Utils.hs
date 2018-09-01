module OA.Utils.Utils (
    argMax,
    argMin,
    elemIndex',
    randomChoice',
    randomChoiceIO,
    probability,
    probabilityIO,
    probabilityIO',
    randomBinaryList',
    randomBinaryIO
) where

import           Data.List          as L
import           Data.Ord           as O
import           OA.Utils.RandState
import           System.Random      as R

----------------------
-- Useful functions --
----------------------

-- ### NO IO MONAD ### --

-- |Returns the index of a given element
elemIndex' :: Eq a => a -> [a] -> Int -> Int
elemIndex' e [] l = l-1
elemIndex' e (x:xs) l
        | e == x = l
        | otherwise = elemIndex' e xs l+1

-- |Return the element (from a list) that maximize the given function
argMax :: (Ord b, Num b) => [a] -> (a -> b) -> a
argMax xs f = L.maximumBy (O.comparing f) xs

-- |Return the element (from a list) that minimize the given function
argMin :: (Ord b, Num b) => (a -> b) -> [a] -> a
argMin f = L.minimumBy (O.comparing f)

-- |Choose a random element from a list
randomChoice' :: [a] -> RandState a
randomChoice' xs = do
    p <- randomRange $ length xs - 1
    return $ xs !! p

-- |Choose a random element from a list (No RandState Monad)
randomChoice :: RandomGen g => g -> [a] -> (a, g)
randomChoice g xs = (xs !! n, next)
    where
        (n, next) = randomR (0, length xs - 1) g

-- |Return a random list with bits.
randomBinaryList' :: RandomGen g => g -> Int -> [Int]
randomBinaryList' g ite = go g ite []
    where
        go :: RandomGen g => g -> Int -> [Int] -> [Int]
        go g' ite xs = let (n,next) = randomR (0, 1) g' in
            if ite == 0
                then xs
                else
                    go next (ite-1) (n:xs)

-- |Return 'True' with probability p
probability :: (Ord a, Fractional a) => a -> RandState Bool
probability prob = do
    p <- randomRange 100
    let p' = fromIntegral p / 100
    return $ p' <= prob

probability' :: (RandomGen g, Random a, Ord a, Num a) => g -> a -> (Bool, g)
probability' g p = (p' <= p, g')
    where
        (p', g') = R.randomR (0,1) g


-- ### IO MONAD ### --

-- |Return a random bit (0,1)
randomBinaryIO :: IO Int
randomBinaryIO = getStdGen >>= \g -> return $ fst $ randomR (0, 1) g

-- |Choose a random element from a list
randomChoiceIO :: [a] -> IO a
randomChoiceIO xs = getStdGen >>= \g -> return $ fst $ randomChoice g xs

-- |Return 'True' with probability p
probabilityIO :: (R.Random a, Ord a, Num a) => a -> IO Bool
probabilityIO p = randomIO >>= \q -> return $! (q < p)

probabilityIO' :: (R.Random a, Ord a, Num a) => a -> IO Bool
probabilityIO' p = randomRIO (0, 10) >>= \q -> return $! (q < (p * 10))

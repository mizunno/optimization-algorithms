module OA.Examples.PSOExample (
    main
) where

import           Control.Monad.State
import           OA.Algorithms.PSOAlgorithm
import           System.Random

-- |Solution: [0,0]
--  Value: 0
--  Domain: [-5.12,5.12]
rastriginFunction :: (Floating a) => [a] -> a
rastriginFunction xs = - (fromIntegral (10*length xs ) + sum [x^2 - 10 * cos( 2 * pi * x) | x <- xs])

-- |Solution: [-pi,pi]
--  Value: -1
--  Domain: [-100,100]
easomFunction [x,y] = -(-cos x *cos y *exp(-((x-pi)^2+(y-pi)^2)))

-- |Solution: [0,0]
--  Value: 0
--  Domain: [-5,5]
ackleyFunction [x,y] = -(-20*exp(-2*sqrt(0.5*(x^2+y^2))) - exp(0.5*(cos(2*pi*x)+cos(2*pi*y))) + exp 1 + 20)

-- |Solution: [0,0]
--  Value: 0
--  Domain: [-inf,inf]
sphereFunction [x,y] = -(x^2 + y^2)

-- |Solution: [1,1]
--  Value: 0
--  Domain: [-inf,inf]
rosenbrockFunction [x,y] = -(100*(y-x^2)^2 + (1 - x)^2)

-- |Solution: [3,0.5]
--  Value: 0
--  Domain: [-4.5,4.5]
bealeFunction [x,y] = -((1.5 - x + x*y)^2 + (2.25 - x + x*y^2)^2 + (2.625 - x + x*y^3)^2)

-- |Solution: [1.34941,-1.34941],[1.34941,1.34941],[-1.34941,+1.34941],[-1.34941,-1.34941]
--  Value: -2.06261
--  Domain: [-10,10]
crossInTrayFunction [x,y] = -(-0.0001 * abs(sin x *sin y *exp(abs(100-(sqrt(x^2+y^2)/pi)))+1)**0.1)

-- |Solution: [512,404.2319]
--  Value: -959.6407
--  Domain: [-512,512]
eggholderFunction [x,y] = -(-(y+47)*sin(sqrt(abs(x/2 + (y+47)))) - x*sin(sqrt(abs(x-(y+47)))))

-- |Solution: [8.05502,9.66459],[-8.05502,9.66459],[8.05502,-9.66459],[-8.05502,-9.66459]
--  Value: -19.2085
--  Domain: [-10,10]
holderTableFunction [x,y] = -(-abs(sin x *cos y*exp(abs(1-(sqrt(x^2+y^2)/pi)))))

-- |Solution: [0,0]
--  Value: 0
--  Domain: [-100,100]
schafferFunctionN2 [x,y] = -(0.5 + ((sin(x^2-y^2)^2 - 0.5)/(1+0.001*(x^2+y^2))^2))

-- |Solution: [0,1.25313]
--  Value: 0.292579
--  Domain: [-100,100]
schafferFunctionN4 [x,y] = -(0.5 + (cos(sin(abs(x^2-y^2)))^2 - 0.5) / (1+0.001*(x^2+y^2))^2)

psoInfo = PSOInfo 100 (-5.12,5.12) 100 1 100

main = do
    solution <- runPSO psoInfo rastriginFunction
    print $ "[x,y] = " ++ show solution
    print $ "f(x,y) = " ++ show (rastriginFunction solution)

media :: [Double] -> Double
media xs = sum xs / fromIntegral (length xs)

varianza xs = media $ map (\x -> (x - m)^2) xs
    where m = media xs

{- runPSOdata 0 fs _ = return $ "Media: " ++ show (media fs) ++ " Varianza: " ++ show (varianza fs) ++ " Mejor: " ++ show (maximum fs)
    ++ " Peor: " ++ show (minimum fs)
runPSOdata ite fs g = do
    let (solution,g') = runState (psoAlgorithm psoInfo rastriginFunction) g
    let value = rastriginFunction solution
    print $ show ite ++ " -- f(x,y) = " ++ show value
    runPSOdata (ite-1) (value:fs) g'
 -}
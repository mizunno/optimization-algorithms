module OA.Examples.PSOExample (
    runPSO
) where

import           Control.Monad.State
import           OA.Algorithms.PSOAlgorithm
import           System.Random

-- |Solution: [0,0]
--  Value: 0
--  Domain: [-5.12,5.12]
rastriginFunction [x,y] = -(10*2 + x^2 + y^2 - 10*cos(2*pi*x) - 10*cos(2*pi*y))

-- |Solution: [-pi,pi]
--  Value: -1
--  Domain: [-100,100]
easomFunction [x,y] = -(-cos(x)*cos(y)*exp(-((x-pi)^2+(y-pi)^2)))

-- |Solution: [0,0]
--  Value: 0
--  Domain: [-5,5]
ackleyFunction [x,y] = -(-20*exp(-2*sqrt(0.5*(x^2+y^2))) - exp(0.5*(cos(2*pi*x)+cos(2*pi*y))) + exp(1) + 20)

-- |Solution: [0,0]
--  Value: 0
--  Domain: [-inf,inf]
sphereFunction [x,y] = -(x^2 + y^2)

-- |Solution: [1,1]
--  Value: 0
--  Domain: [-inf,inf]
rosenbrockFunction [x,y] = -((100*(y-x^2)^2 + (1 - x)^2))

-- |Solution: [3,0.5]
--  Value: 0
--  Domain: [-4.5,4.5]
bealeFunction [x,y] = -((1.5 - x + x*y)^2 + (2.25 - x + x*y^2)^2 + (2.625 - x + x*y^3)^2)

-- |Solution: [1.34941,-1.34941],[1.34941,1.34941],[-1.34941,+1.34941],[-1.34941,-1.34941]
--  Value: -2.06261
--  Domain: [-10,10]
crossInTrayFunction [x,y] = -(-0.0001 * (abs(sin(x)*sin(y)*exp(abs(100-(sqrt(x^2+y^2)/pi)))+1))**0.1)

-- |Solution: [512,404.2319]
--  Value: -959.6407
--  Domain: [-512,512]
eggholderFunction [x,y] = -(-(y+47)*sin(sqrt(abs(x/2 + (y+47)))) - x*sin(sqrt(abs(x-(y+47)))))

-- |Solution: [8.05502,9.66459],[-8.05502,9.66459],[8.05502,-9.66459],[-8.05502,-9.66459]
--  Value: -19.2085
--  Domain: [-10,10]
holderTableFunction [x,y] = -(-abs(sin(x)*cos(y)*exp(abs(1-(sqrt(x^2+y^2)/pi)))))

-- |Solution: [0,0]
--  Value: 0
--  Domain: [-100,100]
schafferFunctionN2 [x,y] = -(0.5 + (((sin(x^2-y^2))^2 - 0.5)/(1+0.001*(x^2+y^2))^2))

-- |Solution: [0,1.25313]
--  Value: 0.292579
--  Domain: [-100,100]
schafferFunctionN4 [x,y] = -(0.5 + ((cos(sin(abs(x^2-y^2))))^2 - 0.5) / (1+0.001*(x^2+y^2))^2)

psoInfo = PSOInfo 2 (-100,100) 100 1

runPSO = do
    g <- getStdGen
    let solution = evalState (psoAlgorithm psoInfo 100 schafferFunctionN4) g
    print solution
    print $ schafferFunctionN4 solution

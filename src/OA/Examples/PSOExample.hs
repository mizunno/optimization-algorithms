module OA.Examples.PSOExample (
    runPSO
) where

import           Control.Monad.State
import           OA.Algorithms.PSOAlgorithm
import           System.Random

rastriginFunction [x,y] = -(10*2 + x^2 + y^2 - 10*cos(2*pi*x) - 10*cos(2*pi*y))

easomFunction [x,y] = -(-cos(x)*cos(y)*exp(-((x-pi)^2+(y-pi)^2)))

ackleyFunction [x,y] = -(-20*exp(-2*sqrt(0.5*(x^2+y^2))) - exp(0.5*(cos(2*pi*x)+cos(2*pi*y))) + exp(1) + 20)

sphereFunction [x,y] = -(x^2 + y^2)

rosenbrockFunction [x,y] = -((100*(y-x^2)^2 + (1 - x)^2))

bealeFunction [x,y] = -((1.5 - x + x*y)^2 + (2.25 - x + x*y^2)^2 + (2.625 - x + x*y^3)^2)

crossInTrayFunction [x,y] = -(-0.0001 * (abs(sin(x)*sin(y)*exp(abs(100-(sqrt(x^2+y^2)/pi)))+1))**0.1)

eggholderFunction [x,y] = -(-(y+47)*sin(sqrt(abs(x/2 + (y+47)))) - x*sin(sqrt(abs(x-(y+47)))))

holderTableFunction [x,y] = -(-abs(sin(x)*cos(y)*exp(abs(1-(sqrt(x^2+y^2)/pi)))))

mcCormickFunction [x,y] = -(sin(x+y) + (x-y)^2 - 1.5*x + 2.5*y + 1)

psoInfo = PSOInfo 2 (-10,10) 1000 2.0

runPSO = do
    g <- getStdGen
    let solution = evalState (psoAlgorithm psoInfo 1000  mcCormickFunction) g
    print $ solution
    print $ mcCormickFunction solution

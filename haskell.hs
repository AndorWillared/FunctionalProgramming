import Data.Matrix
import System.Random
import System.IO.Unsafe


data NeuralNetwork = NeuralNetwork { weights::[Matrix Float], biases::[Matrix Float] } deriving Show

main = do
    let network = initializeNeuralNetwork [2,3,2]
    print network
    
-- initialize <inputCount> <outputCount> <hiddenCount>
initializeNeuralNetwork :: [Int] -> NeuralNetwork
initializeNeuralNetwork l = NeuralNetwork 
                            [randomMatrix (l!!(i+1)) (l!!i) (-1.0, 1.0) | i <- [0..((length l) -2)]]
                            [zeroMatrix (l!!i) 1 | i <- [1..((length l) -1)]]

randomMatrix :: Int -> Int -> (Float,Float) -> Matrix Float
randomMatrix n m b = matrix n m $ \(i,j) -> unsafePerformIO $ getStdRandom $ randomR b

zeroMatrix :: Int -> Int -> Matrix Float
zeroMatrix n m = matrix n m $ \(i,j) -> 0.0

sigmoid x = 0.5 * (1 + tanh (x/2))

forward [] _ activation = []
forward _ [] activation = []
forward (w:weights) (b:biases) activation = ergebnis : (forward weights biases ergebnis) where ergebnis = ((multStd w activation))

forwardTest (w:weights) (b:biases) activation = ergebnis where ergebnis = (multStd w activation)

forwardPass network input = input : forward (weights network) (biases network) input

--backProp network input = 

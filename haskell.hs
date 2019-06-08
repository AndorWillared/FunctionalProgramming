import Data.Matrix
import System.Random
import System.IO.Unsafe

data NeuralNetwork = NeuralNetwork { weights::[Matrix Float], biases::[Matrix Float] } deriving Show

-- initialize <inputCount> <outputCount> <hiddenCount>
initializeNeuralNetwork :: [Int] -> NeuralNetwork
initializeNeuralNetwork l = NeuralNetwork 
                            [randomMatrix (l!!i) (l!!(i+1)) (-1.0, 1.0) | i <- [0..((length l) -2)]]
                            [zeroMatrix (l!!i) 1 | i <- [1..((length l) -1)]]

-- generateRandomMatrix :: (Float a) => Int -> Int -> Matrix a
randomMatrix n m b = matrix n m $ \(i,j) -> unsafePerformIO $ getStdRandom $ randomR b

-- generateZeroMatrix :: (Float a) => Int -> Int -> Matrix a
zeroMatrix n m = matrix n m $ \(i,j) -> 0.0

-- get number between -1 and 1 (both inclusive)

-- randomNumber = fst randomValue where randomValue = randomR ((-1.0), 1.0)

-- randomNumber = unsafePerformIO $ getStdRandom randomGen

-- randomGen = unsafePerformIO $ getStdGen randomGen where randomGen = unsafePerformIO

main = do
    print $ show (initializeNeuralNetwork [2,3,2])

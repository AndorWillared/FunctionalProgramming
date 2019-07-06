module NeuralNetwork where

import Data.Matrix
import System.Random
import System.IO.Unsafe

data NeuralNetwork = NeuralNetwork { weights::[Matrix Float], biases::[Matrix Float] }

createNeuralNetwork :: [Int] -> NeuralNetwork
createNeuralNetwork layers = NeuralNetwork
                              [randomRMatrix (layers!!(i+1)) (layers!!i) (-1.0, 1.0) | i <- [0..((length layers) -2)]]
                              [zeroMatrix (layers!!i) 1 | i <- [1..((length layers) -1)]]

randomRMatrix :: Int -> Int -> (Float, Float) -> Matrix Float
randomRMatrix rows columns range = matrix rows columns (\(i, j) -> unsafePerformIO (getStdRandom (randomR range)))

zeroMatrix :: Int -> Int -> Matrix Float
zeroMatrix rows columns = matrix rows columns (\(i, j) -> 0.0)

predict :: NeuralNetwork -> Matrix Float -> Matrix Float
predict network input = last (forwardPass network input)

forwardPass :: NeuralNetwork -> Matrix Float -> [Matrix Float]
forwardPass network input = input : forwardPass2 (weights network) (biases network) input

forwardPass2 :: [Matrix Float] -> [Matrix Float] -> Matrix Float -> [Matrix Float]
forwardPass2 [] _ activation = []
forwardPass2 _ [] activation = []
forwardPass2 (w:weights) (b:biases) activation = nextActivation : forwardPass2 weights biases nextActivation
                                                  where nextActivation = fmap sigmoid ((multStd w activation) + b)

train :: NeuralNetwork -> [(Matrix Float, Matrix Float)] -> Float -> NeuralNetwork
train network [] _ = network
train network ((input, output):trainingData) learningRate = train (backprop network input output learningRate) trainingData learningRate

backprop :: NeuralNetwork -> Matrix Float -> Matrix Float -> Float -> NeuralNetwork
backprop network input output learningRate = applyUpdates network (reverse (getGradients (reverse (weights network)) (reverse (biases network)) (reverse (init activations)) ((last activations) - output))) learningRate
                                              where activations = forwardPass network input

applyUpdates :: NeuralNetwork -> [(Matrix Float,Matrix Float)] -> Float -> NeuralNetwork
applyUpdates network updates learningRate = NeuralNetwork (getUpdated (weights network) (fst (unzip updates)) learningRate) (getUpdated (biases network) (snd (unzip updates)) learningRate)

getUpdated :: [Matrix Float] -> [Matrix Float] -> Float -> [Matrix Float]
getUpdated list updates learningRate =  zipWith (\l u -> l - fmap (*learningRate) u) list updates

getGradients :: [Matrix Float] -> [Matrix Float] -> [Matrix Float] -> Matrix Float -> [(Matrix Float, Matrix Float)]
getGradients rWeights rBiases rActivations error = zipWith  (\e a ->  (multStd e (transpose a), e) ) errors rActivations
                                                   where errors = zipWith3 (\w b a ->(multiplyElementwise error (fmap sigmoid' ((multStd w a) + b)))) rWeights rBiases rActivations

multiplyElementwise :: Matrix Float -> Matrix Float -> Matrix Float
multiplyElementwise a b = fromList (nrows a) (ncols b) (zipWith (*) listA listB)
                          where listA = toList a
                                listB = toList b

sigmoid :: Float -> Float
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Float -> Float
sigmoid' x = (sigmoid x) * (1 - (sigmoid x))


mapToResult :: Matrix Float -> (Float, Int)
mapToResult m | l /= 10 = (0.0 ,(-1))                        -- this is a user - error
              | otherwise = maximum (zip matList [0..9])       -- this will work since maximum on tuples compares fst_s, then snd_s
                where l = length $ matList
                      matList = toList m

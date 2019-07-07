module NN where


-- aktuelle Version der Netz-Funktionalitäten
-- unterstützt Ausgabe beim Training
-- funktioniert zusammen mit der Oberfläche

import Data.Matrix
import System.Random
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as BSL
import Data.Binary
import Data.List.Split

data NeuralNetwork = NeuralNetwork { config::[Int] , weights::[Matrix Float] , biases::[Matrix Float] }

instance Binary NeuralNetwork where
    put (NeuralNetwork config weights biases) = do
      put config
      put $ fmap toList weights
      put $ fmap toList biases

    get = do
      config <- get
      rawWeights <- get
      let weights = [fromList (config!!(i+1)) (config!!i) (rawWeights!!i) | i <- [0..(length config-2)]]
      rawBiases <- get
      let biases = [fromList (config!!i) 1 (rawBiases!!(i-1)) | i <- [1..((length config)-1)]]
      return (NeuralNetwork config weights biases)

createNeuralNetwork :: [Int] -> NeuralNetwork
createNeuralNetwork config = NeuralNetwork
  config
  [randomRMatrix (config!!(i+1)) (config!!i) (-1.0,1.0) | i <- [0..((length config)-2)]]
  [zeroMatrix (config!!i) 1 | i <- [1..((length config)-1)]]

predict :: NeuralNetwork -> Matrix Float -> Matrix Float
predict nn input = last (forwardPass nn input)

train :: NeuralNetwork -> [(Matrix Float, Matrix Float)] -> Float -> NeuralNetwork

train nn samples learningRate = foldl back nn samples
                                where back  net sample = backprop net (fst sample) (snd sample) learningRate

serialize :: NeuralNetwork -> FilePath -> IO ()
serialize nn path = do
  let encoded_nn = encode nn
  BSL.writeFile path encoded_nn

deserialize :: FilePath -> IO NeuralNetwork
deserialize path = do
  nn <- decodeFile path :: IO NeuralNetwork
  return nn

serialize2 :: NeuralNetwork -> FilePath -> IO ()
serialize2 nn path = do
  writeFile path (show (
    [fromIntegral (length (config nn))]
    ++ (map fromIntegral (config nn))
    ++ (concat [ toList ((weights nn)!!i) | i <- [0..((length (config nn))-2)] ])
    ++ (concat [ toList ((biases nn)!!i) | i <- [0..((length (config nn))-2)] ])))

deserialize2 :: FilePath -> IO NeuralNetwork
deserialize2 path = do
  input <- (readFile path)
  let flist = map stringToFloat (splitOn "," (take ((length (drop 1 input)) - 1) (drop 1 input)))
  let config = map round (take (round (flist!!0)) (drop 1 flist))
  let wstart = drop (1 + (length config)) flist
  let weights = [ fromList (config!!(i+1)) (config!!i) (take ((config!!(i+1)) * (config!!i)) (drop (sum [ (config!!j)*(config!!(j+1)) | j <- [0..i-1]]) wstart)) | i <- [0..((length config)-2)] ]
  let bstart = drop (sum [ (config!!j)*(config!!(j+1)) | j <- [0..((length config)-2)]]) wstart
  let biases = [ fromList (config!!(i+1)) 1 (take ((config!!(i+1))) (drop (sum [ (config!!(j+1)) | j <- [0..i-1]]) bstart)) | i <- [0..((length config)-2)] ]
  return (NeuralNetwork config weights biases)

forwardPass :: NeuralNetwork -> Matrix Float -> [Matrix Float]
forwardPass nn input = input : forwardPass' (weights nn) (biases nn) input

forwardPass' :: [Matrix Float] -> [Matrix Float] -> Matrix Float -> [Matrix Float]
forwardPass' [] _ activation = []
forwardPass' _ [] activation = []
forwardPass' (w:weights) (b:biases) activation = nextActivation : forwardPass' weights biases nextActivation
                                              where nextActivation = fmap sigmoid ((multStd w activation) + b)


backprop :: NeuralNetwork -> Matrix Float -> Matrix Float -> Float -> NeuralNetwork
backprop nn input output learningRate = apply nn (reverse (gradients (reverse (weights nn)) (reverse (biases nn)) (reverse (init activations)) ((last activations) - output))) learningRate
                                          where activations = forwardPass nn input

apply :: NeuralNetwork -> [(Matrix Float,Matrix Float)] -> Float -> NeuralNetwork
apply nn updates learningRate = NeuralNetwork (config nn) (getUpdated (weights nn) (fst (unzip updates)) learningRate) (getUpdated (biases nn) (snd (unzip updates)) learningRate)

getUpdated :: [Matrix Float] -> [Matrix Float] -> Float -> [Matrix Float]
getUpdated list updates learningRate =  zipWith (\l u -> l - fmap (*learningRate) u) list updates

gradients :: [Matrix Float] -> [Matrix Float] -> [Matrix Float] -> Matrix Float -> [(Matrix Float, Matrix Float)]
gradients [] _ _ _ = []
gradients _ [] _ _ = []
gradients _ _ [] _ = []
gradients (w:rWeights) (b:rBiases) (a:rActivations) error = (multStd error' (transpose a), error') : gradients rWeights rBiases rActivations error''
                                                                where error' = (multiplyElementwise error (fmap sigmoid' ((multStd w a) + b)))
                                                                      error'' = (multStd (transpose w) error')

randomRMatrix :: Int -> Int -> (Float, Float) -> Matrix Float
randomRMatrix rows columns range = matrix rows columns (\(i, j) -> unsafePerformIO (getStdRandom (randomR range)))

zeroMatrix :: Int -> Int -> Matrix Float
zeroMatrix rows columns = matrix rows columns (\(i, j) -> 0.0)

multiplyElementwise :: Matrix Float -> Matrix Float -> Matrix Float
multiplyElementwise a b = fromList (nrows a) (ncols b) (zipWith (*) listA listB)
                          where listA = toList a
                                listB = toList b

sigmoid :: Float -> Float
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Float -> Float
sigmoid' x = (sigmoid x) * (1 - (sigmoid x))

stringToFloat :: String -> Float
stringToFloat string = read string :: Float

trainVerbose :: NeuralNetwork -> [(Matrix Float, Matrix Float)] -> Float -> IO NeuralNetwork
trainVerbose nn ((input, output):samples) learningRate = trainVerbose' nn samples learningRate 0 0

data BackpropR = BackpropR { nn :: NeuralNetwork , totalError :: Float , totalIterations :: Int}

trainVerbose' :: NeuralNetwork -> [(Matrix Float, Matrix Float)] -> Float -> Float -> Int -> IO NeuralNetwork
trainVerbose' nn' [] _ _ _ = return nn'
trainVerbose' nn' ((input, output):samples) learningRate totalError' trainingIterations' = do
  backpropR <- (backpropVerbose nn' input output learningRate totalError' trainingIterations')
  trainVerbose' (nn backpropR) samples learningRate (totalError backpropR) (totalIterations backpropR)

backpropVerbose :: NeuralNetwork -> Matrix Float -> Matrix Float -> Float -> Float -> Int -> IO BackpropR
backpropVerbose nn input output learningRate totalError totalIterations = do
  let err = 0.5 * (sum $ toList (fmap (^2) ((last activations) - output)))
  let updatedNN = apply nn (reverse (gradients (reverse $ weights nn) (reverse $ biases nn) (reverse $ init activations) ((last activations) - output))) learningRate
  putStrLn ((show (totalIterations + 1)) ++ ": " ++ show ((totalError + err)/(fromIntegral (totalIterations + 1))))
  return (BackpropR updatedNN (totalError + err) (totalIterations + 1))
  where activations = forwardPass nn input


mapToResult :: Matrix Float -> (Float, Int)
mapToResult m | l /= 10 = (0.0 ,(-1))
              | otherwise = maximum (zip matList [0..9])
                where l = length $ matList
                      matList = toList m

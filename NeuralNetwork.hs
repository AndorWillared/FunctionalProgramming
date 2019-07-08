module NeuralNetwork where

import Data.Matrix
import System.Random
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as BSL
import Data.Binary
import Data.List.Split
import System.Random.Shuffle
import Prelude hiding (zip,zipWith,zipWith3,unzip)
import Data.Zip

data NeuralNetwork = !NeuralNetwork { config::[Int] , weights::[Matrix Float] , biases::[Matrix Float] }

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

forwardPass :: NeuralNetwork -> Matrix Float -> [Matrix Float]
forwardPass nn input = input : forwardPass' (weights nn) (biases nn) input

forwardPass' :: [Matrix Float] -> [Matrix Float] -> Matrix Float -> [Matrix Float]
forwardPass' [] _ activation = []
forwardPass' _ [] activation = []
forwardPass' (w:weights) (b:biases) activation = nextActivation : forwardPass' weights biases nextActivation
                                                  where nextActivation = fmap sigmoid ((multStd w activation) + b)

train :: NeuralNetwork -> [(Matrix Float, Matrix Float)] -> Float -> IO (NeuralNetwork)
train nn ((input, output):samples) learningRate = train' nn samples learningRate 0 0

train' :: NeuralNetwork -> [(Matrix Float, Matrix Float)] -> Float -> Float -> Int -> IO (NeuralNetwork)
train' nn' [] _ _ _ = return nn'
train' nn' ((input, output):samples) learningRate totalError' trainingIterations' = do
  backpropR <- (backprop nn' input output learningRate totalError' trainingIterations')
  train' (nn backpropR) samples learningRate (totalError backpropR) (totalIterations backpropR)

data BackpropR = BackpropR { nn :: NeuralNetwork , totalError :: Float , totalIterations :: Int }

backprop :: NeuralNetwork -> Matrix Float -> Matrix Float -> Float -> Float -> Int -> IO (BackpropR)
backprop nn input output learningRate totalError totalIterations = do
  let err = 0.5 * (sum $ toList (fmap (^2) ((last activations) - output)))
  let updatedNN = apply nn (reverse (gradients (reverse (weights nn)) (reverse (biases nn)) (reverse (init activations)) ((last activations) - output))) learningRate
  putStrLn ((show (totalIterations + 1)) ++ ": " ++ show ((totalError + err)/(fromIntegral (totalIterations + 1))))
  return (BackpropR updatedNN (totalError + err) (totalIterations + 1))
  where activations = forwardPass nn input

apply :: NeuralNetwork -> [(Matrix Float,Matrix Float)] -> Float -> NeuralNetwork
apply nn updates learningRate = NeuralNetwork (config nn) (update (weights nn) (fst (unzip updates)) learningRate) (update (biases nn) (snd (unzip updates)) learningRate)

update :: [Matrix Float] -> [Matrix Float] -> Float -> [Matrix Float]
update [] _ _ = []
update _ [] _ = []
update (m:matrices) (u:updates) learningRate = m - fmap (*learningRate) u : update matrices updates learningRate

gradients :: [Matrix Float] -> [Matrix Float] -> [Matrix Float] -> Matrix Float -> [(Matrix Float, Matrix Float)]
gradients [] _ _ _ = []
gradients _ [] _ _ = []
gradients _ _ [] _ = []
gradients (w:weights) (b:biases) (a:activations) error = (multStd error' (transpose a), error') : gradients weights biases activations error''
                                                            where error' = multiplyElementwise error (fmap sigmoid' ((multStd w a) + (b)))
                                                                  error'' = multStd (transpose w) error'

serialize :: NeuralNetwork -> FilePath -> IO ()
serialize nn path = do
  BSL.writeFile path (encode nn)

deserialize :: FilePath -> IO (NeuralNetwork)
deserialize path = do
  nn <- decodeFile path :: IO (NeuralNetwork)
  return nn

serialize2 :: NeuralNetwork -> FilePath -> IO ()
serialize2 nn path = do
  writeFile path (show (
    [fromIntegral (length (config nn))]
    ++ (map fromIntegral (config nn))
    ++ (concat [ toList ((weights nn)!!i) | i <- [0..((length (config nn))-2)] ])
    ++ (concat [ toList ((biases nn)!!i) | i <- [0..((length (config nn))-2)] ])))

deserialize2 :: FilePath -> IO (NeuralNetwork)
deserialize2 path = do
  input <- (readFile path)
  let flist = map read (splitOn "," (take ((length (drop 1 input)) - 1) (drop 1 input)))
  let config = map round (take (round (flist!!0)) (drop 1 flist))
  let wstart = drop (1 + (length config)) flist
  let weights = [ fromList (config!!(i+1)) (config!!i) (take ((config!!(i+1)) * (config!!i)) (drop (sum [ (config!!j)*(config!!(j+1)) | j <- [0..i-1]]) wstart)) | i <- [0..((length config)-2)] ]
  let bstart = drop (sum [ (config!!j)*(config!!(j+1)) | j <- [0..((length config)-2)]]) wstart
  let biases = [ fromList (config!!(i+1)) 1 (take ((config!!(i+1))) (drop (sum [ (config!!(j+1)) | j <- [0..i-1]]) bstart)) | i <- [0..((length config)-2)] ]
  return (NeuralNetwork config weights biases)

shuffleSamples :: [(Matrix Float, Matrix Float)] -> Int -> [(Matrix Float, Matrix Float)]
shuffleSamples samples seed = shuffle' samples (length samples) (mkStdGen seed)

argmax :: Matrix Float -> Int
argmax matrix = snd (maximum (zip (toList matrix) [0..(length (toList matrix))]))

toCategorical :: Int -> Int -> [Float]
toCategorical label classes = [if i == label then 1 else 0 | i <- [0..classes-1]]

test :: NeuralNetwork -> [(Matrix Float, Matrix Float)] -> IO ()
test _ [] = putStrLn ""
test nn (s:samples) = do
                    let prediction = argmax (predict nn (fst s))
                    let label = argmax (snd s)
                    if prediction == label then
                      putStrLn "right"
                    else
                      putStrLn "wrong"
                    test nn samples

-- Helper

randomRMatrix :: Int -> Int -> (Float, Float) -> Matrix Float
randomRMatrix rows columns range = matrix rows columns (\(i, j) -> unsafePerformIO (getStdRandom (randomR range)))

zeroMatrix :: Int -> Int -> Matrix Float
zeroMatrix rows columns = matrix rows columns (\(i, j) -> 0.0)

multiplyElementwise :: Matrix Float -> Matrix Float -> Matrix Float
multiplyElementwise m1 m2 = fromList (nrows m1) (ncols m2) (zipWith (*) m1List m2List)
                              where m1List = toList m1
                                    m2List = toList m2

--multiplyElementwise m1 m2 = (zipWith (*) [m1] [m2])!!0

sigmoid :: Float -> Float
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Float -> Float
sigmoid' x = (sigmoid x) * (1 - (sigmoid x))

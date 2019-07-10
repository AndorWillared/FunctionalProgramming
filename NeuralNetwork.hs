{-|
Module      : NeuralNetwork
Description : A haskell implementation of a neural network
License     : MIT
Maintainer  : adiel.ahmad@mni.thm.de
              andor.willared@mni.thm.de
              felix.willared@mni.thm.de
              marco.herzog@mni.thm.de
              jannis.weber@mni.thm.de
Stability   : experimental

A naive neural-network implementation in haskell. 

Use 'initNN' to get an initialised neural network and train it using the 'train' function.

You can get predictions of your trained neural network by running 'predict'.
-}

module NeuralNetwork (
  -- * DataType
  NeuralNetwork,
  -- * Initialisiation
  initNN,
  -- * Prediction
  predict,
  -- * Training
  train,
  -- * Serializiation
  -- ** Binary coded
  serialize,
  deserialize,
  -- ** Plain coded
  serializePlain,
  deserializePlain,
  -- * Helper functions
  sigmoid,
  sigmoid',
  randomRMatrix,
  zeroMatrix,
  multiplyElementwise,
  ) where

import Data.Matrix
import System.Random
import qualified Data.ByteString.Lazy as BSL
import Data.Binary
import Data.List.Split
import System.Random.Shuffle

-- | The data type 'NeuralNetwork' represents the state of a network.
--
-- It contains:
-- 
-- - config (nodes per layer) as a list of 'Int' 
-- - weights as 'Matrix' of 'Float' 
-- - biases as 'Matrix' of 'Float' 
data NeuralNetwork 
    = NeuralNetwork { config::[Int], weights::[Matrix Float], biases::[Matrix Float] }

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

-- | 'initNN' creates a randomly initialised network with the specified layers
--
-- __For example:__ 
-- 
-- A network with 784 input-nodes, 2 hidden layers with 1000 nodes each and 10 output-nodes 
-- 
-- @> network <- initNN [784,1000,1000,10] 123123@
initNN :: [Int]               -- ^ List of Nodes per Layer
       -> Int                 -- ^ Seed for the random generation of nodes
       -> IO (NeuralNetwork)

initNN config seed = do
  weights <- sequence [ randomRMatrix (config!!(i+1)) (config!!i) (-1.0, 1.0) seed | i <- [0..((length config)-2)] ]
  let biases = [zeroMatrix (config!!i) 1 | i <- [1..((length config)-1)]]
  return (NeuralNetwork config weights biases)

-- | 'predict' takes an network and a fitting input and runs a forwardPass with these parameters. 
-- The resulting output 'Matrix' is returned
predict :: NeuralNetwork    -- ^ Trained 'NeuralNetwork' that will be used to 'predict' an output 
        -> Matrix Float     -- ^ Matrix of the input values for the given network
        -> Matrix Float     -- ^ Matrix of the output node values
        
predict nn input = last (forwardPass nn input)

-- | A function that runs one 'forwardPass' for the provided 'NeuralNetwork' 
-- with the given input and returns the activations of all layers except the input layer.
forwardPass :: NeuralNetwork    -- ^ Trained 'NeuralNetwork' that will be used for 'forwardPass'
            -> Matrix Float     -- ^ Matrix of the input values for the given network
            -> [Matrix Float]   -- ^ List of the matrices of the activations (last one is the output matrix of the network)
            
forwardPass nn input = input : forwardPass' (weights nn) (biases nn) input

-- | A helper function that provides the recursive calculation of the activations of the network and returns them.
forwardPass' :: [Matrix Float]  -- ^ Weights of the network
             -> [Matrix Float]  -- ^ Biases
             -> Matrix Float    -- ^ Activation of the previous recursion (initialised with [])
             -> [Matrix Float]
             
forwardPass' [] _ activation = []
forwardPass' _ [] activation = []
forwardPass' (w:weights) (b:biases) activation = nextActivation : forwardPass' weights biases nextActivation
  where nextActivation = fmap sigmoid ((multStd w activation) + b)

-- | The function 'train' should be used to train the network with the passed inputs/outputs and the specified learning rate.
-- Returns the updated 'NeuralNetwork' as 'IO'
train :: NeuralNetwork                  -- ^ Network
      -> [(Matrix Float, Matrix Float)] -- ^ List of Tupels of the corresponding in-/output matrices
      -> Float                          -- ^ Learing rate
      -> IO (NeuralNetwork)
      
train nn ((input, output):samples) learningRate = train' nn samples learningRate 0 0

-- | 'train\'' should not be used manually or only for testing purpose. 
-- It is called by 'train' and traines the network by applying the 'backprop' function and passing the outcome to the next recursion of 'train\''. 
-- Returns the trained network as IO.
train' :: NeuralNetwork                     -- ^ Network
       -> [(Matrix Float, Matrix Float)]    -- ^ List of Tupels of the corresponding in-/output matrices
       -> Float                             -- ^ Learning rate
       -> Float                             -- ^ Total error of one iteration (initialised with 0)
       -> Int                               -- ^ Training counter (initialised with 0)
       -> IO (NeuralNetwork)
       
train' nn' [] _ _ _ = return nn'
train' nn' ((input, output):samples) learningRate totalError' trainingIterations' = do
  backpropR <- (backprop nn' input output learningRate totalError' trainingIterations')
  train' (nn backpropR) samples learningRate (totalError backpropR) (totalIterations backpropR)

-- | DataType to store the result of one backpropagation in 'backprop'. 
-- It contains the network the total error and the number of iterations that the network was trained.
data BackpropResult 
    = BackpropResult { nn :: NeuralNetwork , totalError :: Float , totalIterations :: Int }

-- | 'backprop' gets the activations of the given 'NeuralNetwork' from calling 'forwardPass' and applies the calculated updates to the network.
backprop :: NeuralNetwork   -- ^ Network
         -> Matrix Float    -- ^ Input matrix
         -> Matrix Float    -- ^ Output matrix
         -> Float           -- ^ Learing rate
         -> Float           -- ^ Total error
         -> Int             -- ^ Training Counter (Number of training iterations)
         -> IO (BackpropResult)
         
backprop nn input output learningRate totalError totalIterations = do
  
  let err = 0.5 * (sum $ toList (fmap (^2) ((last activations) - output)))
  let updatedNN = apply nn (reverse (gradients (reverse (weights nn)) (reverse (biases nn)) (reverse (init activations)) ((last activations) - output))) learningRate
  
  putStrLn ((show (totalIterations + 1)) ++ ": " ++ show ((totalError + err)/(fromIntegral (totalIterations + 1))))
  
  return (BackpropResult updatedNN (totalError + err) (totalIterations + 1))
    where activations = forwardPass nn input

-- | Applies an list of updates to a given network and returns the updated 'NeuralNetwork'
apply :: NeuralNetwork                  -- ^ Network
      -> [(Matrix Float,Matrix Float)]  -- ^ List of Tupels of weight and bias update matrices
      -> Float                          -- ^ Learing rate
      -> NeuralNetwork                  
      
apply nn updates learningRate = NeuralNetwork (config nn) (update (weights nn) (fst (unzip updates)) learningRate) (update (biases nn) (snd (unzip updates)) learningRate)

-- | 'update' is called by 'apply' to apply one update matrix to either a weight or a bias matrix.
-- Returns the updated list of matrices.
update :: [Matrix Float]    -- ^ matrices to update
       -> [Matrix Float]    -- ^ update matrices
       -> Float             -- ^ Learing rate 
       -> [Matrix Float]
       
update [] _ _ = []
update _ [] _ = []
update (m:matrices) (u:updates) learningRate = m - fmap (*learningRate) u : update matrices updates learningRate

-- | Todo
gradients :: [Matrix Float] 
          -> [Matrix Float] 
          -> [Matrix Float] 
          -> Matrix Float 
          -> [(Matrix Float, Matrix Float)]
          
gradients [] _ _ _ = []
gradients _ [] _ _ = []
gradients _ _ [] _ = []
gradients (w:weights) (b:biases) (a:activations) error = (multStd error' (transpose a), error') : gradients weights biases activations error''
  where error' = multiplyElementwise error (fmap sigmoid' ((multStd w a) + (b)))
        error'' = multStd (transpose w) error'

-- | The 'serialize' function is used to write a network to a specified file path as a binary file. Use 'deserialize' to read this file.
-- Alternatively you can use 'serializePlain' and 'deserializePlain' to save/read as text file.
serialize :: NeuralNetwork  -- ^ Network that should be saved
          -> FilePath       -- ^ Relative path to the file
          -> IO ()          -- ^
          
serialize nn path = do
  BSL.writeFile path (encode nn)

-- | Used to load a network from a binary encoded file
--
-- Example usage:
--
-- @> network <- deserialize "networks/network_3.txt"@
deserialize :: FilePath -- ^ Relative path to the file
            -> IO (NeuralNetwork)
            
deserialize path = do
  nn <- decodeFile path :: IO (NeuralNetwork)
  return nn

-- | Used the same as 'serialize' but saves the network as text instead of binary. 
-- Use 'deserializePlain' to read these files!
serializePlain :: NeuralNetwork -- ^ Network that should be saved
               -> FilePath      -- ^ Relative path to the file
               -> IO ()
               
serializePlain nn path = do
  writeFile path (show (
    [fromIntegral (length (config nn))]
    ++ (map fromIntegral (config nn))
    ++ (concat [ toList ((weights nn)!!i) | i <- [0..((length (config nn))-2)] ])
    ++ (concat [ toList ((biases nn)!!i) | i <- [0..((length (config nn))-2)] ])))

-- | Used the same as 'deserialize' but reads the network as text instead of binary. 
deserializePlain :: FilePath    -- ^ Relative path to the file
                 -> IO (NeuralNetwork)
                 
deserializePlain path = do
  input <- (readFile path)
  let flist = map read (splitOn "," (take ((length (drop 1 input)) - 1) (drop 1 input)))
  let config = map round (take (round (flist!!0)) (drop 1 flist))
  let wstart = drop (1 + (length config)) flist
  let weights = [ fromList (config!!(i+1)) (config!!i) (take ((config!!(i+1)) * (config!!i)) (drop (sum [ (config!!j)*(config!!(j+1)) | j <- [0..i-1]]) wstart)) | i <- [0..((length config)-2)] ]
  let bstart = drop (sum [ (config!!j)*(config!!(j+1)) | j <- [0..((length config)-2)]]) wstart
  let biases = [ fromList (config!!(i+1)) 1 (take ((config!!(i+1))) (drop (sum [ (config!!(j+1)) | j <- [0..i-1]]) bstart)) | i <- [0..((length config)-2)] ]
  return (NeuralNetwork config weights biases)

-- Helper

-- | Generates a IO matrix with the specified dimensions, initialised with a random number in the given range.
randomRMatrix :: Int                -- ^ (m) Number of rows 
              -> Int                -- ^ (n) Number of columns
              -> (Float, Float)     -- ^ Range of the random numbers e.g. (-1,1)
              -> Int                -- ^ Seed for the random generator
              -> IO (Matrix Float)  
              
randomRMatrix rows columns range seed = do
                                    let weights = randomRs range (mkStdGen seed)
                                    return (matrix rows columns (\(row, column) -> weights!!(column+row*columns)))
-- | Generates a matrix with the specified dimensions, initialised 0's.
zeroMatrix :: Int           -- ^ (m) Number of rows
           -> Int           -- ^ (n) Number of columns
           -> Matrix Float
           
zeroMatrix rows columns = matrix rows columns (\(i, j) -> 0.0)

-- | The function 'multiplyElementwise' is used to multiply each element of matrix A with the element at the same position in matrix B.
multiplyElementwise :: Matrix Float -- ^ Matrix A
                    -> Matrix Float -- ^ Matrix B
                    -> Matrix Float
                    
multiplyElementwise m1 m2 = fromList (nrows m1) (ncols m2) (zipWith (*) m1List m2List)
  where m1List = toList m1
        m2List = toList m2

-- | Sigmoid function
sigmoid :: Float -- ^ x
        -> Float -- ^ y
        
sigmoid x = 1 / (1 + exp (-x))

-- | Derivate of Sigmoid funtion
sigmoid' :: Float -- ^ x
         -> Float -- ^ y

sigmoid' x = (sigmoid x) * (1 - (sigmoid x))
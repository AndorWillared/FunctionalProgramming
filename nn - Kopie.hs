import Data.Matrix
import System.Random
import System.IO.Unsafe
import PngToVector
import Mnist_converter
import Data.Tuple.Extra
import System.Random.Shuffle

data NeuralNetwork = NeuralNetwork { weights::[Matrix Float], biases::[Matrix Float] } deriving Show

-- current test-function

getTrainFactor :: Float -> Float
-- standard trainFactor = 1
getTrainFactor x   | x <= 0 = 1
                | otherwise = x

main = do
    let network = initializeNeuralNetwork [784,16,16,10]
--  print network
    putStrLn "|| Initialisation finished"

    print "-------------------"

    putStrLn "Enter train factor:"
    t_in <- readLn
    let t = t_in :: Float
    let trainFactor = getTrainFactor t

    putStrLn "Enter amount of training data :"
    num_in <- readLn
    let num = num_in :: Int
    training_data <- convert_mnist_auto num

    let transformed_td = map (\x -> (fmap (/255) (fst x), numToOut $ snd x)) training_data   -- transforms generated training data to correct format (Matrix, Matrix)
--  putStrLn $ show $ transformed_td

    putStrLn "|| Configuration finished"

    trained_network <- train network transformed_td trainFactor

    putStrLn "Enter path to test picture:"
    pic <- pngToVector

    prediction <- predict trained_network (fmap (/255) pic)    --get prediction from test pic  || ATTENTION: may take VERY long (lazy evaluation)
    putStrLn $ show $  mapToResult $ prediction -- shows result (percent, number)
    return trained_network -- return result vector for further testing (is sum ~ 1 ?)


-- simple test function, to visualize sum of result vector

tester res = sum ( toList res)

 ----

main2 = do
    let nn = initializeNeuralNetwork [784,16,16,10]
    pic <- pngToVector
    res <- predict nn pic
    putStrLn $ show $  mapToResult $ res

--________________________________________________________________________


predict network input = do
                        return ( last (forwardPass network input))

    {-
    main3 = do
    let nn = initializeNeuralNetwork [2,10,2]
    let training_data = getTrainingData 10000
    trained_nn <- train nn training_data 0.1
    temp1 <- predict trained_nn (fromList 2 1 [0, 0])
    temp2 <- predict trained_nn (fromList 2 1 [0, 1])
    temp3 <- predict trained_nn (fromList 2 1 [1, 0])
    temp4 <- predict trained_nn (fromList 2 1 [1, 1])
    putStrLn(show(temp1))
    putStrLn(show(temp2))
    putStrLn(show(temp3))
    putStrLn(show(temp4))-}

getTrainingData :: Int -> [(Matrix Float,Matrix Float)]
getTrainingData n = [(getInput (mod x 4), getOutput (mod x 4)) | x<-[1..n]]
getInput :: Int -> (Matrix Float)
getInput x | x==0 = fromList 2 1 [0.0,0.0] | x==1 = fromList 2 1 [0.0,1.0] | x==2 = fromList 2 1 [1.0,0.0] | otherwise = fromList 2 1 [1.0,1.0]
getOutput :: Int -> (Matrix Float)
getOutput x | x==0 = fromList 2 1 [1.0, 0.0] | x==1 = fromList 2 1 [1.0, 0.0] | x==2 = fromList 2 1 [1.0, 0.0] | otherwise = fromList 2 1 [0.0, 1.0]
-- || Main Functions || --

-- takes list of Integers as an argument,
-- the first element representing the number of nodes in the input layer
-- the second   "           "      "    "     "   "    "  "  output layer
-- the remaining ints represent the num of nodes in the hidden layer(s)

initializeNeuralNetwork :: [Int] -> NeuralNetwork
initializeNeuralNetwork l = NeuralNetwork 
                            [randomMatrix (l!!(i+1)) (l!!i) (-1.0, 1.0) | i <- [0..((length l) -2)]]
                            [zeroMatrix (l!!i) 1 | i <- [1..((length l) -1)]]

-- Functionality: generate Matrix filled with random numbers

-- Arguments:
-- Int - num of rows
-- Int - num of columns
-- (Fl,Fl) - pair representing random range

randomMatrix :: Int -> Int -> (Float,Float) -> Matrix Float
randomMatrix n m b = matrix n m $ \(i,j) -> unsafePerformIO $ getStdRandom $ randomR b


-- Functionality: generate zero-filled Matrix

-- Arguments:
-- Int - num of rows
-- Int - num of columns

zeroMatrix :: Int -> Int -> Matrix Float
zeroMatrix n m = matrix n m $ \(i,j) -> 0.0


-- Functionality: implementation of sigmoid-function

-- Arguments:
-- Int - arg of sigmoid-function

sigmoid :: Float -> Float
sigmoid x = 1 / (1 + exp (-x)) -- 0.5 * (1 + tanh (x/2))

sigmoid' :: Float -> Float
sigmoid' x = (sigmoid x) * (1 - (sigmoid x))


-- Functionality: recursive part of forward pass

-- Arguments:
-- [Matrix Float] - List of weight - matrixes
-- [Matrix Float] - List of bias - matrixes
--  Matrix Float  - activations passed from previous layer (or input)
-- [Matrix Float] - (current) List of activation - matrixes

forward :: [Matrix Float] -> [Matrix Float] -> Matrix Float -> [Matrix Float]

forward [] _ activation = []
forward _ [] activation = []
forward (w:weights) (b:biases) activation = result : (forward weights biases result)
                                            where result = fmap sigmoid (b + (multStd w activation))

-- forwardTest (w:weights) (b:biases) activation = ergebnis where ergebnis = (multStd w activation)


-- Functionality - starter for forwardPass

-- Arguments:
-- NeuralNetwork : state of neural net (before forward pass)
--  Matrix Float : matrix representing input
-- [Matrix Float]: list of activation - matrixes (after completed forward pass9

forwardPass network input = input : forward (weights network) (biases network) input
-- || ------------------------------ ||--


-- [a,b,c,d,e] [1,2,3,4,5] [(a,1),(b,2),(c,3)]


getUpdates [] _ _ _ = []
getUpdates _ [] _ _ = []
getUpdates _ _ [] _ = []
getUpdates (w:r_weights) (b:r_biases) (a:r_activations) error = ((multStd act_error (transpose a)), act_error) : getUpdates r_weights r_biases r_activations (multStd (transpose w) act_error) -- ist das berechnen des nächsten fehlers richtig?
                                                                where act_error = (mul error (fmap sigmoid' ((multStd w a) + b)))

-- getUpdatedValues :: [Matrix Float] -> [(Matrix Float,Matrix Float)] -> Bool -> Float -> [Matrix Float]
-- getUpdatedValues [] _ _ _ = []
-- getUpdatedValues _ [] _ _ = []
-- getUpdatedValues (x:to_update) ((wU,bU):updates) is_bias trainFactor = x - fmap (*trainFactor) (if(is_bias) then bU else wU) : getUpdatedValues to_update updates is_bias trainFactor

getUpdatedWeights :: [Matrix Float] -> [Matrix Float] -> Float -> [Matrix Float]
getUpdatedWeights [] _ _ = []
getUpdatedWeights _ [] _ = []
getUpdatedWeights (w:weights) (wu:weight_updates) trainFactor = w - fmap (*trainFactor) wu : getUpdatedWeights weights weight_updates trainFactor

getUpdatedBiases :: [Matrix Float] -> [Matrix Float] -> Float -> [Matrix Float]
getUpdatedBiases [] _ _ = []
getUpdatedBiases _ [] _ = []
getUpdatedBiases (b:biases) (bu:bias_updates) trainFactor = b - fmap (*trainFactor) bu : getUpdatedBiases biases bias_updates trainFactor

applyUpdates network updates trainFactor = NeuralNetwork (getUpdatedWeights (weights network) (fst(unzip updates)) trainFactor) (getUpdatedBiases (biases network) (snd(unzip updates)) trainFactor)

backprop network input output trainFactor total_error training_iterations = do                               
                                            let err = 0.5 * (sum $ toList (fmap (^2) ((last fp) - output)))
                                            -- putStrLn $ (show err)
                                            let nn = applyUpdates network (reverse (getUpdates (reverse $ weights network) (reverse $ biases network) (reverse $ init fp) ((last fp) - output))) trainFactor
                                            return (nn, total_error + err, training_iterations + 1)
                                            where fp = forwardPass network input

train2 network [] _ _ _= return network
train2 network ((input,output):training_data) trainFactor total_error training_iterations = do                    
                                                          triple <- backprop network input output trainFactor total_error training_iterations
                                                          putStrLn(show((thd3 triple)))
                                                          putStrLn(show((snd3 triple) / (thd3 triple)))
                                                          train2 (fst3 triple) training_data trainFactor (snd3 triple) (thd3 triple)

train network ((input,output):training_data) trainFactor = train2 network ((input,output):training_data) trainFactor 0 0
-- ...


-- convert a given result - Matrix to a Pair representing that result
-- Matrix Float = result - Matrix
-- (Float, Int) = Pair, where 'Float'= certainty of result, 'Int'= actual result


mapToResult :: Matrix Float -> (Float, Int)

mapToResult m | len /= 10 = (0.0 ,(-1))                        -- this is a user - error
              | otherwise = maximum (zip matList [0..9])       -- this will work since maximum on tuples compares fst_s, then snd_s


                where len = length $ matList
                      matList = toList m


-- convert a number to standard out - layer format
-- Int = num to convert
-- Matrix Float = (Matrix Float, 10 1), representing 'Int'

numToOut :: Int -> Matrix Float

 -- ATTENTION only args 0 - 9 lead to sensible results

numToOut int = fromList 10 1 list
               where list = (replicate int 0.0) ++ [1.0] ++ (replicate (10 - 1 - int) 0.0)

-- |Helpers| --

-- Functionality: 'Multiply' two matrixes (not matrix multiplication)

-- Arguments:
-- Matrix Float - input matrix m
-- Matrix Float - input matrix n
-- Matrix Float - result - matrix,
--                  where each field is r_ij = m_ij * n_ij

mul :: Matrix Float -> Matrix Float -> Matrix Float

mul a b = fromList (nrows a) (ncols b) (zipWith (*) listM listN)
           where listM = toList a
                 listN = toList b

-- | ---- | --

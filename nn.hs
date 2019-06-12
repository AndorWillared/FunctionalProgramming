import Data.Matrix
import System.Random
import System.IO.Unsafe


data NeuralNetwork = NeuralNetwork { weights::[Matrix Float], biases::[Matrix Float] } deriving Show

-- current test-function

main = do
    let trainig_data = getTrainingData 1000
    let network = initializeNeuralNetwork [2,3,1]
    print network
    print "-------------------"
    print (forwardPass network (fromList 2 1[2,1]))
    let trained_network = train network trainig_data
    input1 <- getLine
    input2 <- getLine
    let val1 = read input1 :: Float
    let val2 = read input2 :: Float
    print $ last $ forwardPass trained_network (fromList 2 1 [val1,val2])

--________________________________________________________________________

getTrainingData :: Int -> [(Matrix Float,Matrix Float)]
getTrainingData n = [(getInput (mod x 4), getOutput (mod x 4)) | x<-[1..n]]
getInput :: Int -> (Matrix Float)
getInput x | x==0 = fromList 2 1 [0.0,0.0] | x==1 = fromList 2 1 [0.0,1.0] | x==2 = fromList 2 1 [1.0,0.0] | otherwise = fromList 2 1 [1.0,1.0]
getOutput :: Int -> (Matrix Float)
getOutput x | x==0 = fromList 1 1 [0.0] | x==1 = fromList 1 1 [0.0] | x==2 = fromList 1 1 [0.0] | otherwise = fromList 1 1 [1.0]
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
sigmoid x = 0.5 * (1 + tanh (x/2))
sigmoid' x = sigmoid x * (1-sigmoid x)


-- Functionality: recursive part of forward pass

-- Arguments:
-- [Matrix Float] - List of weight - matrixes
-- [Matrix Float] - List of bias - matrixes
--  Matrix Float  - activations passed from previous layer (or input)
-- [Matrix Float] - (current) List of activation - matrixes

forward :: [Matrix Float] -> [Matrix Float] -> Matrix Float -> [Matrix Float]

forward [] _ activation = []
forward _ [] activation = []
forward (w:weights) (b:biases) activation = ergebnis : (forward weights biases ergebnis)
                                            where ergebnis = fmap sigmoid (b + (multStd w activation))

-- forwardTest (w:weights) (b:biases) activation = ergebnis where ergebnis = (multStd w activation)


-- Functionality - starter for forwardPass

-- Arguments:
-- NeuralNetwork : state of neural net (before forward pass)
--  Matrix Float : matrix representing input
-- [Matrix Float]: list of activation - matrixes (after completed forward pass9

forwardPass network input = input : forward (weights network) (biases network) input

-- || ------------------------------ ||--


-- [a,b,c,d,e] [1,2,3,4,5] [(a,1),(b,2),(c,3)]


reshape n m matrix = fromList n m $ toList matrix


getUpdates [] _ _ _ = []
getUpdates _ [] _ _ = []
getUpdates _ _ [] _ = []
getUpdates (w:r_weights) (b:r_biases) (a:r_activations) error = ((multStd act_error (transpose a)), act_error) : getUpdates r_weights r_biases r_activations (multStd (transpose w) act_error) -- ist das berechnen des nächsten fehlers richtig?
                                                                where act_error = (mul error (fmap sigmoid' ((multStd w a) + b)))

getUpdatedValues [] _ _ = []
getUpdatedValues _ [] _ = []
getUpdatedValues (x:to_update) ((wU,bU):updates) is_bias = x - (if(is_bias) then bU else wU) : getUpdatedValues to_update updates is_bias

applyUpdates network updates = NeuralNetwork (getUpdatedValues (weights network) updates False) (getUpdatedValues (biases network) updates True)

backprop network input output = applyUpdates network (reverse $ getUpdates (reverse $ weights network) (reverse $ biases network) (reverse $ init fp) (last fp - output)) where fp = forwardPass network input

q = backprop (initializeNeuralNetwork [2,3,2]) (fromList 2 1 [2.0,1.0]) (fromList 2 1 [1.0,2.0])

train network [] = network
train network ((input,output):trainig_data) = train (backprop network input output) trainig_data;
-- ...

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

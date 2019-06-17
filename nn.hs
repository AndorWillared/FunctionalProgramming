import Data.Matrix
import System.Random
import System.IO.Unsafe


data NeuralNetwork = NeuralNetwork { weights::[Matrix Float], biases::[Matrix Float] } deriving Show

-- current test-function

getTrainFactor :: Float -> Float
-- standard trainFactor = 1
getTrainFactor x   | x <= 0 = 1
                | otherwise = x

main = do
    let trainig_data = getTrainingData 1000
    let network = initializeNeuralNetwork [2,3,1]
    putStrLn "Das Netzwerk vor dem Trainieren:"
    print network
    predict network "====================================="
    putStrLn "Wie groß soll der Trainingsfaktor gewählt werden ?"
    train_in <- getLine
    let trainFactor = getTrainFactor (read train_in :: Float)
    let trained_network = train network trainig_data trainFactor
    putStrLn "Training beendet !"
    putStrLn "Das Netz sieht nun so aus:"
    print trained_network
    putStrLn "==============================="
    predict trained_network "Programm beendet !"


predict :: NeuralNetwork -> [Char] -> IO [Char]

predict net msg =     do
              putStrLn "Geben sie den Input ein, den das Netwzwerk verarbeiten soll (a AND b = ...) !"
              putStrLn "Geben sie Input a ein:"
              input1 <- getLine
              putStrLn " "
              putStrLn "Geben sie Input b ein:"
              input2 <- getLine
              putStrLn " "
              let val1 = read input1:: Float
              let val2 = read input2 :: Float
              let net_out =  last $ forwardPass net (fromList 2 1 [val1,val2])
              let net_percent = head $ toList net_out

              let treshold = 0.5 :: Float
              let result = net_percent > treshold

              putStrLn ("Für den Input "++ (show val1) ++ " AND " ++ (show  val2) ++ " ergibt sich das Ergebnis " ++ (show result))
              let diff = (abs (treshold - net_percent)) *2
              putStrLn ("Das Netz ist sich mit dem Ergebnis zu " ++ (show $ diff*100) ++"% sicher")

              putStrLn "Erneute Eingabe ? (j/n)"
              u_in <- getLine
              let yon = u_in :: [Char]
              putStrLn " "
              if (yon == "j") then (predict net msg) else return msg


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

getUpdatedValues :: [Matrix Float] -> [(Matrix Float,Matrix Float)] -> Bool -> Float -> [Matrix Float]
getUpdatedValues [] _ _ _ = []
getUpdatedValues _ [] _ _ = []
getUpdatedValues (x:to_update) ((wU,bU):updates) is_bias trainFactor = x - fmap (*trainFactor) (if(is_bias) then bU else wU) : getUpdatedValues to_update updates is_bias trainFactor

applyUpdates network updates trainFactor = NeuralNetwork (getUpdatedValues (weights network) updates False trainFactor) (getUpdatedValues (biases network) updates True trainFactor)

backprop network input output trainFactor = applyUpdates network (reverse $ getUpdates (reverse $ weights network) (reverse $ biases network) (reverse $ init fp) (last fp - output)) trainFactor where fp = forwardPass network input

q = backprop (initializeNeuralNetwork [2,3,2]) (fromList 2 1 [2.0,1.0]) (fromList 2 1 [1.0,2.0]) 0.1

train network [] _ = network
train network ((input,output):trainig_data) trainFactor = train (backprop network input output trainFactor) trainig_data trainFactor
-- ...

-- |Helpers| --

-- Functionality: 'Multiply' two matrixes (not matrix multiplication)

-- Arguments:
-- Matrix Float - input matrix m
-- Matrix Float - input matrix n
-- Matrix Float - result - matrix,
--                  where each field is r_ij = m_ij * n_ij

mul :: Matrix Float -> Matrix Float -> Matrix Float

mul m n = fromList (nrows n) (ncols m) (zipWith (*) listM listN)
           where listM = toList m
                 listN = toList n
-- | ---- | --

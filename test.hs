import qualified Data.ByteString.Lazy as B
import Data.Binary

import System.IO.Unsafe
import System.Random
import System.IO
import Data.Matrix

data NeuralNetwork = NeuralNetwork { weights::[Matrix Float], biases::[Matrix Float] } deriving Show
 
instance Binary NeuralNetwork where
    put (NeuralNetwork weights biases) = do    
                                        put $ fmap toList weights
                                        put $ fmap toList biases
  
    get = do 
        raw_weights <- get
        let weights = fmap (fromList (length raw_weights) 1) raw_weights
        raw_biases <- get
        let biases = fmap (fromList (length raw_biases) 1) raw_biases
        return (NeuralNetwork weights biases)
        
save net = do
            let encoded_net = encode net
            B.writeFile "tmp.txt" encoded_net
           
load = do
            net <- decodeFile  "tmp.txt" :: IO NeuralNetwork
            return net

main = do
    let net = createNeuralNetwork [1,2,1]
    putStrLn "#Built net"
    print net

    save net
    putStrLn "#Encoded & Saved net"
    
    x <- load
    putStrLn "#Loaded & Decoded net"
    
    print x

createNeuralNetwork :: [Int] -> NeuralNetwork
createNeuralNetwork layers = NeuralNetwork
                              [randomRMatrix (layers!!(i+1)) (layers!!i) (-1.0, 1.0) | i <- [0..((length layers) -2)]]
                              [zeroMatrix (layers!!i) 1 | i <- [1..((length layers) -1)]]

randomRMatrix :: Int -> Int -> (Float, Float) -> Matrix Float
randomRMatrix rows columns range = matrix rows columns (\(i, j) -> unsafePerformIO (getStdRandom (randomR range)))

zeroMatrix :: Int -> Int -> Matrix Float
zeroMatrix rows columns = matrix rows columns (\(i, j) -> 0.0)
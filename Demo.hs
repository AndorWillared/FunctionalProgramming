import NeuralNetwork
import MNIST

main = do
        net <- demo
        putStrLn "Choose image file: "
        path <- getLine
        pic <- pngToVector path
        let res =  mapToResult $ predict net pic
        putStrLn ("The result is: " ++ show (snd res) ++ "with a confidence of " ++ show (fst res ))


demo = do
        let nn = createNeuralNetwork [784, 16, 16, 10]
        trainingSamples <- getTrainingSamples
        return (train nn trainingSamples 0.1)
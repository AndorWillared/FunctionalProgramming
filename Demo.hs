import NeuralNetwork
import MNIST

main = do
        net <- demo
        putStrLn "Choose image file: "
        path <- getLine
        pic <- pngToVector path
        let res =  mapToResult $ predict net pic
        return res


demo = do
        let nn = createNeuralNetwork [784, 16, 16, 10]
        trainingSamples <- getTrainingSamplesN 5000
        return (train nn trainingSamples 0.1)
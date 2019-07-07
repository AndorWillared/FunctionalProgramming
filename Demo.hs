import X
import MNIST

main = do
        t <- demo
        net <- t
        putStrLn "Choose image file: "
        path <- getLine
        pic <- pngToVector path
        let res =  mapToResult $ predict net pic
        putStrLn $ show res


demo = do
        let nn = createNeuralNetwork [784, 16, 16, 10]
        trainingSamples <- getTrainingSamples
        return (trainVerbose nn trainingSamples 0.1)


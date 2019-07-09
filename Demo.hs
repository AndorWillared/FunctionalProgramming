import NeuralNetwork
import MNIST

demo = do
        let nn = createNeuralNetwork [784, 16, 16, 10]
        trainingSamples <- getTrainingSamples
        return (train nn trainingSamples 0.1)
data NeuralNetwork = NeuralNetwork { weights::[Matrix Float], biases::[Matrix Float] }

load = do
    readFile "/tmp/exp.txt" >>= return . decode :: IO NeuralNetwork
    
save net = do
    writeFile "/tmp/exp.txt" (encode net)
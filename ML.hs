import NeuralNetwork
import MNIST
import Util

generate i testSamples = do 
                        if i < 60000 then
                            vectorToPNG (fst (testSamples!!0)) ("test_"++(show i)++"_" ++ (show (argmax (snd (testSamples!!i)))) ++ ".png")
                            return generate i+1
                        else
                            return

main = do
    testSamples <- getTestSamples
    generate 0 testSamples

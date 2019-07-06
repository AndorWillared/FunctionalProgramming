import System.IO as IO
import Data.Matrix
import Data.ByteString as BS
import Prelude as P


-- FUNCTION TO TEST :

convert_mnist_auto size = mnist_to_trainData "data/train-images" "data/train-labels" size 28 28    -- max size is 60000


-- String, String -> names of the files to be parsed to TrainData
-- Int -> number of TrainDatas to be created
-- Int, Int -> measurements of the images (X*Y)

-- mnist_to_trainData :: String -> String -> Int -> Int -> Int -> [TrainData]

mnist_to_trainData imgFile labelFile count imgX imgY = do
                                              contentLabel <- BS.readFile labelFile
                                              let labels = P.drop imgDrop $ unpack $ BS.take (imgX * imgY * count + imgDrop) contentLabel
                                              matrixes <- mnist_to_matrixes imgFile count imgX imgY
                                              let trainData = P.zip matrixes labels
                                              --IO.putStrLn $ show trainData
                                              IO.putStrLn $ show $ P.length trainData
                                              where imgDrop = 16

--mnist_to_matrixes :: String -> Int-> Int-> Int -> [Matrix Float]

mnist_to_matrixes fileName count imgX imgY = do
                                             content <- BS.readFile fileName
                                             let res = P.drop matDrop $ unpack $ BS.take (imgX * imgY * count + matDrop) content
                                             let matList = P.map (fromList (imgX*imgY) 1) (part res (imgX * imgY))
                                             return matList
                                             where matDrop = 8

-- Helpers

-- Splits List into List of Lists
-- Argument 1: List to split
-- Argument 2: number of elems per List
-- Result: List of Lists (with specified nElems)

part:: (Num a) => [a] -> Int-> [[a]]

part [] _ = []
part list nElems = [P.take (nElems) list] ++ part (P.reverse (P.take ((P.length list) - nElems) (P.reverse list)))  nElems

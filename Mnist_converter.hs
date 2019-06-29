module Mnist_converter where

import System.IO as IO
import Data.Matrix
import Data.ByteString as BS
import Prelude as P
import Data.List.Split


convert_mnist_auto size = mnist_to_trainData "mnist/train-images" "mnist/train-labels" size 28 28    -- max size is 60000

convert_minst' size = mnist_to_trainData "mnist/t10k-images-idx3-ubyte" "mnist/t10k-labels.idx1-ubyte" size 28 28

type TrainData = (Matrix Float, Int)

-- String, String -> names of the files to be parsed to TrainData
-- Int -> number of TrainDatas to be created
-- Int, Int -> measurements of the images (X*Y)

mnist_to_trainData :: String -> String -> Int -> Int -> Int -> IO [TrainData]

mnist_to_trainData imgFile labelFile count imgX imgY = do
                                              matrixes <- mnist_to_matrixes imgFile count imgX imgY
                                              labels <- mnist_to_labellist labelFile count
                                              let trainData = P.zip matrixes labels
                                              return trainData


mnist_to_matrixes :: String -> Int-> Int-> Int -> IO [Matrix Float]

mnist_to_matrixes fileName count imgX imgY = do
                                             imgContent <- BS.readFile fileName
                                             let imgs = unpack $ BS.drop matDrop imgContent
                                             let float_imgs = ( P.map fromIntegral imgs) :: [Float]
                                             let matList = P.map (fromList (imgX*imgY) 1) (chunksOf (imgX * imgY) float_imgs)
                                             return matList
                                             where matDrop = 16     -- drops 16 bytes of file info from start of imgFile

mnist_to_labellist :: String -> Int -> IO [Int]

mnist_to_labellist fileName count = do
                                   labelContent <- BS.readFile fileName
                                   let labels = unpack $ BS.drop labelDrop labelContent
                                   let int_labels = P.map fromIntegral labels
                                   return int_labels
                                   where labelDrop = 8 -- drops 8 bytes of file info from start of labelFile


module MNIST (getTrainingSamples, getTestSamples, pngToVector) where

import System.IO
import qualified Data.ByteString as B
import Data.Matrix
import Data.List.Split

import Codec.Picture.Types
import Codec.Picture.RGBA8
import Codec.Picture.Png

getTrainingSamples :: IO ([(Matrix Float, Matrix Float)])
getTrainingSamples = do
  images <- parseImages "train-images.idx3-ubyte"
  labels <- parseLabels "train-labels.idx1-ubyte"
  return (zip images labels)

getTestSamples :: IO ([(Matrix Float, Matrix Float)])
getTestSamples = do
  images <- parseImages "t10k-images.idx3-ubyte"
  labels <- parseLabels "t10k-labels.idx1-ubyte"
  return (zip images labels)

parseLabels :: FilePath -> IO ([Matrix Float])
parseLabels path = do
  labels <- B.readFile path
  return (map (fromList 10 1) (map toCategorical10 (map fromIntegral (B.unpack (B.drop 8 labels)))))

parseImages :: FilePath -> IO ([Matrix Float])
parseImages path = do
  images <- B.readFile path
  return (map (fmap (/255)) (map (fromList 784 1) (chunksOf 784 (map fromIntegral (B.unpack (B.drop 16 images))))))

pngToVector :: FilePath -> IO (Matrix Float)
pngToVector path = do
  pngData <- B.readFile path
  let decodedPng = decodePng pngData
  case decodedPng of
    Left err -> error (show err)
    Right succ -> return (fromList 784 1 (map fromIntegral ([redChannelAt (fromDynamicImage succ) x y | x <- [0..27], y <- [0..27]])))

-- Helper

toCategorical10 :: Int -> [Float]
toCategorical10 label = [if i == label then 1 else 0 | i <- [0..9]]

redChannelAt :: Image PixelRGBA8 -> Int -> Int -> Int
redChannelAt image x y = redChannel (pixelAt image x y)

redChannel :: PixelRGBA8 -> Int
redChannel (PixelRGBA8 r g b a) = fromIntegral r

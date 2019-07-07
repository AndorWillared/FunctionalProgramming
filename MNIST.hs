module MNIST where

import System.IO
import qualified Data.ByteString as B
import Data.Matrix
import Data.List.Split

import Codec.Picture.Types
import Codec.Picture.RGBA8
import Codec.Picture.Png

getTrainingSamples :: IO [(Matrix Float, Matrix Float)]
getTrainingSamples = do
                      images <- parseImages "data/train-images"
                      labels <- parseLabels "data/train-labels"
                      return (zip images labels)

getTestSamples :: IO [(Matrix Float, Matrix Float)]
getTestSamples = do
                  images <- parseImages "data/test-images"
                  labels <- parseLabels "data/test-labels"
                  return (zip images labels)

parseLabels :: String -> IO [Matrix Float]
parseLabels path = do
                        temp <- B.readFile path
                        return (map (fromList 10 1) (map toCategorical (map fromIntegral (B.unpack (B.drop 8 temp)))))

toCategorical :: Int -> [Float]
toCategorical index = [if i == index then 1 else 0 | i <- [0..9]]

parseImages :: String -> IO [Matrix Float]
parseImages path = do
                        temp <- B.readFile path
                        return (map (fmap (/255)) (map (fromList 784 1) (chunksOf 784 (map fromIntegral (B.unpack (B.drop 16 temp))))))

pngToVector :: String -> IO (Matrix Float)
pngToVector path = do
    fc <- B.readFile path
    let image = (decodePng fc)
    case image of
        Left err -> return (fromList 1 1 [-1.0 :: Float])
        Right msg -> return (fromList 784 1 ([temp msg x y | x <- [0..27], y <- [0..27]]))

getR (PixelRGBA8 r g b a) = r

temp msg x y = (fromIntegral (getR (pixelAt (fromDynamicImage msg) x y)))
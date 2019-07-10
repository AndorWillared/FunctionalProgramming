{-|
Module      : MNIST
Description : Functions for working with the MNIST dataset
License     : MIT
Maintainer  : andor.willared@mni.thm.de
Stability   : experimental

Provides functions for parsing <http://yann.lecun.com/exdb/mnist MNIST> data, aswell as additional functions for working with PNGSs.
-}


module MNIST (
    -- * Parsing
    getTrainingSamples,
    getTestSamples,
    pngToVector,
    -- * Rendering
    vectorToPNG,
    ) where

import System.IO
import qualified Data.ByteString as B
import Data.Matrix
import Data.List.Split

import Codec.Picture.Types
import Codec.Picture.RGBA8
import Codec.Picture.Png


-- Parsing

-- | 'getTrainingSamples' is used to parse the raw MNIST training data to a representation usable in Haskell
--

getTrainingSamples :: FilePath -- ^ path to "train-images.idx3-ubyte"
                   -> FilePath -- ^ path to "train-labels.idx3-ubyte"
                   -> IO ([(Matrix Float, Matrix Float)])   -- ^ Training data as a list of pairs, where fst represents an image and snd the corresponding label

getTrainingSamples pathImgs pathLabels = do
  images <- parseImages pathImgs
  labels <- parseLabels pathLabels
  return (zip images labels)

-- | 'getTestSamples' is used to parse the raw MNIST test data to a representation usable in Haskell
--

getTestSamples :: FilePath   -- ^ path to "t10k-images.idx3-ubyte"
               -> FilePath   -- ^ path to "t10k-labels.idx3-ubyte"
               -> IO ([(Matrix Float, Matrix Float)])  -- ^ Test data as a list of pairs, where fst represents an image and snd the corresponding label

getTestSamples pathImgs pathLabels = do
  images <- parseImages pathImgs
  labels <- parseLabels pathLabels
  return (zip images labels)


parseLabels :: FilePath -> IO ([Matrix Float])
parseLabels path = do
  labels <- B.readFile path
  return (map (fromList 10 1) (map toCategorical10 (map fromIntegral (B.unpack (B.drop 8 labels)))))

parseImages :: FilePath -> IO ([Matrix Float])
parseImages path = do
  images <- B.readFile path
  return (map (fmap (/255)) (map (fromList 784 1) (chunksOf 784 (map fromIntegral (B.unpack (B.drop 16 images))))))



-- Png

-- |  'pngToVector' takes a file path and parses it to an equivalent float matrix
--

pngToVector :: FilePath     -- ^ path to a .png file
            -> IO (Matrix Float)    -- ^ float matrix representing the input file

pngToVector path = do
  pngData <- B.readFile path
  let decodedPng = decodePng pngData
  case decodedPng of
    Left err -> error (show err)
    Right succ -> return (fromList 784 1 (map fromIntegral ([redChannelAt (fromDynamicImage succ) x y | y <- [0..27], x <- [0..27]])))

-- | 'vectorToPng' takes a float matrix and a file path, creates an image representation of the input matrix
--

vectorToPNG :: Matrix Float     -- ^ float matrix to write
            -> FilePath     -- ^ path to write the .png to
            -> IO()

vectorToPNG vector path = writePng path (generateImage (grayscaleAt vector) 28 28)

grayscaleAt :: Matrix Float -> Int -> Int -> PixelRGBA8
grayscaleAt vector x y = PixelRGBA8 grayscale grayscale grayscale 255
  where grayscale = round ((getElem (x+y*28+1) 1 vector)*255)

-- Helper

toCategorical10 :: Int -> [Float]
toCategorical10 label = [if i == label then 1 else 0 | i <- [0..9]]

redChannelAt :: Image PixelRGBA8 -> Int -> Int -> Int
redChannelAt rgba8 x y = redChannel (pixelAt rgba8 x y)

redChannel :: PixelRGBA8 -> Int
redChannel (PixelRGBA8 r g b a) = fromIntegral r

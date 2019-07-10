module Util where

import Data.Matrix
import System.Random

{-
shuffleSamples :: [(Matrix Float, Matrix Float)] -> Int -> [(Matrix Float, Matrix Float)]
shuffleSamples samples seed = shuffle' samples (length samples) (mkStdGen seed)
 -}

--TODO: Insert missing function shuffle'

argmax :: Matrix Float -> Int
argmax matrix = snd (maximum (zip (toList matrix) [0..(length (toList matrix))]))

toCategorical :: Int -> Int -> [Float]
toCategorical label classes = [if i == label then 1 else 0 | i <- [0..classes-1]]
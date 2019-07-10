{-|
Module      : Util
Description : A haskell implementation of a neural network
License     : MIT
Maintainer  : andor.willared@mni.thm.de
              felix.willared@mni.thm.de
              marco.herzog@mni.thm.de
              adiel.ahmad@mni.thm.de
              jannis.weber@mni.thm.de
Stability   : experimental

Util functions for Machine Learning.
-}

module Util (
  argmax,
  toCategorical,
  shuffle,
  reshape
) where

import Data.Matrix
import System.Random
import System.Random.Shuffle (shuffle')

-- | 'argmax' takes a Nx1 matrix and returns the index of the highest value
-- | important: this function is undefined for NxM matrices
-- __For example:__
--
-- @> argmax (fromList 4 1 [0,42,1,17]) -- returns 1
--

argmax :: Matrix Float -- ^ matrix
       -> Int          -- ^ argmax

argmax matrix = snd $ maximum $ zip (toList matrix) [0..(length (toList matrix))]

-- | 'toCategorical' converts a class to a binary class matrix
-- __For example:__
--
-- @> toCategorical 1 3 -- returns [0,1,0]
--

toCategorical :: Int          -- ^ class
              -> Int          -- ^ number of classes
              -> Matrix Float -- ^ binary class matrix

toCategorical _class numOfClasses = fromList numOfClasses 1 [if i == _class then 1 else 0 | i <- [0..numOfClasses-1]]

-- | 'shuffle' shuffles a list
-- __For example:__
--
-- @> shuffle [1,2,3] 42 -> returns [3,2,1]
--

shuffle :: [a] -- ^ unshuffled list
        -> Int -- ^ seed
        -> [a] -- ^ shuffled list

shuffle list seed = shuffle' list (length list) (mkStdGen seed)

reshape :: Matrix a -- ^ matrix
        -> Int      -- ^ rows
        -> Int
        -> Matrix a
reshape matrix rows columns = fromList rows columns (toList matrix)


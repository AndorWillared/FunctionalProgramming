module PngToVector where

import Data.ByteString as BS
import Codec.Picture.Png
import Codec.Picture.Types
import System.IO as IO
import Data.Either
import Codec.Picture.RGBA8
import Data.Matrix

pngToVector = do
    input <- IO.getLine
    fc <- BS.readFile input
    let image = (decodePng fc)
    case image of
        Left err -> return (fromList 1 1 [-1.0 :: Float])
        Right msg -> return (fromList 784 1 ([temp msg x y | x <- [0..27], y <- [0..27]]))

getR (PixelRGBA8 r g b a) = r

temp msg x y = (fromIntegral (getR (pixelAt (fromDynamicImage msg) x y)))




module Img where

import Codec.Picture
import Codec.Picture.Types
import Data.Word

import ComonadList
import PeanoNum
import Logic ( result
             )

imgRun :: Show a =>
     PeanoNum
     -> p
     -> (Word8 -> Word8 -> Word8 -> Word8)
     -> Cycle Word8
     -> a
     -> IO ()
imgRun ls b r s rn = savePngImage (imgPath rn) . ImageY8 $
                        generateImage (genPixel r s)
                                      ((length s) * 2 + 1)
                                      $ fromNat ls
  where
    imgPath r = "img/rule" ++ show r ++ ".png"
    genPixel r s x y = showPixel $ result showPixel r s !! y !! x

    showPixel :: Word8 -> Pixel8
    showPixel 0 = 0
    showPixel 1 = 255
    showPixel _ = 127

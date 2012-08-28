{-# LANGUAGE ExistentialQuantification #-}
module Img where

import Data.Word
import Codec.Picture
import Data.Vector.Storable

getData :: DynamicImage -> Vector Word8
getData (ImageY8     i) = imageData i
getData (ImageYA8    i) = imageData i
getData (ImageRGB8   i) = imageData i
getData (ImageRGBA8  i) = imageData i
getData (ImageYCbCr8 i) = imageData i



some :: a -> Maybe a -> a
some a (Just b) = b
some a Nothing = a
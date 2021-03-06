{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Codec.Picture 
import System.Environment
import Codec.Picture.Types 

main = do
    args <- getArgs
    case args of
        [input, out] -> run input out
        _            -> print "Fuck"

run :: String -> String -> IO ()
run fileIn fileOut = do
    image <- readPng fileIn
    either 
        putStrLn
        (withImg (print . extractLumaPlane))
        image

withImg :: (forall a. LumaPlaneExtractable a => Image a -> b) -> DynamicImage -> b
withImg fun (ImageY8 i) = fun i
withImg fun (ImageYA8 i) = fun i  
withImg fun (ImageRGB8 i) = fun i
withImg fun (ImageRGBA8 i) = fun i  
withImg fun (ImageYCbCr8 i) = fun i


instance Show (Image Pixel8) where
    show img = show $ imageData img

        






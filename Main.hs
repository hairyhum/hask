{-# LANGUAGE PackageImports, BangPatterns, QuasiQuotes #-}
module Main where

import Codec.Picture
import System.Environment
import Data.ByteString

main = do
    args <- getArgs
    case args of
        [input, out] -> run input out
        _            -> print "Fuck"

run fileIn fileOut = do
    image <- readPng fileIn
--    (width, height) <- imageSize image
    --map (\x -> process_line x image) [0..width]
    print $ either (id) (encodeImg) image



encodeImg :: DynamicImage -> String
encodeImg a =
    let encoded = encodeDynamicPng a
    in either (id) (show) encoded



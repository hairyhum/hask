{-# LANGUAGE TemplateHaskell #-}
module Example1 where
import Language.Haskell.SealModule
import Control.Monad

sealModule [d|
    verbose :: Bool
    verbose = sealedParam

    param :: Int
    param = sealedParam

    worker :: Int -> Int
    worker n =
        if verbose then n else 0

    withParam :: Int -> Int
    withParam a =
        if verbose then param else a
    |]

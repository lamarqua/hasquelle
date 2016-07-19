module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    file <- readFile (head args)
    dict <- readFile (head $ tail args)
    putStrLn (translateHaskell (loadDictionary dict) file)

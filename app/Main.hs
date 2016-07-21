module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if (length args < 2)
        then putStrLn "hasquelle fichier_a_traduire.hsq dictionnaire.txt"
        else do 
                file <- readFile (head args)
                dict <- readFile (head $ tail args)
                putStrLn (translateHaskell (loadDictionary dict) file)


module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    a <- getExecutablePath
    putStrLn a
    file <- readFile "/Users/adrien/dev/haskell/hasquelle/test/test.hs"
    putStrLn (translateHaskell file)


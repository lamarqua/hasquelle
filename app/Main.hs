module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    -- programName <- getExecutablePath
    args <- getArgs
    -- putStrLn programName
    -- mapM putStrLn args
    file <- readFile (head args) --"/Users/adrien/dev/haskell/hasquelle/test/test.hs"
    putStrLn (translateHaskell file)

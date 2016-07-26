import Control.Applicative
import Control.Monad
import System.IO
import Data.List

chopsticks :: [Int] -> IO ()
chopsticks [] = return ()
chopsticks xs = do
    putStrLn (show . length $ xs)
    let min = head xs
    chopsticks [x - min | x <- xs, x /= min]

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    arr_temp <- getLine
    let arr = map read $ words arr_temp :: [Int]
    if length arr == n then chopsticks (Data.List.sort arr) else return ()

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- getMultipleLines (n-1)
        let ret = (x:xs)
        return ret



module Main where

import Lib (maxProfit)
import System.IO (putStrLn)
import System.Environment (getArgs)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [pricesStr, feeStr] ->
            let prices = fmap read (splitOn " " pricesStr) :: [Int]
                fee    = read feeStr :: Int
            in print (maxProfit fee prices)
        _ -> putStrLn "Usage: maxProfit \"2 3 5 15 24\" \"2\""
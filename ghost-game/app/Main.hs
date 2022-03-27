module Main where

import Lib (calculateFirstLetters, parseDictionary)
import System.IO (putStrLn)

main :: IO ()
main = do
    dict <- parseDictionary ""
    let res = calculateFirstLetters dict
    print res

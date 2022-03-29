module Main where

import Lib (calculateFirstLetters, parseDictionary)
import System.IO (putStrLn)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      dict <- parseDictionary filePath
      let res = calculateFirstLetters dict
      print res
    _ -> putStrLn "Usage: playGhost <path_to_dictionary_file>"

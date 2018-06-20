module Main where

import System.Environment (getArgs)
import ProgramRunner (process)

main :: IO ()
main = do args   <- getArgs
          result <- process args
          putStrLn result

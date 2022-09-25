module Main (main) where

import System.Environment (getArgs)
import HBrainfuck (evalAndPrint, runRepl)

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> evalAndPrint $ args !! 0
            _ -> putStrLn "Pass no args for REPL, 1 for one-time evaluation."

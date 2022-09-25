module HBrainfuck.Internal.Actions
  ( runRepl
  , printReplStartMsg
  , doUntil
  , readPrompt
  , evalAndPrint
  ) where

import System.IO (hFlush, stdout)
import Control.Monad.Except (catchError)
import Data.Either (fromRight)

import HBrainfuck.Internal.Interpreter (runBF)


runRepl :: IO ()
runRepl = printReplStartMsg >> doUntil (== "quit") (readPrompt "brainfuck> ")
                                       evalAndPrint (putStrLn "")

printReplStartMsg :: IO ()
printReplStartMsg = putStrLn "--- Brainfuck ---\nPrint 'quit' to exit."

doUntil :: Monad m => (a -> Bool) -> m a -> (a -> m b) -> m b -> m b
doUntil prd val action end = do v <- val
                                if prd v
                                  then end
                                   else action v >> doUntil prd val action end  -- TODO refactor with >>=

readPrompt :: String -> IO String
readPrompt s = (putStr s) >> hFlush stdout >> getLine

evalAndPrint :: String -> IO ()
evalAndPrint s = putStrLn . fromRight "Unknown error" $ result
  where result = catchError (runBF s) (Right . show)

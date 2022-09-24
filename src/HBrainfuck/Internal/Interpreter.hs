{-# LANGUAGE TemplateHaskell #-}
module HBrainfuck.Internal.Interpreter
  ( IntMap
  , BFState (..)
  , CalcError (..)
  , initState
  , runBF
  , propagateError
  , evalAST
  , evalAST'
  , chainEval
  , stepEval
  , current
  ) where

import Data.Char (chr)
import qualified Data.Map as M
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Lazy
import Lens.Micro.Platform ((+=) , (%=) , makeLenses , use)
import Text.ParserCombinators.Parsec (ParseError, parse)
import HBrainfuck.Internal.AST (BFValue (..))
import HBrainfuck.Internal.Parser (parseBFProgram)


type IntMap = M.Map Int Int

data BFState = BFState { _pos  :: Int
                       , _tape :: IntMap
                       } deriving (Show)

makeLenses ''BFState

data CalcError = IllegalComma
               | Parser ParseError deriving (Eq)

instance Show CalcError where
  show IllegalComma = "Comma not allowed in non-interactive interpreter."
  show (Parser e) = show e

initState :: BFState
initState = BFState { _pos = 0, _tape = M.empty }

runBF :: String -> Either CalcError String
runBF src = propagateError (evalAST <$> parsed)
    where parsed = parse parseBFProgram "Brainfuck" src

type InterpreterState = ExceptT CalcError (State BFState)

propagateError :: Either ParseError (Either CalcError a) -> Either CalcError a
propagateError (Left e) = Left . Parser $ e
propagateError (Right a) = a

evalAST :: [BFValue] -> Either CalcError String
evalAST = evalAST' initState

evalAST' :: BFState -> [BFValue] -> Either CalcError String
evalAST' s xs = evalState (runExceptT (chainEval xs)) s

chainEval :: [BFValue] -> InterpreterState String
chainEval xs = concat <$> forM xs stepEval

stepEval :: BFValue -> InterpreterState String
stepEval Gt = pos += 1 >> return ""
stepEval Lt = pos += (-1) >> return ""
stepEval Plus = do p <- use pos
                   tape %= M.insertWith (+) p 1
                   return ""
stepEval Minus = do p <- use pos
                    tape %= M.insertWith (+) p (-1)
                    return ""
stepEval Dot = (return . chr) <$> current
stepEval v@(Brackets cs) = do pre <- current
                              if pre == 0
                              then return ""
                              else do out <- chainEval cs
                                      post <- current
                                      if post == 0
                                      then return out
                                      else do out_post <- stepEval v
                                              return $ out ++ out_post
stepEval Comma = throwError IllegalComma

current :: InterpreterState Int
current = do p <- use pos
             t <- use tape
             let val = M.findWithDefault 0 p t
             return val

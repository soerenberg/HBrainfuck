{-# LANGUAGE TemplateHaskell #-}
module HBrainfuck.Internal.Interpreter
  ( BFState (..)
  , initState
  , runBF
  , evalAST
  , evalAST'
  , chainEval
  , stepEval
  ) where

import Data.Char (chr)
import Data.Either (fromRight)
import qualified Data.Map as M
import Control.Monad.State.Lazy
import Lens.Micro.Platform ((+=) , (%=) , makeLenses , use)
import Text.ParserCombinators.Parsec (parse)
import HBrainfuck.Internal.AST (BFValue (..))
import HBrainfuck.Internal.Parser (parseBFProgram)


type IntMap = M.Map Int Int

data BFState = BFState { _pos  :: Int
                       , _tape :: IntMap
                       } deriving (Show)

makeLenses ''BFState

initState :: BFState
initState = BFState { _pos = 0, _tape = M.empty }

runBF :: String -> String
runBF src = evalAST $ fromRight [] parsed
    where parsed = parse parseBFProgram "Brainfuck" src

evalAST :: [BFValue] -> String
evalAST = evalAST' initState

evalAST' :: BFState -> [BFValue] -> String
evalAST' s xs = evalState (chainEval xs) s

chainEval :: [BFValue] -> State BFState String
chainEval xs = concat <$> forM xs stepEval


stepEval :: BFValue -> State BFState String
stepEval Gt = do pos += 1
                 return ""
stepEval Lt = pos += (-1) >> return ""
stepEval Plus = do p <- use pos
                   tape %= M.insertWith (+) p 1
                   return ""
stepEval Minus = do p <- use pos
                    tape %= M.insertWith (+) p (-1)
                    return ""
stepEval Dot = do p <- use pos
                  t <- use tape
                  let x = M.findWithDefault 0 p t
                  return [chr x]
stepEval v@(Brackets cs) = do pre <- current
                              if pre == 0
                              then return ""
                              else do out <- chainEval cs
                                      post <- current
                                      if post == 0
                                      then return out
                                      else do out_post <- stepEval v
                                              return $ out ++ out_post
-- TODO: throwError for case `stepEval Comma`

current :: State BFState Int
current = do p <- use pos
             t <- use tape
             let val = M.findWithDefault 0 p t
             return val

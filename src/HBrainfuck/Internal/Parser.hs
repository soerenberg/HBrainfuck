module HBrainfuck.Internal.Parser
  ( parseBFProgram
  , parseBFValue
  , parseBrackets
  ) where

import Text.ParserCombinators.Parsec
  ( Parser
  , char
  , eof
  , many
  , manyTill
  , oneOf
  , spaces
  )

import HBrainfuck.Internal.AST (BFValue (..))


parseBFProgram :: Parser [BFValue]
parseBFProgram = many parseBFValue <* eof

parseBFValue :: Parser BFValue
parseBFValue = do c <- oneOf "><+-.,["
                  case c of
                    '>' -> return Gt
                    '<' -> return Lt
                    '+' -> return Plus
                    '-' -> return Minus
                    '.' -> return Dot
                    ',' -> return Comma
                    '[' -> spaces >> parseBrackets
                    _ -> fail $ "Unknown character: " ++ show c
               <* spaces

parseBrackets :: Parser BFValue
parseBrackets = do xs <- manyTill parseBFValue $ char ']'
                   return . Brackets $ xs
                <* spaces

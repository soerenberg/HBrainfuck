module HBrainfuck.Internal.AST
  ( BFValue(..)
  ) where

data BFValue = Gt
             | Lt
             | Plus
             | Minus
             | Dot
             | Comma
             | Brackets [BFValue] deriving (Eq)

instance Show BFValue where
  show Gt            = ">"
  show Lt            = "<"
  show Plus          = "+"
  show Minus         = "-"
  show Dot           = "."
  show Comma         = ","
  show (Brackets xs) = "[" ++ (xs >>= show) ++ "]"

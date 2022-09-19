import HBrainfuck.Internal.AST (BFValue (..))
import HBrainfuck.Internal.Parser (parseBFProgram, parseBFValue, parseBrackets)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ showBFValueTests
  , parseBFValueTests
  , parseBracketsTests
  , parseEndToEndTests
  ]

showBFValueTests :: TestTree
showBFValueTests = testGroup "ShowBFValueTests"
  [ testCase "show Gt" $ (show Gt) @?= ">"
  , testCase "show Lt" $ show Lt @?= "<"
  , testCase "show Plus" $ show Plus @?= "+"
  , testCase "show Minus" $ show Minus @?= "-"
  , testCase "show Dot" $ show Dot @?= "."
  , testCase "show Comma" $ show Comma @?= ","
  , testCase "show Brackets []" $ show (Brackets []) @?= "[]"
  , testCase "show Brackets [Dot, Comma]" $
      show (Brackets [Dot, Comma]) @?= "[.,]"
  ]

parseBFValueTests :: TestTree
parseBFValueTests = testGroup "ParseBFValueTests"
  [ testCase "parseBFValue '>'" $
      parse parseBFValue "Brainfuck" ">" @?= Right Gt
  , testCase "parseBFValue '<'" $
      parse parseBFValue "Brainfuck" "<" @?= Right Lt
  , testCase "parseBFValue '+'" $
      parse parseBFValue "Brainfuck" "+" @?= Right Plus
  , testCase "parseBFValue '-'" $
      parse parseBFValue "Brainfuck" "-" @?= Right Minus
  , testCase "parseBFValue '.'" $
      parse parseBFValue "Brainfuck" "." @?= Right Dot
  , testCase "parseBFValue ','" $
      parse parseBFValue "Brainfuck" "," @?= Right Comma
  , testCase "parseBFValue '+ '" $
      parse parseBFValue "Brainfuck" "+ " @?= Right Plus
  , testCase "parseBFValue '- '" $
      parse parseBFValue "Brainfuck" "- " @?= Right Minus
  , testCase "parseBFValue '.   '" $
      parse parseBFValue "Brainfuck" ".  " @?= Right Dot
  ]

parseBracketsTests :: TestTree
parseBracketsTests = testGroup "ParseBracketsTest"
  [ testCase "parseBrackets '.]'" $
      parse parseBrackets "Brainfuck" ".]" @?= Right (Brackets [Dot])
  , testCase "parseBrackets ']'" $
      parse parseBrackets "Brainfuck" "]" @?= Right (Brackets [])
  , testCase "parseBrackets '+ -]'" $
      parse parseBrackets "Brainfuck" "+ -]" @?= Right (Brackets [Plus, Minus])
  , testCase "parseBrackets '+ - ]'" $
      parse parseBrackets "Brainfuck" "+ - ]" @?= Right (Brackets [Plus, Minus])
  ]

parseEndToEndTests :: TestTree
parseEndToEndTests = testGroup "ParseEndToEndTest"
  [ testCase "parse '> +-'" $
      parse parseBFProgram "Brainfuck" "> +-" @?= Right [Gt, Plus, Minus]
  , testCase "parse '> +- []'" $ parse parseBFProgram "Brainfuck" "> +- []" @?=
      Right [Gt, Plus, Minus, Brackets []]
  , testCase "parse '> +- [ ]'" $
      parse parseBFProgram "Brainfuck" "> +- [ ]" @?=
      Right [Gt, Plus, Minus, Brackets []]
  , testCase "parse '> >> - [ .,  ]'" $
      parse parseBFProgram "Brainfuck" "> >> - [ .,  ]  " @?=
      Right [Gt, Gt, Gt, Minus, Brackets [Dot, Comma]]
  ]

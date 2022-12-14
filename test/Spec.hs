import Data.Char (chr, ord)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Text.ParserCombinators.Parsec (parse)
import HBrainfuck.Internal.AST (BFValue (..))
import HBrainfuck.Internal.Interpreter (CalcError (..), runBF)
import HBrainfuck.Internal.Parser (parseBFProgram, parseBFValue, parseBrackets)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ showBFValueTests
  , parseBFValueTests
  , parseBracketsTests
  , parseEndToEndTests
  , fullEndToEndTests
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

-- Brainfuck code to print 'Hello World!\n', source: Wikipedia
hello_world_src :: String
hello_world_src = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---."
                  ++ "+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

-- Shift small ASCII values by 48 values to produce printable characters
shift48 :: String -> String
shift48 = map $ chr . (+ 48) . ord

fullEndToEndTests :: TestTree
fullEndToEndTests = testGroup "FullEndToEndTest"
  [ testCase "interpret ''" $ runBF "" @?= Right ""
  , testCase "interpret '+++.-.[.-]..'" $
    (shift48 <$> runBF "+++.-.[.-]..") @?= Right "322100"
  , testCase "interpret 42 '+'s" $ runBF ((take 42 $ repeat '+') ++ ".") @?=
    Right "*"
  , testCase "interpret 'Hello World!'" $
      runBF hello_world_src @?= Right "Hello World!\n"
  , testCase "interpret illegal comma expr" $
      runBF "+++.+,.+" @?= Left IllegalComma
  , testCase "interpret illegal '['" $
      case runBF "[" of
        (Left (Parser _)) -> return ()
        x -> assertFailure $ "Expected Left (Parser _) error, but got "
                             ++ show x ++ "."
  ]

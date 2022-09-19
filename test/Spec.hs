import HBrainfuck.Internal.AST (BFValue (..))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ showBFValueTests
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

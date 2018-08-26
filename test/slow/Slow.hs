import NumberPuzzle

import Data.List (nub, sort)

import Test.Tasty
import Test.Tasty.HUnit (testCase, assertEqual)

testManualSolutions :: [TestTree]
testManualSolutions =
    map (\(digs, want, answers) -> testCase (show digs) $ assertEqual "" answers (map show (solve want digs))) [
    ([1, 3, 4, 6], 24, ["4*((1/3)+6)",
                        "((1/3)+4)*6",
                        "(1/3)+(4*6)",
                        "((1^4)+3)*6",
                        "(4-(1/3))*6",
                        "4*(6-(1/3))",
                        "(4*6)-(1/3)",
                        "(1^3)*(4*6)",
                        "(1^3)*4*6",
                        "(4*6)/(1^3)",
                        "(4/(1^3))*6",
                        "4*(6/(1^3))",
                        "(4*6)^(1^3)",
                        "(4^(1^3))*6",
                        "4*(6^(1^3))"]),
    ([5, 5, 5, 1], 24, ["(5*5)-(1^5)"])]


tests :: [TestTree]
tests = [
  testGroup "manual solutions" testManualSolutions
  ]

main :: IO ()
main = defaultMain $ testGroup "Slow Tests" tests

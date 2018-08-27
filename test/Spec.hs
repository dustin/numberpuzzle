{-# LANGUAGE OverloadedStrings #-}

import NumberPuzzle

import Control.Monad (forM_)
import Data.Semigroup ((<>))
import Data.Maybe (isJust)
import Data.Either (fromRight)
import Data.List (nub, sort, intercalate)
import Text.Megaparsec (parse)

import Test.Tasty
import Test.QuickCheck
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty.QuickCheck as QC
import Test.Invariant ((<=>))
import Data.Text (Text, pack, unpack)


newtype Values = Values [Value]

instance Show Values where
  show (Values v) = (intercalate " " . map show) v

instance Arbitrary Values where
  arbitrary = do
    n <- choose(2, 8)
    (d1:d2:digs) <- vectorOf n (Val <$> choose (0,9))
    ops <- vectorOf (pred n) (oneof $ map pure operators)
    rest <- shuffle (digs <> ops)
    pure $ Values (d1:d2:rest)


instance Arbitrary Expression where
  arbitrary = do
    op <- oneof $ map (\(NumberPuzzle.Fun x) -> pure (EFun x)) operators
    s1 <- subex
    s2 <- subex
    pure $ op [s1, s2]

    where
      subex :: Gen Expression
      subex = frequency [
        (3, EVal <$> choose (0,9)),
        (1, arbitrary)
        ]

showSamples :: IO ()
showSamples = do
  samples <- sample' (arbitrary :: Gen Expression)
  forM_ (sort samples) (\s -> do
                           (putStr.show) s
                           putStr " -> "
                           (putStr.show.canonicalizeexpr) s
                           putStr " -> "
                           (print.rpnify.canonicalizeexpr) s
                       )

prop_dedup :: Values -> Bool
prop_dedup (Values v) = nub v == dedup v

prop_dedup2 :: [Values] -> Bool
prop_dedup2 vals = let v = map (\(Values v) -> v) vals in
                     nub v == dedup v

prop_dedup_expr :: [Expression] -> Bool
prop_dedup_expr = nub <=> dedup

prop_exprcanon :: Expression -> Bool
prop_exprcanon = evalexpr <=> evalexpr.canonicalizeexpr

prop_newform :: Values -> Bool
prop_newform (Values v) = (evalexpr =<< exprify v) == eval v

prop_rpnform :: Expression -> Bool
prop_rpnform = evalexpr <=> eval.rpnify

testManualSolutions :: [TestTree]
testManualSolutions =
    map (\(digs, want, answers) -> testCase (show digs) $ assertEqual "" answers (map show (solve want digs))) [
    ([5, 5, 5, 1], 24, ["(5*5)-(1^5)"]),
    ([1, 3, 4, 6], 24, ["4*((1/3)+6)",
                        "((1/3)+4)*6",
                        "(1/3)+(4*6)",
                        "((1^4)+3)*6",
                        "(4-(1/3))*6",
                        "4*(6-(1/3))",
                        "(4*6)-(1/3)",
                        "(1^3)*4*6",
                        "(4*6)/(1^3)",
                        "(4/(1^3))*6",
                        "4*(6/(1^3))",
                        "(4*6)^(1^3)",
                        "(4^(1^3))*6",
                        "4*(6^(1^3))"])]


testManualCanon :: [TestTree]
testManualCanon =
  map (\(expr, want) -> testCase (show expr) $ assertEqual "" want (show $ (canonicalizeexpr.p) expr)) [
  ("(1^3)*(4*6)", "(1^3)*4*6"),
  ("(4*6)*(1^3)", "(1^3)*4*6"),
  ("((1^3)*(4*6))", "(1^3)*4*6"),
  ("(4*6)*(1*3)", "1*3*4*6"),
  ("((4*6))*((1*3))", "1*3*4*6")
  ]
  where
    p :: Text -> Expression
    p = fromRight undefined . parse parseExpr ""

testRPNParser :: [TestTree]
testRPNParser =
  map (\(t, want) -> testCase (unpack t) $ assertEqual "" want (eval <$> parse parseRPN "" t)) [
  ("2 3 +", Right (Just 5)),
  ("1 2 3 + *", Right (Just 5)),
  ("1 2 ^ 3 *", Right (Just 3))
  ]

testExprParser :: [TestTree]
testExprParser =
  map (\(t, want) -> testCase (unpack t) $ assertEqual "" want (show <$> parse parseExpr "" t)) [
  ("1+2+3", Right "(1+2)+3"),
  ("1+2*3-4/5^2", Right "(1+(2*3))-(4/(5^2))")
  ]

prop_showParseEvalExpr :: Expression -> Bool
prop_showParseEvalExpr x = Right (evalexpr x) == (evalexpr <$> parse parseExpr "" (pack $ show x))

prop_showParseEvalRPN :: Values -> Bool
prop_showParseEvalRPN v@(Values x) = Right (eval x) == (eval <$> parse parseRPN "" (pack $ show v))

tests :: [TestTree]
tests = [
  testProperty "dedups [Value]" prop_dedup,
  testProperty "dedups [[Value]]" prop_dedup2,
  testProperty "dedups Expression" prop_dedup_expr,
  testProperty "new form works" prop_newform,
  testProperty "expr canon" prop_exprcanon,
  testProperty "rpn expr" prop_rpnform,

  testProperty "expr show parse eval" prop_showParseEvalExpr,
  testProperty "rpn show parse eval" prop_showParseEvalRPN,

  testGroup "rpn parsing" testRPNParser,
  testGroup "expr parsing" testExprParser,
  testGroup "manual canonicalization" testManualCanon,
  testGroup "manual solutions" testManualSolutions

  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests

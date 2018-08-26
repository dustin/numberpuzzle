{-# LANGUAGE OverloadedStrings #-}

import NumberPuzzle

import Control.Monad (forM_)
import Data.Semigroup ((<>))
import Data.Maybe (isJust)
import Data.List (nub, sort)
import qualified Data.Attoparsec.Text as A

import Test.Tasty
import Test.QuickCheck
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty.QuickCheck as QC
import Test.Invariant ((<=>))
import Data.Text (unpack)


newtype Values = Values [Value]

instance Show Values where
  show (Values v) = show v

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

prop_canon :: Values -> Property
prop_canon (Values v) = (isJust . eval) v ==> eval v == (eval . canonicalize) v

prop_exprcanon :: Expression -> Bool
prop_exprcanon = evalexpr <=> evalexpr.canonicalizeexpr

prop_newform :: Values -> Bool
prop_newform (Values v) = (evalexpr =<< exprify v) == eval v

prop_rpnform :: Expression -> Bool
prop_rpnform = evalexpr <=> eval.rpnify

testManualSolutions :: [TestTree]
testManualSolutions =
    map (\(digs, want, answers) -> testCase (show digs) $ assertEqual "" answers (map show (solve want digs))) [
    ([5, 5, 5, 1], 24, ["(5*5)-(1^5)"])]


testManualCanon :: [TestTree]
testManualCanon =
  map (\(expr, want) -> testCase (show expr) $ assertEqual "" want (show $ canonicalizeexpr expr)) [
  ((EFun ("*", pw (*)) [EFun ("^", pw (^)) [EVal 1, EVal 3],
                        EFun ("*", pw (*)) [EVal 4, EVal 6]]),
    "(1^3)*4*6"),
  ((EFun ("*", pw (*)) [
       EFun ("*", pw (*)) [EVal 4, EVal 6],
       EFun ("^", pw (^)) [EVal 1, EVal 3]
       ]),
    "(1^3)*4*6"),
  ((EFun ("*", pw (*)) [
       EFun ("*", pw (*)) [
           EFun ("^", pw (^)) [EVal 1, EVal 3],
           EFun ("*", pw (*)) [EVal 4, EVal 6]
           ]]),
    "(1^3)*4*6"),
  ((EFun ("*", pw (*)) [
       EFun ("*", pw (*)) [EVal 4, EVal 6],
       EFun ("*", pw (*)) [EVal 1, EVal 3]
       ]),
    "1*3*4*6"),
  ((EFun ("*", pw (*)) [
       EFun ("*", pw (*)) [EFun ("*", pw (*)) [EVal 4, EVal 6]],
       EFun ("*", pw (*)) [EFun ("*", pw (*)) [EVal 1, EVal 3]]
       ]),
    "1*3*4*6")
    ]
  where pw f a b = pure $ f a b

testRPNParser :: [TestTree]
testRPNParser =
  map (\(t, want) -> testCase (unpack t) $ assertEqual "" want (eval <$> A.parseOnly parseRPN t)) [
  ("2 3 +", Right (Just 5)),
  ("1 2 3 + *", Right (Just 5)),
  ("1 2 ^ 3 *", Right (Just 3))
  ]

tests :: [TestTree]
tests = [
  testProperty "canonicalizes" prop_canon,
  testProperty "dedups [Value]" prop_dedup,
  testProperty "dedups [[Value]]" prop_dedup2,
  testProperty "dedups Expression" prop_dedup_expr,
  testProperty "new form works" prop_newform,
  testProperty "expr canon" prop_exprcanon,
  testProperty "rpn expr" prop_rpnform,

  -- A couple manual test cases we know
  testGroup "manual solutions" testManualSolutions,
  testGroup "manual canonicalization" testManualCanon,
  testGroup "rpn parsing" testRPNParser
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests

import NumberPuzzle

import Data.Semigroup ((<>))
import Data.Maybe (isJust)
import Data.List (nub)

import Test.Tasty
import Test.QuickCheck
import Test.Tasty.QuickCheck as QC
import Test.Invariant ((<=>))


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
    pure $ op s1 s2

    where
      subex :: Gen Expression
      subex = do
        frequency [
          (4, EVal <$> choose (0,9)),
          (1, arbitrary)
          ]


prop_dedup :: Values -> Bool
prop_dedup (Values v) = nub v == dedup v

prop_dedup2 :: [Values] -> Bool
prop_dedup2 vals = let v = map (\(Values v) -> v) vals in
                     nub v == dedup v

prop_canon :: Values -> Property
prop_canon (Values v) = (isJust . eval) v ==> eval v == (eval . canonicalize) v

prop_exprcanon :: Expression -> Bool
prop_exprcanon = evalexpr <=> evalexpr.canonicalizeexpr

prop_newform :: Values -> Bool
prop_newform (Values v) = (evalexpr =<< exprify v) == eval v

tests :: [TestTree]
tests = [
  testProperty "canonicalizes" prop_canon,
  testProperty "dedups [Value]" prop_dedup,
  testProperty "dedups [[Value]]" prop_dedup2,
  testProperty "new form works" prop_newform,
  testProperty "expr canon" prop_exprcanon
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests

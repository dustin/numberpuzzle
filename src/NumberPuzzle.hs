module NumberPuzzle
    ( Value(..),
      Expression(..),
      eval, solve,
      operators, canonicalize, dedup, exprify, evalexpr, canonicalizeexpr, depth, rpnify
    ) where

import qualified Data.Set as Set
import Control.Applicative (liftA2)
import Control.Monad (guard, replicateM);
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)
import Data.List (sort, sortBy, permutations, intercalate);
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Semigroup ((<>))

data Value = Fun (String, Int -> Int -> Maybe Int)
           | Val Int

instance Eq Value where
  (Val v1) == (Val v2) = v1 == v2
  (Fun (f1,_)) == (Fun (f2,_)) = f1 == f2
  _ == _ = False

instance Ord Value where
  (Val v1) <= (Val v2) = v1 <= v2
  (Fun (f1,_)) <= (Fun (f2,_)) = f1 <= f2
  (Val _) <= (Fun _) = True
  _ <= _ = False

instance Show Value where
  show (Fun (o,_)) = o
  show (Val x) = show x

safeDiv :: Int -> Int -> Maybe Int
safeDiv a 0 = Nothing
safeDiv a b = pure $ a `div` b

safeExp :: Int -> Int -> Maybe Int
safeExp a b
  | b < 0 = Nothing
  | otherwise = pure $ a ^ b

operators :: [Value]
operators = map (\(s,f) -> Fun (s, justice f)) [("+", (+)),
                                                ("-", (-)),
                                                ("*", (*))] <> [Fun ("/", safeDiv),
                                                                Fun ("^", safeExp)]
  where justice :: (Int -> Int -> Int) -> Int -> Int -> Maybe Int
        justice f a b = pure $ f a b

data Expression = EVal Int
                | EFun (String, Int -> Int -> Maybe Int) [Expression]

instance Show Expression where
  show (EVal x) = show x
  show (EFun (fn, _) exps) = intercalate fn (map inner exps)
    where inner (EVal x) = show x
          inner (EFun (fn,_) exps) = "(" <> intercalate fn (map inner exps) <> ")"

depth :: Expression -> Int
depth (EVal _) = 0
depth (EFun _ exps) = 1 + foldr (\x o -> o + depth x) 0 exps

instance Eq Expression where (==) = on (==) show

-- Expressions are sorted by length followed by value.
instance Ord Expression where
  compare (EVal a) (EVal b) = compare a b
  compare a b = (comparing depth <> comparing evalexpr <> comparing show) a b

exprify :: [Value] -> Maybe Expression
exprify = go []
  where go :: [Expression] -> [Value] -> Maybe Expression
        go [x] [] = pure x
        go st (Val v:ops) = go (EVal v:st) ops
        go (v1:v2:st) (Fun f:ops) = go (EFun f [v2,v1]:st) ops
        go _ _ = Nothing

evalexpr :: Expression -> Maybe Int
evalexpr (EVal v) = pure v
evalexpr (EFun (_,f) exps) = foldr1 (lft f) (map evalexpr exps)

  where lft :: Monad m => (a -> a -> m a) -> (m a -> m a -> m a)
        lft f a b = do
          a' <- a
          b' <- b
          f a' b'

rpnify :: Expression -> [Value]
rpnify (EVal x) = [Val x]
rpnify (EFun f exprs) = concatMap rpnify exprs <> [Fun f]

canonicalizeexpr :: Expression -> Expression
canonicalizeexpr e@(EFun _ _) = (commute . associate) e
  where
    commute :: Expression -> Expression
    commute e@(EFun f@(fn,_) exps)
      | commutes fn = EFun f (map canonicalizeexpr $ sortBy (comparing evalexpr) exps)
      | otherwise = e

    commutes "*" = True
    commutes "+" = True
    commutes _ = False

    associate :: Expression -> Expression
    -- (1+2)+3 -> 1+2+3
    associate e@(EFun f@(fn,_) ((EFun (fn',_) inner):rest))
      | associates fn fn' = EFun f (inner <> rest)
      | otherwise = e
      -- 1+(2+3) -> 1+2+3
    associate e@(EFun f@(fn,_) (l:(EFun (fn',_) inner):rest))
      | associates fn fn' = EFun f (l:inner <> rest)
      | otherwise = e
    associate x = x

    associates "*" "*" = True
    associates "+" "+" = True
    associates _ _ = False
canonicalizeexpr x = x

canonicalize :: [Value] -> [Value]
canonicalize =  order
  where
    order (v1@(Val _):v2@(Val _):o@(Fun _):vals)
      | commutes o = sort [v1, v2] <> [o] <> canonicalize vals
    order (x:xs) = x : order xs
    order x = x

    commutes :: Value -> Bool
    commutes (Fun ("+",_)) = True
    commutes (Fun ("*",_)) = True
    commutes _ = False

eval :: [Value] -> Maybe Int
eval = go []
  where go [Val x] [] = Just x
        go st (v@(Val _):ops) = go (v:st) ops
        go (Val v1:Val v2:st) (Fun (_,f):ops) =
          case f v2 v1 of
            Just x -> go (Val x:st) ops
            _ -> Nothing
        go _ _ = Nothing

-- This is an optimized nub.

dedup :: Ord a => [a] -> [a]
dedup = dedup' Set.empty
  where
    dedup' _ [] = []
    dedup' seen (x : xs)
      | x `Set.member` seen = dedup' seen xs
      | otherwise = x : dedup' (Set.insert x seen) xs

solve :: Int -> [Int] -> [Expression]
solve want digits = dedup $ map (canonicalizeexpr.fromJust.exprify) $ do
  digs <- (dedup . permutations) $ map Val digits
  ops <- replicateM (length digits - 1) operators
  inputs <- (dedup . map canonicalize . permutations) (digs <> ops)
  guard $ eval inputs == Just want
  pure inputs

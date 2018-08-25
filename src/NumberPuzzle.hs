module NumberPuzzle
    ( Value(..),
      Expression(..),
      eval, solve,
      operators, canonicalize, dedup, exprify, evalexpr, canonicalizeexpr, depth
    ) where

import qualified Data.Set as Set
import Data.Semigroup ((<>))
import Data.Ord (comparing)
import Data.Function (on)
import Data.Foldable (minimumBy, maximumBy)
import Control.Applicative (liftA2)
import Data.List (sort, permutations);
import Control.Monad (guard, replicateM);

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
                | EFun (String, Int -> Int -> Maybe Int) Expression Expression

instance Show Expression where
  show (EVal x) = show x
  show (EFun (fn, _) l r) = inner l <> " " <> fn <> " " <> inner r
    where inner (EVal x) = show x
          inner (EFun (fn,_) l r) = "(" <> inner l <> " " <> fn <> " " <> inner r <> ")"

depth :: Expression -> Int
depth (EVal _) = 0
depth (EFun _ l r) = 1 + depth l + depth r

instance Eq Expression where (==) = on (==) show

-- Expressions are sorted by length followed by value.
instance Ord Expression where
  compare (EVal a) (EVal b) = compare a b
  compare a b = (comparing depth <> comparing evalexpr) a b

exprify :: [Value] -> Maybe Expression
exprify = go []
  where go :: [Expression] -> [Value] -> Maybe Expression
        go [x] [] = pure x
        go st (Val v:ops) = go (EVal v:st) ops
        go (v1:v2:st) (Fun f:ops) = go (EFun f v2 v1:st) ops
        go _ _ = Nothing

evalexpr :: Expression -> Maybe Int
evalexpr (EVal v) = pure v
evalexpr (EFun (_,f) l r) = do
  l' <- evalexpr l
  r' <- evalexpr r
  f l' r'

canonicalizeexpr :: Expression -> Expression
canonicalizeexpr e@(EFun _ _ _) = commute e
  where
    commute e@(EFun f@(fn,_) l r)
      | commutes fn = EFun f (canonicalizeexpr $ mn l r) (canonicalizeexpr $ mx l r)
      | otherwise = e

    mn :: Expression -> Expression -> Expression
    mn a b = minimumBy (on compare evalexpr) [a, b]

    mx :: Expression -> Expression -> Expression
    mx a b = maximumBy (on compare evalexpr) [a, b]

    commutes "*" = True
    commutes "+" = True
    commutes _ = False
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

solve :: [Int] -> Int -> [[Value]]
solve digits want = dedup $ do
  digs <- (dedup . permutations) $ map Val digits
  ops <- replicateM (length digits - 1) operators
  inputs <- (dedup . map canonicalize . permutations) (digs <> ops)
  guard $ eval inputs == Just want
  pure inputs

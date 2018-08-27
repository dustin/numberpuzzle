{-# LANGUAGE OverloadedStrings #-}

module NumberPuzzle
    ( Value(..),
      Expression(..),
      eval, solve, parseRPN,
      operators, dedup, exprify, evalexpr, canonicalizeexpr, depth, rpnify, parseExpr
    ) where

import qualified Data.Set as Set
import Control.Applicative (liftA2, (<|>))
import Control.Monad (guard, replicateM);
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)
import Data.Void (Void)
import Data.Ratio (denominator)
import Data.Functor (($>), (<$))
import Data.List (sort, sortBy, permutations, intercalate);
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Semigroup ((<>))
import Data.Text (Text, pack)
import Text.Megaparsec (Parsec, sepBy1, between)
import Text.Megaparsec.Char (char, string, space)
import Text.Megaparsec.Expr (makeExprParser, Operator(..))
import qualified Text.Megaparsec.Char.Lexer as L


data Value = Fun (String, Rational -> Rational -> Maybe Rational)
           | Val Rational

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
  show (Val x) = (show.floor.fromRational) x

safeDiv :: Rational -> Rational -> Maybe Rational
safeDiv a 0 = Nothing
safeDiv a b = Just $ a / b

operators :: [Value]
operators = map (\(s,f) -> Fun (s, pw f)) [("+", (+)),
                                           ("-", (-)),
                                           ("*", (*))] <> [Fun ("/", safeDiv)]

-- sort of a double lift
pw :: (Rational -> Rational -> Rational) -> Rational -> Rational -> Maybe Rational
pw f a b = pure $ f a b

data Expression = EVal Rational
                | EFun (String, Rational -> Rational -> Maybe Rational) [Expression]

instance Show Expression where
  show (EVal x) = show x
  show (EFun (fn, _) exps) = intercalate fn (map inner exps)
    where inner (EVal x) = (show.floor.fromRational) x
          inner (EFun (fn,_) exps) = "(" <> intercalate fn (map inner exps) <> ")"

depth :: Expression -> Rational
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

evalexpr :: Expression -> Maybe Rational
evalexpr = modzero . go

  where lft :: Monad m => (a -> a -> m a) -> (m a -> m a -> m a)
        lft f a b = do
          a' <- a
          b' <- b
          f a' b'

        go (EVal v) = pure v
        go (EFun (_,f) exps) = foldr1 (lft f) (map go exps)

modzero :: Maybe Rational -> Maybe Rational
modzero i
  | (denominator <$> i) == Just 1 = i
  | otherwise = Nothing

rpnify :: Expression -> [Value]
rpnify (EVal x) = [Val x]
rpnify (EFun f exprs) = concatMap rpnify exprs <> [Fun f]

canonicalizeexpr :: Expression -> Expression
canonicalizeexpr e@(EFun _ _) = (commute . associate) e
  where
    commute :: Expression -> Expression
    commute e@(EFun f@(fn,_) exps)
      | commutes fn = EFun f (map canonicalizeexpr $ sortBy (comparing evalexpr) exps)
      | otherwise = EFun f $ map canonicalizeexpr exps

      where
        commutes "*" = True
        commutes "+" = True
        commutes _ = False

    associate :: Expression -> Expression
    associate e@(EFun f@(fn,_) exprs) =
      EFun f $ foldr (ass.canonicalizeexpr) [] exprs
      where ass e'@(EFun (fn',_) exprs) o
              | associates fn fn' = exprs <> o
              | otherwise = e':o
            ass x o = x:o
            associates "*" "*" = True
            associates "+" "+" = True
            associates _ _ = False
    associate x = x
canonicalizeexpr x = x

eval :: [Value] -> Maybe Rational
eval = modzero . go []
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


type Parser = Parsec Void Text

parseRPN :: Parser [Value]
parseRPN =  sepBy1 value (char ' ')

  where value :: Parser Value
        value = Val <$> (toRational <$> L.decimal) <|> op

        op = "+"  $> Fun ("+", pw (+))
          <|> "-" $> Fun ("-", pw (-))
          <|> "*" $> Fun ("*", pw (*))
          <|> "/" $> Fun ("/", safeDiv)

parseExpr :: Parser Expression
parseExpr = makeExprParser term operators

  where term :: Parser Expression
        term = parens parseExpr <|> EVal <$> (toRational <$> lexeme L.decimal)

        operators :: [[Operator Parser Expression]]
        operators = [
          [op "*" (pw (*)), op "/" safeDiv],
          [op "+" (pw (+)), op "-" (pw (-))]
          ]

        symbol = L.symbol space
        lexeme = L.lexeme space

        op :: String -> (Rational -> Rational -> Maybe Rational) -> Operator Parser Expression
        op s f = InfixL (binify (EFun (s, f)) <$ symbol (pack s))

        binify :: ([Expression] -> Expression) -> Expression -> Expression -> Expression
        binify e a b =  e [a, b]

        parens :: Parser Expression -> Parser Expression
        parens = between (symbol "(") (symbol ")")


solve :: Rational -> [Rational] -> [Expression]
solve want digits = dedup . map (canonicalizeexpr.fromJust.exprify) $ do
  digs <- (dedup . permutations) $ map Val digits
  ops <- replicateM (length digits - 1) operators
  input <- permutations (digs <> ops)
  guard $ eval input == Just want
  pure input

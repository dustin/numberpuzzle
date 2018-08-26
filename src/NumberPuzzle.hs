{-# LANGUAGE OverloadedStrings #-}

module NumberPuzzle
    ( Value(..),
      Expression(..),
      eval, solve, parseRPN,
      operators, canonicalize, dedup, exprify, evalexpr, canonicalizeexpr, depth, rpnify, parseExpr
    ) where

import qualified Data.Set as Set
import Control.Applicative (liftA2, (<|>))
import Control.Monad (guard, replicateM);
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)
import Data.Void (Void)
import Data.Functor (($>), (<$))
import Data.List (sort, sortBy, permutations, intercalate);
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Semigroup ((<>))
import Data.Text (Text, pack)
import Text.Megaparsec (Parsec, sepBy1, between)
import Text.Megaparsec.Char (char, string, space)
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L


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
operators = map (\(s,f) -> Fun (s, pw f)) [("+", (+)),
                                           ("-", (-)),
                                           ("*", (*))] <> [Fun ("/", safeDiv),
                                                           Fun ("^", safeExp)]

-- sort of a double lift
pw :: (Int -> Int -> Int) -> Int -> Int -> Maybe Int
pw f a b = pure $ f a b

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


type Parser = Parsec Void Text

parseRPN :: Parser [Value]
parseRPN =  sepBy1 value (char ' ')

  where value :: Parser Value
        value = Val <$> L.decimal <|> op

        op = "+"  $> Fun ("+", pw (+))
          <|> "-" $> Fun ("-", pw (-))
          <|> "*" $> Fun ("*", pw (*))
          <|> "/" $> Fun ("/", safeDiv)
          <|> "^" $> Fun ("^", safeExp)

parseExpr :: Parser Expression
parseExpr = makeExprParser term operators

  where term :: Parser Expression
        term = parens parseExpr <|> EVal <$> lexeme L.decimal

        operators :: [[Operator Parser Expression]]
        operators = [
          [op "^" safeExp],
          [op "*" (pw (*)), op "/" safeDiv],
          [op "+" (pw (+)), op "-" (pw (-))]
          ]

        symbol = L.symbol space
        lexeme = L.lexeme space

        op :: String -> (Int -> Int -> Maybe Int) -> Operator Parser Expression
        op s f = InfixL (binify (EFun (s, f)) <$ symbol (pack s))

        binify :: ([Expression] -> Expression) -> Expression -> Expression -> Expression
        binify e a b =  e [a, b]

        parens :: Parser Expression -> Parser Expression
        parens = between (symbol "(") (symbol ")")


solve :: Int -> [Int] -> [Expression]
solve want digits = dedup $ map (canonicalizeexpr.fromJust.exprify) $ do
  digs <- (dedup . permutations) $ map Val digits
  ops <- replicateM (length digits - 1) operators
  inputs <- (dedup . map canonicalize . permutations) (digs <> ops)
  guard $ eval inputs == Just want
  pure inputs

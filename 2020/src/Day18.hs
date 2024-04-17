module Day18 (part1, part2) where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Text.Parsec (ParseError, eof, parse)
import Text.Parsec.Expr (Assoc (AssocLeft), Operator (Infix), buildExpressionParser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (TokenParser, integer, makeTokenParser, parens, reservedOp, reservedOpNames)

data Expr
  = Val Integer
  | Add Expr Expr
  | Mult Expr Expr
  deriving (Show)

expr :: Bool -> Parser Expr
expr samePrecedence = buildExpressionParser table term
 where
  term = parens lexer (expr samePrecedence) <|> fmap Val (integer lexer)
  table =
    (if samePrecedence then (: []) . concat else id)
      [ [binary "+" Add AssocLeft]
      , [binary "*" Mult AssocLeft]
      ]

lexer :: TokenParser ()
lexer = makeTokenParser $ emptyDef{reservedOpNames = ["+", "*"]}

binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
binary name fun = Infix (reservedOp lexer name $> fun)

parseExpression :: Bool -> String -> Either ParseError Expr
parseExpression samePrecedence = parse (expr samePrecedence <* eof) ""

calc :: Either ParseError Expr -> Integer
calc (Left err) = error $ show err
calc (Right expression) = calc' expression
 where
  calc' (Val x) = x
  calc' (Add x y) = calc' x + calc' y
  calc' (Mult x y) = calc' x * calc' y

solve :: Bool -> String -> Integer
solve samePrecedence = sum . map (calc . parseExpression samePrecedence) . lines

part1 :: String -> Integer
part1 = solve True

part2 :: String -> Integer
part2 = solve False

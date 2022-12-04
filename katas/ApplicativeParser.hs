module ApplicativeParser where

import Data.Char
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P {unP :: String -> [(String, a)]}

mapSecond :: (b -> c) -> (a, b) -> (a, c)
mapSecond f (a, b) = (a, f b)

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P $ \xs -> map (mapSecond f) $ unP p xs

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) a p = const a <#> p

infixl 4 <#>

infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P $ \xs ->
  ( case xs of
      [] -> []
      (c : cs) -> [(cs, c) | p c]
  )

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP c = predP (c ==)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ \cs -> [(cs, x)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P $ \cs -> [(r2, f a) | (r, f) <- unP pf cs, (r2, a) <- unP px r]

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = const <#> pa <@> pb

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = (\_ b -> b) <#> pa <@> pb

infixl 4 <@

infixl 4 @>

infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP "" = inject ""
stringP (c : cs) = (:) <#> charP c <@> stringP cs

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
pa <<>> pb = P $ \cs -> unP pa cs ++ unP pb cs

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = some p <<>> inject []

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = (:) <#> p <@> (some p <<>> inject [])

-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = map snd $ filter ((== "") . fst) $ unP p cs

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = if length result == 1 then Just (head result) else Nothing
  where
    result = runParser p cs

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr
  = ConstE Int
  | BinOpE BinOp Expr Expr
  | NegE Expr
  | ZeroE
  deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE n) = n
evalExpr (BinOpE AddBO e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (BinOpE MulBO e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (NegE e) = (-1) * evalExpr e
evalExpr ZeroE = 0

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique expr

expr :: Parser Expr
expr = constExpr <<>> binOpExpr <<>> neg <<>> zero

is1To9 :: Char -> Bool
is1To9 c = ord '1' <= ord c && ord c <= ord '9'

is0To9 :: Char -> Bool
is0To9 c = ord '0' <= ord c && ord c <= ord '9'

constExpr :: Parser Expr
constExpr = (ConstE . read) <#> ((:) <#> predP is1To9 <@> many (predP is0To9))

binOpExpr :: Parser Expr
binOpExpr = op <#> (charP '(' @> expr <@ charP ' ') <@> (charP '*' <<>> charP '+') <@> (charP ' ' @> expr <@ charP ')')
  where
    op :: Expr -> Char -> Expr -> Expr
    op l '+' r = BinOpE AddBO l r
    op l '*' r = BinOpE MulBO l r
    op _ _ _ = error "unknown symbol "

neg :: Parser Expr
neg = NegE <#> (charP '-' @> expr)

zero :: Parser Expr
zero = ZeroE <# charP 'z'

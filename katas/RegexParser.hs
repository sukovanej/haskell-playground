{-# LANGUAGE LambdaCase #-}

module RegexParser where

import Control.Applicative
import Control.Monad
import Data.Functor

-- parser combinators

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return c = Parser (\cs -> [(c, cs)])
  p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])

instance Alternative Parser where
  empty = Parser (const [])
  a <|> b = Parser (\cs -> parse a cs ++ parse b cs)

infixl 9 +++

(+++) :: Parser a -> Parser a -> Parser a
p +++ q =
  Parser
    ( \cs -> case parse (p <|> q) cs of
        [] -> []
        (x : _) -> [x]
    )

item :: Parser Char
item =
  Parser
    ( \case
        "" -> []
        (x : xs) -> [(x, xs)]
    )

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c
    then return c
    else empty

char :: Char -> Parser Char
char c = sat (c ==)

many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many1 p +++ return []
  return $ a : as

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  a <- p
  rest a
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        +++ return a

chainl1Unary :: Parser a -> Parser (a -> a) -> Parser a
p `chainl1Unary` op = do
  a <- p
  rest a
  where
    rest a = (do f <- op; rest $ f a) +++ return a

-- solution

-- "ab*"     -> Str [Normal 'a', ZeroOrMore (Normal 'b')]
-- "(ab)*"   -> ZeroOrMore (Str [Normal 'a', Normal 'b'])
-- "ab|a"    -> Or (Str [Normal 'a',Normal 'b']) (Normal 'a')
-- "a(b|a)"  -> Str [Normal 'a',Or (Normal 'b') (Normal 'a')]
-- "a|b*"    -> Or (Normal 'a') (ZeroOrMore (Normal 'b'))
-- "(a|b)*"  -> ZeroOrMore (Or (Normal 'a') (Normal 'b'))
--
-- FAILING
-- "ab*"          -> Str [Normal 'a', ZeroOrMore (Normal 'b')]
-- "a(b|a)"       -> Str [Normal 'a', Or (Normal 'b') (Normal 'a')]
-- "a.*"          -> Str [Normal 'a',ZeroOrMore Any]
-- "((a.)|.b)*|a" -> Or (ZeroOrMore (Or (Str [Normal 'a',Any]) (Str [Any,Normal 'b']))) (Normal 'a')

data RegExp
  = Normal !Char
  | Any
  | ZeroOrMore !RegExp
  | Or !RegExp !RegExp
  | Str ![RegExp]
  deriving (Eq, Show)

reverseRegExp :: RegExp -> RegExp
reverseRegExp (Normal ch) = Normal ch
reverseRegExp Any = Any
reverseRegExp (ZeroOrMore tree) = ZeroOrMore $ reverseRegExp tree
reverseRegExp (Or left right) = Or (reverseRegExp right) (reverseRegExp left)
reverseRegExp (Str xs) = Str . reverse $ map reverseRegExp xs

-- "ab*"
-- "a*a"
-- "aa*a"
-- "a*aa" -> Str [ZeroOrMore (Normal 'a'),Normal 'a',Normal 'a']
--
-- "a*(aa)" -> Str [ZeroOrMore (Normal 'a'),Str [Normal 'a',Normal 'a']]

-- a*bb

-- expr         ::= expr orOp term | term
-- term         ::= factor term | zeroOrMoreOp factor2 term | zeroOrMoreOp factor2 | factor
-- factor2      ::= normalChar | any | '(' expr ')'
-- factor       ::= string | normalChar | any | '(' expr ')'
-- normalChar   ::= 'a' | 'b' | ... | 'z'
-- any          ::= '.'
-- orOp         ::= '|'
-- zeroOrMoreOp ::= '*'
-- string       ::= [atleast 2 normalChar]

-- "a*aa"
-- "aaa*"
-- "aa*a"
--
-- "*aaa"
-- "*ab(cd)e"

-- expr         ::= expr orOp stringterm | stringterm
-- stringterm   ::= term instringterm | term
-- instringterm ::= term instringterm | term
-- term         ::= zeroOrMoreOp factor | factor
-- factor       ::= normalChar | any | '(' expr ')'
-- normalChar   ::= 'a' | 'b' | ... | 'z'
-- any          ::= '.'
-- orOp         ::= '|'
-- zeroOrMoreOp ::= '*'

parseRegExp :: String -> Maybe RegExp
parseRegExp xs = case parse expr xs of
  [(r, [])] -> Just r
  _other -> Nothing

-- parser

parseExpr :: String -> [(RegExp, String)]
parseExpr s = let result = parse expr (map mapPar $ reverse s) in map (mapFirst reverseRegExp) result
  where
    mapFirst f (a, b) = (f a, b)
    mapPar ')' = '('
    mapPar '(' = ')'
    mapPar x = x

expr :: Parser RegExp
expr = orExpr +++ stringterm
  where
    orExpr = do
      a <- stringterm
      _ <- char '|'
      Or a <$> stringterm

stringterm :: Parser RegExp
stringterm =
  ( do
      t <- term
      instring <- stringterm
      return $ case instring of
        Str xs -> Str $ t : xs
        x -> Str [t, x]
  )
    +++ term

term :: Parser RegExp
term = (char '*' *> factor <&> ZeroOrMore) +++ factor

factor :: Parser RegExp
factor = normalChar +++ anyExpr +++ parenthesisExpr

parenthesisExpr :: Parser RegExp
parenthesisExpr = char '(' *> expr <* char ')'

orOp :: Parser (RegExp -> RegExp -> RegExp)
orOp = char '|' $> Or

normalChar :: Parser RegExp
normalChar = sat (`notElem` "()*|.") <&> Normal

anyExpr :: Parser RegExp
anyExpr = char '.' $> Any

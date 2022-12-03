module Imperative (
  def, var, lit, while, (+=), (-=), (*=)
) where
import Monads (State)

type MyState = State Integer

def :: MyState Integer -> Integer
def r         = error "todo: def"
var :: Integer -> MyState Integer
var v         = error "todo: var"
lit :: Integer -> MyState Integer
lit l         = error "todo: lit"
while :: Integer -> (Integer -> Bool) -> MyState Integer -> MyState Integer
while r f act = error "todo: while"

(+=) :: MyState Integer -> MyState Integer -> MyState Integer
a += b = error "todo: (+=)"
(-=) :: MyState Integer -> MyState Integer -> MyState Integer
a -= b = error "todo: (-=)"
(*=) :: MyState Integer -> MyState Integer -> MyState Integer
a *= b = error "todo: (*=)"

factorial :: Integer -> Integer
factorial n = def $ do
  result <- var 1
  i      <- var n
  while i (>0) $ do
    result *= i
    i      -= lit 1
  return result
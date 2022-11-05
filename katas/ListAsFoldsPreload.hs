{-# LANGUAGE RankNTypes #-}

module ListAsFoldsPreload where

import Prelude (undefined, ($), Integer, Bool (True), (+), (-), Show(show), (++))

newtype List a = List (forall z . (a -> z -> z) -> z -> z)

instance Show a => Show (List a) where
  show xs = "[" ++ foldr xs (\a n -> show a ++ ", " ++ n) "" ++ "]"

foldr :: List a -> (a -> z -> z) -> z -> z
foldr (List xs) = xs

type Number = Integer

zero :: Number -> Boolean
zero 0 = true
zero _ = false

succ, pred :: Number -> Number
succ = (+) 1
pred = (-) 1

plus, times :: Number -> Number -> Number
plus = (+)
times = (-)

data Boolean = MyTrue | MyFalse

true :: Boolean
true = MyTrue

false :: Boolean
false = MyFalse

(?) :: Boolean -> z -> z -> z
(?) MyTrue x _ = x
(?) MyFalse _ x = x

or :: Boolean -> Boolean -> Boolean
or MyTrue _ = MyTrue
or _ MyTrue = MyTrue
or _ _ = MyFalse

data Option a = Just a | Nothing

instance Show a => Show (Option a) where
  show (Just x) = "Just " ++ show x
  show Nothing = "Nothing "

nothing :: Option a
nothing = Nothing

just :: a -> Option a
just = Just

fmap :: (a -> b) -> Option a -> Option b
fmap f (Just x) = just $ f x
fmap _ Nothing = nothing

option :: Option a -> z -> (a -> z) -> z
option (Just x) _ f = f x
option Nothing d _ = d

type Pair a b = (a, b)
pair :: a -> b -> Pair a b
pair = (,)

fst :: Pair a b -> a
fst (x, _) = x

snd :: Pair a b -> b
snd (_, x) = x

first :: (a -> z) -> Pair a b -> Pair z b
first f (x, y) = (f x, y)

second :: (b -> z) -> Pair a b -> Pair a z
second f (x, y) = (x, f y)

both :: (a -> x) -> Pair a a -> Pair x x
both f (x, y) = (f x, f y)

double :: (a -> x) -> (b -> y) -> Pair a b -> Pair x y
double f g (x, y) = (f x, g y)
{-# LANGUAGE RankNTypes #-}
module ChurchNumbers where

import Prelude hiding (succ)

newtype Number = Nr (forall a. (a -> a) -> a -> a)

zero :: Number
zero = Nr (\ _ z -> z)

succ :: Number -> Number
succ (Nr a) = Nr (\ s z -> s (a s z))

one :: Number
one = succ zero

add :: Number -> Number -> Number
add (Nr a) (Nr b) = Nr (\s z -> a s (b s z))

mult :: Number -> Number -> Number
mult (Nr a) (Nr b) = Nr (\f x -> a (\y -> f (b f y)) x)

pow :: Number -> Number -> Number
pow (Nr m) (Nr n) = Nr (\f x -> (n m) f x)
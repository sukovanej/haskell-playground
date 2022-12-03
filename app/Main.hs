{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Main where
import Data.Functor

main :: IO ()
main = putStrLn "Hello, Haskell!"

ioValue :: IO Int
ioValue = pure 1

newList :: IO Int
newList = ioValue <&> (* 2) <&> (+ 2) <&> (`mod` 2)

data Free f a = Pure a | Free (f (Free f a))


-- f (Free f a)
-- fx :: f (Free f a)


instance Functor f => Functor (Free f) where
  fmap f (Free fx) = Free $ fmap f <$> fx
  fmap f (Pure a) = Pure $ f a


-- instance Monad f => Monad (Free f) where
--   return = Pure
--   Pure x  >>= g  =  g x
--   Free fx >>= g  =  Free ((>>= g) <$> fx)


infixr 0 ~>
type f ~> g = forall x. f x -> g x

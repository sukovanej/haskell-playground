{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a,b)
toPair (SPair p) = p (,)
fromPair :: (a,b) -> SPair a b
fromPair (a, b) = SPair $ \f -> f a b
fst :: SPair a b -> a
fst (SPair p) = p $ \a _ -> a
snd :: SPair a b -> b
snd (SPair p) = p $ \_ b -> b
swap :: SPair a b -> SPair b a
swap (SPair p) = SPair $ \f -> (p $ \a b -> f b a)
curry :: (SPair a b -> c) -> (a -> b -> c)
curry p = \a b -> p $ fromPair (a, b)
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f p = f a b
  where (a, b) = toPair p

toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe m) = m Nothing Just
fromMaybe :: Maybe a -> SMaybe a
fromMaybe (Just a) = SMaybe $ \_ f -> f a
fromMaybe Nothing = SMaybe $ \d _ -> d
isJust :: SMaybe a -> Bool
isJust (SMaybe m) = m False $ const True
isNothing :: SMaybe a -> Bool
isNothing (SMaybe m) = m True $ const False
catMaybes :: SList (SMaybe a) -> SList a
catMaybes (SList l) = l emptyList $ \a xs -> runMaybe a (catMaybes xs) $ \x -> cons x (catMaybes xs)

toEither :: SEither a b -> Either a b
toEither (SEither e) = e Left Right
fromEither :: Either a b -> SEither a b
fromEither (Left x) = SEither $ \l _ -> l x
fromEither (Right x) = SEither $ \_ r -> r x
isLeft :: SEither a b -> Bool
isLeft (SEither e) = e (const True) (const False)
isRight :: SEither a b -> Bool
isRight (SEither e) = e (const False) (const True)
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition ll = pair (lefts ll) (rights ll)
  where lefts (SList l) = l emptyList $ \x r -> runEither x (\a -> cons a (lefts r)) (\_ -> lefts r)
        rights (SList l) = l emptyList $ \x r -> runEither x (\_ -> rights r) (\a -> cons a (rights r))

toList :: SList a -> [a]
toList (SList l) = l [] $ \a l' -> a : toList l'
fromList :: [a] -> SList a
fromList [] = SList $ \d _ -> d
fromList (x:xs) = SList $ \_ f -> f x (fromList xs)
cons :: a -> SList a -> SList a
cons x (SList l) = SList $ \_ f -> f x (SList l)
concat :: SList a -> SList a -> SList a
concat (SList a) bb = a bb (\x xs -> cons x (concat xs bb))
null :: SList a -> Bool
null (SList l) = l True $ \_ _ -> False
length :: SList a -> Int
length (SList l) = l 0 $ \_ l' -> 1 + length l'
map :: (a -> b) -> SList a -> SList b
map g (SList l) = SList $ \d f -> l d $ \x r -> f (g x) (map g r)
zip :: SList a -> SList b -> SList (SPair a b)
zip (SList al) (SList bl) = al emptyList $ \a ra -> bl emptyList (\b rb -> cons (pair a b) (zip ra rb))

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f i (SList l) = l i $ \x xs -> foldl f (f i x) xs

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f i (SList l) = l i $ \x xs -> f x (foldr f i xs)

take :: Int -> SList a -> SList a
take n ll@(SList l) = if n == 0 || null ll
  then emptyList
  else SList $ \d f -> l d $ \a r -> f a (take (n - 1) r)

-- helpers
emptyList :: SList a
emptyList = SList $ \d _ -> d

pair :: a -> b -> SPair a b
pair a b = SPair $ \f -> f a b
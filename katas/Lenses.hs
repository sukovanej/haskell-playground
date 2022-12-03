{-# LANGUAGE RankNTypes #-}
module Lenses where

data Atom = Atom { _element :: String, _point :: Point }
data Point = Point { _x :: Double, _y :: Double }

point :: Lens Atom Point
point = mkLens _point (\p a -> Atom (_element a) p)

element :: Lens Atom String
element = mkLens _element (\e a -> Atom e (_point a))

x,y :: Lens Point Double
x = mkLens _x (\x' p -> Point x' (_y p))
y = mkLens _y (\y' p -> Point (_x p) y')

getAtomX :: Atom -> Double
getAtomX = view (point `comp` x)

setPoint :: Point -> Atom -> Atom
setPoint = set point

setElement :: String -> Atom -> Atom
setElement = set element

setX, setY:: Double -> Point -> Point
setX = set x
setY = set y

data Lens s a = Lens {
  view :: s -> a,
  over :: (a -> a) -> s -> s,
  overF :: forall f. Functor f => (a -> f a) -> s -> f s
}

set :: Lens s a -> a -> s -> s
set l a = over l (const a)

comp :: Lens a b -> Lens b c -> Lens a c
comp ab bc = Lens view' over' overF'
  where view' = view bc . view ab
        over' = over ab . over bc
        overF' = overF ab . overF bc

mkLens :: (s -> a) -> (a -> s -> s) -> Lens s a
mkLens view' set' = Lens view'
                         (\f a -> set' (f $ view' a) a)
                         (\f a -> set' (f $ view' a) a)

moveAtom :: Atom -> Atom
moveAtom = over (point `comp` x) (+1)
module ListsAsFolds where

import ListAsFoldsPreload

import Prelude (undefined, ($), Integer)

-- test data

test1 :: List Integer
test1 = cons 1 nil
test2 :: List Integer
test2 = cons 1 (cons 2 nil)
test3 :: List Integer
test3 = cons 1 (cons 2 (cons 3 nil))
testInf :: List Integer
testInf = cons 1 (cons 2 (const undefined (cons undefined nil)))

takenInf = take 1 testInf
appendInf = take 2 $ append test1 testInf

-- utils

getOrElse :: Option a -> a -> a
getOrElse x d = option x d id

id :: a -> a
id a = a

chain :: (a -> Option b) -> Option a -> Option b
chain f a = option a nothing f

min :: Number -> Number -> Number
min a b = min' a b
  where min' ca cb = (?) (zero ca) b ((?) (zero cb) a (min' (pred ca) (pred cb)))

const :: a -> b -> a
const a = \_ -> a

-- primitive operations

isNothing :: Option a -> Boolean
isNothing x = option x true (const false)

cons :: a -> List a -> List a
cons x xs = List (\c n -> c x (foldr xs c n))

nil :: List a
nil = List (\_ n -> n)

-- derived operations
one :: Number
one = succ 0

sum,product :: List Number -> Number
sum xs = foldr xs plus 0
product xs = foldr xs times one

-- derived constructors

iterate :: (a -> a) -> a -> List a
iterate f a = cons a (iterate f (f a))

repeat :: a -> List a
repeat x = cons x (repeat x)

cycle :: List a -> List a
cycle xs = undefined

replicate :: Number -> a -> List a
replicate n a = (?) (zero n) nil (cons a $ replicate (pred n) a)

-- more derived operations

null :: List a -> Boolean
null xs = foldr xs (\_ _ -> false) true

length :: List a -> Number
length xs = foldr xs (\_ n -> plus 1 n) 0

snoc :: List a -> a -> List a
snoc xs a = List $ \c n -> foldr xs c (c a n)

append :: List a -> List a -> List a
append l1 l2 = (?) (null l1) l2 $ (?) (null l2) l1 recursiveCall
  where l2Head = head l2
        l2Tail = l2
        recursiveCall = append (option l2Head l1 (snoc l1)) l2Tail

concat :: List (List a) -> List a
concat xss = (?) (null xss) nil (append head' (concat tail'))
  where head' = getOrElse (head xss) nil
        tail' = getOrElse (tail xss) nil

map :: (a -> z) -> List a -> List z
map f xs = List $ \c n -> foldr xs (\i -> c (f i)) n

indexes :: List a -> List Number
indexes = indexes' 0
  where indexes' index xs' = (?) (null xs') nil (cons index (option (tail xs') nil (indexes' (succ index))))

mapWithIndex :: (Number -> a -> b) -> List a -> List b
mapWithIndex f xs = undefined

filter :: (a -> Boolean) -> List a -> List a
filter = undefined

take :: Number -> List a -> List a
take n xs = (?) (zero n) nil take'
  where take' = option (head xs) nil take''
        take'' h = cons h remaining
        remaining = option (tail xs) nil (take (pred n))

drop :: Number -> List a -> List a
drop n xs = undefined

splitAt :: Number -> List a -> Pair (List a) (List a)
splitAt = undefined

get :: Number -> List a -> Option a
get n xs = (?) (zero n) (head xs) (chain (get (pred n)) (tail xs))

set :: Number -> a -> List a -> List a
set = undefined

any :: (a -> Boolean) -> List a -> Boolean
any = undefined

all :: (a -> Boolean) -> List a -> Boolean
all = undefined

find :: (a -> Boolean) -> List a -> Option a
find = undefined

findIndex :: (a -> Boolean) -> List a -> Option Number
findIndex = undefined

partition :: (a -> Boolean) -> List a -> Pair (List a) (List a)
partition = undefined

span :: (a -> Boolean) -> List a -> Pair (List a) (List a)
span = undefined

minimumBy :: (a -> a -> Boolean) -> List a -> Option a
minimumBy cmp xs = foldr xs (\a n -> just $ getOrElse (fmap (\b -> (?) (cmp a b) a b) n) a) nothing

maximumBy :: (a -> a -> Boolean) -> List a -> Option a
maximumBy cmp xs = foldr xs (\a n -> just $ getOrElse (fmap (\b -> (?) (cmp b a) a b) n) a) nothing

sortBy :: (a -> a -> Boolean) -> List a -> List a
sortBy = undefined

foldl :: List a -> (z -> a -> z) -> z -> z
foldl = undefined

scanl :: List a -> (z -> a -> z) -> z -> List z
scanl = undefined

scanr :: List a -> (a -> z -> z) -> z -> List z
scanr = undefined

reverse :: List a -> List a
reverse xs = option (last xs) nil (\l -> cons l (option (init xs) nil reverse))

head :: List a -> Option a
head xs = (?) (null xs) nothing (foldr xs (\a _ -> just a) nothing)

-- TODO: remove pattern matching

data ConsList a = Cons a (ConsList a) | Nil

toConsList :: List a -> ConsList a
toConsList xs = foldr xs Cons Nil

tailConsList :: ConsList a -> Option (ConsList a)
tailConsList Nil = nothing
tailConsList (Cons _ xs) = just xs

foldrConsList :: ConsList a -> (a -> z -> z) -> z -> z
foldrConsList Nil _ n = n
foldrConsList (Cons x xs) c n = c x (foldrConsList xs c n)

fromConsList :: ConsList a -> List a
fromConsList xs = List $ foldrConsList xs

tail :: List a -> Option (List a)
tail xs = (?) (null xs) nothing (fmap fromConsList $ tailConsList $ toConsList xs)

init :: List a -> Option (List a)
init xs = (?) (null xs) nothing (just $ List $ \c n -> snd $ foldr xs (\a n' -> (?) (fst n') (first (const false) n') (second (c a) n')) (pair true n))

last :: List a -> Option a
last xs = foldr xs (\a n -> (?) (isNothing n) (just a) n) nothing

zip :: List a -> List b -> List (Pair a b)
zip = undefined

unzip :: List (Pair a b) -> Pair (List a) (List b)
unzip = undefined

zipWith :: (a -> b -> z) -> List a -> List b -> List z
zipWith f l1 l2 = map (\i -> f (fst i) (snd i)) $ zip l1 l2

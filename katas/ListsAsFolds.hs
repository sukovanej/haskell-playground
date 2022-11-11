module ListsAsFolds where

import ListsAsFoldsPreload

import Prelude (undefined, ($))

-- utils

getOrElse :: Option a -> a -> a
getOrElse x d = option x d id

id :: a -> a
id a = a

chain :: (a -> Option b) -> Option a -> Option b
chain f a = option a nothing f

min :: Number -> Number -> Number
min a b = minc a b
  where minc ca cb = (?) (zero ca) b ((?) (zero cb) a (minc (pred ca) (pred cb)))

const :: a -> b -> a
const a _ = a

indexed :: List a -> List (Pair Number a)
indexed xs = indexedN xs 0
    where indexedN xsn n = option (head xsn) nil (\h -> cons (pair n h) (indexedN (unsafeTail xsn) (succ n)))

unsafeInit :: List a -> List a
unsafeInit xs = getOrElse (init xs) undefined

unsafeHead :: List a -> a
unsafeHead xs = foldr xs const undefined

unsafeLast :: List a -> a
unsafeLast xs = getOrElse (last xs) undefined

unsafeTail :: List a -> List a
unsafeTail xs = getOrElse (tail xs) undefined

xor :: Boolean -> Boolean -> Boolean
xor a b = or (and a (not b)) (and b (not a))

numberEq :: Number -> Number -> Boolean
numberEq a b = and (not oneIsZero) (or bothZero (numberEq (pred a) (pred b)))
  where oneIsZero = xor (zero a) (zero b)
        bothZero = and (zero a) (zero b)

takeWhile :: (a -> Boolean) -> List a -> List a
takeWhile f xs = option (head xs) xs (\h -> (?) (f h) (cons h (takeWhile f (unsafeTail xs))) nil)

dropWhile :: (a -> Boolean) -> List a -> List a
dropWhile f xs = option (head xs) xs (\h -> (?) (f h) (dropWhile f (unsafeTail xs)) xs)

singleton :: a -> List a
singleton a = cons a nil

isNothing :: Option a -> Boolean
isNothing x = option x true (const false)

-- primitive operations

cons :: a -> List a -> List a
cons x xs = List (\c n -> c x (foldr xs c n))

nil :: List a
nil = List (\_ n -> n)

-- derived operations

sum,product :: List Number -> Number
sum xs = foldr xs plus 0
product xs = foldr xs times 1

-- derived constructors

iterate :: (a -> a) -> a -> List a
iterate f a = cons a (iterate f (f a))

repeat :: a -> List a
repeat x = cons x (repeat x)

cycle :: List a -> List a
cycle xs = (?) (null xs) undefined (append xs (cycle xs))

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
append l1 l2 = (?) (null l1) ((?) (null l2) nil (append l2 nil)) appendn
  where tailn = getOrElse (tail l1) nil
        appendn = option (head l1) nil (\h -> cons h (append tailn l2))

concat :: List (List a) -> List a
concat xss = foldr xss append nil

map :: (a -> z) -> List a -> List z
map f xs = List $ \c n -> foldr xs (\i -> c (f i)) n

filter :: (a -> Boolean) -> List a -> List a
filter f xs = List $ \c n -> foldr xs (\a nn -> (?) (f a) (c a nn) nn) n

take :: Number -> List a -> List a
take n xs = (?) (zero n) nil taken
  where taken = option (head xs) nil takenn
        takenn h = cons h remaining
        remaining = option (tail xs) nil (take (pred n))

drop :: Number -> List a -> List a
drop n xs = (?) (or (zero n) (null xs)) xs (drop (pred n) tailn)
  where tailn = getOrElse (tail xs) nil

splitAt :: Number -> List a -> Pair (List a) (List a)
splitAt i xs = pair (map snd lxs) (map snd rxs)
  where indexedXs = indexed xs
        lxs = takeWhile (\x -> not (numberEq (fst x) i)) indexedXs
        rxs = dropWhile (\x -> not (numberEq (fst x) i)) indexedXs

get :: Number -> List a -> Option a
get n xs = (?) (zero n) (head xs) (chain (get (pred n)) (tail xs))

set :: Number -> a -> List a -> List a
set i newA xs = List $ \c n -> foldr (indexed xs) (\a nn -> (?) (numberEq (fst a) i) (c newA nn) (c (snd a) nn)) n

any :: (a -> Boolean) -> List a -> Boolean
any f xs = foldr xs (\a n -> or (f a) n) false

all :: (a -> Boolean) -> List a -> Boolean
all f xs = foldr xs (\a n -> and (f a) n) true

find :: (a -> Boolean) -> List a -> Option a
find f xs = foldr xs (\a n -> (?) (f a) (just a) n) nothing

findIndex :: (a -> Boolean) -> List a -> Option Number
findIndex f xs = foldr (indexed xs) (\a n -> (?) (f (snd a)) (just $ fst a) n) nothing

partition :: (a -> Boolean) -> List a -> Pair (List a) (List a)
partition f xs = pair (filter f xs) (filter (\i -> not $ f i) xs)

span :: (a -> Boolean) -> List a -> Pair (List a) (List a)
span f xs = pair left rest
  where left = takeWhile f xs
        rest = drop (length left) xs

minimumBy :: (a -> a -> Boolean) -> List a -> Option a
minimumBy cmp xs = foldr xs (\a n -> just $ getOrElse (fmap (\b -> (?) (cmp a b) a b) n) a) nothing

maximumBy :: (a -> a -> Boolean) -> List a -> Option a
maximumBy cmp xs = foldr xs (\a n -> just $ getOrElse (fmap (\b -> (?) (cmp b a) a b) n) a) nothing

isSorted :: (a -> a -> Boolean) -> List a -> Boolean
isSorted f xs = option (last xs) true (\l -> fst $ foldr (unsafeInit xs) (\a n -> (pair (and (fst n) (f a (snd n) )) a)) (pair true l))

sortBy :: (a -> a -> Boolean) -> List a -> List a
sortBy f xs = (?) (isSorted f xs) xs (sortBy f newXs)
  where newXs = bubble f xs

bubble :: (a -> a -> Boolean) -> List a -> List a
bubble f xs = option (head xs) nil (\x -> option (head tailX) (singleton x) (\y -> newXs x y))
  where newXs x y = cons (getFirst x y) (bubble f $ cons (getSecond x y) tailY)
        tailX = unsafeTail xs
        tailY = unsafeTail tailX
        getFirst a b = (?) (f a b) a b
        getSecond a b = (?) (f a b) b a

foldl :: List a -> (z -> a -> z) -> z -> z
foldl xs f = foldr xs (\x g a -> g (f a x)) id

scanl :: List a -> (z -> a -> z) -> z -> List z
scanl xs f d = foldl xs (\n a -> snoc n (f (unsafeLast n) a)) (cons d nil)

scanr :: List a -> (a -> z -> z) -> z -> List z
scanr xs f d = foldr xs (\a n -> cons (f a (unsafeHead n)) n ) (cons d nil)

reverse :: List a -> List a
reverse xs = option (last xs) nil (\l -> cons l (option (init xs) nil reverse))

head :: List a -> Option a
head xs = (?) (null xs) nothing (foldr xs (\a _ -> just a) nothing)

-- please someone kill me
tail :: List a -> Option (List a)
tail list@(List xs) = (?) (null list) nothing $ just $ List $ (\l c n -> l (\h t g -> g h (t c)) (const n) (\_ t -> t)) xs

init :: List a -> Option (List a)
init xs = (?) (null xs) nothing (just $ List $ \c n -> snd $ foldr xs (\a nn -> (?) (fst nn) (first (const false) nn) (second (c a) nn)) (pair true n))

last :: List a -> Option a
last xs = foldr xs (\a n -> (?) (isNothing n) (just a) n) nothing

zip :: List a -> List b -> List (Pair a b)
zip ls rs = option (chain (\l -> fmap (pair l) rhead) lhead) nil (\p -> cons p (zip ltail rtail))
  where lhead = head ls
        rhead = head rs
        ltail = getOrElse (tail ls) nil
        rtail = getOrElse (tail rs) nil

unzip :: List (Pair a b) -> Pair (List a) (List b)
unzip xs = pair (map fst xs) (map snd xs)

zipWith :: (a -> b -> z) -> List a -> List b -> List z
zipWith f l1 l2 = map (\i -> f (fst i) (snd i)) $ zip l1 l2

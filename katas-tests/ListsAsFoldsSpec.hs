module ListsAsFoldsSpec where

import Prelude (($), undefined, Integer, Maybe(Nothing, Just), Bool(True, False), take, (>=), (<=))
import qualified ListsAsFolds as L (cons,nil,sum,product,iterate,repeat,cycle,replicate,
                                    null,length,snoc,append,concat,map,filter,
                                    take,drop,splitAt,get,set,
                                    any,all,find,findIndex,partition,span,minimumBy,maximumBy,sortBy,
                                    foldl,scanl,scanr,
                                    reverse,head,tail,init,last,
                                    zip,unzip,zipWith, isSorted, bubble)
import ListsAsFoldsPreload
import Test.Hspec
import Control.Exception (evaluate)

(#) :: Integer -> List Integer -> List Integer
infixr 0 #
(#) = L.cons

(##) :: List a -> List (List a) -> List (List a)
infixr 0 ##
(##) = L.cons

toList :: List a -> [a]
toList xs = foldr xs (:) []

toMaybe :: Option a -> Maybe a
toMaybe x = option x Nothing Just

toBool :: Boolean -> Bool
toBool x = (?) x True False

fromBool :: Bool -> Boolean
fromBool True = true
fromBool False = false

fromCmp :: (a -> a -> Bool) -> (a -> a -> Boolean)
fromCmp cmp l r = fromBool $ cmp l r

listAsFoldsSpec :: SpecWith ()
listAsFoldsSpec = do
  describe "test" $ do
    let infList = 1 # 2 # 3 # undefined # undefined # L.nil
    let list = 1 # 2 # L.nil
    let list2 = 1 # 2 # 3 # L.nil
    let xs = 3 # 2 # 1 # L.nil
        inf = 3 # 2 # 1 # undefined # undefined

    it "null" $ do
      toBool (L.null (undefined # L.nil)) `shouldBe` False

    it "take" $ do
      toList (L.take 1 list) `shouldBe` toList (1 # L.nil)
      toList (L.take 2 list2) `shouldBe` toList (1 # 2 # L.nil)
      toList (L.take 3 list2) `shouldBe` toList (1 # 2 # 3 # L.nil)
      toList (L.take 0 list2) `shouldBe` toList L.nil
      toList (L.take 0 infList) `shouldBe` toList L.nil
      toList (L.take 1 infList) `shouldBe` toList (1 # L.nil)
      toList (L.take 2 infList) `shouldBe` toList (1 # 2 # L.nil)

    it "append" $ do
      toList (L.append L.nil L.nil) `shouldBe` ([] :: [Number])
      toList (L.append L.nil  xs) `shouldBe` [3,2,1]
      toList (L.append  xs L.nil) `shouldBe` [3,2,1]
      toList (L.append  xs  xs) `shouldBe` [3,2,1,3,2,1]
      toList (L.append (1 # 2 # 3 # L.nil) (4 # 5 # 6 # L.nil)) `shouldBe` [1,2,3,4,5,6]
      toList (L.take 3 $ L.append inf xs) `shouldBe` [3,2,1]
      toList (L.take 6 $ L.append xs inf) `shouldBe` [3,2,1,3,2,1]

    it "head" $ do
      toMaybe (L.head infList) `shouldBe` toMaybe (just 1)

    it "concat" $ do
      toList (L.concat (list ## list2 ## L.nil)) `shouldBe` toList (1 # 2 # 1 # 2 # 3 # L.nil)
      toList (L.take 4 $ L.concat (list ## list2 ## infList ## L.nil)) `shouldBe` toList (1 # 2 # 1 # 2 # L.nil)
      toList (L.concat L.nil) `shouldBe` ([] :: [Number])
      toList (L.concat $ L.nil ## L.nil ## L.nil ## L.nil) `shouldBe` ([] :: [Number])
      toList (L.concat $ xs ## L.nil ## xs ## L.nil ## xs ## L.nil) `shouldBe` [3,2,1,3,2,1,3,2,1]
      toList (L.take 3 $ L.concat $ inf ## inf ## inf ## L.nil) `shouldBe` [3,2,1]

    it "cycle" $ do
      take 17 (toList $ L.cycle xs) `shouldBe` [3,2,1,3,2,1,3,2,1,3,2,1,3,2,1,3,2]
      evaluate (L.cycle L.nil) `shouldThrow` anyException
      toList (L.take 3 $ L.cycle inf) `shouldBe` [3,2,1]

    it "drop" $ do
      toList (L.drop 0 xs) `shouldBe` [3,2,1]
      toList (L.drop 1 xs) `shouldBe` [2,1]
      toList (L.drop 2 xs) `shouldBe` [1]
      toList (L.drop 3 xs) `shouldBe` []
      toList (L.drop 4 xs) `shouldBe` []
      toList (L.take 2 $ L.drop 1 inf) `shouldBe` [2,1]

    it "isSorted" $ do
      toBool (L.isSorted (fromCmp (<=)) $ 1 # 2 # 3 # L.nil) `shouldBe` True
      toBool (L.isSorted (fromCmp ((<=) :: Integer -> Integer -> Bool)) L.nil) `shouldBe` True
      toBool (L.isSorted (fromCmp (<=)) $ 2 # 1 # 3 # L.nil) `shouldBe` False
      toBool (L.isSorted (fromCmp (<=)) $ 1 # 3 # 2 # L.nil) `shouldBe` False
      toBool (L.isSorted (fromCmp (<=)) $ 2 # 3 # 1 # L.nil) `shouldBe` False

    it "bubble" $ do
      toList (L.bubble (fromCmp (>=)) L.nil :: List Integer) `shouldBe` []
      toList (L.bubble (fromCmp (>=)) $ 2 # L.nil) `shouldBe` [2]
      toList (L.bubble (fromCmp (<=)) $ 2 # 1 # L.nil) `shouldBe` [1,2]
      toList (L.bubble (fromCmp (<=)) $ 2 # 1 # 3 # L.nil) `shouldBe` [1,2,3]
      toList (L.bubble (fromCmp (<=)) $ 3 # 1 # 2 # L.nil) `shouldBe` [1,2,3]

    it "sorting" $ do
      toList (L.sortBy (fromCmp (<=)) $ 1 # 2 # 3 # L.nil) `shouldBe` [1,2,3]
      toList (L.sortBy (fromCmp (<=)) $ 3 # 1 # 2 # L.nil) `shouldBe` [1,2,3]
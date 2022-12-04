module ApplicativeParserSpec where

import ApplicativeParser
import Test.Hspec

applicativeParserSpec :: SpecWith ()
applicativeParserSpec = do
  describe "Applicative parser" $ do
    it "stringP" $ do
      unP (stringP "") "a" `shouldBe` [("a", "")]
      runParserUnique (stringP "") "a" `shouldBe` Nothing

    it "stringP" $ do
      runParserUnique (many $ charP 'a') "aaa" `shouldBe` Just "aaa"

    it "math expr" $ do 
     unP expr "((-(4 * 2) * z) + (2 + 3))" `shouldNotBe` []

    it "math expr 2" $ do 
     evalExpr <$> parseExpr "((-(4 * 2) * z) + (2 + 3))" `shouldBe` Just 5

    it "utils" $ do 
      is0To9 '0' `shouldBe` True
      is0To9 '4' `shouldBe` True
      is0To9 '9' `shouldBe` True
      is0To9 '(' `shouldBe` False
      is0To9 '-' `shouldBe` False
      is0To9 'a' `shouldBe` False
      is0To9 'Z' `shouldBe` False

module RegexParserSpec where

import RegexParser (RegExp (..), parseExpr, parseRegExp)
import Test.Hspec

regexParserSpec :: SpecWith ()
regexParserSpec = do
  describe "smallest possible sum" $ do
    it "simple cases 1" $ do
      parseExpr "a" `shouldBe` [(Normal 'a', "")]

    it "simple cases 2" $ do
      parseExpr "ab" `shouldBe` [(Str [Normal 'a', Normal 'b'], "")]

    it "simple cases 3" $ do
      parseExpr "a(b|a)" `shouldBe` [(Str [Normal 'a',Or (Normal 'b') (Normal 'a')], "")]

    it "simple cases 4" $ do
      parseExpr "ab*" `shouldBe` [(Str [Normal 'a',ZeroOrMore (Normal 'b')], "")]

    it "simple cases 4" $ do
      parseExpr "a*a" `shouldBe` [(Str [ZeroOrMore (Normal 'a'),Normal 'a'], "")]

    it "case 5" $ do
      parseExpr "a*aa" `shouldBe` [(Str [ZeroOrMore (Normal 'a'),Normal 'a',Normal 'a'], "")]

    it "case 6" $ do
      parseRegExp "a|a|a" `shouldBe` Nothing


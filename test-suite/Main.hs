-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec
import Test.Hspec
import RegExpDerivatives hiding (main)
import Text.Megaparsec (parseMaybe)
import Data.Text
import RegExpDerivatives.RegExp (RegExp(..),parse, parseDef, deriv, nullable)

main :: IO ()
main = do
    test <- testSpec "reg-exp-derivatives" unitTests
    Test.Tasty.defaultMain test

unitTests :: Spec
unitTests = parallel $ do
    it "pretty printing concat abc" $ do
        show(Concat (Concat (Ch 'a') (Ch 'b')) (Ch 'c')) `shouldBe` "a.b.c"

    it "pretty printing plus and star a+b*" $ do
        show (Star (Plus (Ch 'a') (Ch 'b'))) `shouldBe` "a+b*"

    it "pretty printing ab+c" $ do
        show (Plus (Concat (Ch 'a') (Ch 'b')) (Ch 'c')) `shouldBe` "a.b+c"

    it "parsing ab+c" $ do 
        parseMaybe parse ("ab+c")
          `shouldBe` Just (Plus (Concat (Ch 'a') (Ch 'b')) (Ch 'c'))

    it "parsing (a~)*" $ do
        parseMaybe parse ("(a~)*")
          `shouldBe` Just (Star (Not (Ch 'a')))

    it "testing the derivative func on a.b with respect to a and b" $ do 
        let regex = Concat (Ch 'a') (Ch 'b')
        nullable (deriv 'b' (deriv 'a' regex))
          `shouldBe` True

    it "string matching (a)* " $ do
        let regex = Star ( Ch ('a'))
        Prelude.map (regex ~~) ["a", "aa", "ab", "bbb", "aaaaaaaaaaaaaaab"]
          `shouldBe` [True, True, False, False, False]
    
    it "string matching (a+b)* " $ do
        let regex = Star (Plus  (Ch 'a') (Ch 'b'))
        Prelude.map (regex ~~) ["a", "aa", "ab", "bbb", "aaaaaaaaaaaaaaab", "c", "caaaababababbabad"]
          `shouldBe` [True, True, True, True, True, False, False]

    it "string parsing and matching b*b " $ do
        let regex1 = parseDef "b*b"
        Prelude.map (regex1 ~~) ["a", "aa", "ab", "bbb", "aaaaaaaaaaaaaaab", "c", "caaaababababbabad", "bbbbbb"]
            `shouldBe` [False, False, False, True, False, False, False, True]

    it "string parsing and matching aa " $ do
        let regex1 = parseDef "aa"
        Prelude.map (regex1 ~~) ["a", "aa", "ab", "bbb", "aaaaaaaaaaaaaaab", "c", "caaaababababbabad"]
            `shouldBe` [False, True, False, False, False, False, False]

    it "string parsing and matching ε " $ do
        let regex1 = parseDef "ε"
        Prelude.map (regex1 ~~)["a", "aa", "ab", "bbb", "aaaaaaaaaaaaaaab", "c", "caaaababababbabad"]
            `shouldBe` [False, False, False, False, False, False, False]

    it "string parsing and matching (abba)* " $ do
        let regex1 = parseDef "(abba)*"
        Prelude.map (regex1 ~~) ["a", "aa", "abba", "abbaabba", "aaaaaaaaaaaaaaab", "c", "caaaababababbabad"]
            `shouldBe` [False, False, True, True, False, False, False]
    
    it "testing NOT & and STAR on regex ((ab)*)~ " $ do
        let regex1 = parseDef "((ab)*)~"
        Prelude.map (regex1 ~~) ["ab", "cc", "dsdsdssdsdssdsddcjcdjcdcdncd"]
            `shouldBe` [False, True, True]

    it "testing AND & and STAR on regex ((a+b)&(b+c))* " $ do
        let regex1 = parseDef "((a+b)&(b+c))*"
        Prelude.map (regex1 ~~) ["b", "cc", "a"]
            `shouldBe` [True, False, False]
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
import RegExpDerivatives.RegExp (RegExp(..),parse)

main :: IO ()
main = do
    test <- testSpec "reg-exp-derivatives" unitTests
    Test.Tasty.defaultMain test

unitTests :: Spec
unitTests = parallel $ do
    it "pretty printing concat a.b.c" $ do
        show(Concat (Concat (Ch 'a') (Ch 'b')) (Ch 'c')) `shouldBe` "abc"

    it "pretty printing plus and star a+b*" $ do
        show (Star (Plus (Ch 'a') (Ch 'b'))) `shouldBe` "a+b*"

    it "pretty printing ab+c" $ do
        show (Plus (Concat (Ch 'a') (Ch 'b')) (Ch 'c')) `shouldBe` "ab+c"

    it "parsing ab+c" $ do 
        parseMaybe parse ("ab+c")
          `shouldBe` Just (Plus (Concat (Ch 'a') (Ch 'b')) (Ch 'c'))
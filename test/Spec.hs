import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe           (fromJust)
import           Data.Word8
import           Lib
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  jsons <- runIO (BSL.readFile "sample_input30.json") -- read the sample input file
  onewordfile <- runIO (BSL.readFile "sample_input1.json")
  let json = fixit jsons
  let res = (decode json :: Maybe Board)
  let oneword = (decode onewordfile :: Maybe Board)
  describe "read" $ do
    context "when used with ints" $ do
      it "is inverse to show" $ property $
        \x -> (read . show) x == (x :: Int)
  describe "read/write" $ do
    context "when reading json" $ do
      it "is inverse to FromJSON" $
        ((fixit . encode . fromJust $ res) `shouldBe` json)
  describe "find words" $ do
    context "given a board with one word" $ do
      it "finds one word" $
        (length . findWords $ fromJust oneword) `shouldBe` (1 :: Int)
  -- describe "score" $ do
  --   context "when given letters" $ do
  --     it "can sum letters" $
  --       undefined

fixit = BSL.filter (not . Data.Word8.isSpace)
{-
Local Variables:
compile-command: "stack test"
End:
-}

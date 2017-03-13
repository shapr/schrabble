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
  jsons <- runIO (BSL.readFile "sample_input8.json") -- read the sample input file
  let json = fixit jsons
  let res = (decode json :: Maybe Board)
  describe "read" $ do
    context "when used with ints" $ do
      it "is inverse to show" $ property $
        \x -> (read . show) x == (x :: Int)
  describe "read" $ do
    context "when reading json" $ do
      it "is inverse to FromJSON" $
        ((fixit . encode . fromJust $ res) `shouldBe` json)

fixit = BSL.filter (not . Data.Word8.isSpace)
{-
Local Variables:
compile-command: "stack test"
End:
-}

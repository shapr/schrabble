{-# LANGUAGE OverloadedStrings #-}
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.List            (sort)
import           Data.Maybe           (fromJust)
import qualified Data.Text            as T
import           Data.Word8
import           Lib
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  jsons <- runIO (BSL.readFile "test/sample_input30.json") -- read the sample input file
  onewordfile <- runIO (BSL.readFile "test/sample_input1.json")
  twowordfile <- runIO (BSL.readFile "test/sample_input2.json")
  tenwordfile <- runIO (BSL.readFile "test/sample_input8.json")

  let json = fixit jsons
  let res = (decode json :: Maybe Board)
  let oneword = (cleanBoard $ view board $ fromJust $ (decode onewordfile :: Maybe Board))
  let tenwords = (cleanBoard $ view board $ fromJust $ (decode tenwordfile :: Maybe Board))
  let twowords = (cleanBoard $ view board $ fromJust $ (decode twowordfile :: Maybe Board))

  describe "read/write" $ do
    context "when reading/writing json" $ do
      it "is inverse to FromJSON" $
        ((fixit . encode . fromJust $ res) `shouldBe` json)
    context "when qc'ing it" $
      it "can QC its own datatype" $
        property $ \x -> (fromJust . decode $ encode x) == (x :: Board)
  describe "find words" $ do
    context "given a board with one word" $ do
      it "finds one word" $
        (getWords $ oneword) `shouldBe` (["*OWDY"] :: [T.Text])
    context "given a board with two words" $ do
      it "finds two words" $
        (sort . getWords $ twowords) `shouldBe` (sort ["*OWDY","FAMED"] :: [T.Text])
    context "given a board with ten words" $ do
      it "finds ten words" $
        (sort . getWords $ tenwords) `shouldBe` (sort ["*OWDY","FAMED","GRAYER","CRIE*","YIP","TIGHT","RAZE","MOVE","IT","PI"] :: [T.Text])
  describe "score" $ do
    context "when given letters" $ do
      it "can sum letters" $
        (scoreWord "PEE" == 5)

fixit = BSL.filter (not . Data.Word8.isSpace)
{-
Local Variables:
compile-command: "stack test"
End:
-}

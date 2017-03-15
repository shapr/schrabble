{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Lib where
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types      (Options (fieldLabelModifier))
import           Data.Bits
import           Data.Bits.Lens
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as BSL
import           Data.Char             (toLower)
import           Data.Data             (Data, Typeable)
import           Data.Int
import qualified Data.Map.Strict       as Map
import qualified Data.Text             as T
import qualified Data.Vector           as V
import           Data.Word             (Word32 (..), Word64 (..), byteSwap32,
                                        byteSwap64)
import           GHC.Generics          (Generic)
import           Word128

data Board = Board {
  _board   :: ![[T.Text]]
  ,_scores :: ![[Int]]
  } deriving (Generic,Eq,Ord,Show)

makeLenses ''Board

instance FromJSON Board where
  parseJSON = genericParseJSON opts
    where opts = defaultOptions { fieldLabelModifier = map toLower . drop 1}

instance ToJSON Board where
  toEncoding = genericToEncoding opts
    where opts = defaultOptions { fieldLabelModifier = map toLower . drop 1}

-- letter values and distributions
letterValues = Map.fromList [('*',0),('A',1),('B',3),('C',3),('D',2),('E',1),('F',4),('G',2),('H',4),('I',1),('J',8),('K',5),('L',1),('M',3),('N',1),('O',1),('P',3),('Q',10),('R',1),('S',1),('T',1),('U',1),('V',4),('W',4),('X',8),('Y',4),('Z',10)]

letterDistribution = Map.fromList [('*',2),('A',9),('B',2),('C',2),('D',4),('E',12),('F',2),('G',3),('H',2),('I',9),('J',1),('K',1),('L',4),('M',2),('N',6),('O',8),('P',2),('Q',1),('R',6),('S',4),('T',6),('U',4),('V',2),('W',2),('X',1),('Y',2),('Z',1)]

-- vector of words?
lmain :: IO ()
lmain = do
  contents <- BS.readFile "sowpods.txt"
  let wordslist = V.fromList $ BSC.split '\n' contents :: V.Vector BS.ByteString
  print "hi"


findWords :: Board -> Int
findWords = undefined

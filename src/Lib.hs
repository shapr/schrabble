{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types     (Options (fieldLabelModifier))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Char            (toLower)
import           GHC.Generics         (Generic)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Board = Board {
  _board   :: ![[String]]
  ,_scores :: ![[Int]]
  } deriving (Generic,Eq,Ord,Show)

makeLenses ''Board

instance FromJSON Board where
  parseJSON = genericParseJSON opts
    where opts = defaultOptions { fieldLabelModifier = map toLower . drop 1}

instance ToJSON Board where
  toEncoding = genericToEncoding opts
    where opts = defaultOptions { fieldLabelModifier = map toLower . drop 1}

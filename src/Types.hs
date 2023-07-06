{-# LANGUAGE DeriveGeneric #-}
module Types where
import GHC.Generics
import Data.Aeson
data Movie = Movie
  {
    mname :: String
    ,rating :: Double
    ,genre :: String
  } deriving (Generic,Show)
instance ToJSON Movie
instance FromJSON Movie